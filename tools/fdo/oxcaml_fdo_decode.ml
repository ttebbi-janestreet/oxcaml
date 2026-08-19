(* Decodes a Linux perf profile of an OxCaml-compiled executable into a
   source-position FDO profile (see [Source_position_profile]):

   oxcaml-fdo-decode -perf-data perf.data -binary ./prog -o prog.fdo

   Alternatively, -patchprof decodes the profile written by a
   patchprof-instrumented executable (via OCAML_PATCHPROF_OUT) instead of perf
   data:

   oxcaml-fdo-decode -patchprof prog.patchprof -binary ./prog -o prog.fdo

   Sample addresses are decoded into inlining stacks via the executable's DWARF
   (the binary must have been compiled with -g, using the external assembler,
   and with inlined-frame DWARF enabled, e.g. via -gno-upstream-dwarf).
   Positions are stored hashed; -debug-map additionally embeds a hash ->
   position map, and -dump prints an existing profile. *)

module P = Source_position_profile
module Elf_info = Fdo_decode_lib.Elf_info
module Patchprof_labels = Fdo_decode_lib.Patchprof_labels
module Patchprof_profile = Fdo_decode_lib.Patchprof_profile
module Perf_script = Fdo_decode_lib.Perf_script
module Symbolizer = Fdo_decode_lib.Symbolizer

let usage =
  "usage: oxcaml-fdo-decode -perf-data <perf.data> -binary <exe> -o <out>\n\
  \       oxcaml-fdo-decode -patchprof <profile> -binary <exe> -o <out>\n\
  \       oxcaml-fdo-decode -dump <profile>"

let perf_data = ref "perf.data"

let binary = ref ""

let output = ref ""

let debug_map = ref false

let max_depth = ref 16

let symbolizer = ref "llvm-symbolizer"

let perf = ref "perf"

let perf_script_output = ref ""

let patchprof = ref []

let dump = ref ""

let quiet = ref false

let args =
  [ ( "-perf-data",
      Arg.Set_string perf_data,
      "<file>  perf profile to decode (default: perf.data)" );
    "-binary", Arg.Set_string binary, "<file>  the profiled executable";
    "-o", Arg.Set_string output, "<file>  profile file to write";
    ( "-debug-map",
      Arg.Set debug_map,
      "  include a hash -> source position map for debugging (default: off)" );
    ( "-max-stack-depth",
      Arg.Set_int max_depth,
      "<n>  truncate inlining stacks to <n> frames (default: 16)" );
    ( "-symbolizer",
      Arg.Set_string symbolizer,
      "<cmd>  symbolizer to use (default: llvm-symbolizer)" );
    "-perf-path", Arg.Set_string perf, "<cmd>  perf to use (default: perf)";
    ( "-perf-script-output",
      Arg.Set_string perf_script_output,
      "<file>  read pre-captured 'perf script -F ip,dso,period' output\n\
      \     instead of running perf" );
    ( "-patchprof",
      Arg.String (fun file -> patchprof := file :: !patchprof),
      "<file>  decode a patchprof profile (written via OCAML_PATCHPROF_OUT)\n\
      \     instead of perf data; repeatable, merging the profiles (which\n\
      \     must come from the same executable)" );
    ( "-dump",
      Arg.Set_string dump,
      "<file>  print the given profile in readable form and exit" );
    "-quiet", Arg.Set quiet, "  do not print a summary" ]

let dump_profile filename =
  let p = P.load ~filename in
  Format.printf "%a@." P.print_stats p;
  P.iter p ~f:(fun ~hash ~depth ~count ~labels ->
      let position =
        match P.position_of_hash p hash with
        | Some position -> position
        | None -> Printf.sprintf "%016Lx" hash
      in
      Format.printf "%s%s: %Ld" (String.make (2 * depth) ' ') position count;
      List.iter
        (fun ((disc, edge), count) ->
          Format.printf " [label %d.%d: %Ld]" disc edge count)
        labels;
      Format.printf "@.")

let decode () =
  if String.equal !binary "" || String.equal !output ""
  then (
    prerr_endline usage;
    exit 2);
  let patchprof_mode = not (List.compare_length_with !patchprof 0 = 0) in
  if patchprof_mode && not (String.equal !perf_script_output "")
  then (
    prerr_endline
      "at most one of -patchprof and -perf-script-output may be given";
    exit 2);
  let elf = Elf_info.read !binary in
  (* Patchprof profiles record addresses with the load bias removed, so they
     match the link-time addresses of position-independent executables too; perf
     samples do not. *)
  if Elf_info.is_pie elf && not patchprof_mode
  then
    Printf.eprintf
      "Warning: %s is position-independent; sampled addresses will not\n\
       match its link-time addresses and the profile will likely be empty.\n"
      !binary;
  let patchprof_sites = Hashtbl.create 16 in
  let samples =
    if patchprof_mode
    then (
      (* Merge the profiles of several runs of the executable: the per-run
         counts are already rescaled to that run's exact execution counts, so
         both they and the site counters simply add up. *)
      let merged : Perf_script.t =
        { counts = Hashtbl.create 4096; total = 0L }
      in
      List.iter
        (fun filename ->
          let { Patchprof_profile.samples; sites } =
            Patchprof_profile.read ~filename
          in
          Hashtbl.iter
            (fun stack count ->
              let existing =
                Option.value (Hashtbl.find_opt merged.counts stack) ~default:0L
              in
              Hashtbl.replace merged.counts stack (Int64.add existing count))
            samples.counts;
          merged.total <- Int64.add merged.total samples.total;
          Hashtbl.iter
            (fun addr
                 ({ executions; sampled_weight; tally } :
                   Patchprof_profile.site_counters) ->
              let counters =
                match Hashtbl.find_opt patchprof_sites addr with
                | None ->
                  { Patchprof_profile.executions; sampled_weight; tally }
                | Some (acc : Patchprof_profile.site_counters) ->
                  { Patchprof_profile.executions =
                      Int64.add acc.executions executions;
                    sampled_weight = Int64.add acc.sampled_weight sampled_weight;
                    tally = Int64.add acc.tally tally
                  }
              in
              Hashtbl.replace patchprof_sites addr counters)
            sites)
        !patchprof;
      merged)
    else if String.equal !perf_script_output ""
    then Perf_script.collect ~perf:!perf ~perf_data:!perf_data ~binary:!binary
    else
      In_channel.with_open_text !perf_script_output
        (Perf_script.of_channel ~binary:!binary)
  in
  (* Symbolize each distinct address once. Sampled (leaf) addresses are looked
     up as-is; caller entries are return addresses, so they are looked up one
     byte back to land inside the call instruction. *)
  let leaves = Hashtbl.create 1024 in
  let callers = Hashtbl.create 1024 in
  Hashtbl.iter
    (fun stack _count ->
      match stack with
      | [] -> ()
      | leaf :: rest ->
        Hashtbl.replace leaves leaf ();
        List.iter (fun addr -> Hashtbl.replace callers addr ()) rest)
    samples.counts;
  let symbolize table ~adjust =
    let addrs = Hashtbl.fold (fun addr () acc -> addr :: acc) table [] in
    let stacks =
      Symbolizer.symbolize ~symbolizer:!symbolizer ~binary:!binary
        ~addrs:(List.map (fun addr -> Int64.add addr adjust) addrs)
    in
    let frames = Hashtbl.create (Int.max 16 (List.length addrs)) in
    List.iter2 (fun addr stack -> Hashtbl.add frames addr stack) addrs stacks;
    frames
  in
  let leaf_frames = symbolize leaves ~adjust:0L in
  let caller_frames = symbolize callers ~adjust:(-1L) in
  let frame_strings frames =
    List.map
      (fun ({ file; line; col; discriminator = _ } : Symbolizer.frame) ->
        P.frame_string ~file ~line ~col)
      frames
  in
  let writer = P.Writer.create () in
  let unknown = ref 0L in
  Hashtbl.iter
    (fun stack count ->
      match stack with
      | [] -> ()
      | leaf :: rest -> (
        match Hashtbl.find leaf_frames leaf with
        | [] -> unknown := Int64.add !unknown count
        | leaf_stack ->
          (* Append the callers' inline-expanded frames, stopping at the first
             return address the symbolizer knows nothing about (keeping the
             recorded stack a prefix of the real one). *)
          let rec caller_stacks = function
            | [] -> []
            | addr :: rest -> (
              match Hashtbl.find caller_frames addr with
              | [] -> []
              | frames -> frame_strings frames @ caller_stacks rest)
          in
          let frames = frame_strings leaf_stack @ caller_stacks rest in
          P.Writer.add_stack writer ~frames ~count ~max_depth:!max_depth))
    samples.counts;
  (* Attribute each instrumented site's exact execution count to the
     pseudo-instrumentation labels of its machine edges: the taken-bias
     estimate, scaled to the exact count, goes to the taken edge's labels and
     the rest to the fallthrough edge's. A site the sampler never hit has no
     direction estimate and contributes no label counts (its executions still
     feed the position counts above). *)
  let site_labels =
    if patchprof_mode
    then
      match Elf_info.section_body elf "patchprof_labels" with
      | Some data -> Patchprof_labels.parse data
      | None -> Hashtbl.create 0
    else Hashtbl.create 0
  in
  Hashtbl.iter
    (fun addr { Patchprof_profile.executions; sampled_weight; tally } ->
      match Hashtbl.find_opt site_labels addr with
      | None -> ()
      | Some { Patchprof_labels.taken; fallthrough } ->
        if Int64.compare sampled_weight 0L > 0
        then (
          let taken_count =
            Int64.of_float
              (Float.round
                 (Int64.to_float executions *. Int64.to_float tally
                 /. Int64.to_float sampled_weight))
          in
          let add count ({ frame_hashes; disc; edge } : Patchprof_labels.label)
              =
            P.Writer.add_label_hashes writer ~frame_hashes ~disc ~edge ~count
              ~max_depth:!max_depth
          in
          List.iter (add taken_count) taken;
          List.iter (add (Int64.sub executions taken_count)) fallthrough))
    patchprof_sites;
  P.Writer.write writer ~filename:!output ~buildid:(Elf_info.buildid elf)
    ~total_samples:samples.total
    ~kind:(if patchprof_mode then P.Branches else P.Instructions)
    ~debug_map:!debug_map;
  if not !quiet
  then
    Printf.eprintf
      "%s: %Ld samples in %d distinct stacks (%Ld at positions unknown to\n\
       the symbolizer); wrote %s\n"
      !binary samples.total
      (Hashtbl.length samples.counts)
      !unknown !output

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad ("unexpected argument " ^ anon)))
    usage;
  if not (String.equal !dump "") then dump_profile !dump else decode ()
