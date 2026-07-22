(* Decodes a Linux perf profile of an OxCaml-compiled executable into a
   source-position FDO profile (see [Source_position_profile]):

   oxcaml-fdo-decode -perf-data perf.data -binary ./prog -o prog.fdo

   Sample addresses are decoded into inlining stacks via the executable's DWARF
   (the binary must have been compiled with -g, using the external assembler,
   and with inlined-frame DWARF enabled, e.g. via -gno-upstream-dwarf).
   Positions are stored hashed; -debug-map additionally embeds a hash ->
   position map, and -dump prints an existing profile. *)

module P = Source_position_profile
module Elf_info = Fdo_decode_lib.Elf_info
module Perf_script = Fdo_decode_lib.Perf_script
module Symbolizer = Fdo_decode_lib.Symbolizer

let usage =
  "usage: oxcaml-fdo-decode -perf-data <perf.data> -binary <exe> -o <out>\n\
  \       oxcaml-fdo-decode -dump <profile>"

let perf_data = ref "perf.data"

let binary = ref ""

let output = ref ""

let debug_map = ref false

let max_depth = ref 16

let symbolizer = ref "llvm-symbolizer"

let perf = ref "perf"

let perf_script_output = ref ""

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
    ( "-dump",
      Arg.Set_string dump,
      "<file>  print the given profile in readable form and exit" );
    "-quiet", Arg.Set quiet, "  do not print a summary" ]

let dump_profile filename =
  let p = P.load ~filename in
  Format.printf "%a@." P.print_stats p;
  P.iter p ~f:(fun ~hash ~depth ~count ->
      let position =
        match P.position_of_hash p hash with
        | Some position -> position
        | None -> Printf.sprintf "%016Lx" hash
      in
      Format.printf "%s%s: %Ld@." (String.make (2 * depth) ' ') position count)

let decode () =
  if String.equal !binary "" || String.equal !output ""
  then (
    prerr_endline usage;
    exit 2);
  let elf = Elf_info.read !binary in
  if Elf_info.is_pie elf
  then
    Printf.eprintf
      "Warning: %s is position-independent; sampled addresses will not\n\
       match its link-time addresses and the profile will likely be empty.\n"
      !binary;
  let samples =
    if String.equal !perf_script_output ""
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
      (fun ({ file; line; col } : Symbolizer.frame) ->
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
  P.Writer.write writer ~filename:!output ~buildid:(Elf_info.buildid elf)
    ~total_samples:samples.total ~debug_map:!debug_map;
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
