(* Decodes a Linux perf profile of an OxCaml-compiled executable into an FDO
   profile (see [Source_position_profile]):

   perf record -e cycles:u -j any,u -- ./prog ... oxcaml-fdo-decode -perf-data
   perf.data -binary ./prog -o prog.fdo

   The profile must have been recorded with LBR branch stacks (-j any,u; the
   runtime's single-stepping emulator produces equivalent output, consumed via
   -perf-script-output). The executable's own FDO metadata section (it must have
   been compiled with -fdo-labels or -fdo-profile; no debug info is needed) says
   which locations each observed taken branch (by its source, and, for the call
   graph, by the function entry it lands on joined with its call site) and each
   sequentially executed address count for. Locations are only ever handled
   hashed, so -dump prints an existing profile in terms of hashes; the
   compiler's -dfdo shows the same counts by position. *)

module P = Source_position_profile
module Elf_info = Fdo_decode_lib.Elf_info
module Metadata = Fdo_decode_lib.Metadata
module Perf_script = Fdo_decode_lib.Perf_script

let usage =
  "usage: oxcaml-fdo-decode -perf-data <perf.data> -binary <exe> -o <out>\n\
  \       oxcaml-fdo-decode -dump <profile> [-binary <exe>]"

let perf_data = ref "perf.data"

let perf_script_output = ref ""

let binary = ref ""

let output = ref ""

let dump = ref ""

let ip_ranges = ref false

let args =
  [ ( "-perf-data",
      Arg.Set_string perf_data,
      "<file>  perf profile to decode (default: perf.data)" );
    "-binary", Arg.Set_string binary, "<file>  the profiled executable";
    "-o", Arg.Set_string output, "<file>  profile file to write";
    ( "-perf-script-output",
      Arg.Set_string perf_script_output,
      "<file>  read pre-captured 'perf script -F period,ip,brstack' output\n\
      \     instead of running perf" );
    ( "-ip-ranges",
      Arg.Set ip_ranges,
      " count the code from each sample's most recent branch target up to\n\
      \     its ip as executed; sound only if the branch stack was frozen at\n\
      \     the sampled instruction (the runtime's emulator, Intel LBR; not\n\
      \     AMD)" );
    ( "-dump",
      Arg.Set_string dump,
      "<file>  print the given profile and exit: its trie of locations, by\n\
      \     hash, or by name where -binary names an executable compiled\n\
      \     with -fdo-names" ) ]

let read_metadata () =
  let elf = Elf_info.read !binary in
  (* Perf samples record runtime addresses, which match the link-time addresses
     the metadata describes only for position-dependent executables. *)
  if Elf_info.is_pie elf
  then
    Printf.eprintf
      "Warning: %s is position-independent; sampled addresses will not\n\
       match its link-time addresses and the profile will likely be empty.\n"
      !binary;
  match Elf_info.section_body elf "fdo_metadata" with
  | Some data -> Metadata.parse data
  | None ->
    Printf.eprintf
      "%s has no fdo_metadata section; was it compiled with -fdo-labels?\n"
      !binary;
    exit 1

let dump_profile filename =
  let p = P.load ~filename in
  let names =
    if String.equal !binary ""
    then Hashtbl.create 0
    else (read_metadata ()).names
  in
  let print ~hash ~depth ~count =
    let name =
      match Hashtbl.find_opt names hash with
      | Some level -> Fdo_location.level_string level
      | None -> Printf.sprintf "%016Lx" hash
    in
    Printf.printf "%s%s: %Ld\n" (String.make (2 * depth) ' ') name count
  in
  P.iter p ~f:print;
  print_endline "call targets:";
  P.iter_call_targets p ~f:print

let decode () =
  if String.equal !binary "" || String.equal !output ""
  then (
    prerr_endline usage;
    exit 2);
  let metadata = read_metadata () in
  let events =
    if String.equal !perf_script_output ""
    then
      Perf_script.collect ~ip_ranges:!ip_ranges ~metadata ~perf_data:!perf_data
    else
      In_channel.with_open_text !perf_script_output
        (Perf_script.of_channel ~ip_ranges:!ip_ranges ~metadata)
  in
  let writer = P.Writer.create () in
  let add count locations =
    List.iter
      (fun hashes -> P.Writer.add_hashed_stack writer ~hashes ~count)
      locations
  in
  let total = ref 0L in
  Hashtbl.iter
    (fun (source, target) count ->
      total := Int64.add !total count;
      add count (Metadata.branch_locations metadata ~source ~target);
      List.iter
        (fun (callsite, callee) ->
          P.Writer.add_call_target writer ~callsite ~callee ~count)
        (Metadata.call_targets metadata ~source ~target))
    events.branches;
  Hashtbl.iter
    (fun (lo, hi) count -> Metadata.iter_range metadata ~lo ~hi (add count))
    events.ranges;
  P.Writer.write writer ~filename:!output;
  Printf.eprintf "%s: %Ld taken branches observed; wrote %s\n" !binary !total
    !output

let () =
  Arg.parse args
    (fun anon -> raise (Arg.Bad ("unexpected argument " ^ anon)))
    usage;
  if not (String.equal !dump "") then dump_profile !dump else decode ()
