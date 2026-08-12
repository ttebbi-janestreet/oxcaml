(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved. This file is distributed under the terms of     *)
(*   the GNU Lesser General Public License version 2.1, with the special  *)
(*   exception on linking described in the file LICENSE.                  *)
(*                                                                        *)
(**************************************************************************)

(* Offline consumer for patchprof profiles.

   Aggregates the raw walk records of one or more profiles into a bottom-up
   trie: the roots are the sampled sites' functions and each node's children
   are its callers.  Every site's walks are scaled so that the site's total
   trie mass equals its exact fast-path execution count, and percentages are
   normalized by the total execution count over all instrumented sites, so
   they are not skewed by sampling noise or by sites too cold to have
   produced a trace.  Addresses are symbolized with addr2line against the
   instrumented executable. *)

let usage =
  "patchprof_summary -exe EXECUTABLE [options] PROFILE...\n\
   Summarize patchprof profiles as a bottom-up (callers) trie."

let executable = ref ""
let min_percent = ref 1.0
let max_depth = ref 6
let profiles = ref []

let options =
  [ "-exe", Arg.Set_string executable,
    "EXECUTABLE the instrumented binary, for symbolization";
    "-min-percent", Arg.Set_float min_percent,
    Printf.sprintf
      "P do not print nodes below this share of the total (default %.1f)"
      !min_percent;
    "-max-depth", Arg.Set_int max_depth,
    Printf.sprintf "N caller depth to print (default %d)" !max_depth ]

type walk =
  { weight : int;
    frames : int64 array (* the sampled site first, then its callers *) }

type statistics =
  { mutable attempted : int;
    mutable failed : int;
    mutable dropped : int;
    mutable selection_records : int }

(* One profile's instrumented subset, from its header; the runtime selects
   every [stride]-th unique site within a window of [stride] * 4096 sites,
   starting at [window_start] + [residue]. *)
type selection =
  { stride : int;
    num_unique : int;
    window_start : int;
    residue : int }

let num_countdowns = 4096

let add_to table key amount =
  Hashtbl.replace table key
    (amount + Option.value ~default:0 (Hashtbl.find_opt table key))

(* The profile is a stream of 64-bit little-endian words: the magic word,
   then records of the form [kind, payload_words, payload...]; see
   runtime/caml/patchprof.h for the record kinds.  A record truncated by
   the producer dying mid-write ends the stream. *)
let profile_magic = 0x000a31464f525050L (* "PPROF1\n" *)

let parse_profile path walks statistics selections executions exposures =
  let contents = In_channel.with_open_bin path In_channel.input_all in
  let num_words = String.length contents / 8 in
  let word i = String.get_int64_le contents (8 * i) in
  let int i = Int64.to_int (word i) in
  if num_words < 1 || word 0 <> profile_magic then
    failwith (path ^ ": not a patchprof binary profile");
  let position = ref 1 in
  let continue = ref true in
  while !continue && !position + 2 <= num_words do
    let kind = int !position in
    let length = int (!position + 1) in
    let payload = !position + 2 in
    if length < 0 || payload + length > num_words then continue := false
    else begin
      (match kind with
       | 1 (* selection *) when length >= 7 ->
         statistics.selection_records <- statistics.selection_records + 1;
         Hashtbl.replace selections
           { stride = int (payload + 1);
             num_unique = int (payload + 3);
             window_start = int (payload + 4);
             residue = int (payload + 5) }
           ()
       | 2 (* walk chunk: domain id, then walk records *) ->
         let cursor = ref (payload + 1) in
         let chunk_end = payload + length in
         let ok = ref true in
         while !ok && !cursor + 2 <= chunk_end do
           let weight = int !cursor in
           let packed = word (!cursor + 1) in
           let depth = Int64.to_int (Int64.logand packed 0xffffffffL) in
           if depth < 1 || !cursor + 2 + depth > chunk_end then ok := false
           else begin
             let frames =
               Array.init depth (fun i -> word (!cursor + 2 + i))
             in
             walks := { weight; frames } :: !walks;
             cursor := !cursor + 2 + depth
           end
         done
       | 3 (* counter batch: domain id, count, active ns, then 5 words
              per site *) ->
         let count = if length >= 3 then int (payload + 1) else 0 in
         let active_ns = if length >= 3 then int (payload + 2) else 0 in
         for site = 0 to min count ((length - 3) / 5) - 1 do
           let base = payload + 3 + (5 * site) in
           add_to executions (word base) (int (base + 1));
           add_to exposures (word base) active_ns
         done
       | 4 (* domain statistics *) when length >= 5 ->
         statistics.attempted <- statistics.attempted + int (payload + 1);
         statistics.failed <- statistics.failed + int (payload + 2);
         statistics.dropped <- statistics.dropped + int (payload + 4)
       | _ -> ());
      position := payload + length
    end
  done

(* [camlStdlib__Set__bal_3_151_code] reads better as [Stdlib.Set.bal]:
   strip the [caml] prefix, the [_code] suffix and the stamp/uid suffixes,
   decode the [$XX] hex escapes of the mangling, and turn the [__] module
   separators into dots. *)
let prettify_name name =
  let strip_prefix prefix name =
    let n = String.length prefix in
    if String.length name > n && String.sub name 0 n = prefix
    then String.sub name n (String.length name - n)
    else name
  in
  let strip_suffix suffix name =
    let n = String.length suffix in
    if String.length name > n
       && String.sub name (String.length name - n) n = suffix
    then String.sub name 0 (String.length name - n)
    else name
  in
  let rec strip_stamps name =
    match String.rindex_opt name '_' with
    | Some i
      when i > 0 && i + 1 < String.length name
           && String.for_all
                (fun c -> c >= '0' && c <= '9')
                (String.sub name (i + 1) (String.length name - i - 1)) ->
      strip_stamps (String.sub name 0 i)
    | _ -> name
  in
  let decode_escapes name =
    let buffer = Buffer.create (String.length name) in
    let hex c =
      match c with
      | '0' .. '9' -> Some (Char.code c - Char.code '0')
      | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
      | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
      | _ -> None
    in
    let i = ref 0 in
    while !i < String.length name do
      (match name.[!i] with
       | '$' when !i + 2 < String.length name ->
         (match hex name.[!i + 1], hex name.[!i + 2] with
          | Some high, Some low ->
            Buffer.add_char buffer (Char.chr ((high lsl 4) lor low));
            i := !i + 2
          | _ -> Buffer.add_char buffer '$')
       | c -> Buffer.add_char buffer c);
      incr i
    done;
    Buffer.contents buffer
  in
  let dot_separators name =
    let buffer = Buffer.create (String.length name) in
    let i = ref 0 in
    while !i < String.length name do
      if !i + 1 < String.length name
         && name.[!i] = '_' && name.[!i + 1] = '_'
      then begin
        Buffer.add_char buffer '.';
        i := !i + 2
      end
      else begin
        Buffer.add_char buffer name.[!i];
        incr i
      end
    done;
    Buffer.contents buffer
  in
  if String.length name > 4
     && String.sub name 0 4 = "caml"
     && name.[4] >= 'A' && name.[4] <= 'Z'
  then
    name
    |> strip_prefix "caml"
    |> strip_suffix "_code"
    |> strip_stamps
    |> decode_escapes
    |> dot_separators
  else name

let prettify_location location =
  let location =
    match String.index_opt location ' ' with
    | Some i -> String.sub location 0 i (* drop " (discriminator N)" *)
    | None -> location
  in
  let prefix = "/workspace_root/" in
  if String.length location > String.length prefix
     && String.sub location 0 (String.length prefix) = prefix
  then
    String.sub location (String.length prefix)
      (String.length location - String.length prefix)
  else location

(* Return addresses point after their call instruction; querying one byte
   back attributes them to the call site.  The first frame is the sampled
   site itself and is queried as is. *)
let query_address ~frame_index address =
  if frame_index = 0 then address else Int64.sub address 1L

let symbolize addresses =
  let queries = Filename.temp_file "patchprof" ".addresses" in
  let answers = Filename.temp_file "patchprof" ".symbols" in
  Fun.protect
    ~finally:(fun () -> Sys.remove queries; Sys.remove answers)
    (fun () ->
       let file = open_out queries in
       List.iter (fun a -> Printf.fprintf file "0x%Lx\n" a) addresses;
       close_out file;
       let command =
         Filename.quote_command "addr2line" ~stdin:queries ~stdout:answers
           ["-f"; "-C"; "-e"; !executable]
       in
       if Sys.command command <> 0 then
         failwith "addr2line failed; is it installed and is -exe right?";
       let file = open_in answers in
       let symbols = Hashtbl.create 65536 in
       List.iter
         (fun address ->
            let name = input_line file in
            let location = input_line file in
            Hashtbl.replace symbols address
              (prettify_name name, prettify_location location))
         addresses;
       close_in file;
       symbols)

type node =
  { mutable mass : float;
    children : (string, node) Hashtbl.t;
    locations : (string, float) Hashtbl.t (* location -> mass *) }

let create_node () =
  { mass = 0.; children = Hashtbl.create 8; locations = Hashtbl.create 4 }

let insert root symbols (walk : walk) mass =
  root.mass <- root.mass +. mass;
  let node = ref root in
  Array.iteri
    (fun frame_index address ->
       let name, location =
         Hashtbl.find symbols (query_address ~frame_index address)
       in
       let child =
         match Hashtbl.find_opt !node.children name with
         | Some child -> child
         | None ->
           let child = create_node () in
           Hashtbl.add !node.children name child;
           child
       in
       child.mass <- child.mass +. mass;
       Hashtbl.replace child.locations location
         (mass
          +. Option.value ~default:0.
               (Hashtbl.find_opt child.locations location));
       node := child)
    walk.frames

let dominant_location node =
  Hashtbl.fold
    (fun location mass best ->
       match best with
       | Some (_, best_mass) when best_mass >= mass -> best
       | _ -> Some (location, mass))
    node.locations None
  |> Option.fold ~none:"?" ~some:fst

let sorted_children node =
  Hashtbl.fold (fun name child acc -> (name, child) :: acc) node.children []
  |> List.sort (fun (_, a) (_, b) -> Float.compare b.mass a.mass)

(* The percentage lives in a fixed-width leading column and only the name is
   indented: right-aligning the number inside the indentation would make its
   width read as an extra level. *)
let rec print_node ~depth ~total name node =
  let percent = 100. *. node.mass /. total in
  if percent >= !min_percent && depth <= !max_depth then begin
    Printf.printf "%6.2f%%  %s%s  (%s)\n"
      percent
      (String.make (2 * depth) ' ')
      name (dominant_location node);
    List.iter
      (fun (name, child) -> print_node ~depth:(depth + 1) ~total name child)
      (sorted_children node)
  end

(* How much of the executable's instrumentable-site space the profiles'
   random selections have covered, per (stride, total sites) combination. *)
let report_coverage selections =
  let groups = Hashtbl.create 4 in
  Hashtbl.iter
    (fun selection () ->
       let key = (selection.stride, selection.num_unique) in
       Hashtbl.replace groups key
         (selection
          :: Option.value ~default:[] (Hashtbl.find_opt groups key)))
    selections;
  Hashtbl.iter
    (fun (stride, num_unique) group ->
       let window_size = stride * num_countdowns in
       let num_windows = 1 + (num_unique - 1) / window_size in
       let possible_combos = ref 0 in
       for window = 0 to num_windows - 1 do
         let remaining = num_unique - (window * window_size) in
         possible_combos := !possible_combos + min remaining stride
       done;
       let covered = Bytes.make num_unique '\000' in
       List.iter
         (fun selection ->
            let window_end =
              min (selection.window_start + window_size) num_unique
            in
            let index = ref (selection.window_start + selection.residue) in
            while !index < window_end do
              Bytes.set covered !index '\001';
              index := !index + stride
            done)
         group;
       let covered_sites = ref 0 in
       Bytes.iter
         (fun byte -> if byte <> '\000' then incr covered_sites)
         covered;
       Printf.printf
         "coverage at stride %d: %d distinct selections covering %d/%d \
          (window, residue) combos; %d/%d sites instrumented at least once \
          (%.1f%%)\n"
         stride (List.length group) (List.length group) !possible_combos
         !covered_sites num_unique
         (100. *. float_of_int !covered_sites /. float_of_int num_unique))
    groups

let () =
  Arg.parse options (fun profile -> profiles := profile :: !profiles) usage;
  if !executable = "" || !profiles = [] then begin
    Arg.usage options usage;
    exit 2
  end;
  let walks = ref [] in
  let statistics =
    { attempted = 0; failed = 0; dropped = 0; selection_records = 0 }
  in
  let selections = Hashtbl.create 256 in
  let executions = Hashtbl.create 65536 in
  let exposures = Hashtbl.create 65536 in
  List.iter
    (fun path ->
       parse_profile path walks statistics selections executions exposures)
    !profiles;
  if !walks = [] then begin
    prerr_endline "no walk records found; was the profile produced with \
                   walk logging enabled?";
    exit 1
  end;
  (* Scale every site's walks so that the site's trie mass equals its
     execution *rate*: exact fast-path executions divided by the wall-clock
     time its windows were installed.  Counts alone would overweight sites
     whose windows happened to be instrumented longer or more often, e.g.
     under rotation. *)
  let site_walk_weight = Hashtbl.create 65536 in
  List.iter
    (fun walk -> add_to site_walk_weight walk.frames.(0) walk.weight)
    !walks;
  let rate address =
    match Hashtbl.find_opt executions address with
    | None -> None
    | Some count ->
      let exposure =
        max 1 (Option.value ~default:0 (Hashtbl.find_opt exposures address))
      in
      Some (1e9 *. float_of_int count /. float_of_int exposure)
  in
  let total_executions =
    Hashtbl.fold (fun _address count acc -> acc + count) executions 0
  in
  let total_exposure =
    Hashtbl.fold (fun _address ns acc -> acc + ns) exposures 0
  in
  let total_rate =
    Hashtbl.fold
      (fun address _ acc ->
         acc +. Option.value ~default:0. (rate address))
      executions 0.
  in
  let addresses = Hashtbl.create 65536 in
  List.iter
    (fun walk ->
       Array.iteri
         (fun frame_index address ->
            Hashtbl.replace addresses
              (query_address ~frame_index address) ())
         walk.frames)
    !walks;
  let symbols =
    symbolize
      (Hashtbl.fold (fun address () acc -> address :: acc) addresses [])
  in
  let root = create_node () in
  List.iter
    (fun walk ->
       let site = walk.frames.(0) in
       let mass =
         match rate site, Hashtbl.find_opt site_walk_weight site with
         | Some site_rate, Some site_weight when site_weight > 0 ->
           float_of_int walk.weight *. site_rate /. float_of_int site_weight
         | _ -> 0.
       in
       insert root symbols walk mass)
    !walks;
  let total = total_rate in
  Printf.printf
    "%d profile(s), %d selection records, %d walks (%d attempted, \
     %d failed, %d dropped), %d distinct addresses\n"
    (List.length !profiles) statistics.selection_records
    (List.length !walks) statistics.attempted statistics.failed
    statistics.dropped (Hashtbl.length addresses);
  report_coverage selections;
  Printf.printf
    "total fast-path executions of instrumented sites: %d over %.2f \
     site-seconds of window exposure\n\
     percentages are shares of the exposure-normalized execution rate; \
     walks cover %.1f%% of it (the rest executed too rarely to be \
     sampled)\n\
     bottom-up trie (indentation = callers), pruned below %.2f%%:\n\n"
    total_executions
    (float_of_int total_exposure /. 1e9)
    (100. *. root.mass /. total) !min_percent;
  List.iter
    (fun (name, child) -> print_node ~depth:0 ~total name child)
    (sorted_children root)
