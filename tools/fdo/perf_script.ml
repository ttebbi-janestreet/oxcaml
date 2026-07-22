(* Extraction of raw samples from a Linux perf profile.

   The heavy lifting is done by "perf script", which is spawned with the field
   list "ip,dso,period". For a profile without call graphs each sample prints as
   a single line

   <period> <ip-hex> (<dso-path>)

   (perf always orders fields canonically: period before ip before dso). When
   the profile was recorded with call graphs (perf record -g / --call-graph),
   the ip and dso fields move into the call chain: each sample prints as a
   header line containing just the period, then one indented "<ip-hex>
   (<dso-path>)" line per frame (the sampled instruction first, callers below),
   then a blank line.

   Samples whose sampled instruction is outside the profiled executable are
   dropped; call chains are cut at the first frame outside it (we cannot decode
   foreign code with the executable's debug info, and cutting keeps the recorded
   stack a prefix of the real one). The remaining stacks are aggregated into a
   per-stack histogram.

   Not every perf.data records periods; if perf rejects the field list, we retry
   without "period" and weigh each sample as 1. (In that legacy mode, call-chain
   entries are indistinguishable from samples; profiles with call graphs should
   not be decoded with a perf too old to print periods.) *)

type t =
  { counts : (int64 list, int64) Hashtbl.t;
    (* leaf-first call stack (sampled address, then return addresses of callers)
       -> accumulated count *)
    mutable total : int64 (* sum of all counts, i.e. of in-target samples *)
  }

let create () = { counts = Hashtbl.create 4096; total = 0L }

let add t ~stack ~count =
  let existing = Option.value (Hashtbl.find_opt t.counts stack) ~default:0L in
  Hashtbl.replace t.counts stack (Int64.add existing count);
  t.total <- Int64.add t.total count

type line =
  | Blank
  | Chain_header of int64 (* the period of a sample with a call chain *)
  | Sample of int64 * int64 * string (* count, address, dso *)
  | Address of int64 * string (* address, dso: chain entry or legacy sample *)

(* Classify one output line. Any line that does not have a known shape aborts
   loudly: silently skipped samples would skew the profile. *)
let classify_line line =
  let fail () = failwith (Printf.sprintf "cannot parse sample %S" line) in
  let line = String.trim line in
  let int64_of_token token =
    match Int64.of_string_opt token with Some n -> n | None -> fail ()
  in
  if String.equal line ""
  then Blank
  else
    match String.rindex_opt line '(' with
    | Some lparen when String.ends_with ~suffix:")" line -> (
      let dso =
        String.sub line (lparen + 1) (String.length line - lparen - 2)
      in
      let rest = String.trim (String.sub line 0 lparen) in
      let tokens =
        String.split_on_char ' ' rest
        |> List.filter (fun token -> not (String.equal token ""))
      in
      match tokens with
      | [addr] -> Address (int64_of_token ("0x" ^ addr), dso)
      | [count; addr] ->
        Sample (int64_of_token count, int64_of_token ("0x" ^ addr), dso)
      | _ -> fail ())
    | Some _ -> fail ()
    | None -> Chain_header (int64_of_token line)

let dso_matches ~binary dso =
  String.equal dso binary
  || String.equal (Filename.basename dso) (Filename.basename binary)

(* [entries] is a sample's call chain, leaf (sampled address) first. When the
   DWARF has inlined-frame info, perf synthesizes an extra entry per inlining
   level, with "inlined" in the dso position and the same address as the real
   entry it expands. Drop them: the decoder performs its own inline expansion
   (with the file strings the hash contract needs) when it symbolizes each
   address. *)
let add_chain t ~binary ~count ~entries =
  let entries =
    List.filter (fun (_, dso) -> not (String.equal dso "inlined")) entries
  in
  match entries with
  | [] -> ()
  | (leaf, leaf_dso) :: callers ->
    if dso_matches ~binary leaf_dso
    then
      let rec in_target = function
        | (addr, dso) :: rest when dso_matches ~binary dso ->
          addr :: in_target rest
        | (_, _) :: _ | [] -> []
      in
      add t ~stack:(leaf :: in_target callers) ~count

let of_channel ic ~binary =
  let t = create () in
  (* The call chain being accumulated: the pending sample's period and its
     entries so far (in reverse). [None] outside a call chain. *)
  let chain = ref None in
  let flush () =
    (match !chain with
    | None -> ()
    | Some (count, rev_entries) ->
      add_chain t ~binary ~count ~entries:(List.rev rev_entries));
    chain := None
  in
  let rec loop () =
    match In_channel.input_line ic with
    | None -> flush ()
    | Some line ->
      (match classify_line line with
      | Blank -> flush ()
      | Chain_header count ->
        flush ();
        chain := Some (count, [])
      | Sample (count, addr, dso) ->
        flush ();
        if dso_matches ~binary dso then add t ~stack:[addr] ~count
      | Address (addr, dso) -> (
        match !chain with
        | Some (count, rev_entries) ->
          chain := Some (count, (addr, dso) :: rev_entries)
        | None ->
          (* A sample of the legacy no-period form. *)
          if dso_matches ~binary dso then add t ~stack:[addr] ~count:1L));
      loop ()
  in
  loop ();
  t

let run_perf_script ~perf ~perf_data ~fields ~binary =
  let args =
    [| perf; "script"; "-i"; perf_data; "--no-demangle"; "-F"; fields |]
  in
  let ic = Unix.open_process_args_in perf args in
  let result =
    match of_channel ic ~binary with t -> Ok t | exception exn -> Error exn
  in
  let status = Unix.close_process_in ic in
  match status, result with
  (* A parse error is ours to report whatever perf exited with. *)
  | _, Error exn -> raise exn
  | Unix.WEXITED 0, Ok t -> Ok t
  | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), Ok _ -> Error status

let collect ~perf ~perf_data ~binary =
  match run_perf_script ~perf ~perf_data ~fields:"ip,dso,period" ~binary with
  | Ok t -> t
  | Error _ -> (
    (* Old perf versions reject "period" for some event types; retry with every
       sample weighing 1. *)
    match run_perf_script ~perf ~perf_data ~fields:"ip,dso" ~binary with
    | Ok t -> t
    | Error _ ->
      failwith
        (Printf.sprintf "'%s script -i %s' failed; is %s a perf profile?" perf
           perf_data perf_data))
