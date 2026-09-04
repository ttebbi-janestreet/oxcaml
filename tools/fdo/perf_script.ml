(* Extraction of raw samples from a Linux perf profile.

   The heavy lifting is done by "perf script". For a profile recorded with LBR
   branch stacks (perf record -j any,u), it is spawned with the field list
   "brstack,period" and each sample prints as a single line

   <period> <from>/<to>/<flags...> <from>/<to>/<flags...> ...

   with one entry per recorded taken branch (most recent first; the number and
   shape of the slash-separated flags varies with the perf version). Both
   addresses of every branch become one sample each: every executed basic block
   starts at some branch target and ends at some branch source, so the endpoints
   give precise block coverage without disassembling the executable (blocks only
   ever entered and left by falling through stay unsampled and are repaired by
   the consumer's flow conservation). Addresses outside the executable's own
   code sections are dropped.

   For a plain instruction profile the field list is "ip,dso,period" and each
   sample prints as a single line

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

   [collect] tries the field lists in the above order: perf rejects "brstack"
   when the profile has no branch stacks, and rejects "period" when it is too
   old for that field (in which case we weigh each sample as 1; in that legacy
   mode, call-chain entries are indistinguishable from samples, so profiles with
   call graphs should not be decoded with such a perf). *)

type branch_site =
  { address : int64;
    fallthrough_address : int64
  }

type branch_counts =
  { mutable taken : int64;
    mutable fallthrough : int64
  }

type t =
  { counts : (int64 list, int64) Hashtbl.t;
    (* leaf-first call stack (sampled address, then return addresses of callers)
       -> accumulated count *)
    mutable total : int64; (* sum of all counts, i.e. of in-target samples *)
    branches : (int64, branch_counts) Hashtbl.t
        (* per branch site of interest, the observed taken and fallthrough
           counts (only fed by branch-stack samples) *)
  }

let create () =
  { counts = Hashtbl.create 4096; total = 0L; branches = Hashtbl.create 256 }

let add t ~stack ~count =
  let existing = Option.value (Hashtbl.find_opt t.counts stack) ~default:0L in
  Hashtbl.replace t.counts stack (Int64.add existing count);
  t.total <- Int64.add t.total count

let bump_branch t address ~taken ~fallthrough =
  let counts =
    match Hashtbl.find_opt t.branches address with
    | Some counts -> counts
    | None ->
      let counts = { taken = 0L; fallthrough = 0L } in
      Hashtbl.add t.branches address counts;
      counts
  in
  counts.taken <- Int64.add counts.taken taken;
  counts.fallthrough <- Int64.add counts.fallthrough fallthrough

(* [lower_bound sites addr] is the first index whose site address is >= [addr]
   ([Array.length sites] if none), [sites] being sorted by address. *)
let lower_bound (sites : branch_site array) addr =
  let lo = ref 0 and hi = ref (Array.length sites) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if Int64.compare sites.(mid).address addr < 0
    then lo := mid + 1
    else hi := mid
  done;
  !lo

let find_site (sites : branch_site array) addr =
  let i = lower_bound sites addr in
  if i < Array.length sites && Int64.equal sites.(i).address addr
  then Some sites.(i)
  else None

(* One branch-stack sample: [branches] are the taken branches of the LBR record,
   most recent first, weighted [count] each.

   Both addresses of every branch become one position sample each: every
   executed basic block starts at some branch target and ends at some branch
   source, so the endpoints give block coverage without disassembly.

   Known branch sites additionally collect their outcome counts. A branch
   observed as an entry's source was executed, and fell through exactly when the
   entry's target is its fallthrough address (hardware LBR only records taken
   branches, so there it always counts as taken; the single-stepping emulator
   also records fallthroughs of conditional branches as entries). Moreover, the
   instructions between one entry's target and the next entry's source executed
   sequentially, so every branch site strictly inside that range was executed
   and fell through; those sites also become position samples (the range's first
   address already got one as a target endpoint). *)
let add_branch_stack t ~sites ~in_code ~count ~branches =
  List.iter
    (fun (source, target) ->
      if in_code source then add t ~stack:[source] ~count;
      if in_code target then add t ~stack:[target] ~count;
      match find_site sites source with
      | None -> ()
      | Some site ->
        if Int64.equal target site.fallthrough_address
        then bump_branch t source ~taken:0L ~fallthrough:count
        else bump_branch t source ~taken:count ~fallthrough:0L)
    branches;
  let rec ranges = function
    | (newer_source, _) :: ((_, older_target) :: _ as rest) ->
      (if Int64.compare older_target newer_source < 0
       then
         let i = ref (lower_bound sites older_target) in
         while
           !i < Array.length sites
           && Int64.compare sites.(!i).address newer_source < 0
         do
           let site = sites.(!i) in
           bump_branch t site.address ~taken:0L ~fallthrough:count;
           if Int64.compare site.address older_target > 0
           then add t ~stack:[site.address] ~count;
           incr i
         done);
      ranges rest
    | [_] | [] -> ()
  in
  ranges branches

type line =
  | Blank
  | Chain_header of int64 (* the period of a sample with a call chain *)
  | Sample of int64 * int64 * string (* count, address, dso *)
  | Address of int64 * string (* address, dso: chain entry or legacy sample *)
  | Branch_stack of int64 * (int64 * int64) list
(* count, then one (source, target) address pair per taken branch of the
   sample's LBR record *)

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
    | None ->
      if String.contains line '/'
      then
        (* A branch-stack sample: an optional period, then one
           "<from>/<to>/<flags...>" entry per taken branch. The addresses carry
           the "0x" prefix; the flags vary with the perf version and are
           ignored. *)
        let parse_entry token =
          match String.split_on_char '/' token with
          | source :: target :: _flags ->
            int64_of_token source, int64_of_token target
          | [_] | [] -> fail ()
        in
        let tokens =
          String.split_on_char ' ' line
          |> List.filter (fun token -> not (String.equal token ""))
        in
        match tokens with
        | first :: (_ :: _ as entries) when not (String.contains first '/') ->
          Branch_stack (int64_of_token first, List.map parse_entry entries)
        | entries -> Branch_stack (1L, List.map parse_entry entries)
      else Chain_header (int64_of_token line)

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

let of_channel ic ~binary ~code_bounds ~branch_sites =
  let t = create () in
  let in_code addr =
    match code_bounds with
    | None -> false
    | Some (lo, hi) -> Int64.compare addr lo >= 0 && Int64.compare addr hi < 0
  in
  let sites =
    let sites = Array.copy branch_sites in
    Array.sort (fun a b -> Int64.compare a.address b.address) sites;
    sites
  in
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
      | Branch_stack (count, branches) ->
        flush ();
        add_branch_stack t ~sites ~in_code ~count ~branches
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

let run_perf_script ~perf ~perf_data ~fields ~binary ~code_bounds ~branch_sites
    =
  let args =
    [| perf; "script"; "-i"; perf_data; "--no-demangle"; "-F"; fields |]
  in
  let ic = Unix.open_process_args_in perf args in
  let result =
    match of_channel ic ~binary ~code_bounds ~branch_sites with
    | t -> Ok t
    | exception exn -> Error exn
  in
  let status = Unix.close_process_in ic in
  match status, result with
  (* A parse error is ours to report whatever perf exited with. *)
  | _, Error exn -> raise exn
  | Unix.WEXITED 0, Ok t -> Ok t
  | (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _), Ok _ -> Error status

let collect ~perf ~perf_data ~binary ~code_bounds ~branch_sites =
  let run fields =
    run_perf_script ~perf ~perf_data ~fields ~binary ~code_bounds ~branch_sites
  in
  (* Branch stacks first (perf rejects "brstack" when the profile has none),
     then instruction samples, then the legacy form without periods. *)
  match run "brstack,period" with
  | Ok t -> t
  | Error _ -> (
    match run "ip,dso,period" with
    | Ok t -> t
    | Error _ -> (
      (* Old perf versions reject "period" for some event types; retry with
         every sample weighing 1. *)
      match run "ip,dso" with
      | Ok t -> t
      | Error _ ->
        failwith
          (Printf.sprintf "'%s script -i %s' failed; is %s a perf profile?" perf
             perf_data perf_data)))
