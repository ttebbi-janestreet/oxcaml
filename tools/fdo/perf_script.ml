(* Extraction of taken branches and sequentially executed ranges from a Linux
   perf profile recorded with LBR branch stacks (perf record -j any,u), via
   "perf script -F period,ip,brstack". Each sample is printed as the period, the
   ip and the branch stack entries on one line, or, when the profile has call
   chains (perf record --call-graph), as the period alone on a line, then one
   tab-indented line per call chain frame (the ip first, then the return
   addresses up the stack), then the branch stack entries on a line. The ip and
   the frames are hexadecimal without prefix; an entry is <from>/<to>/<flags...>
   (the flags vary with the perf version and are ignored); entries are most
   recent first, one per recorded taken branch.

   Every entry says that a taken branch went from <from> to <to>; every pair of
   consecutive entries that the code from the older one's target up to the newer
   one's source (a range that may be that single branch instruction) executed
   sequentially; and, with [ip_ranges], the ip that the code from the most
   recent entry's target up to the ip did. That last inference needs the branch
   stack to have been frozen at the sampled instruction, as it is in the
   runtime's single-stepping emulator (which produces the same format) and with
   Intel's LBR; on AMD's the ip and the branch stack are not synchronized (the
   ip is often below the most recent target or megabytes above it), and ranges
   from the ip would attribute executions to whole swathes of code. The call
   chains are checked for shape but not used.

   Calls through the runtime's stubs (caml_applyN, caml_curryN, caml_sendN...)
   are seen through: the branch stack is consecutive, so when a call site (an
   address with call site metadata) lands on code without entry metadata, the
   call site stays pending, and the next branch landing on a function entry is
   recorded as a branch from the call site as well. A branch from a labelled
   conditional branch clears it (the stub returned without calling: a partial
   application being built); branches from unknown code (inside the stub, the C
   runtime) leave it alone. Consumers thus see the call site reaching the
   function the stub jumped to. The imprecision is small: calls into OCaml code
   compiled without labels get their callees' calls attributed to them, and a
   pending call site survives until the next labelled branch. *)

type t =
  { branches : (int64 * int64, int64) Hashtbl.t;
    ranges : (int64 * int64, int64) Hashtbl.t
  }

let add table key count =
  let existing = Option.value (Hashtbl.find_opt table key) ~default:0L in
  Hashtbl.replace table key (Int64.add existing count)

let add_sample t ~ip_ranges ~(metadata : Metadata.t) ~count ~ip ~branches =
  let range ~lo ~hi =
    if Int64.compare lo hi <= 0 then add t.ranges (lo, hi) count
  in
  (match ip, branches with
  | Some ip, (_, target) :: _ when ip_ranges -> range ~lo:target ~hi:ip
  | (Some _ | None), _ -> ());
  let rec go = function
    | (source, _) :: ((_, target) :: _ as older) ->
      range ~lo:target ~hi:source;
      go older
    | [_] | [] -> ()
  in
  go branches;
  (* Oldest branch first, so that calls through stubs can be seen through: the
     stub's own branch onto the entry is recorded as coming from the pending
     call site (its source, inside the stub, has no metadata to lose). *)
  let pending = ref None in
  List.iter
    (fun (source, target) ->
      let from_callsite = Hashtbl.mem metadata.callsite source in
      let to_entry = Hashtbl.mem metadata.target target in
      let branch =
        if from_callsite
        then (
          pending := if to_entry then None else Some source;
          source, target)
        else if to_entry
        then
          match !pending with
          | Some callsite ->
            pending := None;
            callsite, target
          | None -> source, target
        else (
          if Hashtbl.mem metadata.taken source then pending := None;
          source, target)
      in
      add t.branches branch count)
    (List.rev branches)

let of_channel ~ip_ranges ~metadata ic =
  let t = { branches = Hashtbl.create 4096; ranges = Hashtbl.create 4096 } in
  (* The period and ip of a sample whose branch stack is still to come (the call
     chain frames are in between). *)
  let pending = ref None in
  let rec loop () =
    match In_channel.input_line ic with
    | None -> ()
    | Some line ->
      let fail () = failwith (Printf.sprintf "cannot parse sample %S" line) in
      let int64 s =
        match Int64.of_string_opt s with Some n -> n | None -> fail ()
      in
      let hex s = int64 ("0x" ^ s) in
      let entry token =
        match String.split_on_char '/' token with
        | source :: target :: _ -> int64 source, int64 target
        | [_] | [] -> fail ()
      in
      let tokens =
        String.split_on_char ' ' (String.trim line)
        |> List.filter (fun s -> not (String.equal s ""))
      in
      let is_entry s = String.contains s '/' in
      (match tokens with
      | [] -> ()
      | [frame]
        when (not (is_entry frame)) && String.starts_with ~prefix:"\t" line -> (
        (* A call chain frame: the first one is the ip. *)
        match !pending with
        | None -> fail ()
        | Some (period, None) -> pending := Some (period, Some (hex frame))
        | Some (_, Some _) -> ignore (hex frame))
      | [period] when not (is_entry period) ->
        (* A sample with call chain: its frames and branch stack follow. (An old
           perf printing an instruction profile has no branch stacks at all.) *)
        pending := Some (int64 period, None)
      | first :: rest when not (is_entry first) ->
        let count = int64 first in
        let ip, branches =
          match rest with
          | ip :: rest when not (is_entry ip) -> Some (hex ip), rest
          | rest -> None, rest
        in
        pending := None;
        add_sample t ~ip_ranges ~metadata ~count ~ip
          ~branches:(List.map entry branches)
      | entries ->
        let count, ip =
          match !pending with
          | Some (period, ip) -> period, ip
          | None -> 1L, None
        in
        pending := None;
        add_sample t ~ip_ranges ~metadata ~count ~ip
          ~branches:(List.map entry entries));
      loop ()
  in
  loop ();
  t

let collect ~ip_ranges ~metadata ~perf_data =
  let args =
    [| "perf";
       "script";
       "-i";
       perf_data;
       "--no-demangle";
       "-F";
       "period,ip,brstack"
    |]
  in
  let ic = Unix.open_process_args_in "perf" args in
  let t = of_channel ~ip_ranges ~metadata ic in
  (match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
    failwith (Printf.sprintf "'perf script -i %s' failed" perf_data));
  if Hashtbl.length t.branches = 0
  then
    failwith
      (Printf.sprintf
         "%s has no branch stacks; record with perf record -j any,u" perf_data);
  t
