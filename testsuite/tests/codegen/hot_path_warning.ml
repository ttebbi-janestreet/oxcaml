(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 module = "intrinsics.ml";
 flags = "-O3 -w -a";
 ocamlopt.opt;
 module = "hot_path_warning.ml";
 flags = "-O3 -flambda2-match-in-match";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

(* This test exercises the hot-path inference (does a call site get marked as
   "on a hot path"?) as a side effect of the inlining-impossible warning.

   The probe: [Intrinsics.not_inlinable] is a cross-unit [@inline never] function
   whose code is not exported, so a direct call to it has the inlining decision
   [Missing_code]. The compiler emits Warning 55 for such a call IFF the call
   site is inferred to be on a hot path. [hot_path_to_here ()] is the marker
   meaning "all code that can reach here is hot" (propagated backwards). So each
   [not_inlinable ()] below fires the warning exactly when the inference marks it
   hot; the reference file records precisely which calls are hot.

   [Sys.opaque_identity] is used on branch conditions/bounds so the optimizer
   cannot fold a branch away and delete a probe. *)

open Intrinsics

(* ===== sequences: hotness propagates strictly backwards ===== *)

(* No marker anywhere: every call is cold. *)
let plain_cold c =
  ignore (Sys.opaque_identity c);
  not_inlinable () (* cold *)

(* A call before the marker is hot; a call after it is cold. *)
let seq_before_after c =
  not_inlinable () (* HOT *);
  hot_path_to_here 10.;
  ignore (Sys.opaque_identity c);
  not_inlinable () (* cold *)

(* The defining expression of a [let] is hot when the let body reaches a
   marker. *)
let let_rhs_before_marker c =
  let x =
    not_inlinable () (* HOT *);
    Sys.opaque_identity c
  in
  hot_path_to_here 10.;
  x

(* Contrast: same shape, no marker, so the RHS call is cold. *)
let let_rhs_no_marker c =
  let x =
    not_inlinable () (* cold *);
    Sys.opaque_identity c
  in
  x

(* ===== branches: no leak across arms ===== *)

(* A call before an [if] whose continuation reaches a marker is hot. *)
let before_if_cont_hot c =
  not_inlinable () (* HOT *);
  (if Sys.opaque_identity c
   then not_inlinable () (* HOT *)
   else not_inlinable () (* HOT *););
  hot_path_to_here 10.

(* Marker in the [then] branch: the [then] call (before it) is hot, the [else]
   call is cold -- hotness does not leak between sibling branches. *)
let if_branch_marker c =
  if Sys.opaque_identity c
  then (
    not_inlinable () (* HOT *);
    hot_path_to_here 10.)
  else not_inlinable () (* cold *)

(* Same for [match] arms. *)
let match_arm_marker n =
  match Sys.opaque_identity n with
  | 0 ->
    not_inlinable () (* HOT *);
    hot_path_to_here 10.
  | 1 -> not_inlinable () (* cold *)
  | _ -> ()

(* ===== loops ===== *)

(* A call in a loop body before a marker is hot. *)
let while_body_hot n =
  let i = ref 0 in
  while Sys.opaque_identity !i < n do
    not_inlinable () (* HOT *);
    incr i
   done;
   hot_path_to_here 10.

let for_body_hot n =
  for _i = 0 to Sys.opaque_identity n do
    not_inlinable () (* HOT *);
    hot_path_to_here 10.
  done

(* A call after the marker in a loop body is hot: the loop back-edge means a
   later iteration reaches the marker again. The flow analysis follows the
   back-edge in the control flow graph, so this is handled precisely. *)
let while_body_after_marker n =
  let i = ref 0 in
  while Sys.opaque_identity !i < n do
    hot_path_to_here 10.;
    not_inlinable () (* HOT (reaches the marker on the next iteration) *);
    incr i
  done

(* ===== try / with ===== *)

(* A call in the try body before a marker is hot. *)
let try_body_marker c =
  (try
     not_inlinable () (* HOT *)
   with _ -> ());
   hot_path_to_here 10.

(* Marker in the handler: the handler call (before it) is hot. The body call is
   hot too -- raising from the body transfers control to the handler, so the
   body can reach the marker. *)
let try_handler_hot c =
  ignore (Sys.opaque_identity c);
  try not_inlinable () (* HOT (can raise into the hot handler) *)
  with _ ->
    not_inlinable () (* HOT *);
    hot_path_to_here 10.

(* ===== into inlined functions: the cascade ===== *)

(* A hot call to an [@inline always] wrapper makes the calls inside the inlined
   body hot too, even though the wrapper has no marker of its own. *)
let cascade_hot () =
  let[@inline always] w () = not_inlinable () (* HOT via cascade *) in
  w ();
  hot_path_to_here 10.

(* Contrast: the same wrapper inlined in a cold context stays cold. *)
let cascade_cold () =
  let[@inline always] w () = not_inlinable () (* cold *) in
  w ()

(* The cascade persists through nested inlines and cold intermediate wrappers. *)
let nested_cascade_hot () =
  let[@inline always] inner () = not_inlinable () (* HOT via cascade *) in
  let[@inline always] mid () = inner () in
  let[@inline always] outer () = mid () in
  outer ();
  hot_path_to_here 10.

(* The cascade marks the whole inlined body hot, including its branches. *)
let cascade_branch c =
  let[@inline always] w () =
    if Sys.opaque_identity c then not_inlinable () (* HOT via cascade *) else ()
  in
  w ();
  hot_path_to_here 10.

(* ===== marker inside an inlined function (the let*-style case) ===== *)

(* A marker that lives inside an inlined function makes the caller's pre-call
   code hot. The marker is hidden behind the [Lfunction] boundary up front, so
   the call is cold on the first pass; but once [mark] is inlined the marker is
   revealed in the caller's body, the flow analysis sees that the call's
   continuation reaches it, and re-simplification (via the [resimplify] flag)
   gives the call the hot-path budget. *)
let marker_in_inlined_callee () =
  let[@inline always] mark () = hot_path_to_here 10. in
  not_inlinable () (* HOT (revealed after [mark] is inlined) *);
  mark ()

(* The same thing through a [let*]-style [bind]: the marker lives in the
   continuation closure, which is inlined into [bind]; the bound expression then
   precedes the revealed marker and is hot. This is the motivating case for
   monadic-let code. *)
let[@inline always] bind m k = match m with None -> None | Some m -> k m

let let_star_bound_expr x =
  bind (
    if x > 0
    then (
      not_inlinable (); (* HOT: triggers continuation *)
      Some 5)
    else (
      not_inlinable (); (* cold: does not trigger continuation *)
      None)
  ) (fun _x -> hot_path_to_here 10.; None)

(* ===== tail recursion turned into a loop ===== *)

let rec tail_loop_1 n =
  let rec loop i =
    if Sys.opaque_identity i > 0
    then begin
      hot_path_to_here 10.;
      not_inlinable () (* HOT *);
      loop (i - 1)
    end
    else not_inlinable () (* cold *)
  in
  not_inlinable () (* HOT *);
  loop n

let tail_loop_2 n =
  let rec loop i =
    if Sys.opaque_identity i > 0
    then begin
      not_inlinable () (* HOT *);
      loop (i - 1)
    end
    else not_inlinable () (* HOT *)
  in
  loop n;
  hot_path_to_here 10.

(* ===== forward coldness ===== *)

(* Code following a call to a [@cold] function is cold. Coldness is accurate (a
   merge is cold only if every predecessor is cold) and so beats hotness when
   they meet: a call that both reaches a marker and follows a [@cold] call is
   cold, hence not warned about. *)

(* A non-empty [@cold] function: [@cold] implies [@inline never], so the call is
   not inlined; the code after it is cold. *)
let[@cold] cold_fn () = ignore (Sys.opaque_identity 0)

(* An empty [@cold] [@inline always] marker: it is inlined like any function,
   and inlining a [@cold] function leaves a (code-free) cold marker -- so the
   call generates no code but still marks the following code as cold. *)
let[@cold] [@inline always] mark_cold () = ()

(* Cold beats hot: the probe reaches a marker (so the backward analysis would
   mark it hot) but it follows a [@cold] call, so it is cold. *)
let cold_beats_hot () =
  cold_fn ();
  not_inlinable () (* cold (beats hot) *);
  hot_path_to_here 10.

(* The empty marker is inlined away, yet the following probe is still cold. *)
let empty_marker_propagates () =
  mark_cold ();
  not_inlinable () (* cold (beats hot) *);
  hot_path_to_here 10.

(* A probe *before* an inlined marker is cold too: a continuation containing a
   cold marker is fully cold, so hotness does not propagate back through it. *)
let probe_before_marker () =
  not_inlinable () (* cold (precedes a cold marker) *);
  mark_cold ();
  hot_path_to_here 10.

(* The inlined-marker version of [merge_one_cold]: only the [then] path is cold
   (it contains the marker), so the merge stays hot, but the [then] probe before
   the marker is cold. *)
let merge_one_cold_marker c =
  (if c
   then (
     not_inlinable () (* cold (precedes a cold marker) *);
     mark_cold ())
   else not_inlinable () (* HOT (hot propagates to all predecessors) *));
  not_inlinable () (* HOT (merge not all-cold) *);
  hot_path_to_here 10.

(* AND-merge: only the [then] path is cold, so the merge has a non-cold
   predecessor and is therefore not cold -- the probe stays hot. *)
let merge_one_cold c =
  (if c
    then (not_inlinable () (* cold (cold stops hot from propagating) *);
          cold_fn ())
    else not_inlinable () (* HOT (hot propagates to all predecessors) *));
  not_inlinable () (* HOT (merge not all-cold) *);
  hot_path_to_here 10.

(* AND-merge: both paths into the merge are cold, so the merge is cold and the
   probe is cold. *)
let merge_both_cold c =
  (if c then cold_fn () else cold_fn ());
  not_inlinable () (* cold (beats hot) *);
  hot_path_to_here 10.

(* The cold region ends at a merge with a hot sibling: the in-region probe is
   cold, the post-merge probe is hot. *)
let cold_then_merge c =
  (if c
   then (
     cold_fn ();
     not_inlinable () (* cold *))
   else not_inlinable () (* HOT *));
  hot_path_to_here 10.

(* A loop entered from a cold dominator is cold. The probe in the loop body
   reaches the marker (after the loop) on the back-edge, so the backward analysis
   would mark it hot; but the recursive continuation's environment is
   approximated by its fork, which follows the [@cold] call and is therefore
   cold, so the whole loop is cold. *)
let cold_loop n =
  cold_fn ();
  let rec loop i =
    if Sys.opaque_identity i > 0
    then (
      not_inlinable () (* cold (loop dominated by a cold call) *);
      loop (i - 1))
    else ()
  in
  loop n;
  hot_path_to_here 10.

(* Contrast: the same loop with no preceding cold call has a hot fork, so its
   body probe is hot. *)
let hot_loop n =
  let rec loop i =
    if Sys.opaque_identity i > 0
    then (
      not_inlinable () (* HOT *);
      if Sys.opaque_identity i > 10 then cold_fn();
      loop (i - 1))
    else ()
  in
  loop n;
  hot_path_to_here 10.
