(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 module = "intrinsics.ml";
 flags = "-O3 -w -a";
 ocamlopt.opt;
 module = "hot_path_warning.ml";
 flags = "-O3";
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
let[@inline always] bind m k = k m

let let_star_bound_expr () =
  bind
    (not_inlinable () (* HOT: precedes the marker in the inlined continuation *);
     0)
    (fun _x -> hot_path_to_here 10.)

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
