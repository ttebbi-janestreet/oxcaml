(* TEST
 setup-ocamlopt.opt-build-env;
 flags = "-O3";
 ocamlopt_opt_exit_status = "2";
 ocamlopt.opt;
 check-ocamlopt.opt-output;
*)

external hot_path_to_here : float -> unit = "%hot_path_to_here" [@@noalloc]

(* The inlining-budget factor passed to [hot_path_to_here] must be a
   compile-time float literal. Passing a non-constant (here, a function
   parameter) is a compile error. *)
let mark_dynamically (factor : float) = hot_path_to_here factor
