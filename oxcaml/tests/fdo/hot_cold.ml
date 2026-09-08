(* Positions in this file are referenced by line and column from
   hot_cold_profile.txt; keep both in sync when editing. This file is
   excluded from ocamlformat (.ocamlformat-ignore) so that formatting
   changes cannot silently move the referenced positions. *)

let[@inline never] hot () = print_string "hot"

let[@inline never] cold () = print_string "cold"

let[@inline never] f b =
  if b
  then hot ()
  else cold ()

let () = f (Array.length Sys.argv > 1)
