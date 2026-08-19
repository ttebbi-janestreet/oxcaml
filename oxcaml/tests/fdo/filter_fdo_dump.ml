(* Filters a -dcfg dump down to the layouts printed by the FDO reorder pass: the
   function name and block order of each "After cfg_fdo_layout" section. *)

let () =
  let ic = open_in Sys.argv.(1) in
  let in_section = ref false in
  (try
     while true do
       let line = input_line ic in
       if String.starts_with ~prefix:"*** FDO block frequencies" line
       then (
         in_section := false;
         print_endline line)
       else if
         (String.starts_with ~prefix:"  block " line
         || String.starts_with ~prefix:"  edge " line
         || String.starts_with ~prefix:"  chain:" line)
         && not !in_section
       then print_endline line
       else if String.starts_with ~prefix:"  function:" line
       then print_endline line
       else if String.starts_with ~prefix:"*** " line
       then in_section := String.equal line "*** After cfg_fdo_layout"
       else if !in_section
       then
         if String.starts_with ~prefix:"cfg for " line
         then print_endline line
         else if String.starts_with ~prefix:"block " line
         then
           match String.split_on_char ' ' line with
           | "block" :: label :: _ -> Printf.printf "  block %s\n" label
           | _ -> failwith ("cannot parse block line: " ^ line)
     done
   with End_of_file -> ());
  close_in ic
