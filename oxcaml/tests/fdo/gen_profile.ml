(* Builds a source-position FDO profile from a text description, for tests. Each
   non-empty line is "<count> <frame> <frame> ...", the frames being canonical
   position strings ("file:line:col"), leaf (most-inlined) first. Lines starting
   with '#' are comments. *)

let () =
  match Sys.argv with
  | [| _; input; output |] ->
    let w = Source_position_profile.Writer.create () in
    let total = ref 0L in
    In_channel.with_open_text input (fun ic ->
        let rec loop () =
          match In_channel.input_line ic with
          | None -> ()
          | Some line ->
            let line = String.trim line in
            (if not (String.equal line "" || String.starts_with ~prefix:"#" line)
             then
               match String.split_on_char ' ' line with
               | count :: (_ :: _ as frames) ->
                 let count = Int64.of_string count in
                 total := Int64.add !total count;
                 Source_position_profile.Writer.add_stack w ~frames ~count
                   ~max_depth:16
               | _ -> failwith ("bad profile line: " ^ line));
            loop ()
        in
        loop ());
    Source_position_profile.Writer.write w ~filename:output ~buildid:None
      ~total_samples:!total ~kind:Instructions ~debug_map:true
  | _ ->
    prerr_endline "usage: gen_profile <input.txt> <output.fdo>";
    exit 2
