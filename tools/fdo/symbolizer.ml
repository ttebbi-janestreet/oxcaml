(* Decoding of code addresses into inlining stacks of source positions, by
   driving an external symbolizer (llvm-symbolizer by default).

   The symbolizer is asked for --inlines (one frame per inlining level,
   most-inlined first) and --relativenames (report the file exactly as recorded
   in the DWARF line table, without joining the compilation directory). The
   latter is essential: the compiler hashes [dinfo_file] verbatim, and the raw
   line-table file string is that same string.

   With --verbose, the output for each queried address is one block per inlining
   level (most-inlined first) -- a function-name line followed by indented "Key:
   value" attribute lines -- terminated by an empty line. Of the attributes,
   only Filename, Line, Column and Discriminator are used (the symbolizer prints
   the discriminator only when it is nonzero). Unknown addresses produce a
   single block whose Filename is "??". *)

type frame =
  { file : string;
    line : int;
    col : int;
    discriminator : int
  }

(* Parse the output lines into one (possibly empty) leaf-first stack per queried
   address. *)
let parse_output lines =
  let fail fmt = Printf.ksprintf failwith fmt in
  let stacks = ref [] in
  (* The frames of the current address so far, in reverse. *)
  let frames = ref [] in
  (* The frame block being accumulated, if any. *)
  let current = ref None in
  let finish_frame () =
    match !current with
    | None -> ()
    | Some frame ->
      (* An address the symbolizer knows nothing about produces a "??" frame;
         represent it as an empty stack. *)
      if not (String.equal frame.file "??") then frames := frame :: !frames;
      current := None
  in
  let finish_address () =
    finish_frame ();
    stacks := List.rev !frames :: !stacks;
    frames := []
  in
  List.iter
    (fun line ->
      if String.equal line ""
      then finish_address ()
      else if String.starts_with ~prefix:"  " line
      then
        (* An attribute of the current frame. Keys contain no colon, so the
           first colon is the separator (the value, e.g. a filename, may contain
           more). *)
        let colon =
          match String.index_opt line ':' with
          | Some colon -> colon
          | None -> fail "cannot parse symbolizer attribute %S" line
        in
        let key = String.trim (String.sub line 0 colon) in
        let value =
          String.trim
            (String.sub line (colon + 1) (String.length line - colon - 1))
        in
        let int_value () =
          match int_of_string_opt value with
          | Some n -> n
          | None -> fail "cannot parse symbolizer attribute %S" line
        in
        match !current with
        | None -> fail "symbolizer attribute %S outside a frame" line
        | Some frame ->
          let frame =
            match key with
            | "Filename" -> { frame with file = value }
            | "Line" -> { frame with line = int_value () }
            | "Column" -> { frame with col = int_value () }
            | "Discriminator" -> { frame with discriminator = int_value () }
            | _ -> frame (* e.g. "Function start line" *)
          in
          current := Some frame
      else (
        (* A function-name line starts the next inlining level. *)
        finish_frame ();
        current := Some { file = "??"; line = 0; col = 0; discriminator = 0 }))
    lines;
  (* Robustness against output without a trailing blank line. *)
  if Option.is_some !current then finish_address ();
  List.rev !stacks

let read_all_lines ic =
  let rec loop acc =
    match In_channel.input_line ic with
    | None -> List.rev acc
    | Some line -> loop (line :: acc)
  in
  loop []

(* The batches only bound the memory of the parsed answers; the symbolizer
   reparses the binary's DWARF on every spawn, so they should be large. *)
let batch_size = 65536

(* The symbolizer is driven through temporary files rather than pipes: its
   per-address output is large, so a write-everything-then-read-everything pipe
   protocol deadlocks once the symbolizer fills its output pipe and stops
   consuming the request. *)
let run_batch ~symbolizer ~binary addrs =
  let queries = Filename.temp_file "fdo_decode" ".addrs" in
  let answers = Filename.temp_file "fdo_decode" ".sym" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove queries with Sys_error _ -> ());
      try Sys.remove answers with Sys_error _ -> ())
    (fun () ->
      Out_channel.with_open_text queries (fun oc ->
          List.iter (fun addr -> Printf.fprintf oc "0x%Lx\n" addr) addrs);
      let command =
        Filename.quote_command symbolizer ~stdin:queries ~stdout:answers
          ["--inlines"; "--relativenames"; "--verbose"; "-e"; binary]
      in
      if Sys.command command <> 0
      then failwith (Printf.sprintf "'%s' failed" symbolizer);
      let lines = In_channel.with_open_text answers read_all_lines in
      let stacks = parse_output lines in
      if List.compare_lengths stacks addrs <> 0
      then
        failwith
          (Printf.sprintf "'%s' returned %d answers for %d addresses" symbolizer
             (List.length stacks) (List.length addrs));
      stacks)

(* [symbolize ~symbolizer ~binary ~addrs] is the leaf-first inlining stack of
   each address in [addrs], in order. An empty stack means the symbolizer knows
   nothing about that address. *)
let symbolize ~symbolizer ~binary ~addrs =
  let rec batches = function
    | [] -> []
    | addrs ->
      let batch, rest =
        if List.compare_length_with addrs batch_size <= 0
        then addrs, []
        else
          let rec split n acc = function
            | rest when n = 0 -> List.rev acc, rest
            | [] -> List.rev acc, []
            | addr :: rest -> split (n - 1) (addr :: acc) rest
          in
          split batch_size [] addrs
      in
      run_batch ~symbolizer ~binary batch :: batches rest
  in
  List.concat (batches addrs)
