(* Decoding of code addresses into inlining stacks of source positions, by
   driving an external symbolizer (llvm-symbolizer by default).

   The symbolizer is asked for --inlines (one frame per inlining level,
   most-inlined first) and --relativenames (report the file exactly as recorded
   in the DWARF line table, without joining the compilation directory). The
   latter is essential: the compiler hashes [dinfo_file] verbatim, and the raw
   line-table file string is that same string.

   With --output-style=LLVM, the output for each queried address is a block of
   (function name, "file:line:col") line pairs, most-inlined first, terminated
   by an empty line. Unknown addresses produce a single "??" frame. *)

type frame =
  { file : string;
    line : int;
    col : int
  }

(* Split "file:line:col" from the right: the file may itself contain colons. *)
let parse_location str =
  let fail () = failwith (Printf.sprintf "cannot parse location %S" str) in
  let int_of_suffix str =
    match int_of_string_opt str with Some n -> n | None -> fail ()
  in
  match String.rindex_opt str ':' with
  | None -> fail ()
  | Some second_colon -> (
    let col =
      int_of_suffix
        (String.sub str (second_colon + 1)
           (String.length str - second_colon - 1))
    in
    match String.rindex_from_opt str (second_colon - 1) ':' with
    | None -> fail ()
    | Some first_colon ->
      let line =
        int_of_suffix
          (String.sub str (first_colon + 1) (second_colon - first_colon - 1))
      in
      let file = String.sub str 0 first_colon in
      { file; line; col })

(* Parse the output lines into one (possibly empty) leaf-first stack per queried
   address. *)
let parse_output lines =
  let finish_stack frames = List.rev frames in
  let rec loop stacks frames = function
    | [] -> List.rev stacks
    | "" :: rest -> loop (finish_stack frames :: stacks) [] rest
    | function_name :: location :: rest ->
      if String.equal function_name ""
      then failwith "unexpected empty function name from symbolizer";
      let frames =
        (* An unknown address symbolizes to a "??" frame; represent it as an
           empty stack. *)
        if String.starts_with ~prefix:"??" location
        then frames
        else parse_location location :: frames
      in
      loop stacks frames rest
    | [_] -> failwith "truncated symbolizer output"
  in
  loop [] [] lines

let read_all_lines ic =
  let rec loop acc =
    match In_channel.input_line ic with
    | None -> List.rev acc
    | Some line -> loop (line :: acc)
  in
  loop []

(* Keep each request batch smaller than a pipe buffer (64 KiB): the whole batch
   is written before any output is read, which cannot deadlock as long as the
   request fits in the pipe. *)
let batch_size = 2048

let run_batch ~symbolizer ~binary addrs =
  let args =
    [| symbolizer;
       "--inlines";
       "--relativenames";
       "--output-style=LLVM";
       "-e";
       binary
    |]
  in
  let ic, oc = Unix.open_process_args symbolizer args in
  List.iter (fun addr -> Printf.fprintf oc "0x%Lx\n" addr) addrs;
  Out_channel.close oc;
  let lines = read_all_lines ic in
  (match Unix.close_process (ic, oc) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
    failwith (Printf.sprintf "'%s' failed" symbolizer));
  let stacks = parse_output lines in
  if List.compare_lengths stacks addrs <> 0
  then
    failwith
      (Printf.sprintf "'%s' returned %d answers for %d addresses" symbolizer
         (List.length stacks) (List.length addrs));
  stacks

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
