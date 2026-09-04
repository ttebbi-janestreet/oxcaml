(* Round-trip and validation tests for [Source_position_profile]. *)

module P = Source_position_profile

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.eprintf "FAILED: %s\n%!" name)

let check_count name actual expected =
  if not (Int64.equal actual expected)
  then (
    incr failures;
    Printf.eprintf "FAILED: %s: got %Ld, expected %Ld\n%!" name actual expected)

(* Golden values, computed independently (first 8 bytes of the MD5 of the frame
   string, read little-endian). They pin the hash function: any change to it
   silently invalidates every existing profile, so it must fail here. *)
let () =
  check "golden hash a.ml:1:0"
    (Int64.equal (P.hash_frame "a.ml:1:0") 0x1a667da7c0bfe443L);
  check "golden hash foo/bar.ml:42:17"
    (Int64.equal (P.hash_frame "foo/bar.ml:42:17") 0x2e6ec3f5b6bcad49L);
  check "golden hash x.ml:7:0"
    (Int64.equal (P.hash_frame "x.ml:7:0") 0xb8b575290ea7a750L)

let () =
  check "frame_string clamps negative column"
    (String.equal (P.frame_string ~file:"a.ml" ~line:1 ~col:(-1)) "a.ml:1:0");
  check "frame_string keeps column"
    (String.equal
       (P.frame_string ~file:"foo/bar.ml" ~line:42 ~col:17)
       "foo/bar.ml:42:17")

(* The forest under test: - leaf [a.ml:1:0]: 10 samples with no recorded context
   and 5 samples inlined from a call site at [b.ml:2:3]; - leaf [c.ml:9:1]: 7
   samples from [b.ml:2:3] which itself was inlined at [d.ml:4:0], added with
   [max_depth:2] so the deepest frame is dropped. *)
let leaf_a = "a.ml:1:0"

let ctx_b = "b.ml:2:3"

let leaf_c = "c.ml:9:1"

let ctx_d = "d.ml:4:0"

let make_writer () =
  let w = P.Writer.create () in
  P.Writer.add_stack w ~frames:[leaf_a] ~count:10L ~max_depth:16;
  P.Writer.add_stack w ~frames:[leaf_a; ctx_b] ~count:5L ~max_depth:16;
  P.Writer.add_stack w ~frames:[leaf_c; ctx_b; ctx_d] ~count:7L ~max_depth:2;
  P.Writer.add_stack w ~frames:[] ~count:100L ~max_depth:16;
  w

let check_queries name (p : P.t) =
  let check_count subname actual expected =
    check_count (name ^ ": " ^ subname) actual expected
  in
  check_count "root count sums all contexts"
    (P.count_for_frames p ~frames:[leaf_a])
    15L;
  check_count "context refines the count"
    (P.count_for_frames p ~frames:[leaf_a; ctx_b])
    5L;
  check_count "unrecorded context counts as 0"
    (P.count_for_frames p ~frames:[leaf_a; ctx_d])
    0L;
  check_count "frames below the recorded depth count as 0"
    (P.count_for_frames p ~frames:[leaf_c; ctx_b; ctx_d])
    0L;
  check_count "the recorded prefix of a truncated stack"
    (P.count_for_frames p ~frames:[leaf_c; ctx_b])
    7L;
  check_count "position sampled only as a context is not a root"
    (P.count_for_frames p ~frames:[ctx_b])
    0L;
  check_count "never-sampled position"
    (P.count_for_frames p ~frames:["z.ml:1:0"])
    0L;
  check_count "empty stack" (P.count_for_frames p ~frames:[]) 0L

let with_temp_file f =
  let filename = Filename.temp_file "source_position_profile" ".fdo" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () -> f filename)

let () =
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename ~buildid:(Some "abc123")
        ~total_samples:22L ~debug_map:true;
      let p = P.load ~filename in
      check "buildid" (Option.equal String.equal (P.buildid p) (Some "abc123"));
      check "total_samples" (Int64.equal (P.total_samples p) 22L);
      check_queries "file round-trip" p;
      check "debug map resolves a leaf"
        (Option.equal String.equal
           (P.position_of_hash p (P.hash_frame leaf_a))
           (Some leaf_a));
      check "debug map resolves a context frame"
        (Option.equal String.equal
           (P.position_of_hash p (P.hash_frame ctx_b))
           (Some ctx_b));
      check "frame truncated by max_depth is not in the debug map"
        (Option.is_none (P.position_of_hash p (P.hash_frame ctx_d)));
      check "debug map misses unknown hash"
        (Option.is_none (P.position_of_hash p 0L)))

let () =
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename ~buildid:None ~total_samples:22L
        ~debug_map:false;
      let p = P.load ~filename in
      check "no buildid" (Option.is_none (P.buildid p));
      check_queries "no debug map" p;
      check "absent debug map"
        (Option.is_none (P.position_of_hash p (P.hash_frame leaf_a))))

let () =
  let p =
    P.Writer.to_profile (make_writer ()) ~buildid:None ~total_samples:22L
      ~debug_map:true
  in
  check_queries "in-memory profile" p;
  check "in-memory debug map"
    (Option.equal String.equal
       (P.position_of_hash p (P.hash_frame ctx_b))
       (Some ctx_b))

(* [load] goes through the registered mapper when there is one (the tests below
   keep using this one). *)
let () =
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename ~buildid:(Some "abc123")
        ~total_samples:22L ~debug_map:true;
      let mapped = ref false in
      P.register_mmap (fun filename ->
          mapped := true;
          let contents =
            In_channel.with_open_bin filename In_channel.input_all
          in
          Bigarray.Array1.init Bigarray.char Bigarray.c_layout
            (String.length contents) (String.get contents));
      let p = P.load ~filename in
      check "registered mapper used" !mapped;
      check "mapped buildid"
        (Option.equal String.equal (P.buildid p) (Some "abc123"));
      check_queries "mapped round-trip" p;
      check "mapped debug map"
        (Option.equal String.equal
           (P.position_of_hash p (P.hash_frame leaf_a))
           (Some leaf_a)))

let () =
  with_temp_file (fun filename ->
      P.Writer.write (P.Writer.create ()) ~filename ~buildid:None
        ~total_samples:0L ~debug_map:false;
      let p = P.load ~filename in
      check "empty profile total" (Int64.equal (P.total_samples p) 0L);
      check_count "empty profile query"
        (P.count_for_frames p ~frames:[leaf_a])
        0L)

(* [count_for_debuginfo]: build real [Debuginfo.t] values and check they query
   the same trie. *)
let () =
  let location ~file ~line ~col =
    let pos_start =
      { Lexing.pos_fname = file;
        pos_lnum = line;
        pos_bol = 100;
        pos_cnum = 100 + col
      }
    in
    let pos_end = { pos_start with pos_cnum = pos_start.pos_cnum + 1 } in
    { Location.loc_start = pos_start; loc_end = pos_end; loc_ghost = false }
  in
  let debuginfo ~file ~line ~col =
    Debuginfo.from_location
      (Debuginfo.Scoped_location.of_location
         ~scopes:Debuginfo.Scoped_location.empty_scopes
         (location ~file ~line ~col))
  in
  let p =
    P.Writer.to_profile (make_writer ()) ~buildid:None ~total_samples:22L
      ~debug_map:false
  in
  let dbg_a = debuginfo ~file:"a.ml" ~line:1 ~col:0 in
  let dbg_b = debuginfo ~file:"b.ml" ~line:2 ~col:3 in
  check_count "debuginfo leaf" (P.count_for_debuginfo p dbg_a) 15L;
  (* [inline] appends the inlined body's frames after the caller's, so the
     leaf-first stack is [a.ml:1:0; b.ml:2:3]. *)
  let inlined = Debuginfo.inline dbg_b ~from_inlined_body:dbg_a in
  check_count "debuginfo inlined stack" (P.count_for_debuginfo p inlined) 5L;
  check_count "empty debuginfo" (P.count_for_debuginfo p Debuginfo.none) 0L;
  (* The location stack of a pseudo-instrumentation label: the edge string of
     its anchor and tree path (the edge index last), then the call sites it was
     inlined through, innermost first. Labels need no source position. *)
  let labels =
    Debuginfo.create_edge_labels ~anchor:leaf_a ~path:[0; -2] ~num_edges:2
  in
  let stack_of_edge position labels =
    match labels with
    | Debuginfo.Positional sets -> (
      match sets.(position) with
      | [label] -> P.frames_of_branch_label label
      | _ -> [])
    | Debuginfo.Resolved _ -> []
  in
  check "label stack of edge 0"
    (List.equal String.equal (stack_of_edge 0 labels) ["a.ml:1:0@0.-2.0"]);
  check "label stack of edge 1"
    (List.equal String.equal (stack_of_edge 1 labels) ["a.ml:1:0@0.-2.1"]);
  check "unit anchor" (String.equal (P.unit_anchor "Foo") "<Foo>");
  check "nested anchor"
    (String.equal
       (P.nested_anchor ~anchor:leaf_a ~path:[-1; 2])
       "a.ml:1:0@-1.2");
  let carrier = Debuginfo.with_edge_labels Debuginfo.none labels in
  check "labels ride on debug info without a position"
    (Option.is_some (Debuginfo.edge_labels carrier)
    && not (Debuginfo.is_none carrier));
  (* Inlining records the call site; inlining the result again prepends the
     outer call site, so the stack lists call sites innermost first. *)
  let dbg_d = debuginfo ~file:"d.ml" ~line:4 ~col:0 in
  let inlined_once = Debuginfo.inline dbg_b ~from_inlined_body:carrier in
  let inlined_twice = Debuginfo.inline dbg_d ~from_inlined_body:inlined_once in
  let check_inlined name dbg expected =
    match Debuginfo.edge_labels dbg with
    | Some labels ->
      check name (List.equal String.equal (stack_of_edge 0 labels) expected)
    | None -> check (name ^ " (labels kept)") false
  in
  check_inlined "inlined label stack" inlined_once ["a.ml:1:0@0.-2.0"; ctx_b];
  check_inlined "twice inlined label stack" inlined_twice
    ["a.ml:1:0@0.-2.0"; ctx_b; ctx_d]

(* Edges are locations like any other: the edge string of the anchor and tree
   path heads the stack, the inlining call sites follow. *)
let () =
  let edge_a edge = P.edge_string ~anchor:leaf_a ~path:[edge] in
  check "edge string" (String.equal (edge_a 3) "a.ml:1:0@3");
  let make_writer () =
    let w = make_writer () in
    (* A construct at the root of the function anchored at [leaf_a], with edges
       3 and 4: edge 3 was recorded in two contexts (once inlined at
       [ctx_b]). *)
    P.Writer.add_stack w ~frames:[edge_a 3] ~count:40L ~max_depth:16;
    P.Writer.add_stack w ~frames:[edge_a 4] ~count:20L ~max_depth:16;
    P.Writer.add_stack w ~frames:[edge_a 3; ctx_b] ~count:10L ~max_depth:16;
    w
  in
  let check_queries name p =
    let check_count subname actual expected =
      check_count (name ^ ": " ^ subname) actual expected
    in
    check_count "edge aggregates all contexts"
      (P.count_for_frames p ~frames:[edge_a 3])
      50L;
    check_count "the other edge" (P.count_for_frames p ~frames:[edge_a 4]) 20L;
    check_count "context refines the edge"
      (P.count_for_frames p ~frames:[edge_a 3; ctx_b])
      10L;
    check_count "unrecorded context"
      (P.count_for_frames p ~frames:[edge_a 3; ctx_d])
      0L;
    check_count "unrecorded edge" (P.count_for_frames p ~frames:[edge_a 9]) 0L;
    check_count "position samples are untouched"
      (P.count_for_frames p ~frames:[leaf_a])
      15L
  in
  check_count "hashed stacks hit the same trie nodes"
    (let w = make_writer () in
     P.Writer.add_hashed_stack w
       ~hashes:[P.hash_frame (edge_a 3); P.hash_frame ctx_b]
       ~count:5L ~max_depth:16;
     let p =
       P.Writer.to_profile w ~buildid:None ~total_samples:22L ~debug_map:false
     in
     P.count_for_frames p ~frames:[edge_a 3; ctx_b])
    15L;
  check_queries "in-memory edges"
    (P.Writer.to_profile (make_writer ()) ~buildid:None ~total_samples:22L
       ~debug_map:false);
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename ~buildid:None ~total_samples:22L
        ~debug_map:false;
      check_queries "edges round-trip" (P.load ~filename))

(* Strict validation of malformed files. *)
let () =
  let read_file filename =
    In_channel.with_open_bin filename In_channel.input_all
  in
  let write_file filename contents =
    Out_channel.with_open_bin filename (fun oc ->
        Out_channel.output_string oc contents)
  in
  (* Validation is lazy: loading only checks the header and the root index, so a
     malformed trie may only be rejected once it is read. A full [iter] reads
     (and validates) everything. *)
  let expect_error name contents =
    with_temp_file (fun filename ->
        write_file filename contents;
        match
          let p = P.load ~filename in
          P.iter p ~f:(fun ~hash:_ ~depth:_ ~count:_ -> ());
          ignore (P.position_of_hash p 0L)
        with
        | () ->
          incr failures;
          Printf.eprintf "FAILED: %s: no error raised\n%!" name
        | exception P.Error _ -> ())
  in
  let good =
    with_temp_file (fun filename ->
        P.Writer.write (make_writer ()) ~filename ~buildid:(Some "abc123")
          ~total_samples:22L ~debug_map:true;
        read_file filename)
  in
  let magic_len = String.length P.magic_number in
  expect_error "empty file" "";
  expect_error "truncated magic" (String.sub good 0 (magic_len - 1));
  expect_error "truncated header" (String.sub good 0 (magic_len + 3));
  expect_error "truncated body" (String.sub good 0 (String.length good - 1));
  expect_error "trailing bytes" (good ^ "x");
  expect_error "wrong magic" ("X" ^ String.sub good 1 (String.length good - 1));
  let wrong_version = Bytes.of_string good in
  Bytes.set wrong_version (magic_len - 1) '\255';
  expect_error "wrong version" (Bytes.to_string wrong_version)

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
