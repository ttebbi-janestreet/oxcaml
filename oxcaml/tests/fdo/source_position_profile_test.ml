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

(* Golden values, computed independently (first 8 bytes of the MD5 of the
   level's components joined with NUL bytes, read little-endian). They pin the
   hash function: any change to it silently invalidates every existing profile,
   so it must fail here. *)
let () =
  check "golden hash a.ml:1:0"
    (Int64.equal
       (Fdo_location.hash_level ["a.ml"; "1"; "0"])
       0xad44b602922f1cb9L);
  check "golden hash foo/bar.ml:42:17"
    (Int64.equal
       (Fdo_location.hash_level ["foo/bar.ml"; "42"; "17"])
       0x79d55eb68d66fb15L);
  check "golden hash <Foo>:3"
    (Int64.equal (Fdo_location.hash_level ["<Foo>"; "3"]) 0x930e43b39a419800L);
  check "level string"
    (String.equal
       (Fdo_location.level_string ["foo/bar.ml"; "42"; "17"])
       "foo/bar.ml:42:17")

(* The forest under test: - leaf [a.ml:1:0]: 10 samples with no recorded context
   and 5 samples inlined from a call site at [b.ml:2:3]; - leaf [c.ml:9:1]: 7
   samples from [b.ml:2:3] which itself was inlined at [d.ml:4:0]. *)
let leaf_a = ["a.ml"; "1"; "0"]

let ctx_b = ["b.ml"; "2"; "3"]

let leaf_c = ["c.ml"; "9"; "1"]

let ctx_d = ["d.ml"; "4"; "0"]

let make_writer () =
  let w = P.Writer.create () in
  P.Writer.add_location w ~location:[leaf_a] ~count:10L;
  P.Writer.add_location w ~location:[leaf_a; ctx_b] ~count:5L;
  P.Writer.add_location w ~location:[leaf_c; ctx_b; ctx_d] ~count:7L;
  P.Writer.add_location w ~location:[] ~count:100L;
  w

let check_queries name (p : P.t) =
  let check_count subname actual expected =
    check_count (name ^ ": " ^ subname) actual expected
  in
  check_count "root count sums all contexts"
    (P.count_for_location p [leaf_a])
    15L;
  check_count "context refines the count"
    (P.count_for_location p [leaf_a; ctx_b])
    5L;
  check_count "unrecorded context counts as 0"
    (P.count_for_location p [leaf_a; ctx_d])
    0L;
  check_count "deep stack" (P.count_for_location p [leaf_c; ctx_b; ctx_d]) 7L;
  check_count "prefix of a deep stack"
    (P.count_for_location p [leaf_c; ctx_b])
    7L;
  check_count "position sampled only as a context is not a root"
    (P.count_for_location p [ctx_b])
    0L;
  check_count "never-sampled position"
    (P.count_for_location p [["z.ml"; "1"; "0"]])
    0L;
  check_count "empty stack" (P.count_for_location p []) 0L

let with_temp_file f =
  let filename = Filename.temp_file "source_position_profile" ".fdo" in
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () -> f filename)

let () =
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename;
      let p = P.load ~filename in
      check_queries "file round-trip" p)

let () =
  check_queries "in-memory profile" (P.Writer.to_profile (make_writer ()))

(* [load] goes through the registered mapper when there is one (the tests below
   keep using this one). *)
let () =
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename;
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
      check_queries "mapped round-trip" p)

let () =
  with_temp_file (fun filename ->
      P.Writer.write (P.Writer.create ()) ~filename;
      let p = P.load ~filename in
      check_count "empty profile query" (P.count_for_location p [leaf_a]) 0L;
      check "empty profile has no call targets"
        (List.is_empty (P.call_targets p ctx_b)))

(* The call-target index: the call site at [ctx_b] reached the functions whose
   entries are [leaf_a] and [leaf_c]; their counts by context are in the trie
   (the profile above: [leaf_a] from [ctx_b] 5 times, [leaf_c] from [ctx_b]
   inlined at [ctx_d] 7 times). *)
let () =
  let w = make_writer () in
  let hash = Fdo_location.hash_level in
  P.Writer.add_call_target w ~callsite:(hash ctx_b) ~callee:(hash leaf_a)
    ~count:5L;
  P.Writer.add_call_target w ~callsite:(hash ctx_b) ~callee:(hash leaf_c)
    ~count:7L;
  let check_targets name p =
    check (name ^ ": call targets")
      (List.equal Int64.equal
         (List.sort Int64.unsigned_compare (P.call_targets p ctx_b))
         (List.sort Int64.unsigned_compare [hash leaf_a; hash leaf_c]));
    check
      (name ^ ": unknown call site")
      (List.is_empty (P.call_targets p ctx_d));
    (* Counts with as much context as the profile has: exact, refined by the
       context, aggregated when the context goes deeper than recorded. *)
    let count root context =
      P.count_for_deepest_context p ~root:(hash root)
        ~context:(List.map hash context)
    in
    check_count (name ^ ": context refines") (count leaf_a [ctx_b]) 5L;
    check_count
      (name ^ ": deeper context aggregates")
      (count leaf_a [ctx_b; ctx_d])
      5L;
    check_count
      (name ^ ": unknown context stops at the root")
      (count leaf_a [ctx_d]) 15L;
    check_count (name ^ ": full context") (count leaf_c [ctx_b; ctx_d]) 7L;
    check_count (name ^ ": unknown root") (count ctx_d [ctx_b]) 0L;
    let totals = ref [] in
    P.iter_call_targets p ~f:(fun ~hash ~depth ~count ->
        totals := (hash, depth, count) :: !totals);
    check
      (name ^ ": call-target totals")
      (List.sort compare !totals
      = List.sort compare
          [hash ctx_b, 1, 12L; hash leaf_a, 2, 5L; hash leaf_c, 2, 7L])
  in
  check_targets "in-memory call targets" (P.Writer.to_profile w);
  with_temp_file (fun filename ->
      P.Writer.write w ~filename;
      check_targets "file call targets" (P.load ~filename))

(* Labels on real [Debuginfo.t] values: creation, inlining, specialization,
   function entries. *)
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
  let dbg_a = debuginfo ~file:"a.ml" ~line:1 ~col:0 in
  let dbg_b = debuginfo ~file:"b.ml" ~line:2 ~col:3 in
  (* A pseudo-instrumentation label is a location: the level of its anchor,
     branch index and edge index, then the call sites it was inlined through,
     innermost first. Labels need no source position. *)
  let labels =
    Debuginfo.create_edge_labels ~anchor:leaf_a ~index:7 ~num_edges:2
  in
  let location_of_edge position labels =
    match labels with
    | Debuginfo.Positional sets -> (
      match sets.(position) with
      | [{ location; rider = false }] -> location
      | _ -> [])
    | Debuginfo.Resolved _ | Debuginfo.Callsite _ -> []
  in
  let equal_locations = List.equal (List.equal String.equal) in
  check "label location of edge 0"
    (equal_locations (location_of_edge 0 labels) [["a.ml"; "1"; "0"; "7"; "0"]]);
  check "label location of edge 1"
    (equal_locations (location_of_edge 1 labels) [["a.ml"; "1"; "0"; "7"; "1"]]);
  check "unit anchor"
    (List.equal String.equal (Fdo_location.unit_anchor "Foo") ["<Foo>"]);
  check "nested anchor"
    (List.equal String.equal
       (Fdo_location.nested_anchor ~anchor:leaf_a ~index:2)
       ["a.ml"; "1"; "0"; "2"]);
  let entry_locations dbg =
    List.map
      (fun (l : Debuginfo.branch_label) -> l.location)
      (Debuginfo.entry_labels dbg)
  in
  check "entry label"
    (List.equal equal_locations
       (entry_locations (Debuginfo.with_entry_label dbg_a))
       [[leaf_a]]);
  check "no entry label without a position"
    (Debuginfo.entry_labels (Debuginfo.with_entry_label Debuginfo.none) = []);
  (* Riders: the labels of the calls inlined at the head of the code an edge
     leads to, here the function entry, are added once each and flagged. *)
  let with_riders =
    Debuginfo.add_riders
      (Debuginfo.with_entry_label dbg_a)
      ~position:0
      [[ctx_b; leaf_a]; [ctx_d; leaf_a]]
  in
  let with_riders =
    Debuginfo.add_riders with_riders ~position:0 [[ctx_b; leaf_a]]
  in
  check "riders"
    (List.equal equal_locations
       (entry_locations with_riders)
       [[leaf_a]; [ctx_b; leaf_a]; [ctx_d; leaf_a]]
    && List.equal Bool.equal
         (List.map
            (fun (l : Debuginfo.branch_label) -> l.rider)
            (Debuginfo.entry_labels with_riders))
         [false; true; true]);
  check "riders need a position"
    (Debuginfo.add_riders dbg_a ~position:0 [[ctx_b]] == dbg_a);
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
      check name (equal_locations (location_of_edge 0 labels) expected)
    | None -> check (name ^ " (labels kept)") false
  in
  let edge_0 = ["a.ml"; "1"; "0"; "7"; "0"] in
  check_inlined "inlined label location" inlined_once [edge_0; ctx_b];
  check_inlined "twice inlined label location" inlined_twice
    [edge_0; ctx_b; ctx_d];
  (* Specialization (a copy of the function made by inlining the code that
     defined it) renames the outermost level, the copied function's own, instead
     of adding one, and leaves the positions alone. *)
  let specialized = Debuginfo.specialize_edge_labels ~site:dbg_d inlined_once in
  check_inlined "specialized label location" specialized [edge_0; ctx_b @ ctx_d];
  check_inlined "specialized single-level label"
    (Debuginfo.specialize_edge_labels ~site:dbg_d carrier)
    [edge_0 @ ctx_d];
  check "specialization keeps the positions"
    (Debuginfo.compare specialized inlined_once = 0);
  (* The call site label of an application is inlined like the others. *)
  check "call site label"
    (Option.equal equal_locations
       (Debuginfo.callsite_label (Debuginfo.with_callsite_label dbg_b))
       (Some [ctx_b]));
  check "call site label inlined"
    (Option.equal equal_locations
       (Debuginfo.callsite_label
          (Debuginfo.inline dbg_d
             ~from_inlined_body:(Debuginfo.with_callsite_label dbg_b)))
       (Some [ctx_b; ctx_d]))

(* Edges are locations like any other: the level of the anchor and indices heads
   the location, the inlining call sites follow. *)
let () =
  let edge_a edge = leaf_a @ ["0"; string_of_int edge] in
  let make_writer () =
    let w = make_writer () in
    (* A construct at the root of the function anchored at [leaf_a], with edges
       3 and 4: edge 3 was recorded in two contexts (once inlined at
       [ctx_b]). *)
    P.Writer.add_location w ~location:[edge_a 3] ~count:40L;
    P.Writer.add_location w ~location:[edge_a 4] ~count:20L;
    P.Writer.add_location w ~location:[edge_a 3; ctx_b] ~count:10L;
    w
  in
  let check_queries name p =
    let check_count subname actual expected =
      check_count (name ^ ": " ^ subname) actual expected
    in
    check_count "edge aggregates all contexts"
      (P.count_for_location p [edge_a 3])
      50L;
    check_count "the other edge" (P.count_for_location p [edge_a 4]) 20L;
    check_count "context refines the edge"
      (P.count_for_location p [edge_a 3; ctx_b])
      10L;
    check_count "unrecorded context"
      (P.count_for_location p [edge_a 3; ctx_d])
      0L;
    check_count "unrecorded edge" (P.count_for_location p [edge_a 9]) 0L;
    check_count "position samples are untouched"
      (P.count_for_location p [leaf_a])
      15L
  in
  check_count "hashed stacks hit the same trie nodes"
    (let w = make_writer () in
     P.Writer.add_hashed_stack w
       ~hashes:
         [Fdo_location.hash_level (edge_a 3); Fdo_location.hash_level ctx_b]
       ~count:5L;
     let p = P.Writer.to_profile w in
     P.count_for_location p [edge_a 3; ctx_b])
    15L;
  check_queries "in-memory edges" (P.Writer.to_profile (make_writer ()));
  with_temp_file (fun filename ->
      P.Writer.write (make_writer ()) ~filename;
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
          P.iter p ~f:(fun ~hash:_ ~depth:_ ~count:_ -> ())
        with
        | () ->
          incr failures;
          Printf.eprintf "FAILED: %s: no error raised\n%!" name
        | exception P.Error _ -> ())
  in
  let good =
    with_temp_file (fun filename ->
        P.Writer.write (make_writer ()) ~filename;
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
