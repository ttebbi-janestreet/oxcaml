(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* Round-trip tests for the source-call-stack profile reader. We encode the
   on-disk format by hand (the producer, ocamlfdo, lives outside this repo) and
   check that the reader parses it, answers exact-match and estimate queries
   correctly, and rejects malformed input. Each frame is a single opaque
   string. *)

module P = Source_stack_profile

(* -- Encoder mirroring the on-disk format (all integers little-endian) -- *)

let add_u8 b n = Buffer.add_uint8 b n

let add_u32 b n = Buffer.add_int32_le b (Int32.of_int n)

let add_i64 b n = Buffer.add_int64_le b n

let add_string b s =
  add_u32 b (String.length s);
  Buffer.add_string b s

let add_option b add_elt = function
  | None -> add_u8 b 0
  | Some x ->
    add_u8 b 1;
    add_elt x

let add_set b frames =
  add_u32 b (List.length frames);
  List.iter (add_string b) frames

type tnode =
  { tcount : int64;
    tchildren : (string * tnode) list
  }

let rec add_node b { tcount; tchildren } =
  add_i64 b tcount;
  add_u32 b (List.length tchildren);
  List.iter
    (fun (frame, child) ->
      add_string b frame;
      add_node b child)
    tchildren

let encode ?(magic = P.magic_number) ~depth ~buildid ~total_calls
    ~known_positions root =
  let b = Buffer.create 256 in
  Buffer.add_string b magic;
  add_u32 b depth;
  add_option b (add_string b) buildid;
  add_i64 b (Int64.of_int total_calls);
  add_set b known_positions;
  add_node b root;
  Buffer.contents b

let write_temp contents =
  let file = Filename.temp_file "sstack" ".profile" in
  Out_channel.with_open_bin file (fun oc ->
      Out_channel.output_string oc contents);
  file

let load_bytes contents =
  let file = write_temp contents in
  Fun.protect
    ~finally:(fun () -> try Sys.remove file with Sys_error _ -> ())
    (fun () -> P.load ~filename:file)

(* -- Tiny assertion harness -- *)

let failures = ref 0

let check name cond =
  if not cond
  then begin
    incr failures;
    Printf.eprintf "FAIL: %s\n" name
  end

let check_count name expected got =
  check name (Option.equal Int.equal expected got)

(* [estimate] now returns an absolute count ([int option]), like
   [find_count]. *)
let check_estimate = check_count

let expect_error name f =
  match f () with
  | exception P.Error _ -> ()
  | _ ->
    incr failures;
    Printf.eprintf "FAIL: %s (expected an Error)\n" name

(* Sample profile. Trie (root count 130): "a" (10) with no recorded caller
   context; "b" (100) called from "c" (40) and "e" (30); "cold" (0), a known
   position that never executed. Known positions: the trie frames a, b, c, e,
   cold, plus "never" and "d" (known but not in the trie). *)

let leaf tcount = { tcount; tchildren = [] }

let trie =
  { tcount = 130L;
    tchildren =
      [ "a", leaf 10L;
        "b", { tcount = 100L; tchildren = ["c", leaf 40L; "e", leaf 30L] };
        "cold", leaf 0L ]
  }

let known_positions = ["a"; "b"; "c"; "e"; "cold"; "never"; "d"]

(* [total_calls] (200): the count consumers normalize [estimate] against. It is
   deliberately larger than the trie root count (130) to exercise that the two
   are distinct (the profile may observe calls not represented in the trie). *)
let sample_total_calls = 200

let encode_sample ?magic ?(buildid = Some "build-xyz") () =
  encode ?magic ~depth:8 ~buildid ~total_calls:sample_total_calls
    ~known_positions trie

(* [estimate] returns the absolute matched count; the caller normalizes it (e.g.
   by [total_calls] = 200). *)
let count n = Some n

let () =
  let profile = load_bytes (encode_sample ()) in
  (* Header. *)
  check "depth" (P.depth profile = 8);
  check "buildid"
    (Option.equal String.equal (P.buildid profile) (Some "build-xyz"));
  check "total_count" (P.total_count profile = 130);
  check "total_calls" (P.total_calls profile = 200);
  (* find_count: exact full-stack matches only. *)
  check_count "exact b>c" (Some 40) (P.find_count profile ["b"; "c"]);
  check_count "exact b>e" (Some 30) (P.find_count profile ["b"; "e"]);
  check_count "exact b" (Some 100) (P.find_count profile ["b"]);
  check_count "exact a" (Some 10) (P.find_count profile ["a"]);
  check_count "exact cold" (Some 0) (P.find_count profile ["cold"]);
  check_count "exact a>b>c misses" None (P.find_count profile ["a"; "b"; "c"]);
  check_count "exact unknown" None (P.find_count profile ["nope"]);
  (* estimate (absolute matched count; the caller normalizes it). *)
  check_estimate "est a" (count 10) (P.estimate profile ["a"]);
  check_estimate "est b" (count 100) (P.estimate profile ["b"]);
  check_estimate "est b>c" (count 40) (P.estimate profile ["b"; "c"]);
  check_estimate "est b>e" (count 30) (P.estimate profile ["b"; "e"]);
  check_estimate "est cold" (count 0) (P.estimate profile ["cold"]);
  (* Known position that never executed (not in the trie). *)
  check_estimate "est never-executed" (count 0) (P.estimate profile ["never"]);
  (* Unknown leaf (actual position) -> None. *)
  check_estimate "est unknown leaf" None (P.estimate profile ["nope"]);
  check_estimate "est unknown leaf, known caller" None
    (P.estimate profile ["nope"; "b"]);
  (* An unknown frame occupies a level but cannot be identified, so the estimate
     sums over all children. "x" is not a known position, so ["b"; "x"] sums
     "b"'s children (40 + 30 = 70). *)
  check_estimate "est b>x (sum over children)" (count 70)
    (P.estimate profile ["b"; "x"]);
  (* No children to sum over -> 0 ("a" has no recorded caller contexts). *)
  check_estimate "est a>x (no children)" (count 0)
    (P.estimate profile ["a"; "x"]);
  (* Known frame with no recorded context here -> 0 (never observed): "d" is a
     known position but not a child of "c". *)
  check_estimate "est b>c>d (known frame, no context)" (count 0)
    (P.estimate profile ["b"; "c"; "d"]);
  (* "a" is a known position matched as a call site; it is not a child of "b",
     so that context was never observed -> 0. *)
  check_estimate "est b>a (known caller, no context)" (count 0)
    (P.estimate profile ["b"; "a"]);
  check_estimate "est empty" None (P.estimate profile [])

let () =
  (* Loading from an actual file works (the other tests go through [load]
     too). *)
  let file = write_temp (encode_sample ~buildid:None ()) in
  let t = P.load ~filename:file in
  check "load value" (P.total_count t = 130);
  try Sys.remove file with Sys_error _ -> ()

let () =
  (* Malformed input is rejected (loudly). *)
  let good = encode_sample ~buildid:(Some "b") () in
  let minimal ?magic () =
    encode ?magic ~depth:1 ~buildid:None ~total_calls:0 ~known_positions:[]
      (leaf 0L)
  in
  expect_error "bad magic" (fun () ->
      load_bytes
        (minimal ~magic:(String.make (String.length P.magic_number) 'x') ()));
  let wrong_version =
    String.sub P.magic_number 0 (String.length P.magic_number - 1) ^ "\003"
  in
  expect_error "wrong version" (fun () ->
      load_bytes (minimal ~magic:wrong_version ()));
  expect_error "truncated" (fun () ->
      load_bytes (String.sub good 0 (String.length good - 3)));
  expect_error "trailing bytes" (fun () -> load_bytes (good ^ "junk"));
  let dup_sibling = { tcount = 1L; tchildren = ["a", leaf 2L; "a", leaf 3L] } in
  expect_error "duplicate sibling frame" (fun () ->
      load_bytes
        (encode ~depth:2 ~buildid:None ~total_calls:1 ~known_positions:[]
           dup_sibling));
  expect_error "duplicate set entry" (fun () ->
      load_bytes
        (encode ~depth:1 ~buildid:None ~total_calls:1
           ~known_positions:["a"; "a"] (leaf 0L)))

let () =
  if !failures = 0
  then print_string "source_stack_profile_test: all checks passed\n"
  else begin
    Printf.eprintf "source_stack_profile_test: %d check(s) failed\n" !failures;
    exit 1
  end
