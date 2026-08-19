(* Unit tests for the parsing in the oxcaml-fdo-decode tool ([Fdo_decode_lib]),
   on canned "perf script" output, patchprof profile bytes, and llvm-symbolizer
   output. *)

module Patchprof_labels = Fdo_decode_lib.Patchprof_labels
module Patchprof_profile = Fdo_decode_lib.Patchprof_profile
module Perf_script = Fdo_decode_lib.Perf_script
module Symbolizer = Fdo_decode_lib.Symbolizer

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.eprintf "FAILED: %s\n%!" name)

(* Perf_script.classify_line *)
let () =
  let check_line name line (expected : Perf_script.line) =
    let actual : Perf_script.line = Perf_script.classify_line line in
    check name
      (match actual, expected with
      | Perf_script.Blank, Perf_script.Blank -> true
      | Chain_header c1, Chain_header c2 -> Int64.equal c1 c2
      | Sample (c1, a1, d1), Sample (c2, a2, d2) ->
        Int64.equal c1 c2 && Int64.equal a1 a2 && String.equal d1 d2
      | Address (a1, d1), Address (a2, d2) ->
        Int64.equal a1 a2 && String.equal d1 d2
      | (Blank | Chain_header _ | Sample _ | Address _), _ -> false)
  in
  check_line "sample with period" "     3 401234 (/tmp/a.out)"
    (Sample (3L, 0x401234L, "/tmp/a.out"));
  check_line "address without period" "  401234 (/tmp/a.out)"
    (Address (0x401234L, "/tmp/a.out"));
  check_line "kernel sample" " 1 ffffffff81000000 ([kernel.kallsyms])"
    (Sample (1L, 0xffffffff81000000L, "[kernel.kallsyms]"));
  check_line "chain header" "       42 " (Chain_header 42L);
  check_line "chain entry" "\t    401200 (prog)" (Address (0x401200L, "prog"));
  check_line "blank line" "   " Blank;
  check "garbage aborts"
    (match Perf_script.classify_line "not a sample" with
    | exception Failure _ -> true
    | Blank | Chain_header _ | Sample _ | Address _ -> false)

(* Perf_script.of_channel: aggregation, dso filtering, call chains. *)
let () =
  let script_output =
    String.concat "\n"
      [ "  2 401000 (/build/prog)";
        "  3 401000 (/build/prog)";
        "401008 (prog)";
        " 10 402000 (/lib/x86_64-linux-gnu/libc.so.6)";
        "  1 ffffffff81000000 ([kernel.kallsyms])";
        "        7 ";
        "\t    401000 (/build/prog)";
        "\t    401200 (prog)";
        "\t    7f0000 (/lib/x86_64-linux-gnu/libc.so.6)";
        "\t    401300 (/build/prog)";
        "";
        "        4 ";
        "\t    401000 (inlined)";
        "\t    401000 (/build/prog)";
        "\t    401200 (prog)";
        "";
        "        2 ";
        "\t    402000 (/lib/x86_64-linux-gnu/libc.so.6)";
        "\t    401000 (/build/prog)";
        "" ]
  in
  let samples =
    let filename = Filename.temp_file "fdo_decode_test" ".txt" in
    Fun.protect
      ~finally:(fun () -> Sys.remove filename)
      (fun () ->
        Out_channel.with_open_text filename (fun oc ->
            Out_channel.output_string oc script_output);
        In_channel.with_open_text filename
          (Perf_script.of_channel ~binary:"/home/me/prog"))
  in
  let count stack = Hashtbl.find_opt samples.Perf_script.counts stack in
  check "aggregated same flat address"
    (Option.equal Int64.equal (count [0x401000L]) (Some 5L));
  check "legacy no-period sample"
    (Option.equal Int64.equal (count [0x401008L]) (Some 1L));
  check "other dso filtered" (Option.is_none (count [0x402000L]));
  check "kernel filtered" (Option.is_none (count [0xffffffff81000000L]));
  check "call chain kept and cut at first foreign frame"
    (Option.equal Int64.equal (count [0x401000L; 0x401200L]) (Some 11L));
  check "chain with foreign leaf dropped"
    (Option.is_none (count [0x402000L; 0x401000L]));
  check "total counts only target samples"
    (Int64.equal samples.Perf_script.total 17L)

(* Patchprof_profile.of_string: record framing, aggregation, rescaling to exact
   execution counts, walkless sites, truncated tails. *)
let () =
  let profile records =
    let buffer = Buffer.create 256 in
    let word w =
      let b = Bytes.create 8 in
      Bytes.set_int64_le b 0 w;
      Buffer.add_bytes buffer b
    in
    word 0x000a31464f525050L (* "PPROF1\n" *);
    List.iter (List.iter word) records;
    Buffer.contents buffer
  in
  let record kind payload =
    kind :: Int64.of_int (List.length payload) :: payload
  in
  let { Patchprof_profile.samples; sites } =
    Patchprof_profile.of_string
      (profile
         [ (* Selection and statistics records do not affect the samples. *)
           record 1L [1L; 2L; 17L; 1000L; 0L; 0L; 500L];
           (* Walks of domain 0: site 0x1000 sampled with weight 5 (one caller
              walked) and weight 15 (no caller walked). *)
           record 2L [0L; 5L; 2L; 0x1000L; 0x2001L; 15L; 1L; 0x1000L];
           (* The same stack from another domain aggregates. *)
           record 2L [1L; 10L; 2L; 0x1000L; 0x2001L];
           (* Site 0x3000, whose counter record will turn out lost. *)
           record 2L [1L; 30L; 1L; 0x3000L];
           record 4L [0L; 3L; 1L; 5L; 0L];
           (* Exact executions: site 0x1000 ran 25 + 35 = 60 times (its walk
              weights sum to 30, so every walk count is doubled); site 0x5000
              ran 7 times but was never sampled. *)
           record 3L
             [0L; 2L; 999L; 0x1000L; 25L; 2L; 20L; 3L; 0x5000L; 7L; 0L; 0L; 0L];
           record 3L [1L; 1L; 888L; 0x1000L; 35L; 1L; 10L; 2L];
           (* A record truncated by the producer dying mid-write ends the
              stream. *)
           [2L; 100L; 0L] ])
  in
  let count stack = Hashtbl.find_opt samples.Perf_script.counts stack in
  check "walks rescaled to exact executions"
    (Option.equal Int64.equal (count [0x1000L; 0x2001L]) (Some 30L));
  check "site-only walk rescaled"
    (Option.equal Int64.equal (count [0x1000L]) (Some 30L));
  check "lost counter record falls back to the sampled weight"
    (Option.equal Int64.equal (count [0x3000L]) (Some 30L));
  check "walkless site kept with its exact count"
    (Option.equal Int64.equal (count [0x5000L]) (Some 7L));
  check "no other stacks" (Hashtbl.length samples.Perf_script.counts = 4);
  check "total sums the counts" (Int64.equal samples.Perf_script.total 97L);
  let site_counters address = Hashtbl.find_opt sites address in
  check "site counters aggregate across records"
    (match site_counters 0x1000L with
    | Some { Patchprof_profile.executions; sampled_weight; tally } ->
      Int64.equal executions 60L
      && Int64.equal sampled_weight 30L
      && Int64.equal tally 5L
    | None -> false);
  check "walkless site has counters"
    (match site_counters 0x5000L with
    | Some { Patchprof_profile.executions; sampled_weight; tally } ->
      Int64.equal executions 7L
      && Int64.equal sampled_weight 0L
      && Int64.equal tally 0L
    | None -> false);
  check "site without a counter record has no counters"
    (Option.is_none (site_counters 0x3000L));
  check "bad magic aborts"
    (match Patchprof_profile.of_string "not a patchprof profile" with
    | exception Failure _ -> true
    | (_ : Patchprof_profile.t) -> false);
  check "malformed walk record aborts"
    (match
       Patchprof_profile.of_string (profile [record 2L [0L; 5L; 0L; 0x1000L]])
     with
    | exception Failure _ -> true
    | (_ : Patchprof_profile.t) -> false)

(* Patchprof_labels.parse: the "patchprof_labels" section format. *)
let () =
  let buffer = Buffer.create 128 in
  let uleb n =
    let rec loop n =
      if n < 0x80
      then Buffer.add_char buffer (Char.chr n)
      else (
        Buffer.add_char buffer (Char.chr (n land 0x7f lor 0x80));
        loop (n lsr 7))
    in
    loop n
  in
  let u64 v =
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 v;
    Buffer.add_bytes buffer b
  in
  (* First compilation unit: a two-frame and a one-frame label of the same
     construct, and a site carrying one of each. *)
  Buffer.add_string buffer "PPLB";
  uleb 1 (* version *);
  uleb 1 (* label definition (id 1) *);
  uleb 2;
  u64 0x1111L;
  u64 0x2222L;
  uleb 3 (* disc *);
  uleb 0 (* edge *);
  uleb 1 (* label definition (id 2) *);
  uleb 1;
  u64 0x1111L;
  uleb 3;
  uleb 1;
  uleb 0 (* site entry *);
  u64 0x4000L;
  uleb 1;
  uleb 1 (* taken: label 1 *);
  uleb 1;
  uleb 2 (* fallthrough: label 2 *);
  (* Second compilation unit: definition ids restart from 1, and a multi-byte
     uleb discriminator. *)
  Buffer.add_string buffer "PPLB";
  uleb 1 (* version *);
  uleb 1 (* label definition (id 1) *);
  uleb 1;
  u64 0x3333L;
  uleb 300;
  uleb 5;
  uleb 0 (* site entry *);
  u64 0x5000L;
  uleb 0 (* taken: none *);
  uleb 1;
  uleb 1 (* fallthrough: label 1 *);
  let sites = Patchprof_labels.parse (Buffer.contents buffer) in
  check "number of sites" (Hashtbl.length sites = 2);
  check "site with a label per edge"
    (match Hashtbl.find_opt sites 0x4000L with
    | Some
        { Patchprof_labels.taken =
            [{ frame_hashes = [0x1111L; 0x2222L]; disc = 3; edge = 0 }];
          fallthrough = [{ frame_hashes = [0x1111L]; disc = 3; edge = 1 }]
        } ->
      true
    | Some _ | None -> false);
  check "definition ids restart per compilation unit"
    (match Hashtbl.find_opt sites 0x5000L with
    | Some
        { Patchprof_labels.taken = [];
          fallthrough = [{ frame_hashes = [0x3333L]; disc = 300; edge = 5 }]
        } ->
      true
    | Some _ | None -> false);
  check "bad magic aborts"
    (match Patchprof_labels.parse "XXXX" with
    | exception Failure _ -> true
    | (_ : (int64, Patchprof_labels.site_labels) Hashtbl.t) -> false);
  check "undefined label id aborts"
    ((* Version 1, then a site entry whose taken edge references the undefined
        label id 7. *)
     let bad = "PPLB\x01\x00\x00\x60\x00\x00\x00\x00\x00\x00\x01\x07\x00" in
     match Patchprof_labels.parse bad with
     | exception Failure _ -> true
     | (_ : (int64, Patchprof_labels.site_labels) Hashtbl.t) -> false)

(* Symbolizer.parse_output, on llvm-symbolizer --inlines --verbose output. *)
let () =
  let check_frame name (actual : Symbolizer.frame) (file, line, col, disc) =
    check name
      (String.equal actual.file file
      && actual.line = line && actual.col = col
      && actual.discriminator = disc)
  in
  let output =
    [ "camlFoo.f_0_1_code";
      "  Filename: foo.ml";
      "  Function start filename: foo.ml";
      "  Function start line: 10";
      "  Function start address: 0x401000";
      "  Line: 12";
      "  Column: 5";
      "  Discriminator: 3";
      "camlFoo.g_0_2_code";
      "  Filename: dir/a:b.ml";
      "  Line: 3";
      "  Column: 0";
      "";
      "??";
      "  Filename: ??";
      "  Line: 0";
      "  Column: 0";
      "";
      "camlFoo.h_0_3_code";
      "  Filename: foo.ml";
      "  Line: 20";
      "  Column: 1";
      "" ]
  in
  (match Symbolizer.parse_output output with
  | [inlined; unknown; plain] -> (
    (match inlined with
    | [leaf; caller] ->
      check_frame "inlined leaf with discriminator" leaf ("foo.ml", 12, 5, 3);
      check_frame "inlined caller, colon in filename" caller
        ("dir/a:b.ml", 3, 0, 0)
    | _ -> check "inlined stack shape" false);
    check "unknown address is empty stack" (List.is_empty unknown);
    match plain with
    | [leaf] -> check_frame "plain leaf" leaf ("foo.ml", 20, 1, 0)
    | _ -> check "plain stack shape" false)
  | _ -> check "number of stacks" false);
  check "attribute outside a frame aborts"
    (match Symbolizer.parse_output ["  Line: 3"; ""] with
    | exception Failure _ -> true
    | (_ : Symbolizer.frame list list) -> false);
  check "unparsable attribute aborts"
    (match Symbolizer.parse_output ["f"; "  Line: x"; ""] with
    | exception Failure _ -> true
    | (_ : Symbolizer.frame list list) -> false)

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
