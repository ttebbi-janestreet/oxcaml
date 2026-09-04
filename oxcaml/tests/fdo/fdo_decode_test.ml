(* Unit tests for the parsing in the oxcaml-fdo-decode tool ([Fdo_decode_lib]),
   on canned "perf script" output, branch-label metadata bytes, and
   llvm-symbolizer output. *)

module Branch_labels = Fdo_decode_lib.Branch_labels
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
      | Branch_stack (c1, b1), Branch_stack (c2, b2) ->
        Int64.equal c1 c2
        && List.equal
             (fun (f1, t1) (f2, t2) -> Int64.equal f1 f2 && Int64.equal t1 t2)
             b1 b2
      | (Blank | Chain_header _ | Sample _ | Address _ | Branch_stack _), _ ->
        false)
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
  check_line "branch stack with period"
    "  5 0x401234/0x401250/P/-/-/0/COND/- 0x401300/0x401200/M/-/-/3"
    (Branch_stack (5L, [0x401234L, 0x401250L; 0x401300L, 0x401200L]));
  check_line "branch stack without period" "0x10/0x20/P/-/-/0"
    (Branch_stack (1L, [0x10L, 0x20L]));
  check "garbage aborts"
    (match Perf_script.classify_line "not a sample" with
    | exception Failure _ -> true
    | Blank | Chain_header _ | Sample _ | Address _ | Branch_stack _ -> false);
  check "garbage branch entry aborts"
    (match Perf_script.classify_line "x/y/z" with
    | exception Failure _ -> true
    | Blank | Chain_header _ | Sample _ | Address _ | Branch_stack _ -> false)

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
          (Perf_script.of_channel ~binary:"/home/me/prog"
             ~code_bounds:None (* only branch stacks consult these *)
             ~branch_sites:[||]))
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

(* Perf_script.of_channel on branch-stack output: endpoint counting, code-bounds
   filtering, and branch outcome counts (direct classification via the
   fallthrough address and fallthroughs of sites inside the sequential range
   between consecutive entries). *)
let () =
  let script_output =
    String.concat "\n"
      [ "  2 0x401000/0x401100/P/-/-/0/COND/- 0x7f0000000000/0x401000/P/-/-/0";
        "  3 0x401000/0x401100/M/-/-/1";
        "  7 0x401200/0x401300/P/-/-/0 0x401000/0x401006/M/-/-/0";
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
          (Perf_script.of_channel ~binary:"/home/me/prog"
             ~code_bounds:(Some (0x400000L, 0x500000L))
             ~branch_sites:
               [| { address = 0x401000L; fallthrough_address = 0x401006L };
                  { address = 0x401080L; fallthrough_address = 0x401086L }
               |]))
  in
  let count stack = Hashtbl.find_opt samples.Perf_script.counts stack in
  check "branch endpoints counted"
    (Option.equal Int64.equal (count [0x401000L]) (Some 14L));
  check "branch targets counted"
    (Option.equal Int64.equal (count [0x401100L]) (Some 5L));
  check "out-of-bounds address dropped"
    (Option.is_none (count [0x7f0000000000L]));
  check "site inside a sequential range gets a position sample"
    (Option.equal Int64.equal (count [0x401080L]) (Some 7L));
  check "total counts every in-bounds sample"
    (Int64.equal samples.Perf_script.total 47L);
  let outcome address =
    match Hashtbl.find_opt samples.Perf_script.branches address with
    | Some ({ taken; fallthrough } : Perf_script.branch_counts) ->
      Some (taken, fallthrough)
    | None -> None
  in
  check "site classified taken and fallen through by target"
    (match outcome 0x401000L with
    | Some (5L, 7L) -> true
    | Some _ | None -> false);
  check "site inside a sequential range fell through"
    (match outcome 0x401080L with
    | Some (0L, 7L) -> true
    | Some _ | None -> false)

(* Branch_labels.parse: the "fdo_branch_labels" section format. *)
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
  (* First compilation unit: a two-location and a one-location label, and a site
     carrying one of each. *)
  Buffer.add_string buffer "FDLB";
  uleb 1 (* version *);
  uleb 1 (* label definition (id 1) *);
  uleb 2;
  u64 0x1111L;
  u64 0x2222L;
  uleb 1 (* label definition (id 2) *);
  uleb 1;
  u64 0x3333L;
  uleb 0 (* site entry *);
  u64 0x4000L;
  u64 0x4006L (* fallthrough address *);
  uleb 1;
  uleb 1 (* taken: label 1 *);
  uleb 1;
  uleb 2 (* fallthrough: label 2 *);
  (* Second compilation unit: definition ids restart from 1, and a multi-byte
     uleb (the site's taken-edge count). *)
  Buffer.add_string buffer "FDLB";
  uleb 1 (* version *);
  uleb 1 (* label definition (id 1) *);
  uleb 1;
  u64 0x4444L;
  uleb 0 (* site entry *);
  u64 0x5000L;
  u64 0x5002L;
  uleb 130 (* taken: label 1, 130 times *);
  for _ = 1 to 130 do
    uleb 1
  done;
  uleb 1;
  uleb 1 (* fallthrough: label 1 *);
  let sites = Branch_labels.parse (Buffer.contents buffer) in
  check "number of sites" (Hashtbl.length sites = 2);
  check "site with a label per edge"
    (match Hashtbl.find_opt sites 0x4000L with
    | Some
        { Branch_labels.fallthrough_address = 0x4006L;
          taken = [[0x1111L; 0x2222L]];
          fallthrough = [[0x3333L]]
        } ->
      true
    | Some _ | None -> false);
  check "definition ids restart per compilation unit"
    (match Hashtbl.find_opt sites 0x5000L with
    | Some
        { Branch_labels.fallthrough_address = 0x5002L;
          taken;
          fallthrough = [[0x4444L]]
        } ->
      List.length taken = 130
      && List.for_all
           (fun label -> List.equal Int64.equal label [0x4444L])
           taken
    | Some _ | None -> false);
  check "bad magic aborts"
    (match Branch_labels.parse "XXXX" with
    | exception Failure _ -> true
    | (_ : (int64, Branch_labels.site) Hashtbl.t) -> false);
  check "undefined label id aborts"
    ((* Version 1, then a site entry whose taken edge references the undefined
        label id 7. *)
     let bad = Buffer.create 32 in
     Buffer.add_string bad "FDLB\x01\x00";
     Buffer.add_string bad "\x00\x40\x00\x00\x00\x00\x00\x00";
     Buffer.add_string bad "\x06\x40\x00\x00\x00\x00\x00\x00";
     Buffer.add_string bad "\x01\x07\x00";
     match Branch_labels.parse (Buffer.contents bad) with
     | exception Failure _ -> true
     | (_ : (int64, Branch_labels.site) Hashtbl.t) -> false)

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
