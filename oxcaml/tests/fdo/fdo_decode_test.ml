(* Unit tests for the parsing in the oxcaml-fdo-decode tool ([Fdo_decode_lib]),
   on canned "perf script" and llvm-symbolizer output. *)

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

(* Symbolizer.parse_location / parse_output *)
let () =
  let check_frame name (actual : Symbolizer.frame) (file, line, col) =
    check name
      (String.equal actual.file file && actual.line = line && actual.col = col)
  in
  check_frame "location"
    (Symbolizer.parse_location "foo/bar.ml:42:17")
    ("foo/bar.ml", 42, 17);
  check_frame "location with colon in file"
    (Symbolizer.parse_location "a:b.ml:3:7")
    ("a:b.ml", 3, 7);
  let output =
    [ "camlFoo.f_0_1_code";
      "foo.ml:12:5";
      "camlFoo.g_0_2_code";
      "bar/baz.ml:3:0";
      "";
      "??";
      "??:0:0";
      "";
      "camlFoo.h_0_3_code";
      "foo.ml:20:1";
      "" ]
  in
  match Symbolizer.parse_output output with
  | [inlined; unknown; plain] -> (
    check "inlined stack length" (List.length inlined = 2);
    (match inlined with
    | [leaf; caller] ->
      check_frame "inlined leaf" leaf ("foo.ml", 12, 5);
      check_frame "inlined caller" caller ("bar/baz.ml", 3, 0)
    | _ -> check "inlined stack shape" false);
    check "unknown address is empty stack" (List.is_empty unknown);
    match plain with
    | [leaf] -> check_frame "plain leaf" leaf ("foo.ml", 20, 1)
    | _ -> check "plain stack shape" false)
  | _ -> check "number of stacks" false

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
