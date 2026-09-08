(* Unit tests for the parsing in the oxcaml-fdo-decode tool ([Fdo_decode_lib]),
   on canned "perf script" output and FDO metadata section bytes. *)

module Metadata = Fdo_decode_lib.Metadata
module Perf_script = Fdo_decode_lib.Perf_script

let failures = ref 0

let check name cond =
  if not cond
  then (
    incr failures;
    Printf.eprintf "FAILED: %s\n%!" name)

(* Aggregate canned "perf script" output. *)
let of_string ?(ip_ranges = true) ?(metadata = Metadata.empty) script_output =
  let filename = Filename.temp_file "fdo_decode_test" ".txt" in
  Fun.protect
    ~finally:(fun () -> Sys.remove filename)
    (fun () ->
      Out_channel.with_open_text filename (fun oc ->
          Out_channel.output_string oc script_output);
      In_channel.with_open_text filename
        (Perf_script.of_channel ~ip_ranges ~metadata))

let count table key = Option.value (Hashtbl.find_opt table key) ~default:0L

(* The count of the taken branches from an address. *)
let taken branches source =
  Hashtbl.fold
    (fun (s, _) c acc -> if Int64.equal s source then Int64.add acc c else acc)
    branches 0L

(* Branch stacks: taken branches and sequential ranges. *)
let () =
  let events =
    of_string
      (String.concat "\n"
         [ "  2 0x401000/0x401100/P/-/-/0/COND/- 0x7f0000000000/0x401000/P/-/-/0";
           "  3 0x401000/0x401100/M/-/-/1";
           "0x401200/0x401300/P/-/-/0 0x401000/0x401006/M/-/-/0";
           "  5 0x401400/0x401000/P/-/-/0 0x401300/0x401400/P/-/-/0";
           "  1 0x401500/0x401600/P/-/-/0 0x401700/0x401800/P/-/-/0";
           "         4 ";
           "" ])
  in
  check "taken sources" (Int64.equal (taken events.branches 0x401000L) 6L);
  check "branches by source and target"
    (Int64.equal (count events.branches (0x401000L, 0x401100L)) 5L
    && Int64.equal (count events.branches (0x401000L, 0x401006L)) 1L);
  check "foreign sources kept for the metadata to reject"
    (Int64.equal (taken events.branches 0x7f0000000000L) 2L);
  check "range from the older target up to the newer source"
    (Int64.equal (count events.ranges (0x401006L, 0x401200L)) 1L);
  check "range of a single branch instruction"
    (Int64.equal (count events.ranges (0x401400L, 0x401400L)) 5L
    && Int64.equal (count events.ranges (0x401000L, 0x401000L)) 2L);
  check "no range when the target is above the source"
    (Hashtbl.length events.ranges = 3);
  check "garbage aborts"
    (match of_string "not a sample" with
    | exception Failure _ -> true
    | (_ : Perf_script.t) -> false);
  check "garbage branch entry aborts"
    (match of_string "x/y/z" with
    | exception Failure _ -> true
    | (_ : Perf_script.t) -> false);
  (* The "-F period,ip,brstack" shapes: with the ip on the sample's line, and
     with a call chain (the ip and return addresses on their own lines, the
     branch stack last). *)
  let events =
    of_string
      (String.concat "\n"
         [ "    677874            401050 \
            0x401000/0x401100/P/-/-/0//NON_SPEC_CORRECT_PATH  \
            0x401200/0x401000/P/-/-/0//NON_SPEC_CORRECT_PATH ";
           "         7 ";
           "\t          401234";
           "\t    7f27f3699bc1";
           " 0x401000/0x401100/P/-/-/0//NON_SPEC_CORRECT_PATH  \
            0x401200/0x401000/P/-/-/0//NON_SPEC_CORRECT_PATH ";
           "" ])
  in
  check "ip on the sample line"
    (Int64.equal (taken events.branches 0x401200L) 677881L);
  check "period carried over the call chain"
    (Int64.equal (count events.ranges (0x401000L, 0x401000L)) 677881L);
  check "range from the most recent target up to the ip"
    (Int64.equal (count events.ranges (0x401100L, 0x401234L)) 7L);
  check "no range when the ip is below the most recent target"
    (Hashtbl.length events.ranges = 2);
  check "no ip ranges unless asked"
    (Hashtbl.length
       (of_string ~ip_ranges:false "  7      401234 0x401000/0x401100/P/-/-/0")
         .ranges
    = 0);
  check "call chain frame without a sample aborts"
    (match of_string "\t          401234" with
    | exception Failure _ -> true
    | (_ : Perf_script.t) -> false)

(* Calls seen through stubs: a call site at 0x4010 lands on 0x9000 (no entry
   metadata: a stub), which branches within itself, then onto the entry 0x4000:
   recorded as a branch from the call site to the entry instead. A stub that
   returns without calling (the caller's labelled branch at 0x4008 follows)
   attributes nothing; nor does a call that lands on an entry directly. *)
let () =
  let metadata = Metadata.empty in
  Hashtbl.replace metadata.callsite 0x4010L [[0xeeeeL]];
  Hashtbl.replace metadata.target 0x4000L [[0x1111L]];
  Hashtbl.replace metadata.taken 0x4008L [[0xaaaaL]];
  let events =
    of_string ~ip_ranges:false ~metadata
      (String.concat "\n"
         [ (* most recent first: stub -> entry, stub -> stub, call -> stub *)
           "  1 0x9010/0x4000/P/-/-/0 0x9004/0x9008/P/-/-/0 \
            0x4010/0x9000/P/-/-/0";
           "  1 0x4008/0x4020/P/-/-/0 0x9010/0x4020/P/-/-/0 \
            0x4010/0x9000/P/-/-/0";
           "  1 0x9010/0x4000/P/-/-/0 0x4008/0x4020/P/-/-/0 \
            0x9010/0x4020/P/-/-/0 0x4010/0x9000/P/-/-/0";
           "  1 0x4010/0x4000/P/-/-/0";
           "" ])
  in
  check "call seen through the stub"
    (Int64.equal (count events.branches (0x4010L, 0x4000L)) 2L);
  check "the stub's own branch onto the entry is replaced"
    (Int64.equal (count events.branches (0x9010L, 0x4000L)) 1L);
  check "call into the stub kept"
    (Int64.equal (count events.branches (0x4010L, 0x9000L)) 3L)

(* Metadata.parse, iter_range and branch_locations. *)
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
  let item tag address locations =
    uleb tag;
    u64 address;
    uleb (List.length locations);
    List.iter
      (fun l ->
        uleb (List.length l);
        List.iter u64 l)
      locations
  in
  (* First compilation unit: a function entry (with the rider of a call inlined
     at its head), a labelled branch, and a call. *)
  Buffer.add_string buffer "FDOM";
  uleb 7 (* version *);
  item 2 0x4000L [[0x1111L]; [0x3333L; 0x4444L]];
  item 0 0x4008L [[0xaaaaL]];
  item 1 0x4008L [[0xbbbbL]];
  item 3 0x4010L [[0xeeeeL]];
  (* Second compilation unit: a taken item with a multi-byte count, and a second
     fallthrough item at an address of the first unit. *)
  Buffer.add_string buffer "FDOM";
  uleb 7;
  item 0 0x5000L (List.init 130 (fun i -> [Int64.of_int i]));
  item 1 0x4008L [[0xccccL]];
  (* A name (-fdo-names). *)
  uleb 4;
  u64 0xbbbbL;
  uleb 2;
  List.iter
    (fun component ->
      uleb (String.length component);
      Buffer.add_string buffer component)
    ["a.ml"; "1"];
  let metadata = Metadata.parse (Buffer.contents buffer) in
  let sorted = List.sort compare in
  check "name"
    (Option.equal (List.equal String.equal)
       (Hashtbl.find_opt metadata.names 0xbbbbL)
       (Some ["a.ml"; "1"]));
  check "taken labels"
    (Option.equal
       (List.equal (List.equal Int64.equal))
       (Hashtbl.find_opt metadata.taken 0x4008L)
       (Some [[0xaaaaL]]));
  check "multi-byte count"
    (Option.equal ( = )
       (Option.map List.length (Hashtbl.find_opt metadata.taken 0x5000L))
       (Some 130));
  let between lo hi =
    let acc = ref [] in
    Metadata.iter_range metadata ~lo ~hi (fun l -> acc := l :: !acc);
    sorted (List.concat !acc)
  in
  check "range start included" (between 0x4008L 0x4010L = [[0xbbbbL]; [0xccccL]]);
  check "range end excluded for fallthrough" (between 0x4001L 0x4008L = []);
  check "items at one address merged"
    (between 0x4001L 0x4009L = [[0xbbbbL]; [0xccccL]]);
  let branch ~source ~target =
    sorted (Metadata.branch_locations metadata ~source ~target)
  in
  check "taken branch" (branch ~source:0x4008L ~target:0x4020L = [[0xaaaaL]]);
  check "call: entry labels extended by the call site"
    (branch ~source:0x4010L ~target:0x4000L
    = [[0x1111L; 0xeeeeL]; [0x3333L; 0x4444L; 0xeeeeL]]);
  check "entry from an unlabelled source"
    (branch ~source:0x4030L ~target:0x4000L = [[0x1111L]; [0x3333L; 0x4444L]]);
  check "unlabelled branch" (branch ~source:0x4030L ~target:0x4040L = []);
  check "call targets: the entry only, not its riders"
    (Metadata.call_targets metadata ~source:0x4010L ~target:0x4000L
     = [0xeeeeL, 0x1111L]
    && Metadata.call_targets metadata ~source:0x4030L ~target:0x4000L = []);
  check "bad magic aborts"
    (match Metadata.parse "XXXX" with
    | exception Failure _ -> true
    | (_ : Metadata.t) -> false);
  check "truncated section aborts"
    (match Metadata.parse "FDOM\x07\x00\x00\x40" with
    | exception Failure _ -> true
    | (_ : Metadata.t) -> false)

let () =
  if !failures > 0
  then (
    Printf.eprintf "%d test(s) failed\n%!" !failures;
    exit 1)
  else print_endline "All tests passed"
