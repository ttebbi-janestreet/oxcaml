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

type source_frame = string

module Frame_map = Map.Make (String)

(* A trie node. [children] is keyed by the (unique) frame on the outgoing edge;
   the format forbids two sibling edges with the same frame, and [load] rejects
   any that occur. The per-node fan-out is small, so a [Map] is used. *)
type node =
  { node_count : int;
    children : node Frame_map.t
  }

(* [known_positions] is the set of source positions the profile knows about. A
   single set suffices because the same DWARF [.loc] identifies a position
   whether it is the actual/most-inlined execution point or a caller's call
   site. A queried frame absent from this set is "unknown" and handled
   approximately (see {!estimate}).

   The root of the trie has very high fan-out (one child per executed actual
   position), so its children use a hash table; deeper nodes (small fan-out) use
   maps. [root_count] is the count stored at the root node itself. *)
type t =
  { depth : int;
    buildid : string option;
    (* The total number of calls (actual + virtual/inlined) recorded by the
       profile. This is the normalizer used by {!estimate}: a call site's count
       as a fraction of all calls. It is recorded separately by the producer
       rather than derived from the trie, since the trie may omit calls (e.g.
       below a threshold, or at unknown positions). *)
    total_calls : int;
    known_positions : (source_frame, unit) Hashtbl.t;
    root_count : int;
    root_children : (source_frame, node) Hashtbl.t
  }

type stats =
  { num_nodes : int;
    num_leaves : int;
    max_depth_observed : int;
    total_count : int;
    sum_node_counts : int;
    distinct_frames : int
  }

type error =
  | Cannot_open of string
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string

exception Error of error

let magic_number = "ocamlfdo-source-stack-profile\002"

let depth t = t.depth

let buildid t = t.buildid

let total_count t = t.root_count

let total_calls t = t.total_calls

(* -------------------------------------------------------------------------- *)
(* Binary reader.                                                             *)
(*                                                                            *)
(* The file is read sequentially from a buffered channel (never loaded whole  *)
(* into memory, since the profile may be large). All integers are             *)
(* little-endian. Every read is bounds-checked against the remaining bytes and *)
(* raises [Error (Corrupted _)] on a short read.                              *)
(* -------------------------------------------------------------------------- *)

type cursor =
  { filename : string;
    ic : In_channel.t
  }

let corrupted c fmt =
  Printf.ksprintf
    (fun msg -> raise (Error (Corrupted (c.filename ^ ": " ^ msg))))
    fmt

let remaining c = Int64.sub (In_channel.length c.ic) (In_channel.pos c.ic)

(* Read exactly [n] bytes. Bounds-checking [n] against the bytes remaining
   before reading means a corrupt length cannot trigger an oversized
   allocation. *)
let read_bytes c n =
  if Int64.compare (Int64.of_int n) (remaining c) > 0
  then corrupted c "unexpected end of profile data";
  match In_channel.really_input_string c.ic n with
  | Some s -> s
  | None -> corrupted c "unexpected end of profile data"

let read_u8 c =
  match In_channel.input_byte c.ic with
  | Some b -> b
  | None -> corrupted c "unexpected end of profile data"

let read_u32 c =
  Int32.to_int (String.get_int32_le (read_bytes c 4) 0) land 0xFFFF_FFFF

let read_int c = Int64.to_int (String.get_int64_le (read_bytes c 8) 0)

let read_string c =
  let len = read_u32 c in
  read_bytes c len

(* A set of frames: [u32] count followed by that many strings. *)
let read_set c =
  let n = read_u32 c in
  let set = Hashtbl.create (Int.max 16 (Int.min n (1024 * 1024))) in
  for _ = 1 to n do
    let frame = read_string c in
    if Hashtbl.mem set frame then corrupted c "duplicate set entry %s" frame;
    Hashtbl.add set frame ()
  done;
  set

let read_option c read_elt =
  match read_u8 c with
  | 0 -> None
  | 1 -> Some (read_elt c)
  | tag -> corrupted c "invalid option tag %d" tag

let rec read_node c =
  let count = read_int c in
  let num_children = read_u32 c in
  let children = ref Frame_map.empty in
  for _ = 1 to num_children do
    let frame = read_string c in
    let child = read_node c in
    if Frame_map.mem frame !children
    then corrupted c "duplicate sibling frame %s" frame;
    children := Frame_map.add frame child !children
  done;
  { node_count = count; children = !children }

let check_magic c =
  let mlen = String.length magic_number in
  match In_channel.really_input_string c.ic mlen with
  | Some s when String.equal s magic_number -> ()
  | Some s
    when String.equal
           (String.sub s 0 (mlen - 1))
           (String.sub magic_number 0 (mlen - 1)) ->
    (* Same producer, different version byte. *)
    raise (Error (Wrong_version c.filename))
  | Some _ | None -> raise (Error (Wrong_format c.filename))

let parse ~filename ic =
  let c = { filename; ic } in
  check_magic c;
  let depth = read_u32 c in
  let buildid = read_option c read_string in
  let total_calls = read_int c in
  let known_positions = read_set c in
  (* The root node has high fan-out, so read its children into a hash table
     (deeper nodes are read by [read_node] into maps). The initial size is
     capped so a corrupt [num_children] cannot trigger a huge allocation. *)
  let root_count = read_int c in
  let num_children = read_u32 c in
  let root_children =
    Hashtbl.create (Int.max 16 (Int.min num_children (1024 * 1024)))
  in
  for _ = 1 to num_children do
    let frame = read_string c in
    let child = read_node c in
    if Hashtbl.mem root_children frame
    then corrupted c "duplicate sibling frame %s" frame;
    Hashtbl.add root_children frame child
  done;
  (match In_channel.input_char c.ic with
  | None -> ()
  | Some _ -> corrupted c "unexpected trailing bytes");
  { depth; buildid; total_calls; known_positions; root_count; root_children }

let load ~filename =
  match In_channel.with_open_bin filename (fun ic -> parse ~filename ic) with
  | t -> t
  | exception Sys_error msg -> raise (Error (Cannot_open msg))

(* -------------------------------------------------------------------------- *)
(* Lookup.                                                                    *)
(* -------------------------------------------------------------------------- *)

let rec descend (node : node) = function
  | [] -> Some node
  | frame :: rest -> (
    match Frame_map.find_opt frame node.children with
    | None -> None
    | Some child -> descend child rest)

let find_count t stack =
  match stack with
  | [] -> Some t.root_count
  | leaf :: rest -> (
    match Hashtbl.find_opt t.root_children leaf with
    | None -> None
    | Some node -> (
      match descend node rest with
      | None -> None
      | Some node -> Some node.node_count))

let estimate t stack : int option =
  (* Descend the trie, matching deeper (caller) frames against children. A known
     frame absent from this node's children means that context was never
     observed (the profile records every observed context), so its count is 0. A
     frame that is not a known position still occupies a trie level, but we
     cannot tell which child it is, so we approximate by descending into all
     children and summing their estimates. *)
  let rec descend (node : node) = function
    | [] -> node.node_count
    | frame :: rest ->
      if Hashtbl.mem t.known_positions frame
      then
        match Frame_map.find_opt frame node.children with
        | Some child -> descend child rest
        | None -> 0
      else
        Frame_map.fold
          (fun _frame child acc -> acc + descend child rest)
          node.children 0
  in
  match stack with
  | [] -> None
  | leaf :: rest -> (
    if
      (* The leaf is the actual / most-inlined position. If it is unknown to the
         profile, return [None] so the consumer applies its own default. *)
      not (Hashtbl.mem t.known_positions leaf)
    then None
    else
      match Hashtbl.find_opt t.root_children leaf with
      | None -> Some 0 (* known position, but it never executed *)
      | Some node -> Some (descend node rest))

(* -------------------------------------------------------------------------- *)
(* Statistics.                                                                *)
(* -------------------------------------------------------------------------- *)

let dump_stats t =
  let frames = Hashtbl.create 64 in
  (* Count the root node itself, then walk its (hash-table) children at depth 1
     and the deeper (map) nodes recursively. *)
  let num_nodes = ref 1 in
  let num_leaves = ref (if Hashtbl.length t.root_children = 0 then 1 else 0) in
  let max_depth = ref 0 in
  let sum = ref t.root_count in
  let rec walk d (node : node) =
    incr num_nodes;
    sum := !sum + node.node_count;
    if d > !max_depth then max_depth := d;
    if Frame_map.is_empty node.children then incr num_leaves;
    Frame_map.iter
      (fun (frame : source_frame) child ->
        Hashtbl.replace frames frame ();
        walk (d + 1) child)
      node.children
  in
  Hashtbl.iter
    (fun frame child ->
      Hashtbl.replace frames frame ();
      walk 1 child)
    t.root_children;
  { num_nodes = !num_nodes;
    num_leaves = !num_leaves;
    max_depth_observed = !max_depth;
    total_count = t.root_count;
    sum_node_counts = !sum;
    distinct_frames = Hashtbl.length frames
  }

let print_stats ppf t =
  let s = dump_stats t in
  Format.fprintf ppf
    "@[<v>source-stack profile:@,\
    \  depth bound: %d@,\
    \  build id: %s@,\
    \  total calls: %i@,\
    \  nodes: %d (leaves: %d)@,\
    \  observed depth: %d@,\
    \  root count: %i@,\
    \  sum of node counts: %i@,\
    \  distinct frames: %d@]"
    (depth t)
    (match buildid t with None -> "<none>" | Some b -> b)
    (total_calls t) s.num_nodes s.num_leaves s.max_depth_observed s.total_count
    s.sum_node_counts s.distinct_frames

(* -------------------------------------------------------------------------- *)
(* Querying from compiler debug info.                                         *)
(* -------------------------------------------------------------------------- *)

let frame_of_item (item : Debuginfo.item) =
  (* [dinfo_char_start] is already a 0-based column; it is emitted to DWARF only
     when non-negative, so include the column only in that case. This string
     format must match what the producer (ocamlfdo) writes. *)
  if item.dinfo_char_start >= 0
  then
    Printf.sprintf "%s:%d:%d" item.dinfo_file item.dinfo_line
      item.dinfo_char_start
  else Printf.sprintf "%s:%d" item.dinfo_file item.dinfo_line

let frames_of_debuginfo dbg =
  (* [Debuginfo.to_items] is outermost-first, so reverse to get leaf-first. *)
  List.rev_map frame_of_item (Debuginfo.to_items dbg)

let estimate_for_debuginfo t dbg = estimate t (frames_of_debuginfo dbg)

(* -------------------------------------------------------------------------- *)
(* Error reporting.                                                           *)
(* -------------------------------------------------------------------------- *)

let report_error ppf error =
  let open Format_doc in
  match error with
  | Cannot_open msg -> fprintf ppf "Cannot open source-stack profile:@ %s" msg
  | Wrong_format filename ->
    fprintf ppf "Not a source-stack profile:@ %a" Location.Doc.quoted_filename
      filename
  | Wrong_version filename ->
    fprintf ppf "%a@ is an incompatible source-stack profile version"
      Location.Doc.quoted_filename filename
  | Corrupted msg -> fprintf ppf "Corrupted source-stack profile:@ %s" msg

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
