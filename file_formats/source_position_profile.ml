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

type kind =
  | Instructions
  | Branches

type error =
  | Cannot_open of string
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string

exception Error of error

let magic_number = "oxcaml-source-position-profile\003"

(* -------------------------------------------------------------------------- *)
(* The cross-side frame contract. *)
(* -------------------------------------------------------------------------- *)

(* The column is clamped to 0 because DWARF represents an absent column as 0, so
   the producer (which decodes DWARF) cannot distinguish an absent column from
   column 0. *)
let frame_string ~file ~line ~col =
  Printf.sprintf "%s:%d:%d" file line (Int.max 0 col)

let frame_string_of_item (item : Debuginfo.item) =
  frame_string ~file:item.dinfo_file ~line:item.dinfo_line
    ~col:item.dinfo_char_start

let hash_frame frame = String.get_int64_le (Digest.string frame) 0

(* -------------------------------------------------------------------------- *)
(* The on-disk format (version 3). *)
(* *)
(* All integers are little-endian; "u32" is an unsigned 32-bit count, "i64" a *)
(* signed 64-bit count and "u64" an unsigned 64-bit absolute file offset. *)
(* *)
(*   header: *)
(*     magic number (its last byte is the format version) *)
(*     buildid: u8 option tag, then u32 length + bytes if present *)
(*     total samples: i64 *)
(*     kind: u8 *)
(*     root index offset: u64 *)
(*     debug map offset: u64 (0 if absent) *)
(*   nodes, each child before its parent: *)
(*     count: i64 *)
(*     number of label entries: u32 *)
(*     label entries, sorted by (disc, edge): u32 disc, u32 edge, i64 count *)
(*     number of children: u32 *)
(*     child entries, sorted by unsigned hash: i64 hash, u64 node offset *)
(*   debug map (optional): u32 entry count, then per entry an i64 hash and a *)
(*     u32 length + bytes position string, sorted by unsigned hash *)
(*   root index, last so its end doubles as a trailing-bytes check: u32 entry *)
(*     count, then root entries like child entries, sorted by unsigned hash *)
(* *)
(* The point of this layout is that a query can descend the trie by reading *)
(* only the entry arrays it searches: nothing needs parsing up front, so a *)
(* profile can be memory-mapped and most of it never touched. Reads are *)
(* bounds-checked and validated lazily, as they happen; [load]/[of_bigstring] *)
(* eagerly validate only the header and the root index placement (which *)
(* includes rejecting trailing bytes). *)
(* -------------------------------------------------------------------------- *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* The raw bytes: a string when the file was read into memory, a bigarray when
   it was memory-mapped. [filename] is carried for error messages. *)
type raw =
  { filename : string;
    data : data
  }

and data =
  | Str of string
  | Big of bigstring

type t =
  { raw : raw;
    buildid : string option;
    total_samples : int64;
    kind : kind;
    (* The offset of the first root-index entry and the number of entries. *)
    roots_pos : int;
    num_roots : int;
    debug_map : (int64, string) Hashtbl.t option Lazy.t
  }

let buildid t = t.buildid

let total_samples t = t.total_samples

let kind t = t.kind

let corrupted raw fmt =
  Printf.ksprintf
    (fun msg -> raise (Error (Corrupted (raw.filename ^ ": " ^ msg))))
    fmt

let data_length = function
  | Str s -> String.length s
  | Big b -> Bigarray.Array1.dim b

(* Every primitive read is bounds-checked, so no possible file contents can make
   a read escape the data (offsets read from the file are additionally
   range-checked in [get_offset], keeping the arithmetic here overflow-free). *)
let check_bounds raw pos len =
  if pos < 0 || len < 0 || pos > data_length raw.data - len
  then corrupted raw "unexpected end of profile data"

let get_u8 raw pos =
  check_bounds raw pos 1;
  match raw.data with
  | Str s -> Char.code (String.unsafe_get s pos)
  | Big b -> Char.code (Bigarray.Array1.unsafe_get b pos)

let get_u32 raw pos =
  check_bounds raw pos 4;
  match raw.data with
  | Str s -> Int32.to_int (String.get_int32_le s pos) land 0xFFFF_FFFF
  | Big b ->
    let byte i = Char.code (Bigarray.Array1.unsafe_get b (pos + i)) in
    byte 0 lor (byte 1 lsl 8) lor (byte 2 lsl 16) lor (byte 3 lsl 24)

let get_i64 raw pos =
  check_bounds raw pos 8;
  match raw.data with
  | Str s -> String.get_int64_le s pos
  | Big b ->
    let byte i =
      Int64.of_int (Char.code (Bigarray.Array1.unsafe_get b (pos + i)))
    in
    let word = ref 0L in
    for i = 7 downto 0 do
      word := Int64.logor (Int64.shift_left !word 8) (byte i)
    done;
    !word

let get_string raw pos len =
  check_bounds raw pos len;
  match raw.data with
  | Str s -> String.sub s pos len
  | Big b -> String.init len (fun i -> Bigarray.Array1.unsafe_get b (pos + i))

let read_count raw pos =
  let count = get_i64 raw pos in
  if Int64.compare count 0L < 0 then corrupted raw "negative count";
  count

(* A file offset: an unsigned 64-bit word that must fall within the data. *)
let get_offset raw pos =
  let offset = get_i64 raw pos in
  if Int64.unsigned_compare offset (Int64.of_int (data_length raw.data)) > 0
  then corrupted raw "file offset %Lu out of bounds" offset;
  Int64.to_int offset

(* -------------------------------------------------------------------------- *)
(* Entry arrays and interpolation search. *)
(* -------------------------------------------------------------------------- *)

(* Root-index and child entries are 16 bytes: the (unsigned) 64-bit frame hash,
   then the node's file offset. *)
let entry_hash raw ~base i = get_i64 raw (base + (16 * i))

let entry_offset raw ~base i = get_offset raw (base + (16 * i) + 8)

let unsigned_to_float hash =
  let f = Int64.to_float hash in
  if Int64.compare hash 0L < 0 then f +. 0x1p64 else f

(* Search an entry array sorted by unsigned hash. Frame hashes are uniformly
   distributed (MD5 prefixes), so interpolation search homes in on the entry in
   O(log log n) probes, touching far fewer pages of a memory-mapped profile than
   binary search would. Probes are clamped strictly inside the remaining range,
   so the range shrinks every step and the search terminates even on corrupted
   (unsorted) data; equal endpoints fall back to bisection. *)
let find_entry raw ~base ~n hash =
  let key i = entry_hash raw ~base i in
  let rec linear i hi =
    if i > hi
    then None
    else if Int64.equal (key i) hash
    then Some i
    else linear (i + 1) hi
  in
  let rec search lo hi =
    if hi - lo < 8
    then linear lo hi
    else
      let klo = key lo in
      if Int64.unsigned_compare hash klo <= 0
      then if Int64.equal hash klo then Some lo else None
      else
        let khi = key hi in
        if Int64.unsigned_compare hash khi >= 0
        then if Int64.equal hash khi then Some hi else None
        else
          let mid =
            if Int64.equal klo khi
            then lo + ((hi - lo) / 2)
            else
              let estimate =
                float_of_int (hi - lo)
                *. ((unsigned_to_float hash -. unsigned_to_float klo)
                   /. (unsigned_to_float khi -. unsigned_to_float klo))
              in
              Int.max (lo + 1) (Int.min (hi - 1) (lo + int_of_float estimate))
          in
          match Int64.unsigned_compare hash (key mid) with
          | 0 -> Some mid
          | c when c < 0 -> search lo (mid - 1)
          | _ -> search (mid + 1) hi
  in
  if n = 0 then None else search 0 (n - 1)

(* -------------------------------------------------------------------------- *)
(* Node handles. *)
(* -------------------------------------------------------------------------- *)

(* A node handle is the byte offset of the node in the raw data. *)
type node = int

let node_count t (node : node) = read_count t.raw node

let node_num_labels t (node : node) = get_u32 t.raw (node + 8)

let node_labels_pos (node : node) = node + 12

(* The number of children and the offset of the first child entry. *)
let node_children t (node : node) =
  let pos = node_labels_pos node + (16 * node_num_labels t node) in
  get_u32 t.raw pos, pos + 4

let find_root t hash =
  match find_entry t.raw ~base:t.roots_pos ~n:t.num_roots hash with
  | None -> None
  | Some i -> Some (entry_offset t.raw ~base:t.roots_pos i)

let find_child t node hash =
  let num_children, entries = node_children t node in
  match find_entry t.raw ~base:entries ~n:num_children hash with
  | None -> None
  | Some i -> Some (entry_offset t.raw ~base:entries i)

(* -------------------------------------------------------------------------- *)
(* Loading. *)
(* -------------------------------------------------------------------------- *)

let check_magic raw =
  let mlen = String.length magic_number in
  if data_length raw.data < mlen then raise (Error (Wrong_format raw.filename));
  let s = get_string raw 0 mlen in
  if not (String.equal s magic_number)
  then
    if
      String.equal
        (String.sub s 0 (mlen - 1))
        (String.sub magic_number 0 (mlen - 1))
    then (* Same producer, different version byte. *)
      raise (Error (Wrong_version raw.filename))
    else raise (Error (Wrong_format raw.filename))

(* The initial table size is capped so a corrupt entry count cannot trigger a
   huge allocation. *)
let capped_table_size n = Int.max 16 (Int.min n (1024 * 1024))

let parse_debug_map raw pos =
  let n = get_u32 raw pos in
  let map = Hashtbl.create (capped_table_size n) in
  let pos = ref (pos + 4) in
  for _ = 1 to n do
    let hash = get_i64 raw !pos in
    let len = get_u32 raw (!pos + 8) in
    let position = get_string raw (!pos + 12) len in
    if Hashtbl.mem map hash
    then corrupted raw "duplicate debug map hash %Lx" hash;
    Hashtbl.add map hash position;
    pos := !pos + 12 + len
  done;
  map

let of_data ~filename data =
  let raw = { filename; data } in
  check_magic raw;
  let pos = ref (String.length magic_number) in
  let u8 () =
    let v = get_u8 raw !pos in
    incr pos;
    v
  in
  let u32 () =
    let v = get_u32 raw !pos in
    pos := !pos + 4;
    v
  in
  let i64 () =
    let v = get_i64 raw !pos in
    pos := !pos + 8;
    v
  in
  let buildid =
    match u8 () with
    | 0 -> None
    | 1 ->
      let len = u32 () in
      let s = get_string raw !pos len in
      pos := !pos + len;
      Some s
    | tag -> corrupted raw "invalid option tag %d" tag
  in
  let total_samples = i64 () in
  if Int64.compare total_samples 0L < 0 then corrupted raw "negative count";
  let kind =
    match u8 () with
    | 0 -> Instructions
    | 1 -> Branches
    | tag -> corrupted raw "invalid profile kind %d" tag
  in
  let root_index_offset = get_offset raw !pos in
  pos := !pos + 8;
  let debug_map_offset = get_offset raw !pos in
  pos := !pos + 8;
  let header_end = !pos in
  if root_index_offset < header_end
  then corrupted raw "root index offset inside the header";
  let num_roots = get_u32 raw root_index_offset in
  let index_end = root_index_offset + 4 + (16 * num_roots) in
  if index_end < data_length data
  then corrupted raw "unexpected trailing bytes"
  else if index_end > data_length data
  then corrupted raw "unexpected end of profile data";
  let debug_map =
    if debug_map_offset = 0
    then Lazy.from_val None
    else (
      if debug_map_offset < header_end || debug_map_offset >= root_index_offset
      then corrupted raw "debug map offset out of bounds";
      lazy (Some (parse_debug_map raw debug_map_offset)))
  in
  { raw;
    buildid;
    total_samples;
    kind;
    roots_pos = root_index_offset + 4;
    num_roots;
    debug_map
  }

let load ~filename =
  match In_channel.with_open_bin filename In_channel.input_all with
  | contents -> of_data ~filename (Str contents)
  | exception Sys_error msg -> raise (Error (Cannot_open msg))

let of_bigstring ~filename data = of_data ~filename (Big data)

(* -------------------------------------------------------------------------- *)
(* Queries. *)
(* -------------------------------------------------------------------------- *)

let count_for_frames t ~frames =
  match frames with
  | [] -> None
  | leaf :: rest -> (
    match find_root t (hash_frame leaf) with
    | None -> Some 0L
    | Some node ->
      (* When an enclosing frame is absent from the children, distinguish two
         cases. If the node has no children at all, the profile recorded nothing
         deeper at this position (for example because of depth truncation, or
         because it was built without call chains): keep the count aggregated
         over all contexts matched so far. If it did record deeper contexts,
         then the queried context is one the profiled program was observed never
         to execute: the count is 0. *)
      let rec descend node = function
        | [] -> node_count t node
        | frame :: rest -> (
          match find_child t node (hash_frame frame) with
          | Some child -> descend child rest
          | None ->
            let num_children, _ = node_children t node in
            if num_children = 0 then node_count t node else 0L)
      in
      Some (descend node rest))

let count_for_debuginfo t dbg =
  match List.rev (Debuginfo.to_items dbg) with
  | [] -> None
  | ({ dinfo_line; _ } : Debuginfo.item) :: _ when dinfo_line <= 0 ->
    (* Not a real source position (see PR#6243), so the profile cannot know
       it. *)
    None
  | items -> count_for_frames t ~frames:(List.map frame_string_of_item items)

(* Descend as far as the frames match. A node's label entries aggregate its
   whole subtree, so stopping early (the profile recorded a shallower or
   diverging context) yields the aggregate over the contexts the query could not
   distinguish. *)
let node_for_frames t ~frames =
  match frames with
  | [] -> None
  | leaf :: rest -> (
    match find_root t (hash_frame leaf) with
    | None -> None
    | Some node ->
      let rec descend node = function
        | [] -> node
        | frame :: rest -> (
          match find_child t node (hash_frame frame) with
          | None -> node
          | Some child -> descend child rest)
      in
      Some (descend node rest))

(* Label entries are sorted by (disc, edge) and there are few per node, so a
   linear scan with early exit suffices. *)
let node_label_count t node ~disc ~edge =
  let n = node_num_labels t node in
  let base = node_labels_pos node in
  let rec go i =
    if i >= n
    then None
    else
      let pos = base + (16 * i) in
      let d = get_u32 t.raw pos in
      let e = get_u32 t.raw (pos + 4) in
      if d = disc && e = edge
      then Some (read_count t.raw (pos + 8))
      else if d > disc || (d = disc && e > edge)
      then None
      else go (i + 1)
  in
  go 0

let node_label_counts t node ~disc =
  let n = node_num_labels t node in
  let base = node_labels_pos node in
  let rec go i acc =
    if i >= n
    then List.rev acc
    else
      let pos = base + (16 * i) in
      let d = get_u32 t.raw pos in
      if d > disc
      then List.rev acc
      else if d < disc
      then go (i + 1) acc
      else
        let edge = get_u32 t.raw (pos + 4) in
        go (i + 1) ((edge, read_count t.raw (pos + 8)) :: acc)
  in
  go 0 []

let label_count_for_frames t ~frames ~disc ~edge =
  match node_for_frames t ~frames with
  | None -> None
  | Some node -> node_label_count t node ~disc ~edge

let label_counts_for_frames t ~frames ~disc =
  match node_for_frames t ~frames with
  | None -> []
  | Some node -> node_label_counts t node ~disc

let frames_of_debuginfo dbg =
  match List.rev (Debuginfo.to_items dbg) with
  | [] -> None
  | ({ dinfo_line; _ } : Debuginfo.item) :: _ when dinfo_line <= 0 -> None
  | items -> Some (List.map frame_string_of_item items)

let label_count_for_debuginfo t dbg ~disc ~edge =
  match frames_of_debuginfo dbg with
  | None -> None
  | Some frames -> label_count_for_frames t ~frames ~disc ~edge

let count_for_branch_label t (label : Debuginfo.branch_label) =
  (* [label_creator] is outermost-first; the trie wants leaf-first. *)
  match List.rev label.label_creator with
  | [] -> None
  | ({ dinfo_line; _ } : Debuginfo.item) :: _ when dinfo_line <= 0 -> None
  | items ->
    label_count_for_frames t
      ~frames:(List.map frame_string_of_item items)
      ~disc:label.label_disc ~edge:label.label_edge

let label_counts_for_debuginfo t dbg ~disc =
  match frames_of_debuginfo dbg with
  | None -> []
  | Some frames -> label_counts_for_frames t ~frames ~disc

let position_of_hash t hash =
  match Lazy.force t.debug_map with
  | None -> None
  | Some map -> Hashtbl.find_opt map hash

let node_labels t node =
  let n = node_num_labels t node in
  let base = node_labels_pos node in
  List.init n (fun i ->
      let pos = base + (16 * i) in
      (get_u32 t.raw pos, get_u32 t.raw (pos + 4)), read_count t.raw (pos + 8))

(* Full traversals double as the deep validation that loading no longer
   performs: they check that entries are strictly sorted (which also rules out
   duplicate siblings) and bound the depth (a corrupt offset graph could
   otherwise recurse forever; real profiles are depth-truncated by the
   producer). *)
let max_reasonable_depth = 1000

let check_sorted t prev hash =
  match prev with
  | Some prev when Int64.unsigned_compare prev hash >= 0 ->
    corrupted t.raw "unsorted or duplicate sibling hash %Lx" hash
  | Some _ | None -> ()

let iter t ~f =
  let rec walk depth hash node =
    if depth > max_reasonable_depth then corrupted t.raw "unreasonable depth";
    f ~hash ~depth ~count:(node_count t node) ~labels:(node_labels t node);
    let num_children, entries = node_children t node in
    let prev = ref None in
    for i = 0 to num_children - 1 do
      let child_hash = entry_hash t.raw ~base:entries i in
      check_sorted t !prev child_hash;
      prev := Some child_hash;
      walk (depth + 1) child_hash (entry_offset t.raw ~base:entries i)
    done
  in
  let prev = ref None in
  for i = 0 to t.num_roots - 1 do
    let hash = entry_hash t.raw ~base:t.roots_pos i in
    check_sorted t !prev hash;
    prev := Some hash;
    walk 1 hash (entry_offset t.raw ~base:t.roots_pos i)
  done

(* -------------------------------------------------------------------------- *)
(* Statistics. *)
(* -------------------------------------------------------------------------- *)

let print_stats ppf t =
  let num_nodes = ref 0 in
  let num_leaves = ref 0 in
  let num_labels = ref 0 in
  let max_depth = ref 0 in
  let sum = ref 0L in
  let rec walk depth node =
    if depth > max_reasonable_depth then corrupted t.raw "unreasonable depth";
    incr num_nodes;
    sum := Int64.add !sum (node_count t node);
    if depth > !max_depth then max_depth := depth;
    if depth = 1 then num_labels := !num_labels + node_num_labels t node;
    let num_children, entries = node_children t node in
    if num_children = 0 then incr num_leaves;
    for i = 0 to num_children - 1 do
      walk (depth + 1) (entry_offset t.raw ~base:entries i)
    done
  in
  for i = 0 to t.num_roots - 1 do
    walk 1 (entry_offset t.raw ~base:t.roots_pos i)
  done;
  Format.fprintf ppf
    "@[<v>source-position profile:@,\
    \  build id: %s@,\
    \  kind: %s@,\
    \  total samples: %Ld@,\
    \  roots: %d@,\
    \  nodes: %d (leaves: %d)@,\
    \  labels at roots: %d@,\
    \  observed depth: %d@,\
    \  sum of node counts: %Ld@,\
    \  debug map: %s@]"
    (match t.buildid with None -> "<none>" | Some b -> b)
    (match t.kind with
    | Instructions -> "instruction samples"
    | Branches -> "branch executions")
    t.total_samples t.num_roots !num_nodes !num_leaves !num_labels !max_depth
    !sum
    (match Lazy.force t.debug_map with
    | None -> "absent"
    | Some map -> Printf.sprintf "%d entries" (Hashtbl.length map))

(* -------------------------------------------------------------------------- *)
(* Writer. *)
(* -------------------------------------------------------------------------- *)

module Writer = struct
  type wnode =
    { mutable acc : int64;
      wlabels : (int * int, int64) Hashtbl.t;
      next : (int64, wnode) Hashtbl.t
    }

  type t =
    { forest : (int64, wnode) Hashtbl.t;
      positions : (int64, string) Hashtbl.t
    }

  let create () =
    { forest = Hashtbl.create 1024; positions = Hashtbl.create 1024 }

  (* Since the writer sees the frame strings, it can (and does) detect 64-bit
     hash collisions, which would silently merge two positions. *)
  let intern t frame =
    let hash = hash_frame frame in
    (match Hashtbl.find_opt t.positions hash with
    | None -> Hashtbl.add t.positions hash frame
    | Some existing ->
      if not (String.equal existing frame)
      then
        Misc.fatal_errorf
          "Source_position_profile: frame hash collision between %S and %S"
          existing frame);
    hash

  let find_or_add_node table hash =
    match Hashtbl.find_opt table hash with
    | Some node -> node
    | None ->
      let node =
        { acc = 0L; wlabels = Hashtbl.create 1; next = Hashtbl.create 4 }
      in
      Hashtbl.add table hash node;
      node

  let add_stack t ~frames ~count ~max_depth =
    let rec go table depth = function
      | frame :: rest when depth < max_depth ->
        let node = find_or_add_node table (intern t frame) in
        node.acc <- Int64.add node.acc count;
        go node.next (depth + 1) rest
      | [] | _ :: _ -> ()
    in
    go t.forest 0 frames

  let add_label_at (node : wnode) ~disc ~edge ~count =
    let key = disc, edge in
    let existing =
      Option.value (Hashtbl.find_opt node.wlabels key) ~default:0L
    in
    Hashtbl.replace node.wlabels key (Int64.add existing count)

  let add_label t ~frames ~disc ~edge ~count ~max_depth =
    let rec go table depth = function
      | frame :: rest when depth < max_depth ->
        let node = find_or_add_node table (intern t frame) in
        add_label_at node ~disc ~edge ~count;
        go node.next (depth + 1) rest
      | [] | _ :: _ -> ()
    in
    go t.forest 0 frames

  let add_label_hashes t ~frame_hashes ~disc ~edge ~count ~max_depth =
    let rec go table depth = function
      | hash :: rest when depth < max_depth ->
        let node = find_or_add_node table hash in
        add_label_at node ~disc ~edge ~count;
        go node.next (depth + 1) rest
      | [] | _ :: _ -> ()
    in
    go t.forest 0 frame_hashes

  (* Sorted by unsigned hash, as the entry arrays of the format require. *)
  let sorted_entries table =
    let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table [] in
    List.sort (fun (k1, _) (k2, _) -> Int64.unsigned_compare k1 k2) entries

  let sorted_labels wlabels =
    let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) wlabels [] in
    List.sort
      (fun ((disc1, edge1), _) ((disc2, edge2), _) ->
        match Int.compare disc1 disc2 with
        | 0 -> Int.compare edge1 edge2
        | c -> c)
      entries

  let add_u8 buf n = Buffer.add_uint8 buf n

  let add_u32 buf n = Buffer.add_int32_le buf (Int32.of_int n)

  let add_i64 buf n = Buffer.add_int64_le buf n

  let add_offset buf n = Buffer.add_int64_le buf (Int64.of_int n)

  let add_string buf s =
    add_u32 buf (String.length s);
    Buffer.add_string buf s

  (* Emit a node's subtree into [body], children first so their offsets are
     known when the node's child entries are written, and return the node's
     absolute file offset ([body] starts at file offset [base]). *)
  let rec emit_node body ~base (node : wnode) =
    let children =
      List.map
        (fun (hash, child) -> hash, emit_node body ~base child)
        (sorted_entries node.next)
    in
    let offset = base + Buffer.length body in
    add_i64 body node.acc;
    let labels = sorted_labels node.wlabels in
    add_u32 body (List.length labels);
    List.iter
      (fun ((disc, edge), count) ->
        add_u32 body disc;
        add_u32 body edge;
        add_i64 body count)
      labels;
    add_u32 body (List.length children);
    List.iter
      (fun (hash, child_offset) ->
        add_i64 body hash;
        add_offset body child_offset)
      children;
    offset

  let serialize t ~buildid ~total_samples ~kind ~debug_map =
    let header_size =
      String.length magic_number
      + (1 (* buildid tag *)
        + match buildid with None -> 0 | Some s -> 4 + String.length s)
      + 8 (* total samples *) + 1
      (* kind *) + 8 (* root index offset *)
      + 8 (* debug map offset *)
    in
    let body = Buffer.create 65536 in
    let roots =
      List.map
        (fun (hash, node) -> hash, emit_node body ~base:header_size node)
        (sorted_entries t.forest)
    in
    let debug_map_offset =
      if not debug_map
      then 0
      else
        let offset = header_size + Buffer.length body in
        let entries = sorted_entries t.positions in
        add_u32 body (List.length entries);
        List.iter
          (fun (hash, position) ->
            add_i64 body hash;
            add_string body position)
          entries;
        offset
    in
    let root_index_offset = header_size + Buffer.length body in
    add_u32 body (List.length roots);
    List.iter
      (fun (hash, offset) ->
        add_i64 body hash;
        add_offset body offset)
      roots;
    let header = Buffer.create header_size in
    Buffer.add_string header magic_number;
    (match buildid with
    | None -> add_u8 header 0
    | Some s ->
      add_u8 header 1;
      add_string header s);
    add_i64 header total_samples;
    (match kind with
    | Instructions -> add_u8 header 0
    | Branches -> add_u8 header 1);
    add_offset header root_index_offset;
    add_offset header debug_map_offset;
    assert (Buffer.length header = header_size);
    Buffer.contents header ^ Buffer.contents body

  let write t ~filename ~buildid ~total_samples ~kind ~debug_map =
    let contents = serialize t ~buildid ~total_samples ~kind ~debug_map in
    Out_channel.with_open_bin filename (fun oc ->
        Out_channel.output_string oc contents)

  let to_profile t ~buildid ~total_samples ~kind ~debug_map =
    of_data ~filename:"<in-memory profile>"
      (Str (serialize t ~buildid ~total_samples ~kind ~debug_map))
end

(* -------------------------------------------------------------------------- *)
(* Error reporting. *)
(* -------------------------------------------------------------------------- *)

let report_error ppf error =
  let open Format_doc in
  match error with
  | Cannot_open msg ->
    fprintf ppf "Cannot open source-position profile:@ %s" msg
  | Wrong_format filename ->
    fprintf ppf "Not a source-position profile:@ %a"
      Location.Doc.quoted_filename filename
  | Wrong_version filename ->
    fprintf ppf "%a@ is an incompatible source-position profile version"
      Location.Doc.quoted_filename filename
  | Corrupted msg -> fprintf ppf "Corrupted source-position profile:@ %s" msg

let () =
  Location.register_error_of_exn (function
    | Error err -> Some (Location.error_of_printer_file report_error err)
    | _ -> None)
