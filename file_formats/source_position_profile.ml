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

module Hash_map = Map.Make (Int64)

(* A trie node. [children] is keyed by the hash on the outgoing edge, matching
   one more enclosing frame of the inlining/call stack; the format forbids two
   sibling edges with the same hash, and [load] rejects any that occur. The
   per-node fan-out is small, so a [Map] is used. The forest roots (one per
   most-inlined position, so very high fan-out) live in a hash table instead. *)
type node =
  { count : int64;
    children : node Hash_map.t
  }

type t =
  { buildid : string option;
    total_samples : int64;
    roots : (int64, node) Hashtbl.t;
    debug_map : (int64, string) Hashtbl.t option
  }

type error =
  | Cannot_open of string
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string

exception Error of error

let magic_number = "oxcaml-source-position-profile\001"

let buildid t = t.buildid

let total_samples t = t.total_samples

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
(* Binary reader. *)
(* *)
(* The file is read sequentially from a buffered channel (never loaded whole *)
(* into memory, since the profile may be large). All integers are *)
(* little-endian. Every read is bounds-checked against the remaining bytes *)
(* and raises [Error (Corrupted _)] on a short read. *)
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

let read_i64 c = String.get_int64_le (read_bytes c 8) 0

let read_count c =
  let count = read_i64 c in
  if Int64.compare count 0L < 0 then corrupted c "negative count";
  count

let read_string c =
  let len = read_u32 c in
  read_bytes c len

let read_option c read_elt =
  match read_u8 c with
  | 0 -> None
  | 1 -> Some (read_elt c)
  | tag -> corrupted c "invalid option tag %d" tag

let rec read_node c =
  let count = read_count c in
  let num_children = read_u32 c in
  let children = ref Hash_map.empty in
  for _ = 1 to num_children do
    let hash = read_i64 c in
    let child = read_node c in
    if Hash_map.mem hash !children
    then corrupted c "duplicate sibling hash %Lx" hash;
    children := Hash_map.add hash child !children
  done;
  { count; children = !children }

(* The initial table sizes below are capped so a corrupt entry count cannot
   trigger a huge allocation. *)
let capped_table_size n = Int.max 16 (Int.min n (1024 * 1024))

let read_debug_map c =
  let n = read_u32 c in
  let map = Hashtbl.create (capped_table_size n) in
  for _ = 1 to n do
    let hash = read_i64 c in
    let position = read_string c in
    if Hashtbl.mem map hash then corrupted c "duplicate debug map hash %Lx" hash;
    Hashtbl.add map hash position
  done;
  map

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
  let buildid = read_option c read_string in
  let total_samples = read_count c in
  let num_roots = read_u32 c in
  let roots = Hashtbl.create (capped_table_size num_roots) in
  for _ = 1 to num_roots do
    let hash = read_i64 c in
    let node = read_node c in
    if Hashtbl.mem roots hash then corrupted c "duplicate root hash %Lx" hash;
    Hashtbl.add roots hash node
  done;
  let debug_map = read_option c read_debug_map in
  (match In_channel.input_char c.ic with
  | None -> ()
  | Some _ -> corrupted c "unexpected trailing bytes");
  { buildid; total_samples; roots; debug_map }

let load ~filename =
  match In_channel.with_open_bin filename (fun ic -> parse ~filename ic) with
  | t -> t
  | exception Sys_error msg -> raise (Error (Cannot_open msg))

(* -------------------------------------------------------------------------- *)
(* Queries. *)
(* -------------------------------------------------------------------------- *)

let count_for_frames t ~frames =
  match frames with
  | [] -> None
  | leaf :: rest -> (
    match Hashtbl.find_opt t.roots (hash_frame leaf) with
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
        | [] -> node.count
        | frame :: rest -> (
          match Hash_map.find_opt (hash_frame frame) node.children with
          | None -> if Hash_map.is_empty node.children then node.count else 0L
          | Some child -> descend child rest)
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

let position_of_hash t hash =
  match t.debug_map with None -> None | Some map -> Hashtbl.find_opt map hash

let iter t ~f =
  let rec walk depth hash (node : node) =
    f ~hash ~depth ~count:node.count;
    Hash_map.iter (fun hash child -> walk (depth + 1) hash child) node.children
  in
  let roots =
    Hashtbl.fold (fun hash node acc -> (hash, node) :: acc) t.roots []
  in
  let roots =
    List.sort (fun (hash1, _) (hash2, _) -> Int64.compare hash1 hash2) roots
  in
  List.iter (fun (hash, node) -> walk 1 hash node) roots

(* -------------------------------------------------------------------------- *)
(* Statistics. *)
(* -------------------------------------------------------------------------- *)

let print_stats ppf t =
  let num_nodes = ref 0 in
  let num_leaves = ref 0 in
  let max_depth = ref 0 in
  let sum = ref 0L in
  let rec walk depth (node : node) =
    incr num_nodes;
    sum := Int64.add !sum node.count;
    if depth > !max_depth then max_depth := depth;
    if Hash_map.is_empty node.children then incr num_leaves;
    Hash_map.iter (fun _hash child -> walk (depth + 1) child) node.children
  in
  Hashtbl.iter (fun _hash node -> walk 1 node) t.roots;
  Format.fprintf ppf
    "@[<v>source-position profile:@,\
    \  build id: %s@,\
    \  total samples: %Ld@,\
    \  roots: %d@,\
    \  nodes: %d (leaves: %d)@,\
    \  observed depth: %d@,\
    \  sum of node counts: %Ld@,\
    \  debug map: %s@]"
    (match t.buildid with None -> "<none>" | Some b -> b)
    t.total_samples (Hashtbl.length t.roots) !num_nodes !num_leaves !max_depth
    !sum
    (match t.debug_map with
    | None -> "absent"
    | Some map -> Printf.sprintf "%d entries" (Hashtbl.length map))

(* -------------------------------------------------------------------------- *)
(* Writer. *)
(* -------------------------------------------------------------------------- *)

module Writer = struct
  type wnode =
    { mutable acc : int64;
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
      let node = { acc = 0L; next = Hashtbl.create 4 } in
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

  let sorted_entries table =
    let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table [] in
    List.sort (fun (k1, _) (k2, _) -> Int64.compare k1 k2) entries

  let write_u8 oc n = Out_channel.output_byte oc n

  let write_u32 oc n =
    let b = Bytes.create 4 in
    Bytes.set_int32_le b 0 (Int32.of_int n);
    Out_channel.output_bytes oc b

  let write_i64 oc n =
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 n;
    Out_channel.output_bytes oc b

  let write_string oc s =
    write_u32 oc (String.length s);
    Out_channel.output_string oc s

  let write_option oc write_elt = function
    | None -> write_u8 oc 0
    | Some elt ->
      write_u8 oc 1;
      write_elt oc elt

  let rec write_node oc (node : wnode) =
    write_i64 oc node.acc;
    let children = sorted_entries node.next in
    write_u32 oc (List.length children);
    List.iter
      (fun (hash, child) ->
        write_i64 oc hash;
        write_node oc child)
      children

  let write t ~filename ~buildid ~total_samples ~debug_map =
    Out_channel.with_open_bin filename (fun oc ->
        Out_channel.output_string oc magic_number;
        write_option oc write_string buildid;
        write_i64 oc total_samples;
        let roots = sorted_entries t.forest in
        write_u32 oc (List.length roots);
        List.iter
          (fun (hash, node) ->
            write_i64 oc hash;
            write_node oc node)
          roots;
        if debug_map
        then (
          write_u8 oc 1;
          let entries = sorted_entries t.positions in
          write_u32 oc (List.length entries);
          List.iter
            (fun (hash, position) ->
              write_i64 oc hash;
              write_string oc position)
            entries)
        else write_u8 oc 0)

  let to_profile t ~buildid ~total_samples ~debug_map =
    let rec convert (w : wnode) =
      { count = w.acc;
        children =
          Hashtbl.fold
            (fun hash child acc -> Hash_map.add hash (convert child) acc)
            w.next Hash_map.empty
      }
    in
    let roots = Hashtbl.create (Hashtbl.length t.forest) in
    Hashtbl.iter (fun hash w -> Hashtbl.add roots hash (convert w)) t.forest;
    { buildid;
      total_samples;
      roots;
      debug_map = (if debug_map then Some (Hashtbl.copy t.positions) else None)
    }
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
