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

exception Error of string

let magic_number = "oxcaml-source-position-profile\006"

(* -------------------------------------------------------------------------- *)
(* Locations. *)
(* -------------------------------------------------------------------------- *)

(* -------------------------------------------------------------------------- *)
(* The on-disk format (version 6). *)
(* *)
(* All integers are little-endian; "u32" is an unsigned 32-bit count, "i64" a *)
(* signed 64-bit count and "u64" an unsigned 64-bit absolute file offset. *)
(* *)
(*   header: *)
(*     magic number (its last byte is the format version) *)
(*     root index offset: u64 *)
(*     call-target index offset: u64 *)
(*   nodes, each child before its parent: *)
(*     count: i64 *)
(*     number of children: u32 *)
(*     child entries, sorted by unsigned hash: i64 hash, u64 node offset *)
(*   root index: u32 entry count, then root entries like child entries, *)
(*     sorted by unsigned hash *)
(*   call-target index, last so its end doubles as a trailing-bytes check: *)
(*     like the root index; each root is a call site level whose children *)
(*     are the entry levels of the functions calls from it reached (their *)
(*     counts are totals over all contexts; the ordinary trie has the *)
(*     counts by context) *)
(* *)
(* The point of this layout is that a query can descend the trie by reading *)
(* only the entry arrays it searches: nothing needs parsing up front, so a *)
(* profile can be memory-mapped and most of it never touched. Reads are *)
(* bounds-checked and validated lazily, as they happen; [load] eagerly *)
(* validates only the header and the root index placement (which *)
(* includes rejecting trailing bytes). *)
(* -------------------------------------------------------------------------- *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* The raw bytes, memory-mapped or read into memory. [filename] is carried for
   error messages. *)
type raw =
  { filename : string;
    data : bigstring
  }

type t =
  { raw : raw;
    (* The offset of the first root-index entry and the number of entries, for
       the trie and for the call-target index. *)
    roots_pos : int;
    num_roots : int;
    call_roots_pos : int;
    num_call_roots : int
  }

let corrupted raw fmt =
  Printf.ksprintf
    (fun msg ->
      raise
        (Error
           (Printf.sprintf "Corrupted source-position profile %s: %s"
              raw.filename msg)))
    fmt

let data_length data = Bigarray.Array1.dim data

(* Every primitive read is bounds-checked, so no possible file contents can make
   a read escape the data (offsets read from the file are additionally
   range-checked in [get_offset], keeping the arithmetic here overflow-free). *)
let check_bounds raw pos len =
  if pos < 0 || len < 0 || pos > data_length raw.data - len
  then corrupted raw "unexpected end of profile data"

let get_u32 raw pos =
  check_bounds raw pos 4;
  let byte i = Char.code (Bigarray.Array1.unsafe_get raw.data (pos + i)) in
  byte 0 lor (byte 1 lsl 8) lor (byte 2 lsl 16) lor (byte 3 lsl 24)

let get_i64 raw pos =
  check_bounds raw pos 8;
  let byte i =
    Int64.of_int (Char.code (Bigarray.Array1.unsafe_get raw.data (pos + i)))
  in
  let word = ref 0L in
  for i = 7 downto 0 do
    word := Int64.logor (Int64.shift_left !word 8) (byte i)
  done;
  !word

let get_string raw pos len =
  check_bounds raw pos len;
  String.init len (fun i -> Bigarray.Array1.unsafe_get raw.data (pos + i))

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

(* The number of children and the offset of the first child entry. *)
let node_children t (node : node) = get_u32 t.raw (node + 8), node + 12

let find_root t hash =
  match find_entry t.raw ~base:t.roots_pos ~n:t.num_roots hash with
  | None -> None
  | Some i -> Some (entry_offset t.raw ~base:t.roots_pos i)

let find_call_root t hash =
  match find_entry t.raw ~base:t.call_roots_pos ~n:t.num_call_roots hash with
  | None -> None
  | Some i -> Some (entry_offset t.raw ~base:t.call_roots_pos i)

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
  let wrong_format () =
    raise (Error ("Not a source-position profile: " ^ raw.filename))
  in
  if data_length raw.data < mlen then wrong_format ();
  let s = get_string raw 0 mlen in
  if not (String.equal s magic_number)
  then
    if
      String.equal
        (String.sub s 0 (mlen - 1))
        (String.sub magic_number 0 (mlen - 1))
    then
      (* Same producer, different version byte. *)
      raise
        (Error
           (raw.filename ^ " is an incompatible source-position profile version"))
    else wrong_format ()

let of_data ~filename data =
  let raw = { filename; data } in
  check_magic raw;
  let pos = String.length magic_number in
  let root_index_offset = get_offset raw pos in
  let call_index_offset = get_offset raw (pos + 8) in
  let header_end = pos + 16 in
  if root_index_offset < header_end
  then corrupted raw "root index offset inside the header";
  let num_roots = get_u32 raw root_index_offset in
  let index_end = root_index_offset + 4 + (16 * num_roots) in
  if call_index_offset <> index_end
  then corrupted raw "call-target index does not follow the root index";
  let num_call_roots = get_u32 raw call_index_offset in
  let call_index_end = call_index_offset + 4 + (16 * num_call_roots) in
  if call_index_end < data_length data
  then corrupted raw "unexpected trailing bytes"
  else if call_index_end > data_length data
  then corrupted raw "unexpected end of profile data";
  { raw;
    roots_pos = root_index_offset + 4;
    num_roots;
    call_roots_pos = call_index_offset + 4;
    num_call_roots
  }

(* Memory-mapping needs [Unix], which this library cannot link; the native
   driver registers a mapper built on it. *)
let mmap : (string -> bigstring) option ref = ref None

let register_mmap f = mmap := Some f

let read_file filename =
  In_channel.with_open_bin filename (fun ic ->
      let length = Int64.to_int (In_channel.length ic) in
      let data =
        Bigarray.Array1.create Bigarray.char Bigarray.c_layout length
      in
      match In_channel.really_input_bigarray ic data 0 length with
      | Some () -> data
      | None ->
        raise
          (Error
             ("Corrupted source-position profile " ^ filename
            ^ ": unexpected end of profile data")))

let map_or_read filename =
  match !mmap with
  | None -> read_file filename
  | Some map_file -> (
    match map_file filename with
    | data -> data
    | exception _ ->
      (* Mapping can legitimately fail (empty file, exotic filesystem); reading
         then either succeeds or reports a proper error. *)
      read_file filename)

let load ~filename =
  match map_or_read filename with
  | data -> of_data ~filename data
  | exception Sys_error msg ->
    raise (Error ("Cannot open source-position profile: " ^ msg))

(* -------------------------------------------------------------------------- *)
(* Queries. *)
(* -------------------------------------------------------------------------- *)

(* A location the profile did not record counts as 0, whether the profiled
   program never executed it or the profile could not have recorded it (e.g.
   depth truncation): the two are not distinguished for now. *)
let count_for_location t location =
  let rec descend node = function
    | [] -> node_count t node
    | level :: rest -> (
      match find_child t node (Fdo_location.hash_level level) with
      | Some child -> descend child rest
      | None -> 0L)
  in
  match location with
  | [] -> 0L
  | leaf :: rest -> (
    match find_root t (Fdo_location.hash_level leaf) with
    | None -> 0L
    | Some node -> descend node rest)

let count_for_deepest_context t ~root ~context =
  let rec descend node = function
    | [] -> node_count t node
    | hash :: rest -> (
      match find_child t node hash with
      | Some child -> descend child rest
      | None -> node_count t node)
  in
  match find_root t root with None -> 0L | Some node -> descend node context

let call_targets t callsite =
  match find_call_root t (Fdo_location.hash_level callsite) with
  | None -> []
  | Some node ->
    let num_children, entries = node_children t node in
    List.init num_children (fun i -> entry_hash t.raw ~base:entries i)

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

let iter_forest t ~roots_pos ~num_roots ~f =
  let rec walk depth hash node =
    if depth > max_reasonable_depth then corrupted t.raw "unreasonable depth";
    f ~hash ~depth ~count:(node_count t node);
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
  for i = 0 to num_roots - 1 do
    let hash = entry_hash t.raw ~base:roots_pos i in
    check_sorted t !prev hash;
    prev := Some hash;
    walk 1 hash (entry_offset t.raw ~base:roots_pos i)
  done

let iter t ~f = iter_forest t ~roots_pos:t.roots_pos ~num_roots:t.num_roots ~f

let iter_call_targets t ~f =
  iter_forest t ~roots_pos:t.call_roots_pos ~num_roots:t.num_call_roots ~f

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
      call_targets : (int64, wnode) Hashtbl.t
    }

  let create () =
    { forest = Hashtbl.create 1024; call_targets = Hashtbl.create 256 }

  let find_or_add_node table hash =
    match Hashtbl.find_opt table hash with
    | Some node -> node
    | None ->
      let node = { acc = 0L; next = Hashtbl.create 4 } in
      Hashtbl.add table hash node;
      node

  (* Add [count] to the node of every prefix of the leaf-first stack [hashes],
     creating the nodes as needed. *)
  let add_path table ~hashes ~count =
    let rec go table = function
      | hash :: rest ->
        let node = find_or_add_node table hash in
        node.acc <- Int64.add node.acc count;
        go node.next rest
      | [] -> ()
    in
    go table hashes

  let add_hashed_stack t ~hashes ~count = add_path t.forest ~hashes ~count

  let add_location t ~location ~count =
    add_hashed_stack t ~hashes:(Fdo_location.hash location) ~count

  let add_call_target t ~callsite ~callee ~count =
    add_path t.call_targets ~hashes:[callsite; callee] ~count

  (* Sorted by unsigned hash, as the entry arrays of the format require. *)
  let sorted_entries table =
    let entries = Hashtbl.fold (fun k v acc -> (k, v) :: acc) table [] in
    List.sort (fun (k1, _) (k2, _) -> Int64.unsigned_compare k1 k2) entries

  let add_u32 buf n = Buffer.add_int32_le buf (Int32.of_int n)

  let add_i64 buf n = Buffer.add_int64_le buf n

  let add_offset buf n = Buffer.add_int64_le buf (Int64.of_int n)

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
    add_u32 body (List.length children);
    List.iter
      (fun (hash, child_offset) ->
        add_i64 body hash;
        add_offset body child_offset)
      children;
    offset

  let serialize t =
    let header_size =
      String.length magic_number + 8 (* root index offset *)
      + 8 (* call-target index offset *)
    in
    let body = Buffer.create 65536 in
    let emit_forest forest =
      List.map
        (fun (hash, node) -> hash, emit_node body ~base:header_size node)
        (sorted_entries forest)
    in
    let emit_index roots =
      let offset = header_size + Buffer.length body in
      add_u32 body (List.length roots);
      List.iter
        (fun (hash, offset) ->
          add_i64 body hash;
          add_offset body offset)
        roots;
      offset
    in
    let roots = emit_forest t.forest in
    let call_roots = emit_forest t.call_targets in
    let root_index_offset = emit_index roots in
    let call_index_offset = emit_index call_roots in
    let header = Buffer.create header_size in
    Buffer.add_string header magic_number;
    add_offset header root_index_offset;
    add_offset header call_index_offset;
    assert (Buffer.length header = header_size);
    Buffer.contents header ^ Buffer.contents body

  let write t ~filename =
    let contents = serialize t in
    Out_channel.with_open_bin filename (fun oc ->
        Out_channel.output_string oc contents)

  let to_profile t =
    let contents = serialize t in
    of_data ~filename:"<in-memory profile>"
      (Bigarray.Array1.init Bigarray.char Bigarray.c_layout
         (String.length contents) (String.get contents))
end

(* -------------------------------------------------------------------------- *)
(* Error reporting. *)
(* -------------------------------------------------------------------------- *)

let () =
  Location.register_error_of_exn (function
    | Error msg ->
      Some (Location.error_of_printer_file Format_doc.pp_print_text msg)
    | _ -> None)
