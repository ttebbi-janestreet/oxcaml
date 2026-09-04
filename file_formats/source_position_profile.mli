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

(** An FDO profile of execution counts per location.

    A location is a source position or an edge of a branching construct, both
    represented by the 64-bit hash of a canonical string ({!frame_string},
    {!edge_string}). The profile is a forest of inverse tries: each root is a
    most-inlined (leaf) location together with its count summed over all
    inlining contexts; walking down the trie matches increasingly long prefixes
    of the inlining stack, most-inlined frame first, refining the count to the
    contexts observed at run time.

    Locations are not stored directly, only their hashes. The file optionally
    carries a map from hashes back to human-readable strings for debugging; it
    is omitted by default (for output size and so that profiles do not leak
    source structure).

    The on-disk format is designed to be queried in place, without parsing: trie
    nodes reference their children by file offset through hash-sorted entry
    arrays, so a query reads only the entries it searches. Loading validates the
    header eagerly but the trie lazily, as it is read, so a memory-mapped
    profile has most of its pages never touched. *)

type t

type error =
  | Cannot_open of string
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string

(** Raised on any problem reading or validating the profile: by {!load} and
    {!of_bigstring} for problems in the header or the root index, and by any
    query or traversal that reads a malformed part of the trie (validation is
    lazy). The profile is deliberately validated strictly: a malformed profile
    signals that feedback-directed optimization is broken and should be surfaced
    loudly rather than silently ignored. *)
exception Error of error

(** The magic number at the start of the on-disk format (its last byte is the
    format version). *)
val magic_number : string

(** {2 The cross-side location contract}

    The producer decodes source positions from the DWARF of the profiled
    executable; the consumer recomputes them from [Debuginfo.item]. Both sides
    must hash the exact same canonical string: ["<file>:<line>:<col>"] where
    [file] is the path as passed to the compiler ([dinfo_file], which is also
    the raw file string in the DWARF line table), [line] is 1-based, and [col]
    is the 0-based [dinfo_char_start] clamped to 0 when negative (DWARF cannot
    represent an absent column distinctly from column 0).

    An edge of a branching construct is a location too, named structurally
    ({!Debuginfo.branch_label}): the anchor of the enclosing function (its
    position string) or compilation unit, then the tree path from the function's
    body to the edge: ["<file>:<line>:<col>@<i>.<j>.<k>"]. The compiler records
    the hashed stack of each emitted branch's edges in the executable's
    metadata, so the profile producer only ever handles edge hashes. *)

val frame_string : file:string -> line:int -> col:int -> string

val frame_string_of_item : Debuginfo.item -> string

(** The anchor of a function with a position, of a compilation unit's toplevel
    code, and of a function without a position (named by its own tree path in
    the enclosing function). *)
val function_anchor : Debuginfo.item -> string

val unit_anchor : string -> string

val nested_anchor : anchor:string -> path:int list -> string

val edge_string : anchor:string -> path:int list -> string

(** The leaf-first location stack of a pseudo-instrumentation label: its edge
    string, then the call sites it was inlined through, innermost first. *)
val frames_of_branch_label : Debuginfo.branch_label -> string list

(** The 64-bit location hash: the first 8 bytes of the MD5 digest of the
    canonical string, read little-endian. *)
val hash_frame : string -> int64

(** {2 Reading and querying} *)

(** [load ~filename] opens a profile and validates its header. Raises {!Error}
    if the file cannot be opened, has the wrong magic number or version, or has
    a malformed header or root index (including trailing bytes). The trie itself
    is validated lazily: queries and traversals raise {!Error} when they read a
    malformed part.

    The file is memory-mapped when a mapper has been registered with
    {!register_mmap}, so that queries only fault in the pages they touch;
    otherwise, or if mapping fails, it is read into memory. *)
val load : filename:string -> t

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Register how to memory-map a file. Mapping needs [Unix], which this library
    cannot depend on; the native compiler driver registers a mapper at startup.
*)
val register_mmap : (string -> bigstring) -> unit

(** The GNU build id of the profiled executable, if the producer recorded it.
    Provenance information only; it is not enforced. *)
val buildid : t -> string option

(** The total number of decoded samples the profile was built from, across all
    source positions (the normalizer for count queries). *)
val total_samples : t -> int64

(** [count_for_frames t ~frames] is the recorded count for the leaf-first stack
    [frames] of canonical location strings: the count at the trie node reached
    by matching every frame, and 0 if any frame fails to match (the profile does
    not distinguish a stack it observed never to execute from one it could not
    record). *)
val count_for_frames : t -> frames:string list -> int64

(** [count_for_debuginfo t dbg] is {!count_for_frames} on the leaf-first stack
    implied by [dbg]; 0 if [dbg] is empty or its most-inlined item has no valid
    line number. This is the intended entry point for optimization passes. *)
val count_for_debuginfo : t -> Debuginfo.t -> int64

(** The recorded execution count of one pseudo-instrumentation label, as carried
    in the compiler's {!Debuginfo.edge_labels}: {!count_for_frames} on
    {!frames_of_branch_label}. *)
val count_for_branch_label : t -> Debuginfo.branch_label -> int64

(** The human-readable string for a location hash, if the profile was written
    with its debugging map. *)
val position_of_hash : t -> int64 -> string option

(** Pre-order iteration over every trie node, in deterministic (unsigned hash)
    order: the location hash on the node's incoming edge, its depth (roots have
    depth 1) and its count. Intended for dumping and debugging; doubles as a
    deep validation of the profile (raising {!Error} on malformed parts, since
    validation is lazy). *)
val iter : t -> f:(hash:int64 -> depth:int -> count:int64 -> unit) -> unit

val print_stats : Format.formatter -> t -> unit

(** {2 Writing}

    Used by the profile producer and by tests. *)

module Writer : sig
  type profile := t

  type t

  val create : unit -> t

  (** [add_stack t ~frames ~count] adds [count] to every trie node along the
      leaf-first stack [frames] of canonical location strings, truncated to its
      first [max_depth] frames. An empty [frames] is ignored. *)
  val add_stack :
    t -> frames:string list -> count:int64 -> max_depth:int -> unit

  (** {!add_stack} for a stack given as location hashes (as recorded in an
      executable's branch-label metadata) rather than strings. No debug-map
      entries are recorded for such locations. *)
  val add_hashed_stack :
    t -> hashes:int64 list -> count:int64 -> max_depth:int -> unit

  (** [write t ~filename ~buildid ~total_samples ~debug_map] serializes the
      accumulated forest. When [debug_map] is set, the hash-to-string map for
      every location recorded from a string is included. The output is
      deterministic (children are ordered by hash). *)
  val write :
    t ->
    filename:string ->
    buildid:string option ->
    total_samples:int64 ->
    debug_map:bool ->
    unit

  (** In-memory counterpart of {!write} followed by {!load}, for testing. *)
  val to_profile :
    t ->
    buildid:string option ->
    total_samples:int64 ->
    debug_map:bool ->
    profile
end
