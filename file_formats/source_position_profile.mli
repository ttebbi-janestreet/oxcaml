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

(** An FDO profile of execution counts per source position.

    The profile is a forest of inverse tries. Each trie root is a most-inlined
    (leaf) source position together with its execution count summed over all
    inlining contexts and call sites; walking down the trie matches increasingly
    long prefixes of the inlining/call stack, most-inlined frame first, refining
    the count to the contexts observed at run time.

    Source positions are not stored directly: every frame is represented by a
    64-bit hash of a canonical [frame_string]. The file optionally carries a map
    from hashes back to human-readable positions for debugging; it is omitted by
    default (for output size and so that profiles do not leak source structure).
*)

type t

type error =
  | Cannot_open of string
  | Wrong_format of string
  | Wrong_version of string
  | Corrupted of string

(** Raised by {!load} on any problem reading or validating the profile. The
    profile is deliberately validated strictly: a malformed profile signals that
    feedback-directed optimization is broken and should be surfaced loudly
    rather than silently ignored. *)
exception Error of error

(** The magic number at the start of the on-disk format (its last byte is the
    format version). *)
val magic_number : string

(** {2 The cross-side frame contract}

    The producer decodes source positions from the DWARF of the profiled
    executable; the consumer recomputes them from [Debuginfo.item]. Both sides
    must hash the exact same canonical string: ["<file>:<line>:<col>"] where
    [file] is the path as passed to the compiler ([dinfo_file], which is also
    the raw file string in the DWARF line table), [line] is 1-based, and [col]
    is the 0-based [dinfo_char_start] clamped to 0 when negative (DWARF cannot
    represent an absent column distinctly from column 0). *)

val frame_string : file:string -> line:int -> col:int -> string

val frame_string_of_item : Debuginfo.item -> string

(** The 64-bit frame hash: the first 8 bytes of the MD5 digest of the canonical
    frame string, read little-endian. *)
val hash_frame : string -> int64

(** {2 Reading and querying} *)

(** [load ~filename] reads and validates a profile. Raises {!Error} if the file
    cannot be opened, has the wrong magic number or version, is truncated, or
    otherwise violates the format (including duplicate sibling hashes at a trie
    node). *)
val load : filename:string -> t

(** The GNU build id of the profiled executable, if the producer recorded it.
    Provenance information only; it is not enforced. *)
val buildid : t -> string option

(** The total number of decoded samples the profile was built from, across all
    source positions (the normalizer for count queries). *)
val total_samples : t -> int64

(** [count_for_frames t ~frames] is the estimated execution count for the
    leaf-first stack [frames] of canonical frame strings: [None] if [frames] is
    empty, [Some 0L] if the leaf position was never sampled, and otherwise the
    count at the deepest trie node reached by matching [frames]. When a frame
    fails to match at a node that recorded no children, the profile simply knows
    nothing deeper (depth truncation, or a profile without call chains) and that
    node's count is used; failing to match at a node that did record children
    means the queried context was observed never to execute, and the count is 0.
*)
val count_for_frames : t -> frames:string list -> int64 option

(** [count_for_debuginfo t dbg] is {!count_for_frames} on the leaf-first stack
    implied by [dbg]. [None] if [dbg] is empty or its most-inlined item has no
    valid line number. This is the intended entry point for optimization passes.
*)
val count_for_debuginfo : t -> Debuginfo.t -> int64 option

(** The human-readable position for a frame hash, if the profile was written
    with its debugging map. *)
val position_of_hash : t -> int64 -> string option

(** Pre-order iteration over every trie node, in deterministic (hash) order: the
    frame hash on the node's incoming edge, its depth (roots have depth 1), and
    its count. Intended for dumping and debugging. *)
val iter : t -> f:(hash:int64 -> depth:int -> count:int64 -> unit) -> unit

val print_stats : Format.formatter -> t -> unit

(** {2 Writing}

    Used by the profile producer and by tests. *)

module Writer : sig
  type profile := t

  type t

  val create : unit -> t

  (** [add_stack t ~frames ~count] adds [count] to every trie node along the
      leaf-first stack [frames] of canonical frame strings, truncated to its
      first [max_depth] frames. An empty [frames] is ignored. *)
  val add_stack :
    t -> frames:string list -> count:int64 -> max_depth:int -> unit

  (** [write t ~filename ~buildid ~total_samples ~debug_map] serializes the
      accumulated forest. When [debug_map] is set, the hash-to-position map for
      every frame recorded in the forest is included. The output is
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
