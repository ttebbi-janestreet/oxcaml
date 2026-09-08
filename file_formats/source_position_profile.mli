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

    A location ({!Fdo_location.t}) is a stack of levels, most-inlined first: the
    source position or branch edge itself, then the call sites it was inlined
    through. Each level is hashed to 64 bits ({!Fdo_location.hash_level}). The
    profile is a forest of inverse tries keyed by these hashes: each root is a
    most-inlined (leaf) level together with its count summed over all inlining
    contexts; walking down the trie matches increasingly long prefixes of the
    inlining stack, refining the count to the contexts observed at run time.

    Locations are not stored directly, only their hashes.

    The profile also indexes the call graph: for each call site level, the entry
    labels of the functions that calls from it reached ({!call_targets}). The
    counts stay in the trie, under the callee's entry label refined by the call
    site and its inlining context like any other location, so a consumer lists
    the candidates and then walks the trie from each with as much context as it
    has ({!count_for_deepest_context}).

    The on-disk format is designed to be queried in place, without parsing: trie
    nodes reference their children by file offset through hash-sorted entry
    arrays, so a query reads only the entries it searches. Loading validates the
    header eagerly but the trie lazily, as it is read, so a memory-mapped
    profile has most of its pages never touched. *)

type t

(** Raised, with a message, on any problem reading or validating the profile: by
    {!load} for problems in the header or the root index, and by any query or
    traversal that reads a malformed part of the trie (validation is lazy). The
    profile is deliberately validated strictly: a malformed profile signals that
    feedback-directed optimization is broken and should be surfaced loudly
    rather than silently ignored. *)
exception Error of string

(** The magic number at the start of the on-disk format (its last byte is the
    format version). *)
val magic_number : string

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

(** [count_for_location t location] is the recorded count for [location]: the
    count at the trie node reached by matching every level, and 0 if any level
    fails to match (the profile does not distinguish a location it observed
    never to execute from one it could not record) or [location] is empty. *)
val count_for_location : t -> Fdo_location.t -> int64

(** [count_for_deepest_context t ~root ~context], for level hashes, walks the
    trie from the root [root] along [context] as far as the profile has nodes
    and returns the count there: the count for the longest recorded prefix of
    the context, i.e. refined by the context the profile knows and aggregated
    over the rest. 0 when there is no such root. *)
val count_for_deepest_context : t -> root:int64 -> context:int64 list -> int64

(** The hashes of the entry labels of the functions that calls from the call
    site [callsite] (the level of the call site, without inlining context)
    reached. *)
val call_targets : t -> Fdo_location.level -> int64 list

(** Pre-order iteration over every trie node, in deterministic (unsigned hash)
    order: the location hash on the node's incoming edge, its depth (roots have
    depth 1) and its count. Intended for dumping and debugging; doubles as a
    deep validation of the profile (raising {!Error} on malformed parts, since
    validation is lazy). *)
val iter : t -> f:(hash:int64 -> depth:int -> count:int64 -> unit) -> unit

(** As {!iter}, over the call-target index: roots are call site levels, their
    children (depth 2) the callee entry labels, with the total counts. *)
val iter_call_targets :
  t -> f:(hash:int64 -> depth:int -> count:int64 -> unit) -> unit

(** {2 Writing}

    Used by the profile producer and by tests. *)

module Writer : sig
  type profile := t

  type t

  val create : unit -> t

  (** [add_location t ~location ~count] adds [count] to every trie node along
      [location]. An empty [location] is ignored. *)
  val add_location : t -> location:Fdo_location.t -> count:int64 -> unit

  (** {!add_location} for a location given as level hashes (as recorded in an
      executable's FDO metadata). *)
  val add_hashed_stack : t -> hashes:Fdo_location.hashed -> count:int64 -> unit

  (** [add_call_target t ~callsite ~callee ~count] records that calls from the
      call site level hashed [callsite] reached the function whose entry label
      hashes to [callee], [count] times (the count by context goes into the trie
      with {!add_hashed_stack}). *)
  val add_call_target :
    t -> callsite:int64 -> callee:int64 -> count:int64 -> unit

  (** Serialize the accumulated forest. The output is deterministic (children
      are ordered by hash). *)
  val write : t -> filename:string -> unit

  (** In-memory counterpart of {!write} followed by {!load}, for testing. *)
  val to_profile : t -> profile
end
