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

(** Reader and query API for an FDO profile based on source call stacks.

    The profile is a bounded-depth trie keyed by source frames, each a single
    string (see {!source_frame}). Each path from the root is a leaf-first source
    call stack: the actual executed source position first, then the enclosing
    inline/call-site contexts outwards. Execution-count estimates are stored on
    every node, so a consumer can query a full stack suffix or any shorter
    suffix of it. The profile also carries the set of source positions it knows
    about (used by {!estimate} to classify queried frames) and the total number
    of calls it observed (used by {!estimate} as the normalizer; see
    {!total_calls}). *)

(** A source location, encoded as a single string. By convention the producer we
    use ["<filename>:<line>:<column>"] (and ["<filename>:<line>"] when no column
    is available). Keeping it a plain string keeps the format compact and makes
    it easy to change later (e.g. to a hash of richer location information). *)
type source_frame = string

type t

(** Aggregate information about a loaded profile, for diagnostics and tuning.
    The [depth] and [buildid] are available directly via {!depth} and
    {!buildid}. *)
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

(** Raised by {!load} on any problem reading or validating the profile. The
    profile is deliberately validated strictly: a malformed profile signals that
    feedback-directed optimization is broken and should be surfaced loudly
    rather than silently ignored. *)
exception Error of error

(** The magic number at the start of the on-disk format. *)
val magic_number : string

(** [load ~filename] reads and validates a source-call-stack profile. Raises
    {!Error} if the file cannot be opened, has the wrong magic number or
    version, is truncated, or otherwise violates the format (including duplicate
    sibling frames at a trie node). *)
val load : filename:string -> t

val depth : t -> int

val buildid : t -> string option

(** The count stored at the root of the trie. *)
val total_count : t -> int

(** The total number of calls (actual + virtual/inlined) the profile recorded.
    This is the normalizer for {!estimate}: a call site's count as a fraction of
    all calls. It is recorded by the producer rather than derived from the trie
    (the trie may omit some calls), so it can exceed {!total_count}. *)
val total_calls : t -> int

(** [find_count t stack] returns the count at the node reached by matching all
    of [stack] (leaf-first), or [None] if any frame fails to match. Matching is
    exact string equality on each frame. This is a low-level primitive; most
    callers want {!estimate} or {!estimate_for_debuginfo}. *)
val find_count : t -> source_frame list -> int option

(** [estimate t stack] is an estimated execution count for the leaf-first
    [stack] (the actual position first, then enclosing inline/call-site contexts
    outwards): the count at the deepest matching node. The caller normalizes it
    as desired (e.g. against {!total_calls} for a call frequency, or
    {!total_count}). It descends the trie from the root following [stack],
    classifying each frame against the profile's set of known positions:

    - The leaf (actual / most-inlined position) must be a known position;
      otherwise the result is [None] and the consumer applies its own default. A
      known leaf that never executed yields [Some 0].
    - A deeper frame that is a known position is matched against the trie as a
      call site: if that context was recorded the estimate is refined, otherwise
      it is [0] (the context was never observed). A frame that is not a known
      position still occupies a trie level but cannot be identified, so the
      estimate sums over all children at that level. *)
val estimate : t -> source_frame list -> int option

(** [estimate_for_debuginfo t dbg] is the estimated execution count
    ({!estimate}) for the leaf-first stack implied by [dbg]. This is the
    intended entry point for optimization passes: they hand over the debug info
    they have and need not know how source frames are encoded or matched. *)
val estimate_for_debuginfo : t -> Debuginfo.t -> int option

val dump_stats : t -> stats

val print_stats : Format.formatter -> t -> unit
