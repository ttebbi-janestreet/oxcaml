(** Parsing of the "fdo_metadata" section of an OxCaml-compiled executable (see
    backend/fdo_metadata.ml for the emitter and the format). *)

(** A location as a stack of level hashes, most-inlined first. *)
type location = int64 list

type t =
  { taken : (int64, location list) Hashtbl.t;
        (** the locations counted when a taken branch leaves from an address *)
    fallthrough : (int64 * location list) array;
        (** the locations counted when an address executes without branching
            away, in address order *)
    target : (int64, location list) Hashtbl.t;
        (** the locations counted when a taken branch lands on an address (a
            function entry), extended by the call site locations of the branch's
            source, if any *)
    callsite : (int64, location list) Hashtbl.t;
        (** the call site locations of the calls and tail jumps at an address *)
    names : (int64, Fdo_location.level) Hashtbl.t
        (** the levels behind the hashes, for those compilation units built with
            -fdo-names *)
  }

(** Raises [Failure] on malformed input. *)
val parse : string -> t

val empty : t

(** [iter_range t ~lo ~hi f], for the code from [lo] up to the taken branch at
    [hi] having executed sequentially, calls [f] on the fallthrough locations of
    every address from [lo] up to but excluding [hi]. *)
val iter_range : t -> lo:int64 -> hi:int64 -> (location list -> unit) -> unit

(** [branch_locations t ~source ~target]: the locations a taken branch from
    [source] to [target] counts for: those of a taken branch leaving [source],
    and, when [target] has target locations (it is a function entry), each of
    them extended by each call site location of [source], or unextended when
    [source] has none. *)
val branch_locations : t -> source:int64 -> target:int64 -> location list

(** [call_targets t ~source ~target]: the (call site, callee entry) level hash
    pairs a taken branch from [source] to [target] records in the profile's
    call-target index: the level of every call site location of [source]
    (without its inlining context) with every entry label of [target]. A
    function's entry label is a single level (entries are never inlined: a
    copy's label is renamed, not extended), while the riders recorded with it
    carry their call site's levels, so the single-level target locations are the
    entries. *)
val call_targets : t -> source:int64 -> target:int64 -> (int64 * int64) list
