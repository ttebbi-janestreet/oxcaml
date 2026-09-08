(** Locations for feedback-directed optimization: what execution counts are
    attributed to. A location is a stack of levels, most inlined first: the
    source position or branch edge itself, then the call sites it was inlined
    through, innermost first. A level is a list of string components hashed as
    a unit; the hashes are the keys of the profile's inlining trie (see
    [Source_position_profile]).

    Only the compiler ever computes these hashes: when emitting an executable's
    FDO metadata and when consuming a profile. The profile producer only copies
    hashes from the metadata, so the encoding needs no agreement with any other
    tool, only with itself. *)

type level = string list

type t = level list

(** The hashes of a location's levels, in the same order. *)
type hashed = int64 list

(** The 64-bit hash of a level: the first 8 bytes of the MD5 digest of the
    components joined with NUL bytes (which no component contains), read
    little-endian. Memoized. *)
val hash_level : level -> int64

val hash : t -> hashed

(** The components joined with ':', for humans. *)
val level_string : level -> string

(** {2 Anchors}

    Pseudo-instrumentation labels ([Debuginfo.branch_label]) are anchored at
    the enclosing function: a function's anchor is its position level
    ([Debuginfo.item_level]), a compilation unit's is [["<Unit>"]], and a
    function without a position is anchored like a branching construct of the
    enclosing function, by the enclosing anchor and a fresh index. No level of
    an edge can thus collide with a position: it has more than three components
    unless it starts with a bracketed unit name. *)

val unit_anchor : string -> string list

val nested_anchor : anchor:string list -> index:int -> string list
