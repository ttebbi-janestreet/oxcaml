(** The little we need to know about the profiled executable's ELF file. *)

type t

val read : string -> t

(** Whether the executable is position-independent (ET_DYN), in which case
    sampled runtime addresses do not match link-time addresses. *)
val is_pie : t -> bool

(** The GNU build id, as a hex string, if present. *)
val buildid : t -> string option

(** The raw contents of the named section, if present. *)
val section_body : t -> string -> string option
