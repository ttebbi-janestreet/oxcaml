(** The branch-label metadata of an executable: the "fdo_branch_labels" ELF
    section (see the emitter in backend/fdo_branch_labels.ml for the format)
    records, for every emitted conditional branch that carries
    pseudo-instrumentation labels, its fallthrough address and the labels of its
    taken and fallthrough edges. *)

(** A label is the leaf-first location stack of the edge it names: the edge
    location, then the creating construct's inlining context, as canonical
    location hashes (see [Source_position_profile.hash_frame]). *)
type label = int64 list

type site =
  { fallthrough_address : int64;
        (** the address of the instruction after the branch *)
    taken : label list;
    fallthrough : label list
  }

(** Parse the section contents into a table keyed by the branch instructions'
    addresses. Raises [Failure] on a malformed section. *)
val parse : string -> (int64, site) Hashtbl.t
