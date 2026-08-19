(** The pseudo-instrumentation label metadata of a patchprof-instrumented
    executable: the "patchprof_labels" ELF section (see the emitter in
    backend/patchprof.ml for the format) records, for every instrumented site
    that carries branch provenance, the labels of its taken and fallthrough
    edges. *)

type label =
  { frame_hashes : int64 list;
        (** the creating construct's inlining stack, most-inlined first, as
            canonical frame hashes (see [Source_position_profile.hash_frame]) *)
    disc : int;
    edge : int
  }

type site_labels =
  { taken : label list;
    fallthrough : label list
  }

(** Parse the section contents into a table keyed by the sites' flag-writer
    addresses (the addresses the runtime's counter records report). Raises
    [Failure] on a malformed section. *)
val parse : string -> (int64, site_labels) Hashtbl.t
