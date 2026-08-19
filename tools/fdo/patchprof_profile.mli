(** Extraction of raw samples from a patchprof profile (the binary stream a
    patchprof-instrumented executable writes to [OCAML_PATCHPROF_OUT]; see
    runtime/caml/patchprof.h for the format).

    Walk records provide leaf-first stacks: the instrumented site's address
    (the [cmp]/[test] of a conditional branch) first, then the return
    addresses of its callers. The runtime removes the load bias, so the
    addresses match the executable's link-time addresses even for
    position-independent executables. Counter records provide the exact
    number of executions of every instrumented site; each site's sampled
    walks are scaled so that their counts sum to that exact number, and
    sites that executed too rarely to produce any walk are kept as
    single-frame stacks. *)

(** The aggregated counters of one instrumented site (a conditional branch),
    from the profile's counter records. *)
type site_counters =
  { executions : int64;  (** exact fast-path executions *)
    sampled_weight : int64;
        (** the executions consumed at sampled periods, the denominator of the
            taken-bias estimate *)
    tally : int64
        (** the portion of [sampled_weight] on which the branch was taken *)
  }

type t =
  { samples : Perf_script.t;
    sites : (int64, site_counters) Hashtbl.t  (** keyed by site address *)
  }

(** Aggregate the samples of an in-memory profile. Raises [Failure] on a
    malformed profile; a trailing record truncated by the producer dying
    mid-write ends the stream instead. Exposed for testing. *)
val of_string : string -> t

(** [read ~filename] is {!of_string} on the contents of [filename]. *)
val read : filename:string -> t
