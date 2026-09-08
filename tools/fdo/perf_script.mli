(** Extraction of raw execution events from a Linux perf profile via "perf
    script", as histograms of addresses. *)

type t =
  { branches : (int64 * int64, int64) Hashtbl.t;
        (** [(source, target)]: the taken branches, plus the calls seen through
            the runtime's stubs (see the implementation) *)
    ranges : (int64 * int64, int64) Hashtbl.t
        (** [(lo, hi)]: the code from [lo] up to [hi] executed sequentially:
            from a branch target up to the next taken branch or the sampled
            instruction *)
  }

(** Aggregate pre-captured "perf script -F period,ip,brstack" output, with or
    without call chains. [metadata] is the executable's, for seeing calls
    through stubs. With [ip_ranges], the code from each sample's most recent
    branch target up to its ip counts as executed too, which is only sound when
    the branch stack was frozen at the sampled instruction (see the
    implementation). Raises [Failure] on lines of unknown shape (silently
    skipped samples would skew the profile). *)
val of_channel : ip_ranges:bool -> metadata:Metadata.t -> In_channel.t -> t

(** Run "perf script" on [perf_data] and aggregate its output. *)
val collect : ip_ranges:bool -> metadata:Metadata.t -> perf_data:string -> t
