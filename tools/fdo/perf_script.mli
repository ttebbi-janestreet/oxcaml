(** Extraction of raw samples from a Linux perf profile, via "perf script".
    Samples are filtered to the profiled executable and aggregated into a
    histogram of leaf-first call stacks (for profiles recorded without call
    graphs, every stack is a single sampled address). *)

type t =
  { counts : (int64 list, int64) Hashtbl.t;
        (** leaf-first call stack (sampled address, then return addresses of
            callers, cut at the first frame outside the executable) ->
            accumulated count *)
    mutable total : int64  (** sum of all counts *)
  }

type line =
  | Blank
  | Chain_header of int64  (** the period of a sample with a call chain *)
  | Sample of int64 * int64 * string  (** count, address, dso *)
  | Address of int64 * string
      (** address, dso: a call-chain entry, or a sample of the legacy no-period
          form *)

(** Classify one line of "perf script -F ip,dso,period" output. Raises [Failure]
    on lines of unknown shape (silently skipped samples would skew the profile).
    Exposed for testing. *)
val classify_line : string -> line

(** Whether a sample's dso refers to the profiled executable (by full path or
    basename). *)
val dso_matches : binary:string -> string -> bool

(** Aggregate the samples of pre-captured "perf script -F ip,dso,period" output.
*)
val of_channel : In_channel.t -> binary:string -> t

(** Run "perf script" on [perf_data] and aggregate the samples (retrying without
    the period field on perf versions that reject it). *)
val collect : perf:string -> perf_data:string -> binary:string -> t
