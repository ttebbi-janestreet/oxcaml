(** Extraction of raw samples from a Linux perf profile, via "perf script".
    Samples are filtered to the profiled executable and aggregated into a
    histogram of leaf-first call stacks. For profiles recorded with LBR branch
    stacks (perf record -j any,u), both addresses of every recorded taken branch
    become one single-address stack each; for instruction profiles without call
    graphs, every stack is a single sampled address. *)

(** A conditional branch whose outcome counts should be collected (from the
    executable's branch-label metadata; see [Branch_labels]). *)
type branch_site =
  { address : int64;  (** the branch instruction *)
    fallthrough_address : int64  (** the instruction after it *)
  }

type branch_counts =
  { mutable taken : int64;
    mutable fallthrough : int64
  }

type t =
  { counts : (int64 list, int64) Hashtbl.t;
        (** leaf-first call stack (sampled address, then return addresses of
            callers, cut at the first frame outside the executable) ->
            accumulated count *)
    mutable total : int64;  (** sum of all counts *)
    branches : (int64, branch_counts) Hashtbl.t
        (** observed outcome counts per branch site, keyed by branch address
            (only fed by branch-stack samples): a site is taken when observed as
            an entry's source with a target other than its fallthrough address,
            and falls through when observed with that target or strictly inside
            the sequential range between two consecutive entries *)
  }

type line =
  | Blank
  | Chain_header of int64  (** the period of a sample with a call chain *)
  | Sample of int64 * int64 * string  (** count, address, dso *)
  | Address of int64 * string
      (** address, dso: a call-chain entry, or a sample of the legacy no-period
          form *)
  | Branch_stack of int64 * (int64 * int64) list
      (** count, then one (source, target) address pair per taken branch of the
          sample's LBR record *)

(** Classify one line of "perf script" output (of the "brstack,period" or
    "ip,dso,period" field lists). Raises [Failure] on lines of unknown shape
    (silently skipped samples would skew the profile). Exposed for testing. *)
val classify_line : string -> line

(** Whether a sample's dso refers to the profiled executable (by full path or
    basename). *)
val dso_matches : binary:string -> string -> bool

(** Aggregate the samples of pre-captured "perf script" output (of the
    "brstack,period" or "ip,dso,period" field lists). [code_bounds] is the
    executable's code address range (see [Elf_info.code_bounds]), used to filter
    branch-stack addresses, which perf prints without a dso. *)
val of_channel :
  In_channel.t ->
  binary:string ->
  code_bounds:(int64 * int64) option ->
  branch_sites:branch_site array ->
  t

(** Run "perf script" on [perf_data] and aggregate the samples, preferring
    branch stacks when the profile has them (and retrying without the period
    field on perf versions that reject it). *)
val collect :
  perf:string ->
  perf_data:string ->
  binary:string ->
  code_bounds:(int64 * int64) option ->
  branch_sites:branch_site array ->
  t
