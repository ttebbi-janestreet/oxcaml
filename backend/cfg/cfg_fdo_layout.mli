[@@@ocaml.warning "+a-40-41-42"]

(** Profile-guided basic-block layout.

    [reorder_cold_blocks profile cl] sinks the blocks that [profile] knows to be
    cold to the end of the layout, keeping the relative order of all other
    blocks (and the entry block first). Measured per-block counts are first
    repaired using flow conservation (exact propagation through
    single-successor/predecessor relationships and proportional scaling first;
    even splits into all-zero successor/predecessor sets only as a last resort),
    so that blocks the sampler missed (e.g. compiler-generated checks or loop
    backedges in the middle of a hot path) inherit the frequency of the flow
    passing through them; a block is cold if its count is still 0 afterwards. A
    function none of whose blocks has a positive estimate is left untouched: the
    profile evidently does not cover it, so the absence of samples means
    nothing.

    When [dump] is provided, every block's measured and repaired frequency (and
    the instruction position stack each measured count came from) is printed to
    it (the [-dfdo] flag). *)
val reorder_cold_blocks :
  dump:Format.formatter option ->
  Source_position_profile.t ->
  Cfg_with_layout.t ->
  unit
