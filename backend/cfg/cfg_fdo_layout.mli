[@@@ocaml.warning "+a-40-41-42"]

(** Profile-guided basic-block layout.

    [reorder_blocks profile cl] lays the blocks out so that the hottest edges
    become fallthroughs and cold blocks sink to the end (the entry block stays
    first). Measured per-block counts are first repaired using flow conservation
    (exact propagation through single-successor/predecessor relationships and
    proportional scaling first; even splits into all-zero successor/predecessor
    sets only as a last resort), so that blocks the sampler missed (e.g.
    compiler-generated checks or loop backedges in the middle of a hot path)
    inherit the frequency of the flow passing through them; a block is cold if
    its count is still 0 afterwards. Edge weights come from the profile's
    pseudo-instrumentation label counts where the terminator carries recorded
    labels, and otherwise from the block counts (the whole count for
    single-successor flow, a flow bound for the rest). Blocks are then chained
    greedily along the hottest edges (bottom-up positioning à la Pettis-Hansen),
    which in particular orients conditional branches so their hot side falls
    through. A function none of whose blocks has a positive estimate is left
    untouched: the profile evidently does not cover it, so the absence of
    samples means nothing.

    When [dump] is provided, every block's measured and repaired frequency (and
    the instruction position stack each measured count came from), the weighted
    edges, and the resulting multi-block chains are printed to it (the [-dfdo]
    flag). *)
val reorder_blocks :
  dump:Format.formatter option ->
  Source_position_profile.t ->
  Cfg_with_layout.t ->
  unit
