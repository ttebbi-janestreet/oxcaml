[@@@ocaml.warning "+a-40-41-42"]

(** Profile-guided basic-block layout.

    [reorder_blocks profile cl] lays the blocks out so that the hottest edges
    become fallthroughs, loops are closed by backwards conditional branches, and
    cold blocks sink to the end (the entry block stays first). The profile
    measures edges: the pseudo-instrumentation labels of conditional and switch
    edges, and the function's entry. A block's measured count is derived from
    the edges around it, and the counts are then repaired using flow
    conservation, so that blocks with no labelled edge around them (e.g.
    compiler-generated checks or loop backedges in the middle of a hot path)
    inherit the frequency of the flow passing through them. Edge weights are the
    measured ones where the terminator carries recorded labels, and otherwise
    come from the block counts (the whole count for single-successor flow, a
    flow bound for the rest). The layout is then computed by ext-TSP
    ([Cfg_fdo_ext_tsp]) or, with [-fdo-layout greedy], by following the heaviest
    edges from the entry and rotating loops so that their backedge is a
    conditional branch (see [build_layout] in the implementation). A function
    none of whose blocks has a positive estimate is left untouched: the profile
    evidently does not cover it, so the absence of samples means nothing.

    When [dump] is provided, the function's entry label, every block's measured
    and repaired frequency, and the edges with their labels and weights are
    printed to it (the [-dfdo] flag). *)
val reorder_blocks :
  dump:Format.formatter option ->
  Source_position_profile.t ->
  Cfg_with_layout.t ->
  unit
