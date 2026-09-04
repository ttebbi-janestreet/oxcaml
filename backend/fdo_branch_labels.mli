(** Collection and emission of the branch-label metadata ("fdo_branch_labels"
    section): for every conditional branch carrying resolved
    pseudo-instrumentation labels, the label sets of its taken and fallthrough
    edges, keyed by the branch instruction's address. Profile decoding
    attributes branch counts observed at those addresses (e.g. from LBR branch
    stacks) to the labels. *)

(** Record one conditional branch of the current compilation unit. [jcc] labels
    the branch instruction and [fin] the instruction after it (the fallthrough
    address). [dbg] is the branch's debug info; only resolved edge labels are
    emitted. *)
val record :
  jcc:Asm_targets.Asm_label.t ->
  fin:Asm_targets.Asm_label.t ->
  dbg:Debuginfo.t ->
  unit

val reset : unit -> unit

(** Emit the metadata of the current compilation unit and reset the state. *)
val emit_section : unit -> unit
