(** Collection of patchprof instrumentation sites. *)

val enabled : unit -> bool

(** Record a site in the current function body. [site] labels the [cmp] or
    [test], [jcc] the conditional branch, and [fin] the instruction after it.
    [section] identifies the concrete text section, so a new metadata block is
    started when the section changes. [retaddr_offset] is the byte offset from
    the stack pointer to the return address at the site (a multiple of 8), if
    known. *)
val record :
  section:string ->
  site:Asm_targets.Asm_label.t ->
  jcc:Asm_targets.Asm_label.t ->
  fin:Asm_targets.Asm_label.t ->
  retaddr_offset:int option ->
  unit

val reset : unit -> unit

(** Emit [patchprof_sites] for the current compilation unit and reset the state. *)
val emit_section : unit -> unit
