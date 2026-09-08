(** Emission of the FDO metadata section ("fdo_metadata"): the locations
    ([Fdo_location]) whose counts the LBR events at an address contribute to, so
    that profile decoding needs no debug info. *)

type kind =
  | Taken  (** counted when a taken branch leaves from the address *)
  | Fallthrough
      (** counted when the address is executed without branching away: it lies
          in the sequentially executed range between two consecutive LBR
          entries, the range's start included and its end (the branching
          instruction) excluded *)
  | Target
      (** counted when a taken branch lands on the address (a function's entry,
          only ever reached by calls and tail jumps); when the branch's source
          carries [Callsite] locations, the location is counted extended by each
          of them, giving the call graph: the callee's entry label with the call
          site as its context *)
  | Callsite
      (** the call site: not counted itself, but extends the [Target] locations
          of the addresses that calls and tail jumps from the address land on *)

(** Record the locations of one kind at one address of the current compilation
    unit. Only their hashes are emitted, and, with [-fdo-names], the names
    behind them. *)
val record : kind -> Asm_targets.Asm_label.t -> Fdo_location.t list -> unit

val reset : unit -> unit

(** Emit the metadata of the current compilation unit and reset the state. *)
val emit_section : unit -> unit
