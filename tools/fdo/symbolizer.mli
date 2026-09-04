(** Decoding of code addresses into inlining stacks of source positions, by
    driving an external symbolizer (llvm-symbolizer by default). *)

type frame =
  { file : string;  (** as recorded in the DWARF line table, verbatim *)
    line : int;
    col : int;
    discriminator : int
        (** the DWARF discriminator of the line-table entry, 0 when absent
            (currently unused by the decoder) *)
  }

(** Parse llvm-symbolizer --inlines --verbose output into one leaf-first stack
    per queried address; an empty stack is an address the symbolizer knows
    nothing about. Exposed for testing. *)
val parse_output : string list -> frame list list

(** [symbolize ~symbolizer ~binary ~addrs] is the leaf-first inlining stack of
    each address in [addrs], in order. *)
val symbolize :
  symbolizer:string -> binary:string -> addrs:int64 list -> frame list list
