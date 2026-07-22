(** Decoding of code addresses into inlining stacks of source positions, by
    driving an external symbolizer (llvm-symbolizer by default). *)

type frame =
  { file : string;  (** as recorded in the DWARF line table, verbatim *)
    line : int;
    col : int
  }

(** Parse a "file:line:col" location (splitting from the right, so the file may
    contain colons). Raises [Failure] on anything else. Exposed for testing. *)
val parse_location : string -> frame

(** Parse llvm-symbolizer --inlines --output-style=LLVM output lines into one
    leaf-first stack per queried address; an empty stack is an address the
    symbolizer knows nothing about. Exposed for testing. *)
val parse_output : string list -> frame list list

(** [symbolize ~symbolizer ~binary ~addrs] is the leaf-first inlining stack of
    each address in [addrs], in order. *)
val symbolize :
  symbolizer:string -> binary:string -> addrs:int64 list -> frame list list
