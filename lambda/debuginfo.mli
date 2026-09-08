(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module ZA = Zero_alloc_utils

module Scoped_location : sig
  type scope_item = private
    | Sc_anonymous_function
    | Sc_value_definition
    | Sc_module_definition
    | Sc_class_definition
    | Sc_method_definition
    | Sc_partial_or_eta_wrapper
    | Sc_lazy

  val equal_scope_item : scope_item -> scope_item -> bool

  type scopes = private
    | Empty
    | Cons of {item: scope_item; str: string; str_fun: string; name : string; prev: scopes;
               assume_zero_alloc: ZA.Assume_info.t;
               mangling_item:
                 Compilation_unit.t Structured_mangling.path_item option}

  val string_of_scopes : include_zero_alloc:bool -> scopes -> string

  val compilation_unit : scopes -> Compilation_unit.t option

  val empty_scopes : scopes
  val enter_anonymous_function :
    scopes:scopes ->
    assume_zero_alloc:ZA.Assume_info.t ->
    loc:Location.t ->
    scopes
  val enter_anonymous_module :
    scopes:scopes ->
    loc:Location.t ->
    scopes
  val enter_value_definition :
    scopes:scopes -> assume_zero_alloc:ZA.Assume_info.t -> Ident.t -> scopes
  val enter_compilation_unit : scopes:scopes -> Compilation_unit.t -> scopes
  val enter_module_definition : scopes:scopes -> Ident.t -> scopes
  val enter_class_definition : scopes:scopes -> Ident.t -> scopes
  val enter_method_definition : scopes:scopes -> Asttypes.label -> scopes
  val enter_lazy : scopes:scopes -> scopes
  val enter_partial_or_eta_wrapper : scopes:scopes -> loc:Location.t -> scopes
  val update_assume_zero_alloc :
    scopes:scopes -> assume_zero_alloc:ZA.Assume_info.t -> scopes
  val get_assume_zero_alloc : scopes:scopes -> ZA.Assume_info.t

  type t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }

  val of_location : scopes:scopes -> Location.t -> t
  val to_location : t -> Location.t
  val string_of_scoped_location : include_zero_alloc:bool -> t -> string

  val map_scopes : (scopes:scopes -> loc:Location.t -> scopes) -> t -> t
end

(** Pseudo-instrumentation labels for branch profiling: a label is created
    for each control-flow edge of a branching/switching construct when the
    construct is created or lowered, is carried in the debug info of the
    resulting branch instructions (one set of labels per outgoing edge,
    since transformations may stack several labels on one edge, e.g. by
    constant folding a branch), and is preserved - only ever swapped or
    rearranged along with the control flow - until emission into the
    executable's metadata, where profile decoding attributes each
    instrumented branch's counts to the labels of its taken and fallthrough
    edges.

    A label is a location ([Fdo_location.t]): a stack of levels, each a list
    of string components hashed as a unit, most inlined first.  Its first
    level identifies the edge structurally, not by source positions: the
    anchor of the enclosing function (or compilation unit), the index of the
    branching construct among those of the function's body in translation
    order, and the edge's index.  The remaining levels are the call sites the
    label was inlined through, innermost first, exactly like the debug info of
    the inlined code (see [inline]); in the profile they select the trie node
    the counts live at. *)

type item = private {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_scopes: Scoped_location.scopes;
  (** See the [Inlined_debuginfo] module in Flambda 2 for an explanation
      of the uid and function symbol fields.  (They are used for generation
      of DWARF inlined frame information.)  These fields should only be
      set to [Some] by Flambda 2. *)
  dinfo_uid: string option;
  dinfo_function_symbol: string option;
  dinfo_dir: string option;
}

(** Successor information of a branching instruction.  [Positional] maps
    each successor position of the current representation ([ifso]/[ifnot]
    for a two-way conditional, [lt]/[eq]/[gt](/[uo]) for comparison
    terminators, the scrutinee value for a switch) to the set of labels
    carried by that edge.  Once linearization has fixed which side of a
    concrete conditional jump is taken, [Resolved] records the label sets
    of its two outcomes.  [Callsite] is carried by a function application
    instead: the label of the call site, joined at profile decoding time
    with the entry label of the function the call lands in, giving the
    call graph. *)
type edge_labels =
  | Positional of branch_label list array
  | Resolved of { taken: branch_label list; fallthrough: branch_label list }
  | Callsite of Fdo_location.t

(** A label on an edge either identifies the edge ([rider = false]: the
    profile count of its location is the count of the edge) or rides on it
    ([rider = true]: the location of something else that executes exactly
    when the edge does, namely the entry of a function inlined at the head
    of the code the edge leads to, joined with its call site as the decoded
    call graph joins them for real calls).  The metadata records both alike;
    only consumers measuring the edge itself must skip riders, whose counts
    would double the edge's. *)
and branch_label = { location: Fdo_location.t; rider: bool }

(** The location level of a source position: [[file; line; col]]. *)
val item_level : item -> Fdo_location.level

val item_with_uid_and_function_symbol : item -> dinfo_uid:string option
  -> dinfo_function_symbol:string option -> item

type t

val none : t

val is_none : t -> bool

val of_items : item list -> t

val mapi_items : t -> f:(int -> item -> item) -> t

val to_items : t -> item list

val to_string : t -> string

val from_location : Scoped_location.t -> t

val to_location : t -> Location.t

(** [inline dbg ~from_inlined_body] composes debug info for inlining.
    Pseudo-instrumentation labels in [from_inlined_body] get the levels of the
    call site [dbg] appended (innermost first). *)
val inline : t -> from_inlined_body:t -> t

(** [specialize_edge_labels ~site dbg], for debug info from the body of a
    function that inlining copied (it was defined inside the inlined body, e.g.
    by an inlined functor application), makes its pseudo-instrumentation labels
    those of the copy: the outermost level of each label, the one in the copied
    function's own code (the inner levels are the inlined callees'), gets the
    components of the positions of [site], the inlined application, appended.
    Unlike [inline], this adds no levels (the copy's code was not inlined
    anywhere) and leaves the positions, and thus the DWARF, of the original
    definition alone. *)
val specialize_edge_labels : site:t -> t -> t

(** [with_entry_label dbg], for the debug info of a function, attaches the label
    of the function's entry edge: its anchor (the level of its position), as a
    one-position edge label set. [entry_labels] reads it back (possibly extended
    by inlining contexts); [[]] when there is none. *)
val with_entry_label : t -> t

val entry_labels : t -> branch_label list

(** [add_riders dbg ~position locations] adds the riders [locations] to the
    label set at successor [position] of the positional edge labels of [dbg]
    (position 0 for a function's entry label), skipping those already
    there.  [dbg] is returned unchanged when it carries no positional labels
    or too few positions. *)
val add_riders : t -> position:int -> Fdo_location.t list -> t

(** The positions alone, for reusing a function's debug info for code that is
    not its entry (e.g. the allocation of its closure). *)
val without_edge_labels : t -> t

(** [with_callsite_label dbg], for the debug info of a function application,
    labels the call site by its position; unchanged when there is none. *)
val with_callsite_label : t -> t

(** The call site label carried by the debug info of an application, if any. *)
val callsite_label : t -> Fdo_location.t option

(** Create fresh labels for the [index]th branching construct of the function
    or unit anchored at [anchor]: one singleton label set per successor
    position [i], whose label is the single level [anchor @ [index; i]] (as
    strings). *)
val create_edge_labels :
  anchor:string list -> index:int -> num_edges:int -> edge_labels

(** Attach edge labels to the debug info, which may carry no position: a
    branch on a bare variable has none.  Like the zero_alloc assumption, the
    labels ride along with the positions but are not part of [compare]. *)
val with_edge_labels : t -> edge_labels -> t

(** The edge labels carried by the debug info, if any. *)
val edge_labels : t -> edge_labels option

val compare : t -> t -> int

val print_compact : Format.formatter -> t -> unit

(** Like [print_compact] but uses [Format_doc.formatter]. *)
val doc_print_compact : Format_doc.formatter -> t -> unit

(** Like [print_compact] but also prints uid and function symbol info. *)
val print_compact_extended : Format.formatter -> t -> unit

val merge : into:t -> t -> t

val assume_zero_alloc : t -> ZA.Assume_info.t

(** [to_structured_mangling_path] converts the debug info into a mangling path.
    In all cases, the [name] is used to populate the last element of the path.
*)
val to_structured_mangling_path :
  name:string -> t -> Compilation_unit.t Structured_mangling.path

module Dbg : sig
  type t

  (** [compare] and [hash] ignore the [dinfo_scopes] field of item;
      [compare] additionally ignores [dinfo_function_symbol]. *)

  val is_none : t -> bool

  (** [compare] Inner-most inlined debug info is used first. Allocates. *)
  val compare : t -> t -> int

  (** [compare_outer_first] Outer-most inlined debug info is used first.
      Does not allocate. *)
  val compare_outer_first : t -> t -> int

  val hash : t -> int
  val to_list : t -> item list
  val length : t -> int
end

val get_dbg : t -> Dbg.t
