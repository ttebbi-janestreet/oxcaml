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

    A label is identified by the debug info of the creating construct (its
    inlining stack selects the profile trie node the counts live at), a
    discriminator counted globally per compilation unit to distinguish
    several constructs sharing that debug info, and an edge discriminator
    naming the original construct's edge (the scrutinee value, for
    switches).  Inlining extends the creator stack like any other debug
    info and remaps discriminators to fresh ones (see [inline]). *)

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
  dinfo_edges: edge_labels option;
}

(** Successor information of a branching instruction.  [Positional] maps
    each successor position of the current representation ([ifso]/[ifnot]
    for a two-way conditional, [lt]/[eq]/[gt](/[uo]) for comparison
    terminators, the scrutinee value for a switch) to the set of labels
    carried by that edge.  Once linearization has fixed which side of a
    concrete conditional jump is taken, [Resolved] records the label sets
    of its two outcomes. *)
and edge_labels =
  | Positional of branch_label list array
  | Resolved of { taken: branch_label list; fallthrough: branch_label list }

and branch_label = private {
  label_creator: item list; (** outermost frame first *)
  label_disc: int;
  label_edge: int;
}

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

(** [inline ?remap_instance dbg ~from_inlined_body] composes debug info for
    inlining.  Pseudo-instrumentation labels in [from_inlined_body] get
    their creator stack extended by [dbg] and, when [remap_instance] is
    given (a string unique to this inlining instance), their discriminators
    remapped to fresh ones, memoized per (instance, discriminator) so that
    the edges of one construct keep sharing a discriminator. *)
val inline : ?remap_instance:string -> t -> from_inlined_body:t -> t

(** Create fresh labels for a new branching construct whose debug info is
    [t]: one singleton label set per successor position, with the given
    edge discriminators, all sharing a freshly counted discriminator. *)
val create_edge_labels : t -> edges:int array -> edge_labels

val next_branch_label_disc : unit -> int

(** Reset the per-compilation-unit discriminator state. *)
val reset_branch_label_discs : unit -> unit

(** Attach edge labels to the innermost frame (no-op on [none]). *)
val with_edge_labels : t -> edge_labels -> t

(** The edge labels of the innermost frame, if any. *)
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
