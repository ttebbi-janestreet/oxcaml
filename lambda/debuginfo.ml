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

open! Int_replace_polymorphic_compare
open Lexing
open Location

module ZA = Zero_alloc_utils

module Scoped_location = struct
  type scope_item =
    | Sc_anonymous_function
    | Sc_value_definition
    | Sc_module_definition
    | Sc_class_definition
    | Sc_method_definition
    | Sc_partial_or_eta_wrapper
    | Sc_lazy

  let equal_scope_item si1 si2 =
    match si1, si2 with
    | Sc_anonymous_function, Sc_anonymous_function
    | Sc_value_definition, Sc_value_definition
    | Sc_module_definition, Sc_module_definition
    | Sc_class_definition, Sc_class_definition
    | Sc_method_definition, Sc_method_definition
    | Sc_partial_or_eta_wrapper, Sc_partial_or_eta_wrapper
    | Sc_lazy, Sc_lazy -> true
    | (Sc_anonymous_function | Sc_value_definition | Sc_module_definition
      | Sc_class_definition | Sc_method_definition | Sc_partial_or_eta_wrapper
      | Sc_lazy), _ -> false

  type scopes =
    | Empty
    | Cons of {item: scope_item; str: string; str_fun: string; name : string; prev: scopes;
               assume_zero_alloc: ZA.Assume_info.t;
               mangling_item:
                 Compilation_unit.t Structured_mangling.path_item option}

  let str = function
    | Empty -> ""
    | Cons r -> r.str

  let str_fun = function
    | Empty -> "(fun)"
    | Cons r -> r.str_fun

  let cons scopes item str name mangling_item ~assume_zero_alloc =
    Cons {item; str; str_fun = str ^ ".(fun)"; name; prev = scopes;
          assume_zero_alloc; mangling_item}

  let empty_scopes = Empty

  let add_parens_if_symbolic = function
    | "" -> ""
    | s ->
       match s.[0] with
       | 'a'..'z' | 'A'..'Z' | '_' | '0'..'9' -> s
       | _ -> "(" ^ s ^ ")"

  let dot ?(sep = ".") ?no_parens scopes s =
    let s =
      match no_parens with
      | None -> add_parens_if_symbolic s
      | Some () -> s
    in
    match scopes with
    | Empty -> s
    | Cons {str; _} -> str ^ sep ^ s

  let enter_anonymous_function ~scopes ~assume_zero_alloc ~loc =
    let str = str_fun scopes in
    let (file, line, col) = Location.get_pos_info loc.loc_start in
    let file = Filename.basename file in
    let mangling_item : _ Structured_mangling.path_item option =
      Some (Anonymous_function (line, col, Some file))
    in
    Cons {item = Sc_anonymous_function; str; str_fun = str; name = ""; prev = scopes;
          assume_zero_alloc; mangling_item }

  let enter_anonymous_module ~scopes ~loc =
    let str = str scopes in
    let (file, line, col) = Location.get_pos_info loc.loc_start in
    let file = Filename.basename file in
    let mangling_item : _ Structured_mangling.path_item option =
      Some (Anonymous_module (line, col, Some file))
    in
    Cons {item = Sc_module_definition; str; str_fun = str ^ ".(fun)"; name = "";
          prev = scopes; assume_zero_alloc = ZA.Assume_info.none;
          mangling_item }

  let enter_value_definition ~scopes ~assume_zero_alloc id =
    cons scopes Sc_value_definition (dot scopes (Ident.name id)) (Ident.name id)
      (Some (Function (Ident.name id)))
      ~assume_zero_alloc

  let enter_compilation_unit ~scopes cu =
    let name = Compilation_unit.name_as_string cu in
    cons scopes Sc_module_definition (dot scopes name) name
      (Some (Compilation_unit cu))
      ~assume_zero_alloc:ZA.Assume_info.none

  let enter_module_definition ~scopes id =
    let name = Ident.name id in
    cons scopes Sc_module_definition (dot scopes name) name (Some (Module name))
      ~assume_zero_alloc:ZA.Assume_info.none

  let enter_class_definition ~scopes id =
    let name = Ident.name id in
    cons scopes Sc_class_definition (dot scopes name) name (Some (Class name))
      ~assume_zero_alloc:ZA.Assume_info.none

  let enter_method_definition ~scopes (s : Asttypes.label) =
    let str =
      match scopes with
      | Cons {item = Sc_class_definition; _} -> dot ~sep:"#" scopes s
      | _ -> dot scopes s
    in
    cons scopes Sc_method_definition str s
      ~assume_zero_alloc:ZA.Assume_info.none (Some (Function s))

  let enter_lazy ~scopes = cons scopes Sc_lazy (str scopes) ""
                             ~assume_zero_alloc:ZA.Assume_info.none None

  let enter_partial_or_eta_wrapper ~scopes ~loc =
    let (file, line, col) = Location.get_pos_info loc.loc_start in
    let file = Filename.basename file in
    cons scopes Sc_partial_or_eta_wrapper (dot ~no_parens:() scopes "(partial)")
      "" ~assume_zero_alloc:ZA.Assume_info.none
      (Some (Partial_function (line, col, Some file)))

  let update_assume_zero_alloc ~scopes ~assume_zero_alloc =
    match scopes with
    | Empty -> Empty
    | Cons r ->
      if ZA.Assume_info.equal r.assume_zero_alloc assume_zero_alloc
      then scopes
      else
        let assume_zero_alloc =
          ZA.Assume_info.meet r.assume_zero_alloc assume_zero_alloc
        in
        Cons { r with assume_zero_alloc }

  let get_assume_zero_alloc ~scopes =
    match scopes with
    | Empty -> ZA.Assume_info.none
    | Cons { assume_zero_alloc; _ } -> assume_zero_alloc

  let string_of_scopes ~include_zero_alloc = function
    | Empty -> "<unknown>"
    | Cons {str; assume_zero_alloc; _} ->
      if include_zero_alloc then
        str^(ZA.Assume_info.to_string assume_zero_alloc)
      else
        str

  let string_of_scopes ~include_zero_alloc =
    let module StringSet = Set.Make (String) in
    let repr = ref StringSet.empty in
    fun scopes ->
      let res = string_of_scopes scopes ~include_zero_alloc in
      match StringSet.find_opt res !repr with
      | Some x -> x
      | None ->
        repr := StringSet.add res !repr;
        res

  let rec outermost_scope scopes =
    match scopes with
    | Empty -> None
    | Cons { prev = Empty; _ } -> Some scopes
    | Cons { prev } -> outermost_scope prev

  let compilation_unit scopes =
    match outermost_scope scopes with
    | None -> None
    | Some scopes ->
      (* CR mshinwell: this won't work with -pack, but it isn't clear how
         to fix it easily, and we're not using packs anyway these days. *)
      match scopes with
      | Cons { item = Sc_module_definition; str; _ } ->
        Some (Compilation_unit.of_string str)
      | _ -> None

  type t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }

  let of_location ~scopes loc =
    if Location.is_none loc then
      Loc_unknown
    else
      Loc_known { loc; scopes }

  let to_location = function
    | Loc_unknown -> Location.none
    | Loc_known { loc; _ } -> loc

  let string_of_scoped_location ~include_zero_alloc = function
    | Loc_unknown -> "??"
    | Loc_known { loc = _; scopes } -> string_of_scopes ~include_zero_alloc scopes

  let map_scopes f t =
    match t with
    | Loc_unknown -> Loc_unknown
    | Loc_known { loc; scopes } -> Loc_known { loc; scopes = f ~scopes ~loc }
end

type item = {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_scopes: Scoped_location.scopes;
  dinfo_uid: string option;
  dinfo_function_symbol: string option;
  dinfo_dir: string option;
}

let item_with_uid_and_function_symbol item ~dinfo_uid ~dinfo_function_symbol =
  { item with dinfo_uid; dinfo_function_symbol }

module Dbg = struct
 type t = item list

  let[@inline always] compare_aux dbg1 dbg2 =
    let rec loop ds1 ds2 =
      match ds1, ds2 with
      | [], [] -> 0
      | _ :: _, [] -> 1
      | [], _ :: _ -> -1
      | d1 :: ds1, d2 :: ds2 ->
       (* The record patterns below list every field explicitly, making it
          clear which fields participate in the comparison.  The
          [dinfo_scopes] and [dinfo_function_symbol] fields are deliberately
          not compared. *)
       let { dinfo_file = dinfo_file1;
                            dinfo_line = dinfo_line1;
                            dinfo_char_start = dinfo_char_start1;
                            dinfo_char_end = dinfo_char_end1;
                            dinfo_start_bol = dinfo_start_bol1;
                            dinfo_end_bol = dinfo_end_bol1;
                            dinfo_end_line = dinfo_end_line1;
                            dinfo_scopes = _;
                            dinfo_uid = dinfo_uid1;
                            dinfo_function_symbol = _;
                            dinfo_dir = dinfo_dir1 } = d1
       in
       let { dinfo_file = dinfo_file2;
                            dinfo_line = dinfo_line2;
                            dinfo_char_start = dinfo_char_start2;
                            dinfo_char_end = dinfo_char_end2;
                            dinfo_start_bol = dinfo_start_bol2;
                            dinfo_end_bol = dinfo_end_bol2;
                            dinfo_end_line = dinfo_end_line2;
                            dinfo_scopes = _;
                            dinfo_uid = dinfo_uid2;
                            dinfo_function_symbol = _;
                            dinfo_dir = dinfo_dir2 } = d2
       in
       let c = String.compare dinfo_file1 dinfo_file2 in
       if c <> 0 then c else
       let c = Int.compare dinfo_line1 dinfo_line2 in
       if c <> 0 then c else
       let c = Int.compare dinfo_char_end1 dinfo_char_end2 in
       if c <> 0 then c else
       let c = Int.compare dinfo_char_start1 dinfo_char_start2 in
       if c <> 0 then c else
       let c = Int.compare dinfo_start_bol1 dinfo_start_bol2 in
       if c <> 0 then c else
       let c = Int.compare dinfo_end_bol1 dinfo_end_bol2 in
       if c <> 0 then c else
       let c = Int.compare dinfo_end_line1 dinfo_end_line2 in
       if c <> 0 then c else
       let c = Option.compare String.compare dinfo_dir1 dinfo_dir2 in
       if c <> 0 then c else
       let c = Option.compare String.compare dinfo_uid1 dinfo_uid2 in
       if c <> 0 then c else
       loop ds1 ds2
    in
    loop dbg1 dbg2

  (* CR-someday afrisch: FWIW, the current compare function does not seem very
     good, since it reverses the two lists. I don't know how long the lists are,
     nor if the specific currently implemented ordering is useful in other
     contexts, but if one wants to use Map, a more efficient comparison should
     be considered. *)
  let compare dbg1 dbg2 = compare_aux (List.rev dbg1) (List.rev dbg2)

  (* Outermost inlined location first. *)
  let compare_outer_first dbg1 dbg2 = compare_aux dbg1 dbg2

  let is_none dbg =
    match dbg with
    | [] -> true
    | _ :: _ -> false

  let hash dbg =
    List.fold_left (fun hash item -> Hashtbl.hash (hash, item)) 0 dbg

  let to_string dbg =
    match dbg with
    | [] -> ""
    | ds ->
      let items =
        List.map
          (fun d ->
             Printf.sprintf "%s:%d,%d-%d"
               d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end)
          ds
      in
      "{" ^ String.concat ";" items ^ "}"

  let to_list t = t

  let length t = List.length t

end

(* Pseudo-instrumentation labels for branch profiling: a label is created
   for each control-flow edge of a branching/switching construct when the
   construct is created or lowered, is carried in the debug info of the
   resulting branch instructions (one set of labels per outgoing edge, since
   transformations may stack several labels on one edge, e.g. by constant
   folding a branch), and is preserved - only ever swapped or rearranged
   along with the control flow - until emission into the executable's
   metadata.  A label is a location (a stack of levels, see [Fdo_location])
   whose first level identifies the edge structurally, not by source
   positions: the anchor of the enclosing function (or compilation unit), the
   index of the branching construct among those of the function's body in
   translation order, and the edge's index.  Inlining appends the call sites
   the label was inlined through, exactly like the debug info of the inlined
   code.

   Successor information travels in two forms.  While the branch still has
   positional successors, [Positional] maps each successor position of the
   current representation (its meaning follows the construct: [ifso]/[ifnot]
   for a two-way conditional, [lt]/[eq]/[gt](/[uo]) for comparison
   terminators, the scrutinee value for a switch) to the set of labels
   carried by that edge.  Once linearization has fixed which side of a
   concrete conditional jump is taken, [Resolved] records the label sets of
   its two outcomes directly.  A function application carries [Callsite]
   instead (see the interface). *)
type edge_labels =
  | Positional of branch_label list array
  | Resolved of { taken: branch_label list; fallthrough: branch_label list }
  | Callsite of Fdo_location.t

and branch_label = { location: Fdo_location.t; rider: bool }

let item_level item =
  [ item.dinfo_file;
    string_of_int item.dinfo_line;
    string_of_int item.dinfo_char_start ]

(* Like [assume_zero_alloc], [edge_labels] is not debug information proper
   but rides along because debug info reaches every branch instruction. *)
type t =
  { dbg : Dbg.t;
    assume_zero_alloc : ZA.Assume_info.t;
    edge_labels : edge_labels option
  }

let none =
  { dbg = []; assume_zero_alloc = ZA.Assume_info.none; edge_labels = None }

let of_items items =
  { dbg = items; assume_zero_alloc = ZA.Assume_info.none; edge_labels = None }

let mapi_items { dbg; assume_zero_alloc; edge_labels } ~f =
  { dbg = List.mapi f dbg;
    assume_zero_alloc;
    edge_labels
  }

let to_items t = t.dbg

let to_string { dbg; assume_zero_alloc; } =
  let s = Dbg.to_string dbg in
  let a = ZA.Assume_info.to_string assume_zero_alloc in
  s^a

let item_from_location ~scopes loc =
  let valid_endpos =
    String.equal loc.loc_end.pos_fname loc.loc_start.pos_fname in
  { dinfo_file = loc.loc_start.pos_fname;
    dinfo_line = loc.loc_start.pos_lnum;
    dinfo_char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_char_end =
      if valid_endpos
      then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
      else loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_start_bol = loc.loc_start.pos_bol;
    dinfo_end_bol =
      if valid_endpos then loc.loc_end.pos_bol
      else loc.loc_start.pos_bol;
    dinfo_end_line =
      if valid_endpos then loc.loc_end.pos_lnum
      else loc.loc_start.pos_lnum;
    dinfo_scopes = scopes;
    dinfo_uid = None;
    dinfo_function_symbol = None;
    dinfo_dir = !Clflags.directory;
  }

let from_location = function
  | Scoped_location.Loc_unknown -> none
  | Scoped_location.Loc_known {scopes; loc} ->
    assert (not (Location.is_none loc));
    let assume_zero_alloc = Scoped_location.get_assume_zero_alloc ~scopes in
    { dbg = [item_from_location ~scopes loc]; assume_zero_alloc;
      edge_labels = None }

let to_location { dbg; assume_zero_alloc=_ } =
  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _ :: r -> last r
  in
  match last dbg with
  | None -> Location.none
  | Some d ->
    let loc_start =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_line;
        pos_bol = d.dinfo_start_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_start;
      } in
    let loc_end =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_end_line;
        pos_bol = d.dinfo_end_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_end;
      } in
    { loc_ghost = false; loc_start; loc_end; }

(* [f] maps the locations of all the labels. *)
let map_edge_labels t ~f =
  let label l = { l with location = f l.location } in
  let edge_labels =
    match t.edge_labels with
    | None -> None
    | Some (Positional sets) ->
      Some (Positional (Array.map (List.map label) sets))
    | Some (Resolved { taken; fallthrough }) ->
      Some (Resolved { taken = List.map label taken;
                       fallthrough = List.map label fallthrough })
    | Some (Callsite location) -> Some (Callsite (f location))
  in
  { t with edge_labels }

let inline { dbg = dbg1; assume_zero_alloc = a1; edge_labels = _ }
      ~from_inlined_body:({ dbg = dbg2; assume_zero_alloc = a2;
                           edge_labels = _ } as body) =
  (* Pseudo-instrumentation labels carried by the inlinee's branches record
     the call site exactly like the carrying debug info: [dbg1] is outermost
     first, a label innermost first. *)
  let body = map_edge_labels body ~f:(fun location ->
    location @ List.rev_map item_level dbg1) in
  { dbg = dbg1 @ dbg2;
    assume_zero_alloc =
      (* Drop "inferred" zero_alloc annotation from a call when
         the callee is inlined. *)
      if ZA.Assume_info.is_inferred a1 then a2 else
      ZA.Assume_info.meet a1 a2;
    edge_labels = body.edge_labels }

let specialize_edge_labels ~site body =
  let suffix = List.concat_map item_level site.dbg in
  (* Only the outermost level is the copied function's own; the inner ones
     belong to the callees inlined into it, copies or not. *)
  let rec rename = function
    | [] -> []
    | [level] -> [level @ suffix]
    | level :: levels -> level :: rename levels
  in
  map_edge_labels body ~f:rename

let with_edge_labels t edges = { t with edge_labels = Some edges }

let edge_labels t = t.edge_labels

(* A function's entry is an edge like any other, labelled by the function's
   anchor alone; it rides on the function's debug info as a one-position set. *)
let edge_label location = { location; rider = false }

let with_entry_label t =
  match t.dbg with
  | [] -> t
  | item :: _ ->
    with_edge_labels t (Positional [| [edge_label [item_level item]] |])

let entry_labels t =
  match t.edge_labels with
  | Some (Positional [| labels |]) -> labels
  | Some (Positional _ | Resolved _ | Callsite _) | None -> []

let add_riders t ~position locations =
  match t.edge_labels with
  | Some (Positional sets) when position >= 0 && position < Array.length sets ->
    let sets = Array.copy sets in
    let equal_location = List.equal (List.equal String.equal) in
    let present location =
      List.exists (fun l -> l.rider && equal_location l.location location)
        sets.(position)
    in
    let riders =
      List.filter_map (fun location ->
        if present location then None
        else Some { location; rider = true })
        locations
    in
    sets.(position) <- sets.(position) @ riders;
    with_edge_labels t (Positional sets)
  | Some (Positional _ | Resolved _ | Callsite _) | None -> t

let without_edge_labels t = { t with edge_labels = None }

let with_callsite_label t =
  match t.dbg with
  | [] -> t
  | item :: _ -> with_edge_labels t (Callsite [item_level item])

let callsite_label t =
  match t.edge_labels with
  | Some (Callsite location) -> Some location
  | Some (Positional _ | Resolved _) | None -> None

let create_edge_labels ~anchor ~index ~num_edges =
  Positional
    (Array.init num_edges (fun i ->
       [edge_label [anchor @ [string_of_int index; string_of_int i]]]))

let is_none { dbg; assume_zero_alloc; edge_labels } =
  ZA.Assume_info.is_none assume_zero_alloc && Dbg.is_none dbg
  && Option.is_none edge_labels

let compare { dbg = dbg1; assume_zero_alloc = a1; edge_labels = _ }
      { dbg = dbg2; assume_zero_alloc = a2; edge_labels = _ } =
  let res = Dbg.compare dbg1 dbg2 in
  if res <> 0 then res else ZA.Assume_info.compare a1 a2

let print_item ppf item =
  Format.fprintf ppf "%a:%i"
    Location.print_filename item.dinfo_file
    item.dinfo_line;
  if item.dinfo_char_start >= 0 then begin
    Format.fprintf ppf ",%i--%i" item.dinfo_char_start item.dinfo_char_end
  end

let rec print_compact ppf t =
  match t with
  | [] -> ()
  | [item] -> print_item ppf item
  | item::t ->
    print_item ppf item;
    Format.fprintf ppf ";";
    print_compact ppf t

let print_compact ppf { dbg; } = print_compact ppf dbg

let doc_print_compact ppf t =
  (* We use [deprecated_printer] instead of changing the formatting code in this
     file to be compatible with upstream (which hasn't switched yet for this
     file). *)
  Format_doc.deprecated_printer (fun fmt -> print_compact fmt t) ppf

let rec print_compact_extended ppf t =
  let print_item item =
    print_item ppf item;
    (match item.dinfo_uid with
    | None -> ()
    | Some uid -> Format.fprintf ppf "[%s]" uid);
    (match item.dinfo_function_symbol with
    | None -> ()
    | Some function_symbol -> Format.fprintf ppf "[FS=%s]" function_symbol)
  in
  match t with
  | [] -> ()
  | [item] -> print_item item
  | item::t ->
    print_item item;
    Format.fprintf ppf ";";
    print_compact_extended ppf t

let print_compact_extended ppf { dbg; } = print_compact_extended ppf dbg

let merge ~into:{ dbg = dbg1; assume_zero_alloc = a1; edge_labels = e1 }
      { dbg = dbg2; assume_zero_alloc = a2; edge_labels = e2 } =
  (* Keep the first [dbg] info to match existing behavior.
     When assume_zero_alloc is only on one of the inputs but not both, keep [dbg]
     from the other.
  *)
  let dbg, edge_labels =
    match ZA.Assume_info.is_none a1, ZA.Assume_info.is_none a2 with
    | false, true -> dbg2, e2
    | _,  _ -> dbg1, e1
  in
  { dbg;
    assume_zero_alloc = ZA.Assume_info.join a1 a2;
    edge_labels
  }

let assume_zero_alloc t = t.assume_zero_alloc

let get_dbg t = t.dbg

let rec path_of_debug_info_scopes acc (scopes : Scoped_location.scopes) =
  match scopes with
  | Empty -> acc
  | Cons { prev; mangling_item = None; _ } -> path_of_debug_info_scopes acc prev
  | Cons { prev; mangling_item = Some mangling_item; _ } ->
    path_of_debug_info_scopes (mangling_item :: acc) prev

let to_structured_mangling_path ~name dbg :
    Compilation_unit.t Structured_mangling.path =
  (* An anonymous function or module is precisely located by its own position
     information, so the scopes enclosing it (its ancestors, up to the
     compilation unit) are redundant. [located_by_child] becomes true once we
     have passed such an item; while it is set we drop every enclosing item
     except compilation units, which keep it and reset the flag. (There is no
     need to worry about the inlining marker, since it is inserted later by
     [mangle_ident].) *)
  let rec collapse_anonymous ~located_by_child
      (path : Compilation_unit.t Structured_mangling.path) =
    match path with
    | [] -> []
    | (Compilation_unit _ as cu) :: path ->
      cu :: collapse_anonymous ~located_by_child:false path
    | _ :: path when located_by_child ->
      collapse_anonymous ~located_by_child path
    | ((Anonymous_function _ | Anonymous_module _) as item) :: path ->
      item :: collapse_anonymous ~located_by_child:true path
    | item :: path -> item :: collapse_anonymous ~located_by_child:false path
  in
  (* Drop the suffix of partial applications and the innermost named function
     (if any), then end the path with [name]. Using [name] preserves the stamps
     it includes for uniqueness; we append it even after an innermost anonymous
     function (which is kept for its position) so the stamps are not lost. *)
  let rec drop_partials_and_adjust_function_name ~name
      (path : Compilation_unit.t Structured_mangling.path)
      =
    match path with
    | Partial_function _ :: path ->
      drop_partials_and_adjust_function_name ~name path
    | Function _ :: path -> Structured_mangling.Function name :: path
    | path -> Structured_mangling.Function name :: path
  in
  let path_from_debug =
    match to_items dbg with
    | [] -> []
    | item :: _ ->
      (* CR sspies: The list of debuginfo items can contain more than one item
         in case of inlining (see [merge]). For the moment, we use the first
         item. In the future, it would be good to track the original source of
         the function. See #5099. *)
      path_of_debug_info_scopes [] item.dinfo_scopes
  in
  List.rev path_from_debug
  |> collapse_anonymous ~located_by_child:false
  |> drop_partials_and_adjust_function_name ~name
  |> List.rev
