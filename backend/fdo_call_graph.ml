module D = Asm_targets.Asm_directives
module S = Asm_targets.Asm_symbol

type callee =
  | Symbol of string
  | Entry of Fdo_location.hashed

let alias_symbol hashes =
  S.create_global
    (String.concat "_"
       ("caml_fdo" :: List.map (Printf.sprintf "%016Lx") hashes))

let callee_symbol = function
  | Symbol name -> S.create_global name
  | Entry hashes -> alias_symbol hashes

(* Edges of the current compilation unit, keyed by their endpoints. *)
let edges : (string * callee, int64) Hashtbl.t = Hashtbl.create 64

let add_edge ~from ~callee ~weight =
  let key = from, callee in
  let existing = Option.value (Hashtbl.find_opt edges key) ~default:0L in
  Hashtbl.replace edges key (Int64.add existing weight)

let reset () = Hashtbl.reset edges

(* The section is SHT_LLVM_CALL_GRAPH_PROFILE (0x6fff4c09) with SHF_EXCLUDE
   (not linked into the output) and SHF_MERGE with an entry size of 8, as lld
   requires. Each entry is the 8-byte weight, preceded at the same offset by two
   R_X86_64_NONE relocations: the caller, then the callee. *)
let section =
  Asm_targets.Asm_section.Custom
    { names = [".llvm.call_graph_profile"];
      flags = Some "eM";
      args = ["@0x6fff4c09"; "8"];
      is_delayed = false
    }

let emit_section () =
  if Hashtbl.length edges > 0
  then (
    let sorted =
      Hashtbl.fold (fun key weight acc -> (key, weight) :: acc) edges []
      |> List.sort compare
    in
    (* Aliases are defined by the callee's compilation unit only when the
       profile knows the function: reference them weakly, so that an edge to a
       function the linker never sees is dropped rather than an error. *)
    List.filter_map
      (fun ((_, callee), _) ->
        match callee with Entry hashes -> Some hashes | Symbol _ -> None)
      sorted
    |> List.sort_uniq compare
    |> List.iter (fun hashes -> D.weak (alias_symbol hashes));
    D.switch_to_section ~emit_label_on_first_occurrence:false section;
    List.iter
      (fun ((from, callee), weight) ->
        D.reloc_x86_64_none ~target_symbol:(S.create_global from);
        D.reloc_x86_64_none ~target_symbol:(callee_symbol callee);
        D.int64 weight)
      sorted;
    reset ())
