module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label

type kind =
  | Taken
  | Fallthrough
  | Target
  | Callsite

(* Items of the current compilation unit, in reverse emission order. *)
let items : (kind * L.t * Fdo_location.hashed list) list ref = ref []

(* The levels behind the hashes, when -fdo-names asks for them. *)
let names : (int64, Fdo_location.level) Hashtbl.t = Hashtbl.create 64

let record kind label (locations : Fdo_location.t list) =
  if locations <> []
  then (
    if !Oxcaml_flags.fdo_names
    then
      List.iter
        (List.iter (fun level ->
             Hashtbl.replace names (Fdo_location.hash_level level) level))
        locations;
    items := (kind, label, List.map Fdo_location.hash locations) :: !items)

let reset () =
  items := [];
  Hashtbl.reset names

(* Per compilation unit: the magic "FDOM", then a uleb128 version, then items,
   each starting with a uleb128 tag. Tags 0 (taken), 1 (fallthrough), 2 (target)
   and 3 (call site) are followed by the 8-byte absolute address (an assembler
   label) and the locations: a uleb128 count of locations, each a uleb128 count
   of levels followed by their 8-byte little-endian hashes, most-inlined first.
   Tag 4 (a name, only with -fdo-names) is followed by an 8-byte level hash and
   the level's components: a uleb128 count, each a uleb128 length and the bytes.
   The section is not needed at runtime, only at profile decode time, hence not
   allocated. *)
let section =
  Asm_targets.Asm_section.Custom
    { names = ["fdo_metadata"];
      flags = Some "";
      args = ["@progbits"];
      is_delayed = false
    }

let emit_section () =
  match List.rev !items with
  | [] -> ()
  | recorded ->
    let buf = Buffer.create 4096 in
    let flush () =
      D.string (Buffer.contents buf);
      Buffer.clear buf
    in
    let rec uleb n =
      if n < 0x80
      then Buffer.add_char buf (Char.chr n)
      else (
        Buffer.add_char buf (Char.chr (n land 0x7f lor 0x80));
        uleb (n lsr 7))
    in
    D.switch_to_section section;
    D.string "FDOM";
    uleb 7 (* version *);
    List.iter
      (fun (kind, label, locations) ->
        uleb
          (match kind with
          | Taken -> 0
          | Fallthrough -> 1
          | Target -> 2
          | Callsite -> 3);
        flush ();
        D.label label;
        (* A label reaching one machine edge through several successor positions
           must be counted once. *)
        let locations = List.sort_uniq compare locations in
        uleb (List.length locations);
        List.iter
          (fun hashes ->
            uleb (List.length hashes);
            List.iter (Buffer.add_int64_le buf) hashes)
          locations)
      recorded;
    Hashtbl.fold (fun hash level acc -> (hash, level) :: acc) names []
    |> List.sort compare
    |> List.iter (fun (hash, level) ->
        uleb 4;
        Buffer.add_int64_le buf hash;
        uleb (List.length level);
        List.iter
          (fun component ->
            uleb (String.length component);
            Buffer.add_string buf component)
          level);
    flush ();
    reset ()
