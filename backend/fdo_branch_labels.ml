module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label

type site =
  { jcc : L.t;
    fin : L.t;
    dbg : Debuginfo.t
  }

(* Sites of the current compilation unit, in reverse emission order. *)
let sites : site list ref = ref []

let record ~jcc ~fin ~dbg = sites := { jcc; fin; dbg } :: !sites

let reset () = sites := []

(* Branch-label metadata: for every conditional branch that carries resolved
   pseudo-instrumentation labels, the label sets of its taken and fallthrough
   edges, keyed by the address of the branch instruction (the source address of
   LBR branch records) together with its fallthrough address (which lets the
   decoder classify an observed execution of the branch as taken or fallen
   through). Not needed at runtime, only at profile decode time, hence a
   non-allocated section.

   Per compilation unit: the magic "FDLB", then uleb128-encoded items
   (interspersed with 8-byte absolute addresses, see below), the first of which
   is the version. Each item starts with a tag: 1 label definition: the label's
   location stack (see [Source_position_profile.frames_of_branch_label]) as the
   number of locations, then per location (leaf first) its 8-byte little-endian
   hash. Definitions are numbered from 1 in order of appearance. 0 site entry:
   followed by the 8-byte absolute address of the branch instruction and the
   8-byte absolute address of the instruction after it (its fallthrough
   address), then the number of taken-edge labels and their ids, then the number
   of fallthrough-edge labels and their ids. *)
let section =
  Asm_targets.Asm_section.Custom
    { names = ["fdo_branch_labels"];
      flags = Some "";
      args = ["@progbits"];
      is_delayed = false
    }

let add_uleb buf n =
  assert (n >= 0);
  let rec loop n =
    let byte = n land 0x7f in
    let rest = n lsr 7 in
    if rest = 0
    then Buffer.add_char buf (Char.chr byte)
    else (
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop rest)
  in
  loop n

let emit_items recorded =
  let buf = Buffer.create 256 in
  let flush () =
    if Buffer.length buf > 0
    then (
      D.string (Buffer.contents buf);
      Buffer.clear buf)
  in
  add_uleb buf 1 (* version *);
  let ids = Hashtbl.create 16 in
  let next_id = ref 0 in
  let label_id label =
    let hashes =
      List.map Source_position_profile.hash_frame
        (Source_position_profile.frames_of_branch_label label)
    in
    match Hashtbl.find_opt ids hashes with
    | Some id -> id
    | None ->
      add_uleb buf 1;
      add_uleb buf (List.length hashes);
      List.iter (fun hash -> Buffer.add_int64_le buf hash) hashes;
      incr next_id;
      Hashtbl.add ids hashes !next_id;
      !next_id
  in
  List.iter
    (fun { jcc; fin; dbg } ->
      match Debuginfo.edge_labels dbg with
      | None | Some (Debuginfo.Positional _) -> ()
      | Some (Debuginfo.Resolved { taken; fallthrough }) ->
        (* Sets: a label reaching one machine edge through several successor
           positions must be counted once. *)
        let taken = List.sort_uniq compare (List.map label_id taken) in
        let fallthrough =
          List.sort_uniq compare (List.map label_id fallthrough)
        in
        add_uleb buf 0;
        flush ();
        D.label jcc;
        D.label fin;
        add_uleb buf (List.length taken);
        List.iter (add_uleb buf) taken;
        add_uleb buf (List.length fallthrough);
        List.iter (add_uleb buf) fallthrough)
    recorded;
  flush ()

let emit_section () =
  match List.rev !sites with
  | [] -> ()
  | recorded ->
    D.switch_to_section section;
    D.string "FDLB";
    emit_items recorded;
    reset ()
