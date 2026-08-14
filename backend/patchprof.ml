module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label

type site =
  { section : string;
    site : L.t;
    jcc : L.t;
    fin : L.t;
    retaddr_offset : int option;
    provenance : Debuginfo.t option
  }

let enabled () = Oxcaml_flags.patchprof_enabled ()

(* Sites of the current compilation unit, in reverse emission order. *)
let sites : site list ref = ref []

let record ~section ~site ~jcc ~fin ~retaddr_offset ~provenance =
  sites := { section; site; jcc; fin; retaddr_offset; provenance } :: !sites

(* The return-address offset is encoded in units of 8 bytes in an 11-bit
   field; the all-ones value means "unknown". *)
let unknown_retaddr_words = 0x7ff

let retaddr_words = function
  | None -> unknown_retaddr_words
  | Some offset ->
    assert (offset >= 0 && offset land 7 = 0);
    let words = offset asr 3 in
    if words >= unknown_retaddr_words then unknown_retaddr_words else words

let reset () = sites := []

(* Not allocated and not loaded: it costs no RSS and is read with [pread] on
   [/proc/self/exe]. *)
let section =
  Asm_targets.Asm_section.Custom
    { names = ["patchprof_sites"];
      flags = Some "";
      args = ["@progbits"];
      is_delayed = false
    }

(* Pseudo-instrumentation label metadata: for every instrumented site that
   carries labels, the label sets of its taken and fallthrough edges.  Not
   needed at runtime, only at profile decode time, hence a separate
   non-allocated section.

   Per compilation unit: the magic "PPLB", then uleb128-encoded items
   (interspersed with 8-byte absolute site addresses, see below), the first
   of which is the version.  Each item starts with a tag:
     1  label definition: number of frames, then per frame (leaf first) the
        8-byte little-endian frame hash - the first 8 bytes of the MD5 of
        "<file>:<line>:<col>" (line 1-based, col = character offset clamped
        to 0) - then the global discriminator and the edge discriminator.
        Definitions are numbered from 1 in order of appearance.
     0  site entry: followed by the 8-byte absolute address of the site's
        flag-writing instruction (as reported by the runtime's counter
        records), then the number of taken-edge labels and their ids, then
        the number of fallthrough-edge labels and their ids.
   Sites without labels have no entry. *)
let labels_section =
  Asm_targets.Asm_section.Custom
    { names = ["patchprof_labels"];
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

let frame_hash (item : Debuginfo.item) =
  let frame =
    Printf.sprintf "%s:%d:%d" item.dinfo_file item.dinfo_line
      (max 0 item.dinfo_char_start)
  in
  String.get_int64_le (Digest.string frame) 0

let emit_labels recorded =
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
  let label_id (label : Debuginfo.branch_label) =
    (* Leaf-first frame hashes. *)
    let hashes =
      List.rev_map frame_hash label.label_creator
    in
    let key = hashes, label.label_disc, label.label_edge in
    match Hashtbl.find_opt ids key with
    | Some id -> id
    | None ->
      add_uleb buf 1;
      add_uleb buf (List.length hashes);
      List.iter (fun hash -> Buffer.add_int64_le buf hash) hashes;
      add_uleb buf label.label_disc;
      add_uleb buf label.label_edge;
      incr next_id;
      Hashtbl.add ids key !next_id;
      !next_id
  in
  List.iter
    (fun { site; provenance; _ } ->
      match provenance with
      | None -> ()
      | Some dbg -> (
        match Debuginfo.edge_labels dbg with
        | None | Some (Debuginfo.Positional _) -> ()
        | Some (Debuginfo.Resolved { taken; fallthrough }) ->
          (* Sets: a label reaching one machine edge through several
             successor positions must be counted once. *)
          let taken = List.sort_uniq compare (List.map label_id taken) in
          let fallthrough =
            List.sort_uniq compare (List.map label_id fallthrough)
          in
          add_uleb buf 0;
          flush ();
          D.label site;
          add_uleb buf (List.length taken);
          List.iter (add_uleb buf) taken;
          add_uleb buf (List.length fallthrough);
          List.iter (add_uleb buf) fallthrough))
    recorded;
  flush ()

let emit_section () =
  let rec emit_blocks = function
    | [] -> ()
    | ({ section = section_name; _ } as first) :: sites ->
      let rec take_same_section rev_block = function
        | ({ section; _ } as site) :: sites
          when String.equal section section_name ->
          take_same_section (site :: rev_block) sites
        | sites -> List.rev rev_block, sites
      in
      let block, remaining = take_same_section [first] sites in
      (* The payload byte length lets the runtime hop from block header to
         block header without decoding the variable-length site records, so
         startup only decodes the blocks it selects sites from. *)
      let payload_start = L.create section in
      let payload_end = L.create section in
      D.string "PPMD";
      D.int32 4l;
      D.int32 (Int32.of_int (List.length block));
      D.between_labels_32_bit ~upper:payload_end ~lower:payload_start ();
      D.label first.site;
      D.define_label payload_start;
      let rec emit_sites previous = function
        | [] -> ()
        | { site; jcc; fin; retaddr_offset; section = _ } :: sites ->
          D.delta_uleb128 ~upper:site ~lower:previous;
          D.patchprof_lengths ~site ~jcc ~fin
            ~retaddr_words:(retaddr_words retaddr_offset);
          emit_sites site sites
      in
      emit_sites first.site block;
      D.define_label payload_end;
      emit_blocks remaining
  in
  match List.rev !sites with
  | [] -> ()
  | recorded ->
    D.switch_to_section section;
    emit_blocks recorded;
    D.switch_to_section labels_section;
    D.string "PPLB";
    emit_labels recorded;
    reset ()
