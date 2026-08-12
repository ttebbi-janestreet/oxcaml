module D = Asm_targets.Asm_directives
module L = Asm_targets.Asm_label

type site =
  { section : string;
    site : L.t;
    jcc : L.t;
    fin : L.t;
    retaddr_offset : int option
  }

(* The metadata records one address delta per site within a text section, so
   sections that the linker may reorder are unsupported.  The default-enabled
   flag yields to them silently; an explicit [-patchprof] must error. *)
let enabled () =
  !Oxcaml_flags.patchprof
  &&
  let incompatible_sections =
    !Clflags.function_sections
    || !Oxcaml_flags.basic_block_sections
    || !Oxcaml_flags.module_entry_functions_section
  in
  if incompatible_sections && !Oxcaml_flags.patchprof_explicit
  then Misc.fatal_error "-patchprof does not yet support function sections";
  not incompatible_sections

(* Sites of the current compilation unit, in reverse emission order. *)
let sites : site list ref = ref []

let record ~section ~site ~jcc ~fin ~retaddr_offset =
  sites := { section; site; jcc; fin; retaddr_offset } :: !sites

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
    reset ()
