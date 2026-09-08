(* The little we need to know about the profiled executable's ELF file: its type
   (to warn about position-independent executables, whose runtime addresses do
   not match their link-time addresses) and the contents of its FDO metadata
   section. *)

module Owee_buf = Compiler_owee.Owee_buf
module Owee_elf = Compiler_owee.Owee_elf

type t =
  { e_type : int;
    buffer : Owee_buf.t;
    sections : Owee_elf.section array
  }

let et_dyn = 3

let is_pie t = t.e_type = et_dyn

(* Eta-expansion gives [create_process] and [waitpid] the unannotated types
   required by [Compiler_owee.Unix_intf.S]. *)
module Unix_for_owee = struct
  include Unix

  let create_process prog args stdin stdout stderr =
    Unix.create_process prog args stdin stdout stderr

  let waitpid flags pid = Unix.waitpid flags pid
end

let read filename =
  let buffer =
    Owee_buf.map_binary
      (module Unix_for_owee : Compiler_owee.Unix_intf.S)
      filename
  in
  let header, sections = Owee_elf.read_elf buffer in
  { e_type = header.e_type; buffer; sections }

let section_body t name =
  Option.map
    (Owee_elf.section_body_string t.buffer)
    (Owee_elf.find_section t.sections name)
