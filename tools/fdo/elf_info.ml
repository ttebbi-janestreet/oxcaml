(* The little we need to know about the profiled executable's ELF file: its type
   (to warn about position-independent executables, whose runtime addresses do
   not match their link-time addresses) and its GNU build id (recorded in the
   profile as provenance). *)

module Owee_buf = Compiler_owee.Owee_buf
module Owee_elf = Compiler_owee.Owee_elf

type t =
  { e_type : int;
    buildid : string option
  }

let et_dyn = 3

let is_pie t = t.e_type = et_dyn

let buildid t = t.buildid

let hex_string s =
  String.to_seq s
  |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
  |> List.of_seq |> String.concat ""

(* A .note.gnu.build-id section contains a single note: namesz, descsz and type
   words, then the name ("GNU\000", padded to a word boundary), then descsz
   bytes of build id. *)
let read_buildid buffer sections =
  match Owee_elf.find_section sections ".note.gnu.build-id" with
  | None -> None
  | Some section ->
    let body = Owee_elf.section_body buffer section in
    let cursor = Owee_buf.cursor body in
    let namesz = Owee_buf.Read.u32 cursor in
    let descsz = Owee_buf.Read.u32 cursor in
    let typ = Owee_buf.Read.u32 cursor in
    let name = Owee_buf.Read.fixed_string cursor ((namesz + 3) land lnot 3) in
    if typ = 3 (* NT_GNU_BUILD_ID *) && String.starts_with ~prefix:"GNU" name
    then Some (hex_string (Owee_buf.Read.fixed_string cursor descsz))
    else None

let read filename =
  let buffer =
    Owee_buf.map_binary (module Unix : Compiler_owee.Unix_intf.S) filename
  in
  let header, sections = Owee_elf.read_elf buffer in
  { e_type = header.e_type; buildid = read_buildid buffer sections }
