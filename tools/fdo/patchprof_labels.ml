(* Parsing of the "patchprof_labels" section; see backend/patchprof.ml for
   the emitter. The section concatenates one blob per compilation unit: the
   magic "PPLB" and a uleb128 version, then tagged items. Tag 1 defines a
   label (uleb frame count, that many 8-byte little-endian frame hashes, leaf
   first, then the global and edge discriminators as ulebs); definitions are
   numbered from 1 per blob. Tag 0 is a site entry: the site's 8-byte
   absolute address, then the taken edge's label ids (uleb count, then that
   many ulebs) and likewise the fallthrough edge's. *)

type label =
  { frame_hashes : int64 list;
    disc : int;
    edge : int
  }

type site_labels =
  { taken : label list;
    fallthrough : label list
  }

let parse data =
  let sites = Hashtbl.create 256 in
  let length = String.length data in
  let pos = ref 0 in
  let fail fmt =
    Printf.ksprintf
      (fun msg -> failwith ("patchprof_labels section: " ^ msg))
      fmt
  in
  let byte () =
    if !pos >= length then fail "truncated section";
    let b = Char.code data.[!pos] in
    incr pos;
    b
  in
  let uleb () =
    let rec loop shift acc =
      let b = byte () in
      let acc = acc lor ((b land 0x7f) lsl shift) in
      if b land 0x80 = 0 then acc else loop (shift + 7) acc
    in
    loop 0 0
  in
  let u64 () =
    if !pos + 8 > length then fail "truncated section";
    let v = String.get_int64_le data !pos in
    pos := !pos + 8;
    v
  in
  let at_magic () =
    length - !pos >= 4 && String.equal (String.sub data !pos 4) "PPLB"
  in
  while !pos < length do
    if not (at_magic ()) then fail "bad magic";
    pos := !pos + 4;
    let version = uleb () in
    if version <> 1 then fail "unsupported version %d" version;
    (* The definitions of the current compilation unit, numbered from 1. *)
    let labels = Hashtbl.create 16 in
    let num_labels = ref 0 in
    let continue = ref true in
    while !continue && !pos < length do
      if at_magic ()
      then continue := false
      else
        match uleb () with
        | 1 ->
          let num_frames = uleb () in
          let frame_hashes = List.init num_frames (fun _ -> u64 ()) in
          let disc = uleb () in
          let edge = uleb () in
          incr num_labels;
          Hashtbl.add labels !num_labels { frame_hashes; disc; edge }
        | 0 ->
          let address = u64 () in
          let set () =
            let n = uleb () in
            List.init n (fun _ ->
                let id = uleb () in
                match Hashtbl.find_opt labels id with
                | Some label -> label
                | None -> fail "undefined label id %d" id)
          in
          let taken = set () in
          let fallthrough = set () in
          Hashtbl.replace sites address { taken; fallthrough }
        | tag -> fail "bad item tag %d" tag
    done
  done;
  sites
