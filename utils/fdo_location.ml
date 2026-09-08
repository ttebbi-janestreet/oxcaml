type level = string list

type t = level list

type hashed = int64 list

(* Hashing is on every instruction's location when emitting FDO metadata or
   consuming a profile, and the same few thousand levels recur throughout a
   compilation unit, hence the memo table. *)
let level_hashes : (level, int64) Hashtbl.t = Hashtbl.create 1024

let hash_level level =
  match Hashtbl.find_opt level_hashes level with
  | Some hash -> hash
  | None ->
    let hash =
      String.get_int64_le (Digest.string (String.concat "\000" level)) 0
    in
    Hashtbl.add level_hashes level hash;
    hash

let hash t = List.map hash_level t

let level_string level = String.concat ":" level

let unit_anchor name = ["<" ^ name ^ ">"]

let nested_anchor ~anchor ~index = anchor @ [string_of_int index]
