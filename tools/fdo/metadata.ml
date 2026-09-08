(* Parsing of the "fdo_metadata" section; see backend/fdo_metadata.ml for the
   emitter and the format. *)

type location = int64 list

type t =
  { taken : (int64, location list) Hashtbl.t;
    fallthrough : (int64 * location list) array;
    target : (int64, location list) Hashtbl.t;
    callsite : (int64, location list) Hashtbl.t;
    names : (int64, Fdo_location.level) Hashtbl.t
  }

let empty =
  { taken = Hashtbl.create 0;
    fallthrough = [||];
    target = Hashtbl.create 0;
    callsite = Hashtbl.create 0;
    names = Hashtbl.create 0
  }

let parse data =
  let taken = Hashtbl.create 256
  and fallthrough = Hashtbl.create 256
  and target = Hashtbl.create 256
  and callsite = Hashtbl.create 256
  and names = Hashtbl.create 256 in
  let pos = ref 0 in
  let fail fmt =
    Printf.ksprintf (fun msg -> failwith ("fdo_metadata section: " ^ msg)) fmt
  in
  let need n = if !pos + n > String.length data then fail "truncated section" in
  let rec uleb shift acc =
    need 1;
    let b = Char.code data.[!pos] in
    incr pos;
    let acc = acc lor ((b land 0x7f) lsl shift) in
    if b land 0x80 = 0 then acc else uleb (shift + 7) acc
  in
  let u64 () =
    need 8;
    let v = String.get_int64_le data !pos in
    pos := !pos + 8;
    v
  in
  let at_magic () =
    need 4;
    String.equal (String.sub data !pos 4) "FDOM"
  in
  let add table address locations =
    let existing = Option.value (Hashtbl.find_opt table address) ~default:[] in
    Hashtbl.replace table address (locations @ existing)
  in
  while !pos < String.length data do
    if not (at_magic ()) then fail "bad magic";
    pos := !pos + 4;
    (match uleb 0 0 with 7 -> () | v -> fail "unsupported version %d" v);
    while !pos < String.length data && not (at_magic ()) do
      match uleb 0 0 with
      | (0 | 1 | 2 | 3) as tag ->
        let address = u64 () in
        let locations =
          List.init (uleb 0 0) (fun _ -> List.init (uleb 0 0) (fun _ -> u64 ()))
        in
        add
          (match tag with
          | 0 -> taken
          | 1 -> fallthrough
          | 2 -> target
          | _ -> callsite)
          address locations
      | 4 ->
        let hash = u64 () in
        let level =
          List.init (uleb 0 0) (fun _ ->
              let len = uleb 0 0 in
              need len;
              let component = String.sub data !pos len in
              pos := !pos + len;
              component)
        in
        Hashtbl.replace names hash level
      | tag -> fail "bad item tag %d" tag
    done
  done;
  let sorted table =
    let array = Array.of_seq (Hashtbl.to_seq table) in
    Array.sort (fun (a, _) (b, _) -> Int64.unsigned_compare a b) array;
    array
  in
  { taken; fallthrough = sorted fallthrough; target; callsite; names }

(* The number of entries whose address is < [address]. *)
let lower_bound array address =
  let lo = ref 0 and hi = ref (Array.length array) in
  while !lo < !hi do
    let mid = (!lo + !hi) / 2 in
    if Int64.unsigned_compare (fst array.(mid)) address < 0
    then lo := mid + 1
    else hi := mid
  done;
  !lo

let iter_range t ~lo ~hi f =
  for i = lower_bound t.fallthrough lo to lower_bound t.fallthrough hi - 1 do
    f (snd t.fallthrough.(i))
  done

let find table address =
  Option.value (Hashtbl.find_opt table address) ~default:[]

let branch_locations t ~source ~target =
  let entered =
    match find t.target target, find t.callsite source with
    | [], _ -> []
    | entries, [] -> entries
    | entries, callsites ->
      List.concat_map
        (fun entry -> List.map (fun callsite -> entry @ callsite) callsites)
        entries
  in
  find t.taken source @ entered

let call_targets t ~source ~target =
  match find t.callsite source with
  | [] -> []
  | callsites ->
    let entries =
      List.filter_map
        (fun (location : location) ->
          match location with [entry] -> Some entry | [] | _ :: _ :: _ -> None)
        (find t.target target)
    in
    List.concat_map
      (fun (callsite : location) ->
        match callsite with
        | [] -> []
        | level :: _ -> List.map (fun entry -> level, entry) entries)
      callsites
