(* Extraction of raw samples from a patchprof profile.

   The profile is a stream of 64-bit little-endian words: a magic word, then
   records of the form [kind, payload_words, payload...]; see
   runtime/caml/patchprof.h for the record kinds. Only walk records (the
   sampled leaf-first stacks) and counter records (the exact per-site
   execution counts) matter here; selection and statistics records do not
   affect the histogram. A record truncated by the producer dying mid-write
   ends the stream; anything else malformed aborts loudly (a silently skewed
   profile is worse than no profile).

   Sampling weighs each site's walks by its adaptive sampling period, so the
   walk weights of one site sum to roughly, but not exactly, its execution
   count. The counter records carry the exact count, so each site's walks are
   rescaled to sum to it, and sites that executed too rarely to be sampled at
   all still contribute their exact count as a single-frame stack. *)

let profile_magic = 0x000a31464f525050L (* "PPROF1\n" *)

type site_counters =
  { executions : int64;
    sampled_weight : int64;
    tally : int64
  }

type t =
  { samples : Perf_script.t;
    sites : (int64, site_counters) Hashtbl.t
  }

type raw =
  { (* leaf-first walk stack -> total sampled weight *)
    stacks : (int64 list, int64) Hashtbl.t;
    (* site address -> total sampled weight of its walks *)
    site_weights : (int64, int64) Hashtbl.t;
    (* site address -> counters, aggregated over domains and rotations *)
    counters : (int64, site_counters) Hashtbl.t
  }

let add table key amount =
  let existing = Option.value (Hashtbl.find_opt table key) ~default:0L in
  Hashtbl.replace table key (Int64.add existing amount)

let malformed fmt = Printf.ksprintf failwith fmt

let non_negative what value =
  if Int64.compare value 0L < 0 then malformed "negative %s %Ld" what value;
  value

let parse contents =
  let num_words = String.length contents / 8 in
  let word i = String.get_int64_le contents (8 * i) in
  if num_words < 1 || not (Int64.equal (word 0) profile_magic)
  then malformed "not a patchprof profile (bad magic number)";
  let raw =
    { stacks = Hashtbl.create 4096;
      site_weights = Hashtbl.create 1024;
      counters = Hashtbl.create 1024
    }
  in
  let parse_walk_chunk ~payload ~chunk_end =
    (* One word of domain id, then per walk: the sampled weight, the frame
       count (low half) and site index (high half), then one word per frame
       address, the instrumented site first. *)
    let cursor = ref (payload + 1) in
    while !cursor < chunk_end do
      if !cursor + 2 > chunk_end then malformed "truncated walk record";
      let weight = non_negative "walk weight" (word !cursor) in
      let depth =
        Int64.to_int (Int64.logand (word (!cursor + 1)) 0xFFFF_FFFFL)
      in
      if depth < 1 || !cursor + 2 + depth > chunk_end
      then malformed "malformed walk record";
      let frames = List.init depth (fun i -> word (!cursor + 2 + i)) in
      add raw.stacks frames weight;
      add raw.site_weights (word (!cursor + 2)) weight;
      cursor := !cursor + 2 + depth
    done
  in
  let parse_counters ~payload ~length =
    (* One word each of domain id, site count and active time, then per
       site: address, executions, slow-path entries, sampled weight,
       tally. *)
    if length < 3 then malformed "malformed counter record";
    let num_sites = Int64.to_int (word (payload + 1)) in
    if num_sites < 0 || length <> 3 + (5 * num_sites)
    then malformed "malformed counter record";
    for i = 0 to num_sites - 1 do
      let base = payload + 3 + (5 * i) in
      let address = word base in
      let executions = non_negative "execution count" (word (base + 1)) in
      let sampled_weight = non_negative "sampled weight" (word (base + 3)) in
      let tally = non_negative "tally" (word (base + 4)) in
      if Int64.compare tally sampled_weight > 0
      then malformed "tally exceeds the sampled weight";
      let counters =
        match Hashtbl.find_opt raw.counters address with
        | None -> { executions; sampled_weight; tally }
        | Some acc ->
          { executions = Int64.add acc.executions executions;
            sampled_weight = Int64.add acc.sampled_weight sampled_weight;
            tally = Int64.add acc.tally tally
          }
      in
      Hashtbl.replace raw.counters address counters
    done
  in
  let position = ref 1 in
  let truncated = ref false in
  while (not !truncated) && !position + 2 <= num_words do
    let kind = word !position in
    let length = Int64.to_int (word (!position + 1)) in
    let payload = !position + 2 in
    if length < 0 || payload + length > num_words
    then truncated := true
    else (
      (match kind with
      | 2L -> parse_walk_chunk ~payload ~chunk_end:(payload + length)
      | 3L -> parse_counters ~payload ~length
      | _ -> ());
      position := payload + length)
  done;
  raw

let to_samples raw =
  let samples : Perf_script.t =
    { counts = Hashtbl.create (Hashtbl.length raw.stacks); total = 0L }
  in
  let record stack count =
    let existing =
      Option.value (Hashtbl.find_opt samples.counts stack) ~default:0L
    in
    Hashtbl.replace samples.counts stack (Int64.add existing count);
    samples.total <- Int64.add samples.total count
  in
  Hashtbl.iter
    (fun stack weight ->
      match stack with
      | [] -> ()
      | site :: _ ->
        let count =
          match Hashtbl.find_opt raw.counters site with
          | None ->
            (* The counter record was lost (the producer died before its
               final dump); the sampled weight is the best estimate left. *)
            weight
          | Some { executions; _ } ->
            (* Rescale so the site's walks sum to its exact count. *)
            let site_weight =
              Option.value
                (Hashtbl.find_opt raw.site_weights site)
                ~default:weight
            in
            Int64.of_float
              (Float.round
                 (Int64.to_float weight *. Int64.to_float executions
                 /. Int64.to_float site_weight))
        in
        record stack count)
    raw.stacks;
  Hashtbl.iter
    (fun site { executions; _ } ->
      if not (Hashtbl.mem raw.site_weights site)
      then record [site] executions)
    raw.counters;
  { samples; sites = raw.counters }

let of_string contents = to_samples (parse contents)

let read ~filename =
  let contents =
    try In_channel.with_open_bin filename In_channel.input_all
    with Sys_error msg -> failwith msg
  in
  match of_string contents with
  | samples -> samples
  | exception Failure msg -> failwith (filename ^ ": " ^ msg)
