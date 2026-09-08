[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Doubly_linked_list

(* Only blocks with labelled edges around them are measured; the others (whose
   edges are unconditional, or compiler-generated checks) come out as 0 even in
   the middle of a hot path. Repair the counts using flow conservation: all
   executions of a block continue into its successors and arrived through its
   predecessors, so each of those sets must account for at least the block's own
   count. A set that falls short has the deficit added to its unmeasured
   members; measured counts are left alone (a measured zero next to a hot
   sibling is meaningful: the flow demonstrably goes to the sibling).

   Exact knowledge propagates first. A single unmeasured member carries the
   whole deficit; several unmeasured members that already received counts share
   it proportionally.

   Unmeasured members that are ALL still 0 once no exact propagation applies
   anywhere are different: the flow has to go somewhere, and sinking a block
   that might carry it costs a jump on a hot path, so as a last resort the
   deficit is split evenly across them. Deferring the split until the exact
   rules have converged matters: a member that merely has not received its
   counts yet would otherwise leak flow into its siblings, and since counts
   never decrease, such a mistake would stick.

   A set all of whose members are measured can only fall short when the
   measurements are inconsistent (sampling); then its members are scaled up
   proportionally.

   Counts only increase, up to the hottest measured block, so the iteration
   reaches a fixed point. *)
let repair_frequencies (cfg : Cfg.t) counts ~measured =
  let count label = Label.Tbl.find counts label in
  let changed = ref true in
  let set label c =
    Label.Tbl.replace counts label c;
    changed := true
  in
  let scale labels ~sum ~target =
    let factor = Int64.to_float target /. Int64.to_float sum in
    Label.Set.iter
      (fun l ->
        let c = count l in
        let scaled = Int64.of_float (Int64.to_float c *. factor) in
        if Int64.compare scaled c > 0 then set l scaled)
      labels
  in
  let raise_group ~even_split labels ~target =
    let sum_of labels =
      Label.Set.fold (fun l acc -> Int64.add acc (count l)) labels 0L
    in
    let sum = sum_of labels in
    if (not (Label.Set.is_empty labels)) && Int64.compare sum target < 0
    then
      let deficit = Int64.sub target sum in
      let unmeasured = Label.Set.diff labels measured in
      match Label.Set.elements unmeasured with
      | [] -> if Int64.compare sum 0L > 0 then scale labels ~sum ~target
      | [single] -> set single (Int64.add (count single) deficit)
      | _ :: _ :: _ ->
        let unmeasured_sum = sum_of unmeasured in
        if Int64.compare unmeasured_sum 0L > 0
        then
          scale unmeasured ~sum:unmeasured_sum
            ~target:(Int64.add unmeasured_sum deficit)
        else if even_split
        then
          let cardinal = Int64.of_int (Label.Set.cardinal unmeasured) in
          let share = Int64.div deficit cardinal in
          let first_share = Int64.add share (Int64.rem deficit cardinal) in
          let first = ref true in
          Label.Set.iter
            (fun l ->
              let share = if !first then first_share else share in
              first := false;
              if Int64.compare share 0L > 0 then set l share)
            unmeasured
  in
  let sweep ~even_split =
    changed := false;
    Cfg.iter_blocks cfg ~f:(fun label block ->
        let target = count label in
        if Int64.compare target 0L > 0
        then (
          raise_group ~even_split
            (Cfg.successor_labels ~normal:true ~exn:true block)
            ~target;
          raise_group ~even_split block.predecessors ~target))
  in
  let rec converge ~even_split =
    sweep ~even_split;
    if !changed
    then converge ~even_split:false
    else if not even_split
    then converge ~even_split:true
  in
  converge ~even_split:false

let print_location ppf location =
  Format.pp_print_string ppf
    (String.concat " <- " (List.map Fdo_location.level_string location))

(* A label in brackets; a rider (see [Debuginfo.branch_label]) marked with a
   plus. *)
let print_label ppf (label : Debuginfo.branch_label) =
  Format.fprintf ppf "%s[%a]"
    (if label.rider then "+" else "")
    print_location label.location

(* The count the profile recorded for the labels: the sum over the labels
   identifying the edge (a label reaching one machine edge through several
   successor positions is counted once, as the metadata emitter does), riders
   excluded. *)
let count_of_labels profile (labels : Debuginfo.branch_label list) =
  List.filter_map
    (fun (label : Debuginfo.branch_label) ->
      if label.rider then None else Some label.location)
    labels
  |> List.sort_uniq Stdlib.compare
  |> List.fold_left
       (fun acc location ->
         Int64.add acc
           (Source_position_profile.count_for_location profile location))
       0L

(* The labels of every instrumented edge, by source block then destination, so
   that a profile for the function can be written by hand (see
   oxcaml/tests/fdo). *)
let edge_labels (cfg : Cfg.t) =
  let table : (Label.t * Debuginfo.branch_label list) list Label.Tbl.t =
    Label.Tbl.create 16
  in
  Cfg.iter_blocks cfg ~f:(fun src block ->
      match Debuginfo.edge_labels block.terminator.dbg with
      | Some (Debuginfo.Positional sets) ->
        let positions = Cfg.edge_label_positions block.terminator.desc in
        let edges = ref [] in
        Array.iteri
          (fun i labels ->
            if i < Array.length positions
            then
              let dst = positions.(i) in
              let existing =
                Option.value (List.assoc_opt dst !edges) ~default:[]
              in
              let by_kind_then_location (a : Debuginfo.branch_label)
                  (b : Debuginfo.branch_label) =
                Stdlib.compare (a.rider, a.location) (b.rider, b.location)
              in
              edges
                := ( dst,
                     List.sort_uniq by_kind_then_location (labels @ existing) )
                   :: List.remove_assoc dst !edges)
          sets;
        Label.Tbl.replace table src (List.rev !edges)
      | Some (Debuginfo.Resolved _ | Debuginfo.Callsite _) | None -> ());
  table

let labels_of edge_labels src dst =
  match Label.Tbl.find_opt edge_labels src with
  | Some edges -> Option.value (List.assoc_opt dst edges) ~default:[]
  | None -> []

(* An edge, its labels in brackets, and its weight when known. *)
let print_edge ppf ~edge_labels src dst weight =
  Format.fprintf ppf "  edge %a -> %a" Label.format src Label.format dst;
  List.iter
    (fun label -> Format.fprintf ppf " %a" print_label label)
    (labels_of edge_labels src dst);
  Option.iter (fun weight -> Format.fprintf ppf ": %Ld" weight) weight;
  Format.fprintf ppf "@."

let dump_entry ppf (cfg : Cfg.t) =
  List.iter
    (fun label -> Format.fprintf ppf "  entry: %a@." print_label label)
    (Debuginfo.entry_labels cfg.fun_dbg)

let dump_frequencies ppf cfg_with_layout ~measured ~repaired =
  DLL.iter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
      let m = Label.Tbl.find measured label in
      let r = Label.Tbl.find repaired label in
      Format.fprintf ppf "  block %a: measured %Ld, repaired %Ld@." Label.format
        label m r)

(* The weights of the normal successor edges of a block whose terminator carries
   pseudo-instrumentation labels the profile recorded: an edge's weight is the
   sum of its labels' counts. [None] for a terminator none of whose labels was
   recorded (not instrumented, or a profile without labels). *)
let measured_edge_weights profile (block : Cfg.basic_block) =
  let terminator = block.terminator in
  let positions = Cfg.edge_label_positions terminator.desc in
  match Debuginfo.edge_labels terminator.dbg with
  | Some (Debuginfo.Positional sets) ->
    Cfg.check_edge_labels ~context:"Cfg_fdo_layout" terminator sets;
    (* Group the positions by successor, in first-occurrence order. *)
    let edges : (Label.t * Debuginfo.branch_label list ref) list ref = ref [] in
    Array.iteri
      (fun i dst ->
        match List.find_opt (fun (d, _) -> Label.equal d dst) !edges with
        | Some (_, labels) -> labels := sets.(i) @ !labels
        | None -> edges := (dst, ref sets.(i)) :: !edges)
      positions;
    let weights =
      List.rev_map
        (fun (dst, labels) -> dst, count_of_labels profile !labels)
        !edges
    in
    (* An edge the profile never recorded counts as 0, so a terminator none of
       whose edges was seen is indistinguishable from an unmeasured one. *)
    if List.exists (fun (_, weight) -> Int64.compare weight 0L > 0) weights
    then Some weights
    else None
  | None | Some (Debuginfo.Resolved _ | Debuginfo.Callsite _) -> None

let sum_weights weights =
  List.fold_left (fun acc (_, weight) -> Int64.add acc weight) 0L weights

(* The measured execution count of every block, from the profile's edge counts:
   a block whose successor edges were measured executed as often as they were
   taken together (every execution leaves through one of them); a block all of
   whose predecessors' successor edges were measured, as often as those into it
   were taken; the entry block, as often as its entry edge was taken
   ([Debuginfo.entry_labels]). Blocks about which the profile knows nothing come
   out as 0 and rely on the frequency repair; the set of the others (whose count
   may be a measured 0) is returned along with the measured edge weights per
   block. *)
let measured_counts profile (cfg : Cfg.t) =
  let edges = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      Label.Tbl.replace edges label (measured_edge_weights profile block));
  let counts = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let measured = ref Label.Set.empty in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let from_successors =
        match Label.Tbl.find edges label with
        | Some weights -> Some (sum_weights weights)
        | None -> None
      in
      let from_predecessors =
        let predecessors = Label.Set.elements block.predecessors in
        let weights =
          List.map (fun pred -> Label.Tbl.find edges pred) predecessors
        in
        if List.is_empty weights || List.exists Option.is_none weights
        then None
        else
          Some
            (List.fold_left
               (fun acc weights ->
                 List.fold_left
                   (fun acc (dst, weight) ->
                     if Label.equal dst label then Int64.add acc weight else acc)
                   acc (Option.get weights))
               0L weights)
      in
      let from_entry =
        if Label.equal label cfg.entry_label
        then
          let count =
            count_of_labels profile (Debuginfo.entry_labels cfg.fun_dbg)
          in
          (* An unrecorded entry is indistinguishable from an unmeasured one. *)
          if Int64.compare count 0L > 0 then Some count else None
        else None
      in
      let count =
        List.fold_left
          (fun acc measurement ->
            match acc, measurement with
            | None, m | m, None -> m
            | Some a, Some b -> Some (Int64.max a b))
          None
          [from_successors; from_predecessors; from_entry]
      in
      Option.iter (fun _ -> measured := Label.Set.add label !measured) count;
      Label.Tbl.replace counts label (Option.value count ~default:0L));
  counts, edges, !measured

(* The weights of a block's normal successor edges, in profile-count units: the
   measured weights when the terminator has them, otherwise the flow bound [min
   (count src) (count dst)] per successor (single-successor control flow
   carrying the whole block count). *)
let successor_edge_weights edges counts src (block : Cfg.basic_block) =
  let count label = Label.Tbl.find counts label in
  match Label.Tbl.find edges src with
  | Some weights -> weights
  | None -> (
    let src_count = count src in
    match
      Label.Set.elements (Cfg.successor_labels ~normal:true ~exn:false block)
    with
    | [dst] -> [dst, src_count]
    | successors ->
      List.map (fun dst -> dst, Int64.min src_count (count dst)) successors)

(* Greedy layout aimed at what CPUs do well: a conditional branch whose hot side
   falls through, and loops closed by a conditional branch backwards, which is
   what static branch prediction assumes taken (forward conditional branches
   being assumed not taken) and saves the unconditional jump of a loop closed by
   a fallthrough into a jump back.

   Starting from the entry block, the layout eagerly appends the last placed
   block's not-yet-placed successor of heaviest edge; once none is left, it
   restarts from the remaining block with the heaviest edge from the blocks
   placed so far (ties, and the blocks nothing placed flows into, in the
   original order). Each time a block [b] is placed after [a], its loops are
   rotated onto it: the heaviest edge into [b] from a not-yet-placed block [p]
   is followed backwards, as long as it is heavier than the edge from [a] and
   [p] has a single successor, by inserting [p] between [a] and [b], so that the
   jump [p] would need becomes a fallthrough; this repeats on [p], up to a block
   with a conditional branch (typically the loop's exit test, whose branch back
   into the loop becomes a backwards conditional branch). *)
let build_layout ~dump measured_edges counts cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let num_blocks = Label.Tbl.length cfg.blocks in
  (* Original positions, for deterministic tie-breaking. *)
  let position = Label.Tbl.create num_blocks in
  DLL.iter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
      Label.Tbl.replace position label (Label.Tbl.length position));
  let position label = Label.Tbl.find position label in
  let weights = Label.Tbl.create num_blocks in
  Cfg.iter_blocks cfg ~f:(fun src block ->
      Label.Tbl.replace weights src
        (successor_edge_weights measured_edges counts src block));
  let weight src dst =
    Option.value (List.assoc_opt dst (Label.Tbl.find weights src)) ~default:0L
  in
  Option.iter
    (fun ppf ->
      let edge_labels = edge_labels cfg in
      Label.Tbl.fold
        (fun src edges acc ->
          List.fold_left
            (fun acc (dst, weight) ->
              if Label.equal src dst then acc else (src, dst, weight) :: acc)
            acc edges)
        weights []
      |> List.sort (fun (src1, dst1, weight1) (src2, dst2, weight2) ->
          let c = Int64.compare weight2 weight1 in
          if c <> 0
          then c
          else
            let c = Int.compare (position src1) (position src2) in
            if c <> 0 then c else Int.compare (position dst1) (position dst2))
      |> List.iter (fun (src, dst, weight) ->
          print_edge ppf ~edge_labels src dst (Some weight)))
    dump;
  match !Oxcaml_flags.fdo_layout with
  | Ext_tsp ->
    (* Block sizes are estimated at 4 bytes per instruction. *)
    let blocks =
      DLL.to_list (Cfg_with_layout.layout cfg_with_layout)
      |> List.map (fun label ->
          let block = Cfg.get_block_exn cfg label in
          label, 4 * (DLL.length block.body + 1), Label.Tbl.find counts label)
      |> Array.of_list
    in
    let edges =
      Label.Tbl.fold
        (fun src edges acc ->
          List.fold_left
            (fun acc (dst, weight) -> (src, dst, weight) :: acc)
            acc edges)
        weights []
      |> List.sort (fun (src1, dst1, _) (src2, dst2, _) ->
          let c = Int.compare (position src1) (position src2) in
          if c <> 0 then c else Int.compare (position dst1) (position dst2))
    in
    Cfg_fdo_ext_tsp.layout ~blocks ~edges
  | Greedy ->
    let placed = Label.Tbl.create num_blocks in
    (* The layout so far, last placed block first. *)
    let layout = ref [] in
    let place label = Label.Tbl.replace placed label () in
    (* Among [candidates], the one of greatest [weight], ties broken by original
       position; [None] if there is none. *)
    let heaviest candidates ~weight =
      List.fold_left
        (fun best label ->
          match best with
          | Some (best_label, best_weight)
            when Int64.compare best_weight (weight label) > 0
                 || Int64.equal best_weight (weight label)
                    && position best_label < position label ->
            best
          | Some _ | None -> Some (label, weight label))
        None candidates
    in
    let unplaced label = not (Label.Tbl.mem placed label) in
    let single_successor label =
      match
        Label.Set.elements
          (Cfg.successor_labels ~normal:true ~exn:false
             (Cfg.get_block_exn cfg label))
      with
      | [dst] -> Some dst
      | [] | _ :: _ :: _ -> None
    in
    (* [rotate b before], where [before] is the layout preceding [b] (last block
       first), inserts the predecessors of [b] that should precede it. *)
    let rec rotate b before =
      match before with
      | [] -> []
      | a :: _ -> (
        let block = Cfg.get_block_exn cfg b in
        let candidates =
          List.filter
            (fun p -> unplaced p && not (Label.equal p b))
            (Label.Set.elements block.predecessors)
        in
        match heaviest candidates ~weight:(fun p -> weight p b) with
        | Some (p, w)
          when Int64.compare w (weight a b) > 0
               && Option.is_some (single_successor p) ->
          place p;
          p :: rotate p before
        | Some _ | None -> before)
    in
    let append label =
      place label;
      layout := label :: rotate label !layout
    in
    let rec extend () =
      let current = List.hd !layout in
      let successors =
        List.filter unplaced
          (Label.Set.elements
             (Cfg.successor_labels ~normal:true ~exn:false
                (Cfg.get_block_exn cfg current)))
      in
      match heaviest successors ~weight:(weight current) with
      | Some (next, _) ->
        append next;
        extend ()
      | None -> ()
    in
    append cfg.entry_label;
    extend ();
    let remaining () =
      Cfg.fold_blocks cfg ~init:[] ~f:(fun label _ acc ->
          if unplaced label then label :: acc else acc)
    in
    (* The heaviest edge into [label] from the blocks placed so far. *)
    let weight_from_placed label =
      Label.Set.fold
        (fun pred acc ->
          if unplaced pred then acc else Int64.max acc (weight pred label))
        (Cfg.get_block_exn cfg label).predecessors 0L
    in
    let rec restart () =
      match heaviest (remaining ()) ~weight:weight_from_placed with
      | Some (label, _) ->
        append label;
        extend ();
        restart ()
      | None -> ()
    in
    restart ();
    List.rev !layout

(* The call graph edges of the function for the linker (see [Fdo_call_graph]):
   every real call instruction in a block with a positive count, weighted by
   that count. The callees are the functions the profile saw the call site reach
   (its call-target index, each weighted by the count the trie has for the call
   site in as much of its inlining context as recorded, the block count split in
   proportion), so that calls through the runtime's stubs (caml_applyN and
   friends) and indirect calls resolve to the actual functions; a call the
   profile knows nothing about keeps its static callee if it has one. Self tail
   calls and external calls are not calls of the call graph.

   CR-someday ttebbi: the block counts of a function aggregate over its inlined
   copies (a location's root sums all inlining contexts), so a function that is
   only ever inlined, or mostly so, still has hot blocks here and produces hot
   edges, e.g. from a functor's [@@inline always] function to the stubs of its
   calls through functor arguments. Whether the standalone copy is really hot
   depends on the inlining decisions of other compilation units, which are not
   known here; only a global computation of the weights could tell. *)
let record_call_edges ~dump profile (cfg : Cfg.t) counts =
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let weight = Label.Tbl.find counts label in
      let callee : Cfg.func_call_operation option =
        match block.terminator.desc with
        | Call { op; label_after = _ } | Tailcall_func op -> Some op
        | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
        | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
        | Call_no_return _ | Prim _ | Invalid _ ->
          None
      in
      match callee with
      | None -> ()
      | Some _ when Int64.compare weight 0L <= 0 -> ()
      | Some op -> (
        let from_profile =
          match Debuginfo.callsite_label block.terminator.dbg with
          | None | Some [] -> []
          | Some (level :: _ as callsite) ->
            let context = Fdo_location.hash callsite in
            List.filter_map
              (fun root ->
                let count =
                  Source_position_profile.count_for_deepest_context profile
                    ~root ~context
                in
                if Int64.compare count 0L > 0 then Some (root, count) else None)
              (Source_position_profile.call_targets profile level)
        in
                let add (callee : Fdo_call_graph.callee) weight =
          Option.iter
            (fun ppf ->
              Format.fprintf ppf "  call from block %a%a to %s: %Ld@."
                Label.format label
                (fun ppf callsite ->
                  Option.iter
                    (fun callsite ->
                      Format.fprintf ppf " [%a]" print_location callsite)
                    callsite)
                (Debuginfo.callsite_label block.terminator.dbg)
                (Asm_targets.Asm_symbol.encode
                   (match callee with
                   | Symbol name -> Asm_targets.Asm_symbol.create_global name
                   | Entry hashes -> Fdo_call_graph.alias_symbol hashes))
                weight)
            dump;
          Fdo_call_graph.add_edge ~from:cfg.fun_name ~callee ~weight
        in
        match from_profile, op with
        | [], Direct func -> add (Symbol func.sym_name) weight
        | [], Indirect _ -> ()
        | targets, (Direct _ | Indirect _) ->
          let total =
            List.fold_left (fun acc (_, n) -> Int64.add acc n) 0L targets
          in
          List.iter
            (fun (root, n) ->
              let share = Int64.div (Int64.mul weight n) total in
              if Int64.compare share 0L > 0 then add (Entry [root]) share)
            targets))

let reorder_blocks ~dump profile cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let counts, measured_edges, measured_blocks = measured_counts profile cfg in
  let function_is_hot =
    Label.Tbl.fold
      (fun _label count acc -> acc || Int64.compare count 0L > 0)
      counts false
  in
  Option.iter
    (fun ppf ->
      Format.fprintf ppf "*** FDO block frequencies for %s%s@." cfg.fun_name
        (if function_is_hot then "" else ": no samples");
      dump_entry ppf cfg;
      if not function_is_hot
      then
        (* The instrumented edges alone; the weighted edges of a hot function
           are printed by [build_layout]. *)
        let edge_labels = edge_labels cfg in
        Label.Tbl.iter
          (fun src edges ->
            List.iter
              (fun (dst, _) -> print_edge ppf ~edge_labels src dst None)
              edges)
          edge_labels)
    dump;
  if function_is_hot
  then (
    let measured = Label.Tbl.copy counts in
    repair_frequencies cfg counts ~measured:measured_blocks;
    record_call_edges ~dump profile cfg counts;
    Option.iter
      (fun ppf ->
        dump_frequencies ppf cfg_with_layout ~measured ~repaired:counts)
      dump;
    Cfg_with_layout.set_layout cfg_with_layout
      (DLL.of_list (build_layout ~dump measured_edges counts cfg_with_layout)))
