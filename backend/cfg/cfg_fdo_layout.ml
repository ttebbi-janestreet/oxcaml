[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Doubly_linked_list

(* For a profile of instruction samples, a block's measured execution count is
   the maximum estimate over its instructions (body and terminator):
   instructions inlined from elsewhere or sharing a line can carry different
   counts, and the largest one reflects how often control actually reaches the
   block. Blocks about which the profile knows nothing start at 0 and rely on
   the frequency repair below. Also returns the debug info of the instruction
   the count came from, for [-dfdo]. *)
let block_count_from_instructions profile (block : Cfg.basic_block) =
  let best = ref 0L in
  let origin = ref Debuginfo.none in
  let update (instr : _ Cfg.instruction) =
    match Source_position_profile.count_for_debuginfo profile instr.dbg with
    | None -> ()
    | Some count ->
      if Int64.compare count !best > 0
      then (
        best := count;
        origin := instr.dbg)
  in
  DLL.iter block.body ~f:update;
  update block.terminator;
  !best, !origin

(* A profile of branch executions only counted conditional branches, so only a
   conditional terminator's position may consult it: a body instruction that
   happens to share the branch's source position would otherwise soak up the
   branch's count. (A three-way integer test emits two branches at one position;
   their counts add up here, overestimating the block by up to 2x until the
   terminator-level edge counts are consumed directly.) *)
let block_count_from_branches profile (block : Cfg.basic_block) =
  let terminator = block.terminator in
  match[@warning "-4"] terminator.desc with
  | Cfg.Parity_test _ | Cfg.Truth_test _ | Cfg.Float_test _ | Cfg.Int_test _
    -> (
    match
      Source_position_profile.count_for_debuginfo profile terminator.dbg
    with
    | Some count -> count, terminator.dbg
    | None -> 0L, Debuginfo.none)
  | _ -> 0L, Debuginfo.none

let block_count profile block =
  match Source_position_profile.kind profile with
  | Instructions -> block_count_from_instructions profile block
  | Branches -> block_count_from_branches profile block

(* Sampling only measures blocks whose instructions have source positions that
   were actually hit; compiler-generated blocks (e.g. a division's zero-check)
   or blocks whose few instructions escaped the sampler come out as 0 even in
   the middle of a hot path. Repair the counts using flow conservation: all
   executions of a block continue into its successors and arrived through its
   predecessors, so each of those sets must account for at least the block's own
   count.

   Exact knowledge propagates first. A set consisting of a single block must
   carry the whole count. A set with several members whose counts fall short is
   scaled up proportionally, which never raises a member that is still 0: a zero
   next to a hot sibling is meaningful (the flow demonstrably goes to the
   sibling).

   A set whose members are ALL still 0 once no exact propagation applies
   anywhere is different: the flow has to go somewhere, and sinking a block that
   might carry it (e.g. a loop backedge none of whose instructions got sampled)
   costs a jump on a hot path, so as a last resort the count is split evenly
   across the members. Deferring the split until the exact rules have converged
   matters: a set that merely has not received its counts yet would otherwise
   leak flow into genuinely cold members, and since counts never decrease, such
   a mistake would stick.

   Counts only increase, up to the hottest measured block, so the iteration
   reaches a fixed point. *)
let repair_frequencies (cfg : Cfg.t) counts =
  let count label = Label.Tbl.find counts label in
  let changed = ref true in
  let raise_group ~even_split labels ~target =
    if not (Label.Set.is_empty labels)
    then
      let sum =
        Label.Set.fold (fun l acc -> Int64.add acc (count l)) labels 0L
      in
      if Int64.compare sum target < 0
      then
        match Label.Set.elements labels with
        | [single] ->
          Label.Tbl.replace counts single target;
          changed := true
        | [] -> ()
        | _ :: _ :: _ ->
          if Int64.compare sum 0L > 0
          then
            let factor = Int64.to_float target /. Int64.to_float sum in
            Label.Set.iter
              (fun l ->
                let c = count l in
                let scaled = Int64.of_float (Int64.to_float c *. factor) in
                if Int64.compare scaled c > 0
                then (
                  Label.Tbl.replace counts l scaled;
                  changed := true))
              labels
          else if even_split
          then
            let cardinal = Int64.of_int (Label.Set.cardinal labels) in
            let share = Int64.div target cardinal in
            let first_share = Int64.add share (Int64.rem target cardinal) in
            let first = ref true in
            Label.Set.iter
              (fun l ->
                let share = if !first then first_share else share in
                first := false;
                if Int64.compare share 0L > 0
                then (
                  Label.Tbl.replace counts l share;
                  changed := true))
              labels
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

let print_origin ppf dbg =
  match List.rev (Debuginfo.to_items dbg) with
  | [] -> Format.pp_print_string ppf "<no debug info>"
  | items ->
    Format.pp_print_string ppf
      (String.concat " <- "
         (List.map Source_position_profile.frame_string_of_item items))

let dump_frequencies ppf cfg_with_layout ~measured ~origins ~repaired =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  Format.fprintf ppf "*** FDO block frequencies for %s@." cfg.fun_name;
  DLL.iter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
      let m = Label.Tbl.find measured label in
      let r = Label.Tbl.find repaired label in
      Format.fprintf ppf "  block %a: measured %Ld" Label.format label m;
      if Int64.compare m 0L > 0
      then
        Format.fprintf ppf " from %a" print_origin
          (Label.Tbl.find origins label);
      Format.fprintf ppf ", repaired %Ld%s@." r
        (if Int64.equal r 0L then " -> cold" else ""))

(* Measured functions are sorted by hotness, approximating ocamlfdo's
   sorting of functions by execution count with power-of-two classes: a
   function whose hottest measured block has a count in
   [total/2^(k+1), total/2^k) goes to class k.  The class names use the
   ".text.sorted." prefix so that the default GNU ld linker script
   (binutils >= 2.35) packs them via its "SORT(.text.sorted.<star>)" input
   section rule in name order - hottest class first - between ".text.hot"
   and ".text",
   with no linker script involvement; an older linker matches them with its
   plain ".text.*" rule and the layout degrades to input order.  Functions
   the profile never saw stay in their usual section. *)
let hot_section_name klass = Printf.sprintf ".text.sorted.caml.%02d" klass

(* Two digits of zero-padding orders up to 100 classes; class 99 would mean
   a count 2^99 times smaller than the total, so the clamp is theoretical. *)
let hot_section_max_class = 99

let assign_function_section ~dump profile cfg_with_layout counts =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let fun_count =
    Label.Tbl.fold
      (fun _label count acc ->
        if Int64.compare count acc > 0 then count else acc)
      counts 0L
  in
  let total = Source_position_profile.total_samples profile in
  let section =
    (* The shared predicate keeps this in sync with the DWARF code-layout
       mode selection in [Emitaux.begin_dwarf]. *)
    if
      Int64.compare fun_count 0L > 0
      && Int64.compare total fun_count >= 0
      && Oxcaml_flags.fdo_section_sorting_enabled ()
    then (
      let klass =
        int_of_float
          (Float.log2 (Int64.to_float total /. Int64.to_float fun_count))
      in
      let klass = Int.max 0 (Int.min hot_section_max_class klass) in
      let name = hot_section_name klass in
      cfg.fun_text_section <- Some name;
      Some name)
    else None
  in
  Option.iter
    (fun ppf ->
      Format.fprintf ppf "  function: max count %Ld%s@." fun_count
        (match section with Some name -> ", section " ^ name | None -> ""))
    dump

(* The weights of a block's normal successor edges, in profile-count units.
   Conditional and switch edges carrying pseudo-instrumentation labels whose
   counts the profile recorded are measured directly: an edge's weight is the
   sum of its labels' counts. A terminator none of whose labels was recorded
   (not instrumented, or a profile without labels) falls back to the flow bound
   [min (count src) (count dst)] per successor; single-successor control flow
   carries the whole block count. *)
let successor_edge_weights profile counts src (block : Cfg.basic_block) =
  let count label = Label.Tbl.find counts label in
  let src_count = count src in
  let terminator = block.terminator in
  (* The successor of each label-set position; must mirror the resolution in
     [Cfg_to_linear.resolve_edge_labels] (and, for [Switch], the indexing of
     [Simplify_terminator]). *)
  let positions : Label.t array =
    match[@warning "-4"] terminator.desc with
    | Parity_test { ifso; ifnot } | Truth_test { ifso; ifnot } ->
      [| ifso; ifnot |]
    | Int_test { lt; eq; gt; is_signed = _; imm = _ } -> [| lt; eq; gt |]
    | Float_test { lt; eq; gt; uo; width = _ } -> [| lt; eq; gt; uo |]
    | Switch labels -> labels
    | _ -> [||]
  in
  let measured =
    match Debuginfo.edge_labels terminator.dbg with
    | Some (Debuginfo.Positional sets)
      when Array.length positions > 0
           && Array.length sets = Array.length positions ->
      (* Group the positions by successor, in first-occurrence order. *)
      let edges : (Label.t * Debuginfo.branch_label list ref) list ref =
        ref []
      in
      Array.iteri
        (fun i dst ->
          match List.find_opt (fun (d, _) -> Label.equal d dst) !edges with
          | Some (_, labels) -> labels := sets.(i) @ !labels
          | None -> edges := (dst, ref sets.(i)) :: !edges)
        positions;
      let any_measured = ref false in
      let weights =
        List.rev_map
          (fun (dst, labels) ->
            let weight =
              List.fold_left
                (fun acc label ->
                  match
                    Source_position_profile.count_for_branch_label profile label
                  with
                  | None -> acc
                  | Some count ->
                    any_measured := true;
                    Int64.add acc count)
                0L !labels
            in
            dst, weight)
          !edges
      in
      if !any_measured then Some weights else None
    | None | Some (Debuginfo.Positional _) | Some (Debuginfo.Resolved _) -> None
  in
  match measured with
  | Some weights -> weights
  | None -> (
    match
      Label.Set.elements (Cfg.successor_labels ~normal:true ~exn:false block)
    with
    | [dst] -> [dst, src_count]
    | successors ->
      List.map (fun dst -> dst, Int64.min src_count (count dst)) successors)

(* Bottom-up chaining (à la Pettis-Hansen): every block starts as a chain of its
   own; the edges are considered hottest first, and an edge whose source is some
   chain's tail and whose destination is another chain's head concatenates the
   two chains, making the edge a fallthrough. The chains are then laid out: the
   entry block's chain first, the others in the original layout order of their
   first-occurring blocks, and chains consisting only of cold blocks sunk to the
   end (an edge only links blocks with positive counts, so this preserves the
   cold-block sinking). *)
type chain =
  { mutable blocks : Label.t list; (* in layout order, never empty *)
    head : Label.t; (* fixed: merging appends to the head chain *)
    mutable tail : Label.t;
    mutable cold : bool;
    mutable visited : bool (* for ordering the finished chains *)
  }

let build_chains ~dump profile counts cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let layout = Cfg_with_layout.layout cfg_with_layout in
  let chains = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  DLL.iter layout ~f:(fun label ->
      let cold = Int64.equal (Label.Tbl.find counts label) 0L in
      Label.Tbl.replace chains label
        { blocks = [label]; head = label; tail = label; cold; visited = false });
  let edges = ref [] in
  DLL.iter layout ~f:(fun src ->
      let block = Cfg.get_block_exn cfg src in
      List.iter
        (fun (dst, weight) ->
          (* The entry block can head no chain but its own, and a block cannot
             fall through to itself. *)
          if
            Int64.compare weight 0L > 0
            && (not (Label.equal dst cfg.entry_label))
            && not (Label.equal src dst)
          then edges := (src, dst, weight) :: !edges)
        (successor_edge_weights profile counts src block));
  (* Sorting is stable, so ties keep the original layout order and the result is
     deterministic. *)
  let edges =
    List.stable_sort
      (fun (_, _, weight1) (_, _, weight2) -> Int64.compare weight2 weight1)
      (List.rev !edges)
  in
  Option.iter
    (fun ppf ->
      List.iter
        (fun (src, dst, weight) ->
          Format.fprintf ppf "  edge %a -> %a: %Ld@." Label.format src
            Label.format dst weight)
        edges)
    dump;
  List.iter
    (fun (src, dst, _weight) ->
      let chain = Label.Tbl.find chains src in
      let successor_chain = Label.Tbl.find chains dst in
      if
        (not (chain == successor_chain))
        && Label.equal chain.tail src
        && Label.equal successor_chain.head dst
      then (
        chain.blocks <- chain.blocks @ successor_chain.blocks;
        chain.tail <- successor_chain.tail;
        chain.cold <- chain.cold && successor_chain.cold;
        List.iter
          (fun label -> Label.Tbl.replace chains label chain)
          successor_chain.blocks))
    edges;
  (* The distinct chains, in the original layout order of their first-occurring
     blocks. *)
  let ordered = ref [] in
  DLL.iter layout ~f:(fun label ->
      let chain = Label.Tbl.find chains label in
      if not chain.visited
      then (
        chain.visited <- true;
        ordered := chain :: !ordered));
  let entry_chain = Label.Tbl.find chains cfg.entry_label in
  let hot, cold =
    List.partition
      (fun chain -> not chain.cold)
      (List.filter
         (fun chain -> not (chain == entry_chain))
         (List.rev !ordered))
  in
  (entry_chain :: hot) @ cold

let reorder_blocks ~dump profile cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let counts = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  let origins = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let count, origin = block_count profile block in
      Label.Tbl.replace counts label count;
      Label.Tbl.replace origins label origin);
  let function_is_hot =
    Label.Tbl.fold
      (fun _label count acc -> acc || Int64.compare count 0L > 0)
      counts false
  in
  if not function_is_hot
  then
    Option.iter
      (fun ppf ->
        Format.fprintf ppf "*** FDO block frequencies for %s: no samples@."
          cfg.fun_name)
      dump
  else
    let measured = Label.Tbl.copy counts in
    repair_frequencies cfg counts;
    Option.iter
      (fun ppf ->
        dump_frequencies ppf cfg_with_layout ~measured ~origins ~repaired:counts)
      dump;
    assign_function_section ~dump profile cfg_with_layout measured;
    let chains = build_chains ~dump profile counts cfg_with_layout in
    Option.iter
      (fun ppf ->
        List.iter
          (fun chain ->
            match chain.blocks with
            | [] | [_] -> ()
            | blocks ->
              Format.fprintf ppf "  chain:%s@."
                (String.concat ""
                   (List.map (Format.asprintf " %a" Label.format) blocks)))
          chains)
      dump;
    let rank = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
    let next = ref 0 in
    List.iter
      (fun chain ->
        List.iter
          (fun label ->
            Label.Tbl.replace rank label !next;
            incr next)
          chain.blocks)
      chains;
    Cfg_with_layout.reorder_blocks
      ~comparator:(fun label1 label2 ->
        Int.compare (Label.Tbl.find rank label1) (Label.Tbl.find rank label2))
      cfg_with_layout
