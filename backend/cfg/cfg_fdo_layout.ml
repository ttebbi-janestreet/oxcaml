[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
module DLL = Doubly_linked_list

(* A block's measured execution count is the maximum estimate over its
   instructions (body and terminator): instructions inlined from elsewhere or
   sharing a line can carry different counts, and the largest one reflects how
   often control actually reaches the block. Blocks about which the profile
   knows nothing start at 0 and rely on the frequency repair below. Also returns
   the debug info of the instruction the count came from, for [-dfdo]. *)
let block_count profile (block : Cfg.basic_block) =
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

(* Functions are placed into one text section per hotness class: the class is
   the base-2 order of magnitude of the function's hottest block, encoded so
   that hotter classes sort lexicographically first. The default GNU ld linker
   script sorts ".text.sorted.*" input sections by name, which groups all
   functions of a class together (dense hot pages) and orders the classes, with
   no linker script involvement. Functions without samples are left in their
   usual sections. *)
let section_for_count fun_count =
  let log2 =
    let rec loop count acc =
      if Int64.compare count 1L <= 0
      then acc
      else loop (Int64.shift_right_logical count 1) (acc + 1)
    in
    loop fun_count 0
  in
  Printf.sprintf ".text.sorted.%02d.caml" (63 - log2)

let assign_function_section ~dump cfg_with_layout counts =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let fun_count =
    Label.Tbl.fold
      (fun _label count acc ->
        if Int64.compare count acc > 0 then count else acc)
      counts 0L
  in
  let section =
    (* Named text sections are not supported on all targets (macOS, Windows);
       [Config.function_sections] tracks exactly that support.
       [-basic-block-sections] emits each function across sections of its own,
       which would override the placement, so leave that mode alone. *)
    if
      Int64.compare fun_count 0L > 0
      && Config.function_sections
      && not !Oxcaml_flags.basic_block_sections
    then (
      let name = section_for_count fun_count in
      cfg.fun_text_section <- Some name;
      Some name)
    else None
  in
  Option.iter
    (fun ppf ->
      Format.fprintf ppf "  function: max count %Ld%s@." fun_count
        (match section with Some name -> ", section " ^ name | None -> ""))
    dump

let reorder_cold_blocks ~dump profile cfg_with_layout =
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
    assign_function_section ~dump cfg_with_layout measured;
    let is_cold label = Int64.equal (Label.Tbl.find counts label) 0L in
    Cfg_with_layout.reorder_blocks
      ~comparator:(fun label1 label2 ->
        Bool.compare (is_cold label1) (is_cold label2))
      cfg_with_layout
