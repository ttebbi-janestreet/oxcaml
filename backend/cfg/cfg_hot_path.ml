(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* Hot/cold code layout.

   [hot_path_to_here] lowers to an [Operation.Hint Hot_path] marker, and an
   empty [@cold] marker function lowers to an [Operation.Hint Cold_path] marker
   (both runtime no-ops). The semantics:

   - all code that can reach a [Hot_path] marker (backwards through the CFG) is
   hot;

   - all code reachable from a [Cold_path] marker on every path is cold; this is
   an AND-merge (a control-flow merge is cold only if all its predecessors are
   cold), so coldness is accurate;

   - cold beats hot: a cold block is never hot, and coldness stops the backward
   hot propagation (hot does not propagate through a cold block).

   This pass computes the cold and hot block sets, then reorders the layout into
   three groups -- hot, then unmarked, then cold -- so that an unmarked block
   behaves like cold when any hot block is present, and like hot otherwise. It
   then strips the (code-free) markers.

   Functions using neither marker have empty sets, so the layout is
   unchanged. *)

[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let is_marker kind (instr : Cfg.basic Cfg.instruction) =
  match[@ocaml.warning "-4"] instr.desc with
  | Op (Hint k) -> Cmm.equal_hint_kind k kind
  | _ -> false

let block_contains_marker kind (block : Cfg.basic_block) =
  DLL.exists block.body ~f:(is_marker kind)

let seed_blocks cfg kind =
  Cfg.fold_blocks cfg ~init:Label.Set.empty ~f:(fun label block acc ->
      if block_contains_marker kind block then Label.Set.add label acc else acc)

(* Forward "must be cold" analysis (greatest fixpoint): a block is cold iff it
   contains a [Cold_path] marker, or it is not the entry, has at least one
   predecessor and all its predecessors are cold. We start from "all blocks
   cold" and remove blocks that fail the condition; this handles loops inside a
   cold region, where a naive least fixpoint would stall on the back edge. *)
let cold_blocks cfg =
  let seeds = seed_blocks cfg Cmm.Cold_path in
  if Label.Set.is_empty seeds
  then Label.Set.empty
  else begin
    let entry = Cfg.entry_label cfg in
    let all =
      Cfg.fold_blocks cfg ~init:Label.Set.empty ~f:(fun label _ acc ->
          Label.Set.add label acc)
    in
    let cold = ref all in
    let changed = ref true in
    while !changed do
      changed := false;
      Label.Set.iter
        (fun label ->
          if Label.Set.mem label !cold && not (Label.Set.mem label seeds)
          then begin
            let preds = Cfg.predecessor_labels (Cfg.get_block_exn cfg label) in
            let stays_cold =
              (not (Label.equal label entry))
              && (match preds with [] -> false | _ :: _ -> true)
              && List.for_all (fun p -> Label.Set.mem p !cold) preds
            in
            if not stays_cold
            then begin
              cold := Label.Set.remove label !cold;
              changed := true
            end
          end)
        all
    done;
    !cold
  end

(* Blocks from which a [Hot_path] marker is reachable, walking predecessors
   backwards from the seeds. Cold blocks act as barriers: a cold block is never
   marked hot and the walk does not propagate through it (cold beats hot). *)
let hot_blocks cfg ~cold =
  let seeds = seed_blocks cfg Cmm.Hot_path in
  let hot = ref Label.Set.empty in
  let rec visit label =
    if (not (Label.Set.mem label !hot)) && not (Label.Set.mem label cold)
    then begin
      hot := Label.Set.add label !hot;
      List.iter visit (Cfg.predecessor_labels (Cfg.get_block_exn cfg label))
    end
  in
  Label.Set.iter visit seeds;
  !hot

let remove_markers cfg =
  let is_any_marker instr =
    is_marker Cmm.Hot_path instr || is_marker Cmm.Cold_path instr
  in
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      DLL.filter_left block.body ~f:(fun instr -> not (is_any_marker instr)))

let reorder cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let cold = cold_blocks cfg in
  let hot = hot_blocks cfg ~cold in
  (if not (Label.Set.is_empty hot && Label.Set.is_empty cold)
   then
     (* Stable three-way partition: hot blocks first, then unmarked, then cold.
        [reorder_blocks] keeps the entry block first and a zero comparison
        preserves the original relative order, so blocks within a group are not
        gratuitously reshuffled. With no hot blocks present, unmarked blocks
        sort before cold ones (unmarked behaves like hot); with hot blocks
        present, unmarked blocks sort after them (unmarked behaves like
        cold). *)
     let rank label =
       if Label.Set.mem label cold
       then 2
       else if Label.Set.mem label hot
       then 0
       else 1
     in
     let comparator label1 label2 = Int.compare (rank label1) (rank label2) in
     Cfg_with_layout.reorder_blocks ~comparator cfg_with_layout);
  remove_markers cfg;
  cfg_with_layout
