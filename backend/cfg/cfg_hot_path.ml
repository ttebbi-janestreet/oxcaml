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

(* Hot-path code layout.

   A [hot_path_to_here ()] call lowers to an [Operation.Hint Hot_path] marker
   instruction. The semantics is that all code that can reach such a marker
   (propagated backwards through the control-flow graph) is hot. This pass:

   1. finds the "seed" blocks that contain a marker;

   2. computes, by backward reachability over predecessors, the set of blocks
   that can reach a seed (the hot blocks);

   3. reorders the block layout so that hot blocks are emitted before cold ones
   (keeping the entry block first and otherwise preserving the original order);
   and

   4. removes the now-redundant marker instructions, which generate no code.

   Functions that do not use the intrinsic have no seeds, hence no hot blocks,
   so their layout is left unchanged. *)

[@@@ocaml.warning "+a-40-41-42"]

module DLL = Oxcaml_utils.Doubly_linked_list

let is_marker (instr : Cfg.basic Cfg.instruction) =
  match[@ocaml.warning "-4"] instr.desc with
  | Op (Hint Cmm.Hot_path) -> true
  | _ -> false

let block_contains_marker (block : Cfg.basic_block) =
  DLL.exists block.body ~f:is_marker

(* Blocks from which a marker block is reachable, computed by walking
   predecessors backwards from the seeds. *)
let hot_blocks cfg =
  let seeds =
    Cfg.fold_blocks cfg ~init:[] ~f:(fun label block acc ->
        if block_contains_marker block then label :: acc else acc)
  in
  let hot = ref Label.Set.empty in
  let rec visit label =
    if not (Label.Set.mem label !hot)
    then (
      hot := Label.Set.add label !hot;
      List.iter visit (Cfg.predecessor_labels (Cfg.get_block_exn cfg label)))
  in
  List.iter visit seeds;
  !hot

let remove_markers cfg =
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      DLL.filter_left block.body ~f:(fun instr -> not (is_marker instr)))

let reorder cfg_with_layout =
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let hot = hot_blocks cfg in
  (if not (Label.Set.is_empty hot)
   then
     (* Stable partition: hot blocks before cold ones. [reorder_blocks] keeps
        the entry block first; a zero comparison preserves the original relative
        order, so cold code does not get gratuitously reshuffled. *)
     let comparator label1 label2 =
       match Label.Set.mem label1 hot, Label.Set.mem label2 hot with
       | true, false -> -1
       | false, true -> 1
       | true, true | false, false -> 0
     in
     Cfg_with_layout.reorder_blocks ~comparator cfg_with_layout);
  remove_markers cfg;
  cfg_with_layout
