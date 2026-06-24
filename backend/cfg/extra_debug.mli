(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
(** Insertion of extra debugging information used to correlate between machine
    instructions, [Linear] and [Cfg] code. *)

[@@@ocaml.warning "+a-40-41-42"]

(** Writes the id of each cfg instruction into [fdo] field. This information is
    copied to the corresponding field of [Linear.instruction] during
    [cfg_to_linear]. Having this as a separate pass allows us to choose whether
    to add the extra information to the IR or not, without passing extra
    arguments to [cfg_to_linear] and independently of it.. *)
val add : Cfg_with_layout.t -> unit

(** Stamp the [fdo] discriminator of OCaml call sites so that ocamlfdo can count
    calls: actual (non-inlined) calls — the [Call] and tail-call terminators —
    get one discriminator, and the virtual (inlined) call markers
    ([Source_location] left by inlining) another. C/runtime calls and probes are
    not marked. Used by [-emit-fdo-instrumentation]; the discriminators are
    copied to the emitted [.loc] during [cfg_to_linear]. *)
val mark_calls : Cfg_with_layout.t -> unit
