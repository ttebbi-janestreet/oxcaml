/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*   Copyright 2026 Jane Street Group LLC                                 */
/*                                                                        */
/*   All rights reserved. This file is distributed under the terms of     */
/*   the GNU Lesser General Public License version 2.1, with the special  */
/*   exception on linking described in the file LICENSE.                  */
/*                                                                        */
/**************************************************************************/

/* Linked only when [-patchprof] is passed to the final ocamlopt invocation. */

#include "caml/m.h"
#include "caml/asm.h"

        .section .patchprof_stubs,"aw",%nobits
        .balign 2097152
        .globl caml_patchprof_stub_arena_force_link
        .hidden caml_patchprof_stub_arena_force_link
caml_patchprof_stub_arena_force_link:
        .space 2097152

        NONEXECSTACK_NOTE
