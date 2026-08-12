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

/* The C part of the patchprof slow stub.  This file is compiled with
   -mgeneral-regs-only (see Makefile.upstream): together with
   [no_caller_saved_registers] this guarantees that the function preserves
   every register it touches, including the vector state. */

#define CAML_INTERNALS

#include <stdint.h>

#include "caml/fiber.h"
#include "caml/mlvalues.h"
#include "caml/patchprof.h"
#include "caml/stack.h"

/* These helpers are always inlined into [caml_patchprof_sample].  This whole
   translation unit is compiled with [-mgeneral-regs-only], so the walk cannot
   touch vector state or call ordinary runtime code. */
static inline __attribute__((always_inline))
frame_descr *find_frame_descriptor(caml_frame_descrs *fds, uintnat pc)
{
  uintnat slot = Hash_retaddr(pc, fds->mask);
  while (1) {
    frame_descr *descriptor = fds->descriptors[slot];
    if (descriptor == NULL) return NULL;
    if (Retaddr_frame(descriptor) == pc) return descriptor;
    slot = (slot + 1) & fds->mask;
  }
}

static inline __attribute__((always_inline))
uint32_t patchprof_frame_size(frame_descr *descriptor)
{
  uint16_t data = descriptor->frame_data;
  uint32_t full_data =
    data == FRAME_LONG_MARKER
    ? ((frame_descr_long *)descriptor)->frame_data
    : data;
  return full_data & ~FRAME_DESCRIPTOR_FLAGS;
}

static inline __attribute__((always_inline))
frame_descr *next_frame_descriptor(caml_frame_descrs *fds,
                                   uintnat *pc,
                                   char **sp,
                                   struct stack_info *stack)
{
  while (1) {
    frame_descr *descriptor = find_frame_descriptor(fds, *pc);
    if (descriptor == NULL) return NULL;
    if (descriptor->frame_data != FRAME_RETURN_TO_C) {
      *sp += patchprof_frame_size(descriptor);
      *pc = Saved_return_address(*sp);
      return descriptor;
    }
    *sp += Stack_header_size;
    if (*sp == (char *)Stack_high(stack)) {
      *pc = 0;
      return NULL;
    }
    *sp = First_frame(*sp);
    *pc = Saved_return_address(*sp);
  }
}

/* Counter bookkeeping and stack walking for one expired countdown, called
   from the slow stub with all OCaml registers live and only the stub stack to
   run on. */
CAMLno_tsan
CAMLno_asan
__attribute__((no_caller_saved_registers))
void caml_patchprof_sample(struct caml_patchprof_domain *domain,
                           uint64_t taken, uint64_t index)
{
  /* The 32-bit countdowns saturate the exponential backoff: once the
     period reaches UINT32_MAX, sampling becomes periodic.  [totals] and
     [tallies] advance by the reload actually consumed, keeping the exact
     execution-count identity and the tally weighting. */
  uint64_t period =
    domain->totals[index] / CAML_PATCHPROF_BACKOFF_DENOMINATOR;
  uint64_t reload = period > UINT32_MAX ? UINT32_MAX : period;
  domain->countdowns[index] = (uint32_t)reload;
  domain->totals[index] += reload;
  if (taken) domain->tallies[index] += reload;
  domain->slow_path_entries[index]++;

  const struct caml_patchprof_site *site = &domain->sites[index];
  uint64_t *frames = domain->last_walk_frames;
  uint32_t depth = 0;
  domain->walks_attempted++;
  frames[depth++] = (uint64_t)(uintptr_t)site->address;
  if (site->retaddr_offset != UINT16_MAX) {
    const uint64_t *slots =
      (const uint64_t *)(domain->countdowns + CAML_PATCHPROF_NUM_SITES);
    char *interrupted_rsp =
      (char *)(uintptr_t)slots[CAML_PATCHPROF_SLOT_SAVED_RSP];
    uintnat pc = *(uintnat *)(interrupted_rsp + site->retaddr_offset);
    char *sp = interrupted_rsp + site->retaddr_offset + sizeof(uintnat);
    struct stack_info *stack = domain->state->current_stack;
    while (depth < CAML_PATCHPROF_MAX_WALK_FRAMES) {
      uintnat current = pc;
      if (next_frame_descriptor(domain->frame_descrs, &pc, &sp, stack) == NULL)
        break;
      frames[depth++] = current;
    }
  }
  domain->last_walk_length = depth;
  domain->walk_frames_total += depth;
  if (depth < 2) domain->walks_failed++;

  /* Append the raw walk to the buffer: the sample weight (the period
     credited to [totals] above), the frame count and site index packed
     into one word, then the frame addresses with the load bias removed.
     Plain stores only: this translation unit must not touch vector
     state.  When the next record might not fit, the buffer is flushed
     with raw write syscalls (no libc: its wrappers may touch vector
     state or run cancellation machinery). */
  if (domain->walk_log != NULL) {
    size_t needed = 2 + (size_t)depth;
    if (domain->walk_log_capacity - domain->walk_log_used < needed) {
      size_t used = domain->walk_log_used;
      domain->walk_log_used = CAML_PATCHPROF_WALK_CHUNK_HEADER_WORDS;
      int fd = caml_patchprof_profile_fd;
      if (fd >= 0) {
        domain->walk_log[0] = CAML_PATCHPROF_RECORD_WALKS;
        domain->walk_log[1] = used - 2;
        domain->walk_log[2] = (uint64_t)domain->state->id;
        const char *cursor = (const char *)domain->walk_log;
        uint64_t remaining = used * sizeof(uint64_t);
        while (remaining > 0) {
          long written;
          __asm__ volatile ("syscall"
                            : "=a"(written)
                            : "0"(1L) /* SYS_write */, "D"((long)fd),
                              "S"(cursor), "d"(remaining)
                            : "rcx", "r11", "memory");
          if (written == -4 /* EINTR */) continue;
          if (written <= 0) break; /* the dump path will notice and close */
          cursor += written;
          remaining -= (uint64_t)written;
        }
      }
    }
    if (domain->walk_log_capacity - domain->walk_log_used >= needed) {
      uint64_t *record = domain->walk_log + domain->walk_log_used;
      record[0] = reload;
      record[1] = (uint64_t)depth | ((uint64_t)index << 32);
      for (uint32_t i = 0; i < depth; i++)
        record[2 + i] = frames[i] - domain->load_bias;
      domain->walk_log_used += needed;
    } else {
      domain->walks_dropped++;
    }
  }
}
