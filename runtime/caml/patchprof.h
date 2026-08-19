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

#ifndef CAML_PATCHPROF_H
#define CAML_PATCHPROF_H

#include <stdint.h>

#ifdef CAML_INTERNALS

#include "mlvalues.h"
#include "domain_state.h"
#include "frame_descriptors.h"

#define CAML_PATCHPROF_NUM_SITES 4096
#define CAML_PATCHPROF_DEFAULT_SITE_STRIDE 4

/* Rotation of the instrumented subset is on by default, so that even a
   single long-running process converges on full coverage; the period is
   long enough to make the ~6.5ms re-patching cost negligible.
   [OCAML_PATCHPROF_ROTATE_MS] overrides it; 0 disables rotation. */
#define CAML_PATCHPROF_DEFAULT_ROTATE_MS 6000
#define CAML_PATCHPROF_INITIAL_COUNTDOWN 500
#define CAML_PATCHPROF_BACKOFF_DENOMINATOR 50
#define CAML_PATCHPROF_STUB_STACK_BYTES 65536

/* Data appended to each domain state, in this order: the 32-bit per-site
   countdowns (the only counters the fast stub touches, %r14-relative; the
   slow stub reloads them with the sampling period saturated to
   UINT32_MAX), followed by the word-sized slots the slow stub needs to
   hand over to [caml_patchprof_sample]. */

enum caml_patchprof_slot {
  CAML_PATCHPROF_SLOT_DOMAIN,     /* the struct caml_patchprof_domain */
  CAML_PATCHPROF_SLOT_STACK_TOP,  /* where the slow stub points %rsp while
                                     calling C: the top of the mmap'd stub
                                     stack (guard page below), offset so
                                     that the stub's pushes leave the callee
                                     ABI-aligned */
  CAML_PATCHPROF_SLOT_SAVED_RSP,  /* the interrupted OCaml %rsp.  Scratch
                                     registers are pushed on the stub stack
                                     instead, but %rsp itself must live
                                     here, and this also keeps the OCaml
                                     stack walkable from the slow path. */
  CAML_PATCHPROF_NUM_SLOTS
};

#define CAML_PATCHPROF_COUNTER_BYTES                                  \
  (CAML_PATCHPROF_NUM_SITES * sizeof(uint32_t)                         \
   + CAML_PATCHPROF_NUM_SLOTS * sizeof(uint64_t))

#define CAML_PATCHPROF_MAX_WALK_FRAMES 32

/* The profile is a stream of 64-bit little-endian words: a magic word,
   then records of the form [kind, payload_words, payload...].  Chunks from
   different domains interleave at record granularity: the kernel
   serializes concurrent writes on the shared file description. */
#define CAML_PATCHPROF_MAGIC 0x000a31464f525050ull /* "PPROF1\n" */

enum caml_patchprof_record {
  /* seed, stride, initial_countdown, num_unique_sites, window_start,
     residue, num_selected_sites, install monotonic ns, install realtime
     ns */
  CAML_PATCHPROF_RECORD_SELECTION = 1,
  /* domain_id, then walk records: weight, frame count in the low half and
     site index in the high half, then one word per frame address */
  CAML_PATCHPROF_RECORD_WALKS = 2,
  /* domain_id, num_sites, active ns (wall-clock time this domain spent
     observing the current window), then per site: address, executions,
     slow_path_entries, sampled_weight, tally */
  CAML_PATCHPROF_RECORD_COUNTERS = 3,
  /* domain_id, walks_attempted, walks_failed, walk_frames_total,
     walks_dropped */
  CAML_PATCHPROF_RECORD_STATS = 4,
};

/* [kind, payload_words, domain_id] precede the walk records in the
   per-domain buffer, so a filled buffer is flushed with a single write. */
#define CAML_PATCHPROF_WALK_CHUNK_HEADER_WORDS 3

/* Per-domain walk buffer; the slow path flushes it with a raw write
   syscall whenever the next record might not fit. */
#define CAML_PATCHPROF_WALK_LOG_BYTES ((size_t)256 * 1024)

/* The profile file, -1 when profiling is inactive or the file died.  Read
   by the slow path when flushing walk chunks. */
extern int caml_patchprof_profile_fd;

struct caml_patchprof_site {
  unsigned char *address;
  uint8_t flag_writer_length;
  uint8_t jcc_length;
  /* Byte offset from the interrupted stack pointer to the return address of
     the frame containing the site; UINT16_MAX when unknown. */
  uint16_t retaddr_offset;
};

/* The remaining per-domain data, allocated separately from the domain
   state and reachable from CAML_PATCHPROF_SLOT_DOMAIN. */
struct caml_patchprof_domain {
  uint32_t *countdowns;      /* into the owning domain state */
  caml_domain_state *state;  /* the owning domain state */
  const struct caml_patchprof_site *sites;
  caml_frame_descrs *frame_descrs;
  uintptr_t load_bias;       /* subtracted from logged frame addresses */
  uint64_t window_start_ns;  /* when this domain started observing the
                                current window (monotonic) */
  uint64_t walks_attempted;
  uint64_t walks_failed;     /* no valid frame beyond the site itself */
  uint64_t walk_frames_total;
  /* Raw walk log: each record is one word of sample weight, one word
     packing the frame count (low half) and site index (high half), then
     one word per frame address.  Dumped with the profile and reset. */
  uint64_t *walk_log;
  size_t walk_log_used;      /* in 64-bit words */
  size_t walk_log_capacity;  /* in 64-bit words */
  uint64_t walks_dropped;    /* walks that did not fit in the log */
  uint32_t last_walk_length;
  uint64_t last_walk_frames[CAML_PATCHPROF_MAX_WALK_FRAMES];
  uint64_t totals[CAML_PATCHPROF_NUM_SITES];
  uint64_t tallies[CAML_PATCHPROF_NUM_SITES];
  uint64_t slow_path_entries[CAML_PATCHPROF_NUM_SITES];
};

/* The C part of the slow stub; defined in patchprof_sample.c, which is
   compiled so that the function preserves all registers.  [taken] is 0 or
   1; it comes before [index] because the slow stub materializes the
   condition directly into the second argument register.  The attribute
   must be on the declaration as well: clang rejects declarations that
   disagree about [no_caller_saved_registers].  It only exists on x86;
   other targets don't run patched code and need no attribute. */
#ifdef __x86_64__
#define CAML_PATCHPROF_SAMPLE_ATTRIBUTES \
  __attribute__((no_caller_saved_registers))
#else
#define CAML_PATCHPROF_SAMPLE_ATTRIBUTES
#endif
extern void CAML_PATCHPROF_SAMPLE_ATTRIBUTES
caml_patchprof_sample(struct caml_patchprof_domain *domain,
                      uint64_t taken, uint64_t index);

Caml_inline uint32_t *caml_patchprof_countdowns(caml_domain_state *state)
{
  return (uint32_t *)((char *)state + sizeof(caml_domain_state));
}

/* The countdown array is 16384 bytes, so the slots stay 8-aligned. */
Caml_inline uint64_t *caml_patchprof_slots(caml_domain_state *state)
{
  return (uint64_t *)
    (caml_patchprof_countdowns(state) + CAML_PATCHPROF_NUM_SITES);
}

#endif /* CAML_INTERNALS */

#endif /* CAML_PATCHPROF_H */
