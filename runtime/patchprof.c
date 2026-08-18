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

/* Prototype runtime for patchprof.

   If [OCAML_PATCHPROF_OUT] is set (a file name, or a directory into which
   each process writes a freshly created, uniquely named profile, so that
   concurrent instances can share the environment), choose one fixed subset
   of recorded sites,
   patch it before the OCaml program starts, and append one counter batch
   whenever a domain stops.  Moving the subset during execution is outside
   the scope of this prototype.

   The subset is chosen deterministically from the selection seed, which is
   randomized by default and recorded in the profile's selection records, so
   a run can be reproduced exactly by setting [OCAML_PATCHPROF_SEED] to the
   recorded value. */

#define _GNU_SOURCE
#define CAML_INTERNALS

#include <elf.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <link.h>
#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/random.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#include "caml/fiber.h"
#include "caml/frame_descriptors.h"
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/misc.h"
#include "caml/osdeps.h"
#include "caml/patchprof.h"

int caml_patchprof_stub_arena_present;
static unsigned char *stub_arena_begin;
static unsigned char *stub_arena_end;

static struct caml_patchprof_site sites[CAML_PATCHPROF_NUM_SITES];
static uint32_t num_sites;
int caml_patchprof_profile_fd = -1;
#define profile_fd caml_patchprof_profile_fd

static int write_record(uint64_t kind, const uint64_t *payload,
                        size_t words);
static uintptr_t load_bias;
static caml_domain_state *initial_domain;
static int initial_domain_dumped;
static pthread_mutex_t output_mutex = PTHREAD_MUTEX_INITIALIZER;

static uint64_t initial_countdown = CAML_PATCHPROF_INITIAL_COUNTDOWN;

/* Measurement knob (OCAML_PATCHPROF_NO_DUMP): keep all instrumentation and
   sampling active but skip the counter/walk dump, to isolate its cost. */
static int dump_disabled;

/* Selection parameters, recorded in the profile header so that the
   instrumented subset can be reconstructed from the executable. */
static uint64_t selection_seed;
static uint32_t selection_stride;

/* Window rotation (OCAML_PATCHPROF_ROTATE_MS): from a stop-the-world
   section, the current window's counters are dumped, its text restored
   from the executable image, and a fresh window selected and patched.
   The PRNG continues from the seed, so the whole trajectory of windows is
   reproducible. */
static uint64_t rotate_period_ns;
static uint64_t last_rotation_ns;
static uintptr_t patched_hull_lo;
static uintptr_t patched_hull_hi;
static uint64_t patched_hull_file_offset;
static size_t selection_num_unique;
static size_t selection_window_start;
static uint32_t selection_residue;

static int write_all(int fd, const void *data, size_t length)
{
  const char *cursor = data;
  while (length > 0) {
    ssize_t written = write(fd, cursor, length);
    if (written < 0 && errno == EINTR) continue;
    if (written <= 0) return -1;
    cursor += written;
    length -= (size_t)written;
  }
  return 0;
}

static int find_load_bias(struct dl_phdr_info *info, size_t size, void *data)
{
  (void)size;
  if (info->dlpi_name == NULL || info->dlpi_name[0] == '\0') {
    *(uintptr_t *)data = (uintptr_t)info->dlpi_addr;
    return 1;
  }
  return 0;
}

static int range_is_in_file(size_t offset, size_t length, size_t file_size)
{
  return offset <= file_size && length <= file_size - offset;
}

struct mapped_file {
  const unsigned char *data;
  size_t size;
};

static int map_self_exe(struct mapped_file *exe)
{
  int result = -1;
  int fd = open("/proc/self/exe", O_RDONLY | O_CLOEXEC);
  if (fd < 0) return -1;
  struct stat st;
  if (fstat(fd, &st) == 0 && st.st_size >= (off_t)sizeof(Elf64_Ehdr)) {
    void *data =
      mmap(NULL, (size_t)st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
    if (data != MAP_FAILED) {
      exe->data = data;
      exe->size = (size_t)st.st_size;
      result = 0;
    }
  }
  close(fd);
  return result;
}

static void unmap_file(struct mapped_file *exe)
{
  munmap((void *)exe->data, exe->size);
}

/* Find the section header named [name], validating all the ELF structures
   the lookup touches.  The section's *contents* are not validated to lie in
   the file: NOBITS sections such as the stub arena have none. */
static const Elf64_Shdr *find_elf_section(const struct mapped_file *exe,
                                          const char *name)
{
  const Elf64_Ehdr *ehdr = (const Elf64_Ehdr *)exe->data;
  if (memcmp(ehdr->e_ident, ELFMAG, SELFMAG) != 0
      || ehdr->e_ident[EI_CLASS] != ELFCLASS64
      || ehdr->e_ident[EI_DATA] != ELFDATA2LSB
      || ehdr->e_shentsize != sizeof(Elf64_Shdr)
      || ehdr->e_shstrndx >= ehdr->e_shnum
      || !range_is_in_file((size_t)ehdr->e_shoff,
                           (size_t)ehdr->e_shnum * sizeof(Elf64_Shdr),
                           exe->size))
    return NULL;
  const Elf64_Shdr *sections =
    (const Elf64_Shdr *)(exe->data + (size_t)ehdr->e_shoff);
  const Elf64_Shdr *strings = &sections[ehdr->e_shstrndx];
  if (strings->sh_size == 0
      || !range_is_in_file((size_t)strings->sh_offset,
                           (size_t)strings->sh_size, exe->size))
    return NULL;
  const char *names = (const char *)exe->data + strings->sh_offset;
  /* A terminated string table makes the [strcmp]s below safe. */
  if (names[strings->sh_size - 1] != '\0') return NULL;
  for (uint16_t i = 0; i < ehdr->e_shnum; i++) {
    if (sections[i].sh_name >= strings->sh_size) return NULL;
    if (strcmp(names + sections[i].sh_name, name) == 0) return &sections[i];
  }
  return NULL;
}

static void caml_patchprof_find_stub_arena(void) __attribute__((constructor));

static void caml_patchprof_find_stub_arena(void)
{
  if (caml_secure_getenv(T("OCAML_PATCHPROF_OUT")) == NULL) return;
  struct mapped_file exe;
  if (map_self_exe(&exe) != 0) return;
  const Elf64_Shdr *stubs = find_elf_section(&exe, ".patchprof_stubs");
  if (stubs != NULL) {
    dl_iterate_phdr(find_load_bias, &load_bias);
    stub_arena_begin =
      (unsigned char *)(load_bias + (uintptr_t)stubs->sh_addr);
    stub_arena_end = stub_arena_begin + stubs->sh_size;
    caml_patchprof_stub_arena_present = 1;
  }
  unmap_file(&exe);
}

static int read_uleb128(const unsigned char **cursor,
                        const unsigned char *end,
                        uint64_t *value)
{
  uint64_t result = 0;
  unsigned shift = 0;
  while (*cursor < end && shift < 64) {
    uint8_t byte = *(*cursor)++;
    result |= (uint64_t)(byte & 0x7f) << shift;
    if ((byte & 0x80) == 0) {
      *value = result;
      return 0;
    }
    shift += 7;
  }
  return -1;
}

static int compare_patchprof_site(const void *left, const void *right)
{
  const struct caml_patchprof_site *a = left;
  const struct caml_patchprof_site *b = right;
  uintptr_t a_address = (uintptr_t)a->address;
  uintptr_t b_address = (uintptr_t)b->address;
  if (a_address != b_address)
    return (a_address > b_address) - (a_address < b_address);
  if (a->flag_writer_length != b->flag_writer_length)
    return (a->flag_writer_length > b->flag_writer_length) ? 1 : -1;
  if (a->jcc_length != b->jcc_length)
    return (a->jcc_length > b->jcc_length) ? 1 : -1;
  return (a->retaddr_offset > b->retaddr_offset)
         - (a->retaddr_offset < b->retaddr_offset);
}

/* SplitMix64, seeded from [OCAML_PATCHPROF_SEED]. */
static uint64_t prng_state;

static uint64_t random_u64(void)
{
  uint64_t z = (prng_state += 0x9e3779b97f4a7c15ull);
  z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ull;
  z = (z ^ (z >> 27)) * 0x94d049bb133111ebull;
  return z ^ (z >> 31);
}

/* Parse a decimal unsigned integer, rejecting empty strings, signs, and
   trailing junk. */
static int parse_u64(const char *text, uint64_t *value)
{
  if (text == NULL || text[0] < '0' || text[0] > '9') return -1;
  char *end;
  errno = 0;
  unsigned long long parsed = strtoull(text, &end, 10);
  if (errno != 0 || *end != '\0') return -1;
  *value = (uint64_t)parsed;
  return 0;
}

static int read_seed(void)
{
  uint64_t seed;
  const char *text =
    (const char *)caml_secure_getenv(T("OCAML_PATCHPROF_SEED"));
  if (text == NULL) {
    /* Randomize by default; the seed is recorded in the profile's selection
       records, so the run can still be reproduced exactly by setting
       [OCAML_PATCHPROF_SEED] to the recorded value. */
    if (getrandom(&seed, sizeof seed, 0) != sizeof seed)
      caml_fatal_error("patchprof: could not obtain a random seed");
  } else {
    if (parse_u64(text, &seed) != 0) return -1;
  }
  selection_seed = seed;
  prng_state = seed;
  return 0;
}

static int read_initial_countdown(uint64_t *countdown)
{
  const char *text =
    (const char *)caml_secure_getenv(T("OCAML_PATCHPROF_N0"));
  if (text == NULL || text[0] == '\0') {
    *countdown = CAML_PATCHPROF_INITIAL_COUNTDOWN;
    return 0;
  }
  return parse_u64(text, countdown) != 0
         || *countdown == 0
         || *countdown > UINT32_MAX
         ? -1 : 0;
}

static int read_rotation_period(void)
{
  const char *text =
    (const char *)caml_secure_getenv(T("OCAML_PATCHPROF_ROTATE_MS"));
  if (text == NULL || text[0] == '\0') {
    rotate_period_ns = 0;
    return 0;
  }
  uint64_t milliseconds;
  if (parse_u64(text, &milliseconds) != 0
      || milliseconds > UINT64_MAX / 1000000)
    return -1;
  rotate_period_ns = milliseconds * 1000000;
  return 0;
}

static uint64_t monotonic_ns(void)
{
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return (uint64_t)now.tv_sec * 1000000000ull + (uint64_t)now.tv_nsec;
}


static int read_site_stride(uint32_t *stride)
{
  const char *text =
    (const char *)caml_secure_getenv(T("OCAML_PATCHPROF_D"));
  if (text == NULL || text[0] == '\0') {
    *stride = CAML_PATCHPROF_DEFAULT_SITE_STRIDE;
    return 0;
  }
  uint64_t parsed;
  if (parse_u64(text, &parsed) != 0 || parsed == 0 || parsed > UINT32_MAX)
    return -1;
  *stride = (uint32_t)parsed;
  return 0;
}

/* Fallback selection for metadata that is not strictly sorted: sort a full
   decode, collapse duplicates, and pick the window.  Not reached for
   executables our linkers produce; see [select_from_sorted_blocks]. */
static int select_sites(struct caml_patchprof_site *all_sites,
                        size_t all_count,
                        uint32_t stride)
{
  if (all_count == 0) return -1;
  qsort(all_sites, all_count, sizeof *all_sites, compare_patchprof_site);

  size_t unique_count = 0;
  for (size_t i = 0; i < all_count; i++) {
    if (unique_count > 0
        && all_sites[unique_count - 1].address == all_sites[i].address) {
      if (all_sites[unique_count - 1].flag_writer_length
            != all_sites[i].flag_writer_length
          || all_sites[unique_count - 1].jcc_length
               != all_sites[i].jcc_length
          || all_sites[unique_count - 1].retaddr_offset
               != all_sites[i].retaddr_offset)
        return -1;
      continue;
    }
    all_sites[unique_count++] = all_sites[i];
  }

  uint64_t block_random = random_u64();
  uint64_t residue_random = random_u64();

  size_t window_size = (size_t)stride * CAML_PATCHPROF_NUM_SITES;
  size_t num_windows = 1 + (unique_count - 1) / window_size;
  size_t window_start = (size_t)(block_random % num_windows) * window_size;
  size_t remaining = unique_count - window_start;
  size_t window_count = remaining < window_size ? remaining : window_size;
  size_t window_end = window_start + window_count;
  uint32_t num_residues =
    remaining < stride ? (uint32_t)remaining : stride;
  uint32_t residue = (uint32_t)(residue_random % num_residues);

  for (size_t i = window_start + residue;
       i < window_end;
       i += stride) {
    sites[num_sites++] = all_sites[i];
  }
  selection_stride = stride;
  selection_num_unique = unique_count;
  selection_window_start = window_start;
  selection_residue = residue;
  return num_sites > 0 ? 0 : -1;
}

struct site_block {
  const unsigned char *payload;
  const unsigned char *payload_end;
  uintptr_t first_address; /* with the load bias applied */
  uint32_t count;
};

/* Validate the 24-byte block header at [*cursor], fill in [block], and
   advance [*cursor] past the block's payload. */
static int read_block(const unsigned char **cursor,
                      const unsigned char *end,
                      struct site_block *block)
{
  if ((size_t)(end - *cursor) < 24 || memcmp(*cursor, "PPMD", 4) != 0)
    return -1;
  uint32_t version;
  uint32_t count;
  uint32_t payload_size;
  uint64_t first_address;
  memcpy(&version, *cursor + 4, sizeof version);
  memcpy(&count, *cursor + 8, sizeof count);
  memcpy(&payload_size, *cursor + 12, sizeof payload_size);
  memcpy(&first_address, *cursor + 16, sizeof first_address);
  *cursor += 24;
  if (version != 4 || count == 0
      || (size_t)(end - *cursor) < (size_t)payload_size)
    return -1;
  block->payload = *cursor;
  block->payload_end = *cursor + payload_size;
  /* [load_bias] was computed by [caml_patchprof_find_stub_arena], which must
     have found the stub arena for this function to be reached. */
  block->first_address = load_bias + (uintptr_t)first_address;
  block->count = count;
  *cursor += payload_size;
  return 0;
}

static int decode_site_record(const unsigned char **cursor,
                              const unsigned char *end,
                              uintptr_t *previous,
                              struct caml_patchprof_site *site)
{
  uint64_t delta;
  if (read_uleb128(cursor, end, &delta) != 0
      || (size_t)(end - *cursor) < 2)
    return -1;
  uint16_t packed;
  memcpy(&packed, *cursor, sizeof packed);
  *cursor += sizeof packed;
  uint8_t flag_writer_length = packed & 0xf;
  if (flag_writer_length < 3) return -1;
  uint16_t retaddr_words = packed >> 5;
  *previous += (uintptr_t)delta;
  site->address = (unsigned char *)*previous;
  site->flag_writer_length = flag_writer_length;
  site->jcc_length = (packed & 0x10) != 0 ? 6 : 2;
  site->retaddr_offset =
    retaddr_words == 0x7ff ? UINT16_MAX : retaddr_words * 8;
  return 0;
}

/* Choose the instrumented subset assuming the sites are strictly sorted and
   duplicate-free, which holds for our executables: each block's deltas are
   emitted in text order and the linker concatenates blocks in text order.
   The block headers alone give the site total the window choice needs, so
   only the blocks the window touches are decoded.  Returns 0 on success, -1
   on malformed metadata, and 1 when a duplicate site disproves the
   assumption and the caller must fall back to sorting a full decode. */
static int select_from_sorted_blocks(const struct site_block *blocks,
                                     size_t num_blocks,
                                     size_t total_count,
                                     uint32_t stride)
{
  uint64_t block_random = random_u64();
  uint64_t residue_random = random_u64();

  size_t window_size = (size_t)stride * CAML_PATCHPROF_NUM_SITES;
  size_t num_windows = 1 + (total_count - 1) / window_size;
  size_t window_start = (size_t)(block_random % num_windows) * window_size;
  size_t remaining = total_count - window_start;
  size_t window_count = remaining < window_size ? remaining : window_size;
  size_t window_end = window_start + window_count;
  uint32_t num_residues =
    remaining < stride ? (uint32_t)remaining : stride;
  uint32_t residue = (uint32_t)(residue_random % num_residues);

  size_t next = window_start + residue;
  size_t base = 0;
  for (size_t b = 0; b < num_blocks && next < window_end; b++) {
    if (base + blocks[b].count <= next) {
      base += blocks[b].count;
      continue;
    }
    const unsigned char *cursor = blocks[b].payload;
    uintptr_t previous = blocks[b].first_address;
    for (uint32_t i = 0; i < blocks[b].count && next < window_end; i++) {
      uintptr_t before = previous;
      struct caml_patchprof_site site;
      if (decode_site_record(&cursor, blocks[b].payload_end,
                             &previous, &site) != 0)
        return -1;
      if (i > 0 && previous == before) {
        num_sites = 0;
        return 1;
      }
      if (base + i == next) {
        sites[num_sites++] = site;
        next += stride;
      }
    }
    base += blocks[b].count;
  }
  selection_stride = stride;
  selection_num_unique = total_count;
  selection_window_start = window_start;
  selection_residue = residue;
  return num_sites > 0 ? 0 : -1;
}

static int load_sites_from_executable(uint32_t stride)
{
  struct site_block *blocks = NULL;
  struct caml_patchprof_site *all_sites = NULL;
  int result = -1;

  struct mapped_file exe;
  if (map_self_exe(&exe) != 0) return -1;

  const Elf64_Shdr *patchprof = find_elf_section(&exe, "patchprof_sites");
  if (patchprof == NULL
      || !range_is_in_file((size_t)patchprof->sh_offset,
                           (size_t)patchprof->sh_size, exe.size))
    goto out;
  const unsigned char *section_begin = exe.data + patchprof->sh_offset;
  const unsigned char *section_end = section_begin + patchprof->sh_size;

  size_t num_blocks = 0;
  for (const unsigned char *cursor = section_begin; cursor < section_end; ) {
    struct site_block block;
    if (read_block(&cursor, section_end, &block) != 0) goto out;
    num_blocks++;
  }
  if (num_blocks == 0) goto out;
  blocks = malloc(num_blocks * sizeof *blocks);
  if (blocks == NULL) goto out;

  size_t total_count = 0;
  int headers_sorted = 1;
  const unsigned char *cursor = section_begin;
  for (size_t b = 0; b < num_blocks; b++) {
    if (read_block(&cursor, section_end, &blocks[b]) != 0) goto out;
    total_count += blocks[b].count;
    if (b > 0 && blocks[b].first_address <= blocks[b - 1].first_address)
      headers_sorted = 0;
  }

  if (headers_sorted) {
    int selected =
      select_from_sorted_blocks(blocks, num_blocks, total_count, stride);
    if (selected <= 0) {
      result = selected;
      goto out;
    }
  }

  /* The metadata is not strictly sorted: decode everything and sort it,
     redrawing the selection randomness from scratch. */
  prng_state = selection_seed;
  if (total_count > SIZE_MAX / sizeof *all_sites) goto out;
  all_sites = malloc(total_count * sizeof *all_sites);
  if (all_sites == NULL) goto out;
  size_t all_count = 0;
  for (size_t b = 0; b < num_blocks; b++) {
    const unsigned char *payload_cursor = blocks[b].payload;
    uintptr_t previous = blocks[b].first_address;
    for (uint32_t i = 0; i < blocks[b].count; i++) {
      if (decode_site_record(&payload_cursor, blocks[b].payload_end,
                             &previous, &all_sites[all_count]) != 0)
        goto out;
      all_count++;
    }
  }
  result = select_sites(all_sites, all_count, stride);

out:
  free(all_sites);
  free(blocks);
  unmap_file(&exe);
  return result;
}

struct emitter {
  unsigned char *cursor;
  unsigned char *end;
};

static int emit_bytes(struct emitter *emitter, const void *bytes, size_t length)
{
  if ((size_t)(emitter->end - emitter->cursor) < length) return -1;
  memcpy(emitter->cursor, bytes, length);
  emitter->cursor += length;
  return 0;
}

/* The stub code is written once as the assembly templates below;
   [generate_stub] copies them into the arena and replaces the 32-bit
   placeholder constants with per-site values. */

#define PP_COUNTDOWN_OFFSET 0x0defac01    /* disp32 fields off %r14 */
#define PP_DOMAIN_SLOT 0x0defac02
#define PP_STACK_TOP_SLOT 0x0defac03
#define PP_SAVED_RSP_SLOT 0x0defac04
#define PP_SITE_INDEX 0x0defac05
#define PP_SLOW_STUB 0x0defac06           /* rel32 jump targets */
#define PP_RESUME 0x0defac07
#define PP_TAKEN_TARGET 0x0defac08
#define PP_FALLTHROUGH 0x0defac09

#define PP_STR_(x) #x
#define PP_STR(x) PP_STR_(x)

/* Every patched field is followed by a label, so that [stub_field] can
   address it by its exact offset within the template. */
#define PP_LABEL(name)                                                \
  ".globl caml_patchprof_" #name "\n"                                  \
  ".hidden caml_patchprof_" #name "\n"                                 \
  "caml_patchprof_" #name ":\n"

extern const unsigned char
  caml_patchprof_fast_template[],
  caml_patchprof_after_fast_countdown[],
  caml_patchprof_after_fast_jz[],
  caml_patchprof_fast_template_end[],
  caml_patchprof_jcc_template[],
  caml_patchprof_after_jcc_taken[],
  caml_patchprof_after_jcc_fallthrough[],
  caml_patchprof_jcc_template_end[],
  caml_patchprof_slow_template[],
  caml_patchprof_after_save_rsp[],
  caml_patchprof_after_load_stack_top[],
  caml_patchprof_after_setcc[],
  caml_patchprof_after_load_domain[],
  caml_patchprof_after_load_site_index[],
  caml_patchprof_after_restore_rsp[],
  caml_patchprof_after_jmp_resume[],
  caml_patchprof_slow_template_end[];

__asm__ (
  ".pushsection .rodata.caml_patchprof_templates, \"a\", @progbits\n"

  /* Fast stub: sample countdown, followed by the cloned flag writer and the
     [jcc] template.  A jump to the patched site lands here. */
  /* Jumps that leave a template are written as
     [j.. 1f + <placeholder>], with [1:] placed directly after the
     instruction: the encoded rel32 field is then exactly the placeholder,
     whose large value also forces the long encoding.  [stub_field] verifies
     both.  (A local label is used because branching to a global symbol would
     emit a relocation instead of the placeholder.) */

  PP_LABEL(fast_template)
  "  decl " PP_STR(PP_COUNTDOWN_OFFSET) "(%r14)\n"
  PP_LABEL(after_fast_countdown)
  "  jz 1f + " PP_STR(PP_SLOW_STUB) "\n"
  "1:\n"
  PP_LABEL(after_fast_jz)
  PP_LABEL(fast_template_end)

  /* Rest of the fast stub, after the cloned flag writer: a copy of the
     original conditional branch (the condition nibble of the [jo] opcode is
     patched), then a jump to the original fall-through address. */
  PP_LABEL(jcc_template)
  /* [jo] is the base conditional jump; its condition nibble is patched. */
  "  jo 1f + " PP_STR(PP_TAKEN_TARGET) "\n"
  "1:\n"
  PP_LABEL(after_jcc_taken)
  "  jmp 1f + " PP_STR(PP_FALLTHROUGH) "\n"
  "1:\n"
  PP_LABEL(after_jcc_fallthrough)
  PP_LABEL(jcc_template_end)

  /* Slow stub, entered when the countdown expires; the cloned flag writer is
     copied in front of it.  Captures the condition, calls
     [caml_patchprof_sample] to update the counters, then re-runs the cloned
     flag writer in the fast stub.  It must not touch the OCaml stack (OCaml
     code may use memory below %rsp), so it switches to the per-domain stub
     stack right away and saves scratch registers there; only %rsp itself
     needs a domain-state slot.  MOV and PUSH do not alter the flags, which
     stay live until the SETO.  [caml_patchprof_sample] preserves all other
     registers, see its attributes. */
  PP_LABEL(slow_template)
  "  movq %rsp, " PP_STR(PP_SAVED_RSP_SLOT) "(%r14)\n"
  PP_LABEL(after_save_rsp)
  "  movq " PP_STR(PP_STACK_TOP_SLOT) "(%r14), %rsp\n"
  PP_LABEL(after_load_stack_top)
  "  pushq %rsi\n"
  "  movl $0, %esi\n"                        /* MOV does not alter flags */
  "  seto %sil\n"                            /* condition nibble patched */
  PP_LABEL(after_setcc)
  "  pushq %rdi\n"
  "  pushq %rdx\n"
  "  pushq %r11\n"
  "  movq " PP_STR(PP_DOMAIN_SLOT) "(%r14), %rdi\n"
  PP_LABEL(after_load_domain)
  "  movl $" PP_STR(PP_SITE_INDEX) ", %edx\n"
  PP_LABEL(after_load_site_index)
  /* The linker resolves the absolute address; unlike a direct call's
     rel32, it stays correct when the template is copied. */
  "  movabsq $caml_patchprof_sample, %r11\n"
  "  call *%r11\n"
  "  popq %r11\n"
  "  popq %rdx\n"
  "  popq %rdi\n"
  "  popq %rsi\n"
  "  movq " PP_STR(PP_SAVED_RSP_SLOT) "(%r14), %rsp\n"
  PP_LABEL(after_restore_rsp)
  /* Patched to jump to the cloned flag writer in the fast stub. */
  "  jmp 1f + " PP_STR(PP_RESUME) "\n"
  "1:\n"
  PP_LABEL(after_jmp_resume)
  PP_LABEL(slow_template_end)

  ".popsection\n"
);

/* All patched fields are the last 4 bytes in front of their [PP_LABEL], so
   the labels pin down each field's exact offset within its template; no
   scanning is involved.  The bytes being replaced are additionally verified
   to be exactly the expected placeholder, so a template change that moves or
   re-encodes a field crashes instead of corrupting a stub.  Template
   inconsistencies are build bugs, not runtime conditions. */

static unsigned char *stub_field(unsigned char *stub_begin,
                                 const unsigned char *template_begin,
                                 const unsigned char *after_field,
                                 uint32_t placeholder)
{
  unsigned char *field =
    stub_begin + (after_field - template_begin) - sizeof placeholder;
  uint32_t found;
  memcpy(&found, field, sizeof found);
  if (found != placeholder)
    caml_fatal_error("patchprof: unexpected bytes at stub template field");
  return field;
}

static void patch_u32(unsigned char *stub_begin,
                      const unsigned char *template_begin,
                      const unsigned char *after_field,
                      uint32_t placeholder, uint32_t value)
{
  unsigned char *field =
    stub_field(stub_begin, template_begin, after_field, placeholder);
  memcpy(field, &value, sizeof value);
}

static void write_rel32(unsigned char *field, const unsigned char *target)
{
  int64_t displacement =
    (int64_t)(uintptr_t)target
    - (int64_t)((uintptr_t)field + sizeof(int32_t));
  if (displacement < INT32_MIN || displacement > INT32_MAX)
    caml_fatal_error("patchprof: stub jump displacement overflow");
  int32_t rel32 = (int32_t)displacement;
  memcpy(field, &rel32, sizeof rel32);
}

static void patch_rel32(unsigned char *stub_begin,
                        const unsigned char *template_begin,
                        const unsigned char *after_field,
                        uint32_t placeholder, const unsigned char *target)
{
  write_rel32(stub_field(stub_begin, template_begin, after_field, placeholder),
              target);
}

static void patch_setcc_condition(unsigned char *stub_begin,
                                  const unsigned char *template_begin,
                                  const unsigned char *after_setcc,
                                  unsigned condition)
{
  static const unsigned char seto_sil[] = { 0x40, 0x0f, 0x90, 0xc6 };
  unsigned char *instruction =
    stub_begin + (after_setcc - template_begin) - sizeof seto_sil;
  if (memcmp(instruction, seto_sil, sizeof seto_sil) != 0)
    caml_fatal_error("patchprof: unexpected bytes at stub template field");
  instruction[2] |= condition;
}

static int decode_site(const struct caml_patchprof_site *site,
                       unsigned *condition,
                       unsigned char **target,
                       unsigned char **fallthrough_address)
{
  unsigned char *jcc = site->address + site->flag_writer_length;
  if (site->jcc_length == 2
      && jcc[0] >= 0x70 && jcc[0] <= 0x7f) {
    *condition = jcc[0] & 0x0f;
    *target = jcc + 2 + (int8_t)jcc[1];
  } else if (site->jcc_length == 6
             && jcc[0] == 0x0f
             && jcc[1] >= 0x80 && jcc[1] <= 0x8f) {
    int32_t displacement;
    memcpy(&displacement, jcc + 2, sizeof displacement);
    *condition = jcc[1] & 0x0f;
    *target = jcc + 6 + displacement;
  } else {
    return -1;
  }
  *fallthrough_address = jcc + site->jcc_length;
  return 0;
}

static int generate_stub(uint32_t index, unsigned char **slow_cursor)
{
  const struct caml_patchprof_site *site = &sites[index];
  unsigned condition;
  unsigned char *target;
  unsigned char *fallthrough_address;
  if (decode_site(site, &condition, &target, &fallthrough_address) != 0)
    return -1;

  unsigned char *fast_start = stub_arena_begin + 64 * index;
  struct emitter fast = { fast_start, fast_start + 64 };
  struct emitter slow = { *slow_cursor, stub_arena_end };
  unsigned char *slow_start = slow.cursor;

  uint32_t countdown_offset =
    (uint32_t)(sizeof(caml_domain_state) + sizeof(uint32_t) * index);
#define PP_SLOT_OFFSET(slot)                                          \
  ((uint32_t)(sizeof(caml_domain_state)                                \
              + sizeof(uint32_t) * CAML_PATCHPROF_NUM_SITES            \
              + sizeof(uint64_t) * (slot)))

  /* Fast stub: the countdown template, then the cloned flag writer, then the
     [jcc] template redirected to the original successors. */
  if (emit_bytes(&fast, caml_patchprof_fast_template,
                 (size_t)(caml_patchprof_fast_template_end
                          - caml_patchprof_fast_template)))
    return -1;
  patch_u32(fast_start, caml_patchprof_fast_template,
            caml_patchprof_after_fast_countdown,
            PP_COUNTDOWN_OFFSET, countdown_offset);
  patch_rel32(fast_start, caml_patchprof_fast_template,
              caml_patchprof_after_fast_jz, PP_SLOW_STUB, slow_start);
  unsigned char *resume = fast.cursor;
  if (emit_bytes(&fast, site->address, site->flag_writer_length))
    return -1;

  unsigned char *jcc_begin = fast.cursor;
  if (emit_bytes(&fast, caml_patchprof_jcc_template,
                 (size_t)(caml_patchprof_jcc_template_end
                          - caml_patchprof_jcc_template)))
    return -1;
  unsigned char *taken_field =
    stub_field(jcc_begin, caml_patchprof_jcc_template,
               caml_patchprof_after_jcc_taken, PP_TAKEN_TARGET);
  if (taken_field[-1] != 0x80)
    caml_fatal_error("patchprof: malformed jcc template");
  taken_field[-1] |= condition;
  write_rel32(taken_field, target);
  patch_rel32(jcc_begin, caml_patchprof_jcc_template,
              caml_patchprof_after_jcc_fallthrough,
              PP_FALLTHROUGH, fallthrough_address);

  /* Slow stub: the cloned flag writer, then the counter-update template. */
  if (emit_bytes(&slow, site->address, site->flag_writer_length))
    return -1;
  unsigned char *slow_template_begin = slow.cursor;
  if (emit_bytes(&slow, caml_patchprof_slow_template,
                 (size_t)(caml_patchprof_slow_template_end
                          - caml_patchprof_slow_template)))
    return -1;
#define PP_PATCH_U32(after_field, placeholder, value)                 \
  patch_u32(slow_template_begin, caml_patchprof_slow_template,         \
            caml_patchprof_##after_field, (placeholder), (value))
  patch_setcc_condition(slow_template_begin, caml_patchprof_slow_template,
                        caml_patchprof_after_setcc, condition);
  PP_PATCH_U32(after_save_rsp, PP_SAVED_RSP_SLOT,
               PP_SLOT_OFFSET(CAML_PATCHPROF_SLOT_SAVED_RSP));
  PP_PATCH_U32(after_restore_rsp, PP_SAVED_RSP_SLOT,
               PP_SLOT_OFFSET(CAML_PATCHPROF_SLOT_SAVED_RSP));
  PP_PATCH_U32(after_load_stack_top, PP_STACK_TOP_SLOT,
               PP_SLOT_OFFSET(CAML_PATCHPROF_SLOT_STACK_TOP));
  PP_PATCH_U32(after_load_domain, PP_DOMAIN_SLOT,
               PP_SLOT_OFFSET(CAML_PATCHPROF_SLOT_DOMAIN));
  PP_PATCH_U32(after_load_site_index, PP_SITE_INDEX, index);
#undef PP_PATCH_U32
  patch_rel32(slow_template_begin, caml_patchprof_slow_template,
              caml_patchprof_after_jmp_resume, PP_RESUME, resume);
#undef PP_SLOT_OFFSET

  *slow_cursor = slow.cursor;
  return 0;
}

#ifndef MADV_POPULATE_WRITE
#define MADV_POPULATE_WRITE 23
#endif

#define PP_HUGE_PAGE ((uintptr_t)2 * 1024 * 1024)

struct exec_segment {
  uintptr_t site;  /* in: an address the segment must contain */
  uintptr_t begin;
  uintptr_t end;
  uint64_t file_offset;  /* of [begin] in the executable image */
};

static int find_exec_segment(struct dl_phdr_info *info, size_t size,
                             void *data)
{
  struct exec_segment *segment = data;
  (void)size;
  if (info->dlpi_name != NULL && info->dlpi_name[0] != '\0') return 0;
  for (int i = 0; i < info->dlpi_phnum; i++) {
    const ElfW(Phdr) *phdr = &info->dlpi_phdr[i];
    if (phdr->p_type != PT_LOAD || (phdr->p_flags & PF_X) == 0) continue;
    uintptr_t begin = (uintptr_t)info->dlpi_addr + (uintptr_t)phdr->p_vaddr;
    uintptr_t end = begin + (uintptr_t)phdr->p_memsz;
    if (segment->site >= begin && segment->site < end) {
      segment->begin = begin;
      segment->end = end;
      segment->file_offset = (uint64_t)phdr->p_offset;
      return 1;
    }
  }
  return 0;
}

/* Patch the sites in an anonymous copy of the text range containing all of
   them (the hull) and swap the copy in with one mremap, instead of writing
   to the live file-backed mapping: breaking copy-on-write costs about 2 us
   per 4 KiB page in kernel bookkeeping, while a transparent-huge-page copy
   populates and copies at about 0.6 us per page and reduces iTLB pressure
   on the instrumented text afterwards.  The hull is rounded out to the
   2 MiB alignment that huge pages need, clamped to the executable segment;
   the copy costs its rounded size in resident memory per process.
   [sites] is sorted by address. */
static int patch_sites_via_hull_copy(void)
{
  long page_size = sysconf(_SC_PAGESIZE);
  if (page_size <= 0 || num_sites == 0) return -1;
  struct exec_segment segment = { (uintptr_t)sites[0].address, 0, 0 };
  if (dl_iterate_phdr(find_exec_segment, &segment) != 1) return -1;
  const struct caml_patchprof_site *last_site = &sites[num_sites - 1];
  uintptr_t sites_end =
    (uintptr_t)last_site->address
    + last_site->flag_writer_length + last_site->jcc_length;
  if (sites_end > segment.end) return -1;

  /* Round the hull out to 2 MiB boundaries for the huge pages, clamped to
     the executable segment; a clamped edge merely falls back to 4 KiB
     pages there.  In the huge binaries where huge pages matter most, the
     hull sits comfortably inside the segment and both edges stay
     aligned. */
  uintptr_t page_mask = ~((uintptr_t)page_size - 1);
  uintptr_t lo = (uintptr_t)sites[0].address & ~(PP_HUGE_PAGE - 1);
  uintptr_t hi = (sites_end + PP_HUGE_PAGE - 1) & ~(PP_HUGE_PAGE - 1);
  uintptr_t segment_first_page = segment.begin & page_mask;
  uintptr_t segment_last_page =
    (segment.end + page_size - 1) & page_mask;
  if (lo < segment_first_page) lo = segment_first_page;
  if (hi > segment_last_page) hi = segment_last_page;
  size_t length = hi - lo;

  /* The copy is mapped executable so that the swap below never leaves the
     hull unexecutable: this function's own code may live in it.  Its bytes
     agree with the live text except at the patched sites, which nothing
     executes before [caml_patchprof_init] returns.

     The copy is placed congruent to [lo] modulo the huge page size, not
     merely aligned: mremap can only move huge pages between congruent
     addresses, and [lo] itself is not aligned when the executable's
     segments are not (the clamp above).  Every aligned 2 MiB stretch of
     the hull then keeps its huge page through the move; only the
     unaligned head and tail fall back to 4 KiB pages. */
  size_t reservation = length + 2 * PP_HUGE_PAGE;
  unsigned char *reserved =
    mmap(NULL, reservation, PROT_NONE,
         MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
  if (reserved == MAP_FAILED) return -1;
  unsigned char *copy =
    (unsigned char *)((((uintptr_t)reserved + PP_HUGE_PAGE - 1)
                       & ~(PP_HUGE_PAGE - 1))
                      + (lo & (PP_HUGE_PAGE - 1)));
  if (mmap(copy, length, PROT_READ | PROT_WRITE | PROT_EXEC,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED, -1, 0) == MAP_FAILED)
    goto fail;
  (void)madvise(copy, length, MADV_HUGEPAGE);
  /* Best effort: populating in one pass is a little cheaper than faulting
     during the memcpy, but some kernels lack it. */
  (void)madvise(copy, length, MADV_POPULATE_WRITE);
  memcpy(copy, (const void *)lo, length);

  for (uint32_t i = 0; i < num_sites; i++) {
    struct caml_patchprof_site *site = &sites[i];
    size_t pair_length = site->flag_writer_length + site->jcc_length;
    unsigned char patch[32];
    patch[0] = 0xe9;
    int64_t displacement =
      (int64_t)(uintptr_t)(stub_arena_begin + 64 * i)
      - (int64_t)((uintptr_t)site->address + 5);
    int32_t rel = (int32_t)displacement;
    memcpy(patch + 1, &rel, sizeof rel);
    memset(patch + 5, 0x90, pair_length - 5);
    memcpy(copy + ((uintptr_t)site->address - lo), patch, pair_length);
  }

  /* The move keeps the huge pages of the congruent aligned stretches.  On
     failure the live text is untouched. */
  if (mremap(copy, length, length, MREMAP_MAYMOVE | MREMAP_FIXED,
             (void *)lo) != (void *)lo)
    goto fail;
  (void)munmap(reserved, reservation);
  __builtin___clear_cache((char *)lo, (char *)hi);
  patched_hull_lo = lo;
  patched_hull_hi = hi;
  patched_hull_file_offset =
    segment.file_offset + (lo - segment.begin);
  return mprotect((void *)lo, length, PROT_READ | PROT_EXEC);

fail:
  (void)munmap(reserved, reservation);
  return -1;
}

static int install_stubs(void)
{
  size_t arena_size =
    (size_t)((uintptr_t)stub_arena_end
             - (uintptr_t)stub_arena_begin);
  if (arena_size < 2 * 1024 * 1024
      || mprotect(stub_arena_begin, arena_size,
                  PROT_READ | PROT_WRITE | PROT_EXEC) != 0)
    return -1;

  unsigned char *slow_cursor =
    stub_arena_begin + 64 * CAML_PATCHPROF_NUM_SITES;
  for (uint32_t i = 0; i < num_sites; i++) {
    if (generate_stub(i, &slow_cursor) != 0) return -1;
  }
  for (uint32_t i = 0; i < num_sites; i++) {
    size_t pair_length =
      sites[i].flag_writer_length + sites[i].jcc_length;
    int64_t displacement =
      (int64_t)(uintptr_t)(stub_arena_begin + 64 * i)
      - (int64_t)((uintptr_t)sites[i].address + 5);
    if (pair_length < 5 || pair_length > 32
        || displacement < INT32_MIN || displacement > INT32_MAX)
      return -1;
  }
  if (patch_sites_via_hull_copy() != 0) return -1;
  __builtin___clear_cache((char *)stub_arena_begin,
                          (char *)slow_cursor);
  /* Nothing writes to the arena after this point: the stubs keep all their
     state in the domain state and the per-domain structure. */
  (void)mprotect(stub_arena_begin, arena_size, PROT_READ | PROT_EXEC);
  return 0;
}

/* Called whenever a domain starts running on [state], before any OCaml code
   runs on it. */
void caml_patchprof_init_domain(caml_domain_state *state)
{
  uint64_t *slots = caml_patchprof_slots(state);
  struct caml_patchprof_domain *domain =
    (struct caml_patchprof_domain *)(uintptr_t)
      slots[CAML_PATCHPROF_SLOT_DOMAIN];
  if (domain == NULL) {
    /* Never freed, like the domain state it extends. */
    domain = caml_stat_calloc_noexc(1, sizeof *domain);
    if (domain == NULL)
      caml_fatal_error("patchprof: out of memory");
    domain->countdowns = caml_patchprof_countdowns(state);
    domain->state = state;
    domain->load_bias = load_bias;
    domain->sites = sites;
    domain->frame_descrs = caml_get_frame_descrs();
    slots[CAML_PATCHPROF_SLOT_DOMAIN] = (uint64_t)(uintptr_t)domain;
    /* The stack the slow stub calls C on; also used by any signal handler
       that interrupts the sampling call.  A PROT_NONE guard page at the low
       end turns an overflow into a segfault instead of silent corruption. */
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0)
      caml_fatal_error("patchprof: could not determine the page size");
    unsigned char *stub_stack =
      mmap(NULL, (size_t)page_size + CAML_PATCHPROF_STUB_STACK_BYTES,
           PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (stub_stack == MAP_FAILED
        || mprotect(stub_stack, (size_t)page_size, PROT_NONE) != 0)
      caml_fatal_error("patchprof: could not allocate a stub stack");
    /* mmap is page-aligned, so the slow stub's four pushes plus the call's
       return address leave %rsp 16-aligned in [caml_patchprof_sample], as
       the ABI requires. */
    slots[CAML_PATCHPROF_SLOT_STACK_TOP] =
      (uint64_t)(uintptr_t)(stub_stack + page_size
                            + CAML_PATCHPROF_STUB_STACK_BYTES);
    /* The raw walk log, dumped with the profile for offline processing. */
    uint64_t *walk_log =
      mmap(NULL, CAML_PATCHPROF_WALK_LOG_BYTES, PROT_READ | PROT_WRITE,
           MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
    if (walk_log == MAP_FAILED)
      caml_fatal_error("patchprof: could not allocate the walk log");
    domain->walk_log = walk_log;
    domain->walk_log_capacity =
      CAML_PATCHPROF_WALK_LOG_BYTES / sizeof(uint64_t);
  }
  for (int i = 0; i < CAML_PATCHPROF_NUM_SITES; i++) {
    domain->countdowns[i] = (uint32_t)initial_countdown;
    domain->totals[i] =
      CAML_PATCHPROF_BACKOFF_DENOMINATOR * initial_countdown;
    domain->tallies[i] = 0;
    domain->slow_path_entries[i] = 0;
  }
  domain->walks_attempted = 0;
  domain->walks_failed = 0;
  domain->walk_frames_total = 0;
  domain->last_walk_length = 0;
  domain->walk_log_used = CAML_PATCHPROF_WALK_CHUNK_HEADER_WORDS;
  domain->walks_dropped = 0;
  domain->window_start_ns = monotonic_ns();
}

/* Flush the domain's pending walk chunk from a normal (non-slow-stub)
   context; the slow path has its own raw-syscall flush. */
static void flush_walk_log(struct caml_patchprof_domain *domain)
{
  size_t used = domain->walk_log_used;
  domain->walk_log_used = CAML_PATCHPROF_WALK_CHUNK_HEADER_WORDS;
  if (used <= CAML_PATCHPROF_WALK_CHUNK_HEADER_WORDS || profile_fd < 0)
    return;
  domain->walk_log[0] = CAML_PATCHPROF_RECORD_WALKS;
  domain->walk_log[1] = used - 2;
  domain->walk_log[2] = (uint64_t)domain->state->id;
  pthread_mutex_lock(&output_mutex);
  (void)write_all(profile_fd, domain->walk_log, used * sizeof(uint64_t));
  pthread_mutex_unlock(&output_mutex);
}

void caml_patchprof_dump_domain(caml_domain_state *state)
{
  if (dump_disabled || profile_fd < 0 || num_sites == 0) return;
  struct caml_patchprof_domain *domain =
    (struct caml_patchprof_domain *)(uintptr_t)
      caml_patchprof_slots(state)[CAML_PATCHPROF_SLOT_DOMAIN];
  if (domain == NULL) return;

  flush_walk_log(domain);

  uint64_t initial = initial_countdown;
  uint64_t initial_total = CAML_PATCHPROF_BACKOFF_DENOMINATOR * initial;
  const uint32_t *countdowns = caml_patchprof_countdowns(state);
  const uint64_t *totals = domain->totals;
  const uint64_t *tallies = domain->tallies;
  const uint64_t *slow_path_entries = domain->slow_path_entries;

  uint64_t *payload =
    malloc((3 + 5 * (size_t)num_sites) * sizeof(uint64_t));
  if (payload == NULL) return;
  size_t words = 3;
  uint64_t emitted = 0;
  for (uint32_t i = 0; i < num_sites; i++) {
    /* [total] exceeds its initial value by the sampling periods reloaded so
       far, i.e. by the executions consumed at expired periods except for the
       very first one; adding the initial period and subtracting what remains
       of the current period yields the exact execution count.  The taken
       bias is [tally] / [sampled_weight], which advance in the same
       per-sample increments; both are recorded raw. */
    uint64_t sampled_weight = totals[i] - initial_total;
    uint64_t executions = sampled_weight + initial - countdowns[i];
    if (executions == 0) continue;
    payload[words++] = (uint64_t)(uintptr_t)sites[i].address - load_bias;
    payload[words++] = executions;
    payload[words++] = slow_path_entries[i];
    payload[words++] = sampled_weight;
    payload[words++] = tallies[i];
    emitted++;
  }
  payload[0] = (uint64_t)state->id;
  payload[1] = emitted;
  payload[2] = monotonic_ns() - domain->window_start_ns;
  int write_result = write_record(CAML_PATCHPROF_RECORD_COUNTERS,
                                  payload, words);
  free(payload);

  uint64_t stats[5] = {
    (uint64_t)state->id,
    domain->walks_attempted,
    domain->walks_failed,
    domain->walk_frames_total,
    domain->walks_dropped
  };
  if (write_result == 0)
    write_result = write_record(CAML_PATCHPROF_RECORD_STATS, stats, 5);

  if (write_result != 0) {
    pthread_mutex_lock(&output_mutex);
    close(profile_fd);
    profile_fd = -1;
    pthread_mutex_unlock(&output_mutex);
  }
  if (write_result == 0 && state == initial_domain)
    initial_domain_dumped = 1;
}

static void dump_initial_domain(void)
{
  if (!initial_domain_dumped && initial_domain != NULL)
    caml_patchprof_dump_domain(initial_domain);
}

/* Append one record to the profile: [kind, payload_words, payload]. */
static int write_record(uint64_t kind, const uint64_t *payload, size_t words)
{
  int result = -1;
  size_t bytes = (2 + words) * sizeof(uint64_t);
  uint64_t *record = malloc(bytes);
  if (record == NULL) return -1;
  record[0] = kind;
  record[1] = words;
  memcpy(record + 2, payload, words * sizeof(uint64_t));
  pthread_mutex_lock(&output_mutex);
  result = write_all(profile_fd, record, bytes);
  pthread_mutex_unlock(&output_mutex);
  free(record);
  return result;
}

/* Together with the [patchprof_sites] metadata of the executable, the
   selection parameters identify the instrumented subset: sort the recorded
   sites by address, deduplicate, and take every [stride]-th one starting
   at index [window_start] + [residue], staying within the window of
   [stride] * CAML_PATCHPROF_NUM_SITES sites.  All addresses have the load
   bias subtracted, i.e. they match the executable file.  Emitted once at
   startup and again at each window rotation. */
static int emit_selection_record(void)
{
  struct timespec wall;
  clock_gettime(CLOCK_REALTIME, &wall);
  uint64_t payload[9] = {
    selection_seed,
    selection_stride,
    initial_countdown,
    selection_num_unique,
    selection_window_start,
    selection_residue,
    num_sites,
    monotonic_ns(),
    (uint64_t)wall.tv_sec * 1000000000ull + (uint64_t)wall.tv_nsec
  };
  return write_record(CAML_PATCHPROF_RECORD_SELECTION, payload, 9);
}

/* When [OCAML_PATCHPROF_OUT] names a directory, emit into a freshly
   created, uniquely named file inside it, so that many instrumented
   processes can run concurrently with the same environment.  [O_EXCL]
   guarantees uniqueness; the name additionally carries the executable's
   base name and the pid to be informative. */
static int open_profile(const char *output)
{
  struct stat st;
  if (stat(output, &st) != 0 || !S_ISDIR(st.st_mode))
    return open(output,
                O_WRONLY | O_CREAT | O_TRUNC | O_CLOEXEC | O_NOFOLLOW
                  | O_NONBLOCK,
                0600);
  char exe[PATH_MAX];
  ssize_t exe_len = readlink("/proc/self/exe", exe, sizeof exe - 1);
  const char *base = "unknown";
  if (exe_len > 0) {
    exe[exe_len] = '\0';
    base = strrchr(exe, '/');
    base = base == NULL ? exe : base + 1;
  }
  for (int attempt = 0; attempt < 100; attempt++) {
    uint64_t z;
    if (getrandom(&z, sizeof z, 0) != sizeof z) return -1;
    char path[PATH_MAX];
    if (snprintf(path, sizeof path, "%s/%s.%ld.%016llx.patchprof", output,
                 base, (long)getpid(), (unsigned long long)z)
        >= (int)sizeof path)
      return -1;
    int fd = open(path,
                  O_WRONLY | O_CREAT | O_EXCL | O_CLOEXEC | O_NOFOLLOW
                    | O_NONBLOCK,
                  0600);
    if (fd >= 0 || errno != EEXIST) return fd;
  }
  return -1;
}

void caml_patchprof_init(void)
{
  if (!caml_patchprof_stub_arena_present) return;
  const char *output =
    (const char *)caml_secure_getenv(T("OCAML_PATCHPROF_OUT"));
  if (output == NULL || output[0] == '\0') return;
  uint32_t stride;
  if (read_initial_countdown(&initial_countdown) != 0)
    caml_fatal_error(
      "patchprof: OCAML_PATCHPROF_N0 must be a positive integer "
      "of at most 4294967295");
  if (read_site_stride(&stride) != 0)
    caml_fatal_error(
      "patchprof: OCAML_PATCHPROF_D must be a positive integer");
  dump_disabled = caml_secure_getenv(T("OCAML_PATCHPROF_NO_DUMP")) != NULL;
  if (read_seed() != 0)
    caml_fatal_error(
      "patchprof: OCAML_PATCHPROF_SEED must be an unsigned integer");
  if (read_rotation_period() != 0)
    caml_fatal_error(
      "patchprof: OCAML_PATCHPROF_ROTATE_MS must be an unsigned integer");
  if (load_sites_from_executable(stride) != 0)
    caml_fatal_error("patchprof: could not select instrumentation sites");
  /* Re-initialize the initial domain: it was created before this function
     read N0 and selected the sites. */
  caml_patchprof_init_domain(Caml_state);

  profile_fd = open_profile(output);
  struct stat output_stat;
  if (profile_fd < 0
      || fstat(profile_fd, &output_stat) != 0
      || !S_ISREG(output_stat.st_mode)
      || fchmod(profile_fd, 0600) != 0)
    caml_fatal_error("patchprof: could not open %s: %s",
                     output, strerror(errno));
  uint64_t magic = CAML_PATCHPROF_MAGIC;
  if (write_all(profile_fd, &magic, sizeof magic) != 0)
    caml_fatal_error("patchprof: could not write the profile magic");
  if (emit_selection_record() != 0 || install_stubs() != 0)
    caml_fatal_error("patchprof: could not install instrumentation");
  last_rotation_ns = monotonic_ns();
  initial_domain = Caml_state;
  (void)atexit(dump_initial_domain);
}

int caml_patchprof_rotation_enabled(void)
{
  return rotate_period_ns != 0 && profile_fd >= 0;
}

/* Restore the executable's original bytes over the currently patched text
   by putting the file mapping back; this also releases the private copies
   of the hull. */
static int restore_original_hull(void)
{
  if (patched_hull_lo == 0) return -1;
  int fd = open("/proc/self/exe", O_RDONLY | O_CLOEXEC);
  if (fd < 0) return -1;
  void *restored =
    mmap((void *)patched_hull_lo, patched_hull_hi - patched_hull_lo,
         PROT_READ | PROT_EXEC, MAP_PRIVATE | MAP_FIXED, fd,
         (off_t)patched_hull_file_offset);
  close(fd);
  return restored == (void *)patched_hull_lo ? 0 : -1;
}

/* Move the instrumentation to the next window of the seeded trajectory.
   The caller must hold every domain at a stop-the-world barrier: no OCaml
   code runs anywhere while the text is rewritten, and blocked threads can
   only ever resume at instruction boundaries that are boundaries in both
   the patched and the unpatched text. */
void caml_patchprof_rotate_from_stw(int participating_count,
                                    caml_domain_state **participating)
{
  if (!caml_patchprof_rotation_enabled()) return;
  uint64_t now = monotonic_ns();
  if (now - last_rotation_ns < rotate_period_ns) return;
  last_rotation_ns = now;

  /* The counters describe the outgoing window: flush them first. */
  for (int i = 0; i < participating_count; i++)
    caml_patchprof_dump_domain(participating[i]);

  num_sites = 0;
  if (restore_original_hull() != 0
      || load_sites_from_executable(selection_stride) != 0
      || emit_selection_record() != 0
      || install_stubs() != 0)
    caml_fatal_error("patchprof: could not rotate the instrumented window");

  for (int i = 0; i < participating_count; i++)
    caml_patchprof_init_domain(participating[i]);
  /* The initial domain has a new batch to dump at exit. */
  initial_domain_dumped = 0;
}
