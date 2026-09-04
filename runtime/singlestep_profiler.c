/* A minimal in-process LBR emulator, for testing FDO profile collection in a
   hardware-independent way.

   When OCAML_SINGLESTEP_PROFILE=<file> is set, the program is sampled at a
   fixed frequency (OCAML_SINGLESTEP_PROFILE_HZ, default 997, of CPU time,
   via ITIMER_PROF). Each sample turns on the x86 trap flag and single-steps
   the interrupted thread until 16 taken branches have been collected, then
   writes them as one line of "perf script -F brstack,period"-like output,
   most recent branch first:

     1 0x<from>/0x<to>/P/-/-/0 ...

   which oxcaml-fdo-decode consumes via -perf-script-output. Taken branches
   are recognized by decoding the previously executed instruction just enough
   to detect control transfers: conditional jumps have fixed lengths, so
   their fallthrough address is known, and calls, returns and unconditional
   jumps always transfer.

   Only intended for testing: taking two signals per executed instruction
   while collecting is orders of magnitude slower than hardware LBR, and only
   the thread that receives the sampling signal is stepped. */

#include <stdbool.h>
#define _GNU_SOURCE
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <ucontext.h>
#include <unistd.h>

#define ENTRIES_PER_SAMPLE 16

/* Abort a collection that found too few branches (e.g. the thread went to
   sleep and CPU-time sampling stopped ticking anyway). */
#define MAX_STEPS_PER_SAMPLE 100000

#define TRAP_FLAG 0x100ll

static int out_fd = -1;
static volatile int collecting = 0;
static uint64_t prev_ip;
static int num_steps;
static int num_entries;
static struct {
  uint64_t source, target;
} entries[ENTRIES_PER_SAMPLE];

/* Whether the transition from [prev] (the previously executed instruction)
   to [cur] was a taken control transfer, by decoding just enough of the
   instruction at [prev]. Anything unrecognized is treated as sequential
   execution. */
static bool is_taken_branch(uint64_t prev, uint64_t cur)
{
  const uint8_t *p = (const uint8_t *)prev;
  uint8_t b0 = p[0];
  /* jcc rel8 */
  if (b0 >= 0x70 && b0 <= 0x7f) return cur != prev + 2;
  /* jcc rel32 */
  if (b0 == 0x0f && p[1] >= 0x80 && p[1] <= 0x8f) return cur != prev + 6;
  /* loop*, jrcxz */
  if (b0 >= 0xe0 && b0 <= 0xe3) return cur != prev + 2;
  /* call/jmp rel */
  if (b0 == 0xe8 || b0 == 0xe9 || b0 == 0xeb) return true;
  /* ret */
  if (b0 == 0xc2 || b0 == 0xc3) return true;
  /* REX prefix */
  if (b0 >= 0x40 && b0 <= 0x4f) b0 = *++p;
  /* call/jmp *r/m */
  if (b0 == 0xff) {
    uint8_t reg = (p[1] >> 3) & 7;
    return reg == 2 || reg == 3 || reg == 4 || reg == 5;
  }
  return false;
}

static char *append_hex(char *pos, uint64_t v)
{
  char digits[16];
  int n = 0;
  *pos++ = '0';
  *pos++ = 'x';
  if (v == 0) {
    *pos++ = '0';
    return pos;
  }
  while (v != 0) {
    digits[n++] = "0123456789abcdef"[v & 0xf];
    v >>= 4;
  }
  while (n > 0) *pos++ = digits[--n];
  return pos;
}

/* Async-signal-safe: hand-rolled formatting and a plain write. */
static void dump_sample(void)
{
  char line[ENTRIES_PER_SAMPLE * 48 + 8];
  char *pos = line;
  int i;
  *pos++ = '1';
  for (i = num_entries - 1; i >= 0; i--) { /* most recent first */
    *pos++ = ' ';
    pos = append_hex(pos, entries[i].source);
    *pos++ = '/';
    pos = append_hex(pos, entries[i].target);
    memcpy(pos, "/P/-/-/0", 8);
    pos += 8;
  }
  *pos++ = '\n';
  if (write(out_fd, line, pos - line) < 0) { /* ignore */ }
}

static void profile_handler(int sig, siginfo_t *info, void *uctx)
{
  ucontext_t *uc = (ucontext_t *)uctx;
  (void)sig;
  (void)info;
  if (out_fd < 0) return;
  /* If the interrupted context has SIGTRAP blocked (e.g. we interrupted
     another signal handler), stepping would raise a synchronous SIGTRAP
     while it is blocked, which the kernel escalates to a forced core dump. */
  if (sigismember(&uc->uc_sigmask, SIGTRAP)) return;
  /* Only one thread collects at a time. */
  if (!__sync_bool_compare_and_swap(&collecting, 0, 1)) return;
  prev_ip = 0;
  num_steps = 0;
  num_entries = 0;
  uc->uc_mcontext.gregs[REG_EFL] |= TRAP_FLAG;
}

static void trap_handler(int sig, siginfo_t *info, void *uctx)
{
  ucontext_t *uc = (ucontext_t *)uctx;
  uint64_t ip = (uint64_t)uc->uc_mcontext.gregs[REG_RIP];
  (void)sig;
  (void)info;
  if (!collecting) { /* a stray trap: stop stepping */
    uc->uc_mcontext.gregs[REG_EFL] &= ~TRAP_FLAG;
    return;
  }
  if (prev_ip != 0 && is_taken_branch(prev_ip, ip)) {
    entries[num_entries].source = prev_ip;
    entries[num_entries].target = ip;
    num_entries++;
  }
  prev_ip = ip;
  num_steps++;
  if (num_entries >= ENTRIES_PER_SAMPLE || num_steps >= MAX_STEPS_PER_SAMPLE) {
    if (num_entries >= ENTRIES_PER_SAMPLE) dump_sample();
    uc->uc_mcontext.gregs[REG_EFL] &= ~TRAP_FLAG;
    collecting = 0;
  }
}

static void stop_profiler(void)
{
  struct itimerval off;
  memset(&off, 0, sizeof(off));
  setitimer(ITIMER_PROF, &off, NULL);
  collecting = 0;
}

void caml_singlestep_profiler_init(void)
{
  const char *path = getenv("OCAML_SINGLESTEP_PROFILE");
  const char *hz_env;
  struct sigaction sa;
  struct itimerval it;
  long hz = 997;
  if (path == NULL || *path == 0) return;
  out_fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
  if (out_fd < 0) {
    fprintf(stderr,
            "[ocaml] OCAML_SINGLESTEP_PROFILE: cannot open %s; profiling "
            "disabled\n",
            path);
    return;
  }
  memset(&sa, 0, sizeof(sa));
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO | SA_RESTART | SA_ONSTACK;
  sa.sa_sigaction = trap_handler;
  sigaction(SIGTRAP, &sa, NULL);
  sa.sa_sigaction = profile_handler;
  sigaction(SIGPROF, &sa, NULL);
  hz_env = getenv("OCAML_SINGLESTEP_PROFILE_HZ");
  if (hz_env != NULL) {
    hz = atol(hz_env);
    if (hz <= 0 || hz > 100000) hz = 997;
  }
  it.it_interval.tv_sec = 0;
  it.it_interval.tv_usec = 1000000 / hz > 0 ? 1000000 / hz : 1;
  it.it_value = it.it_interval;
  setitimer(ITIMER_PROF, &it, NULL);
  atexit(stop_profiler);
}
