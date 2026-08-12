#!/bin/sh

# Checks the binary profile written by the patchprof_rotate.ml test.  Run
# by ocamltest, which executes the [script] command without a shell, hence
# this file.  $1 is the test source directory, four levels below the
# repository root.  The test rotates the instrumented window every
# millisecond from minor-GC stop-the-world sections, so the summary must
# report several distinct selections, and sampling must have worked across
# rotations.

# ocamltest keeps the test's environment for script actions, and the
# summary tool is itself patchprof-instrumented: without this it would
# truncate and overwrite the very profile it is asked to read.
unset OCAML_PATCHPROF_OUT OCAML_PATCHPROF_SEED OCAML_PATCHPROF_D \
      OCAML_PATCHPROF_N0 OCAML_PATCHPROF_ROTATE_MS

root=$(cd "$1/../../../.." && pwd)
summary="${root}/_build/main/tools/patchprof_summary.exe"

if "${summary}" -exe ./patchprof_rotate.opt patchprof_rotate.profile \
     > summary.out 2>&1 \
   && grep -Eq "[1-9][0-9]* walks \([1-9]" summary.out \
   && grep -Eq ", ([3-9]|[1-9][0-9]+) selection records" summary.out
then
  exit ${TEST_PASS}
else
  echo "unexpected patchprof_rotate.profile summary:" > "$ocamltest_response"
  cat summary.out >> "$ocamltest_response"
  exit ${TEST_FAIL}
fi
