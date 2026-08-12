#!/bin/sh

# Checks the binary profile written by the patchprof.ml test by summarizing
# it with the offline tool.  Run by ocamltest, which executes the [script]
# command without a shell, hence this file.  $1 is the test source
# directory, four levels below the repository root.

# ocamltest keeps the test's environment for script actions, and the
# summary tool is itself patchprof-instrumented: without this it would
# truncate and overwrite the very profile it is asked to read.
unset OCAML_PATCHPROF_OUT OCAML_PATCHPROF_SEED OCAML_PATCHPROF_D \
      OCAML_PATCHPROF_N0 OCAML_PATCHPROF_ROTATE_MS

root=$(cd "$1/../../../.." && pwd)
summary="${root}/_build/main/tools/patchprof_summary.exe"

if "${summary}" -exe ./patchprof.opt patchprof.profile > summary.out 2>&1 \
   && grep -Eq "[1-9][0-9]* walks \([1-9]" summary.out \
   && grep -q ", 1 selection records," summary.out \
   && grep -q "coverage at stride 2:" summary.out \
   && grep -Eq "total fast-path executions of instrumented sites: [1-9]" \
        summary.out
then
  exit ${TEST_PASS}
else
  echo "unexpected patchprof.profile summary:" > "$ocamltest_response"
  cat summary.out >> "$ocamltest_response"
  exit ${TEST_FAIL}
fi
