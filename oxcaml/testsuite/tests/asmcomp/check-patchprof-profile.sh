#!/bin/sh

# Checks the binary profile written by the patchprof.ml test by summarizing
# it with the offline tool.  Run by ocamltest, which executes the [script]
# command without a shell, hence this file.  This script lives in the test
# source directory, four levels below the repository root.

# ocamltest keeps the test's environment for script actions, and the
# summary tool is itself patchprof-instrumented: without this it would
# truncate and overwrite the very profile it is asked to read.
unset OCAML_PATCHPROF_OUT OCAML_PATCHPROF_SEED OCAML_PATCHPROF_D \
      OCAML_PATCHPROF_N0 OCAML_PATCHPROF_ROTATE_MS

root=$(cd "$(dirname "$0")/../../../.." && pwd)
summary="${root}/_build/main/tools/patchprof_summary.exe"
decode="${root}/_build/main/tools/fdo/oxcaml_fdo_decode.exe"

# The fixed point of profile-guided rebuilds: recompile the test with the
# decoded profile while keeping the instrumentation, run it, and collect a
# new profile.  This exercises FDO block layout and hot-function sections
# together with the patchprof metadata (whose site deltas must respect the
# section assignment).  Compiled in a subdirectory so the source file name,
# which the profile's positions refer to, stays "patchprof.ml".
check_fdo_rebuild() {
  ocamlopt="${root}/_install/bin/ocamlopt.opt"
  mkdir -p fdo_rebuild
  cp "$(dirname "$0")/patchprof.ml" fdo_rebuild/
  (cd fdo_rebuild \
   && "${ocamlopt}" -patchprof -g -fdo-profile ../patchprof.fdo \
        -o patchprof_fdo.opt patchprof.ml \
   && OCAML_PATCHPROF_OUT=refreshed.profile OCAML_PATCHPROF_SEED=2 \
      OCAML_PATCHPROF_D=2 OCAML_PATCHPROF_N0=17 ./patchprof_fdo.opt \
   && test -s refreshed.profile)
}

# End-to-end check of the FDO decoder's patchprof mode: turn the profile
# into a source-position profile and expect counts attributed to positions
# in the test source.  Symbolization needs llvm-symbolizer, which not every
# machine has; skip the check without it.
if command -v llvm-symbolizer > /dev/null 2>&1; then
  if ! { "${decode}" -patchprof patchprof.profile -binary ./patchprof.opt \
           -o patchprof.fdo -debug-map -quiet \
         && "${decode}" -dump patchprof.fdo > fdo.out 2>&1 \
         && grep -Eq "patchprof\.ml:[0-9]+:[0-9]+: [1-9]" fdo.out \
         && grep -Eq "\[label [0-9]+\.[0-9]+: [1-9]" fdo.out \
         && check_fdo_rebuild >> fdo.out 2>&1 ; }
  then
    echo "unexpected FDO decode of patchprof.profile:" > "$ocamltest_response"
    cat fdo.out >> "$ocamltest_response" 2> /dev/null
    exit ${TEST_FAIL}
  fi
fi

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
