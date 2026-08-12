(* TEST
 arch_amd64;
 linux;
 ocamlopt_flags = "-patchprof";
 set OCAML_PATCHPROF_OUT = "patchprof.profile";
 set OCAML_PATCHPROF_D = "2";
 set OCAML_PATCHPROF_N0 = "17";
 set OCAML_PATCHPROF_SEED = "1";
 native;
 script = "sh ${test_source_directory}/check-patchprof-profile.sh ${test_source_directory}";
 script;
*)

let[@inline never] classify x =
  if x < 10 then 0 else if x = 42 then 1 else 2

let rec loop n sum =
  if n = 0 then sum else loop (n - 1) (sum + classify (n mod 100))

let () =
  assert (classify 3 = 0);
  assert (classify 42 = 1);
  assert (classify 100 = 2);
  assert (loop 100_000 0 = 179_000)
