(* TEST
 arch_amd64;
 linux;
 ocamlopt_flags = "-patchprof";
 set OCAML_PATCHPROF_OUT = "patchprof_rotate.profile";
 set OCAML_PATCHPROF_D = "2";
 set OCAML_PATCHPROF_N0 = "17";
 set OCAML_PATCHPROF_SEED = "1";
 set OCAML_PATCHPROF_ROTATE_MS = "1";
 native;
 script = "sh ${test_source_directory}/check-patchprof-rotate.sh ${test_source_directory}";
 script;
*)

(* Window rotation triggers from minor-GC stop-the-world sections, so this
   test allocates to force minor collections while also executing enough
   conditional branches to be sampled under every rotated window. *)

let[@inline never] classify x =
  if x < 10 then 0 else if x = 42 then 1 else 2

let rec loop n sum =
  if n = 0 then sum else loop (n - 1) (sum + classify (n mod 100))

let () =
  let junk = ref [] in
  for _ = 1 to 200 do
    junk := [List.init 10_000 (fun i -> i)];
    assert (loop 10_000 0 = 17_900)
  done;
  assert (List.length !junk = 1)
