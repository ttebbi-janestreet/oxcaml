(* TEST
 readonly_files = "intrinsics.ml";
 setup-ocamlopt.opt-build-env;
 all_modules = "intrinsics.ml";
 compile_only = "true";
 ocamlopt.opt;

 only-default-codegen;
 flags = " -O3 -I ocamlopt.opt";
 flags += " -cfg-prologue-shrink-wrap";
 flags += " -x86-peephole-optimize";
 flags += " -regalloc-param SPLIT_AROUND_LOOPS:on";
 flags += " -regalloc-param AFFINITY:on -regalloc irc";
 flags += " -cfg-merge-blocks";
 expect.opt;
*)

open Intrinsics

(* The marker is a runtime no-op: it generates no machine code. *)
let no_code x =
  hot_path_to_here 10.;
  x + 1

[%%expect_asm X86_64{|
no_code:
  addq  $2, %rax
  ret
|}]

(* In [dispatch_hot_path] the [x < 10] branch reaches a [hot_path_to_here ()]
   marker, so it (and everything that can reach it) is hot: its block is laid out
   before the cold branches, whose [cold] calls are pushed after the hot
   code, and the hot path gets a larger inlining budget, so more of the [helper]
   chain is inlined than in the unmarked [dispatch_unmarked]. *)

module M = struct

(* A non-inlinable function, used to keep the cold branches as real calls so
   that hot-before-cold block layout is observable in the assembly. *)
let[@inline never] cold () = ()

let helper1 x= 3 * x
let helper2 x = helper1 ( helper1 (helper1 x)) * x + 3
let helper3 x = helper2 ( helper2 (helper2 x)) * x + 3
let helper4 x = helper3 ( helper3 (helper3 x)) * x + 3
let helper5 x = helper4 ( helper4 (helper4 x)) * x + 3

let dispatch_unmarked x =
  if x > 5 then cold ()
  else if x < 10 then (let _ : int = helper5 x in ())
  else cold ()

let dispatch_hot_path b x =
  if x > 5 then cold ()
  else if x < 10 then (let _ : int = helper5 x in hot_path_to_here 10.)
  else cold ()

end
[%%expect{|
module M :
  sig
    val cold : unit -> unit
    val helper1 : int -> int
    val helper2 : int -> int
    val helper3 : int -> int
    val helper4 : int -> int
    val helper5 : int -> int
    val dispatch_unmarked : int -> unit
    val dispatch_hot_path : 'a -> int -> unit
  end
|}]

[%%expect_asm X86_64{|
M.cold:
  movl  $1, %eax
  ret

M.helper1:
  leaq  -2(%rax,%rax,2), %rax
  ret

M.helper2:
  movq  %rax, %rbx
  sarq  $1, %rbx
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rbx, %rax
  addq  $7, %rax
  ret

M.helper3:
  movq  %rax, %rbx
  sarq  $1, %rbx
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rbx, %rax
  addq  $7, %rax
  movq  %rax, %rdi
  sarq  $1, %rdi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rdi, %rax
  addq  $7, %rax
  movq  %rax, %rdi
  sarq  $1, %rdi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rdi, %rax
  addq  $6, %rax
  imulq %rbx, %rax
  addq  $7, %rax
  ret

M.helper4:
  movq  %rax, %rbx
  sarq  $1, %rbx
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rbx, %rax
  addq  $7, %rax
  movq  %rax, %rdi
  sarq  $1, %rdi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rdi, %rax
  addq  $7, %rax
  movq  %rax, %rdi
  sarq  $1, %rdi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rdi, %rax
  addq  $6, %rax
  imulq %rbx, %rax
  addq  $7, %rax
  movq  %rax, %rdi
  sarq  $1, %rdi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rdi, %rax
  addq  $7, %rax
  movq  %rax, %rsi
  sarq  $1, %rsi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rsi, %rax
  addq  $7, %rax
  movq  %rax, %rsi
  sarq  $1, %rsi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rsi, %rax
  addq  $6, %rax
  imulq %rdi, %rax
  addq  $7, %rax
  movq  %rax, %rdi
  sarq  $1, %rdi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rdi, %rax
  addq  $7, %rax
  movq  %rax, %rsi
  sarq  $1, %rsi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rsi, %rax
  addq  $7, %rax
  movq  %rax, %rsi
  sarq  $1, %rsi
  leaq  (%rax,%rax,2), %rax
  imulq $9, %rax
  addq  $-27, %rax
  imulq %rsi, %rax
  addq  $6, %rax
  imulq %rdi, %rax
  addq  $6, %rax
  imulq %rbx, %rax
  addq  $7, %rax
  ret

M.helper5:
  subq  $8, %rsp
  movq  %rax, (%rsp)
  call  camlTOP3__helper4_6_14_code@PLT
.L0:
  call  camlTOP3__helper4_6_14_code@PLT
.L1:
  call  camlTOP3__helper4_6_14_code@PLT
.L2:
  movq  (%rsp), %rbx
  sarq  $1, %rbx
  decq  %rax
  imulq %rbx, %rax
  addq  $7, %rax
  addq  $8, %rsp
  ret

M.dispatch_unmarked:
  cmpq  $11, %rax
  jle   .L0
  movl  $1, %eax
  jmp   camlTOP3__cold_2_10_code@PLT
.L0:
  cmpq  $21, %rax
  jge   .L4
  subq  $8, %rsp
  call  camlTOP3__helper4_6_14_code@PLT
.L1:
  call  camlTOP3__helper4_6_14_code@PLT
.L2:
  call  camlTOP3__helper4_6_14_code@PLT
.L3:
  movl  $1, %eax
  addq  $8, %rsp
  ret
.L4:
  movl  $1, %eax
  jmp   camlTOP3__cold_2_10_code@PLT

M.dispatch_hot_path:
  cmpq  $11, %rbx
  jg    .L0
  cmpq  $21, %rbx
  jge   .L1
  movl  $1, %eax
  ret
.L0:
  movl  $1, %eax
  jmp   camlTOP3__cold_2_10_code@PLT
.L1:
  movl  $1, %eax
  jmp   camlTOP3__cold_2_10_code@PLT
|}]
