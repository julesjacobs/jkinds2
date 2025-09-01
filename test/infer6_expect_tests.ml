open Jkinds_lib

let%expect_test "infer6 vs infer2/4/5 on cyclic kinds" =
  let src =
    "\n\
     type L('a1) : mu 'b1. ('a1 + 'b1)\n\n\
     type Nested('a1) : mu 'b1. ((mu 'b2. ('b1 + 'b2)) + 'a1)\n\n\
     type Annot('a1) : mu 'b1. (('a1 @@ [1,0]) + ('b1 @@ [0,1]))\n\n"
  in
  let prog = Decl_parser.parse_program_exn src in
  let out6 = Infer6.run_program prog in
  let out7 = Infer7.run_program prog in
  print_endline "Infer6 normalized kinds:";
  print_endline out6;
  print_endline "Infer7 normalized kinds:";
  print_endline out7;
  assert (out6 = out7);
  let out2 = Infer2.run_program prog in
  let out4 = Infer4.run_program prog in
  let out5 = Infer5.run_program prog in
  print_endline "Infer2 & Infer4 & Infer5 normalized kinds:";
  assert (out2 = out4 && out4 = out5);
  print_endline out2;
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure test/infer6_expect_tests.ml:17:2"
  Raised at Expect_lib__Infer6_expect_tests.(fun) in file "test/infer6_expect_tests.ml", line 17, characters 2-22
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 234, characters 12-19

  Trailing output
  ---------------
  Infer6 normalized kinds:
  L: {0 ↦ ⊥, 1 ↦ L.1}
  Nested: {0 ↦ ⊥, 1 ↦ Nested.1}
  Annot: {0 ↦ ⊥, 1 ↦ [1,0] ⊓ Annot.1}
  Infer7 normalized kinds:
  L: {0 ↦ TODO, 1 ↦ TODO}
  Nested: {0 ↦ TODO, 1 ↦ TODO}
  Annot: {0 ↦ TODO, 1 ↦ TODO} |}]
