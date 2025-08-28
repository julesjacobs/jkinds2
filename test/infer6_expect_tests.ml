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
  print_endline "Infer6 normalized kinds:";
  print_endline out6;
  let out2 = Infer2.run_program prog in
  let out4 = Infer4.run_program prog in
  let out5 = Infer5.run_program prog in
  print_endline "Infer2 & Infer4 & Infer5 normalized kinds:";
  assert (out2 = out4 && out4 = out5);
  print_endline out2;
  [%expect
    {|
    Infer6 normalized kinds:
    L: {0 ↦ ⊥, 1 ↦ L.1}
    Nested: {0 ↦ ⊥, 1 ↦ Nested.1}
    Annot: {0 ↦ ⊥, 1 ↦ [1,0] ⊓ Annot.1}
    Infer2 & Infer4 & Infer5 normalized kinds:
    L: {0 ↦ ⊥, 1 ↦ L.1}
    Nested: {0 ↦ ⊥, 1 ↦ Nested.1}
    Annot: {0 ↦ ⊥, 1 ↦ [1,0] ⊓ Annot.1}
    |}]
