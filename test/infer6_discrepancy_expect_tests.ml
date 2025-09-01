open Jkinds_lib

let prog_src =
  "\n\
   # Minimal subset highlighting remaining mismatch\n\
   type D('a1,'a2) : E('a2,'a1) + ('a1 @@ [1,0])\n\
   type E('a1,'a2) : (D('a1,'a2) @@ [0,1]) + ('a2 @@ [0,1])\n\n"

let%expect_test "Infer6 vs others on D/E and LDD debug dump" =
  let prog = Decl_parser.parse_program_exn prog_src in
  print_endline "-- Infer2 --";
  print_endline (Infer2.run_program prog);
  print_endline "-- Infer4 --";
  print_endline (Infer4.run_program prog);
  print_endline "-- Infer5 --";
  print_endline (Infer5.run_program prog);
  print_endline "-- Infer6 --";
  print_endline (Infer6.run_program prog);
  print_endline "-- Infer7 --";
  print_endline (Infer7.run_program prog);
  print_endline "-- Infer6 D debug --";
  print_endline (Infer6.debug_constr prog ~constr:"D");
  [%expect
    {|
    -- Infer2 --
    D: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ ([0,1] ⊓ D.1 ⊓ E.2) ⊔ ([1,0] ⊓ D.1), 2 ↦ [0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2}
    E: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ [0,1] ⊓ D.1 ⊓ E.1 ⊓ E.2, 2 ↦ [0,1] ⊓ E.2}
    -- Infer4 --
    D: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ ([0,1] ⊓ D.1 ⊓ E.2) ⊔ ([1,0] ⊓ D.1), 2 ↦ [0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2}
    E: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ [0,1] ⊓ D.1 ⊓ E.1 ⊓ E.2, 2 ↦ [0,1] ⊓ E.2}
    -- Infer5 --
    D: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ ([0,1] ⊓ D.1 ⊓ E.2) ⊔ ([1,0] ⊓ D.1), 2 ↦ [0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2}
    E: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ [0,1] ⊓ D.1 ⊓ E.1 ⊓ E.2, 2 ↦ [0,1] ⊓ E.2}
    -- Infer6 --
    D: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ ([0,1] ⊓ D.0 ⊓ D.1 ⊓ E.0) ⊔ ([0,1] ⊓ D.1 ⊓ E.2) ⊔ ([1,0] ⊓ D.1), 2 ↦ ([0,1] ⊓ D.0 ⊓ D.2 ⊓ E.0) ⊔ ([0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2)}
    E: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ ([0,1] ⊓ D.0 ⊓ E.0 ⊓ E.1) ⊔ ([0,1] ⊓ D.1 ⊓ E.1 ⊓ E.2), 2 ↦ [0,1] ⊓ E.2}
    -- Infer7 --
    D: {0 ↦ TODO, 1 ↦ TODO, 2 ↦ TODO}
    E: {0 ↦ TODO, 1 ↦ TODO, 2 ↦ TODO}
    -- Infer6 D debug --
    -- base.debug --
    Node#103 v#13:Solved(#146) lo=#0 hi=#1
      Leaf#0 c=[0,0]
      Leaf#1 c=[2,1]


    -- base.forced.debug --
    Node#103 v#13:Solved(#146) lo=#0 hi=#1
      Leaf#0 c=[0,0]
      Leaf#1 c=[2,1]


    -- D.1.debug --
    Node#101 v#14:Solved(#148) lo=#0 hi=#1
      Leaf#0 c=[0,0]
      Leaf#1 c=[2,1]


    -- D.1.forced.debug --
    Node#101 v#14:Solved(#148) lo=#0 hi=#1
      Leaf#0 c=[0,0]
      Leaf#1 c=[2,1]


    -- D.2.debug --
    Node#102 v#15:Solved(#150) lo=#0 hi=#1
      Leaf#0 c=[0,0]
      Leaf#1 c=[2,1]


    -- D.2.forced.debug --
    Node#102 v#15:Solved(#150) lo=#0 hi=#1
      Leaf#0 c=[0,0]
      Leaf#1 c=[2,1]
    |}]
