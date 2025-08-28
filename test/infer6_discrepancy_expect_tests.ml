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
    D: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ ([0,1] ⊓ D.1 ⊓ E.2) ⊔ ([1,0] ⊓ D.1), 2 ↦ [0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2}
    E: {0 ↦ [0,1] ⊓ D.0 ⊓ E.0, 1 ↦ [0,1] ⊓ D.1 ⊓ E.1 ⊓ E.2, 2 ↦ [0,1] ⊓ E.2}
    -- Infer6 D debug --
    -- base.debug --
    Node#71 v#17:Rigid(E.0) lo=#0 hi=#70
      Leaf#0 c=[0,0]
      Node#70 v#20:Rigid(D.0) lo=#0 hi=#13
        #0 = <ref>
        Leaf#13 c=[0,1]


    -- base.forced.debug --
    Node#71 v#17:Rigid(E.0) lo=#0 hi=#70
      Leaf#0 c=[0,0]
      Node#70 v#20:Rigid(D.0) lo=#0 hi=#13
        #0 = <ref>
        Leaf#13 c=[0,1]


    -- D.1.debug --
    Node#75 v#19:Rigid(E.2) lo=#63 hi=#73
      Node#63 v#21:Rigid(D.1) lo=#0 hi=#6
        Leaf#0 c=[0,0]
        Leaf#6 c=[1,0]
      Node#73 v#21:Rigid(D.1) lo=#0 hi=#13
        #0 = <ref>
        Leaf#13 c=[0,1]


    -- D.1.forced.debug --
    Node#75 v#19:Rigid(E.2) lo=#63 hi=#73
      Node#63 v#21:Rigid(D.1) lo=#0 hi=#6
        Leaf#0 c=[0,0]
        Leaf#6 c=[1,0]
      Node#73 v#21:Rigid(D.1) lo=#0 hi=#13
        #0 = <ref>
        Leaf#13 c=[0,1]


    -- D.2.debug --
    Node#90 v#18:Rigid(E.1) lo=#0 hi=#89
      Leaf#0 c=[0,0]
      Node#89 v#19:Rigid(E.2) lo=#0 hi=#88
        #0 = <ref>
        Node#88 v#21:Rigid(D.1) lo=#0 hi=#85
          #0 = <ref>
          Node#85 v#22:Rigid(D.2) lo=#0 hi=#13
            #0 = <ref>
            Leaf#13 c=[0,1]


    -- D.2.forced.debug --
    Node#90 v#18:Rigid(E.1) lo=#0 hi=#89
      Leaf#0 c=[0,0]
      Node#89 v#19:Rigid(E.2) lo=#0 hi=#88
        #0 = <ref>
        Node#88 v#21:Rigid(D.1) lo=#0 hi=#85
          #0 = <ref>
          Node#85 v#22:Rigid(D.2) lo=#0 hi=#13
            #0 = <ref>
            Leaf#13 c=[0,1]
    |}]
