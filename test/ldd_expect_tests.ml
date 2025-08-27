open Jkinds_lib

module C = struct
  include Product_lattice.Make (struct
    let axis_sizes = [| 3; 2 |]
  end)

  let hash = Hashtbl.hash
end

let axis_names = [| "a"; "b" |]
let show_c (x : C.t) = C.pp ~axis_names x

module L = Ldd.Make (C)

let printw w = print_endline (L.pp_as_polynomial ~pp_coeff:show_c w)
let c a b = C.encode ~levels:[| a; b |]

let%expect_test "ldd pp: const only" =
  let w = L.const (c 1 0) in
  printw w;
  [%expect {| [1, 0] |}]

let%expect_test "ldd pp: rigid var only" =
  let x = L.rigid "x" in
  printw x;
  [%expect {| x |}]

let%expect_test "ldd pp: const meet rigid (single term, no parens)" =
  let x = L.rigid "x" in
  let w = L.meet (L.const (c 2 0)) x in
  printw w;
  [%expect {| [2, 0] ⊓ x |}]

let%expect_test "ldd pp: join of two meets (parens)" =
  let x = L.rigid "x" in
  let y = L.rigid "y" in
  let w = L.join (L.meet (L.const (c 1 1)) x) (L.meet (L.const (c 2 0)) y) in
  printw w;
  [%expect {| ([1, 1] ⊓ x) ⊔ ([2, 0] ⊓ y) |}]

let%expect_test "ldd pp: top annihilates join" =
  let x = L.rigid "x" in
  let w = L.join (L.const C.top) x in
  printw w;
  [%expect {| ⊤ |}]

let%expect_test "ldd pp: duplicates aggregate by join on coeffs" =
  let x = L.rigid "x" in
  let w = L.join (L.meet (L.const (c 1 0)) x) (L.meet (L.const (c 2 0)) x) in
  printw w;
  [%expect {| [2, 0] ⊓ x |}]

let%expect_test "ldd: meet ⊤ with (x ⊓ z) ⊔ y" =
  (* Repro from randomized parity test: currently LDD drops (x ⊓ z), resulting
     in y, whereas lattice polynomials keep (x ⊓ z) ⊔ y. *)
  let x = L.rigid "x" in
  let y = L.rigid "y" in
  let z = L.rigid "z" in
  let rhs = L.join (L.meet x z) y in
  let r = L.meet (L.const C.top) rhs in
  printw r;
  [%expect {| (x ⊓ z) ⊔ y |}]

let%expect_test "ldd: layered joins reproducer (structure debug)" =
  let x = L.rigid "x" in
  let y = L.rigid "y" in
  let z = L.rigid "z" in
  let c11 = L.const (c 1 1) in
  let c20 = L.const (c 2 0) in
  (* Build inner x-layer: [1,1] ⊔ ([2,0] ⊓ x) *)
  let w_x = L.join c11 (L.meet c20 x) in
  (* Add y layer: join ([2,0] ⊓ y) on top of x-layer *)
  let w_y = L.join (L.meet c20 y) w_x in
  (* Add z layer: join ([2,0] ⊓ z) on top of y-layer *)
  let w_z = L.join (L.meet c20 z) w_y in
  (* Final join with x; observed to drop z in randomized run *)
  let r = L.join x w_z in
  print_endline "-- w_x.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c w_x);
  print_endline "-- w_y.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c w_y);
  print_endline "-- w_z.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c w_z);
  print_endline "-- r.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c r);
  printw r;
  [%expect
    {|
-- w_x.debug --
Node#26 v#9:Rigid(x) lo=#10 hi=#5
  Leaf#10 c=[1, 1]
  Leaf#5 c=[2, 0]

-- w_y.debug --
Node#29 v#9:Rigid(x) lo=#28 hi=#5
  Node#28 v#10:Rigid(y) lo=#10 hi=#5
    Leaf#10 c=[1, 1]
    Leaf#5 c=[2, 0]
  #5 = <ref>

-- w_z.debug --
Node#33 v#9:Rigid(x) lo=#32 hi=#5
  Node#32 v#10:Rigid(y) lo=#31 hi=#5
    Node#31 v#11:Rigid(z) lo=#10 hi=#5
      Leaf#10 c=[1, 1]
      Leaf#5 c=[2, 0]
    #5 = <ref>
  #5 = <ref>

-- r.debug --
Node#33 v#9:Rigid(x) lo=#32 hi=#5
  Node#32 v#10:Rigid(y) lo=#31 hi=#5
    Node#31 v#11:Rigid(z) lo=#10 hi=#5
      Leaf#10 c=[1, 1]
      Leaf#5 c=[2, 0]
    #5 = <ref>
  #5 = <ref>

[1, 1] ⊔ ([2, 0] ⊓ x) ⊔ ([2, 0] ⊓ y) ⊔ ([2, 0] ⊓ z) |}]

let%expect_test
    "ldd: join x with ([0,1] ⊔ ([2,0] ⊓ x) ⊔ ([2,0] ⊓ y) ⊔ ([2,0] ⊓ z))" =
  let x = L.rigid "x" in
  let y = L.rigid "y" in
  let z = L.rigid "z" in
  let a = x in
  let b =
    L.join
      (L.const (c 0 1))
      (L.join
         (L.meet (L.const (c 2 0)) x)
         (L.join (L.meet (L.const (c 2 0)) y) (L.meet (L.const (c 2 0)) z)))
  in
  let r = L.join a b in
  printw r;
  [%expect {| [0, 1] ⊔ ([2, 0] ⊓ x) ⊔ ([2, 0] ⊓ y) ⊔ ([2, 0] ⊓ z) |}]

let%expect_test
    "ldd: join x with [1, 1] ⊔ ([2, 0] ⊓ x) ⊔ ([2, 0] ⊓ y) ⊔ ([2, 0] ⊓ z)" =
  let x = L.rigid "x" in
  let y = L.rigid "y" in
  let z = L.rigid "z" in
  let a = x in
  let b =
    L.join
      (L.const (c 1 1))
      (L.join
         (L.meet (L.const (c 2 0)) x)
         (L.join (L.meet (L.const (c 2 0)) y) (L.meet (L.const (c 2 0)) z)))
  in
  let r = L.join a b in
  printw r;
  [%expect {| [1, 1] ⊔ ([2, 0] ⊓ x) ⊔ ([2, 0] ⊓ y) ⊔ ([2, 0] ⊓ z) |}]

let%expect_test "ldd: minimized reproducer with a,b,c; debug structure and join"
    =
  (* Variable order a < b < c by construction *)
  let a = L.rigid "a" in
  let b = L.rigid "b" in
  let cv = L.rigid "c" in
  let c01 = L.const (c 0 1) in
  let c20 = L.const (c 2 0) in
  (* arg1 = [0,1] ⊔ ([2,0] ⊓ a) ⊔ ([2,0] ⊓ b) ⊔ ([2,0] ⊓ c) *)
  let arg1 =
    L.join c01 (L.join (L.meet c20 a) (L.join (L.meet c20 b) (L.meet c20 cv)))
  in
  let arg2 = cv in
  print_endline "-- arg1.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg1);
  print_endline "-- arg2.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg2);
  let r = L.join arg1 arg2 in
  print_endline "-- result.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c r);
  print_endline "-- prints --";
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg1);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg2);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c r);
  [%expect
    {|
    -- arg1.debug --
    Node#67 v#18:Rigid(a) lo=#66 hi=#5
      Node#66 v#19:Rigid(b) lo=#65 hi=#5
        Node#65 v#20:Rigid(c) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- arg2.debug --
    Node#59 v#20:Rigid(c) lo=#0 hi=#1
      Leaf#0 c=[0, 0]
      Leaf#1 c=[2, 1]

    -- result.debug --
    Node#67 v#18:Rigid(a) lo=#66 hi=#5
      Node#66 v#19:Rigid(b) lo=#65 hi=#5
        Node#65 v#20:Rigid(c) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- prints --
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c)
    c
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c) |}]

let%expect_test "ldd: minimized reproducer order a,c,b; debug and join" =
  (* Variable creation order a < c < b *)
  let a = L.rigid "a" in
  let cv = L.rigid "c" in
  let b = L.rigid "b" in
  let c01 = L.const (c 0 1) in
  let c20 = L.const (c 2 0) in
  let arg1 =
    L.join c01 (L.join (L.meet c20 a) (L.join (L.meet c20 b) (L.meet c20 cv)))
  in
  let arg2 = cv in
  print_endline "-- arg1.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg1);
  print_endline "-- arg2.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg2);
  let r = L.join arg1 arg2 in
  print_endline "-- result.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c r);
  print_endline "-- prints --";
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg1);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg2);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c r);
  [%expect
    {|
    -- arg1.debug --
    Node#78 v#21:Rigid(a) lo=#77 hi=#5
      Node#77 v#22:Rigid(c) lo=#76 hi=#5
        Node#76 v#23:Rigid(b) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- arg2.debug --
    Node#69 v#22:Rigid(c) lo=#0 hi=#1
      Leaf#0 c=[0, 0]
      Leaf#1 c=[2, 1]

    -- result.debug --
    Node#78 v#21:Rigid(a) lo=#77 hi=#5
      Node#77 v#22:Rigid(c) lo=#76 hi=#5
        Node#76 v#23:Rigid(b) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- prints --
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c)
    c
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c) |}]

let%expect_test "ldd: minimized reproducer order b,a,c; debug and join" =
  (* Variable creation order b < a < c *)
  let b = L.rigid "b" in
  let a = L.rigid "a" in
  let cv = L.rigid "c" in
  let c01 = L.const (c 0 1) in
  let c20 = L.const (c 2 0) in
  let arg1 =
    L.join c01 (L.join (L.meet c20 a) (L.join (L.meet c20 b) (L.meet c20 cv)))
  in
  let arg2 = cv in
  print_endline "-- arg1.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg1);
  print_endline "-- arg2.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg2);
  let r = L.join arg1 arg2 in
  print_endline "-- result.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c r);
  print_endline "-- prints --";
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg1);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg2);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c r);
  [%expect
    {|
    -- arg1.debug --
    Node#90 v#24:Rigid(b) lo=#89 hi=#5
      Node#89 v#25:Rigid(a) lo=#88 hi=#5
        Node#88 v#26:Rigid(c) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- arg2.debug --
    Node#81 v#26:Rigid(c) lo=#0 hi=#1
      Leaf#0 c=[0, 0]
      Leaf#1 c=[2, 1]

    -- result.debug --
    Node#90 v#24:Rigid(b) lo=#89 hi=#5
      Node#89 v#25:Rigid(a) lo=#88 hi=#5
        Node#88 v#26:Rigid(c) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- prints --
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c)
    c
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c) |}]

let%expect_test "ldd: minimized reproducer order c,b,a; debug and join" =
  (* Variable creation order c < b < a *)
  let cv = L.rigid "c" in
  let b = L.rigid "b" in
  let a = L.rigid "a" in
  let c01 = L.const (c 0 1) in
  let c20 = L.const (c 2 0) in
  let arg1 =
    L.join c01 (L.join (L.meet c20 a) (L.join (L.meet c20 b) (L.meet c20 cv)))
  in
  let arg2 = cv in
  print_endline "-- arg1.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg1);
  print_endline "-- arg2.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c arg2);
  let r = L.join arg1 arg2 in
  print_endline "-- result.debug --";
  print_endline (L.pp_debug ~pp_coeff:show_c r);
  print_endline "-- prints --";
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg1);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c arg2);
  print_endline (L.pp_as_polynomial ~pp_coeff:show_c r);
  [%expect
    {|
    -- arg1.debug --
    Node#102 v#27:Rigid(c) lo=#101 hi=#5
      Node#101 v#28:Rigid(b) lo=#100 hi=#5
        Node#100 v#29:Rigid(a) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- arg2.debug --
    Node#91 v#27:Rigid(c) lo=#0 hi=#1
      Leaf#0 c=[0, 0]
      Leaf#1 c=[2, 1]

    -- result.debug --
    Node#102 v#27:Rigid(c) lo=#101 hi=#5
      Node#101 v#28:Rigid(b) lo=#100 hi=#5
        Node#100 v#29:Rigid(a) lo=#42 hi=#5
          Leaf#42 c=[0, 1]
          Leaf#5 c=[2, 0]
        #5 = <ref>
      #5 = <ref>

    -- prints --
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c)
    c
    [0, 1] ⊔ ([2, 0] ⊓ a) ⊔ ([2, 0] ⊓ b) ⊔ ([2, 0] ⊓ c) |}]
