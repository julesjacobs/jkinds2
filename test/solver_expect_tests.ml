open Jkinds_lib

module C = struct
  include Axis_lattice
end

let c a b = C.encode ~levels:[| a; b |]

module V = struct
  type t = string

  let compare = String.compare
end

module S = Lattice_solver.Make (C) (V)

let pp_coeff = C.to_string

let pp_poly p = S.pp ~pp_var:(fun s -> s) ~pp_coeff p

let print_state (vars : (string * S.var) list) : unit =
  vars
  |> List.iter (fun (name, v) ->
         let rel = if S.is_eliminated v then "=" else "≤" in
         let rhs = pp_poly (S.bound v) in
         Printf.printf "%s %s %s\n" name rel rhs)

let assert_leq_dump (v : S.var) (p : S.poly)
    (vars_to_show : (string * S.var) list) : unit =
  S.assert_leq v p;
  print_state vars_to_show

let%expect_test "assert_leq meets self and records dependency" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  assert_leq_dump y (S.meet (S.const (c 1 0)) (S.var x)) [ ("y", y) ];
  [%expect {| y ≤ [1,0] ⊓ x ⊓ y |}]

let%expect_test "solve_lfp substitutes into dependents and eliminates var" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  assert_leq_dump y (S.meet (S.const (c 1 0)) (S.var x)) [ ("y", y) ];
  S.solve_lfp x (S.const (c 2 0));
  print_state [ ("y", y) ];
  [%expect {|
    y ≤ [1,0] ⊓ x ⊓ y
    y ≤ [1,0] ⊓ y
    |}]

let%expect_test "assert on eliminated variable fails" =
  let x = S.new_var "x" in
  S.solve_lfp x (S.const (c 0 0));
  (match
     try
       S.assert_leq x (S.const (c 1 0));
       Ok ()
     with Failure msg -> Error msg
   with
  | Ok () -> print_endline "no error"
  | Error msg -> print_endline msg);
  [%expect {| assert_leq: variable already eliminated |}]

let%expect_test "double elimination fails" =
  let x = S.new_var "x" in
  S.solve_lfp x (S.const (c 0 0));
  (match
     try
       S.solve_lfp x (S.const (c 0 0));
       Ok ()
     with Failure msg -> Error msg
   with
  | Ok () -> print_endline "no error"
  | Error msg -> print_endline msg);
  [%expect {| solve_lfp: variable already eliminated |}]

let%expect_test "complex propagation and elimination scenario" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  let z = S.new_var "z" in
  let w = S.new_var "w" in
  let assert_leq_dump v p =
    S.assert_leq v p;
    print_state [ ("x", x); ("y", y); ("z", z); ("w", w) ]
  in
  (* y depends on x and a constant; z depends on y; w depends on z and x *)
  assert_leq_dump y
    (S.join (S.meet (S.const (c 1 0)) (S.var x)) (S.const (c 0 1)));
  [%expect
    {|
    x ≤ x
    y ≤ ([0,1] ⊓ y) ⊔ ([1,0] ⊓ x ⊓ y)
    z ≤ z
    w ≤ w
    |}];
  assert_leq_dump z (S.meet (S.var y) (S.const (c 2 0)));
  [%expect
    {|
    x ≤ x
    y ≤ ([0,1] ⊓ y) ⊔ ([1,0] ⊓ x ⊓ y)
    z ≤ [1,0] ⊓ x ⊓ y ⊓ z
    w ≤ w
    |}];
  assert_leq_dump w (S.meet (S.var z) (S.var x));
  [%expect
    {|
    x ≤ x
    y ≤ ([0,1] ⊓ y) ⊔ ([1,0] ⊓ x ⊓ y)
    z ≤ [1,0] ⊓ x ⊓ y ⊓ z
    w ≤ [1,0] ⊓ w ⊓ x ⊓ y ⊓ z
    |}];
  print_state [ ("x", x); ("y", y); ("z", z); ("w", w) ];
  [%expect
    {|
    x ≤ x
    y ≤ ([0,1] ⊓ y) ⊔ ([1,0] ⊓ x ⊓ y)
    z ≤ [1,0] ⊓ x ⊓ y ⊓ z
    w ≤ [1,0] ⊓ w ⊓ x ⊓ y ⊓ z
    |}];
  (* eliminate x, then z *)
  S.solve_lfp x (S.const (c 1 1));
  print_state [ ("x", x); ("y", y); ("z", z); ("w", w) ];
  [%expect
    {|
    x = [1,1]
    y ≤ [1,1] ⊓ y
    z ≤ [1,0] ⊓ y ⊓ z
    w ≤ [1,0] ⊓ w ⊓ y ⊓ z
    |}];
  (match
     try
       S.solve_lfp z (S.const (c 2 0));
       Ok ()
     with Failure msg -> Error msg
   with
  | Ok () -> print_endline "no error"
  | Error msg -> print_endline msg);
  [%expect {| solve_lfp: violates asserted inequalities |}]

let%expect_test "dependent updates propagate to direct dependents" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  let z = S.new_var "z" in
  let assert_leq_dump v p =
    S.assert_leq v p;
    print_state [ ("x", x); ("y", y); ("z", z) ]
  in
  assert_leq_dump y (S.meet (S.const (c 1 0)) (S.var x));
  [%expect {|
    x ≤ x
    y ≤ [1,0] ⊓ x ⊓ y
    z ≤ z
    |}];
  assert_leq_dump z (S.meet (S.const (c 2 0)) (S.var x));
  [%expect {|
    x ≤ x
    y ≤ [1,0] ⊓ x ⊓ y
    z ≤ [2,0] ⊓ x ⊓ z
    |}];
  S.solve_lfp x (S.const (c 0 1));
  print_state [ ("x", x); ("y", y); ("z", z) ];
  [%expect {|
    x = [0,1]
    y ≤ ⊥
    z ≤ ⊥
    |}]

let%expect_test "complex joins and propagation scenario" =
  let a = S.new_var "a" in
  let b = S.new_var "b" in
  let cv = S.new_var "c" in
  let d = S.new_var "d" in
  let e = S.new_var "e" in
  let f = S.new_var "f" in
  let assert_leq_dump v p =
    S.assert_leq v p;
    print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ]
  in
  (* b depends on a and a constant, plus a join with c; c depends on b via a
     join; d depends on (b ⊔ c) and a *)
  assert_leq_dump b
    (S.join
       (S.meet (S.const (c 1 0)) (S.var a))
       (S.join (S.const (c 0 1)) (S.meet (S.const (c 2 0)) (S.var cv))));
  [%expect
    {|
    a ≤ a
    b ≤ ([0,1] ⊓ b) ⊔ ([1,0] ⊓ a ⊓ b) ⊔ ([2,0] ⊓ b ⊓ c)
    c ≤ c
    d ≤ d
    e ≤ e
    f ≤ f
    |}];
  assert_leq_dump cv
    (S.join (S.meet (S.var b) (S.const (c 2 0))) (S.const (c 1 1)));
  [%expect
    {|
    a ≤ a
    b ≤ ([0,1] ⊓ b) ⊔ ([1,0] ⊓ a ⊓ b) ⊔ ([2,0] ⊓ b ⊓ c)
    c ≤ ([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c)
    d ≤ d
    e ≤ e
    f ≤ f
    |}];
  assert_leq_dump d (S.meet (S.join (S.var b) (S.var cv)) (S.var a));
  [%expect
    {|
    a ≤ a
    b ≤ ([0,1] ⊓ b) ⊔ ([1,0] ⊓ a ⊓ b) ⊔ ([2,0] ⊓ b ⊓ c)
    c ≤ ([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c)
    d ≤ ([1,1] ⊓ a ⊓ b ⊓ d) ⊔ ([1,1] ⊓ a ⊓ c ⊓ d) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d)
    e ≤ e
    f ≤ f
    |}];
  assert_leq_dump e (S.join (S.var d) (S.const (c 0 0)));
  [%expect
    {|
    a ≤ a
    b ≤ ([0,1] ⊓ b) ⊔ ([1,0] ⊓ a ⊓ b) ⊔ ([2,0] ⊓ b ⊓ c)
    c ≤ ([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c)
    d ≤ ([1,1] ⊓ a ⊓ b ⊓ d) ⊔ ([1,1] ⊓ a ⊓ c ⊓ d) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d)
    e ≤ ([1,1] ⊓ a ⊓ b ⊓ d ⊓ e) ⊔ ([1,1] ⊓ a ⊓ c ⊓ d ⊓ e) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d ⊓ e)
    f ≤ f
    |}];
  assert_leq_dump f (S.meet (S.var e) (S.var b));
  [%expect
    {|
    a ≤ a
    b ≤ ([0,1] ⊓ b) ⊔ ([1,0] ⊓ a ⊓ b) ⊔ ([2,0] ⊓ b ⊓ c)
    c ≤ ([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c)
    d ≤ ([1,1] ⊓ a ⊓ b ⊓ d) ⊔ ([1,1] ⊓ a ⊓ c ⊓ d) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d)
    e ≤ ([1,1] ⊓ a ⊓ b ⊓ d ⊓ e) ⊔ ([1,1] ⊓ a ⊓ c ⊓ d ⊓ e) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d ⊓ e)
    f ≤ ([1,1] ⊓ a ⊓ b ⊓ d ⊓ e ⊓ f) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d ⊓ e ⊓ f)
    |}];
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect
    {|
    a ≤ a
    b ≤ ([0,1] ⊓ b) ⊔ ([1,0] ⊓ a ⊓ b) ⊔ ([2,0] ⊓ b ⊓ c)
    c ≤ ([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c)
    d ≤ ([1,1] ⊓ a ⊓ b ⊓ d) ⊔ ([1,1] ⊓ a ⊓ c ⊓ d) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d)
    e ≤ ([1,1] ⊓ a ⊓ b ⊓ d ⊓ e) ⊔ ([1,1] ⊓ a ⊓ c ⊓ d ⊓ e) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d ⊓ e)
    f ≤ ([1,1] ⊓ a ⊓ b ⊓ d ⊓ e ⊓ f) ⊔ ([2,0] ⊓ a ⊓ b ⊓ c ⊓ d ⊓ e ⊓ f)
    |}];
  (* eliminate a safely with a constant; then eliminate c and b with meet-self
     patterns to avoid inequality violations *)
  S.solve_lfp a
    (S.join (S.var a) (S.join (S.var b) (S.const (c 2 0))));
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect
    {|
    a = ([0,1] ⊓ b) ⊔ [2,0]
    b ≤ ([1,1] ⊓ b) ⊔ ([2,0] ⊓ b ⊓ c)
    c ≤ ([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c)
    d ≤ ([1,0] ⊓ c ⊓ d) ⊔ ([1,1] ⊓ b ⊓ d) ⊔ ([2,0] ⊓ b ⊓ c ⊓ d)
    e ≤ ([1,0] ⊓ c ⊓ d ⊓ e) ⊔ ([1,1] ⊓ b ⊓ d ⊓ e) ⊔ ([2,0] ⊓ b ⊓ c ⊓ d ⊓ e)
    f ≤ ([1,1] ⊓ b ⊓ d ⊓ e ⊓ f) ⊔ ([2,0] ⊓ b ⊓ c ⊓ d ⊓ e ⊓ f)
    |}];
  S.solve_lfp cv (S.meet (S.var b) (S.const (c 2 0)));
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect
    {|
    a = ([0,1] ⊓ b) ⊔ [2,0]
    b ≤ [1,1] ⊓ b
    c = [1,0] ⊓ b
    d ≤ [1,1] ⊓ b ⊓ d
    e ≤ [1,1] ⊓ b ⊓ d ⊓ e
    f ≤ [1,1] ⊓ b ⊓ d ⊓ e ⊓ f
    |}];
  S.solve_lfp b (S.meet (S.var b) (S.const (c 1 0)));
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect
    {|
    a = [2,0]
    b = ⊥
    c = ⊥
    d ≤ ⊥
    e ≤ ⊥
    f ≤ ⊥
    |}]

let%expect_test "dependencies" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  let z = S.new_var "z" in
  let assert_leq_dump v p =
    S.assert_leq v p;
    print_state [ ("x", x); ("y", y); ("z", z) ]
  in
  assert_leq_dump y (S.join (S.var x) (S.var z));
  [%expect {|
    x ≤ x
    y ≤ (x ⊓ y) ⊔ (y ⊓ z)
    z ≤ z
    |}];
  assert_leq_dump x (S.meet (S.var z) (S.const (c 0 1)));
  [%expect {|
    x ≤ [0,1] ⊓ x ⊓ z
    y ≤ y ⊓ z
    z ≤ z
    |}];
  assert_leq_dump z (S.meet (S.var y) (S.const (c 2 0)));
  [%expect {|
    x ≤ ⊥
    y ≤ [2,0] ⊓ y ⊓ z
    z ≤ [2,0] ⊓ y ⊓ z
    |}];
  S.solve_lfp y (S.join (S.var z) (S.var y));
  print_state [ ("x", x); ("y", y); ("z", z) ];
  [%expect {|
    x ≤ ⊥
    y = ⊥
    z ≤ ⊥
    |}]

let%expect_test "dependencies2" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  let z = S.new_var "z" in
  let assert_leq_dump v p =
    S.assert_leq v p;
    print_state [ ("x", x); ("y", y); ("z", z) ]
  in
  assert_leq_dump y (S.join (S.var x) (S.var z));
  [%expect {|
    x ≤ x
    y ≤ (x ⊓ y) ⊔ (y ⊓ z)
    z ≤ z
    |}];
  assert_leq_dump x (S.join (S.var z) (S.const (c 0 1)));
  [%expect
    {|
    x ≤ ([0,1] ⊓ x) ⊔ ([2,0] ⊓ x ⊓ z)
    y ≤ ([0,1] ⊓ x ⊓ y) ⊔ (y ⊓ z)
    z ≤ z
    |}];
  assert_leq_dump z (S.join (S.var y) (S.const (c 2 0)));
  [%expect
    {|
    x ≤ ([0,1] ⊓ x) ⊔ ([2,0] ⊓ x ⊓ z)
    y ≤ ([0,1] ⊓ x ⊓ y) ⊔ (y ⊓ z)
    z ≤ ([0,1] ⊓ y ⊓ z) ⊔ ([2,0] ⊓ z)
    |}];
  S.solve_lfp y (S.join (S.var z) (S.var y));
  print_state [ ("x", x); ("y", y); ("z", z) ];
  [%expect
    {|
    x ≤ ([0,1] ⊓ x) ⊔ ([2,0] ⊓ x ⊓ z)
    y = [2,0] ⊓ z
    z ≤ [2,0] ⊓ z
    |}];
  S.solve_lfp x (S.var z);
  print_state [ ("x", x); ("y", y); ("z", z) ];
  [%expect
    {|
    x = [2,0] ⊓ z
    y = [2,0] ⊓ z
    z ≤ [2,0] ⊓ z
    |}];
  S.solve_lfp z (S.join (S.var z) (S.const (c 1 0)));
  print_state [ ("x", x); ("y", y); ("z", z) ];
  [%expect {|
    x = [1,0]
    y = [1,0]
    z = [1,0]
    |}]

let%expect_test
    "order difference: solve_lfp then assert_leq vs assert_leq then solve_lfp" =
  let x = S.new_var "x" in
  (* Solve first, then try to assert on the eliminated variable *)
  S.solve_lfp x (S.const (c 1 0));
  print_state [ ("x", x) ];
  (match
     try
       S.assert_leq x (S.meet (S.var x) (S.const (c 2 0)));
       Ok ()
     with Failure msg -> Error msg
   with
  | Ok () -> print_endline "no error"
  | Error msg -> print_endline msg);
  (* Now the opposite order on a fresh variable: assert first, then solve *)
  let x2 = S.new_var "x2" in
  S.assert_leq x2 (S.meet (S.var x2) (S.const (c 2 0)));
  print_state [ ("x2", x2) ];
  S.solve_lfp x2 (S.const (c 1 0));
  print_state [ ("x2", x2) ];
  [%expect
    {|
    x = [1,0]
    assert_leq: variable already eliminated
    x2 ≤ [2,0] ⊓ x2
    x2 = [1,0]
    |}]

let%expect_test
    "order difference (semantic): y LFP then assert x vs assert x then y LFP" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  let c10 = c 1 0 in
  let c20 = c 2 0 in
  (* Sequence A: solve y first, then assert on x *)
  S.solve_lfp y (S.meet (S.const c20) (S.var x));
  print_endline "-- after solve_lfp y --";
  print_state [ ("x", x); ("y", y) ];
  S.assert_leq x (S.meet (S.const c10) (S.var y));
  print_endline "-- after assert_leq x <= [1,0] /\\ y --";
  print_state [ ("x", x); ("y", y) ];
  (* Sequence B: fresh vars, assert x first, then solve y *)
  let x' = S.new_var "x'" in
  let y' = S.new_var "y'" in
  S.assert_leq x' (S.meet (S.const c10) (S.var y'));
  print_endline "-- fresh: after assert_leq x' <= [1,0] /\\ y' --";
  print_state [ ("x'", x'); ("y'", y') ];
  S.solve_lfp y' (S.meet (S.const c20) (S.var x'));
  print_endline "-- fresh: after solve_lfp y' --";
  print_state [ ("x'", x'); ("y'", y') ];
  [%expect
    {|
    -- after solve_lfp y --
    x ≤ x
    y = [2,0] ⊓ x
    -- after assert_leq x <= [1,0] /\ y --
    x ≤ [1,0] ⊓ x
    y = [1,0] ⊓ x
    -- fresh: after assert_leq x' <= [1,0] /\ y' --
    x' ≤ [1,0] ⊓ x' ⊓ y'
    y' ≤ y'
    -- fresh: after solve_lfp y' --
    x' ≤ ⊥
    y' = ⊥
    |}]
