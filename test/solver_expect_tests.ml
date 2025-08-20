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

let render_norm (p : S.poly) : string =
  let terms = S.normalize p in
  let pp_term (coeff, vars) =
    match vars with
    | [] ->
      let levels = C.decode coeff |> Array.to_list in
      let parts = levels |> List.map string_of_int |> String.concat "," in
      Printf.sprintf "[%s]" parts
    | _ ->
      let vars_str = String.concat " ⊓ " vars in
      let levels = C.decode coeff |> Array.to_list in
      let parts = levels |> List.map string_of_int |> String.concat "," in
      Printf.sprintf "([%s] ⊓ %s)" parts vars_str
  in
  match terms with
  | [] -> ""
  | [ t ] -> pp_term t
  | ts ->
    ts |> List.map pp_term |> String.concat " ⊔ " |> fun s -> "(" ^ s ^ ")"

let pp_norm (p : S.poly) : unit = print_endline (render_norm p)

let print_state (vars : (string * S.var) list) : unit =
  vars
  |> List.iter (fun (name, v) ->
         let s = render_norm (S.var v) in
         if s <> "" then Printf.printf "%s: %s\n" name s)

let assert_leq_dump (v : S.var) (p : S.poly)
    (vars_to_show : (string * S.var) list) : unit =
  S.assert_leq v p;
  print_state vars_to_show

let%expect_test "assert_leq meets self and records dependency" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  assert_leq_dump y (S.meet (S.const (c 1 0)) (S.var x)) [ ("y", y) ];
  [%expect {| y: ([1,0] ⊓ y) |}]

let%expect_test "solve_lfp substitutes into dependents and eliminates var" =
  let x = S.new_var "x" in
  let y = S.new_var "y" in
  assert_leq_dump y (S.meet (S.const (c 1 0)) (S.var x)) [ ("y", y) ];
  S.solve_lfp x (S.const (c 2 0));
  print_state [ ("y", y) ];
  [%expect {|
    y: ([1,0] ⊓ y)
    y: ([1,0] ⊓ y)
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
  [%expect {|
    x: [2,1]
    y: ([1,1] ⊓ y)
    z: [2,1]
    w: [2,1]
    |}];
  assert_leq_dump z (S.meet (S.var y) (S.const (c 2 0)));
  [%expect
    {|
    x: [2,1]
    y: ([1,1] ⊓ y)
    z: ([1,0] ⊓ y ⊓ z)
    w: [2,1]
    |}];
  assert_leq_dump w (S.meet (S.var z) (S.var x));
  [%expect
    {|
    x: [2,1]
    y: ([1,1] ⊓ y)
    z: ([1,0] ⊓ y ⊓ z)
    w: ([1,0] ⊓ y ⊓ z ⊓ w)
    |}];
  print_state [ ("x", x); ("y", y); ("z", z); ("w", w) ];
  [%expect
    {|
    x: [2,1]
    y: ([1,1] ⊓ y)
    z: ([1,0] ⊓ y ⊓ z)
    w: ([1,0] ⊓ y ⊓ z ⊓ w)
    |}];
  (* eliminate x, then z *)
  S.solve_lfp x (S.const (c 1 1));
  print_state [ ("x", x); ("y", y); ("z", z); ("w", w) ];
  [%expect
    {|
    x: [1,1]
    y: ([1,1] ⊓ y)
    z: ([1,0] ⊓ y ⊓ z)
    w: ([1,0] ⊓ y ⊓ z ⊓ w)
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
    a: [2,1]
    b: ([2,1] ⊓ b)
    c: [2,1]
    d: [2,1]
    e: [2,1]
    f: [2,1]
    |}];
  assert_leq_dump cv
    (S.join (S.meet (S.var b) (S.const (c 2 0))) (S.const (c 1 1)));
  [%expect
    {|
    a: [2,1]
    b: ([2,1] ⊓ b)
    c: (([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c))
    d: [2,1]
    e: [2,1]
    f: [2,1]
    |}];
  assert_leq_dump d (S.meet (S.join (S.var b) (S.var cv)) (S.var a));
  [%expect
    {|
    a: [2,1]
    b: ([2,1] ⊓ b)
    c: (([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c))
    d: (([2,1] ⊓ b ⊓ d) ⊔ ([1,1] ⊓ c ⊓ d))
    e: [2,1]
    f: [2,1]
    |}];
  assert_leq_dump e (S.join (S.var d) (S.const (c 0 0)));
  [%expect
    {|
    a: [2,1]
    b: ([2,1] ⊓ b)
    c: (([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c))
    d: (([2,1] ⊓ b ⊓ d) ⊔ ([1,1] ⊓ c ⊓ d))
    e: (([2,1] ⊓ b ⊓ d ⊓ e) ⊔ ([1,1] ⊓ c ⊓ d ⊓ e))
    f: [2,1]
    |}];
  assert_leq_dump f (S.meet (S.var e) (S.var b));
  [%expect
    {|
    a: [2,1]
    b: ([2,1] ⊓ b)
    c: (([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c))
    d: (([2,1] ⊓ b ⊓ d) ⊔ ([1,1] ⊓ c ⊓ d))
    e: (([2,1] ⊓ b ⊓ d ⊓ e) ⊔ ([1,1] ⊓ c ⊓ d ⊓ e))
    f: ([2,1] ⊓ b ⊓ d ⊓ e ⊓ f)
    |}];
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect
    {|
    a: [2,1]
    b: ([2,1] ⊓ b)
    c: (([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c))
    d: (([2,1] ⊓ b ⊓ d) ⊔ ([1,1] ⊓ c ⊓ d))
    e: (([2,1] ⊓ b ⊓ d ⊓ e) ⊔ ([1,1] ⊓ c ⊓ d ⊓ e))
    f: ([2,1] ⊓ b ⊓ d ⊓ e ⊓ f)
    |}];
  (* eliminate a safely with a constant; then eliminate c and b with meet-self
     patterns to avoid inequality violations *)
  S.solve_lfp a (S.const (c 2 1));
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect
    {|
    a: [2,1]
    b: ([2,1] ⊓ b)
    c: (([1,1] ⊓ c) ⊔ ([2,0] ⊓ b ⊓ c))
    d: (([2,1] ⊓ b ⊓ d) ⊔ ([1,1] ⊓ c ⊓ d))
    e: (([2,1] ⊓ b ⊓ d ⊓ e) ⊔ ([1,1] ⊓ c ⊓ d ⊓ e))
    f: ([2,1] ⊓ b ⊓ d ⊓ e ⊓ f)
    |}];
  S.solve_lfp cv (S.meet (S.var cv) (S.const (c 2 0)));
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect
    {|
    a: [2,1]
    b: ([2,1] ⊓ b)
    d: ([2,1] ⊓ b ⊓ d)
    e: ([2,1] ⊓ b ⊓ d ⊓ e)
    f: ([2,1] ⊓ b ⊓ d ⊓ e ⊓ f)
    |}];
  S.solve_lfp b (S.meet (S.var b) (S.const (c 1 0)));
  print_state [ ("a", a); ("b", b); ("c", cv); ("d", d); ("e", e); ("f", f) ];
  [%expect {| a: [2,1] |}]
