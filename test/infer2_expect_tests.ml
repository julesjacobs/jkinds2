open Jkinds_lib

module C = struct
  include Axis_lattice
end

module VL = struct
  type t = Infer2.var_label

  let compare (a : t) (b : t) : int =
    match (a, b) with
    | Infer2.VarLabel.Atom a1, Infer2.VarLabel.Atom a2 -> (
      match String.compare a1.Modality.ctor a2.Modality.ctor with
      | 0 -> Int.compare a1.index a2.index
      | c -> c)
    | Infer2.VarLabel.TyVar x, Infer2.VarLabel.TyVar y -> Int.compare x y
    | Infer2.VarLabel.TyRec x, Infer2.VarLabel.TyRec y -> Int.compare x y
    | Infer2.VarLabel.Atom _, (Infer2.VarLabel.TyVar _ | Infer2.VarLabel.TyRec _)
      ->
      -1
    | Infer2.VarLabel.TyVar _, Infer2.VarLabel.Atom _ -> 1
    | Infer2.VarLabel.TyRec _, Infer2.VarLabel.Atom _ -> 1
    | Infer2.VarLabel.TyVar _, Infer2.VarLabel.TyRec _ -> -1
    | Infer2.VarLabel.TyRec _, Infer2.VarLabel.TyVar _ -> 1

  let to_string _ = "<todo5>"
end

module S = Lattice_solver.Make (C) (VL)

let pp_coeff = C.to_string
let pp_poly p = S.pp p

let%expect_test "two-phase abstract solving (one/two)" =
  (* Setup atom vars *)
  let one0 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "one"; index = 0 })
  in
  let two0 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "two"; index = 0 })
  in
  (* Given from linear decomposition: one.base = ([0,1] ⊓ two.0), two.base =
     one.0 *)
  let m01 = C.encode ~levels:[| 0; 1 |] in
  print_endline (S.pp_state_line one0);
  print_endline (S.pp_state_line two0);
  [%expect {|
    one.0 ≤ one.0
    two.0 ≤ two.0
    |}];
  let base_one = S.meet (S.const m01) (S.var two0) in
  let base_two = S.var one0 in
  (* Abstract phase: assert leq C.0 ≤ base(C), meet-self happens in
     assert_leq. *)
  S.assert_leq one0 base_one;
  print_endline (S.pp_state_line one0);
  print_endline (S.pp_state_line two0);
  [%expect {|
    one.0 ≤ [0,1] ⊓ one.0 ⊓ two.0
    two.0 ≤ two.0
    |}];
  (* Solve concrete: two.0 = one.0, keep abstract as inequality. *)
  (* Manual solution process:
    one.0 ≤ ([0,1] ⊓ one.0 ⊓ two.0)
    two.0 ≤ two.0

    Normal form of one.0 = ([0,1] ⊓ one.0 ⊓ two.0)

    solve_lfp two.0 ([0,1] ⊓ one.0 ⊓ two.0)
      substitute two.0 -> bot in ([0,1] ⊓ one.0 ⊓ two.0) gives bot

    => all bot
  *)
  S.solve_lfp two0 base_two;
  print_endline (S.pp_state_line one0);
  print_endline (S.pp_state_line two0);
  [%expect {|
    one.0 ≤ ⊥
    two.0 = ⊥
    |}]

let%expect_test "two-phase abstract solving (one/two) lfp first" =
  (* Setup atom vars *)
  let one0 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "one"; index = 0 })
  in
  let two0 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "two"; index = 0 })
  in
  (* Given from linear decomposition: one.base = ([0,1] ⊓ two.0), two.base =
     one.0 *)
  let m01 = C.encode ~levels:[| 0; 1 |] in
  print_endline (S.pp_state_line one0);
  print_endline (S.pp_state_line two0);
  [%expect {|
    one.0 ≤ one.0
    two.0 ≤ two.0
    |}];
  let base_one = S.meet (S.const m01) (S.var two0) in
  let base_two = S.var one0 in
  (* Abstract phase: assert leq C.0 ≤ base(C), meet-self happens in
     assert_leq. *)
  S.solve_lfp two0 base_two;
  print_endline (S.pp_state_line one0);
  print_endline (S.pp_state_line two0);
  [%expect {|
    one.0 ≤ one.0
    two.0 = one.0
    |}];
  S.assert_leq one0 base_one;
  print_endline (S.pp_state_line one0);
  print_endline (S.pp_state_line two0);
  [%expect {|
    one.0 ≤ [0,1] ⊓ one.0
    two.0 = [0,1] ⊓ one.0
    |}]
