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
end

module S = Lattice_solver.Make (C) (VL)

let pp_coeff = C.to_string

let%expect_test "abstract F/H constraints show extra F.0 disjunct in F.1" =
  let h0 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "H"; index = 0 })
  in
  let h1 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "H"; index = 1 })
  in
  let f0 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "F"; index = 0 })
  in
  let f1 =
    S.new_var (Infer2.VarLabel.Atom { Modality.ctor = "F"; index = 1 })
  in
  let c01 = C.encode ~levels:[| 0; 1 |] in
  (* Infer2 abstract assertions (phase 2): H.0 ≤ F.0; H.1 ≤ ⊤; F.0 ≤ ([0,1] ⊓
     H.0); F.1 ≤ ([0,1] ⊓ H.0) ∨ ([0,1] ⊓ H.1) *)
  S.assert_leq h0 (S.var f0);
  S.assert_leq h1 (S.const C.top);
  S.assert_leq f0 (S.meet (S.const c01) (S.var h0));
  let f1_rhs =
    S.join (S.meet (S.const c01) (S.var h0)) (S.meet (S.const c01) (S.var h1))
  in
  S.assert_leq f1 f1_rhs;
  print_endline (S.pp_state_line ~pp_var:Infer2.pp_varlabel ~pp_coeff f1);
  [%expect {| F.1 ≤ (([0,1] ⊓ H.1 ⊓ F.1) ⊔ ([0,1] ⊓ H.0 ⊓ F.0 ⊓ F.1)) |}]
