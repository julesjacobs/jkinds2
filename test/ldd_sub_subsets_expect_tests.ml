open Jkinds_lib

module C = struct
  include Axis_lattice
end

module Name = struct
  include String

  let to_string s = s
end

module L = Ldd.Make (C) (Name)

let c a b = C.encode ~levels:[| a; b |]
let rigid_tbl : (string, L.var) Hashtbl.t = Hashtbl.create 16

let rigid_of (s : string) : L.var =
  match Hashtbl.find_opt rigid_tbl s with
  | Some v -> v
  | None ->
    let v = L.rigid s in
    Hashtbl.add rigid_tbl s v;
    v

let v s = L.var (rigid_of s)
let ( &&& ) a b = L.meet a b
let ( ||| ) a b = L.join a b

let%expect_test "sub_subsets eliminates exact-covered superset term" =
  (* base l = [0,1] ⊓ D.0 ⊓ E.0 *)
  let l = L.const (c 0 1) &&& v "D.0" &&& v "E.0" in
  (* coeff h includes extra D.1: [0,1] ⊓ D.0 ⊓ E.0 ⊓ D.1 *)
  let h = L.const (c 0 1) &&& v "D.0" &&& v "E.0" &&& v "D.1" in
  let r = L.sub_subsets h l in
  print_endline (L.pp r);
  [%expect {| ⊥ |}]

let%expect_test "sub_subsets leaves disjoint term untouched" =
  let l = L.const (c 0 1) &&& v "D.0" &&& v "E.0" in
  let h = L.const (c 0 1) &&& v "D.1" in
  let r = L.sub_subsets h l in
  print_endline (L.pp r);
  [%expect {| [0,1] ⊓ D.1 |}]

let%expect_test "sub_subsets eliminates only the covered component of a join" =
  let l = L.const (c 0 1) &&& v "D.0" &&& v "E.0" in
  let t1 = L.const (c 0 1) &&& v "D.0" &&& v "E.0" &&& v "D.1" in
  let t2 = L.const (c 1 0) &&& v "D.1" in
  let h = t1 ||| t2 in
  let r = L.sub_subsets h l in
  print_endline (L.pp r);
  [%expect {| [1,0] ⊓ D.1 |}]

let%expect_test "D slot1: subtracts base-gated terms" =
  let base = L.const (c 0 1) &&& v "D.0" &&& v "E.0" in
  let t_cov = L.const (c 0 1) &&& v "D.0" &&& v "D.1" &&& v "E.0" in
  let t1 = L.const (c 0 1) &&& v "D.1" &&& v "E.2" in
  let t2 = L.const (c 1 0) &&& v "D.1" in
  let h = t_cov ||| t1 ||| t2 in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| ([0,1] ⊓ D.1 ⊓ E.2) ⊔ ([1,0] ⊓ D.1) |}]

let%expect_test "D slot2: subtracts base-gated terms" =
  let base = L.const (c 0 1) &&& v "D.0" &&& v "E.0" in
  let t_cov = L.const (c 0 1) &&& v "D.0" &&& v "D.2" &&& v "E.0" in
  let t_keep =
    L.const (c 0 1) &&& v "D.1" &&& v "D.2" &&& v "E.1" &&& v "E.2"
  in
  let h = t_cov ||| t_keep in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| [0,1] ⊓ D.1 ⊓ D.2 ⊓ E.1 ⊓ E.2 |}]

let%expect_test "S slot1: subtracts S.0-gated terms" =
  let base =
    L.const (c 0 1)
    &&& v "S.0"
    &&& v "U.0"
    ||| (L.const (c 1 0) &&& v "S.0" &&& v "T.0")
  in
  let t_cov1 = L.const (c 0 1) &&& v "S.0" &&& v "S.1" &&& v "U.0" in
  let t_cov2 = L.const (c 1 0) &&& v "S.0" &&& v "S.1" &&& v "T.0" in
  let t_keep1 = L.const (c 0 1) &&& v "S.1" &&& v "U.1" in
  let t_keep2 = L.const (c 1 0) &&& v "S.1" &&& v "T.1" in
  let h = t_cov1 ||| t_cov2 ||| t_keep1 ||| t_keep2 in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| ([0,1] ⊓ S.1 ⊓ U.1) ⊔ ([1,0] ⊓ S.1 ⊓ T.1) |}]

let%expect_test "Z3 slot2: subtract Z3.0-gated term" =
  let base = L.const (c 0 1) &&& v "Z3.0" in
  let t_cov = L.const (c 0 1) &&& v "Z3.0" &&& v "Z3.2" in
  let t_keep1 = L.const (c 0 1) &&& v "Z3.1" &&& v "Z3.2" &&& v "Z3.3" in
  let t_keep2 = L.const (c 1 0) &&& v "Z3.2" in
  let h = t_cov ||| t_keep1 ||| t_keep2 in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| ([0,1] ⊓ Z3.1 ⊓ Z3.2 ⊓ Z3.3) ⊔ ([1,0] ⊓ Z3.2) |}]

let%expect_test "CA2 slot2: subtract CA2.0-gated term" =
  let base = L.const (c 1 0) &&& v "CA2.0" in
  let t_cov = L.const (c 1 0) &&& v "CA2.0" &&& v "CA2.2" in
  let t_keep = L.const (c 1 0) &&& v "CA2.1" &&& v "CA2.2" in
  let h = t_cov ||| t_keep in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| [1,0] ⊓ CA2.1 ⊓ CA2.2 |}]

let%expect_test "U slot1: subtract U.0-gated terms" =
  let base =
    L.const (c 0 1)
    &&& v "U.0"
    &&& v "W.0"
    ||| (L.const (c 1 0) &&& v "U.0" &&& v "V.0")
  in
  let t_cov1 = L.const (c 0 1) &&& v "U.0" &&& v "U.1" &&& v "W.0" in
  let t_cov2 = L.const (c 1 0) &&& v "U.0" &&& v "U.1" &&& v "V.0" in
  let t_keep = L.const (c 1 0) &&& v "U.1" &&& v "V.1" in
  let h = t_cov1 ||| t_cov2 ||| t_keep in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| [1,0] ⊓ U.1 ⊓ V.1 |}]

let%expect_test "P slot1: subtract P.0∧Q.0-gated term" =
  let base = L.const (c 1 0) &&& v "P.0" &&& v "Q.0" in
  let t_cov = L.const (c 1 0) &&& v "P.0" &&& v "P.1" &&& v "Q.0" in
  let t_keep = L.const (c 1 1) &&& v "P.1" &&& v "Q.1" in
  let h = t_cov ||| t_keep in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| [1,1] ⊓ P.1 ⊓ Q.1 |}]

let%expect_test "H2 slot2: subtract F2.0∧H2.0-gated term" =
  let base = L.const (c 0 1) &&& v "F2.0" &&& v "H2.0" in
  let t_cov = L.const (c 0 1) &&& v "F2.0" &&& v "H2.0" &&& v "H2.2" in
  let t_keep = L.const (c 0 1) &&& v "F2.1" &&& v "H2.1" &&& v "H2.2" in
  let h = t_cov ||| t_keep in
  let r = L.sub_subsets h base in
  print_endline (L.pp r);
  [%expect {| [0,1] ⊓ F2.1 ⊓ H2.1 ⊓ H2.2 |}];
  print_endline (L.pp h);
  [%expect {| ([0,1] ⊓ F2.0 ⊓ H2.0 ⊓ H2.2) ⊔ ([0,1] ⊓ F2.1 ⊓ H2.1 ⊓ H2.2) |}];
  print_endline (L.pp base);
  [%expect {| [0,1] ⊓ F2.0 ⊓ H2.0 |}]
