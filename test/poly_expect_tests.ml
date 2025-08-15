open Jkinds_lib

module C = Product_lattice.Make (struct
  let axis_sizes = [| 3; 2 |]
end)

let axis_names = [| "a"; "b" |]
let show_c (x : C.t) = C.pp ~axis_names x

module V = struct
  type t = string

  let compare = String.compare
end

module P = Lattice_polynomial.Make (C) (V)

let pp_poly = P.pp ~pp_var:(fun s -> s) ~pp_coeff:show_c
let printp p = print_endline (pp_poly p)
let c a b = C.encode ~levels:[| a; b |]
let x = P.var "x"
let y = P.var "y"
let z = P.var "z"

let%expect_test "canonicalization eliminates supersets" =
  let p =
    P.join
      (P.const (c 1 1))
      (P.join
         (P.meet (P.const (c 2 1)) x)
         (P.meet (P.const (c 2 1)) (P.meet x y)))
  in
  printp p;
  [%expect {| {{{a=1, b=1}} ⊔ {{a=2, b=0} ⊓ x}} |}]

let%expect_test "distribution of meet over join" =
  let p = P.meet (P.const (c 2 0)) (P.join x y) in
  printp p;
  [%expect {| {{{a=2, b=0} ⊓ x} ⊔ {{a=2, b=0} ⊓ y}} |}]

let%expect_test "subst: x := (c11 /\\ z) \\u2228 c20" =
  let p = P.join (P.const (c 0 1)) (P.meet (P.const (c 2 1)) x) in
  let sub_x = P.join (P.meet (P.const (c 1 1)) z) (P.const (c 2 0)) in
  let p' = P.subst ~subs:(P.VarMap.add "x" sub_x P.VarMap.empty) p in
  printp p';
  [%expect {| {{{a=2, b=1}}} |}]
