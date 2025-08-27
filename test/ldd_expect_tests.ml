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
  [%expect {| y |}]

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
