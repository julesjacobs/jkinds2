open Jkinds_lib

let print_m m = print_endline (Modality.pp m)

let%expect_test "single constant prints without braces" =
  let m = Modality.of_levels [| 1; 0 |] in
  print_m m;
  [%expect {| [1,0] |}]

let%expect_test "single identifier prints without braces" =
  let m = Modality.of_atom { Modality.ctor = "C"; index = 1 } in
  print_m m;
  [%expect {| C.1 |}]

let%expect_test "coeff meet identifier is parenthesized without outer braces" =
  let c10 = Modality.of_levels [| 1; 0 |] in
  let a = Modality.of_atom { Modality.ctor = "C"; index = 1 } in
  let m = Modality.compose c10 a in
  print_m m;
  [%expect {| ([1,0] ⊓ C.1) |}]

let%expect_test
    "join of two terms prints with parens around join and inner parens per term"
    =
  let c10 = Modality.of_levels [| 1; 0 |] in
  let a = Modality.of_atom { Modality.ctor = "C"; index = 1 } in
  let b = Modality.of_atom { Modality.ctor = "D"; index = 2 } in
  let t1 = Modality.compose c10 a in
  let t2 = Modality.compose c10 b in
  let m = Modality.max t1 t2 in
  print_m m;
  [%expect {| (([1,0] ⊓ C.1) ⊔ ([1,0] ⊓ D.2)) |}]
