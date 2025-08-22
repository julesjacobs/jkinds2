open Jkinds_lib

let%expect_test "mu parsing and cyclic pp: simple self" =
  let open Type_parser in
  let mu =
    match Type_menhir_driver.parse_mu "mu 'b1. C('a1, 'b1)" with
    | Ok m -> to_cyclic m
    | Error e -> failwith e
  in
  print_endline (pp_cyclic mu);
  [%expect {| #1=C('a1, #1) |}]

let%expect_test "mu parsing and cyclic pp: nested with annotation" =
  let open Type_parser in
  let mu =
    match Type_menhir_driver.parse_mu "mu 'b1. (D('b1) @@ [0,1]) + E('a2)" with
    | Ok m -> to_cyclic m
    | Error e -> failwith e
  in
  print_endline (pp_cyclic mu);
  [%expect {| #1=(D(#1) @@ [0,1] + E('a2)) |}]

let%expect_test "mu parsing and cyclic pp: two-level cycle" =
  let open Type_parser in
  let mu =
    match Type_menhir_driver.parse_mu "mu 'b1. F(mu 'b2. G('b1,'b2))" with
    | Ok m -> to_cyclic m
    | Error e -> failwith e
  in
  print_endline (pp_cyclic mu);
  [%expect {| #1=F(#2=G(#1, #2)) |}]
