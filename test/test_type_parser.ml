open Jkinds_lib

let () =
  let open Type_parser in
  let p s =
    match parse s with Ok t -> t | Error e -> failwith ("parse error: " ^ e)
  in
  let t = p "F( 'a1 , G('a2, H()), 'a3 )" in
  let expected =
    Type_syntax.C
      ( "F",
        [
          Type_syntax.Var 1;
          Type_syntax.C ("G", [ Type_syntax.Var 2; Type_syntax.C ("H", []) ]);
          Type_syntax.Var 3;
        ] )
  in
  assert (t = expected);
  let t2 = p "'a0" in
  assert (t2 = Type_syntax.Var 0);
  print_endline "✓ parser tests passed"
(* Basic parser-only tests; kind inference no longer exported here *)
