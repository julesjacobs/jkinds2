open Jkinds_lib

let assert_equal msg a b =
  if a <> b then failwith (Printf.sprintf "Assert failed: %s" msg)

let () =
  let open Type_parser in
  let p s = match parse s with Ok t -> t | Error e -> failwith ("parse error: " ^ e) in
  let t = p "F( 'a1 , G('a2, H()), 'a3 )" in
  let expected = Type_syntax.C ("F", [ Type_syntax.Var 1; Type_syntax.C ("G", [ Type_syntax.Var 2; Type_syntax.C ("H", []) ]); Type_syntax.Var 3 ]) in
  assert (t = expected);
  let t2 = p "'a0" in
  assert (t2 = Type_syntax.Var 0);
  print_endline "✓ parser tests passed";
  let parse_kind_pp s =
    let t = Type_parser.parse_exn s in
    let k = Infer.kindof t in
    Kind.pp k
  in
  (* Adjust to ctor-qualified kind pp and ⊤/⊥ printing *)
  assert_equal "pp kind of 'a5" (parse_kind_pp "'a5") "{'a5 -> ⊤}";
  assert_equal "pp kind of F('a5)" (parse_kind_pp "F('a5)") "{'a0 -> {{F.0}}, 'a5 -> {{F.1}}}";
  assert_equal "pp kind of F('a1, G('a2))" (parse_kind_pp "F('a1, G('a2))") "{'a0 -> {{F.0} ⊔ {F.2 ⊓ G.0}}, 'a1 -> {{F.1}}, 'a2 -> {{F.2 ⊓ G.1}}}";

