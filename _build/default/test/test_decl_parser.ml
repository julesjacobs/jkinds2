open Jkinds_lib

let () =
  let open Decl_parser in
  let decls =
    "type F('a1,'a2) = G('a1, H('a2))\n" ^
    "type G('a1,'a2) = 'a1\n" ^
    "type H('a1) = F('a1, 'a1)" in
  let m = parse_exn decls in
  assert (NameMap.cardinal m = 3);
  assert (NameMap.mem "F" m);
  assert (NameMap.mem "G" m);
  assert (NameMap.mem "H" m);
  let open Type_syntax in
  assert (NameMap.find "G" m = Var 1);
  assert (NameMap.find "H" m = C ("F", [ Var 1; Var 1 ]));
  let km = Infer.kinds_of_decls_bindings (NameMap.bindings m) in
  assert (List.length km = 3);
  let dup = "type X('a1) = 'a1\n" ^ "type X('a1) = 'a1" in
  (match Decl_parser.parse dup with
  | Ok _ -> failwith "expected duplicate declaration error"
  | Error _ -> print_endline "✓ decl duplicate error test passed");
  let oos1 = "type Y('a1) = 'a2" in
  (match Decl_parser.parse oos1 with
  | Ok _ -> failwith "expected out-of-scope variable error"
  | Error _ -> print_endline "✓ decl out-of-scope error test passed");
  let a0bad = "type Z('a1) = 'a0" in
  (match Decl_parser.parse a0bad with
  | Ok _ -> failwith "expected 'a0 forbidden error"
  | Error _ -> print_endline "✓ decl 'a0 forbidden error test passed");

