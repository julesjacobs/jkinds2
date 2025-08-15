open Jkinds_lib

let () =
  let open Decl_parser in
  let decls =
    "type F('a1,'a2) = G('a1, H('a2))\n" ^
    "type G('a1,'a2) = 'a1\n" ^
    "type H('a1) = F('a1, 'a1)" in
  let prog = parse_program_exn decls in
  assert (List.length prog = 3);
  assert (List.exists (fun it -> it.name = "F") prog);
  assert (List.exists (fun it -> it.name = "G") prog);
  assert (List.exists (fun it -> it.name = "H") prog);
  let open Type_syntax in
  let find_rhs n = (List.find (fun it -> it.name = n) prog).rhs in
  assert (find_rhs "G" = Var 1);
  assert (find_rhs "H" = C ("F", [ Var 1; Var 1 ]));
  let km = Infer.kinds_of_decls_bindings (List.map (fun it -> (it.name, it.rhs)) prog) in
  assert (List.length km = 3);
  let dup = "type X('a1) = 'a1\n" ^ "type X('a1) = 'a1" in
  (match (try Ok (parse_program_exn dup) with Parse_error msg -> Error msg) with
  | Ok _ -> failwith "expected duplicate declaration error"
  | Error _ -> print_endline "✓ decl duplicate error test passed");
  let oos1 = "type Y('a1) = 'a2" in
  (match (try Ok (parse_program_exn oos1) with Parse_error msg -> Error msg) with
  | Ok _ -> failwith "expected out-of-scope variable error"
  | Error _ -> print_endline "✓ decl out-of-scope error test passed");
  let a0bad = "type Z('a1) = 'a0" in
  (match (try Ok (parse_program_exn a0bad) with Parse_error msg -> Error msg) with
  | Ok _ -> failwith "expected 'a0 forbidden error"
  | Error _ -> print_endline "✓ decl 'a0 forbidden error test passed");

