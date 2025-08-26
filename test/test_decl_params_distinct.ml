open Jkinds_lib

let () =
  let decls = "type A('a1,'a2) = 'a1\n" ^ "type B('a1,'a2) = 'a2\n" in
  let prog = Decl_parser.parse_program_exn decls in
  let a = List.find (fun (d : Decl_parser.decl_item) -> d.name = "A") prog in
  let b = List.find (fun (d : Decl_parser.decl_item) -> d.name = "B") prog in
  assert (List.length a.params = 2);
  assert (List.length b.params = 2);
  let a1 = List.nth a.params 0 in
  let a2 = List.nth a.params 1 in
  let b1 = List.nth b.params 0 in
  let b2 = List.nth b.params 1 in
  (* Distinct across decls *)
  assert (a1.id <> b1.id);
  assert (a2.id <> b2.id);
  assert (a1 != b1);
  assert (a2 != b2);
  (* Within-decl params are also distinct *)
  assert (a1.id <> a2.id);
  assert (b1.id <> b2.id);
  print_endline "✓ decl params have distinct ids across declarations"
