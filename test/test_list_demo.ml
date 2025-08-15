open Jkinds_lib

let () =
  let decls = "type list('a1) = either(nil(), cons('a1, list('a1)))" in
  let prog = Jkinds_lib.Decl_parser.parse_program_exn decls in
  let solved = Jkinds_lib.Infer.solve_program prog ~max_iters:2 in
  let list_kind_lfp = List.assoc "list" solved in
  Printf.printf "list kind after LFP: %s\n" (Kind.pp list_kind_lfp);
  print_endline "✓ list demo completed"

