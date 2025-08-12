open Jkinds_lib

let () =
  let decls = "type list('a1) = either(nil(), cons('a1, list('a1)))" in
  let m = Jkinds_lib.Decl_parser.parse_exn decls in
  let m_bindings = Jkinds_lib.Decl_parser.NameMap.bindings m in
  let km_bindings = Jkinds_lib.Infer.kinds_of_decls_bindings m_bindings in
  let list_kind = List.assoc "list" km_bindings in
  Printf.printf "list kind (from RHS): %s\n" (Kind.pp list_kind);
  let lfp_bindings = Infer.least_fixpoint_bindings km_bindings in
  let list_kind_lfp = List.assoc "list" lfp_bindings in
  Printf.printf "list kind after LFP: %s\n" (Kind.pp list_kind_lfp);
  print_endline "✓ list demo completed"

