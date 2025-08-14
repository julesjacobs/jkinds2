open Jkinds_lib

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s


let () =
  let usage = "Usage: jkinds <typedef-file> [--max-iters N]" in
  let argc = Array.length Sys.argv in
  if argc < 2 then (prerr_endline usage; exit 2);
  let file = Sys.argv.(1) in
  let max_iters =
    let rec find_flag i =
      if i + 1 < argc then
        if Sys.argv.(i) = "--max-iters" then int_of_string Sys.argv.(i + 1)
        else find_flag (i + 1)
      else 10
    in
    find_flag 2
  in
  let content = read_file file in
  let decls = Decl_parser.parse_exn content in
  let decl_items = Decl_parser.parse_items_exn content in
  let abstracts = Decl_parser.abstract_ctors decl_items in
  let decls_bindings = Decl_parser.NameMap.bindings decls in
  let kinds = Infer.kinds_of_decls_bindings decls_bindings in
  print_endline "Kinds:";
  List.iter (fun (n,k) -> Printf.printf "%s: %s\n" n (Kind.pp k)) kinds;
  print_endline "\nLeast fixpoint kinds:";
  let kinds_lfp = Infer.least_fixpoint_bindings_with_self_init ~max_iters ~abstracts kinds in
  List.iter (fun (n,k) -> Printf.printf "%s: %s\n" n (Kind.pp k)) kinds_lfp;
  ()