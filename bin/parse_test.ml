open Jkinds_lib

let () =
  let s = Sys.argv.(1) in
  match Type_menhir_driver.parse_mu s with
  | Ok m -> (
    match Type_parser.to_simple m with
    | Ok t -> print_endline (Type_syntax.pp t)
    | Error e ->
      prerr_endline ("lowering error: " ^ e);
      exit 2)
  | Error e ->
    prerr_endline ("parse error: " ^ e);
    exit 2
