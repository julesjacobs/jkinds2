open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert: " ^ msg)

let () =
  let p s =
    match Type_menhir_driver.parse_mu s with
    | Ok m -> (
      match Type_parser.to_simple m with Ok t -> t | Error e -> failwith e)
    | Error e -> failwith e
  in
  let round s = Type_syntax.pp (p s) in
  (* Round-trip basic *)
  assert_true "pp roundtrip 1" (round "'a1 @@ [2,1]" = "'a1 @@ [2,1]");
  assert_true "pp roundtrip 2"
    (round "(unit + 'a1) @@ [1,0]" = "(unit + 'a1) @@ [1,0]");
  (* Ensure parser accepts mod annotations; inference not tested here *)
  let _ =
    match Type_menhir_driver.parse_mu "F('a1) @@ [1,0]" with
    | Ok m -> ignore (Type_parser.to_simple_exn m)
    | Error e -> failwith e
  in
  print_endline "✓ mod_annot tests passed"
