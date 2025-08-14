open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert: " ^ msg)

let () =
  let open Type_parser in
  let p s = match parse s with Ok t -> t | Error e -> failwith e in
  let round s = Type_syntax.pp (p s) in
  (* Round-trip basic *)
  assert_true "pp roundtrip 1" (round "'a1 @@ [2,1]" = "'a1 @@ [2,1]");
  assert_true "pp roundtrip 2" (round "(unit + 'a1) @@ [1,0]" = "(unit + 'a1) @@ [1,0]");
  (* Inference: annotation meets into kind *)
  let t = parse_exn "F('a1) @@ [1,0]" in
  let k = Infer.kindof t in
  let s = Kind.pp k in
  assert_true "infer contains coeff" (String.contains s '[');
  print_endline "✓ mod_annot tests passed"


