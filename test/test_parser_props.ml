open Jkinds_lib

let assert_true msg b = if not b then failwith ("assert: " ^ msg)

(* Generate small random type ASTs and check pp/parse roundtrip. *)
let () =
  let rec gen_type depth : Type_syntax.t =
    let open Type_syntax in
    if depth = 0 then
      match Random.int 3 with
      | 0 -> Unit
      | 1 -> Var (Random.int 3)
      | _ -> C ("X", [])
    else
      match Random.int 6 with
      | 0 -> Unit
      | 1 -> Var (Random.int 3)
      | 2 -> C ("C", [ gen_type (depth - 1) ])
      | 3 -> C ("D", [ gen_type (depth - 1); gen_type (depth - 1) ])
      | 4 -> Pair (gen_type (depth - 1), gen_type (depth - 1))
      | _ -> Sum (gen_type (depth - 1), gen_type (depth - 1))
  in
  Random.init 1337;
  for _ = 1 to 500 do
    let t = gen_type 3 in
    let s = Type_syntax.pp t in
    match Type_parser.parse s with
    | Error e -> failwith ("parse error on pp string: " ^ e ^ " from: " ^ s)
    | Ok t' -> assert_true "pp/parse roundtrip" (t = t')
  done;
  print_endline "parser roundtrip property passed"
