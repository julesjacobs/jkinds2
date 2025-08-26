open Jkinds_lib

let () =
  let content =
    "type A('a1) : 'a1\n"
    ^ "type M2('a1) = [0,1] * 'a1\n\n"
    ^ "type CN('a1,'a2) = A('a1) + M2('a2)\n"
  in
  let prog = Decl_parser.parse_program_exn content in
  let pp_params (ps : Type_parser.cyclic list) : string =
    ps
    |> List.map (fun c -> string_of_int c.Type_parser.id)
    |> String.concat ","
  in
  let rec collect_cvars acc (c : Type_parser.cyclic) =
    match c.node with
    | Type_parser.CVar i -> (i, c.id) :: acc
    | Type_parser.CUnit | Type_parser.CMod_const _ -> acc
    | Type_parser.CMod_annot (t, _) -> collect_cvars acc t
    | Type_parser.CPair (a, b) | Type_parser.CSum (a, b) ->
      collect_cvars (collect_cvars acc a) b
    | Type_parser.CCtor (_n, args) -> List.fold_left collect_cvars acc args
  in
  List.iter
    (fun (it : Decl_parser.decl_item) ->
      let cvars = collect_cvars [] it.rhs_cyclic |> List.rev in
      Printf.printf "%s params: [%s]\n" it.name (pp_params it.params);
      List.iter
        (fun (i, id) -> Printf.printf "  body 'a%d -> id %d\n" i id)
        cvars)
    prog
