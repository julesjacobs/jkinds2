let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let () =
  let open Jkinds_lib in
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: print_cvars <file.types>";
    exit 2);
  let content = read_file Sys.argv.(1) in
  let prog = Decl_parser.parse_program_exn content in
  let pp_params (ps : Type_parser.cyclic list) : string =
    ps
    |> List.map (fun c -> string_of_int c.Type_parser.id)
    |> String.concat ","
  in
  let rec collect acc (c : Type_parser.cyclic) =
    match c.node with
    | Type_parser.CVar i -> (i, c.id) :: acc
    | Type_parser.CUnit | Type_parser.CMod_const _ -> acc
    | Type_parser.CMod_annot (t, _) -> collect acc t
    | Type_parser.CPair (a, b) | Type_parser.CSum (a, b) ->
      collect (collect acc a) b
    | Type_parser.CCtor (_n, args) -> List.fold_left collect acc args
  in
  List.iter
    (fun (it : Decl_parser.decl_item) ->
      Printf.printf "%s params: [%s]\n" it.name (pp_params it.params);
      let cvars = collect [] it.rhs_cyclic |> List.rev in
      List.iter
        (fun (i, id) -> Printf.printf "  body 'a%d -> id %d\n" i id)
        cvars)
    prog
