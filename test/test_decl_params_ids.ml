open Jkinds_lib

let () =
  let prog =
    Decl_parser.parse_program_exn "type F('a1,'a2) = Pair('a1, Sum('a2, 'a1))\n"
  in
  let it = List.find (fun (d : Decl_parser.decl_item) -> d.name = "F") prog in
  (* Build a map from 'ai index to the canonical param cyclic node *)
  let params = Array.of_list it.params in
  let module H = Hashtbl in
  let visited : (int, unit) H.t = H.create 32 in
  let rec check (n : Type_parser.cyclic) : unit =
    if H.mem visited n.id then ()
    else (
      H.add visited n.id ();
      match n.node with
      | Type_parser.CUnit | Type_parser.CMod_const _ -> ()
      | Type_parser.CVar i ->
        let expected = params.(i - 1) in
        (* Assert we are literally the same node object *)
        assert (n.id = expected.id);
        assert (n == expected)
      | Type_parser.CMod_annot (t, _) -> check t
      | Type_parser.CPair (a, b) | Type_parser.CSum (a, b) ->
        check a;
        check b
      | Type_parser.CCtor (_, args) -> List.iter check args)
  in
  check it.rhs_cyclic;
  print_endline "✓ decl params share cyclic CVar nodes with body"
