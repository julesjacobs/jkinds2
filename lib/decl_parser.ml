module NameMap = Map.Make (String)

exception Parse_error of string

type decl_item = {
  name : string;
  arity : int;
  rhs : Type_syntax.t;
  abstract : bool;
}

type program = decl_item list

let parse_program_exn (s : string) : decl_item list =
  let lines =
    s
    |> String.split_on_char '\n'
    |> List.filter (fun l ->
           let t = String.trim l in
           String.length t >= 5 && String.sub t 0 5 = "type ")
  in
  let parse_line (line : string) : decl_item =
    (* Accept either '=' (concrete) or ':' (abstract) separators. *)
    let line = String.trim line in
    let prefix = "type " in
    if
      not
        (String.length line >= String.length prefix
        && String.sub line 0 (String.length prefix) = prefix)
    then raise (Parse_error "line must start with 'type '");
    let rest =
      String.sub line (String.length prefix)
        (String.length line - String.length prefix)
    in
    let eq_opt = String.index_opt rest '=' in
    let colon_opt = String.index_opt rest ':' in
    let sep_idx, abstract =
      match (eq_opt, colon_opt) with
      | Some i, None -> (i, false)
      | None, Some i -> (i, true)
      | Some _, Some _ ->
        raise (Parse_error "declaration must use exactly one of '=' or ':'")
      | None, None -> raise (Parse_error "missing '=' or ':' in declaration")
    in
    let lhs = String.trim (String.sub rest 0 sep_idx) in
    let rhs =
      String.trim
        (String.sub rest (sep_idx + 1) (String.length rest - sep_idx - 1))
    in
    (* Parse LHS: C('a1,'a2,...) or C() or C *)
    let name, args_str =
      match String.index_opt lhs '(' with
      | None -> (String.trim lhs, None)
      | Some i ->
        if String.length lhs < i + 1 then raise (Parse_error "bad lhs");
        let name = String.sub lhs 0 i |> String.trim in
        if lhs.[String.length lhs - 1] <> ')' then
          raise (Parse_error "missing ')' in lhs");
        let inner = String.sub lhs (i + 1) (String.length lhs - i - 2) in
        (name, Some inner)
    in
    let arity =
      match args_str with
      | None -> 0
      | Some inner ->
        let args =
          inner
          |> String.split_on_char ','
          |> List.map String.trim
          |> List.filter (fun s -> s <> "")
        in
        let expected_args =
          args |> List.mapi (fun i _ -> Printf.sprintf "'a%d" (i + 1))
        in
        if args <> expected_args then
          raise
            (Parse_error
               (Printf.sprintf "constructor %s arguments must be exactly %s"
                  name
                  (String.concat ", " expected_args)));
        List.length args
    in
    let rhs_t =
      match Type_parser.parse rhs with
      | Ok t -> t
      | Error e -> raise (Parse_error ("type expression: " ^ e))
    in
    let rec check_vars (t : Type_syntax.t) : unit =
      match t with
      | Type_syntax.Var n ->
        if n < 1 || n > arity then
          raise (Parse_error (Printf.sprintf "variable 'a%d is out of scope" n))
      | Type_syntax.C (_, args) -> List.iter check_vars args
      | Type_syntax.Unit -> ()
      | Type_syntax.Mod_annot (t', _) -> check_vars t'
      | Type_syntax.Mod_const _ -> ()
      | Type_syntax.Pair (a, b) | Type_syntax.Sum (a, b) ->
        check_vars a;
        check_vars b
    in
    check_vars rhs_t;
    { name; arity; rhs = rhs_t; abstract }
  in
  let items = List.map parse_line lines in
  (* Check duplicates *)
  let _ =
    List.fold_left
      (fun acc it ->
        if NameMap.mem it.name acc then
          raise (Parse_error ("duplicate declaration: " ^ it.name))
        else NameMap.add it.name () acc)
      NameMap.empty items
  in
  items
