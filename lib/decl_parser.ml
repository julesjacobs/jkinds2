module NameMap = Map.Make (String)



exception Parse_error of string

let parse_exn (s : string) : Type_syntax.t NameMap.t =
  let lines =
    s
    |> String.split_on_char '\n'
    |> List.filter (fun l -> String.trim l <> "")
  in
  let parse_line (line : string) : string * Type_syntax.t =
    (* Expect: type C('a1,'a2,...) = <type_expr> *)
    let line = String.trim line in
    let prefix = "type " in
    if not (String.length line >= String.length prefix && String.sub line 0 (String.length prefix) = prefix)
    then raise (Parse_error "line must start with 'type '");
    let rest = String.sub line (String.length prefix) (String.length line - String.length prefix) in
    (* Split on '=' first *)
    let eq_idx =
      try String.index rest '=' with Not_found -> raise (Parse_error "missing '=' in declaration")
    in
    let lhs = String.trim (String.sub rest 0 eq_idx) in
    let rhs = String.trim (String.sub rest (eq_idx + 1) (String.length rest - eq_idx - 1)) in
    (* Parse LHS: C('a1,'a2,...) or C() or C *)
    let name, args_str =
      match String.index_opt lhs '(' with
      | None -> (String.trim lhs, None)
      | Some i ->
          if String.length lhs < i + 1 then raise (Parse_error "bad lhs");
          let name = String.sub lhs 0 i |> String.trim in
          if lhs.[String.length lhs - 1] <> ')' then raise (Parse_error "missing ')' in lhs");
          let inner = String.sub lhs (i + 1) (String.length lhs - i - 2) in
          (name, Some inner)
    in
    let k =
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
            args
            |> List.mapi (fun i _ -> Printf.sprintf "'a%d" (i + 1))
          in
          if args <> expected_args then
            raise
              (Parse_error
                 (Printf.sprintf
                    "constructor %s arguments must be exactly %s"
                    name
                    (String.concat ", " expected_args)));
          List.length args
    in
    (* Parse RHS type expression using existing type parser *)
    let t =
      match Type_parser.parse rhs with
      | Ok t -> t
      | Error e -> raise (Parse_error ("type expression: " ^ e))
    in
    (* Ensure only 'a1..'ak are used on RHS *)
    let rec check_vars (t : Type_syntax.t) : unit =
      match t with
      | Type_syntax.Var n ->
          if n < 1 || n > k then
            raise (Parse_error (Printf.sprintf "variable 'a%d is out of scope" n))
      | Type_syntax.C (_, args) -> List.iter check_vars args
      | Type_syntax.Unit -> ()
      | Type_syntax.Mod_annot (t', _) -> check_vars t'
      | Type_syntax.Pair (a, b) | Type_syntax.Sum (a, b) ->
          check_vars a; check_vars b
    in
    check_vars t;
    (name, t)
  in
  List.fold_left
    (fun acc line ->
      let name, t = parse_line line in
      if NameMap.mem name acc then raise (Parse_error ("duplicate declaration: " ^ name));
      NameMap.add name t acc)
    NameMap.empty lines

let parse s = try Ok (parse_exn s) with Parse_error msg -> Error msg

