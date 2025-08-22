open Type_syntax

let make_var (v : int) : t = Var v
let make_c (name : string) (args : t list) : t = C (name, args)

type token =
  | Ident of string
  | Int_lit of int
  | Quote
  | Dot
  | Lparen
  | Rparen
  | Comma
  | Lbrack
  | Rbrack
  | Star
  | Plus
  | AtAt
  | Eof

let is_ident_start c =
  match c with 'A' .. 'Z' | 'a' .. 'z' | '_' -> true | _ -> false

let is_ident_char c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let tokenize (s : string) : (token list, string) result =
  let len = String.length s in
  let rec skip i =
    if i < len then
      match s.[i] with ' ' | '\n' | '\r' | '\t' -> skip (i + 1) | _ -> i
    else i
  in
  let rec lex i acc =
    let i = skip i in
    if i >= len then Ok (List.rev (Eof :: acc))
    else
      match s.[i] with
      | '[' -> lex (i + 1) (Lbrack :: acc)
      | ']' -> lex (i + 1) (Rbrack :: acc)
      | '.' -> lex (i + 1) (Dot :: acc)
      | '@' when i + 1 < len && s.[i + 1] = '@' -> lex (i + 2) (AtAt :: acc)
      | '\'' -> lex (i + 1) (Quote :: acc)
      | '(' -> lex (i + 1) (Lparen :: acc)
      | ')' -> lex (i + 1) (Rparen :: acc)
      | ',' -> lex (i + 1) (Comma :: acc)
      | '*' -> lex (i + 1) (Star :: acc)
      | '+' -> lex (i + 1) (Plus :: acc)
      | '-' ->
        (* could be negative int; handled in int branch via sign check *)
        if i + 1 < len then (
          match s.[i + 1] with
          | '0' .. '9' ->
            let j = ref (i + 2) in
            while
              !j < len && match s.[!j] with '0' .. '9' -> true | _ -> false
            do
              incr j
            done;
            let n = int_of_string (String.sub s i (!j - i)) in
            lex !j (Int_lit n :: acc)
          | _ ->
            (* allow '-' in identifiers, fall through to ident lexing *)
            let j = ref (i + 1) in
            while !j < len && is_ident_char s.[!j] do
              incr j
            done;
            let id = String.sub s i (!j - i) in
            lex !j (Ident id :: acc))
        else lex (i + 1) (Ident "-" :: acc)
      | '0' .. '9' ->
        let j = ref (i + 1) in
        while !j < len && match s.[!j] with '0' .. '9' -> true | _ -> false do
          incr j
        done;
        let n = int_of_string (String.sub s i (!j - i)) in
        lex !j (Int_lit n :: acc)
      | c when is_ident_start c ->
        let j = ref (i + 1) in
        while !j < len && is_ident_char s.[!j] do
          incr j
        done;
        let id = String.sub s i (!j - i) in
        lex !j (Ident id :: acc)
      | c -> Error (Printf.sprintf "Unexpected character: %c" c)
  in
  lex 0 []

(* Grammar (whitespace ignored): Type := Constr | Var Constr := IDENT [ '(' Type
   (',' Type)* ')' ] Var := 'a' INT (* integers; 'a-1' means a0, reserved
   special *) We also allow bare INT as var for convenience; 'a' prefix is
   optional. *)

exception Parse_error of string

(* Extended: recursive types with mu / &'bN *)

type mu_raw =
  | UnitR
  | PairR of mu_raw * mu_raw
  | SumR of mu_raw * mu_raw
  | CR of string * mu_raw list
  | VarR of int (* 'aN *)
  | ModAnnotR of mu_raw * int array
  | ModConstR of int array
  | MuR of int * mu_raw (* mu 'bN. body *)
  | RecvarR of int (* &'bN *)

let parse_mu_exn s =
  let fail msg = raise (Parse_error msg) in
  let tokens =
    match tokenize s with Ok t -> t | Error e -> fail ("lex: " ^ e)
  in
  let rec parse_type i =
    let lhs, j = parse_primary i in
    parse_bin_tail lhs j
  and parse_primary i =
    match List.nth tokens i with
    | Ident "mu" -> parse_mu_binder (i + 1)
    | Quote -> parse_quote_mu i
    | Ident id -> (
      (* Could be constructor or 'aN var *)
      let as_a_var =
        let n = String.length id in
        if n > 1 && id.[0] = 'a' then
          try Some (int_of_string (String.sub id 1 (n - 1))) with _ -> None
        else None
      in
      match as_a_var with
      | Some v -> (VarR v, i + 1)
      | None -> parse_after_ident id (i + 1))
    | Int_lit v -> (VarR v, i + 1)
    | Lparen -> (
      let t, j = parse_type (i + 1) in
      match List.nth tokens j with
      | Rparen -> (t, j + 1)
      | _ -> fail "expected ')' after parenthesized type")
    | Lbrack ->
      let rec parse_levels k acc =
        match List.nth tokens k with
        | Int_lit n -> parse_levels (k + 1) (n :: acc)
        | Comma -> parse_levels (k + 1) acc
        | Rbrack -> (List.rev acc, k + 1)
        | _ -> fail "expected ']' after modality literal"
      in
      let levels, j = parse_levels (i + 1) [] in
      (ModConstR (Array.of_list levels), j)
    | Rparen | Rbrack | Comma | Star | Plus | AtAt | Dot | Eof ->
      fail "expected type"
  and parse_after_ident name i =
    match List.nth tokens i with
    | Lparen ->
      let args, j = parse_args (i + 1) in
      if String.equal name "a" then
        match args with
        | [ VarR v ] -> (VarR v, j)
        | _ -> fail "expected a(<int>)"
      else if String.equal name "unit" && args = [] then (UnitR, j)
      else (CR (name, args), j)
    | _ -> ( match name with "unit" -> (UnitR, i) | _ -> (CR (name, []), i))
  and parse_quote_mu i =
    (* parse 'aN or 'bN *)
    match List.nth tokens (i + 1) with
    | Ident id ->
      let n = String.length id in
      if n >= 1 then
        match id.[0] with
        | 'a' -> (
          if n = 1 then
            match List.nth tokens (i + 2) with
            | Int_lit v -> (VarR v, i + 3)
            | _ -> fail "expected number after 'a"
          else
            try (VarR (int_of_string (String.sub id 1 (n - 1))), i + 2)
            with _ -> fail "invalid 'a<number> form")
        | 'b' -> (
          if n = 1 then
            match List.nth tokens (i + 2) with
            | Int_lit v -> (RecvarR v, i + 3)
            | _ -> fail "expected number after 'b"
          else
            try (RecvarR (int_of_string (String.sub id 1 (n - 1))), i + 2)
            with _ -> fail "invalid 'b<number> form")
        | _ -> fail "expected a or b after '\''"
      else fail "expected identifier after '\''"
    | _ -> fail "expected a or b after '\''"
  and parse_mu_binder i =
    (* mu 'bN . type *)
    match List.nth tokens i with
    | Quote -> (
      match List.nth tokens (i + 1) with
      | Ident id -> (
        let n = String.length id in
        let bidx, k_after_ident =
          if n >= 1 && id.[0] = 'b' then
            if n = 1 then
              match List.nth tokens (i + 2) with
              | Int_lit v -> (v, i + 3)
              | _ -> fail "expected number after 'b"
            else
              try (int_of_string (String.sub id 1 (n - 1)), i + 2)
              with _ -> fail "invalid 'b<number> form"
          else fail "expected b after '\''"
        in
        match List.nth tokens k_after_ident with
        | Dot ->
          let body, j = parse_type (k_after_ident + 1) in
          (MuR (bidx, body), j)
        | _ -> fail "expected '.' after mu binder")
      | _ -> fail "expected identifier after '\'' in mu binder")
    | _ -> fail "expected ''' after mu"
  and parse_args i =
    match List.nth tokens i with
    | Rparen -> ([], i + 1)
    | _ ->
      let t1, j = parse_type i in
      let rec parse_more_args_rev acc_rev k =
        match List.nth tokens k with
        | Comma ->
          let t, k' = parse_type (k + 1) in
          parse_more_args_rev (t :: acc_rev) k'
        | Rparen -> (List.rev acc_rev, k + 1)
        | _ -> fail "expected ',' or ')' in argument list"
      in
      parse_more_args_rev [ t1 ] j
  and parse_bin_tail lhs i =
    match List.nth tokens i with
    | Star ->
      let rhs, j = parse_primary (i + 1) in
      parse_bin_tail (PairR (lhs, rhs)) j
    | Plus ->
      let rhs, j = parse_primary (i + 1) in
      parse_bin_tail (SumR (lhs, rhs)) j
    | AtAt -> (
      match List.nth tokens (i + 1) with
      | Lbrack ->
        let rec parse_levels k acc =
          match List.nth tokens k with
          | Int_lit n -> parse_levels (k + 1) (n :: acc)
          | Comma -> parse_levels (k + 1) acc
          | Rbrack -> (List.rev acc, k + 1)
          | _ -> fail "expected ']' after modality literal"
        in
        let levels, k = parse_levels (i + 2) [] in
        parse_bin_tail (ModAnnotR (lhs, Array.of_list levels)) k
      | _ -> fail "expected '[' after @@")
    | _ -> (lhs, i)
  in
  let t, i = parse_type 0 in
  match List.nth tokens i with Eof -> t | _ -> fail "trailing tokens"

let parse_mu s = try Ok (parse_mu_exn s) with Parse_error msg -> Error msg

(* Lowering: turn mu_raw into the simple Type_syntax.t when no mu/rec vars *)
let rec to_simple_exn (t : mu_raw) : Type_syntax.t =
  match t with
  | UnitR -> Unit
  | PairR (a, b) -> Pair (to_simple_exn a, to_simple_exn b)
  | SumR (a, b) -> Sum (to_simple_exn a, to_simple_exn b)
  | CR (name, args) -> C (name, List.map to_simple_exn args)
  | VarR v -> Var v
  | ModAnnotR (u, lv) -> Mod_annot (to_simple_exn u, lv)
  | ModConstR lv -> Mod_const lv
  | MuR _ | RecvarR _ ->
    raise (Parse_error "mu/recvar not allowed in simple types")

let to_simple (t : mu_raw) : (Type_syntax.t, string) result =
  try Ok (to_simple_exn t) with Parse_error msg -> Error msg

(* Simple parser wrapper now reuses mu parser + lowering *)
let parse_exn s =
  let m = parse_mu_exn s in
  to_simple_exn m

let parse s = match parse_mu s with Error e -> Error e | Ok m -> to_simple m

(* A cyclic graph representation to hold direct cycles via refs. *)
type cyclic_desc =
  | CUnit
  | CPair of cyclic * cyclic
  | CSum of cyclic * cyclic
  | CCtor of string * cyclic list
  | CVar of int
  | CMod_annot of cyclic * int array
  | CMod_const of int array

and cyclic = cyclic_desc ref

let to_cyclic (t : mu_raw) : cyclic =
  let rec go env t =
    match t with
    | UnitR -> ref CUnit
    | PairR (a, b) -> ref (CPair (go env a, go env b))
    | SumR (a, b) -> ref (CSum (go env a, go env b))
    | CR (name, args) -> ref (CCtor (name, List.map (go env) args))
    | VarR v -> ref (CVar v)
    | ModAnnotR (u, lv) -> ref (CMod_annot (go env u, lv))
    | ModConstR lv -> ref (CMod_const lv)
    | RecvarR bi -> (
      match List.assoc_opt bi env with
      | Some r -> r
      | None -> failwith "unbound 'b index")
    | MuR (bi, body) ->
      let hole : cyclic = ref CUnit in
      let env' = (bi, hole) :: env in
      let body_c = go env' body in
      hole := !body_c;
      hole
  in
  go [] t

let parse_mu_cyclic s =
  match parse_mu s with Ok t -> Ok (to_cyclic t) | Error e -> Error e

(* Pretty-print a cyclic value, cutting cycles with numbered anchors. We only
   introduce an anchor (#n=) when we actually detect a cycle (back-edge) to that
   node. Two-pass approach: first detect nodes that participate in any cycle,
   then print with anchors for those nodes only. *)

let pp_cyclic (root : cyclic) : string =
  (* Pass 1: detect nodes that are targets of back-edges (on recursion). *)
  let onstack : (cyclic, bool) Hashtbl.t = Hashtbl.create 64 in
  let visited : (cyclic, bool) Hashtbl.t = Hashtbl.create 64 in
  let cyclic_nodes : (cyclic, bool) Hashtbl.t = Hashtbl.create 64 in
  let rec dfs (n : cyclic) : unit =
    if Hashtbl.mem visited n then ()
    else (
      Hashtbl.add visited n true;
      Hashtbl.replace onstack n true;
      let explore child =
        if Hashtbl.mem onstack child then
          Hashtbl.replace cyclic_nodes child true
        else dfs child
      in
      (match !n with
      | CUnit | CVar _ | CMod_const _ -> ()
      | CMod_annot (t, _) -> explore t
      | CPair (a, b) | CSum (a, b) ->
        explore a;
        explore b
      | CCtor (_, args) -> List.iter explore args);
      Hashtbl.remove onstack n)
  in
  dfs root;

  (* Pass 2: print with anchors only for nodes in [cyclic_nodes]. *)
  let id_of : (cyclic, int) Hashtbl.t = Hashtbl.create 64 in
  let defined : (cyclic, bool) Hashtbl.t = Hashtbl.create 64 in

  let fresh_id =
    let r = ref 0 in
    fun () ->
      incr r;
      !r
  in

  let rec pp (n : cyclic) : string =
    let needs_anchor = Hashtbl.mem cyclic_nodes n in
    if needs_anchor then (
      let id =
        match Hashtbl.find_opt id_of n with
        | Some k -> k
        | None ->
          let k = fresh_id () in
          Hashtbl.add id_of n k;
          k
      in
      match Hashtbl.find_opt defined n with
      | Some true -> Printf.sprintf "#%d" id
      | _ ->
        Hashtbl.replace defined n true;
        let body = pp_desc !n in
        Printf.sprintf "#%d=%s" id body)
    else pp_desc !n
  and levels_to_string (levels : int array) : string =
    levels |> Array.to_list |> List.map string_of_int |> String.concat ","
  and pp_desc (d : cyclic_desc) : string =
    match d with
    | CUnit -> "unit"
    | CVar v -> Printf.sprintf "'a%d" v
    | CMod_const lv -> Printf.sprintf "[%s]" (levels_to_string lv)
    | CMod_annot (t, lv) ->
      Printf.sprintf "%s @@ [%s]" (pp t) (levels_to_string lv)
    | CPair (a, b) -> Printf.sprintf "(%s * %s)" (pp a) (pp b)
    | CSum (a, b) -> Printf.sprintf "(%s + %s)" (pp a) (pp b)
    | CCtor (name, []) -> name
    | CCtor (name, args) ->
      let args_s = args |> List.map pp |> String.concat ", " in
      Printf.sprintf "%s(%s)" name args_s
  in
  pp root
