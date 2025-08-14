open Type_syntax
let make_var (v : int) : t = Var v
let make_c (name : string) (args : t list) : t = C (name, args)

type token =
  | Ident of string
  | Int_lit of int
  | Quote
  | Lparen
  | Rparen
  | Comma
  | Star
  | Plus
  | Eof

let is_ident_start c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_ident_char c =
  match c with
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
  | _ -> false

let tokenize (s : string) : (token list, string) result =
  let len = String.length s in
  let rec skip i =
    if i < len then
      match s.[i] with
      | ' ' | '\n' | '\r' | '\t' -> skip (i + 1)
      | _ -> i
    else i
  in
  let rec lex i acc =
    let i = skip i in
    if i >= len then Ok (List.rev (Eof :: acc))
    else
      match s.[i] with
      | '\'' -> lex (i + 1) (Quote :: acc)
      | '(' -> lex (i + 1) (Lparen :: acc)
      | ')' -> lex (i + 1) (Rparen :: acc)
      | ',' -> lex (i + 1) (Comma :: acc)
      | '*' -> lex (i + 1) (Star :: acc)
      | '+' -> lex (i + 1) (Plus :: acc)
      | '-' ->
          (* could be negative int; handled in int branch via sign check *)
          if i + 1 < len then
            match s.[i + 1] with
            | '0' .. '9' ->
                let j = ref (i + 2) in
                while !j < len && (match s.[!j] with '0' .. '9' -> true | _ -> false) do
                  incr j
                done;
                let n = int_of_string (String.sub s i (!j - i)) in
                lex !j (Int_lit n :: acc)
            | _ ->
                (* allow '-' in identifiers, fall through to ident lexing *)
                let j = ref (i + 1) in
                while !j < len && is_ident_char s.[!j] do incr j done;
                let id = String.sub s i (!j - i) in
                lex !j (Ident id :: acc)
          else lex (i + 1) (Ident "-" :: acc)
      | '0' .. '9' ->
          let j = ref (i + 1) in
          while !j < len && (match s.[!j] with '0' .. '9' -> true | _ -> false) do
            incr j
          done;
          let n = int_of_string (String.sub s i (!j - i)) in
          lex !j (Int_lit n :: acc)
      | c when is_ident_start c ->
          let j = ref (i + 1) in
          while !j < len && is_ident_char s.[!j] do incr j done;
          let id = String.sub s i (!j - i) in
          lex !j (Ident id :: acc)
      | c -> Error (Printf.sprintf "Unexpected character: %c" c)
  in
  lex 0 []

(* Grammar (whitespace ignored):
   Type    := Constr | Var
   Constr  := IDENT [ '(' Type (',' Type)* ')' ]
   Var     := 'a' INT   (* integers; 'a-1' means a0, reserved special *)
   We also allow bare INT as var for convenience; 'a' prefix is optional.
*)

exception Parse_error of string

let parse_exn s =
  let fail msg = raise (Parse_error msg) in
  let tokens =
    match tokenize s with Ok t -> t | Error e -> fail ("lex: " ^ e)
  in
  let rec parse_type i =
    let (lhs, j) = parse_primary i in
    parse_bin_tail lhs j
  and parse_primary i =
    match List.nth tokens i with
    | Quote -> parse_quote i
    | Ident id ->
        (* Could be constructor or var if id starts with 'a' and rest int *)
        let as_var =
          let n = String.length id in
          if n > 1 && id.[0] = 'a' then
            try Some (int_of_string (String.sub id 1 (n - 1))) with _ -> None
          else None
        in
        begin
          match as_var with
          | Some v -> (make_var v, i + 1)
          | None -> parse_after_ident id (i + 1)
        end
    | Int_lit v -> (make_var v, i + 1)
    | Lparen ->
        let (t, j) = parse_type (i + 1) in
        (match List.nth tokens j with
         | Rparen -> (t, j + 1)
         | _ -> fail "expected ')' after parenthesized type")
    | Rparen | Comma | Star | Plus | Eof ->
        fail "expected type"
  and parse_after_ident name i =
    match List.nth tokens i with
    | Lparen ->
        let (args, j) = parse_args (i + 1) in
        if String.equal name "a" then (
          match args with
          | [ Var v ] -> (make_var v, j)
          | _ -> fail "expected a(<int>)")
        else if String.equal name "unit" && args = [] then (Unit, j)
        else (make_c name args, j)
    | _ ->
        (* special base identifiers without parens *)
        (match name with
         | "unit" -> (Unit, i)
         | _ -> (make_c name [], i))
  and parse_quote i =
    (* parse 'a<int> with no space: 'a1; no bare 'a anymore *)
    match List.nth tokens (i + 1) with
    | Ident id ->
        let n = String.length id in
        if n >= 1 && id.[0] = 'a' then
          if n = 1 then (
            match List.nth tokens (i + 2) with
            | Int_lit v -> (make_var v, i + 3)
            | _ -> fail "expected number after 'a")
          else
            (* 'aN form *)
            (try (make_var (int_of_string (String.sub id 1 (n - 1))), i + 2)
             with _ -> fail "invalid 'a<number> form")
        else fail "expected a after '\''"
    | _ -> fail "expected a after '\''"
  and parse_args i =
    match List.nth tokens i with
    | Rparen -> ([], i + 1)
    | _ ->
        let (t1, j) = parse_type i in
        parse_more_args [ t1 ] j
  and parse_more_args acc i =
    match List.nth tokens i with
    | Comma ->
        let (t, j) = parse_type (i + 1) in
        parse_more_args (acc @ [ t ]) j
    | Rparen -> (acc, i + 1)
    | _ -> fail "expected ',' or ')' in argument list"
  and parse_bin_tail lhs i =
    match List.nth tokens i with
    | Star ->
        let (rhs, j) = parse_primary (i + 1) in
        parse_bin_tail (Pair (lhs, rhs)) j
    | Plus ->
        let (rhs, j) = parse_primary (i + 1) in
        parse_bin_tail (Sum (lhs, rhs)) j
    | _ -> (lhs, i)
  in
  let (t, i) = parse_type 0 in
  match List.nth tokens i with
  | Eof -> t
  | _ -> fail "trailing tokens"

let parse s =
  try Ok (parse_exn s) with Parse_error msg -> Error msg

