open Type_parser

let parse_mu_exn (s : string) : mu_raw =
  let lb = Lexing.from_string s in
  try Type_menhir.main Type_lexer.token lb with
  | Parse_error _ as e -> raise e
  | Failure msg -> raise (Parse_error ("lex: " ^ msg))
  | _ -> raise (Parse_error "parse error")

let parse_mu (s : string) : (mu_raw, string) result =
  try Ok (parse_mu_exn s) with Parse_error msg -> Error msg
