open Type_parser

let parse_mu (s : string) : (mu_raw, string) result =
  try
    let lb = Lexing.from_string s in
    Ok (Type_menhir.main Type_lexer.token lb)
  with
  | Parse_error msg -> Error msg
  | Failure msg -> Error ("lex: " ^ msg)
  | _ -> Error "parse error"
