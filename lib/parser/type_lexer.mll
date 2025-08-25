{
open Type_menhir
}

rule token = parse
| [' ' '\t' '\r' '\n'] { token lexbuf }
| "@@" { ATAT }
| "'" { QUOTE }
| '.' { DOT }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACK }
| ']' { RBRACK }
| ',' { COMMA }
| '*' { STAR }
| '+' { PLUS }
| ('-'? ['0'-'9']+ as i) { INT (int_of_string i) }
| (['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_''-']* as id) {
    if id = "mu" then MU else IDENT id
  }
| eof { EOF }
| _ { failwith (Printf.sprintf "lex error at: %s" (Lexing.lexeme lexbuf)) }
