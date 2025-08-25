%{
%}

%token <string> IDENT
%token <int> INT
%token QUOTE DOT LPAREN RPAREN COMMA LBRACK RBRACK STAR PLUS ATAT
%token MU
%token EOF

%start <Type_parser.mu_raw> main

%left PLUS
%left STAR

%%

main:
| t=type_expr EOF { t }

type_expr:
| t=type_expr PLUS u=type_expr1 { Type_parser.SumR (t, u) }
| type_expr1 { $1 }

type_expr1:
| t=type_expr1 STAR u=postfix { Type_parser.PairR (t, u) }
| postfix { $1 }

postfix:
| t=postfix ATAT LBRACK ls=levels RBRACK { Type_parser.ModAnnotR (t, Array.of_list ls) }
| atom { $1 }

levels:
| i=INT { [i] }
| i=INT COMMA ls=levels { i :: ls }

atom:
| IDENT LPAREN RPAREN { if $1 = "unit" then Type_parser.UnitR else Type_parser.CR ($1, []) }
| IDENT LPAREN args=arg_list RPAREN {
    if $1 = "a" then (
      match args with | [Type_parser.VarR v] -> Type_parser.VarR v | _ -> Type_parser.CR ($1, args)
    ) else if $1 = "unit" && args = [] then Type_parser.UnitR else Type_parser.CR ($1, args)
  }
| IDENT {
    if $1 = "unit" then Type_parser.UnitR else Type_parser.CR ($1, [])
  }
| INT { Type_parser.VarR $1 }
| LPAREN t=type_expr RPAREN { t }
| LBRACK ls=levels RBRACK { Type_parser.ModConstR (Array.of_list ls) }
| QUOTE id=IDENT {
    let n = String.length id in
    if n >= 1 then (
      match id.[0] with
      | 'a' -> if n = 1 then raise (Type_parser.Parse_error "expected number after 'a") else
               (try Type_parser.VarR (int_of_string (String.sub id 1 (n-1))) with _ -> raise (Type_parser.Parse_error "invalid 'a<number> form"))
      | 'b' -> if n = 1 then raise (Type_parser.Parse_error "expected number after 'b") else
               (try Type_parser.RecvarR (int_of_string (String.sub id 1 (n-1))) with _ -> raise (Type_parser.Parse_error "invalid 'b<number> form"))
      | _ -> raise (Type_parser.Parse_error "expected a or b after '\''")
    ) else raise (Type_parser.Parse_error "expected identifier after '\''")
  }
| QUOTE id=IDENT INT {
    match id with
    | "a" -> Type_parser.VarR $3
    | "b" -> Type_parser.RecvarR $3
    | _ -> raise (Type_parser.Parse_error "expected a or b after '\''")
  }
| MU QUOTE id=IDENT DOT body=type_expr {
    let parse_b s =
      let n = String.length s in
      if n >= 1 && s.[0] = 'b' then
        if n = 1 then None else
          try Some (int_of_string (String.sub s 1 (n-1))) with _ -> None
      else None
    in
    match parse_b id with
    | Some bi -> Type_parser.MuR (bi, body)
    | None -> raise (Type_parser.Parse_error "expected b<number> after mu '")
  }

arg_list:
| t=type_expr { [t] }
| t=type_expr COMMA ts=arg_list { t :: ts }

%%
