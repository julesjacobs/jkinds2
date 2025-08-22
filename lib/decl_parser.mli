exception Parse_error of string

type decl_item = {
  name : string;
  arity : int;
  abstract : bool;
  rhs_simple : Type_syntax.t option;
  rhs_mu : Type_parser.mu_raw option;
}

type program = decl_item list

val parse_program_exn : string -> program
