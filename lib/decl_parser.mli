exception Parse_error of string

type decl_item = {
  name : string;
  arity : int;
  rhs : Type_syntax.t;
  abstract : bool;
}

type program = decl_item list

val parse_program_exn : string -> program
