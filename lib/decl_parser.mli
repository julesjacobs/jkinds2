exception Parse_error of string

type decl_item = {
  name : string;
  arity : int;
  rhs : Type_syntax.t;
  abstract : bool;
}

type program = decl_item list

val parse_program_exn : string -> program
val mu_table : (string, Type_parser.mu_raw) Hashtbl.t [@@ocaml.warning "-32"]
val rhs_mu_of_name : string -> Type_parser.mu_raw option [@@ocaml.warning "-32"]
val rhs_mu_of_name : string -> Type_parser.mu_raw option
