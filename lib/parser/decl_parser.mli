exception Parse_error of string

type decl_item = {
  name : string;
  arity : int;
  abstract : bool;
  rhs_mu_raw : Type_parser.mu_raw; (* canonical parsed form *)
  rhs_simple : Type_syntax.t option; (* Some if mu-free; None otherwise *)
  rhs_cyclic : Type_parser.cyclic; (* precomputed cyclic form *)
}

type program = decl_item list

val parse_program_exn : string -> program
