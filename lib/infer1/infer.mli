exception Unsupported_mu of string list

val solve_program :
  Decl_parser.program -> max_iters:int -> (string * Kind.t) list

val run_program : Decl_parser.program -> max_iters:int -> string
