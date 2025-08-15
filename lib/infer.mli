val kindof : Type_syntax.t -> Kind.t

val kinds_of_decls_bindings : (string * Type_syntax.t) list -> (string * Kind.t) list

val substitute_kinds_bindings :
  lhs:(string * Kind.t) list ->
  rhs:(string * Kind.t) list ->
  (string * Kind.t) list

val zero_constructor_entries_bindings : (string * Kind.t) list -> (string * Kind.t) list

val least_fixpoint_bindings :
  ?max_iters:int ->
  (string * Kind.t) list ->
  (string * Kind.t) list

val least_fixpoint_bindings_with_self_init :
  ?max_iters:int ->
  abstracts:(string * int) list ->
  (string * Kind.t) list ->
  (string * Kind.t) list

val solve_program :
  Decl_parser.program ->
  max_iters:int ->
  (string * Kind.t) list
