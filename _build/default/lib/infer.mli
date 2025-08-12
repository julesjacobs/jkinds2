val kindof : Type_syntax.t -> Kind.t

val kinds_of_decls : Type_syntax.t Decl_parser.NameMap.t -> Kind.t Decl_parser.NameMap.t
val kinds_of_decls_bindings : (string * Type_syntax.t) list -> (string * Kind.t) list

val substitute_kinds :
  lhs:Kind.t Decl_parser.NameMap.t ->
  rhs:Kind.t Decl_parser.NameMap.t ->
  Kind.t Decl_parser.NameMap.t

val substitute_kinds_bindings :
  lhs:(string * Kind.t) list ->
  rhs:(string * Kind.t) list ->
  (string * Kind.t) list

val zero_constructor_entries : Kind.t Decl_parser.NameMap.t -> Kind.t Decl_parser.NameMap.t
val zero_constructor_entries_bindings : (string * Kind.t) list -> (string * Kind.t) list

val least_fixpoint :
  ?max_iters:int ->
  Kind.t Decl_parser.NameMap.t ->
  Kind.t Decl_parser.NameMap.t

val least_fixpoint_bindings :
  ?max_iters:int ->
  (string * Kind.t) list ->
  (string * Kind.t) list
