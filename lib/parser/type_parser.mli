exception Parse_error of string

type mu_raw =
  | UnitR
  | PairR of mu_raw * mu_raw
  | SumR of mu_raw * mu_raw
  | CR of string * mu_raw list
  | VarR of int
  | ModAnnotR of mu_raw * int array
  | ModConstR of int array
  | MuR of int * mu_raw
  | RecvarR of int

val to_simple_exn : mu_raw -> Type_syntax.t
val to_simple : mu_raw -> (Type_syntax.t, string) result

type cyclic = { id : int; mutable node : cnode }

and cnode =
  | CUnit
  | CPair of cyclic * cyclic
  | CSum of cyclic * cyclic
  | CCtor of string * cyclic list
  | CVar of int
  | CMod_annot of cyclic * int array
  | CMod_const of int array

val to_cyclic : mu_raw -> cyclic
val to_cyclic_with_vars : (int -> cyclic option) -> mu_raw -> cyclic
val pp_cyclic : cyclic -> string

val mk : cnode -> cyclic
