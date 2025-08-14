type var = int

type t =
  | Unit
  | Pair of t * t
  | Sum of t * t
  | C of string * t list
  | Var of var
  | Mod_annot of t * int array
  | Mod_const of int array

val pp : t -> string
 