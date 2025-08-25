type var = int

type t =
  | Unit
  | Pair of t * t
  | Sum of t * t
  | C of string * t list
  | Var of var
  | Mod_annot of t * int array
  | Mod_const of int array

let rec pp (t : t) : string =
  match t with
  | Var v -> Printf.sprintf "'a%d" v
  | Unit -> "unit"
  | Pair (a, b) -> Printf.sprintf "(%s * %s)" (pp a) (pp b)
  | Sum (a, b) -> Printf.sprintf "(%s + %s)" (pp a) (pp b)
  | C (name, args) ->
    if args = [] then name
    else
      let args_s = args |> List.map pp |> String.concat ", " in
      Printf.sprintf "%s(%s)" name args_s
  | Mod_annot (t, levels) ->
    Printf.sprintf "%s @@ [%s]" (pp t)
      (levels |> Array.to_list |> List.map string_of_int |> String.concat ",")
  | Mod_const levels ->
    Printf.sprintf "[%s]"
      (levels |> Array.to_list |> List.map string_of_int |> String.concat ",")
