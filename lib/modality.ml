(* Concrete coefficient lattice shape (initial default). *)
module Coeff = Product_lattice.Make(struct let axis_sizes = [|3; 2|] end)

module M = Modality_poly.Make(Coeff)

type atom = { ctor : string; index : int }
type t = M.t

let zero : t = M.zero
let id : t = M.id
let of_atom (a : atom) : t = M.of_atom { M.ctor = a.ctor; index = a.index }
let of_levels (levels:int array) : t = M.const (Coeff.encode ~levels)
let max (m1 : t) (m2 : t) : t = M.max m1 m2
let compose (m1 : t) (m2 : t) : t = M.compose m1 m2
let substitute (f : atom -> t option) (m : t) : t =
  let f' (a:M.atom) = f { ctor = a.ctor; index = a.index } in
  M.substitute f' m
let equal (a : t) (b : t) : bool = M.equal a b
let pp (m : t) : string =
  (* Pretty-print coefficients using the concrete product lattice. *)
  if M.equal m M.zero then "⊥"
  else
    let ts = M.terms m in
    let is_id = match ts with [ (vars,c) ] -> vars = [] && Coeff.equal c Coeff.top | _ -> false in
    if is_id then "⊤"
    else
      let pp_vars vars =
        vars |> List.map (fun a -> Printf.sprintf "%s.%d" a.M.ctor a.index)
      in
      let pp_coeff c =
        let levels = Coeff.decode c in
        let parts = Array.to_list levels |> List.map string_of_int in
        "[" ^ String.concat "," parts ^ "]"
      in
      let term_strings =
        ts
        |> List.map (fun (vars,c) ->
               if Coeff.equal c Coeff.top then
                 (* Variables only *)
                 let vars_str = pp_vars vars |> String.concat " ⊓ " in
                 if List.length vars = 1 then vars_str else "(" ^ vars_str ^ ")"
               else if vars = [] then
                 (* Coefficient only *)
                 pp_coeff c
               else
                 (* Coefficient meet variables, flatten without extra braces *)
                 "(" ^ String.concat " ⊓ " (pp_coeff c :: pp_vars vars) ^ ")")
      in
      (match term_strings with
       | [s] ->
           (* Single term: drop braces if it's a plain constant or a single identifier *)
           let is_plain_const =
             match ts with
             | [ (vars,c) ] -> (vars = []) && not (Coeff.equal c Coeff.top)
             | _ -> false
           in
           let is_single_ident =
             match ts with
             | [ (vars,c) ] -> Coeff.equal c Coeff.top && (match vars with [_] -> true | _ -> false)
             | _ -> false
           in
           if is_plain_const || is_single_ident then s else "{" ^ s ^ "}"
       | _ -> "(" ^ (String.concat " ⊔ " term_strings) ^ ")")

