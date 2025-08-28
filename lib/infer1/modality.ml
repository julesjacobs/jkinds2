(* Concrete coefficient lattice moved to its own module. *)
module Coeff = Axis_lattice

(* Merge Modality_poly into this module by specializing lattice polynomials to
   Coeff. *)
type atom = { ctor : string; index : int }

module AtomOrd = struct
  type t = atom

  let compare a b =
    match String.compare a.ctor b.ctor with
    | 0 -> Int.compare a.index b.index
    | c -> c

  let to_string _x = "<todo>"
end

module P = Lattice_polynomial.Make (Coeff) (AtomOrd)

type t = P.t

let zero : t = P.bot
let id : t = P.const Coeff.top
let of_atom (a : atom) : t = P.var a
let of_levels (levels : int array) : t = P.const (Coeff.encode ~levels)
let max (m1 : t) (m2 : t) : t = P.join m1 m2
let compose (m1 : t) (m2 : t) : t = P.meet m1 m2
let co_sub_approx (m1 : t) (m2 : t) : t = P.co_sub_approx m1 m2

let substitute (f : atom -> t option) (m : t) : t =
  let vars = P.support m in
  let subs =
    P.VarSet.fold
      (fun v acc ->
        match f v with None -> acc | Some p -> P.VarMap.add v p acc)
      vars P.VarMap.empty
  in
  if P.VarMap.is_empty subs then m else P.subst ~subs m

let equal (a : t) (b : t) : bool = P.equal a b
let leq (a : t) (b : t) : bool = P.leq a b
let ceil (m : t) : t = P.const (P.ceil m)
let floor (m : t) : t = P.const (P.floor m)

let pp (m : t) : string =
  (* Pretty-print coefficients using the concrete product lattice. *)
  if P.equal m P.bot then "⊥"
  else
    let ts = P.to_list m in
    let is_id =
      match ts with
      | [ (s, c) ] -> P.VarSet.is_empty s && Coeff.equal c Coeff.top
      | _ -> false
    in
    if is_id then "⊤"
    else
      let pp_vars vars =
        vars |> List.map (fun a -> Printf.sprintf "%s.%d" a.ctor a.index)
      in
      let pp_coeff c =
        let levels = Coeff.decode c in
        let parts = Array.to_list levels |> List.map string_of_int in
        "[" ^ String.concat "," parts ^ "]"
      in
      let term_strings =
        ts
        |> List.map (fun (s, c) ->
               let vars = P.VarSet.elements s in
               if Coeff.equal c Coeff.top then
                 (* Variables only *)
                 let vars_str = pp_vars vars |> String.concat " ⊓ " in
                 if List.length vars = 1 then vars_str else "(" ^ vars_str ^ ")"
               else if P.VarSet.is_empty s then
                 (* Coefficient only *)
                 pp_coeff c
               else
                 (* Coefficient meet variables, flatten without extra braces *)
                 "(" ^ String.concat " ⊓ " (pp_coeff c :: pp_vars vars) ^ ")")
      in
      match term_strings with
      | [ s ] -> s
      | _ -> "(" ^ String.concat " ⊔ " term_strings ^ ")"
