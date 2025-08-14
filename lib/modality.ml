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
      let pp_set vars =
        if vars = [] then "{}"
        else
          vars
          |> List.map (fun a -> Printf.sprintf "%s.%d" a.M.ctor a.index)
          |> String.concat " ⊓ "
          |> fun s -> Printf.sprintf "{%s}" s
      in
      let pp_coeff c =
        let levels = Coeff.decode c in
        let parts = Array.to_list levels |> List.map string_of_int in
        "[" ^ String.concat "," parts ^ "]"
      in
      ts
      |> List.map (fun (vars,c) ->
             if Coeff.equal c Coeff.top then pp_set vars
             else if vars = [] then "{" ^ pp_coeff c ^ "}"
             else Printf.sprintf "{%s ⊓ %s}" (pp_coeff c) (pp_set vars))
      |> String.concat " ⊔ "
      |> fun body -> "{" ^ body ^ "}"

