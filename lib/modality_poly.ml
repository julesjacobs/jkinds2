module type COEFF = Lattice_intf.LATTICE

module Make (C : COEFF) = struct
  type atom = { ctor : string; index : int }

  module AtomOrd = struct
    type t = atom
    let compare a b =
      match String.compare a.ctor b.ctor with
      | 0 -> Int.compare a.index b.index
      | c -> c
  end

  module P = Lattice_polynomial.Make(C)(AtomOrd)

  type coeff = C.t
  type t = P.t

  let zero = P.bot
  let id = P.const C.top
  let const c = P.const c
  let of_atom a = P.var a
  let max = P.join
  let compose = P.meet

  let substitute (f : atom -> t option) (m : t) : t =
    (* Build a substitution map for all atoms that appear in m where f returns Some *)
    let vars = P.support m in
    let subs =
      P.VarSet.fold
        (fun v acc -> match f v with None -> acc | Some p -> P.VarMap.add v p acc)
        vars P.VarMap.empty
    in
    if P.VarMap.is_empty subs then m else P.subst ~subs m

  let equal = P.equal
  let leq = P.leq
  let co_sub (p:t) (q:t) : t = P.co_sub p q

  let terms (m:t) : (atom list * coeff) list =
    P.to_list m |> List.map (fun (s,c) -> (P.VarSet.elements s, c))

  let ceil_coeff (m:t) : coeff = P.ceil m
  let floor_coeff (m:t) : coeff = P.floor m
  let ceil (m:t) : t = P.const (P.ceil m)
  let floor (m:t) : t = P.const (P.floor m)

  let pp (m : t) : string =
    (* Keep generic; concrete decode/pretty-print handled by caller/wrapper. *)
    if P.equal m P.bot then "⊥"
    else
      let ts = P.to_list m in
      let is_id = match ts with [ (s,c) ] -> P.VarSet.is_empty s && C.equal c C.top | _ -> false in
      if is_id then "⊤"
      else
        let pp_set s =
          if P.VarSet.is_empty s then "{}"
          else
            P.VarSet.elements s
            |> List.map (fun a -> Printf.sprintf "%s.%d" a.ctor a.index)
            |> String.concat " ⊓ "
            |> fun s -> Printf.sprintf "{%s}" s
        in
        ts
        |> List.map (fun (s,c) ->
               if C.equal c C.top then pp_set s
               else if P.VarSet.is_empty s then "{<c>}"
               else Printf.sprintf "{<c> ⊓ %s}" (pp_set s))
        |> String.concat " ⊔ "
        |> fun body -> "{" ^ body ^ "}"
end


