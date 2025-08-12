type atom = { ctor : string; index : int }

module AtomOrd = struct
  type t = atom
  let compare a b =
    match String.compare a.ctor b.ctor with
    | 0 -> Int.compare a.index b.index
    | c -> c
end

module AtomSet = Set.Make (AtomOrd)

module CompOrd = struct
  type t = AtomSet.t
  let compare = AtomSet.compare
end

module CompSet = Set.Make (CompOrd)

type t = CompSet.t

let zero : t = CompSet.empty

let id : t = CompSet.singleton AtomSet.empty

let of_atom (a : atom) : t =
  let comp = AtomSet.singleton a in
  CompSet.singleton comp

let prune (m : t) : t =
  if CompSet.is_empty m then m
  else
    let comps = CompSet.elements m in
    let keep c =
      not (List.exists (fun d -> (not (AtomSet.equal c d)) && AtomSet.subset d c) comps)
    in
    List.fold_left (fun acc c -> if keep c then CompSet.add c acc else acc) CompSet.empty comps

let max (m1 : t) (m2 : t) : t =
  (* outer union, then prune subsets *)
  prune (CompSet.union m1 m2)

let compose (m1 : t) (m2 : t) : t =
  if CompSet.is_empty m1 || CompSet.is_empty m2 then zero
  else
    let res = CompSet.fold
      (fun c1 acc ->
        CompSet.fold
          (fun c2 acc' -> CompSet.add (AtomSet.union c1 c2) acc')
          m2 acc)
      m1 CompSet.empty
    in
    prune res

let pp_atom (a : atom) = Printf.sprintf "%s.%d" a.ctor a.index

let pp (m : t) : string =
  if CompSet.is_empty m then "⊥"
  else
    let elems = CompSet.elements m in
    if List.length elems = 1 && AtomSet.is_empty (List.hd elems) then "⊤"
    else
      let comps =
        elems
        |> List.map (fun comp ->
               if AtomSet.is_empty comp then "{}"
               else
                 comp
                 |> AtomSet.elements
                 |> List.map pp_atom
                 |> String.concat " ⊓ "
                 |> fun s -> Printf.sprintf "{%s}" s)
      in
      Printf.sprintf "{%s}" (String.concat " ⊔ " comps)

let equal (a : t) (b : t) : bool = CompSet.equal a b

(* substitute each atom using the provided function, distributing over ⊓ and ⊔.
   If f a = None, keep the atom as-is. *)
let substitute (f : atom -> t option) (m : t) : t =
  if CompSet.is_empty m then m
  else
    let substitute_comp (comp : AtomSet.t) : t =
      (* Start with id to represent empty composition *)
      let start = Some id in
      let res =
        AtomSet.elements comp
        |> List.fold_left
             (fun acc a ->
               let term = match f a with Some t -> t | None -> of_atom a in
               match acc with
               | None -> Some term
               | Some acc' -> Some (compose acc' term))
             start
      in
      match res with Some r -> r | None -> id
    in
    let res =
      CompSet.elements m
      |> List.fold_left (fun acc comp -> max acc (substitute_comp comp)) zero
    in
    prune res

