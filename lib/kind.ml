module Var = struct
  type t = int
  let a0 = 0
  let pp v = string_of_int v
end

module VarOrd = struct
  type t = Var.t
  let compare = Int.compare
end

module VarMap = Map.Make (VarOrd)

type t = Modality.t VarMap.t

let empty : t = VarMap.empty

let get (k : t) (v : Var.t) : Modality.t =
  match VarMap.find_opt v k with
  | Some m -> m
  | None -> Modality.zero

let find_opt (k : t) (v : Var.t) : Modality.t option = VarMap.find_opt v k

let set (k : t) (v : Var.t) (m : Modality.t) : t = VarMap.add v m k

let max (k1 : t) (k2 : t) : t =
  let merge_fn _ m1 m2 =
    match (m1, m2) with
    | None, None -> None
    | Some m, None | None, Some m -> Some m
    | Some m1, Some m2 -> Some (Modality.max m1 m2)
  in
  VarMap.merge merge_fn k1 k2

let apply (m : Modality.t) (k : t) : t =
  VarMap.map (fun m' -> Modality.compose m m') k

let pp (k : t) : string =
  if VarMap.is_empty k then "{}"
  else
    k
    |> VarMap.bindings
    |> List.map (fun (v, m) -> Printf.sprintf "%s ↦ %s" (Var.pp v) (Modality.pp m))
    |> String.concat ", "
    |> fun s -> Printf.sprintf "{%s}" s

let pp_with_ctor (_ctor : string) (k : t) : string =
  (* Deprecated: print without outer name, just numeric indices *)
  pp k

exception Substitution_error of string

let substitute_using (f : Modality.atom -> Modality.t option) (k : t) : t =
  VarMap.map (fun m -> Modality.substitute f m) k

let zero_entries (k : t) : t =
  VarMap.map (fun _ -> Modality.zero) k

let equal (a : t) (b : t) : bool =
  let bindings m = VarMap.bindings m in
  let rec eq_lists l1 l2 =
    match (l1, l2) with
    | [], [] -> true
    | (v1, m1) :: tl1, (v2, m2) :: tl2 -> v1 = v2 && Modality.equal m1 m2 && eq_lists tl1 tl2
    | _ -> false
  in
  eq_lists (bindings a) (bindings b)

let leq (a:t) (b:t) : bool =
  (* Must have identical domains, and each component modality must be ≤ *)
  let bindings_a = VarMap.bindings a in
  let bindings_b = VarMap.bindings b in
  let rec go xs ys =
    match xs, ys with
    | [], [] -> true
    | (va, ma) :: xs', (vb, mb) :: ys' -> va = vb && Modality.leq ma mb && go xs' ys'
    | _ -> false
  in
  go bindings_a bindings_b

let ceil (k:t) : t =
  VarMap.map (fun m -> Modality.ceil m) k

let floor (k:t) : t =
  VarMap.map (fun m -> Modality.floor m) k

