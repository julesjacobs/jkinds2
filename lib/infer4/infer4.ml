(* Infer4: polynomial translation and solving using the new
   lattice_fixpoint_solver with explicit rigid vs solver variables.

   Rigid variables are constructor atoms (C.i) and type variables ('ai). Solver
   variables are created for: - Each constructor atom (to hold its solved
   meaning) - Each µ-binder during cyclic translation (LFP-only) Abstract
   declarations are encoded as: x := x_rigid ∧ bound Concrete declarations are
   encoded as: x := bound *)

module RigidName = struct
  type t = Atom of Modality.atom | TyVar of int

  let compare (a : t) (b : t) : int =
    match (a, b) with
    | Atom a1, Atom a2 -> (
      match String.compare a1.Modality.ctor a2.Modality.ctor with
      | 0 -> Int.compare a1.index a2.index
      | c -> c)
    | TyVar x, TyVar y -> Int.compare x y
    | Atom _, TyVar _ -> -1
    | TyVar _, Atom _ -> 1

  let to_string (a : t) : string =
    match a with
    | Atom a -> Printf.sprintf "%s.%d" a.Modality.ctor a.index
    | TyVar v -> Printf.sprintf "'a%d" v
end

module S = Lattice_fixpoint_solver.Make (Axis_lattice) (RigidName)

(* Pretty helpers *)
type poly = S.poly

let pp_coeff_axis = Axis_lattice.to_string

let pp_poly (p : S.poly) : string =
  S.pp ~pp_var:RigidName.to_string ~pp_coeff:pp_coeff_axis p

(* Global atom solver vars (per program run) *)
module AtomKey = struct
  type t = string * int

  let compare (a1, b1) (a2, b2) =
    match String.compare a1 a2 with 0 -> Int.compare b1 b2 | c -> c
end

module AtomMap = Map.Make (AtomKey)

let atom_vars : S.var AtomMap.t ref = ref AtomMap.empty

module StringSet = Set.Make (String)

let abstract_ctors : StringSet.t ref = ref StringSet.empty
let reset_state () = atom_vars := AtomMap.empty

let get_atom_var (ctor : string) (index : int) : S.var =
  let key = (ctor, index) in
  match AtomMap.find_opt key !atom_vars with
  | Some v -> v
  | None ->
    let v = S.new_var () in
    atom_vars := AtomMap.add key v !atom_vars;
    v

let rigid_atom (ctor : string) (index : int) : S.poly =
  S.rigid (RigidName.Atom { Modality.ctor; index })

let rigid_tyvar (i : int) : S.poly = S.rigid (RigidName.TyVar i)

let const_levels (levels : int array) : S.poly =
  S.const (Axis_lattice.encode ~levels)

(* Translation to polynomials (rigid variables only) *)
let is_abstract (name : string) : bool = StringSet.mem name !abstract_ctors
let atom_poly (name : string) (i : int) : S.poly = S.var (get_atom_var name i)

let rec to_poly (t : Type_syntax.t) : S.poly =
  match t with
  | Type_syntax.Unit -> S.const Axis_lattice.bot
  | Type_syntax.Pair (a, b) | Type_syntax.Sum (a, b) ->
    S.join (to_poly a) (to_poly b)
  | Type_syntax.Mod_annot (t', lv) -> S.meet (const_levels lv) (to_poly t')
  | Type_syntax.Mod_const lv -> const_levels lv
  | Type_syntax.Var v -> rigid_tyvar v
  | Type_syntax.C (name, args) ->
    let base = atom_poly name 0 in
    let step (acc : S.poly) (arg : Type_syntax.t) (i : int) : S.poly =
      let vi = atom_poly name i in
      S.join acc (S.meet vi (to_poly arg))
    in
    List.mapi (fun i t -> (i + 1, t)) args
    |> List.fold_left (fun acc (i, t) -> step acc t i) base

(* Cyclic translation with LFP solver vars for µ-binders *)
let to_poly_cyclic (root : Type_parser.cyclic) : S.poly =
  let table : (Type_parser.cyclic, S.var) Hashtbl.t = Hashtbl.create 64 in
  let onstack : (Type_parser.cyclic, unit) Hashtbl.t = Hashtbl.create 64 in

  let rec translate (n : Type_parser.cyclic) : S.poly =
    match Hashtbl.find_opt table n with
    | Some v -> S.var v
    | None ->
      if Hashtbl.mem onstack n then (
        let v = S.new_var () in
        Hashtbl.add table n v;
        S.var v)
      else (
        Hashtbl.add onstack n ();
        let rhs =
          match n.Type_parser.node with
          | Type_parser.CUnit -> S.const Axis_lattice.bot
          | Type_parser.CVar i -> rigid_tyvar i
          | Type_parser.CMod_const lv -> const_levels lv
          | Type_parser.CMod_annot (t, lv) ->
            S.meet (const_levels lv) (translate t)
          | Type_parser.CPair (a, b) | Type_parser.CSum (a, b) ->
            S.join (translate a) (translate b)
          | Type_parser.CCtor (name, args) ->
            let base = atom_poly name 0 in
            let step acc (i, t) =
              let vi = atom_poly name i in
              S.join acc (S.meet vi (translate t))
            in
            List.mapi (fun i t -> (i + 1, t)) args |> List.fold_left step base
        in
        Hashtbl.remove onstack n;
        match Hashtbl.find_opt table n with
        | Some v ->
          S.solve_lfp v rhs;
          S.var v
        | None -> rhs)
  in
  translate root

let to_poly_mu_raw (m : Type_parser.mu_raw) : S.poly =
  let cyc = Type_parser.to_cyclic m in
  to_poly_cyclic cyc

let to_poly_decl_rhs (it : Decl_parser.decl_item) : S.poly =
  to_poly_mu_raw it.rhs_mu_raw

(* Linear decomposition helpers *)
let decompose_by_tyvars ~(arity : int) (p : S.poly) :
    S.poly * S.poly array * (RigidName.t list * S.poly) list =
  let universe =
    let rec loop i acc =
      if i > arity then List.rev acc else loop (i + 1) (RigidName.TyVar i :: acc)
    in
    loop 1 []
  in
  let base, singles, mixed = S.decompose_linear ~universe p in
  let coeffs = Array.init arity (fun _ -> S.bot) in
  List.iter
    (fun (v, poly) ->
      match v with
      | RigidName.TyVar i when i >= 1 && i <= arity ->
        coeffs.(i - 1) <- S.join coeffs.(i - 1) poly
      | _ -> ())
    singles;
  (base, coeffs, mixed)

let pp_varlabel : RigidName.t -> string = RigidName.to_string

(* Solving program: build per-atom equations and solve LFPs *)
type linear_decomp = {
  it : Decl_parser.decl_item;
  base : S.poly;
  coeffs : S.poly array;
}

let compute_linear_decomps (prog : Decl_parser.program) : linear_decomp list =
  let decomp_of_it (it : Decl_parser.decl_item) : linear_decomp =
    let p = to_poly_decl_rhs it in
    let base, coeffs, mixed = decompose_by_tyvars ~arity:it.arity p in
    (if mixed <> [] then
       let parts =
         mixed
         |> List.map (fun (k, poly) ->
                let key =
                  "{" ^ (k |> List.map pp_varlabel |> String.concat ", ") ^ "}"
                in
                Printf.sprintf "%s: %s" key (pp_poly poly))
         |> String.concat "; "
       in
       let msg =
         Printf.sprintf "infer4: non-linear terms for %s: %s" it.name parts
       in
       failwith msg);
    { it; base; coeffs }
  in
  List.map decomp_of_it prog

let solve_linear_for_program (prog : Decl_parser.program) : unit =
  reset_state ();
  (* Determine declared and used constructors to mark implicit abstracts *)
  let declared =
    List.fold_left
      (fun acc (it : Decl_parser.decl_item) -> StringSet.add it.name acc)
      StringSet.empty prog
  in
  let rec collect_used (acc : StringSet.t) (m : Type_parser.mu_raw) :
      StringSet.t =
    match m with
    | Type_parser.UnitR | VarR _ | ModConstR _ -> acc
    | ModAnnotR (t, _) -> collect_used acc t
    | PairR (a, b) | SumR (a, b) -> collect_used (collect_used acc a) b
    | CR (name, args) ->
      List.fold_left collect_used (StringSet.add name acc) args
    | MuR (_, t) -> collect_used acc t
    | RecvarR _ -> acc
  in
  let used =
    List.fold_left
      (fun acc (it : Decl_parser.decl_item) -> collect_used acc it.rhs_mu_raw)
      StringSet.empty prog
  in
  let implicit_abstracts = StringSet.diff used declared in
  (* Initialize which constructors are abstract for rigid/solver choice *)
  abstract_ctors :=
    List.fold_left
      (fun acc (it : Decl_parser.decl_item) ->
        if it.abstract then StringSet.add it.name acc else acc)
      implicit_abstracts prog;
  let decs = compute_linear_decomps prog in
  (* Phase 1: concrete definitions as equalities (LFP) *)
  List.iter
    (fun { it; base; coeffs } ->
      if not it.abstract then (
        let v0 = get_atom_var it.name 0 in
        S.solve_lfp v0 base;
        for i = 1 to it.arity do
          let vi = get_atom_var it.name i in
          S.solve_lfp vi coeffs.(i - 1)
        done))
    decs;
  (* Phase 2: abstract declarations via GFP: x := x_rigid ∧ bound *)
  List.iter
    (fun { it; base; coeffs } ->
      if it.abstract then (
        let v0 = get_atom_var it.name 0 in
        S.enqueue_gfp v0 (S.meet (rigid_atom it.name 0) base);
        for i = 1 to it.arity do
          let vi = get_atom_var it.name i in
          let rhs = S.join base coeffs.(i - 1) in
          S.enqueue_gfp vi (S.meet (rigid_atom it.name i) rhs)
        done))
    decs;
  ()

let atom_state_lines_for_program (prog : Decl_parser.program) : string list =
  let lines = ref [] in
  (* Enter query phase to finalize GFPs if any (none currently), and force *)
  ignore (S.leq S.bot S.top : bool);
  List.iter
    (fun (it : Decl_parser.decl_item) ->
      for i = 0 to it.arity do
        let lhs = Printf.sprintf "%s.%d" it.name i in
        let v = get_atom_var it.name i in
        let rhs = pp_poly (S.var v) in
        lines := (lhs ^ " = " ^ rhs) :: !lines
      done)
    prog;
  List.rev !lines

let atom_bound_poly ~(ctor : string) ~(index : int) : S.poly =
  let v = get_atom_var ctor index in
  S.var v

module PpPoly = Lattice_polynomial.Make (Axis_lattice) (RigidName)

let to_pppoly (p : S.poly) : PpPoly.t =
  let terms = S.normalize p in
  let to_vars (vs : RigidName.t list) : PpPoly.vars =
    List.fold_left (fun acc v -> PpPoly.VarSet.add v acc) PpPoly.VarSet.empty vs
  in
  terms |> List.map (fun (c, vs) -> (to_vars vs, c)) |> PpPoly.of_list

let pp_entry (ctor : string) (i : int) : string =
  let p = atom_bound_poly ~ctor ~index:i in
  if i = 0 then pp_poly p
  else
    let base = atom_bound_poly ~ctor ~index:0 in
    let p_poly = to_pppoly p in
    let base_poly = to_pppoly base in
    let diff = PpPoly.co_sub_approx p_poly base_poly in
    PpPoly.pp ~pp_var:RigidName.to_string ~pp_coeff:pp_coeff_axis diff

let run_program (prog : Decl_parser.program) : string =
  solve_linear_for_program prog;
  (* Ensure query phase for printing *)
  ignore (S.leq S.bot S.top : bool);
  prog
  |> List.map (fun (it : Decl_parser.decl_item) ->
         let body =
           let rec loop i acc =
             if i > it.arity then List.rev acc
             else
               let s = pp_entry it.name i in
               loop (i + 1) (Printf.sprintf "%d ↦ %s" i s :: acc)
           in
           loop 0 [] |> String.concat ", "
         in
         Printf.sprintf "%s: {%s}" it.name body)
  |> String.concat "\n"
