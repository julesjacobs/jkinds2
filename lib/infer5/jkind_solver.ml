module Make
    (Lat : Lattice_intf.LATTICE)
    (Ty : sig
      type t

      val compare_ty : t -> t -> int
      val to_string : t -> string
    end)
    (Constr : sig
      type t

      val compare : t -> t -> int
      val to_string : t -> string
    end) : sig
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type kind

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
    rigid : ty -> kind;
  }

  type ckind = ops -> kind
  type constr_decl = { args : ty list; kind : ckind; abstract : bool }
  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }
  type atom = { constr : constr; arg_index : int }
  type solver

  val make_solver : env -> solver
  val normalize : solver -> ckind -> (lat * atom list) list
  val leq : solver -> ckind -> ckind -> bool
  val round_up : solver -> ckind -> lat
end = struct
  type ty = Ty.t
  type constr = Constr.t
  type lat = Lat.t
  type atom = { constr : constr; arg_index : int }

  (* Touch compare_ty to avoid unused warning in strict test builds. *)
  let _ : ty -> ty -> int = Ty.compare_ty

  module RigidName = struct
    type t = Atom of atom | Ty of ty

    let compare (a : t) (b : t) : int =
      match (a, b) with
      | Atom a1, Atom a2 -> (
        match Constr.compare a1.constr a2.constr with
        | 0 -> Int.compare a1.arg_index a2.arg_index
        | c -> c)
      | Ty x, Ty y -> Stdlib.compare x y
      | Atom _, Ty _ -> -1
      | Ty _, Atom _ -> 1

    let to_string (a : t) : string =
      match a with
      | Atom a -> Printf.sprintf "%s.%d" (Constr.to_string a.constr) a.arg_index
      | Ty v -> Printf.sprintf "<%s>" (Ty.to_string v)

    let atomic constr arg_index = Atom { constr; arg_index }
  end

  module LSolver = Lattice_fixpoint_solver.Make (Lat) (RigidName)

  type kind = LSolver.poly

  type ops = {
    const : lat -> kind;
    join : kind list -> kind;
    modality : lat -> kind -> kind;
    constr : constr -> kind list -> kind;
    kind_of : ty -> kind;
    rigid : ty -> kind;
  }

  type ckind = ops -> kind
  type constr_decl = { args : ty list; kind : ckind; abstract : bool }
  type env = { kind_of : ty -> ckind; lookup : constr -> constr_decl }
  type solver = { ops : ops }

  let make_solver (env : env) : solver =
    (* Define all the trivial ops *)
    let const l = LSolver.const l in
    let join ks = List.fold_left LSolver.join LSolver.bot ks in
    let modality l k = LSolver.meet (LSolver.const l) k in
    let rigid t = LSolver.rigid (RigidName.Ty t) in

    (* Create hash table mapping ty to kind for memoization *)
    let ty_to_kind = Hashtbl.create 0 in
    (* And hash table mapping constructor to coefficients *)
    let constr_to_coeffs = Hashtbl.create 0 in

    (* Define kind_of and constr ops *)
    let rec kind_of (t : ty) : kind =
      match Hashtbl.find_opt ty_to_kind t with
      | Some k -> k
      | None ->
        (* Pre-insert lattice solver var for this type *)
        let v = LSolver.new_var () in
        Hashtbl.add ty_to_kind t (LSolver.var v);
        let kind = env.kind_of t ops in
        LSolver.solve_lfp v kind;
        kind
    and constr_kind c =
      match Hashtbl.find_opt constr_to_coeffs c with
      | Some base_and_coeffs -> base_and_coeffs
      | None ->
        let base = LSolver.new_var () in
        (* Allocate coefficient vars based on declared arity, not on ks
           length *)
        let { args; kind; abstract } = env.lookup c in
        let coeffs =
          List.init (List.length args) (fun _ -> LSolver.new_var ())
        in
        Hashtbl.add constr_to_coeffs c (base, coeffs);
        (* Recursively compute the kind of the body *)
        List.iter
          (fun ty ->
            Hashtbl.add ty_to_kind ty (LSolver.rigid (RigidName.Ty ty)))
          args;
        (* Compute body kind *)
        let kind' = kind ops in
        (* Extract coeffs' from kind' *)
        let base', coeffs', rest =
          LSolver.decompose_linear
            ~universe:(List.map (fun ty -> RigidName.Ty ty) args)
            kind'
        in
        assert (rest = []);
        if List.length coeffs <> List.length coeffs' then
          failwith
            (Printf.sprintf
               "jkind_solver: coeffs mismatch for constr %s (length %d vs %d)"
               (Constr.to_string c) (List.length coeffs) (List.length coeffs'));
        if abstract then (
          (* We need to assert that kind' is less than or equal to the base *)
          LSolver.enqueue_gfp base
            (LSolver.meet base' (LSolver.rigid (RigidName.atomic c 0)));
          List.iteri
            (fun i (coeff, coeff') ->
              let rhs = LSolver.join coeff' base' in
              let bound =
                LSolver.meet rhs (LSolver.rigid (RigidName.atomic c (i + 1)))
              in
              LSolver.enqueue_gfp coeff bound)
            (List.combine coeffs
               (List.map (fun (_name, coeff') -> coeff') coeffs')))
        else (
          (* We need to solve for the coeffs *)
          LSolver.solve_lfp base base';
          List.iter2
            (fun coeff (_name, coeff') -> LSolver.solve_lfp coeff coeff')
            coeffs coeffs');
        (base, coeffs)
    and constr c ks =
      (* Todo: need to explore the constructor definition using lookup to
         contrain the coeffs and base *)
      let base, coeffs = constr_kind c in
      (* Meet each arg with the corresponding coeff *)
      let ks' =
        (* Meet each provided argument with its coeff; missing args are ⊥. *)
        let nth_opt lst i = try Some (List.nth lst i) with _ -> None in
        List.mapi
          (fun i coeff ->
            let k =
              match nth_opt ks i with Some k -> k | None -> LSolver.bot
            in
            LSolver.meet k (LSolver.var coeff))
          coeffs
      in
      (* Join all the ks'' plus the base *)
      let k' = List.fold_left LSolver.join (LSolver.var base) ks' in
      (* Return that kind *)
      k'
    and ops = { const; join; modality; constr; kind_of; rigid } in
    { ops }

  let normalize (solver : solver) (k : ckind) : (lat * atom list) list =
    let p = k solver.ops in
    LSolver.enter_query_phase ();
    let terms = LSolver.normalize p in
    let conv_atom = function
      | RigidName.Atom a -> Some { constr = a.constr; arg_index = a.arg_index }
      | RigidName.Ty _ -> None
    in
    let conv_vars vs = vs |> List.filter_map conv_atom in
    List.map (fun (coeff, vars) -> (coeff, conv_vars vars)) terms

  let leq (solver : solver) (k1 : ckind) (k2 : ckind) : bool =
    let k1' = k1 solver.ops in
    let k2' = k2 solver.ops in
    LSolver.leq k1' k2'

  let round_up (solver : solver) (k : ckind) : lat =
    let terms = normalize solver k in
    match terms with
    | [] -> Lat.bot
    | (c, _) :: rest ->
      List.fold_left (fun acc (c', _) -> Lat.join acc c') c rest
  (* LSolver.round_up (norm env k) *)
end
