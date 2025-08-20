module type LATTICE = Lattice_intf.LATTICE

module type ORDERED = sig
  type t

  val compare : t -> t -> int
end

module Make
    (C : LATTICE)
    (V : sig
      type t
    end) =
struct
  module P =
    Lattice_polynomial.Make
      (C)
      (struct
        type t = var

        let compare a b = Int.compare a.id b.id
      end)

  type var = {
    name : V.t;
    id : int;
    mutable bound : P.t;
    mutable eliminated : bool;
    mutable dependents : var list;
  }

  type lat = C.t
  type poly = P.t

  let new_var =
    let r = ref 0 in
    fun (name : V.t) ->
      let x = !r in
      r := x + 1;
      { name; id = x; bound = P.top; eliminated = false; dependents = [] }

  let var (v : var) : poly = failwith "unimplemented"
  let const (_c : lat) : poly = failwith "unimplemented"
  let top : poly = failwith "unimplemented"
  let bot : poly = failwith "unimplemented"
  let join (_a : poly) (_b : poly) : poly = failwith "unimplemented"
  let meet (_a : poly) (_b : poly) : poly = failwith "unimplemented"
  let assert_leq (_v : var) (_p : poly) : unit = failwith "unimplemented"
  let solve_lfp (_v : var) (_p : poly) : unit = failwith "unimplemented"
  let leq (_a : poly) (_b : poly) : bool = failwith "unimplemented"
  let normalize (_p : poly) : (lat * V.t list) list = failwith "unimplemented"
end
