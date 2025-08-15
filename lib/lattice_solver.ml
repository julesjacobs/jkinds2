module type S = sig
  type var
  type poly

  val new_var : unit -> var
  val assert_leq : var -> poly -> unit
  val leq : poly -> poly -> bool
  val solve_lfp : var -> poly -> poly
end

module Make
    (C : Lattice_intf.LATTICE)
    (V : sig
      type t

      val compare : t -> t -> int
    end) =
struct
  module P = Lattice_polynomial.Make (C) (V)

  type var = V.t
  type poly = P.t

  let new_var () : var = failwith "Unimplemented"
  let assert_leq (_v : var) (_p : poly) : unit = failwith "Unimplemented"
  let leq (_p : poly) (_q : poly) : bool = failwith "Unimplemented"
  let solve_lfp (_v : var) (_body : poly) : poly = failwith "Unimplemented"
end
