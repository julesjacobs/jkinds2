module type S = sig
  type var
  type poly

  (* Create a fresh solver variable *)
  val new_var : unit -> var

  (* Assert a constraint var ≤ poly *)
  val assert_leq : var -> poly -> unit

  (* Pure check: polynomial ≤ polynomial *)
  val leq : poly -> poly -> bool

  (* Solve the least fixpoint for var = poly[var], returning the solution polynomial *)
  val solve_lfp : var -> poly -> poly
end

module Make
  (C : Lattice_intf.LATTICE)
  (V : sig type t val compare : t -> t -> int end)
  : sig
  module P : module type of Lattice_polynomial.Make(C)(V)

  include S with type var = V.t and type poly = P.t
end


