module type S = sig
  type var
  type lat
  type poly

  (* Create a fresh solver variable *)
  val new_var : unit -> var

  (* Variables and constants are leaves of polys *)
  val var : var -> poly
  val const : lat -> poly

  (* Lattice structure on polys *)
  val top : poly
  val bot : poly
  val join : poly -> poly -> poly
  val meet : poly -> poly -> poly

  (* Assert a constraint var ≤ poly; must have a variable on the left *)
  (* Used for abstract types *)
  val assert_leq : var -> poly -> unit

  (* Solve the least fixpoint for var = poly[var] *)
  (* Used for concrete types *)
  val solve_lfp : var -> poly -> unit

  (* Pure check: polynomial ≤ polynomial *)
  (* Used for kind subsumption *)
  val leq : poly -> poly -> bool
end

module Make
    (C : Lattice_intf.LATTICE)
    (V : sig
      type t

      val compare : t -> t -> int
    end) : sig
  module P : module type of Lattice_polynomial.Make (C) (V)
  include S with type var = V.t and type poly = P.t
end
