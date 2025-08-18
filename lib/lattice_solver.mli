(* Solver for (in)equalities over multivariate lattice polynomials *)
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
(* This checks subsumption (leq p q == forall xs, p(xs) <= q(xs)) under two types of hypotheses:
   1. Inequalities (assert_leq x q == forall xs, x <= q(x,xs))
   2. Least fixpoint equations (solve_lpf x p == substitute x := least solution of x = p(x,xs))
*)

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
  (* Can be used on the same var any number of times, used for GADTs *)
  val assert_leq : var -> poly -> unit

  (* Solve the least fixpoint for var = poly[var] *)
  (* Used for concrete types *)
  (* This function can only be used on a var once;
     after that the var is solved, and no longer a var *)
  (* Returns whether the equation violated an existing inequality,
     and if so, the solver state was not modified *)
  val solve_lfp : var -> poly -> bool

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
