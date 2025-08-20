(* Axis lattice specialized from the generic product lattice. *)
module T = Product_lattice.Make (struct
  let axis_sizes = [| 3; 2 |]
end)

include T
