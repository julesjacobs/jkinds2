module type SHAPE = sig
  val axis_sizes : int array (* n_i >= 1 *)
end

module Make (S : SHAPE) = struct
  type t = int

  let axis_sizes = Array.copy S.axis_sizes
  let num_axes = Array.length axis_sizes
  let axis_bits = Array.init num_axes (fun i -> max 0 (axis_sizes.(i) - 1))

  let axis_offsets =
    let off = Array.make num_axes 0 in
    let acc = ref 0 in
    for i = 0 to num_axes - 1 do
      off.(i) <- !acc;
      acc := !acc + axis_bits.(i)
    done;
    off

  let total_bits = Array.fold_left ( + ) 0 axis_bits

  let () =
    (* OCaml int has 63 usable bits on 64-bit platforms *)
    if total_bits > Sys.int_size - 1 then
      invalid_arg "Product_lattice: total bits exceed OCaml int capacity"

  let popcount n =
    let rec loop x acc =
      if x = 0 then acc else loop (x land (x - 1)) (acc + 1)
    in
    loop n 0

  (* Precompute masks and tables for fast get/set *)
  let full_masks =
    Array.init num_axes (fun i ->
        let bits = axis_bits.(i) in
        if bits = 0 then 0 else ((1 lsl bits) - 1) lsl axis_offsets.(i))

  let clear_masks = Array.init num_axes (fun i -> lnot full_masks.(i))
  let all_mask = Array.fold_left ( lor ) 0 full_masks

  let level_masks =
    Array.init num_axes (fun i ->
        let n = axis_sizes.(i) in
        let off = axis_offsets.(i) in
        Array.init n (fun lev ->
            if lev = 0 then 0 else ((1 lsl lev) - 1) lsl off))

  (* Bit-parallel helpers for co_sub *)
  let max_axis_bits = Array.fold_left max 0 axis_bits

  let group_masks_by_bits =
    let arr = Array.make (max_axis_bits + 1) 0 in
    for i = 0 to num_axes - 1 do
      let w = axis_bits.(i) in
      if w > 0 then arr.(w) <- arr.(w) lor full_masks.(i)
    done;
    arr

  let decode_levels =
    Array.init num_axes (fun i ->
        let bits = axis_bits.(i) in
        if bits = 0 then [| 0 |]
        else
          let size = 1 lsl bits in
          Array.init size (fun seg -> popcount seg))

  let bot = 0

  let top =
    let v = ref 0 in
    for i = 0 to num_axes - 1 do
      let top_level = axis_sizes.(i) - 1 in
      v := !v lor level_masks.(i).(top_level)
    done;
    !v

  let join (a : t) (b : t) : t = a lor b
  let meet (a : t) (b : t) : t = a land b
  let leq (a : t) (b : t) : bool = a land b = a
  let equal (a : t) (b : t) : bool = leq a b && leq b a

  let co_sub (a : t) (b : t) : t =
    (* Bit-parallel within groups of equal width; avoid per-axis inspect. *)
    let diff = a land lnot b land all_mask in
    if diff = 0 then 0
    else
      let keep = ref 0 in
      (* For each width w, propagate any 1 within that w-bit field across the
         field. *)
      for w = 1 to max_axis_bits do
        let m = group_masks_by_bits.(w) in
        if m <> 0 then (
          let x = ref (diff land m) in
          (* propagate right within fields (powers of two stride) *)
          let k = ref 1 in
          while !k < w do
            x := !x lor ((!x lsr !k) land m);
            k := !k lsl 1
          done;
          (* propagate left within fields *)
          let k = ref 1 in
          while !k < w do
            x := !x lor ((!x lsl !k) land m);
            k := !k lsl 1
          done;
          keep := !keep lor !x)
      done;
      a land !keep

  let get_axis (v : t) ~axis:i : int =
    if i < 0 || i >= num_axes then invalid_arg "get_axis: axis out of range";
    let bits = axis_bits.(i) in
    let mask = if bits = 0 then 0 else (1 lsl bits) - 1 in
    let seg = (v lsr axis_offsets.(i)) land mask in
    decode_levels.(i).(seg)

  let set_axis (v : t) ~axis:i ~level:lev : t =
    if i < 0 || i >= num_axes then invalid_arg "set_axis: axis out of range";
    let n = axis_sizes.(i) in
    if lev < 0 || lev >= n then invalid_arg "set_axis: level out of range";
    let cleared = v land clear_masks.(i) in
    cleared lor level_masks.(i).(lev)

  let encode ~levels : t =
    if Array.length levels <> num_axes then invalid_arg "encode: wrong arity";
    let v = ref 0 in
    for i = 0 to num_axes - 1 do
      let lev = levels.(i) in
      let n = axis_sizes.(i) in
      if lev < 0 || lev >= n then invalid_arg "encode: level out of range";
      v := !v lor level_masks.(i).(lev)
    done;
    !v

  let decode (v : t) : int array =
    Array.init num_axes (fun i -> get_axis v ~axis:i)

  let pp ?axis_names:_ (v : t) : string =
    let levels = decode v in
    let parts =
      levels |> Array.to_list |> List.map string_of_int |> String.concat ", "
    in
    Printf.sprintf "[%s]" parts

  let to_string (v : t) : string =
    let levels = decode v in
    let parts =
      levels |> Array.to_list |> List.map string_of_int |> String.concat ","
    in
    Printf.sprintf "[%s]" parts
end
