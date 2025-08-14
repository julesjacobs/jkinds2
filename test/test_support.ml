let assert_true msg b = 
  if not b then failwith ("assert failed: " ^ msg)

let assert_eq msg expected actual = 
  if expected <> actual then 
    failwith (Printf.sprintf "assert_eq failed: %s" msg)

let assert_false msg b = 
  if b then failwith ("assert_false failed: " ^ msg)


