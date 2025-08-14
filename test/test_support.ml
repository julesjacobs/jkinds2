let assert_true msg b = if not b then failwith ("assert: " ^ msg)
let assert_eq msg a b = if a <> b then failwith ("assert: " ^ msg)


