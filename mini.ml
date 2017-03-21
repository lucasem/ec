open Interface

let prims = C.prims @ Str_ops.combs

let main () =
  let s = "((B ((B ((replace-substr-all empty) (string-of-char <SPACE>))) (feach ((substr 0) (+1 0))))) (filter-words ((S is) cap)))" in
  let f = Expr.unmarshal s prims in
  let x = Expr.of_str "Structure and Interpretation of Computer Programs" in
  let q = Expr.Application(f, x) in
  let r = Expr.run q in
  print_string (match r with
  | Some(out) -> out
  | _ -> "FAIL")
;;

main ();;
