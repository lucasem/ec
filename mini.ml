open Interface

let prims = C.prims @ Str_ops.combs

let main () =
  let s = "((B ((S (C (substr 0))) (findchar <DOT>))) ((S (C ((B ((C substr) (-1 0))) +1))) (findchar <LESS-THAN>)))" in
  let f = Expr.unmarshal s prims in
  let x = Expr.of_str "#include <os.h>" in
  let q = Expr.Application(f, x) in
  let r = Expr.run q in
  print_string (match r with
  | Some(out) -> out
  | _ -> "FAIL")
;;

main ();;
