open Interface

let prim_combs = C.prims @ Combs_str.combs
let tp = T.arrow T.s T.s
let deserialize_problem json_p = let open Yojson.Basic.Util in
  { i=json_p |> member "i" |> to_string |> Expr.of_str;
    o=json_p |> member "o" |> to_string; }
;;
Entry.execute prim_combs tp deserialize_problem
