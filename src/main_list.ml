open Interface

let prim_combs = C.prims @ Combs_list.combs
let tp = T.arrow T.l T.l
let deserialize_problem json_p = let open Yojson.Basic.Util in
  { i=json_p |> member "i" |> to_list |> List.map ~f:to_int |> Expr.of_list;
    o=json_p |> member "o" |> to_list |> List.map ~f:to_int; }
;;
Entry.execute prim_combs tp deserialize_problem
