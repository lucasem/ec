open Interface

let prim_combs = C.prims @ Combs_list.combs
let deserialize_problem json_p = let open Yojson.Basic.Util in
  { i=json_p |> member "i" |> to_list |> List.map ~f:to_int |> (fun l -> [Expr.of_int_list l]);
    o=json_p |> member "o" |> to_list |> List.map ~f:to_int |> (Obj.magic @@ ref); }
;;
Entry.execute prim_combs deserialize_problem
