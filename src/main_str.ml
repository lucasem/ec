open Interface

let prim_combs = C.prims @ Combs_str.combs
let deserialize_problem json_p = let open Yojson.Basic.Util in
  { i=json_p |> member "i" |> to_string |> (fun s -> [Expr.of_str s]);
    o=json_p |> member "o" |> to_string |> (Obj.magic @@ ref); }
;;
Entry.execute prim_combs deserialize_problem
