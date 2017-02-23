open Interface

let combs = C.prims @ Str_ops.combs

let load_tasks file =
  let open Yojson.Basic.Util in
  let deserialize_problem json_p =
    { i=json_p |> member "i" |> to_string |> Expr.of_str;
      o=json_p |> member "o" |> to_string; } in
  let deserialize_problems json_ps = List.map json_ps ~f:deserialize_problem in
  let deserialize_task json_t =
    task_of_problems ~t:(T.arrow T.s T.s) ~name:(json_t |> member "name" |> to_string)
      @@ deserialize_problems (json_t |> member "problems" |> to_list) in
  let json = Yojson.Basic.from_file file in
  let json_tasks = json |> member "tasks" |> to_list in
  let tasks = List.map json_tasks ~f:deserialize_task in
  tasks

let main () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Failure: must supply tasks file as argument\n";
    exit 1
  end;
  ec combs (load_tasks Sys.argv.(1)) 16
  ~lambda:1.5
  ~smoothing:1.0
  ~frontier_size:1000
  ~log_prefix:"kn"
;;
main ();;
