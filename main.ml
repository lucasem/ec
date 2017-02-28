open Interface

let prim_combs = C.prims @ Str_ops.combs

let load_json file =
  let open Yojson.Basic.Util in
  let deserialize_problem json_p =
    { i=json_p |> member "i" |> to_string |> Expr.of_str;
      o=json_p |> member "o" |> to_string; } in
  let deserialize_problems json_ps = List.map json_ps ~f:deserialize_problem in
  let deserialize_task json_t =
    task_of_problems ~t:(T.arrow T.s T.s) ~name:(json_t |> member "name" |> to_string)
      @@ deserialize_problems (json_t |> member "problems" |> to_list) in
  let deserialize_comb json_c = Expr.unmarshal (json_c |> member "expr" |> to_string) in
  let json = Yojson.Basic.from_file file in
  let json_tasks = json |> member "tasks" |> to_list
  and json_combs = json |> member "grammar" |> to_list in
  let tasks = List.map json_tasks ~f:deserialize_task
  and combs = List.map json_combs ~f:deserialize_comb in
  let combs = List.dedup (prim_combs @ combs) in
  tasks, combs

let json_of_ec_results grammar progs bic hit_rate =
  let open Yojson.Basic in
  let json_grammar = `List (List.map grammar ~f:(fun (e,l) ->
    `Assoc [ ("expr", `String e); ("likelihood", `Float l) ]))
  and json_progs = `List (List.map progs ~f:(fun (name, res) ->
    `Assoc [ ("task", `String name); ("result",
      match res with
        | None -> `Null
        | Some((p, e)) -> `Assoc [
          ("probability", `Float (exp p));
          ("expr", `String e) ]
    )])) in
  `Assoc [
    ("grammar", json_grammar);
    ("programs", json_progs);
    ("bic", `Float (exp bic));
    ("hit_rate", `Int hit_rate);
  ]

let main () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Failure: must supply tasks file as argument\n";
    exit 1
  end;
  let tasks, combs = load_json Sys.argv.(1) in
  let grammar, progs, bic, hit_rate =
    ec combs tasks 4
    ~lambda:1.5
    ~smoothing:1.0
    ~frontier_size:1000 in
  Yojson.Basic.pretty_to_channel stdout
    @@ json_of_ec_results grammar progs bic hit_rate
;;
main ();;
