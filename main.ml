open Interface

let prim_combs = C.prims @ Str_ops.combs

let load_json file =
  let open Yojson.Basic.Util in
  let deserialize_problem json_p =
    { i=json_p |> member "i" |> to_string |> Expr.of_str;
      o=json_p |> member "o" |> to_string; } in
  let deserialize_problems json_ps = List.map json_ps ~f:deserialize_problem in
  let deserialize_task variant json_t =
    task_of_problems ~t:(T.arrow T.s T.s) ~name:(json_t |> member "name" |> to_string)
      @@ deserialize_problems (json_t |> member variant |> to_list) in
  let deserialize_comb json_c = Expr.unmarshal (json_c |> member "expr" |> to_string) prim_combs in
  let json = Yojson.Basic.from_file file in
  let json_tasks  = json |> member "tasks" |> to_list
  and json_combs = json |> member "grammar" |> to_list in
  let train = List.map json_tasks ~f:(deserialize_task "train")
  and test  = List.map json_tasks ~f:(deserialize_task "test")
  and combs = List.map json_combs ~f:deserialize_comb in
  let combs = List.dedup (prim_combs @ combs) in
  train, test, combs

let verify progs (test : Task.t List.t) =
  List.map progs ~f:(fun prog ->
    let (name, res) = prog in
    match res with
    | None -> prog
    | Some((_, _, e, _)) ->
        let task = List.find test ~f:(fun t->t.name=name) in
        match task with
        | None -> prog
        | Some(task) ->
            match task.score with
            | Seed(_) -> prog
            | LogLikelihood(ll) ->
                if Utils.is_valid (ll e)
                then prog
                else (name, None)
  )

let json_of_ec_results grammar progs hit_rate =
  let open Yojson.Basic in
  let json_grammar = `List (List.map grammar ~f:(fun (e,l) ->
    `Assoc [ ("expr", `String e); ("log_likelihood", `Float l) ]))
  and json_progs = `List (List.map progs ~f:(fun (name, res) ->
    `Assoc [ ("task", `String name); ("result",
      match res with
        | None -> `Null
        | Some((p, es, _, dt)) -> `Assoc [
          ("log_probability", `Float p);
          ("expr", `String es);
          ("time", `Float dt) ]
    )])) in
  `Assoc [
    ("grammar", json_grammar);
    ("programs", json_progs);
    ("hit_rate", `Int hit_rate);
  ]

let main () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Failure: must supply tasks file as argument\n";
    exit 1
  end;
  let train, test, combs = load_json Sys.argv.(1) in
  let grammar, progs =
    ec combs train 5 ~lambda:0.5 ~smoothing:0.6 ~frontier_size:2000 in
  let progs = verify progs test in
  let hit_rate =
    List.fold_left ~f:(+) ~init:0 @@ List.map progs
      ~f:(fun (_, res) -> match res with
        | Some(_) -> 1
        | None    -> 0
      ) in
  Yojson.Basic.pretty_to_channel stdout
    @@ json_of_ec_results grammar progs hit_rate
;;

main ();;

