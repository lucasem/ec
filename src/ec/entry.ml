open Interface

(*
 * To create a new entry, you need to call Entry.execute.
 * See main_list.ml or main_str.ml to examples.
*)

let command main =
  Core.Command.basic
    ~summary:"run EC on the given tasks (see file format at github.com/lucasem/ec)"
    Core.Command.Spec.(
      empty
      +> flag "-it" (optional_with_default 5 int) ~doc:"integer Number of iterations (default: 5)"
      +> flag "-lambda" (optional_with_default 1.5 float) ~doc:"float Grammar learning cutoff (lower ~ more eager learning) (default: 1.5)"
      +> flag "-smoothing" (optional_with_default 1.0 float) ~doc:"float Grammar parameter estimation factor (lower ~ more eager learning) (default: 1.0)"
      +> flag "-frontier-size" (optional_with_default 5000 int) ~doc:"integer Frontier size (default: 5000)"
      +> anon ("filename" %: string)
    )
    main

let deserialize_type (s : string) : T.t =
  let type_of_string = function
    | "string" -> T.s
    | "list-of-string" -> T.l T.s
    | "char" -> T.c
    | "list-of-char" -> T.l T.c
    | "int" -> T.i
    | "list-of-int" -> T.l T.i
    | "bool" -> T.b
    | "list-of-bool" -> T.l T.b
    | _ -> raise (Failure "unknown type")
  in
  let strs = Re2.Regex.split (Re2.Regex.create_exn "->") s in
  match strs with
  | [] -> raise (Failure "invalid type")
  | [s] -> type_of_string @@ String.trim s
  | _ ->
      let types = List.rev @@ List.map ~f:(fun s -> type_of_string @@ String.trim s) strs in
      let t_last = List.hd_exn types
      and t_second_last = List.nth_exn types 1
      and t_rest = List.drop types 2 in
      List.fold ~init:(T.arrow t_second_last t_last) ~f:T.arrow t_rest

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

let load_json prim_combs deserialize_problem file =
  let open Yojson.Basic.Util in
  let deserialize_problems json_ps = List.map json_ps ~f:deserialize_problem in
  let deserialize_task variant json_t =
    let tp = deserialize_type (json_t |> member "type" |> to_string)
    and name = json_t |> member "name" |> to_string
    and problems = deserialize_problems (json_t |> member variant |> to_list)
    in task_of_problems ~tp ~name problems in
  let deserialize_comb json_c = Expr.unmarshal (json_c |> member "expr" |> to_string) prim_combs in
  let json = match file with
  | "-" -> Yojson.Basic.from_channel stdin
  | _ -> Yojson.Basic.from_file file
  in
  let json_tasks  = json |> member "tasks" |> to_list
  and json_combs = json |> member "grammar" |> to_list in
  let tasks = List.map json_tasks ~f:(deserialize_task "examples")
  and combs = List.map json_combs ~f:deserialize_comb in
  let combs = List.dedup (prim_combs @ combs) in
  tasks, combs

let execute prim_combs deserialize_problem =
  let main it lambda smoothing frontier_size file () =
    let tasks, combs = load_json prim_combs deserialize_problem file in
    let grammar, progs =
      ec combs tasks it ~lambda ~smoothing ~frontier_size in
    let hit_rate =
      List.fold_left ~f:(+) ~init:0 @@ List.map progs
        ~f:(fun (_, res) -> match res with
          | Some(_) -> 1
          | None    -> 0
        ) in
    Yojson.Basic.pretty_to_channel stdout
      @@ json_of_ec_results grammar progs hit_rate
  in Core.Command.run ~version:"2.0" (command main)
