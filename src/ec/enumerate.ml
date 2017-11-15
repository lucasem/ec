open Core

open Library
open Expression
open Type
open Utils
open Task


let always_sampled_prior = false

let enumerate_bounded
  (dagger : expressionGraph)
  ((l_application : float),
   (distribution : (expression*(float*tp)) list))
  (tp_req: tp)
  (bound : float)
  : (int(*graph index for matching expression*)*float(*time to enumerate*)) list =
  let l_terminal = log (1.0-. exp l_application)
  and type_blacklist = TID(0) :: ([c_S;c_B;c_C;c_K;c_F;c_I] |> List.map ~f:terminal_type) in
  let rec enumerate
    (tp_req : tp)
    (allow_identity : bool)
    (budget : float)
    (callback : expression->float(*loglikelihood*)->tp(*type*)->tp(*general type*)->unit)
    =
    (* terminals *)
    let terminals = List.filter distribution ~f:(fun (e,(_,t)) ->
      can_unify t tp_req &&
      (allow_identity || not (compare_expression c_I e = 0))) in
    let z = lse_list @@ List.map terminals ~f:(fun (_,(l,_)) -> l) in
    List.iter terminals ~f:(fun (e,(l,t)) ->
      let l = l_terminal+.l-.z in
      if -.l < budget then
        let it = get_some (instantiated_type t tp_req) in
        callback e l it t);
    (* nonterminals *)
    if -.l_application < budget then
    let tp_f_req = make_arrow (new_type_variable tp_req) tp_req in
    enumerate tp_f_req false (budget+.l_application) (fun f l_f tp_f tp_f_general ->
      let tp_x_req = argument_request tp_req tp_f in
      enumerate tp_x_req true (budget+.l_application+.l_f) (fun x l_x tp_x tp_x_general ->
        let tp_this = application_type tp_f tp_x
        and tp_general = application_type tp_f_general tp_x_general in
        let tp_reified = instantiated_type tp_this tp_req in
        if (is_some tp_reified
          && not (List.mem type_blacklist tp_this ~equal:(=))
          && not (List.mem type_blacklist tp_general ~equal:(=)))
        then
          let e = Application(f,x)
          and l = l_application +. l_f +. l_x in
          callback e l (get_some tp_reified) tp_general))
  in
  let hits = Int.Table.create ()
  and start_time = Time.now () in
  enumerate tp_req true bound (fun e _ _ _ ->
    let dt = Time.Span.to_sec @@ Time.diff (Time.now ()) start_time in
    Hashtbl.set hits ~key:(insert_expression dagger e) ~data:dt);
  Hashtbl.to_alist hits

(* iterative deepening version of enumerate_bounded *)
let enumerate_ID
  (dagger : expressionGraph)
  (library : library)
  (tp_req : tp)
  (frontier_size : int)
  : (int(*graph index for matching expression*)*float(*time to enumerate*)) list =
  let rec iterate bound =
    let indices = enumerate_bounded dagger library tp_req bound in
    if List.length indices < frontier_size
    then iterate (bound+.0.5)
    else indices
  in iterate (1.5 *. log (Float.of_int frontier_size))

let enumerate_frontiers_for_tasks
  (grammar : library)
  (frontier_size : int)
  (tasks : task list)
  : (tp*(int*float) list) list*expressionGraph = (* this only enumerates normal tasks *)
  let normal_tasks = List.filter tasks ~f:(fun t -> is_none @@ t.proposal) in
  let types = remove_duplicates (List.map tasks ~f:(fun t -> t.task_type)) in
  let dagger = make_expression_graph () in
  let indices = List.map types ~f:(fun t ->
      if always_sampled_prior || not (List.is_empty normal_tasks)
      then enumerate_ID dagger grammar t frontier_size
      else []) in
  let indices = List.zip_exn types @@ List.map indices ~f:(fun ids_with_times ->
        let tab = Int.Table.create () in
        List.iter ids_with_times ~f:(fun (id,dt) -> Int.Table.set tab ~key:id ~data:dt);
        tab) in
  (* disregard special tasks *)
  (indices |> List.map ~f:(fun (t,tab) -> (t,Int.Table.to_alist tab)), dagger)
