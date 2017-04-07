open Core.Std

open Library
open Expression
open Type
open Utils
open Task


let reduce_symmetries = true;;
let filter_enumerated = true;;

let do_not_prune e = false;;
let enumerate_bounded ?prune:(prune = do_not_prune) (* testing of expressions as they are generated *)
    dagger (log_application,distribution) rt bound =
  let number_pruned = ref 0 in
  let log_terminal = log (1.0-. exp log_application) in
  let terminals = List.map distribution ~f:(fun (e,(l,t)) ->
      (t,(e, l))) in
  let type_blacklist = TID(0) :: ([c_S;c_B;c_C;c_K;c_F;c_I] |> List.map ~f:terminal_type) in
  let rec enumerate can_identify requestedType budget k =
    (* first do the terminals *)
    let availableTerminals = List.filter terminals ~f:(fun (t,(e,_)) ->
        can_unify t requestedType && (not (reduce_symmetries) || can_identify || compare_expression c_I e <> 0)) in
    let z = lse_list (List.map availableTerminals ~f:(fun (_,(_,l)) -> l)) in
    let availableTerminals = List.map availableTerminals ~f:(fun (t,(e,l)) -> (t,(e,l-.z))) in
    let availableTerminals = List.filter availableTerminals ~f:(fun (t,(_,l)) -> 0.0-.log_terminal-.l < budget) in
    List.iter availableTerminals ~f:(fun (t,(e,l)) ->
        let it = safe_get_some "enumeration: availableTerminals" (instantiated_type t requestedType) in
        k e (log_terminal+.l) it t);
    if budget > -.log_application then
    let f_request = TID(next_type_variable requestedType) @> requestedType in
    enumerate false f_request (budget+.log_application) (fun f fun_l fun_type fun_general_type ->
        try
          let x_request = argument_request requestedType fun_type in
          enumerate true x_request (budget+.log_application+.fun_l) (fun x arg_l arg_type arg_general_type ->
              try
                let my_type = application_type fun_type arg_type in
                let my_general_type = application_type fun_general_type arg_general_type in
                let reified_type = instantiated_type my_type requestedType in
                if not ((reduce_symmetries && List.mem type_blacklist my_type) || not (is_some reified_type)
                        || (reduce_symmetries && List.mem type_blacklist my_general_type))
                then k (Application(f,x))
                    (arg_l+.fun_l+.log_application) (get_some reified_type)
                    my_general_type
              with _ -> () (* type error *))
        with _ -> () (* type error *))
  in
  let hits = Int.Table.create () in
  let start_time = time () in
  enumerate true rt bound (fun i _ _ _ ->
      if not (prune i) then
        Hashtbl.set hits ~key:(insert_expression dagger i) ~data:((time ())-.start_time)
      else incr number_pruned);
  (Hashtbl.to_alist hits, !number_pruned)


(* iterative deepening version of enumerate_bounded *)
let enumerate_ID ?prune:(prune = do_not_prune) dagger library t frontier_size =
  let rec iterate bound =
    let (indices, number_pruned) = enumerate_bounded ~prune dagger library t bound in
    if List.length indices + number_pruned < frontier_size
    then iterate (bound+.0.5)
    else indices
  in iterate (1.5 *. log (Float.of_int frontier_size))


let always_sampled_prior = false (* forces us to enumerate from the prior *)

let enumerate_frontiers_for_tasks grammar frontier_size tasks
  : (tp*(int*float) list) list*expressionGraph = (* this only enumerates normal tasks *)
  let start_time = time () in
  let normal_tasks = List.filter tasks ~f:(fun t -> is_none @@ t.proposal) in
  let types = remove_duplicates (List.map tasks ~f:(fun t -> t.task_type)) in
  let dagger = make_expression_graph 100000 in
  let indices = List.map types ~f:(fun t ->
      if always_sampled_prior || not (List.is_empty normal_tasks)
      then enumerate_ID dagger grammar t frontier_size
      else []) in
  let end_time = time () in
  let time_enumerate = end_time-.start_time in
  let indices = List.zip_exn types @@ List.map indices ~f:(fun ids_with_times ->
        let tab = Int.Table.create () in
        List.iter ids_with_times ~f:(fun (id,dt) -> Int.Table.set tab ~key:id ~data:dt);
        tab) in
  (* disregard special tasks *)
  (indices |> List.map ~f:(fun (t,tab) -> (t,Int.Table.to_alist tab)), dagger)

(* computes likelihood of MAP parse *)
let rec map_likelihood g request e =
  let (log_application,library) = g in
  let log_terminal = log (1.0 -. exp log_application) in
  (* get all of the different types we can choose from *)
  let terminal_types =
    List.map library ~f:(fun (_,(l,t)) -> (t,l)) in
  let terminal_probability =
    let numerator = List.fold_left library ~init:Float.neg_infinity ~f:(fun a (j,(l,_)) ->
        if expression_equal e j then max a l else a) in
    if is_invalid numerator
    then Float.neg_infinity
    else let z = lse_list (List.map ~f:snd (List.filter terminal_types ~f:(fun (t,_) ->
        can_unify t request))) in
      numerator+.log_terminal-.z
  in match e with
  | Application(f,x) ->
    let left_request = TID(next_type_variable request) @> request in
    let right_type = infer_type f in
    let right_request = argument_request request right_type in
    let application_probability = log_application+. map_likelihood g left_request f
                                  +. map_likelihood g right_request x in
    max terminal_probability application_probability
  | _ -> terminal_probability
