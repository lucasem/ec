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
  enumerate true rt bound (fun i _ _ _ ->
      if not (prune i) then
        Hashtbl.set hits ~key:(insert_expression dagger i) ~data:true
      else incr number_pruned);
  (Hashtbl.keys hits, !number_pruned)


(* iterative deepening version of enumerate_bounded *)
let enumerate_ID ?prune:(prune = do_not_prune) dagger library t frontier_size =
  let rec iterate bound =
    let (indices, number_pruned) = enumerate_bounded ~prune dagger library t bound in
    if !number_of_cores = 1 || List.length indices + number_pruned >= frontier_size then begin
      Printf.printf "Type %s \t Bound %f \t  => %i (%i) / %i programs" (string_of_type t) bound
        (List.length indices) (List.length indices + number_pruned) frontier_size;
      print_newline ()
    end;
    if List.length indices + number_pruned < frontier_size
    then iterate (bound+.0.5)
    else indices
  in iterate (1.5 *. log (Float.of_int frontier_size))


let always_sampled_prior = false (* forces us to enumerate from the prior *)

let enumerate_frontiers_for_tasks grammar frontier_size tasks
  : (tp*int list) list*expressionGraph =
  let start_time = time () in
  let (special_tasks,normal_tasks) = List.partition_tf tasks ~f:(fun t -> is_some @@ t.proposal) in
  let types = remove_duplicates (List.map tasks ~f:(fun t -> t.task_type)) in
  Printf.printf "number of (normal) types: %i \n" (List.length types);
  let dagger = make_expression_graph 100000 in
  let indices = List.map types ~f:(fun t ->
      if always_sampled_prior || not (List.is_empty normal_tasks)
      then enumerate_ID dagger grammar t frontier_size
      else []) in
  let special_indices =
    let parallel_results = parallel_map special_tasks ~f:(fun t ->
        let dagger = make_expression_graph 10000 in
        Printf.printf "Enumerating for task \"%s\"" t.name; print_newline ();
        let special_grammar = modify_grammar grammar t in
        let l = task_likelihood t in
        let prune j = not @@ is_valid @@ l j in
        let special_indices = enumerate_ID ~prune dagger special_grammar t.task_type frontier_size in
        scrub_graph dagger;
        (dagger, t.task_type, List.fold_left special_indices
           ~f:Int.Set.add ~init:Int.Set.empty)) in
    List.map parallel_results ~f:(fun (d,t,i) ->
      dirty_graph d;
      (t,Int.Set.map i ~f:(insert_expression dagger % extract_expression d))) in
  let end_time = time () in
  Printf.printf "Enumerated all programs in %f seconds." (end_time-.start_time);
  print_newline ();
  let start_time = time() in
  let indices = List.zip_exn types @@
    List.map indices ~f:(fun iDs ->
        List.fold_left iDs ~init:Int.Set.empty ~f:Int.Set.add) in
  (* combines special indices with normal indices *)
  let indices = List.fold_left special_indices ~f:(fun i (ty,j) ->
      i |> List.map ~f:(fun (ty2,j2) -> if ty = ty2
                         then (ty2,Int.Set.union j j2)
                         else (ty2,j2))) ~init:indices in
  let number_of_programs = indices |> List.map ~f:snd |>
                           List.fold_left ~f:Int.Set.union ~init:Int.Set.empty |>
                           Int.Set.length in
  let end_time = time() in
  Printf.printf "Coalesced %i programs in %f seconds." number_of_programs (end_time-.start_time);
  print_newline ();
  (indices |> List.map ~f:(fun (t,s) -> (t,Int.Set.to_list s)), dagger)

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
