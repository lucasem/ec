open Core.Std

open Type
open Library
open Task
open Enumerate
open Bottom_up
open Expression
open Utils
open Expression


let frontier_requests frontiers =
  List.fold_left frontiers
    ~init:Int.Map.empty ~f:(fun requests (requested_type,frontier) ->
        List.fold_left frontier ~init:requests ~f:(fun (a : (tp list) Int.Map.t) (i : int) ->
            match Int.Map.find a i with
            | Some(old) ->
              if List.mem old requested_type then a
              else Int.Map.add a ~key:i ~data:(requested_type::old)
            | None -> Int.Map.add a ~key:i ~data:[requested_type]))

(* wrapper over various enumeration and scoring functions *)
(* returns a list of frontiers, one for each task *)
(* each frontier is a list of tuples of (expression ID,log likelihood,log prior) *)
(* also returns an expression graph *)
let make_frontiers size keep_size grammar tasks =
  let (bottom_tasks,top_tasks) = List.partition_tf tasks ~f:(fun t ->
      match t.score with
      | Seed(_) -> true
      | LogLikelihood(_) -> false) in
  let (dagger,top_program_scores) =
    if List.is_empty (top_tasks)
    then (make_expression_graph 100000,[])
    else
      let (top_frontiers, dagger) = enumerate_frontiers_for_tasks grammar size tasks in
      let top_program_scores = score_programs dagger top_frontiers tasks in
      (* display the hit rate *)
      begin
        let number_hit = List.length (List.filter top_program_scores ~f:(fun scores ->
            List.exists scores ~f:(fun (_,s) -> s > log (0.999)))) in
        let number_of_partial = List.length (List.filter ~f:(fun scores ->
            List.length scores > 0
          ) top_program_scores) in
        (* dynamic program for likelihoods *)
        let requests = frontier_requests top_frontiers in
        let likelihoods = program_likelihoods grammar dagger (infer_graph_types dagger) requests in
        let top_program_scores = List.map2_exn tasks top_program_scores ~f:(fun t ss ->
            List.map ss ~f:(fun (i,s) ->
                (i,s,Hashtbl.find_exn likelihoods (i,t.task_type)))) in
        (dagger, top_program_scores)
      end
  in let bottom_program_scores =
    if List.is_empty (bottom_tasks)
    then []
    else begin
      let rewrites = snd grammar |> List.map ~f:(fun (e,(_,t)) ->
          (* load primitives into the graph *)
          ignore(insert_expression dagger e);
          get_templates e t |> List.map ~f:(fun (target,template) ->
              (template,apply_template target))) |> List.concat in
      let graphs_and_frontiers = parallel_map bottom_tasks ~f:(fun t ->
          let temp_dagger = make_expression_graph 10000 in
          let i = insert_expression temp_dagger @@ match t.score with
            | Seed(s) -> s
            | LogLikelihood(_) -> raise (Failure "make_frontiers: partask has no seed") in
          let f = backward_enumerate temp_dagger grammar rewrites size keep_size t.task_type i in
          scrub_graph temp_dagger;
          (temp_dagger,f)) in
      List.map graphs_and_frontiers ~f:(fun (g,f) ->
          dirty_graph g;
          List.map f ~f:(fun (i,l) -> (insert_expression dagger @@ extract_expression g i,0.0,l)))
    end
  in
  (* coalesced top and bottom *)
  let bottom = ref bottom_program_scores
  and top = ref top_program_scores in
  let fs = List.map tasks ~f:(fun t ->
      let f = match t.score with
        | Seed(_) -> bottom
        | LogLikelihood(_) -> top in
      let x = List.hd_exn !f in
      f := List.tl_exn !f;
      x)
  in (dagger, fs)


(* spit out something that is similar to the posterior; *)
let bic_posterior_surrogate ?print:(print = false) lambda dagger grammar task_solutions =
  let likelihood = List.fold_left task_solutions ~init:0. ~f:(fun l (t,f) ->
      if List.length f > 0
      then l +. lse_list (List.map f ~f:(fun (i,s) ->
          s+.get_some (likelihood_option grammar t.task_type (extract_expression dagger i))))
      else l) in
  let m = Float.of_int (List.length @@ snd grammar) in
  let n = List.fold_left ~init:0 task_solutions ~f:(fun n (_,f) ->
      if List.length f > 0 then n + 1 else n) in
  let prior = -. lambda *. m in
  let bic = -.0.5 *. m *. (log @@ Float.of_int n) in
  (if print then Printf.printf "Log Prior (%f) + Log Likelihood (%f) + BIC (%f) = \n\t%f\n"
       prior likelihood bic (prior +. likelihood +. bic));
  (prior +. likelihood +. bic)
