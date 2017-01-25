open Core.Std

open Expression
open Type
open Library
open Utils
open Task

let task_features dagger (log_application,library) tasks_and_solutions =
  let log_terminal = log (1. -. exp log_application) in
  let program_types = infer_graph_types dagger in
  let requests = List.fold_left tasks_and_solutions ~init:Int.Map.empty ~f:(fun r (t,solutions) ->
      List.fold_left solutions ~init:r ~f:(fun a s ->
          try
            let old = Int.Map.find_exn a s in
            if List.mem old t.task_type then a else Int.Map.add a s (t.task_type :: old)
          with Not_found -> Int.Map.add a s [t.task_type])) in
  let likelihoods = program_likelihoods (log_application,library) dagger program_types requests in
  (* find all of the production features *)
  let production_features = List.mapi library (fun i (e,(l,t)) ->
    (insert_expression dagger e,(i,(l,t)))) in
  (* variables for holding expected counts *)
  let expected_applications = ref Float.neg_infinity in
  let expected_terminals = ref Float.neg_infinity in
  let expected_uses = Array.create (List.length production_features) Float.neg_infinity in
  (* fills in the above expected counts *)
  let rec count_uses weight i request =
    if not (is_wildcard dagger i) then begin
      let l = Hashtbl.find_exn likelihoods (i,request) in
      (* if it is in library compute uses if the production was used *)
      let hits = List.filter production_features ~f:(fun (j,_) -> can_match_wildcards dagger i j) in
      if not (List.is_empty hits) then begin
        let offsets = List.map hits ~f:(fun (_,(o,(l,_))) -> (o,l)) in
        let offset_Z = lse_list @@ List.map offsets snd in
        let offsets = List.map offsets ~f:(fun (o,l) -> (o,l-.offset_Z)) in
        let others = List.filter production_features (fun (_,(_,(_,t))) -> can_unify t request) in
        let other_offsets = List.map others (fun (_,(o,(_,_))) -> o) in
        let z = lse_list (List.map others (fun (_,(_,(l,_))) -> l)) in
        let terminal_likelihood = log_terminal+.offset_Z-.z -.l in
        List.iter offsets ~f:(fun (o,l) -> expected_uses.(o) <-
                               lse expected_uses.(o) (l+.terminal_likelihood+.weight));
        expected_terminals := lse !expected_terminals (terminal_likelihood+.weight)
      end;
      match extract_node dagger i with
      (* we have no children, don't recurse *)
      | ExpressionLeaf(_) -> ()
      (* recurse on function and argument *)
      | ExpressionBranch(f,x) ->
        (* get probability that an application was used *)
        let left_request = function_request request in
        let right_request = argument_request request program_types.(f) in
        let left_probability = if is_wildcard dagger f
          then 0.
          else Hashtbl.find_exn likelihoods (f,left_request) in
        let right_probability = if is_wildcard dagger x
          then 0.
          else Hashtbl.find_exn likelihoods (x,right_request) in
        let application_likelihood =
          log_application+.left_probability+.right_probability -.l in
        expected_applications :=
          lse !expected_applications (application_likelihood+.weight);
        (* get the uses from the right and the left *)
        count_uses (weight+.application_likelihood) f left_request;
        count_uses (weight+.application_likelihood) x right_request
    end
  in
  List.map tasks_and_solutions ~f:(fun (t,solutions) ->
      let weighted_solutions = List.map solutions
                                 (fun s -> Hashtbl.find_exn likelihoods (s,t.task_type)) in
      let z = lse_list weighted_solutions in
    expected_applications := Float.neg_infinity;
    expected_terminals := Float.neg_infinity;
    Array.fill expected_uses 0 (Array.length expected_uses) Float.neg_infinity;
    List.iter2_exn ~f:(fun w s -> count_uses (w-.z) s t.task_type) weighted_solutions solutions;
    List.map ~f:exp @@
    !expected_applications :: !expected_terminals :: Array.to_list expected_uses)
