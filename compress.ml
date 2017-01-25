open Core.Std

open Expression
open Type
open Library
open Utils
open Task


let minimum_occurrences = 2;; (* how many tasks a tree must occur in to make it into the grammar *)

(* doesn't instantiate pairs of fragments *)
let candidate_ground_fragments dagger solutions =
  let terminals = List.filter (0--(expression_graph_size dagger - 1)) (is_leaf_ID dagger) in
  let candidates = reachable_expressions dagger @@ List.concat solutions in
  let can = Int.Set.elements candidates |> List.filter ~f:(compose not @@ is_leaf_ID dagger) in
  Printf.printf "\nGot %i (ground) candidates." (List.length can); print_newline (); can



(* finds all of the fragments we might consider adding to the grammar
   this can handle the case when the programs have wildcards in them
   the fragments we consider adding should never have wildcards in them
   a fragment without wildcards is included when it occurs in a different task,
   possibly with wildcards.
   if 2 fragments from different tasks unify to a grounded expression,
   that grounded expression gets included as a fragment.
*)
let candidate_fragments dagger solutions =
  print_string "Preparing for fragment merging..."; print_newline ();
  let terminals = List.filter (0--(expression_graph_size dagger - 1)) (is_leaf_ID dagger) in
  let valid_IDs = reachable_expressions dagger @@ List.concat solutions in
  let ground_pairs = 0--(expression_graph_size dagger - 1) |>
                     List.filter ~f:(compose not @@ has_wildcards dagger) |>
                     List.filter ~f:(fun i -> Int.Set.mem valid_IDs i) |>
                     List.map ~f:(fun x -> (x,x,infer_type @@ extract_expression dagger x)) in
  let q = List.find terminals (is_wildcard dagger) in
  (* map from fragment to the tasks that use that fragment *)
  let task_map = Hashtbl.Poly.create () in
  List.iteri solutions ~f:(fun t s ->
      List.iter s (fun i -> get_sub_IDs dagger i |> Int.Set.iter ~f:(fun j ->
          match Hashtbl.find task_map j with
          | Some(old) ->
            Hashtbl.replace task_map ~key:j ~data:(Int.Set.add old t)
          | None -> ignore(Hashtbl.add task_map ~key:j ~data:(Int.Set.singleton t))
        )));
  (* is (m n) a fragment, and if so, are its tasks complementary to those of i? *)
  let compatible i m n =
    match node_in_graph dagger (ExpressionBranch(m,n)) with
    | Some(mn) when Hashtbl.mem task_map mn ->
      let ti = Hashtbl.find_exn task_map i
      and tmn = Hashtbl.find_exn task_map mn in
      if (Int.Set.length ti > 1 || Int.Set.length tmn > 1 || not (Int.Set.equal ti tmn)) &&
         Int.Set.length (Int.Set.union ti tmn) >= minimum_occurrences
      then Some(mn)
      else None
    | _ -> None
  in
  (* map from expression ID to a list of (grounded , other ID) *)
  let instantiations = Int.Table.create () in
  List.iter terminals (fun t ->
      if Some(t) <> q then ignore(Hashtbl.add instantiations t
          ((t,t,terminal_type @@ extract_expression dagger t) ::
           if is_some q
           then [(t,get_some q,terminal_type @@ extract_expression dagger t)]
           else [])));
  print_string "Done preparing."; print_newline ();
  let rec instantiate i =
    match Hashtbl.find instantiations i with
    | Some(z) -> z
    | None ->
      let answer =
        match extract_node dagger i with
        | ExpressionLeaf(l) ->
          raise (Failure("instantiate: terminal not instantiated: " ^ string_of_expression l))
        | ExpressionBranch(left,right) ->
          let left_matches =
            if Some(left) = q
            then ground_pairs
            else instantiate left in
          let right_matches =
            if Some(right) = q
            then ground_pairs
            else instantiate right in
          List.fold_left left_matches ~init:[] ~f:(fun a (f,m,ft) ->
              List.fold_left right_matches ~init:a ~f:(fun b (x,n,xt) ->
                  match compatible i m n with
                  | Some(mn) -> begin
                      try
                        let fxt = application_type ft xt in
                        (insert_expression_node dagger @@ ExpressionBranch(f,x), mn, fxt) :: b
                      with _ -> b (* typing error *)
                    end
                  | None -> b))
      in ignore(Hashtbl.add instantiations i answer); answer
  in
  let candidates = ref Int.Set.empty in
  let bar = make_progress_bar (Hashtbl.length task_map) in
  Hashtbl.iter task_map (fun ~key:i ~data:_ ->
      update_progress_bar bar (bar.current_progress + 1);
      if not (is_leaf_ID dagger i) then
        List.iter (instantiate i) (fun (j,_,_) ->
              candidates := Int.Set.add !candidates j));
  let can = Int.Set.elements !candidates |> List.filter ~f:(compose not @@ is_leaf_ID dagger) in
  Printf.printf "\nGot %i candidates." (List.length can); print_newline (); can


(*   (* for each task, collect up all the fragments into a set *)
     let fragments = solutions |> List.map (Int.Set.empty |> List.fold_left (fun a i ->
     Int.Set.union a @@ get_sub_IDs dagger i)) in
     (* record candidates in place *)
     let candidates = Hashtbl.create 10000 in
     fragments |> List.iter (fun task_fragments ->
     task_fragments |> Int.Set.iter (fun i ->
        try
          let old = Hashtbl.find candidates i in
          Hashtbl.replace candidates i @@ old+1
        with _ -> Hashtbl.add candidates i 1
          ));
     hash_bindings candidates |> List.filter (fun (_,c) -> c > 1) |>
     List.map fst |> List.filter (compose not @@ is_leaf_ID dagger)
*)(*
  let rec get_fragments head_task other_tasks =
    try (* next 2 lines will throw exception once we're done *)
      let next_head = List.hd other_tasks
      and next_tail = List.tl other_tasks in
      (* loop over every solution to the head task;
         collect up the fragments and check to see if they should be included *)
      head_task |> Int.Set.iter (fun fragment ->
          if Hashtbl.mem candidates fragment then () else
          let wild = has_wildcards dagger fragment in
          other_tasks |> List.iter (Int.Set.iter (fun other_fragment ->
              if wild || has_wildcards dagger other_fragment
              then match combine_wildcards dagger fragment other_fragment with
                | Some(union_fragment) when (not (Hashtbl.mem candidates union_fragment)
                                             && not (has_wildcards dagger union_fragment)) ->
                  Hashtbl.add candidates union_fragment true
                | _ -> ()
              else if fragment = other_fragment && not (Hashtbl.mem candidates fragment)
                        then Hashtbl.add candidates fragment true
            )));
      get_fragments next_head next_tail
    with _ -> () (* no more tasks *)
  in
  get_fragments (List.hd fragments) (List.tl fragments);
  hash_bindings candidates |> List.map fst |> List.filter (compose not @@ is_leaf_ID dagger)
 *)



(* equivalent of a null pointer *)
let no_job_ID = -1

let compute_job_IDs dagger type_array terminals candidates requests =
  let (i2n,_,_) = dagger in
  (* number all of the candidates *)
  let candidates = List.mapi candidates (fun i c -> (c,i)) in
  (* maps tuples of (ID,request) to job ID *)
  let jobs = Hashtbl.Poly.create () in
  (* these lists contain information about the jobs *)
  let candidate_index = ref [] in
  let has_children = ref [] in
  let left_child = ref [] in
  let right_child = ref [] in
  let terminal_conflicts = ref [] in
  let candidate_conflicts = ref [] in
  let rec make_job i request =
    if is_wildcard dagger i then no_job_ID else
      match Hashtbl.find jobs (i,request) with
      | Some(z) -> z
      | None ->
        (match Hashtbl.find_exn i2n i with
        | ExpressionLeaf(Terminal(_,_,_)) ->
           has_children := !has_children @ [false];
           left_child := !left_child @ [no_job_ID];
           right_child := !right_child @ [no_job_ID]
         | ExpressionLeaf(_) -> raise (Failure "leaf not terminal")
         | ExpressionBranch(l,r) ->
           let left_request = canonical_type (make_arrow (TID(next_type_variable request)) request) in
           let right_request = argument_request request type_array.(l) in
           let left_job = make_job l left_request in
           let right_job = make_job r right_request in
           has_children := !has_children @ [true];
           left_child := !left_child @ [left_job];
           right_child := !right_child @ [right_job]);
        candidate_index := !candidate_index @
                           [List.map ~f:snd @@ List.filter candidates (fun (c,_) -> can_match_wildcards dagger i c)];
        terminal_conflicts := !terminal_conflicts @
                              [Float.of_int @@ List.length @@ (List.filter terminals (fun t -> can_unify type_array.(t) request))];
        candidate_conflicts := !candidate_conflicts @
                               [List.map ~f:snd @@ (List.filter candidates
                                                      (fun (c,_) -> can_unify type_array.(c) request))
                               ];
        let j = Hashtbl.length jobs in
        ignore(Hashtbl.add jobs ~key:(i,request) ~data:j);
        j
  in
  (* create a job for each request and all of its sub requests *)
  ignore(Int.Map.iter requests (fun ~key:i ~data:types ->
      List.iter types (fun t -> ignore(make_job i t))));
  (* pack everything up into arrays and then return it all *)
  (Array.of_list !candidate_index,
   Array.of_list !has_children,
   Array.of_list !left_child,
   Array.of_list !right_child,
   Array.of_list !terminal_conflicts,
   Array.of_list !candidate_conflicts,
   jobs)


let compress lambda smoothing dagger type_array requests (task_solutions : (task * (int*float) list) list) =
  let t1 = Time.to_float @@ Time.now () in
  let (i2n,_,_) = dagger in
  let terminals = List.map ~f:fst @@ List.filter (Hashtbl.to_alist i2n) (fun (i,_) -> is_leaf_ID dagger i) in
  (* request might have spurious request for programs that don't solve any tasks *)
  let requests = Int.Map.filter requests (fun ~key:i ~data:_ ->
      List.exists task_solutions (fun (_,s) -> List.exists s (fun (j,_) -> j = i))) in
  let candidates = List.map task_solutions (compose (List.map ~f:fst) snd) |>
                   candidate_fragments dagger in
  (* compute the dependencies of each candidate *)
  let dependencies =
    Array.of_list (List.map candidates (fun i ->
        let children = Int.Set.elements @@ get_sub_IDs dagger i in
        let children = List.filter children (List.mem candidates) in
        List.map children (index_of candidates))) in
  (* precompute all of the typing information *)
  let (candidate_index,has_children,
       left_child,right_child,
       terminal_conflicts,candidate_conflicts,
       jobs) = compute_job_IDs dagger type_array terminals candidates requests
  in
  (* figure out correspondence between jobs and tasks *)
  let task_jobs = List.map task_solutions (fun (task,solutions) ->
      List.map solutions (fun (i,l) ->
          (Hashtbl.find_exn jobs (i,task.task_type), l))) in
  (* routine for performing the dynamic program *)
  let number_jobs = Hashtbl.length jobs in
  let job_likelihoods = Array.create number_jobs 0. in
  let do_jobs productions =
    for j = 0 to (number_jobs-1) do
      let application =
        if has_children.(j)
        then let left_index = left_child.(j)
          and right_index = right_child.(j) in
          let left_likelihood =
            if left_index = no_job_ID then 0. else job_likelihoods.(left_index)
          and right_likelihood =
            if right_index = no_job_ID then 0. else job_likelihoods.(right_index) in
          -.log2 +. left_likelihood +. right_likelihood
        else Float.neg_infinity
      in let terminal =
        let number_library_hits = List.fold_left candidate_index.(j)
            ~init:(if has_children.(j) then 0. else 1.)
            ~f:(fun a h -> if productions.(h) then 1.+.a else a) in
        if number_library_hits > 0.
        then log number_library_hits -. log2 -.
             log (List.fold_left candidate_conflicts.(j) ~init:terminal_conflicts.(j) ~f:(fun a k ->
                 if productions.(k) then a+.1. else a))
        else Float.neg_infinity
      in job_likelihoods.(j) <- lse application terminal
    done
  in
  (* computes log posterior for a given subset of the candidates *)
  let posterior productions =
    let log_prior = -.lambda *. (Array.fold_right productions
                                   ~f:(fun u a -> if u then a+.1. else a) ~init:0.) in
    let likelihood = List.fold_left task_jobs ~f:(fun a t ->
        let ls = List.map t (fun (j,l) -> l +. job_likelihoods.(j)) in
        a +. lse_list ls) ~init:0. in
    log_prior +. likelihood
  in
  (* computes the state successors in the search space *)
  let successors productions =
    let new_productions = 0--(List.length candidates - 1) |>
                          List.filter ~f:(fun p -> not productions.(p))
    in List.map new_productions (fun p ->
        let a = Array.copy productions in
        a.(p) <- true;
        List.iter dependencies.(p) (fun q -> a.(q) <- true);
        a)
  in
  (* performs a greedy search *)
  let rec hill_climb productions =
    do_jobs productions;
    let current_score = posterior productions in
    let new_scores = List.map (successors productions) (fun s -> do_jobs s; (posterior s, s)) in
    if List.length new_scores > 0
    then let (new_score,new_productions) =
      List.fold_left (safe_get_some "compressed tale" @@ List.tl new_scores)
        ~init:(safe_get_some "compressed head" @@ List.hd new_scores)
        ~f:(fun (s1,p1) (s2,p2) -> if s1 > s2 then (s1,p1) else (s2,p2)) in
      if new_score > current_score
      then hill_climb new_productions
      else productions
    else productions
  in
  let t2 = Time.to_float @@ Time.now () in
  Printf.printf "time to prepare for hillclimbing is %f" (t2-.t1);
  print_newline ();
  Printf.printf "about to begin hillclimbing..."; print_newline ();
  let t1 = Time.to_float @@ Time.now () in
  let initial_state = Array.create (List.length candidates) false in
  let productions = hill_climb initial_state in
  let es = List.map ~f:(extract_expression dagger) @@
    terminals @ (List.map ~f:fst @@
                 List.filter ~f:(fun (_,i) -> productions.(i)) @@
                 List.mapi candidates (fun i c -> (c,i))) in
  let new_grammar = fit_grammar_to_tasks smoothing (make_flat_library es)
      dagger type_array requests task_solutions in
  let t2 = Time.to_float @@ Time.now () in
  Printf.printf "time to compute grammar is %f \n " (t2-.t1);
  new_grammar

