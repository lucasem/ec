open Core.Std

open Expression
open Type
open Library
open Utils
open Task


let minimum_occurrences = 2;; (* how many tasks a tree must occur in to make it into the grammar *)

(* doesn't instantiate pairs of fragments *)
let candidate_ground_fragments dagger solutions =
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
  let terminals = List.filter (0--(expression_graph_size dagger - 1)) ~f:(is_leaf_ID dagger) in
  let valid_IDs = reachable_expressions dagger @@ List.concat solutions in
  let ground_pairs = 0--(expression_graph_size dagger - 1) |>
                     List.filter ~f:(compose not @@ has_wildcards dagger) |>
                     List.filter ~f:(fun i -> Int.Set.mem valid_IDs i) |>
                     List.map ~f:(fun x -> (x,x,infer_type @@ extract_expression dagger x)) in
  let q = List.find terminals ~f:(is_wildcard dagger) in
  (* map from fragment to the tasks that use that fragment *)
  let task_map = Hashtbl.Poly.create () in
  List.iteri solutions ~f:(fun t s ->
      List.iter s ~f:(fun i -> get_sub_IDs dagger i |> Int.Set.iter ~f:(fun j ->
          match Hashtbl.find task_map j with
          | Some(old) ->
            Hashtbl.set task_map ~key:j ~data:(Int.Set.add old t)
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
  List.iter terminals ~f:(fun t ->
      if Some(t) <> q then ignore(Hashtbl.add instantiations ~key:t
          ~data:((t,t,terminal_type @@ extract_expression dagger t) ::
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
      in ignore(Hashtbl.add instantiations ~key:i ~data:answer); answer
  in
  let candidates = ref Int.Set.empty in
  let bar = make_progress_bar (Hashtbl.length task_map) in
  Hashtbl.iteri task_map ~f:(fun ~key:i ~data:_ ->
      update_progress_bar bar (bar.current_progress + 1);
      if not (is_leaf_ID dagger i) then
        List.iter (instantiate i) ~f:(fun (j,_,_) ->
              candidates := Int.Set.add !candidates j));
  let can = Int.Set.elements !candidates |> List.filter ~f:(compose not @@ is_leaf_ID dagger) in
  Printf.printf "\nGot %i candidates." (List.length can); print_newline (); can
