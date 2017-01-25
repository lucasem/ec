open Core.Std

(* Performs an iteration of exploration and compression using lower bound refinement *)

open Expression
open Type
open Task
open Library
open Enumerate
open Utils
open Compress
open Frontier
open Em



let lower_bound_refinement_iteration
    prefix lambda smoothing frontier_size
    tasks grammar =
  let (frontiers,dagger) = enumerate_frontiers_for_tasks grammar frontier_size tasks in
  (* if a primitive is never used in the solution, it might not end up in the library;
   * this ensures that this won't happen *)
  List.iter (snd grammar) ~f:(fun (e,_) ->
    if is_terminal e
    then ignore(insert_expression dagger e));
  print_string "Scoring programs...";
  print_newline ();
  let program_scores = score_programs dagger frontiers tasks in
  (* display the hit rate *)
  let number_hit = List.length (List.filter ~f:(fun scores ->
      List.exists ~f:(fun (_,s) -> s > log (0.999)) scores
    ) program_scores) in
  let number_of_partial = List.length (List.filter program_scores (fun scores ->
      List.length scores > 0)) in
  Printf.printf "Hit %i / %i \n" number_hit (List.length tasks);
  Printf.printf "Partial credit %i / %i" (number_of_partial-number_hit) (List.length tasks);
  print_newline ();
  let type_array = infer_graph_types dagger in
  let requests = frontier_requests frontiers in
  let task_solutions = List.filter ~f:(fun (_,s) -> List.length s > 0)
      (List.zip_exn tasks @@ List.map program_scores (List.filter ~f:(fun (_,s) -> is_valid s)))
  in
  let g = compress lambda smoothing dagger type_array requests task_solutions in
  (* save the grammar *)
  Out_channel.write_all (prefix^"_grammar") ~data:(string_of_library g);
  (* save the best programs *)
  let task_solutions = List.zip_exn tasks program_scores |> List.map ~f:(fun (t,solutions) ->
      (t, List.map solutions (fun (i,s) ->
           (i,s+. (get_some @@ likelihood_option g t.task_type (extract_expression dagger i)))))) in
  save_best_programs (prefix^"_programs") dagger task_solutions;
  ignore(bic_posterior_surrogate lambda dagger g task_solutions);
  g
