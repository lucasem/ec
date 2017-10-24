open Core.Std

open Type
open Library
open Task
open Expression
open Utils


let frontier_requests frontiers =
  List.fold_left frontiers
    ~init:Int.Map.empty ~f:(fun requests (requested_type,frontier) ->
        List.fold_left frontier ~init:requests
          ~f:(fun (a : (tp list) Int.Map.t) (i,_dt) ->
              match Int.Map.find a i with
              | Some(old) ->
                if List.mem old requested_type then a
                else Int.Map.add a ~key:i ~data:(requested_type::old)
              | None -> Int.Map.add a ~key:i ~data:[requested_type]))

(* spit out something that is similar to the posterior; *)
let bic_posterior_surrogate ?print:(print = false) lambda dagger grammar task_solutions =
  let likelihood = List.fold_left task_solutions ~init:0. ~f:(fun l (t,f) ->
      if List.length f > 0
      then l +. lse_list (List.map f ~f:(fun (i,s,_) ->
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
