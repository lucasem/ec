open Core.Std

open Expression
open Type
open Utils

type task_objective =
  | LogLikelihood of (expression -> float)
  | Seed of expression

type task =
    { name : string; task_type : tp;
    score : task_objective;
    proposal : ((expression -> float -> float) * (expression*float) list) option; }

let task_likelihood t =
  match t.score with
  | LogLikelihood(f) -> f
  | _ -> raise (Failure "task_likelihood: seed")

let modify_grammar grammar t =
  let propose = fst @@ safe_get_some "modify_grammar propose" t.proposal
  and extra = List.map (snd @@ safe_get_some "modify_grammar extra" t.proposal)
      ~f:(fun (e,w) -> (e,(w,infer_type e))) in
  let special_weights =
    extra @
    (List.map (snd grammar) ~f:(fun (e, (w,ty)) -> (e,(propose e w,ty))) |>
     List.filter ~f:(not % List.Assoc.mem extra ~equal:expression_equal % fst))
  in
  (fst grammar,special_weights)

let score_programs dagger frontiers tasks =
  parallel_map tasks ~f:(fun task ->
      let ll = match task.score with
      | Seed(_) -> raise (Failure "score_programs: task has seed")
      | LogLikelihood(ll) -> ll in
      List.filter_map (List.Assoc.find_exn frontiers task.task_type)
        ~f:(fun (i,dt) ->
            let e = extract_expression dagger i in
            let l = ll e in
            if is_valid l then Some((i,l,dt)) else None))

let best_programs dagger task_solutions =
  List.map task_solutions ~f:(fun (t,s) ->
    if List.length s > 0 then
      let (e,p,dt) = List.fold_left (List.tl_exn s) ~init:(List.hd_exn s)
        ~f:(fun (f,p,dta) (g,q,dtb) -> if p > q then (f,p,dta) else (g,q,dtb)) in
      let e = extract_expression dagger e in
      (t.name, Some((p, string_of_expression e, e, dt)))
    else (t.name, None))
