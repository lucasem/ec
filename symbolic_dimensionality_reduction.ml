open Core.Std

open Expression
open Type
open Library
open Utils
open Task
open Enumerate
open Frontier

let make_decoder dagger i j =
  (* arbitrary cutoffs *)
  let max_wild = 3 in
  let min_tame = 3 in
  let d = antiunify_expressions dagger i j in
  let rec count_terminals = function
    | Terminal(n,_,_) when n = "?" -> (1,0)
    | Application(f,x) ->
      let (a,b) = count_terminals f
      and (p,q) = count_terminals x in
      (a+p,b+q)
    | _ -> (0,1)
  in
  let rec reduce_wild = function
    | Application(f,x) ->
      let f = reduce_wild f
      and x = reduce_wild x in
      if fst (count_terminals f) > 0 && fst (count_terminals x) > 0
      then Terminal("?",t1,ref ())
      else Application(f,x)
    | e -> e
  in
  let d = if max_wild = 1 then reduce_wild d else d in
  let (number_wild, number_tame) = count_terminals d in
  if number_wild <= max_wild && number_tame >= min_tame
  then Some(insert_expression dagger d)
  else None

let potential_decoders dagger solutions =
  (* consider each pair of frontiers *)
  let potentials = solutions |> map_list (function
    | [] -> Int.Set.empty
    | [_] -> Int.Set.empty
    | (t :: rest) -> List.concat rest |> List.fold_left ~f:(fun a other ->
      List.fold_left t ~init:a ~f:(fun b this ->
            match make_decoder dagger this other with
            | None -> b
            | Some(d) -> Int.Set.add b d)) ~init:Int.Set.empty) in
  (* coalesced potentials *)
  let potentials = List.fold_left potentials ~f:Int.Set.union ~init:Int.Set.empty in
  Printf.printf "Computed %i pairwise decoders" @@ Int.Set.length potentials;
  print_newline ();
  Int.Set.iter potentials ~f:(fun p ->
    print_endline @@ string_of_expression @@ extract_expression dagger p);
  (* only keep those that can be used in all of the tasks *)
  let candidates = Int.Set.filter potentials (fun c ->
    List.for_all solutions (List.exists ~f:(can_match_wildcards dagger c))) in
  Printf.printf "Computed %i candidate decoders" @@ Int.Set.length candidates;
  print_newline ();
  Int.Set.to_list candidates

let decode_likelihood grammar dagger paths i =
  let rec score_path r e = function
    | L :: p -> begin
      match e with
      | Application(l,_) -> score_path r l p
      | Terminal(q,_,_) when q.[0] = '?' -> 0.0
      | _ -> Float.neg_infinity
    end
    | R :: p -> begin
      match e with
      | Application(_,x) -> score_path r x p
      | Terminal(q,_,_) when q.[0] = '?' -> 0.0
      | _ -> Float.neg_infinity
    end
    | [] -> match likelihood_option grammar r e with
      | None -> Float.neg_infinity
      | Some(l) -> l
  in
  let e = extract_expression dagger i in
  List.fold_left paths ~f:(fun a (path,r) -> a+.score_path r e path) ~init:0.

let decoder_posterior dagger grammar request solutions decoder =
  let modified_grammar = (fst grammar,(empty_wildcard, (0.,t1)) :: snd grammar) in
  let manifold = extract_expression dagger decoder in
  Printf.printf "Posterior: %s" @@ string_of_expression manifold;
  print_newline ();
  let prior =
    safe_get_some "decoder_posterior: prior" @@
    likelihood_option modified_grammar request manifold in
  print_float prior; print_newline ();
  let paths = wild_types dagger request decoder in
  print_int @@ List.length paths; print_newline ();
  let likelihood = List.fold_left solutions ~init:0.0 ~f:(fun l frontier ->
      let term = List.fold_left frontier ~init:Float.neg_infinity ~f:(fun a i ->
          if can_match_wildcards dagger decoder i
          then lse a (decode_likelihood grammar dagger paths i)
          else a) in
      l +. term) in
  prior+.likelihood

let best_decoder dagger grammar request solutions =
  let decoders = potential_decoders dagger solutions in
  let decoder_scores =
    parallel_map decoders ~f:(decoder_posterior dagger grammar request solutions) in
  fst @@ List.hd_exn @@ List.sort ~cmp:(fun (_,p) (_,q) -> compare q p)
  @@ List.zip_exn decoders decoder_scores

let reduce_symbolically base_grammar posterior_grammar frontier_size keep_size tasks =
  let (dagger, fs) = make_frontiers frontier_size keep_size posterior_grammar tasks in
  let task_solutions = List.map fs ~f:(List.map ~f:(fun (i,_,_) -> i)) in
  let request = (List.hd_exn tasks).task_type in
  let d = time_it "Found decoder"
      (fun () -> best_decoder dagger base_grammar request task_solutions) in
  extract_expression dagger d
