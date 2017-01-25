open Core.Std

open Expression
open Type
open Utils
open Library
open Task
open Compress
open Partial_evaluation


(* generation of bottom-up templates *)
let get_templates e t =
  (* maximum number of times we can make up a value for a wildcard *)
  let maximum_barriers = 20 in
  (* uses partial evaluation to get templates *)
  let rec collect_templates barriers target template =
    if barriers > maximum_barriers then [] else
    match reduce_expression template with
    | Stepped(new_template) ->
      (target,new_template) :: collect_templates barriers target new_template
    | NormalForm -> []
    | Blocked(w,instantiations) ->
      let new_targets = List.map instantiations ~f:(substitute_wildcard target w) in
      let new_templates = List.map instantiations ~f:(substitute_wildcard template w) in
      List.map2_exn ~f:(collect_templates @@ barriers+1) new_targets new_templates |> List.concat
  in
  let arity = get_arity t in
  List.map (0--arity) ~f:(fun number_arguments ->
    let arguments = List.map (1--number_arguments) ~f:(fun a -> make_wildcard @@ a+1) in
    let target = List.fold_left arguments ~init:e ~f:(fun f x -> Application(f,x)) in
    collect_templates 0 target target)
  |> List.concat |> List.filter ~f:(bottomless % snd)

let match_template dagger template i =
  let bindings = ref [] in
  let rec m t j =
    match t with
    | Terminal("?",_,_) -> true
    | Terminal(name,_,_) when name.[0] = '?' -> begin
        let name_ID = int_of_string @@ String.sub name ~pos:1 ~len:(String.length name - 1) in
        try
          let k = List.Assoc.find_exn !bindings name_ID in
          match combine_wildcards dagger j k with
          | None -> false
          | Some(c) -> begin
            bindings := List.map !bindings ~f:(fun (i,l) ->
                (i, if i = name_ID then c else l));
            true
          end
        with _ -> (bindings := (name_ID,j) :: !bindings; true)
      end
    | Application(f,x) -> begin
        try
          match extract_node dagger j with
          | ExpressionLeaf(_) -> false
          | ExpressionBranch(f_,x_) -> m f f_ && m x x_
        with _ -> raise (Failure "match_template, ID not in graph")
      end
    | Terminal(name,_,_) -> begin
        try
          match extract_node dagger j with
          | ExpressionLeaf(Terminal(name_,_,_)) -> name = name_
          | _ -> false
        with _ -> raise (Failure "match_template, ID not in graph (2)")
      end
  in if m template i
     then Some(!bindings)
     else None

let apply_template template bindings =
  let rec apply t =
    match t with
    | Terminal(name,_,_) when name.[0] = '?' && String.length name > 1 -> begin
        let name_ID = int_of_string @@ String.sub name ~pos:1 ~len:(String.length name - 1) in
        try
          List.Assoc.find_exn bindings name_ID
        with _ ->
          Terminal("?",t1,ref ()) (* raise (Failure "apply_template: unbound") *)
      end
    | Terminal(_,_,_) -> t
    | Application(f,x) ->
      Application(apply f,apply x)
  in apply template

let backward_children dagger grammar request rewrites j =
  let (i2n,_,_) = dagger in
  let rec children i =
    let head_rewrites = List.fold_left rewrites ~init:[] ~f:(fun a (template,handler) ->
        match match_template dagger template i with
        | None -> a
        | Some(bindings) ->
          (handler @@ List.map bindings ~f:(fun (b,i) -> (b,extract_expression dagger i)))::a) in
    match Hashtbl.find_exn i2n i with
    | ExpressionLeaf(_) -> head_rewrites
    | ExpressionBranch(f,x) ->
      let left = extract_expression dagger f in
      let right = extract_expression dagger x in
      let left_children = List.map (children f) ~f:(fun l ->
          Application(l,right)) in
      let right_children = List.map (children x) ~f:(fun r ->
          Application(left,r)) in
      head_rewrites @ left_children @ right_children
  in List.map (children j) ~f:(fun e -> (likelihood_option grammar request e, e)) |>
     List.filter ~f:(compose is_some fst) |>
     List.map ~f:(fun (l,e) -> (insert_expression dagger e, get_some l))


module Frontier_node_cmp = Comparable.Make(struct
    type t = int*float [@@deriving sexp]
    let compare (i1,l1) (i2,l2) =
      if l1 = l2 then i1-i2 else (if l1 > l2 then 1 else -1)
  end)

let backward_enumerate dagger grammar rewrites size keep request i =
  let bar = make_progress_bar size in
  let new_dagger = make_expression_graph keep in
  let e = extract_expression dagger i in
  let i = insert_expression new_dagger e in
  let i_likelihood = get_some @@ likelihood_option grammar request e in
  let closed = ref @@ Set.singleton ~comparator:Frontier_node_cmp.comparator (i,i_likelihood) in
  let opened = ref @@ Set.singleton ~comparator:Frontier_node_cmp.comparator (i,i_likelihood) in
  let rec search () =
    if Set.length !closed > size || Set.length !opened = 0
    then List.rev @@ Set.to_list !closed
    else let next = Set.max_elt_exn !opened in
      opened := Set.remove !opened next;
      backward_children new_dagger grammar request rewrites (fst next) |>
      List.iter ~f:(fun c ->
                  if not (Set.mem !closed c)
                  then begin
                    closed := Set.add !closed c;
                    opened := Set.add !opened c
                  end);
      (if !number_of_cores = 1 then update_progress_bar bar (Set.length !closed));
      search ()
  in List.filter (search ()) ~f:(not % (has_trivial_symmetry new_dagger) % fst) |>
     Fn.flip List.take keep |>
     List.map ~f:(fun (j,l) -> (insert_expression dagger @@ extract_expression new_dagger j,l))


