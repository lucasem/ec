open Core.Std

open Expression
open Type
open Utils


type library = float * (expression*(float*tp)) list

(* creates a new library with all the production weights equal *)
let make_flat_library primitives =
  (log 0.35, List.map primitives ~f:(fun p -> (p, (0.0,infer_type p))))

(* computes likelihoods of all expressions using a dynamic program *)
(* program_types is a hashmap from ID to type
   requests maps from ID to list of all requested types *)
(* returns a hash map from (ID,requested type) to log likelihood *)
let program_likelihoods (log_application,library) dagger program_types requests =
  let log_terminal = log (1.0 -. exp log_application) in
  (* store map from production ID to log probability *)
  let terminals = List.map library ~f:(fun (e,(l,_)) -> (insert_expression dagger e, l)) in
  (* is ? in the library *)
  let is_library_wild = List.exists terminals ~f:(is_wildcard dagger % fst) in
  (* get all of the different types we can choose from *)
  let terminal_types = List.map library ~f:(fun (_,(l,t)) -> (t,l)) in
  let likelihoods = Hashtbl.Poly.create () in
  let rec likelihood (i : int) (request : tp) =
    if not is_library_wild && is_wildcard dagger i then 0. else
    try Hashtbl.find_exn likelihoods (i,request)
    with _ ->
      let log_probability =
        let terminal_probability =
          let numerator = List.fold_left terminals ~init:Float.neg_infinity ~f:(fun a (j,l) ->
            if (not is_library_wild && can_match_wildcards dagger i j) || i = j then lse a l else a) in
          if is_invalid numerator
          then Float.neg_infinity
          else let z = lse_list (List.map ~f:snd (List.filter terminal_types ~f:(fun (t,_) ->
              can_unify t request))) in
            numerator+.log_terminal-.z
        in match extract_node dagger i with
        | ExpressionBranch(f,x) ->
            let left_request = function_request request in
            let right_request = argument_request request program_types.(f) in
            let application_probability = log_application+. likelihood f left_request
                                          +. likelihood x right_request in
            lse terminal_probability application_probability
        | _ -> terminal_probability
      in
      ignore(Hashtbl.add likelihoods ~key:(i,request) ~data:log_probability);
      log_probability
  in Int.Map.iteri requests ~f:(fun ~key ~data ->
      List.iter data ~f:(fun r -> ignore (likelihood key r))); likelihoods

(* computes likelihood of a possibly ill typed program: returns None if it doesn't type *)
let likelihood_option library request e =
  let dagger = make_expression_graph () in
  let i = insert_expression dagger e in
  let requests = Int.Map.singleton i [request] in
  try
    let types = infer_graph_types dagger in
    let likelihoods = program_likelihoods library dagger types requests in
    Some(Hashtbl.find_exn likelihoods (i,request))
  with _ -> None

(* tracks the number of times that each production has been used, or could have been used *)
type useCounts = {
    mutable application_counts : float; mutable terminal_counts : float;
    use_counts : float array; possible_counts : float array;
  }

(* uses the inside out algorithm to fit the continuous parameters of a grammar
   does so using a dynamic program similar to the one used to compute likelihoods
   smoothing specifies the number of pseudo-counts
   dagger is the expression graph
   program_types is a map from graph ID to type
   likelihood specifies the likelihood of each expression for each requested type
   corpus is a list of ((expression ID,requested type),weight)
   returns the grammar with the parameters fit *)
let fit_grammar smoothing ?application_smoothing (log_application,library) dagger program_types likelihoods corpus =
  let application_smoothing = match application_smoothing with
  | None -> smoothing
  | Some(s) -> s in
  let log_terminal = log (1.0 -. exp log_application) in
  (* get all of the different terminals we can choose from;
     this ordering determines where they go in the use arrays
     "offsets" index into this list
   *)
  let terminal_order = List.mapi library ~f:(fun i (e,(l,t)) ->
    (insert_expression dagger e, (t, l, i))) in
  let number_terminals = List.length terminal_order in
  let counts = { application_counts = log application_smoothing;
                 terminal_counts = log application_smoothing;
                 use_counts = Array.create ~len:number_terminals (log smoothing);
                 possible_counts = Array.create ~len:number_terminals (log smoothing); } in
  let rec uses weight i request =
    if not (is_wildcard dagger i) then begin
      let l = Hashtbl.find_exn likelihoods (i,request) in
      (* if it is in library compute uses if the production was used *)
      let hits = List.filter terminal_order ~f:(fun (j,_) -> can_match_wildcards dagger i j) in
      if not (List.is_empty hits) then begin
        let offsets = List.map hits ~f:(fun (_,(_,l,o)) -> (o,l)) in
        let offset_Z = lse_list @@ List.map offsets ~f:snd in
        let offsets = List.map offsets ~f:(fun (o,l) -> (o,l-.offset_Z)) in
        let others = List.filter terminal_order ~f:(fun (_,(t,_,_)) -> can_unify t request) in
        let other_offsets = List.map others ~f:(fun (_,(_,_,o)) -> o) in
        let z = lse_list (List.map others ~f:(fun (_,(_,l,_)) -> l)) in
        let terminal_likelihood = log_terminal+.offset_Z-.z -.l in
        List.iter offsets ~f:(fun (o,ol) -> let u = counts.use_counts.(o) in
                               counts.use_counts.(o) <- lse u (ol+.terminal_likelihood+.weight));
        List.iter other_offsets ~f:(fun o -> let p = counts.possible_counts.(o) in
                               counts.possible_counts.(o) <- lse p (terminal_likelihood+.weight));
        counts.terminal_counts <- lse counts.terminal_counts (terminal_likelihood+.weight)
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
        counts.application_counts <-
          lse counts.application_counts (application_likelihood+.weight);
        (* get the uses from the right and the left *)
        uses (weight+.application_likelihood) f left_request;
        uses (weight+.application_likelihood) x right_request
    end else begin (* wildcard *)
      counts.application_counts <-
        lse counts.application_counts (log_application+.weight);
      counts.terminal_counts <-
        lse counts.terminal_counts (log_terminal+.weight);
    end
  in
  List.iter corpus ~f:(fun ((i,request),w) -> uses w i request);
  let log_application = counts.application_counts -.
                        lse counts.application_counts counts.terminal_counts in
  let distribution = List.map terminal_order ~f:(fun (i,(_,_,o)) ->
      let p = counts.use_counts.(o) -. counts.possible_counts.(o)
      and e = extract_expression dagger i
      in (e, (p,infer_type e)))
  in (log_application,distribution)

(* built-in primitives *)
let c_bottom = Terminal("bottom",canonical_type t1,Obj.magic @@ ref None)
let c_S = Terminal("S", canonical_type @@
  make_arrow (make_arrow t1 (make_arrow t2 t3))
             (make_arrow (make_arrow t1 t2)
                         (make_arrow t1 t3)),
  Obj.magic (ref (fun f ->
    Some(fun g ->
      Some(fun x ->
        match f with
        | None -> None
        | Some(f) ->
            match f x with
            | None -> None
            | Some(left) ->
                left @@ match g with
                | None -> None
                | Some(g) -> g x)))))
let c_B = Terminal("B", canonical_type @@
  make_arrow (make_arrow t2 t3)
             (make_arrow (make_arrow t1 t2)
                         (make_arrow t1 t3)),
  Obj.magic (ref (fun f ->
    Some(fun g ->
      Some(fun x ->
        match f with
        | None -> None
        | Some(f) ->
            f @@ match g with
            | None -> None
            | Some(g) -> g x)))))
let c_C = Terminal("C",  canonical_type @@
  make_arrow (make_arrow t1 (make_arrow t2 t3))
             (make_arrow t2 (make_arrow t1 t3)),
  Obj.magic (ref (fun f ->
    Some(fun g ->
      Some(fun x ->
        match f with
        | None -> None
        | Some(f) ->
            match f x with
            | None -> None
            | Some(left) -> left g)))))
let c_K = Terminal("K", canonical_type @@
  make_arrow t1 (make_arrow t2 t1),
  Obj.magic (ref (fun x -> Some(fun _ -> x))))
let c_F = Terminal("F", canonical_type @@
  make_arrow t1 (make_arrow t2 t2),
  Obj.magic (ref (fun _ -> Some(fun x -> x))))
let c_I = Terminal("I", canonical_type @@
  make_arrow t1 t1,
  Obj.magic (ref (fun x -> x)))

let expression_of_string_with_combs s combs =
  let terminals = ref (List.map combs ~f:(fun e -> (string_of_expression e,e))) in
  let i = ref 0 in
  let rec read () =
    if !i < String.length s
    then (if s.[!i] = '('
          then (incr i;
                let f = read () in
                incr i;
                let x = read () in
                incr i;
                Application(f, x))
          else (let j = ref (!i) in
                while !j < String.length s && s.[!j] <> ')' && s.[!j] <> ' ' do
                  incr j
                done;
                let name = String.sub s ~pos:!i ~len:(!j - !i) in
                i := !j;
                if name.[0] = '?'
                then Terminal(name,t1,ref ())
                else try
                  List.Assoc.find_exn !terminals name
                with _ -> raise (Failure ("not in terminals: "^name))))
    else raise (Failure ("expression_of_string: "^s))
  in read ()
