open Core.Std

open Task
open Expression
open Type
open Utils

open Obj


(* specializes the list typed to only work on phonemes *)
let phonetic_list = true

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
  let terminal_types =
    List.map library ~f:(fun (_,(l,t)) -> (t,l)) in
  let likelihoods = Hashtbl.Poly.create () in
  let rec likelihood (i : int) (request : tp) =
    if not is_library_wild && is_wildcard dagger i then 0. else
    try
      Hashtbl.find_exn likelihoods (i,request)
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
      ignore(Hashtbl.add likelihoods (i,request) log_probability);
      log_probability
  in Int.Map.iter requests (fun ~key ~data ->
      List.iter data ~f:(fun r -> ignore (likelihood key r))); likelihoods

(* computes likelihood of a possibly ill typed program: returns None if it doesn't type *)
let likelihood_option library request e =
  let dagger = make_expression_graph 100 in
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
  let terminal_order =
    List.mapi library ~f:(fun i (e,(l,t)) -> (insert_expression dagger e,
                                              (t, l, i))) in
  let number_terminals = List.length terminal_order in
  let counts = { application_counts = log application_smoothing;
                 terminal_counts = log application_smoothing;
                 use_counts = Array.create number_terminals (log smoothing);
                 possible_counts = Array.create number_terminals (log smoothing); } in
  let rec uses weight i request =
    if not (is_wildcard dagger i) then begin
      let l = Hashtbl.find_exn likelihoods (i,request) in
      (* if it is in library compute uses if the production was used *)
      let hits = List.filter terminal_order ~f:(fun (j,_) -> can_match_wildcards dagger i j) in
      if not (List.is_empty hits) then begin
        let offsets = List.map hits ~f:(fun (_,(_,l,o)) -> (o,l)) in
        let offset_Z = lse_list @@ List.map offsets snd in
        let offsets = List.map offsets ~f:(fun (o,l) -> (o,l-.offset_Z)) in
        let others = List.filter terminal_order (fun (_,(t,_,_)) -> can_unify t request) in
        let other_offsets = List.map others (fun (_,(_,_,o)) -> o) in
        let z = lse_list (List.map others (fun (_,(_,l,_)) -> l)) in
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
  let distribution = List.map terminal_order (fun (i,(_,_,o)) ->
      let p = counts.use_counts.(o) -. counts.possible_counts.(o)
      and e = extract_expression dagger i
      in (e, (p,infer_type e)))
  in (log_application,distribution)

(* wrapper over fit_grammar that does not assume a corpus structure *)
let fit_grammar_to_tasks smoothing grammar dagger program_types requests task_solutions =
  let likelihoods = program_likelihoods grammar dagger program_types requests in
  let task_posteriors = List.map task_solutions (fun (t,s) ->
    List.map s (fun (i,l) ->
          ((i,t.task_type),l+.Hashtbl.find_exn likelihoods (i,t.task_type)))) in
  let zs = List.map task_posteriors (fun p ->
    lse_list @@ List.map p snd) in
  let task_posteriors = List.map2_exn task_posteriors zs ~f:(fun p z ->
    List.map p ~f:(fun (i,l) -> (i,l-.z))) in
  let corpus = merge_a_list task_posteriors lse in
  fit_grammar smoothing grammar dagger program_types likelihoods corpus

(* various built-in primitives *)
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
                                 | Some(g) -> g x)))));;
let c_B = Terminal("B", canonical_type @@
                  make_arrow (make_arrow t2 t3)
                             (make_arrow (make_arrow t1 t2)
                                         (make_arrow t1 t3)),
                  Obj.magic (ref (fun f  ->
                       Some(fun g ->
                           Some(fun x ->
                             match f with
                             | None -> None
                             | Some(f) ->
                               f @@ match g with
                               | None -> None
                               | Some(g) -> g x)))));;
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
                               | Some(left) ->
                                 left g)))));;
let c_K = Terminal("K", canonical_type @@
                   make_arrow t1 (make_arrow t2 t1),
                   Obj.magic (ref (fun x -> Some(fun _ -> x))));;
let c_F = Terminal("F", canonical_type @@
                   make_arrow t1 (make_arrow t2 t2),
                   Obj.magic (ref (fun _ -> Some(fun x -> x))));;
let c_I = Terminal("I", canonical_type @@
                   make_arrow t1 t1,
                   Obj.magic (ref (fun x -> x)));;
let combinatory_library =
  make_flat_library [c_S;c_B;c_C;c_K;c_F;c_I]

let c_bottom = Terminal("bottom",canonical_type t1,Obj.magic @@ ref None)

let c_one = Terminal("1",tint,Obj.magic (ref 1));;
let c_zero = Terminal("0",tint,Obj.magic (ref 0));;
let c_numbers = List.map (0--9) expression_of_int;;
let c_plus = Terminal("+",
                     make_arrow tint (make_arrow tint tint),
                     lift_binary (+));;
let c_times = Terminal("*",
                     make_arrow tint (make_arrow tint tint),
                     lift_binary (fun x y ->x*y ));;
let polynomial_library =
  make_flat_library @@ [c_S;c_B;c_C;c_I;c_plus;c_times;c_zero;c_one;](*  @ c_numbers *);;

let c_reals = List.map (0--9) (expression_of_float % Float.of_int);;
let c_sin = Terminal("sin",
                    make_arrow treal treal,
                    lift_unary sin);;
let c_cos = Terminal("cos",
                    make_arrow treal treal,
                    lift_unary cos);;
let c_plus_dot = Terminal("+.",
                     make_arrow treal (make_arrow treal treal),
                     lift_binary (+.));;
let c_times_dot = Terminal("*.",
                     make_arrow treal (make_arrow treal treal),
                     lift_binary ( *. ));;
let fourier_library =
  make_flat_library @@ [c_S;c_B;c_C;c_I;c_plus_dot;c_times_dot;c_sin;c_cos;] @ c_reals;;


let c_inner_product = Terminal("dot",
                              (TCon("list",[treal])) @>
                              (TCon("list",[treal])) @>
                              treal,
                              lift_binary @@ fun x y ->
                              (try
                                 List.fold2_exn ~init:0. ~f:(fun a b c -> a+.b*.c) x y
                               with _ -> 0.0));;


let list_type = if phonetic_list then make_ground "phone" else t1;;
let c_null = Terminal("null",canonical_type (TCon("list",[list_type])),Obj.magic (ref []));;
let c_cons = Terminal("cons",
                      canonical_type @@ make_arrow list_type @@
                      make_arrow (TCon("list",[list_type])) @@ (TCon("list",[list_type])),
                      lift_binary (fun x y -> x::y));;
let c_rcons = Terminal("rcons",
                      canonical_type @@ make_arrow list_type @@
                      make_arrow (TCon("list",[list_type])) @@ (TCon("list",[list_type])),
                      lift_binary (fun x y -> y @ [x]));;
let c_append1 = Terminal("@1",
                      canonical_type @@ (TCon("list",[list_type])) @> list_type @>
                                        (TCon("list",[list_type])),
                      lift_binary (fun y x -> y @ [x]));;
let c_append = Terminal("@",
                        canonical_type @@ make_arrow (TCon("list",[list_type])) @@
                        make_arrow (TCon("list",[list_type])) @@ (TCon("list",[list_type])),
                        lift_binary (@));;
let c_last_one = Terminal("last-one",
                          canonical_type @@ make_arrow (TCon("list",[list_type])) list_type,
                          lift_unary last_one);;
let c_cdr = Terminal("cdr",
                    canonical_type @@ make_arrow (TCon("list",[list_type])) (TCon("list",[list_type])),
                    lift_unary List.tl_exn);;
let c_car = Terminal("car",
                    canonical_type @@ make_arrow (TCon("list",[list_type])) list_type,
                    lift_unary List.hd_exn);;
let c_exists = Terminal("exists",
                        canonical_type @@
                        t4 @> t4 @> (t2 @> t2 @> list_type @> t2) @> (TCon("list",[list_type])) @> t4,
                        (* WARNING: p is handled incorrectly. Only good for type checking and likelihoods. *)
                        lift_reversed_predicate_2 (fun p l -> List.exists ~f:p l));;

let math_list_library =
    make_flat_library @@ [c_S;c_B;c_C;c_I;c_plus_dot;c_times_dot;c_sin;
                          c_cos;c_null;c_cons;c_inner_product] @ c_reals;;

let string_of_library (log_application,bindings) =
  String.concat ~sep:"\n"
    ((Float.to_string (exp log_application))::
     (List.map bindings (fun (e,(w,t)) ->
          Printf.sprintf "\t %f \t %s : %s " w (string_of_expression e) (string_of_type t))));;

let all_terminals = ref (List.map ([c_K;c_S;c_B;c_C;c_I;c_bottom;
                          c_sin;c_cos;c_times_dot;c_plus_dot;c_plus;c_times;c_inner_product;
                          c_null;c_append;c_rcons;c_cons;c_append1;c_last_one;c_exists;c_car;c_cdr;]
                                   @ c_numbers @ c_reals)
                           (fun e -> (string_of_expression e,e)));;
let register_terminal t =
  all_terminals := (string_of_expression t,t) :: !all_terminals;;
let register_terminals t = List.iter t register_terminal;;

(* replaces all of the "unit references" with actual unit references. necessary for marshaling *)
let scrub_graph (i2n,n2i,_) =
  let substitution = ref [] in
  Hashtbl.iter i2n (fun ~key:i ~data:n ->
    match n with
    | ExpressionLeaf(Terminal(name,t,_)) when name.[0] <> '?' ->
      let clean = ExpressionLeaf(Terminal(name,t,ref ()))
      and dirty = n in
      substitution := (i,clean,dirty) :: !substitution
    | _ -> ());
  List.iter !substitution (fun (i,c,d) ->
    Hashtbl.replace i2n i c;
    Hashtbl.remove n2i d;
    ignore(Hashtbl.add n2i c i))

(* undoes the above operation *)
let dirty_graph (i2n,n2i,_) =
  let substitution = ref [] in
  Hashtbl.iter i2n (fun ~key:i ~data:n ->
    match n with
    | ExpressionLeaf(Terminal(name,_,_)) when name.[0] <> '?' ->
      let clean = n
      and dirty = ExpressionLeaf(List.Assoc.find_exn !all_terminals name) in
      substitution := (i,clean,dirty) :: !substitution
    | _ -> ());
  List.iter !substitution (fun (i,c,d) ->
    Hashtbl.replace i2n i d;
    Hashtbl.remove n2i c;
    ignore(Hashtbl.add n2i d i))


(* parses an expression. has to be in library because needs definitions of terminals *)
let expression_of_string s =
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
                let name = String.sub s !i (!j - !i) in
                i := !j;
                if name.[0] = '?'
                then Terminal(name,t1,ref ())
                else try
                  List.Assoc.find_exn !all_terminals name
                with _ -> raise (Failure ("not in all_terminals: "^name))))
    else raise (Failure ("expression_of_string: "^s))
  in read ()

let load_library f =
  let i = open_in f in
  let log_application = Float.of_string @@ input_line i in
  let productions = ref [] in
  try
    while true do
      let l = String.strip @@ input_line i in
      let weight_index = safe_get_some "load_library: None" @@ String.index l ' ' in
      let w = Float.of_string @@ String.sub l 0 weight_index in
      let e = expression_of_string @@ String.strip @@
        String.sub l weight_index (String.length l - weight_index) in
      productions := (e,w) :: !productions
    done; (0.,[])
  with End_of_file ->
    (log log_application, List.map !productions (fun (e,w) -> (e, (w,infer_type e))))

let rec remove_lambda v = function
  | Terminal(b,_,_) when b = v -> c_I
  | Application(f,Terminal(b,_,_)) when b = v ->
    if expression_has_identifier v f
    then Application(Application(c_S,remove_lambda v f),c_I)
    else f
  | Application(Terminal(b,_,_),f) when b = v ->
    if expression_has_identifier v f
    then Application(Application(c_S,c_I),remove_lambda v f)
    else Application(Application(c_S,c_I),f)
  | Application(f,x) ->
    begin
      match (expression_has_identifier v f,
            expression_has_identifier v x) with
      | (true,true) ->
        Application(Application(c_S,remove_lambda v f),
                   remove_lambda v x)
      | (false,true) ->
        Application(Application(c_B,f),remove_lambda v x)
      | (true,false) ->
        Application(Application(c_C,remove_lambda v f),x)
      | (false, false) ->
        Application(c_K,Application(f,x))
    end
  (* only possibility is terminal not matching v *)
  | t -> Application(c_K,t)


let test_library () =
  List.map ["I";"((C +) 1)";"(K (+ (0 S)))"]
    (fun s -> print_string (string_of_expression @@ expression_of_string s); print_newline ());;



(* test_library ();; *)



let debug_type () =
  let problems = ["(C C)";"((C C) S)";"(C ((C C) S))";
                  "((C ((C C) S)) I)";"(((C ((C C) S)) I) C)";]
                 |> List.map ~f:expression_of_string in
  List.iter problems ~f:(fun e ->
      Printf.printf "%s : \t%s\n" (string_of_expression e)
        (string_of_type @@ infer_type e));
  let left_type = expression_of_string "((C ((C C) S)) I)" |> infer_type in
  let requested_type = argument_request (treal @> treal) left_type in
  Printf.printf "request for C: %s\n" (string_of_type requested_type);
  Printf.printf "can see be used: %B\n" (can_unify requested_type (terminal_type c_C));
  let (left_type,c) = infer_context (1,TypeMap.empty) (expression_of_string "((C ((C C) S)) I)") in
  let (rt,c2) = makeTID c in
  let c3 = unify c2 left_type (make_arrow rt (make_arrow treal treal)) in
  let (requested_type,c4) = chaseType c3 rt in
  Printf.printf "request for C: %s\n" (string_of_type requested_type);
  Printf.printf "can see be used: %B\n" (can_unify requested_type (terminal_type c_C));

;;
(* debug_type ();; *)
