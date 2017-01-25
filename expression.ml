open Time_limit
open Type
open Utils

open Obj

open Core.Std


type expression =
  | Terminal of string * tp * unit ref
  | Application of expression * expression

let rec compare_expression (e1 : expression) (e2 : expression) : int =
  match (e1,e2) with
  | (Terminal(n1,_,_),Terminal(n2,_,_)) -> String.compare n1 n2
  | (Terminal(_,_,_),_) -> -1
  | (_,Terminal(_,_,_)) -> 1
  | (Application(l,r),Application(l_,r_)) ->
      let cmp = compare_expression l l_ in
      if cmp = 0 then compare_expression r r_ else cmp

let expression_equal e1 e2 = compare_expression e1 e2 = 0

let is_terminal = function
  | Terminal(_,_,_) -> true
  | _ -> false

let terminal_type = function
  | Terminal(_,t,_) -> t
  | _ -> raise (Failure "terminal_type: not a terminal")

let terminal_thing = function
  | Terminal(_,_,t) -> t
  | _ -> raise (Failure "terminal_thing: not a terminal")

let application_function = function
  | Application(f,_) -> f
  | _ -> raise (Failure "application_function: not an application")

let application_argument = function
  | Application(_,x) -> x
  | _ -> raise (Failure "application_argument: not an application")

let rec run_expression (e:expression) : 'a option =
  match e with
  | Terminal(n,_,_) when n = "bottom" -> None
  | Terminal(_,_,thing) -> Some(!(Obj.magic thing))
  | Application(f,x) ->
    match run_expression f with
    | None -> None
    | Some(left) ->
      (Obj.magic left) (Obj.magic (run_expression x))

let run_expression_for_interval (time : float) (e : expression) : 'a option =
  run_for_interval time (fun _ -> run_expression e)

let lift_trinary k : unit ref = Obj.magic @@ ref (
  fun x -> Some(fun y -> Some(fun z ->
      match (x,y,z) with
      | (Some(a),Some(b),Some(c)) -> Some(k a b c)
      | _ -> None)))


let lift_binary k : unit ref = Obj.magic @@ ref (
  fun x -> Some(fun y ->
      match (x,y) with
      | (Some(a),Some(b)) -> Some(k a b)
      | _ -> None))

let lift_unary k : unit ref = Obj.magic @@ ref (function
  | Some(x) -> (try
                  Some(k x)
                with _ -> None)
  | None -> None)

let lift_predicate p : unit ref = Obj.magic @@ ref (function
  | None -> None
  | Some(x) -> Some(fun y -> Some(fun z ->
    if p x then y else z)))

let lift_reversed_predicate p : unit ref = Obj.magic @@ ref (fun x -> Some(fun y -> Some(function
  | None -> None
  | Some(thing) -> if p thing then x else y)))

let lift_reversed_predicate_2 p : unit ref = Obj.magic @@ ref (fun x -> Some(fun y -> Some(function
  | None -> None
  | Some(x1) -> Some(function
      | None -> None
      | Some(x2) -> if p x1 x2 then x else y))))



let rec infer_context c r =
  match r with
  | Terminal(_,t,_) -> instantiate_type c t
  | Application(f,x) ->
    let (ft,c1) = infer_context c f in
    let (xt,c2) = infer_context c1 x in
    let (rt,c3) = makeTID c2 in
    let c4 = unify c3 ft (make_arrow xt rt) in
    chaseType c4 rt

let infer_type (e : expression) =
  fst (infer_context (1,TypeMap.empty) e)


let rec string_of_expression e =
  match e with
    Terminal(s,_,_) -> s
  | Application(f,x) ->
      "("^(string_of_expression f)^" "^(string_of_expression x)^")"


(* compact representation of expressions sharing many subtrees *)
type expressionNode = ExpressionLeaf of expression
  | ExpressionBranch of int * int;;
type expressionGraph =
    ((int,expressionNode) Hashtbl.t) * ((expressionNode,int) Hashtbl.t) * (int ref);;
let make_expression_graph size : expressionGraph =
  (Hashtbl.create ~hashable:Int.hashable (),Hashtbl.Poly.create (),ref 0)


let insert_expression_node (i2n,n2i,nxt) (n : expressionNode) : int =
  try
    Hashtbl.find_exn n2i n
  with _ ->
    ignore(Hashtbl.add i2n (!nxt) n);
    ignore(Hashtbl.add n2i n (!nxt));
    incr nxt; !nxt - 1

let node_in_graph (_,n2i,_) n =
  Hashtbl.find n2i n

let rec insert_expression g (e : expression) =
  match e with
  | Terminal(_,_,_) ->
      insert_expression_node g (ExpressionLeaf(e))
  | Application(f,x) ->
      insert_expression_node g (ExpressionBranch(insert_expression g f,insert_expression g x))


let rec extract_expression g i =
  let (i2n,_,_) = g in
  match Hashtbl.find i2n i with
  | Some(ExpressionLeaf(e)) -> e
  | Some(ExpressionBranch(f,x)) ->
      Application(extract_expression g f, extract_expression g x)
  | None -> raise (Failure "extract_expression")

let extract_expression_node g i =
  let (i2n,_,_) = g in
  match Hashtbl.find i2n i with
  | Some(n) -> n
  | _ -> raise (Failure "extract_expression_node")

let expression_graph_size (_,_,s) = !s

(* dirty hack *)
let expression_in_graph g e =
  let initial_size = expression_graph_size g in
  ignore(insert_expression g e);
  initial_size = expression_graph_size g

let extract_node (i2n,_,_) i =
  try
    Hashtbl.find_exn i2n i
  with _ -> raise (Failure "extract_node: ID not in graph")

(* returns a set containing all of the expressions reachable from a given list of IDs *)
let reachable_expressions dagger expressions =
  let reachable = ref Int.Set.empty in
  let rec reach i =
    if not (Int.Set.mem !reachable i)
    then begin
      reachable := Int.Set.add !reachable i;
      match extract_node dagger i with
      | ExpressionBranch(f,x) -> reach f; reach x
      | _ -> ()
    end in
  List.iter expressions reach; !reachable

(* pulls the provided IDs out of the old expression graph and into a new one, *)
(* facilitating garbage collection. *)
(* Assumes that the IDs are tagged in a tuple. *)
let gc_expression_graph dagger i =
  let new_dagger = make_expression_graph (List.length i) in
  let new_indices = List.map i
      ~f:(fun j -> (insert_expression new_dagger (extract_expression dagger (fst j)),
                   snd j)) in
  (new_indices,new_dagger)


let is_leaf_ID (g,_,_) i =
  try
    match Hashtbl.find_exn g i with
    | ExpressionLeaf(_) -> true
    | _ -> false
  with Not_found -> raise (Failure "is_leaf_ID: unknown ID")

let rec get_sub_IDs g i =
  let (i2n,_,_) = g in
  match Hashtbl.find i2n i with
  | Some(ExpressionLeaf(_)) -> Int.Set.singleton i
  | Some(ExpressionBranch(f,x)) ->
      Int.Set.add (Int.Set.union (get_sub_IDs g f) (get_sub_IDs g x)) i
  | _ -> raise (Failure "get_sub_IDs")

let is_wildcard dagger i =
  match extract_node dagger i with
  | ExpressionLeaf(Terminal(n,_,_)) when n.[0] = '?' -> true
  | _ -> false

let rec has_wildcards dagger i =
  match extract_node dagger i with
  | ExpressionBranch(f,x) -> has_wildcards dagger f || has_wildcards dagger x
  | ExpressionLeaf(Terminal("?",_,_)) -> true
  | _ -> false

let terminal_wildcard = function
  | Terminal(n,_,_) when n.[0] = '?' -> true
  | _ -> false


(* checks to see if the target could be matched to the template *)
let rec can_match_wildcards dagger template target =
  if template = target || is_wildcard dagger template || is_wildcard dagger target
  then true
  else
  match extract_node dagger template with
  | ExpressionLeaf(_) -> false
  | ExpressionBranch(template_function,template_argument) -> begin
    match extract_node dagger target with
    | ExpressionBranch(target_function,target_argument) ->
      can_match_wildcards dagger template_function target_function &&
      can_match_wildcards dagger template_argument target_argument
    | _ -> false
  end

let rec combine_wildcards dagger i j =
  if i = j then Some(j) else
  match extract_node dagger i with
  | ExpressionLeaf(Terminal("?",_,_)) -> Some(j)
  | ExpressionLeaf(Terminal(_,_,_)) -> (
    match extract_node dagger j with
    | ExpressionLeaf(Terminal("?",_,_)) -> Some(i)
    | _ -> None)
  | ExpressionBranch(l,r) -> (
    match extract_node dagger j with
    | ExpressionBranch(m,n) -> (
        match combine_wildcards dagger m l with
        | None -> None
        | Some(a) -> (
          match combine_wildcards dagger r n with
          | None -> None
          | Some(b) -> Some(insert_expression_node dagger (ExpressionBranch(a,b)))))
    | ExpressionLeaf(Terminal("?",_,_)) -> Some(i)
    | _ -> None)
  | ExpressionLeaf(_) -> raise (Failure "leaf not terminal in wildcards")

let make_wildcard w =
  Terminal("?" ^ string_of_int w,t1, ref ())

let empty_wildcard = Terminal("?", t1, ref ())

let rec maximum_wildcard = function
  | Application(f,x) -> max (maximum_wildcard f) (maximum_wildcard x)
  | Terminal(n,_,_) when n.[0] = '?' -> int_of_string (String.sub n ~pos:1 ~len:(String.length n - 1))
  | _ -> -1

let rec map_wildcard f = function
  | Application(m,n) -> Application(map_wildcard f m,map_wildcard f n)
  | Terminal(n,_,_) when n.[0] = '?' ->
    f @@ int_of_string @@ String.sub n ~pos:1 ~len:(String.length n - 1)
  | e -> e

let substitute_wildcard original w new_W =
  let offset = maximum_wildcard original in
  let new_W = map_wildcard (fun w -> make_wildcard @@ w+offset) new_W in
  map_wildcard (fun q -> if q = w then new_W else make_wildcard q) original

let rec bottomless = function
  | Application(f,x) -> bottomless f && bottomless x
  | Terminal(n,_,_) -> not (n = "bottom")
(*
let rec all_antiunifications dagger i j = function
  | 0 ->
    if i == j then Int.Set.singleton j else Int.Set.empty
  | 1 ->
    if i == j
    then Int.Set.singleton j else Int.Set.empty

 *)

let rec antiunify_expressions dagger i j =
  if i = j
  then extract_expression dagger i
  else
  if is_wildcard dagger i
  then extract_expression dagger j
  else
  if is_wildcard dagger j
  then extract_expression dagger i
  else
  match extract_node dagger i with
  | ExpressionLeaf(_) -> Terminal("?",t1,ref ())
  | ExpressionBranch(l,r) ->
    match extract_node dagger j with
    | ExpressionBranch(f,x) ->
      Application(antiunify_expressions dagger l f,
                 antiunify_expressions dagger r x)
    | ExpressionLeaf(_) -> Terminal("?",t1,ref ())

let rec has_trivial_symmetry dagger i =
  match extract_node dagger i with
  | ExpressionLeaf(_) -> false
  | ExpressionBranch(f,x) ->
    match extract_node dagger f with
    | ExpressionLeaf(Terminal(t,_,_)) when t = "I" -> true
    | ExpressionLeaf(_) -> false
    | ExpressionBranch(a,b) ->
      has_trivial_symmetry dagger a || has_trivial_symmetry dagger b
      || has_trivial_symmetry dagger x

(* used by the procedure below *)
type turn =
  | R | L

(* used by symbolic dimensionality reduction
   returns the types of all of the wildcards, as well as how to get to them. *)
let wild_types dagger request i =
  let rec collect_wild c r = function
    | Terminal(n,_,_) when n.[0] = '?' -> ([([],r)],c)
    | Terminal(_,t,_) ->
      let (t,c) = instantiate_type c t in
      let c = unify c t r in
      ([],c)
    | Application(f,x) ->
      let (argument_type,c) = makeTID c in
      let (function_wild,c) = collect_wild c (make_arrow argument_type r) f in
      let (argument_type,c) = chaseType c argument_type in
      let (argument_wild,c) = collect_wild c argument_type x in
      (List.map ~f:(fun (p,r) -> (L :: p,r)) function_wild @
      List.map ~f:(fun (p,r) -> (R :: p,r)) argument_wild,
      c)
  in fst @@ collect_wild empty_context request @@ extract_expression dagger i


(* performs type inference upon the entire graph of expressions *)
(* returns an array whose ith element is the type of the ith expression *)
let infer_graph_types dagger =
  let type_map = Array.create (expression_graph_size dagger) (TID(0)) in
  let done_map = Array.create (expression_graph_size dagger) false in
  let (i2n,_,_) = dagger in
  let rec infer i =
    if done_map.(i) then type_map.(i)
    else let q = (match Hashtbl.find i2n i with
    | Some(ExpressionLeaf(Terminal(_,t,_))) -> t
    | Some(ExpressionBranch(f,x)) ->
        let ft = infer f in
        let xt = infer x in
        application_type ft xt
    | _ -> raise (Failure "bad id in infer_graph_types")) in
    done_map.(i) <- true; type_map.(i) <- q; q
  in for i = 0 to (expression_graph_size dagger - 1) do
    ignore (try
      infer i
    with _ ->
      raise (Failure ("inference failed for program\n" ^ string_of_expression (extract_expression dagger i))))
  done; type_map


let expression_of_int i = Terminal(Int.to_string i,tint,Obj.magic (ref i));;
let expression_of_float i = Terminal(Float.to_string i,treal,Obj.magic (ref i));;

let rec expression_has_identifier v = function
  | Terminal(b,_,_) -> b = v
  | Application(l,r) ->
    expression_has_identifier v l || expression_has_identifier v r


let test_expression () =
  let t1 = TID(0) in
  let e1 = Terminal("I", t1, Obj.magic (ref (fun x -> x))) in
  let e42 = Terminal("31", t1, Obj.magic (ref 31)) in
  let e2 = Terminal("1", t1, Obj.magic (ref 1)) in
  let e3 = Application(Application(e1,e1),e2) in
  let e4 = Terminal("+", t1, lift_binary (+)) in
  let e5 = Application(Application(e4,e3),e42) in
  let p = Terminal("p",t1,Obj.magic (ref (fun x -> Thread.delay 0.05; x))) in
  let q = Application(p,e5) in
  (match run_expression_for_interval 0.01 q with
    Some(x) -> print_int x
  | None -> print_string "timeout");
  (match run_expression_for_interval 0.1 q with
    Some(x) -> print_int x
  | None -> print_string "timeout");
  Thread.delay 1.;
  (match run_expression_for_interval 0.1 q with
    Some(x) -> print_int x
  | None -> print_string "timeout")
;;


(* test_expression ();;  *)
