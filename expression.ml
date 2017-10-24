open Core.Std

open Obj

open Type
open Utils


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

let rec run_expression (e:expression) : 'a option =
  match e with
  | Terminal(n,_,_) when n = "bottom" -> None
  | Terminal(_,_,thing) -> Some(!(Obj.magic thing))
  | Application(f,x) ->
    match run_expression f with
    | None -> None
    | Some(left) -> (Obj.magic left) (Obj.magic (run_expression x))

let run_expression_for_interval (time : float) (e : expression) : 'a option =
  Time_limit.run_for_interval time (fun _ -> run_expression e)

let lift_quadinary k : unit ref = Obj.magic @@ ref (
  fun x -> Some(fun y -> Some(fun z -> Some(fun w ->
    match (x,y,z,w) with
      | (Some(a),Some(b),Some(c),Some(d)) -> Some(k a b c d)
      | _ -> None
  ))))

let lift_trinary k : unit ref = Obj.magic @@ ref (
  fun x -> Some(fun y -> Some(fun z ->
    match (x,y,z) with
      | (Some(a),Some(b),Some(c)) -> Some(k a b c)
      | _ -> None
  )))

let lift_binary k : unit ref = Obj.magic @@ ref (
  fun x -> Some(fun y ->
    match (x,y) with
      | (Some(a),Some(b)) -> Some(k a b)
      | _ -> None
  ))

let lift_unary k : unit ref = Obj.magic @@ ref (function
  | Some(x) -> (try Some(k x)
                with _ -> None)
  | None -> None)

let lift_predicate p : unit ref = Obj.magic @@ ref (function
  | None -> None
  | Some(x) -> Some(fun y -> Some(fun z ->
    if p x then y else z)))

let rec infer_context c r =
  match r with
  | Terminal(_,t,_) -> instantiate_type c t
  | Application(f,x) ->
    let (ft,c1) = infer_context c f in
    let (xt,c2) = infer_context c1 x in
    let (rt,c3) = makeTID c2 in
    let c4 = unify c3 ft (make_arrow xt rt) in
    chaseType c4 rt

let infer_type (e : expression) = fst (infer_context (1,TypeMap.empty) e)

let rec string_of_expression e =
  match e with
    Terminal(s,_,_) -> s
  | Application(f,x) ->
      "("^(string_of_expression f)^" "^(string_of_expression x)^")"

(* compact representation of expressions sharing many subtrees *)
type expressionNode = ExpressionLeaf of expression
  | ExpressionBranch of int * int
type expressionGraph =
    ((int,expressionNode) Hashtbl.t) * ((expressionNode,int) Hashtbl.t) * (int ref)
let make_expression_graph () : expressionGraph =
  (Hashtbl.create ~hashable:Int.hashable (),Hashtbl.Poly.create (),ref 0)

let insert_expression_node (i2n,n2i,nxt) (n : expressionNode) : int =
  try Hashtbl.find_exn n2i n
  with _ ->
    ignore(Hashtbl.add i2n ~key:(!nxt) ~data:n);
    ignore(Hashtbl.add n2i ~key:n ~data:(!nxt));
    incr nxt; !nxt - 1

let node_in_graph (_,n2i,_) n = Hashtbl.find n2i n

let rec insert_expression g (e : expression) =
  match e with
  | Terminal(_,_,_) -> insert_expression_node g (ExpressionLeaf(e))
  | Application(f,x) -> insert_expression_node g (ExpressionBranch(insert_expression g f,insert_expression g x))

let rec extract_expression g i =
  let (i2n,_,_) = g in
  match Hashtbl.find i2n i with
  | Some(ExpressionLeaf(e)) -> e
  | Some(ExpressionBranch(f,x)) ->
      Application(extract_expression g f, extract_expression g x)
  | None -> raise (Failure "extract_expression")

let expression_graph_size (_,_,s) = !s

let extract_node (i2n,_,_) i =
  try Hashtbl.find_exn i2n i
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
  List.iter expressions ~f:reach; !reachable

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

(* performs type inference upon the entire graph of expressions *)
(* returns an array whose ith element is the type of the ith expression *)
let infer_graph_types dagger =
  let type_map = Array.create ~len:(expression_graph_size dagger) (TID(0)) in
  let done_map = Array.create ~len:(expression_graph_size dagger) false in
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
    try ignore (infer i)
    with _ -> raise (Failure ("inference failed for program\n"
      ^ string_of_expression (extract_expression dagger i)))
  done; type_map
