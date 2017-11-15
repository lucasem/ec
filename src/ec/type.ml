open Core

module TypeMap = Map.Make(Int)


type tp =
  | TID of int
  | TCon of string * tp list

type tContext = int * tp TypeMap.t
let empty_context = (0,TypeMap.empty)
let make_arrow t q = TCon("->", [t;q])
let (@>) = make_arrow

let rec string_of_type (t : tp) : string =
  match t with
  | TID(i) -> string_of_int i
  | TCon(k,[]) -> k
  | TCon(k,[p;q]) when k = "->" -> "("^(string_of_type p)^" -> "^(string_of_type q)^")"
  | TCon(k,a) -> "("^k^" "^(String.concat ~sep:" " (List.map a ~f:string_of_type))^")"

let makeTID context = (TID(fst context), (fst context+1, snd context))

let bindTID i t context = (fst context, TypeMap.add (snd context) ~key:i ~data:t)

let rec chaseType (context : tContext) (t : tp) : tp*tContext =
  match t with
  | TCon(s, ts) ->
    let (ts_, context_) =
      List.fold_right ts
        ~f:(fun t (tz, k) ->
            let (t_, k_) = chaseType k t in
            (t_ :: tz, k_)) ~init:([], context)
    in (TCon(s, ts_), context_)
  | TID(i) ->
    match TypeMap.find (snd context) i with
    | Some(hit) ->
      let (t_, context_) = chaseType context hit in
      let substitution = TypeMap.add (snd context_) ~key:i ~data:t_ in
      (t_, (fst context_, substitution))
    | None -> (t,context)

let rec occurs (i : int) (t : tp) : bool =
  match t with
  | TID(j) -> j = i
  | TCon(_,ts) ->
    List.exists ts ~f:(occurs i)

let rec unify context t1 t2 : tContext =
  let (t1_, context_) = chaseType context t1 in
  let (t2_, context__) = chaseType context_ t2 in
  unify_ context__ t1_ t2_
and unify_ context t1 t2 : tContext =
  match (t1, t2) with
  | (TID(i), TID(j)) when i = j -> context
  | (TID(i), _) ->
    if occurs i t2
    then raise (Failure "occurs")
    else bindTID i t2 context
  | (_,TID(i)) ->
    if occurs i t1
    then raise (Failure "occurs")
    else bindTID i t1 context
  | (TCon(k1,as1),TCon(k2,as2)) when k1 = k2 ->
    List.fold2_exn ~init:context as1 as2 ~f:unify
  | _ -> raise (Failure "unify")

type fast_type =
  | FCon of string * fast_type list
  | FID of (fast_type option) ref

let can_unify (t1 : tp) (t2 : tp) : bool =
  let rec same_structure t1 t2 =
    match (t1, t2) with
    | (TCon(k1,as1), TCon(k2,as2)) when k1 = k2 ->
      List.for_all2_exn as1 as2 ~f:same_structure
    | (TID(_),_) -> true
    | (_,TID(_)) -> true
    | _ -> false
  in if not (same_structure t1 t2) then false else
    let rec make_fast_type dictionary t =
      match t with
      | TID(i) ->
        (match List.Assoc.find !dictionary ~equal:(=) i with
         | None -> let f = FID(ref None) in dictionary := (i,f)::!dictionary; f
         | Some(f) -> f)
      | TCon(k,xs) ->
        FCon(k, List.map xs ~f:(make_fast_type dictionary))
    in let rec fast_occurs r f =
      match f with
      | FID(r_) -> phys_equal r r_
      | FCon(_,fs) -> List.exists ~f:(fast_occurs r) fs
    in let rec fast_chase f =
      match f with
      | FID(r) ->
        (match !r with
         | None -> f
         | Some(f_) ->
           let f__ = fast_chase f_ in
           r := Some(f__); f__)
      | FCon(k,fs) -> FCon(k,List.map ~f:fast_chase fs)
    in let rec fast_unify t1 t2 =
      let t1 = fast_chase t1 in
      let t2 = fast_chase t2 in
      match (t1,t2) with
      | (FID(i1),FID(i2)) when phys_equal i1 i2 -> true
      | (FID(i),_) when fast_occurs i t2 -> false
      | (FID(i),_) -> i := Some(t2); true
      | (_,FID(i)) when fast_occurs i t1 -> false
      | (_,FID(i)) -> i := Some(t1); true
      | (FCon(k1,_),FCon(k2,_)) when k1 <> k2 -> false
      | (FCon(_,[]),FCon(_,[])) -> true
      | (FCon(_,x::xs),FCon(_,y::ys)) ->
        fast_unify x y &&
        List.for_all2_exn xs ys ~f:(fun a b -> fast_unify (fast_chase a) (fast_chase b))
      | _ -> raise (Failure "constructors of different arity")
    in fast_unify (make_fast_type (ref []) t1) (make_fast_type (ref []) t2)

let instantiate_type (n,m) t =
  let substitution = ref [] in
  let next = ref n in
  let rec instantiate j =
    match j with
    | TID(i) -> (try TID(List.Assoc.find_exn !substitution ~equal:(=) i)
                 with Not_found ->
                   substitution := (i,!next)::!substitution; next := (1+ !next); TID(!next-1)
                )
    | TCon(k,js) -> TCon(k,List.map ~f:instantiate js)
  in let q = instantiate t in
  (q,(!next,m))

(* puts a type into normal form *)
let canonical_type t =
  let next = ref 0 in
  let substitution = ref [] in
  let rec canon q =
    match q with
    | TID(i) -> (try TID(List.Assoc.find_exn !substitution ~equal:(=) i)
                 with Not_found ->
                   substitution := (i,!next)::!substitution; next := (1+ !next); TID(!next-1))
    | TCon(k,a) -> TCon(k,List.map ~f:canon a)
  in canon t

let rec next_type_variable_id t =
  match t with
  | TID(i) -> i+1
  | TCon(_,[]) -> 0
  | TCon(_,is) -> List.fold_left ~f:max ~init:0 (List.map is ~f:next_type_variable_id)
let new_type_variable t = TID(next_type_variable_id t)

(* tries to instantiate a universally quantified type with a given request *)
let instantiated_type universal_type requested_type =
  try
    let (universal_type,c) = instantiate_type empty_context universal_type in
    let (requested_type,c) = instantiate_type c requested_type in
    let c = unify c universal_type requested_type in
    Some(canonical_type (fst (chaseType c universal_type)))
  with _ -> None

let application_type f x =
  let (f,c1) = instantiate_type empty_context f in
  let (x,c2) = instantiate_type c1 x in
  let (r,c3) = makeTID c2 in
  let c4 = unify c3 f (x @> r) in
  canonical_type (fst (chaseType c4 r))

let argument_request request left =
  let (request,c1) = instantiate_type empty_context request in
  let (left,c2) = instantiate_type c1 left in
  match left with
  | TID(_) -> TID(0)
  | TCon(_,[right;result]) ->
      let c3 = unify c2 request result in
      canonical_type (fst (chaseType c3 right))
  | _ -> raise (Failure ("invalid function type: "^(string_of_type left)))

let function_request request =
  canonical_type (make_arrow (new_type_variable request) request)

let rec get_arity = function
  | TCon(a,[_;r]) when a = "->" -> 1+get_arity r
  | _ -> 0

let make_ground g = TCon(g,[])
let tint = make_ground "int"
let treal = make_ground "real"
let t0 = TID(0)
let t1 = TID(1)
let t2 = TID(2)
let t3 = TID(3)
let t4 = TID(4)
