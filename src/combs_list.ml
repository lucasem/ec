open Interface

(* basic list ops *)
let c_empty = Expr.Terminal("empty", T.l, Obj.magic (ref []))
let c_concat = Expr.Terminal("concat", T.arrow T.l (T.arrow T.l T.l), Lift.binary (fun a b->List.concat [a;b]))
let c_len  = Expr.Terminal("len", T.arrow T.l T.i, Lift.unary List.length)
let c_singleton  = Expr.Terminal("singleton", T.arrow T.i T.l, Lift.unary (fun n->[n]))

(* numbers *)
let c_zero = Expr.Terminal("zero", T.i, Obj.magic (ref 0))
let c_one  = Expr.Terminal("one", T.i, Obj.magic (ref 1))
let c_two  = Expr.Terminal("two", T.i, Obj.magic (ref 2))
let c_incr = Expr.Terminal("incr", T.arrow T.i T.i, Lift.unary (fun x->x+1))
let c_decr = Expr.Terminal("decr", T.arrow T.i T.i, Lift.unary (fun x->x-1))
let c_neg  = Expr.Terminal("neg", T.arrow T.i T.i, Lift.unary (fun x->(-x)))

(* binary number ops *)
let c_add  = Expr.Terminal("add", T.arrow T.i (T.arrow T.i T.i), Lift.binary (+))
let c_mul  = Expr.Terminal("mul", T.arrow T.i (T.arrow T.i T.i), Lift.binary (fun a b->a*b))
let c_div  = Expr.Terminal("div", T.arrow T.i (T.arrow T.i T.i), Lift.binary (/))
let c_mod  = Expr.Terminal("mod", T.arrow T.i (T.arrow T.i T.i), Lift.binary (mod))

(* patterns & finding *)
let c_find_num  = Expr.Terminal("findnum", T.arrow T.i (T.arrow T.l T.i), Lift.binary (fun n l->
  match List.findi l ~f:(fun _ v->v==n) with
    | Some((i,_)) -> i
    | None -> List.length l))

let c_is = Expr.Terminal("is", T.arrow T.i (T.arrow T.i T.b), Lift.binary (=))

(* advanced ops *)
let subslice i j l =
  let i = i + (if i<0 then List.length l else 0)
  and j = j + (if j<0 then 1 + List.length l else 0) (* substr 0 -1 is identity *)
  in List.sub l ~pos:i ~len:(j - i)
let c_subslice = Expr.Terminal("subslice", T.arrow T.i (T.arrow T.i (T.arrow T.l T.l)), Lift.trinary subslice)

let replace t i l =
  let i = i + (if i<0 then List.length l else 0) in
  let left  = List.sub l ~pos:0 ~len:i
  and right = List.sub l ~pos:i ~len:((List.length l)-(i+1))
  in List.concat [left;[t];right]
let c_replace = Expr.Terminal("replace", T.arrow T.i (T.arrow T.i (T.arrow T.l T.l)), Lift.trinary replace)

let nth i l =
  let i = i + (if i<0 then List.length l else 0)
  in List.nth_or_default l i
let c_nth = Expr.Terminal("nth", T.arrow T.i (T.arrow T.l T.i), Lift.binary nth)

let fnth f i l =
  let i = i + (if i<0 then List.length l else 0)
  in List.mapi ~f:(fun j v -> if j == i then get_some (f (Some(v))) else v) l
let c_fnth = Expr.Terminal("fnth", T.arrow (T.arrow T.i T.i) (T.arrow T.i (T.arrow T.l T.l)), Lift.trinary fnth)

let map f l = List.map ~f:(fun v -> get_some (f (Some(v)))) l
let c_map = Expr.Terminal("map", T.arrow (T.arrow T.i T.i) (T.arrow T.l T.l), Lift.binary map)

let filter f l = List.filter ~f:(fun v -> get_some (f (Some(v)))) l
let c_filter = Expr.Terminal("filter", T.arrow (T.arrow T.i T.b) (T.arrow T.l T.l), Lift.binary filter)

let fold f i l = List.fold ~init:i ~f:(fun a v -> get_some (f a (Some(v)))) l
let c_fold = Expr.Terminal("fold", T.arrow (T.arrow T.i (T.arrow T.i T.i)) (T.arrow T.i (T.arrow T.l T.i)), Lift.trinary fold)

let combs = [
  c_empty;c_concat;c_singleton;
  c_zero;c_one;c_two;c_incr;c_decr;c_neg;
  c_add;c_mul;c_div;c_mod;c_len;
  c_find_num;c_is;
  c_subslice;c_replace;c_nth;c_fnth;
  c_map;c_filter;
]
