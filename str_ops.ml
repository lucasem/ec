open Interface

(* basic string ops *)
let c_empty = Expr.Terminal("empty", T.s, Obj.magic (ref ""))
let c_up    = Expr.Terminal("upper", T.arrow T.s T.s, Lift.unary Str.uppercase)
let c_low   = Expr.Terminal("lower", T.arrow T.s T.s, Lift.unary Str.lowercase)
let c_cap   = Expr.Terminal("cap", T.arrow T.s T.s, Lift.unary Str.capitalize)
let c_concat = Expr.Terminal("+", T.arrow T.s (T.arrow T.s T.s), Lift.binary (^))

(* ints *)
let c_zero  = Expr.Terminal("0", T.i, Obj.magic (ref 0))
let c_incr  = Expr.Terminal("+1", T.arrow T.i T.i, Lift.unary (fun x->x+1))
let c_decr  = Expr.Terminal("-1", T.arrow T.i T.i, Lift.unary (fun x->x-1))
let c_wc    = Expr.Terminal("wc", T.arrow T.s T.i, Lift.unary (fun s->List.length @@ Str.split ~on:' ' s))
let c_cc    = Expr.Terminal("cc", T.arrow T.s T.i, Lift.unary Str.length)
let c_string_of_int  = Expr.Terminal("string-of-int", T.arrow T.i T.s, Lift.unary string_of_int)

(* patterns & finding *)
let c_find_char  = Expr.Terminal("findchar", T.arrow T.s (T.arrow T.c T.i), Lift.binary (fun s c->
  match Str.index s c with
    | Some(i) -> i
    | None -> Str.length s))
let c_char_spc   = Expr.Terminal("<SPACE>", T.c, Obj.magic (ref ' '))
let c_char_comma = Expr.Terminal("<COMMA>", T.c, Obj.magic (ref ','))
let c_char_dot   = Expr.Terminal("<DOT>", T.c, Obj.magic (ref '.'))
let c_char_at    = Expr.Terminal("<AT>", T.c, Obj.magic (ref '@'))
let c_char_lang  = Expr.Terminal("<LESS-THAN>", T.c, Obj.magic (ref '<'))
let c_char_rang  = Expr.Terminal("<GREATER-THAN>", T.c, Obj.magic (ref '>'))
let c_string_of_char = Expr.Terminal("string-of-char", T.arrow T.c T.s, Lift.unary (fun c->String.make 1 c))

(* advanced string ops *)
let substr s i j =
  let i = i + (if i<0 then Str.length s else 0) in
  let j = j + (if j<0 then Str.length s else 0) in
  Str.sub s ~pos:i ~len:(j - i)
let c_substr = Expr.Terminal("substr", T.arrow T.s (T.arrow T.i (T.arrow T.i T.s)), Lift.trinary substr)

let replace s t i =
  let i = i + (if i<0 then Str.length s else 0) in
  let left  = Str.sub s ~pos:0 ~len:(i-1)
  and right = Str.sub s ~pos:(i+1) ~len:((Str.length s)-(i+1))
  in left^t^right
let c_replace = Expr.Terminal("replace-index", T.arrow T.s (T.arrow T.s (T.arrow T.i T.s)), Lift.trinary replace)

let replace_all s t c =
  let parts = Str.split s ~on:c in
  List.fold (List.tl_exn parts) ~init:(List.hd_exn parts) ~f:(fun a b->a^t^b)
let c_replace_all = Expr.Terminal("replace-all", T.arrow T.s (T.arrow T.s (T.arrow T.c T.s)), Lift.trinary replace_all)

let nth s i =
  let parts = Str.split ~on:' ' s in
  let i = i + (if i<0 then List.length parts else 0) in
  List.nth_or_default (Str.split s ~on:' ') i
let c_nth = Expr.Terminal("nth", T.arrow T.s (T.arrow T.i T.s), Lift.binary nth)

let fnth s i f =
  let parts = Str.split ~on:' ' s in
  let i = i + (if i<0 then List.length parts else 0) in
  let newParts = List.mapi ~f:(fun j v -> if j == i then f (Some(v)) else Some(v)) parts in
  let unpackedParts = if List.for_all newParts ~f:is_some then List.map newParts ~f:get_some else parts in
  Str.concat ~sep:" " unpackedParts
let c_fnth = Expr.Terminal("fnth", T.arrow T.s (T.arrow T.i (T.arrow (T.arrow T.s T.s) T.s)), Lift.trinary fnth)

let feach s f =
  let parts = Str.split ~on:' ' s in
  let newParts = List.mapi ~f:(fun _ v -> f (Some(v))) parts in
  let unpackedParts = if List.for_all newParts ~f:is_some then List.map newParts ~f:get_some else parts in
  Str.concat ~sep:" " unpackedParts
let c_feach = Expr.Terminal("feach", T.arrow T.s (T.arrow (T.arrow T.s T.s) T.s), Lift.binary feach)

let combs = [
  c_empty;c_up;c_low;c_cap;c_concat;
  c_zero;c_incr;c_decr;c_wc;c_cc;c_string_of_int;
  c_find_char;c_char_spc;c_char_comma;c_char_dot;c_char_at;c_char_lang;c_char_rang;c_string_of_char;
  c_substr;c_replace;c_replace_all;c_nth;c_fnth;c_feach
]
