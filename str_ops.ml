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
let c_find_char  = Expr.Terminal("findchar", T.arrow T.c (T.arrow T.s T.i), Lift.binary (fun c s->
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
let c_is = Expr.Terminal("is", T.arrow T.s (T.arrow T.s T.b), Lift.binary (=))

let substr i j s =
  let i = i + (if i<0 then Str.length s else 0)
  and j = j + (if j<0 then Str.length s else 0)
  in Str.sub s ~pos:i ~len:(j - i)
let c_substr = Expr.Terminal("substr", T.arrow T.i (T.arrow T.i (T.arrow T.s T.s)), Lift.trinary substr)

let replace_substr_first t p s = Str.substr_replace_first ~with_:t ~pattern:p s
let c_replace_substr_first = Expr.Terminal("replace-substr-first", T.arrow T.s (T.arrow T.s (T.arrow T.s T.s)), Lift.trinary replace_substr_first)

let replace_substr_all t p s = Str.substr_replace_all ~with_:t ~pattern:p s
let c_replace_substr_all = Expr.Terminal("replace-substr-all", T.arrow T.s (T.arrow T.s (T.arrow T.s T.s)), Lift.trinary replace_substr_all)

let replace t i j s =
  let i = i + (if i<0 then Str.length s else 0)
  and j = j + (if j<0 then Str.length s else 0) in
  let left  = Str.sub s ~pos:0 ~len:i
  and right = Str.sub s ~pos:j ~len:((Str.length s)-(j+1))
  in left^t^right
let c_replace = Expr.Terminal("replace", T.arrow T.s (T.arrow T.i (T.arrow T.i (T.arrow T.s T.s))), Lift.quadinary replace)

let nth i s =
  let parts = Str.split ~on:' ' s in
  let i = i + (if i<0 then List.length parts else 0)
  in List.nth_or_default (Str.split s ~on:' ') i
let c_nth = Expr.Terminal("nth", T.arrow T.i (T.arrow T.s T.s), Lift.binary nth)

let fnth f i s =
  let parts = Str.split ~on:' ' s in
  let i = i + (if i<0 then List.length parts else 0) in
  let newParts = List.mapi ~f:(fun j v -> if j == i then f (Some(v)) else Some(v)) parts in
  let unpackedParts = if List.for_all newParts ~f:is_some then List.map newParts ~f:get_some else parts
  in Str.concat ~sep:" " unpackedParts
let c_fnth = Expr.Terminal("fnth", T.arrow (T.arrow T.s T.s) (T.arrow T.i (T.arrow T.s T.s)), Lift.trinary fnth)

let feach f s =
  let parts = Str.split ~on:' ' s in
  let newParts = List.mapi ~f:(fun _ v -> f (Some(v))) parts in
  let unpackedParts = if List.for_all newParts ~f:is_some then List.map newParts ~f:get_some else parts
  in Str.concat ~sep:" " unpackedParts
let c_feach = Expr.Terminal("feach", T.arrow (T.arrow T.s T.s) (T.arrow T.s T.s), Lift.binary feach)

let filter_words f s =
  let parts = Str.split ~on:' ' s in
  let newParts = List.filter ~f:(fun v -> get_some (f (Some(v)))) parts
  in Str.concat ~sep:" " newParts
let c_filter_words = Expr.Terminal("filter-words", T.arrow (T.arrow T.s T.b) (T.arrow T.s T.s), Lift.binary filter_words)

let combs = [
  c_empty;c_up;c_low;c_cap;c_concat;
  c_zero;c_incr;c_decr;c_wc;c_cc;c_string_of_int;
  c_find_char;c_char_spc;c_char_comma;c_char_dot;c_char_at;c_char_lang;c_char_rang;c_string_of_char;
  c_is;c_substr;c_replace_substr_first;c_replace_substr_all;c_replace;c_nth;c_fnth;c_feach;c_filter_words
]
