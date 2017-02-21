open Interface

let c_up    = Expr.Terminal("upper", T.arrow T.s T.s, Lift.unary Str.uppercase)
let c_low   = Expr.Terminal("lower", T.arrow T.s T.s, Lift.unary Str.lowercase)
let c_cap   = Expr.Terminal("cap", T.arrow T.s T.s, Lift.unary Str.capitalize)
let c_uncap = Expr.Terminal("uncap", T.arrow T.s T.s, Lift.unary Str.uncapitalize)
let c_nth   = Expr.Terminal("nth", T.arrow T.s (T.arrow T.i T.s),
                    Lift.binary (fun s i -> try List.nth (Str.split s ~on:' ') i with Failure exp -> ""))
let c_zero  = Expr.Terminal("0",T.i,Obj.magic (ref 0))
let c_one   = Expr.Terminal("1",T.i,Obj.magic (ref 1))
let c_two   = Expr.Terminal("1",T.i,Obj.magic (ref 2))

let fnth s i f =
  let parts = Str.split ~on:' ' s in
  let newParts = List.mapi (fun j v -> if j == i then f (Some(v)) else Some(v)) parts in
  let unpackedParts = if List.for_all is_some newParts then List.map get_some newParts else parts in
  Str.concat ~sep:" " unpackedParts

let c_fnth = Expr.Terminal("fnth", T.arrow T.s (T.arrow T.i (T.arrow (T.arrow T.s T.s) T.s)), Lift.trinary fnth)

let combs = [C._S;C._B;C._C;C._I;C._K;c_up;c_low;c_cap;c_uncap;c_nth;c_zero;c_one;c_two;c_fnth]


let upper_first_task = task_of_problems [
  { i=Expr.of_str "test"; o="TEST"; };
  { i=Expr.of_str "tests"; o="TESTS"; };
  { i=Expr.of_str "test two"; o="TEST"; };
  { i=Expr.of_str "yet another test"; o="YET"; };
] ~t:(T.arrow T.s T.s) ~name:"upper first"
let tasks = [upper_first_task]


let main () =
  ec combs tasks 1
  ~lambda:1.5
  ~smoothing:1.0
  ~frontier_size:1000
  ~log_prefix:"kn"
;;


main ();;
