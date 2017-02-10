open Interface

let c_plus  = Expr.Terminal("+", T.arrow T.i (T.arrow T.i T.i), Lift.binary (+))
let c_times = Expr.Terminal("*", T.arrow T.i (T.arrow T.i T.i), Lift.binary (fun x y ->x*y ))
let c_one   = Expr.Terminal("1",T.i,Obj.magic (ref 1))
let c_zero  = Expr.Terminal("0",T.i,Obj.magic (ref 0))
let combs = [C._S;C._B;C._C;C._I;C._K;c_plus;c_times;c_zero;c_one]


let unity_task = task_of_problems [
  { i=Expr.of_int 1; o=1; };
] ~t:(T.arrow T.i T.i) ~name:"unity"
let three_task = task_of_problems [
  { i=Expr.of_int 1; o=3; };
  { i=Expr.of_int 2; o=3; };
  { i=Expr.of_int 3; o=3; };
] ~t:(T.arrow T.i T.i) ~name:"three"
let tasks = [unity_task;three_task]


let main () =
  ec combs tasks 1
  ~lambda:1.5
  ~smoothing:1.0
  ~frontier_size:1000
  ~log_prefix:"kn"
;;


main ();;
