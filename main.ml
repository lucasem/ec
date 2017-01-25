open Interface

let combs = [C._S;C._B;C._C;C._I;C._plus;C._times;C._zero;C._one] ;;

let unity_task = (* f(1) = 1 *)
  let score_func = (fun (e : Expr.e) ->
    let q = Expr.Application(e,Expr.Terminal("1",T.TID(0),Obj.magic (ref 1))) in
    match Expr.run q with
    | Some(r) when r = 1 -> 0.0
    | _ -> Core.Std.Float.neg_infinity
  ) in
  Task.{
    name = "test";
    task_type = T.arrow T.i T.i;
    score = Task.LogLikelihood(score_func);
    proposal = None;
  }
;;

let main () =
  let tasks = [unity_task] in
  ec combs tasks 16
  ~lambda:1.5
  ~smoothing:1.0
  ~frontier_size:1000
  ~log_prefix:"kn"
;;


main ();;
