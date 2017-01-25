open Core.Std

let ec
    initial_primitives
    tasks
    iterations
    ?lambda:(lambda = 1.5) (* this parameter controls how eager the system is to add new things to the grammar. Increase if you see over fitting and decrease if you see under fitting of grammar structure. *)
    ?smoothing:(smoothing = 1.0) (* pseudo- counts for grammar parameter estimation. Increase if you see over fitting and decrease if you see under fitting of grammar parameters. *)
    ?frontier_size:(frontier_size = 10000) (* how many programs to enumerate in each iteration of EC *)
    ?log_prefix:(log_prefix = "grammar") (* grammars will be logged under log/LOGPREFIXlog_iteration *)
  =
  let g = ref (Library.make_flat_library initial_primitives) in
  for i = 1 to iterations do 
    Printf.printf "\n \n \n Iteration %i \n" i;
    g := Em.expectation_maximization_iteration ("log/"^log_prefix^string_of_int i)
        lambda smoothing frontier_size tasks (!g)
  done;
  List.map ~f:(fun (e,(l,_)) -> (e,l)) (snd !g)

module C = struct
  let _S = Library.c_S
  let _B = Library.c_B
  let _C = Library.c_C
  let _I = Library.c_I
  let _plus = Library.c_plus
  let _times = Library.c_times
  let _zero = Library.c_zero
  let _one = Library.c_one
end;;

module T = struct
  type t = Type.tp = TID of int | TCon of string * t list
  let i = Type.tint
  let r = Type.treal
  let arrow = Type.make_arrow
end;;

module Expr = struct
  type e = Expression.expression = Terminal of string * T.t * unit ref | Application of e * e
  let run q = Expression.run_expression_for_interval 0.02 q
end;;

module Task = struct
  type objective = Task.task_objective = LogLikelihood of (Expr.e -> float) | Seed of Expr.e
  type t = Task.task = {
    name : string;
    task_type : T.t;
    score : objective;
    proposal : ((Expr.e -> float -> float) * (Expr.e*float) list) option;
  }
end;;

