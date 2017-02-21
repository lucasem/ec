open Core.Std

(* EXPORTS:
  * module Str    (* Core.Std.String *)
  * module C      (* combinators *)
  * module Lift   (* for expression terminal from function *)
  * module T      (* types *)
  * module Expr   (* expressions and running them *)
  * module Task   (*** use problem (below) instead ***)
  *
  * ec initial_primitives tasks iterations ?lambda ?smoothing ?frontier_size ?log_prefix
  *
  * type 'a problem = { i: Expr.e; o: 'a }
  * task_of_problems problems ~t ~name
  *
  * is_some
  * get_some
*)


module Str = struct
  include Core.Std.String
end;;

module C = struct
  let _S = Library.c_S
  let _B = Library.c_B
  let _C = Library.c_C
  let _I = Library.c_I
  let _K = Library.c_K
end;;

module Lift = struct
  let unary = Expression.lift_unary
  let binary = Expression.lift_binary
  let trinary = Expression.lift_trinary
  let quadinary = Expression.lift_quadinary
  let predicate = Expression.lift_predicate
end;;

module T = struct
  type t = Type.tp = TID of int | TCon of string * t list
  let i = Type.tint
  let r = Type.treal
  let s = Type.make_ground "string"
  let ls = Type.make_ground "list of strings"
  let arrow = Type.make_arrow
end;;

module Expr = struct
  type e = Expression.expression = Terminal of string * T.t * unit ref | Application of e * e
  let run q = Expression.run_expression_for_interval 0.02 q
  let of_int n = Terminal(string_of_int n, T.TID(0), Obj.magic (ref n))
  let of_str s = Terminal(s, T.TID(0), Obj.magic (ref s))
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
;;


type 'a problem = { i: Expr.e; o: 'a }
;;


let task_of_problems problems ~t ~name =
  let single_score_func e p logl =
    let q = Expr.Application(e, p.i) in
    match Expr.run q with
    | Some(r) when r = p.o -> logl
    | _ -> Core.Std.Float.neg_infinity
  in
  let score_func = (fun (e : Expr.e) ->
    let rec r y =
      match y with
      | [] -> 0.0
      | (p::ps) -> single_score_func e p (r ps)
    in r problems
  ) in
  Task.{
    name = name;
    task_type = t;
    score = LogLikelihood(score_func);
    proposal = None;
  }
;;


let is_some = Utils.is_some
let get_some = Utils.get_some
