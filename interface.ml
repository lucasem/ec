open Core.Std

(* EXPORTS:
  * module Str    (* Core.Std.String *)
  * module List   (* Core.Std.List *)
  * module C      (* combinators (primitive) *)
  * module Lift   (* for expression terminal from function *)
  * module T      (* types *)
  * module Expr   (* expressions and running them *)
  * module Task   (*** use problem (below) instead ***)
  *
  * ec initial_primitives tasks iterations ?lambda ?smoothing ?frontier_size
  *
  * type 'a problem = { i: Expr.e; o: 'a }
  * task_of_problems problems ~t ~name
  *
  * is_some
  * get_some
*)

let is_some = Utils.is_some
let get_some = Utils.get_some


module Str = struct
  include Core.Std.String
end;;

module List = struct
  include Core.Std.List
  let nth_or_default ?default:(default="" ) l i =
    match List.nth l i with
      | Some(x) -> x
      | None -> default
end;;

module C = struct
  let _S = Library.c_S
  let _B = Library.c_B
  let _C = Library.c_C
  let _I = Library.c_I
  let _K = Library.c_K
  let prims = [_S;_B;_C;_I;_K]
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
  let make = Type.make_ground
  let arrow = Type.make_arrow
  let i = make "int"
  let s = make "string"
  let c = make "char"
end;;

module Expr = struct
  type e = Expression.expression = Terminal of string * T.t * unit ref | Application of e * e
  let run q = Expression.run_expression_for_interval 0.02 q
  let of_int n = Terminal(string_of_int n, T.i, Obj.magic (ref n))
  let of_str s = Terminal(s, T.s, Obj.magic (ref s))
  let to_str e = Expression.string_of_expression e
  let unmarshal prims = Library.expression_of_string_with_combs prims
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
    ?lambda:(lambda=1.5)
        (* this parameter controls how eager the system is to add new things to
         * the grammar. Increase if you see over fitting and decrease if you
         * see under fitting of grammar structure. *)
    ?smoothing:(smoothing=1.0)
        (* pseudo- counts for grammar parameter estimation. Increase if you see
         * over fitting and decrease if you see under fitting of grammar
         * parameters. *)
    ?frontier_size:(frontier_size=10000)
        (* how many programs to enumerate in each iteration of EC *)
    iterations
  =
  let g = ref (Library.make_flat_library initial_primitives)
  and p = ref (None)
  and bic = ref (0.0) in
  for _ = 1 to iterations do
    let ng, np, nbic = Em.expectation_maximization_iteration
        lambda smoothing frontier_size tasks (!g) in
    g := ng;
    p := Some(np);
    bic := nbic
  done;
  let grammar = List.map (snd !g) ~f:(fun (e,(l,_)) -> (Expr.to_str e,l))
  and progs = get_some !p in
  let hit_rate =
    List.fold_left ~f:(+) ~init:0 @@ List.map progs
      ~f:(fun (_, res) -> match res with
        | Some(_) -> 1
        | None    -> 0
      ) in
  grammar, progs, !bic, hit_rate
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
