open Core

(* EXPORTS:
  * module Str    (* Core.String *)
  * module List   (* Core.List *)
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
  include Core.String
end

module List = struct
  include Core.List
  let nth_or_default ?default:(default="" ) l i =
    match List.nth l i with
      | Some(x) -> x
      | None -> default
end

module C = struct
  let _S = Library.c_S
  let _B = Library.c_B
  let _C = Library.c_C
  let _I = Library.c_I
  let _K = Library.c_K
  let prims = [_S;_B;_C;_I;_K]
end

module Lift = struct
  let unary = Expression.lift_unary
  let binary = Expression.lift_binary
  let trinary = Expression.lift_trinary
  let quadinary = Expression.lift_quadinary
  let predicate = Expression.lift_predicate
end

module T = struct
  type t = Type.tp = TID of int | TCon of string * t list
  let make = Type.make_ground
  let arrow = Type.make_arrow
  let i = make "int"
  let b = make "bool"
  let s = make "string"
  let c = make "char"
  let l x = TCon("list", [x])
end

module Expr = struct
  type e = Expression.expression = Terminal of string * T.t * unit ref | Application of e * e
  let run = Expression.run_expression
  let of_int n = Terminal(string_of_int n, T.i, Obj.magic (ref n))
  let of_str s = Terminal(s, T.s, Obj.magic (ref s))
  let of_char_list cl = Terminal(
    String.concat ~sep:"" (List.map ~f:(String.make 1) cl),
    T.l T.c, Obj.magic (ref cl))
  let of_int_list l = Terminal(List.to_string ~f:string_of_int l, T.l T.i, Obj.magic (ref l))
  let to_str e = Expression.string_of_expression e
  let unmarshal prims = Library.expression_of_string_with_combs prims
end

module Task = struct
  type objective = Task.task_objective = LogLikelihood of (Expr.e -> float) | Seed of Expr.e
  type t = Task.task = {
    name : string;
    task_type : T.t;
    score : objective;
    proposal : ((Expr.e -> float -> float) * (Expr.e*float) list) option;
  }
end

let ec
    initial_primitives
    tasks
    ?lambda:(lambda=1.5)
        (* this parameter controls how eager the system is to add new things to
         * the grammar. Increase if you see over fitting and decrease if you
         * see under fitting of grammar structure. *)
    ?lambda_final:(lambda_final=lambda)
        (* same as lambda, but only applies to the final iteration. *)
    ?smoothing:(smoothing=1.0)
        (* pseudo- counts for grammar parameter estimation. Increase if you see
         * over fitting and decrease if you see under fitting of grammar
         * parameters. *)
    ?frontier_size:(frontier_size=10000)
        (* how many programs to enumerate in each iteration of EC *)
    iterations
  =
  let g = ref (Library.make_flat_library initial_primitives)
  and p = ref (None) in
  for it = 1 to iterations do
    let l = if it < iterations then lambda else lambda_final in
    let ng, np, _nbic = Em.expectation_maximization_iteration
        l smoothing frontier_size tasks (!g) in
    g := ng;
    p := Some(np)
  done;
  let grammar = List.map (snd !g) ~f:(fun (e,(l,_)) -> (Expr.to_str e,l))
  and progs = get_some !p in
  grammar, progs

type problem = { i: Expr.e list; o: unit ref }

let task_of_problems problems ~tp ~name =
  let single_score_func e p logl =
    let q = List.fold ~init:e ~f:(fun acc e -> Expr.Application(acc, e)) p.i in
    match Expr.run q with
    | Some(r) when r = (!(Obj.magic p.o)) -> logl
    | _ -> Core.Float.neg_infinity
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
    task_type = tp;
    score = LogLikelihood(score_func);
    proposal = None;
  }
