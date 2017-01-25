open Core.Std

open Expression
open Library
open Utils

(* primitives are strict on all of their arguments *)
let extra_primitives = ref [];;
let register_primitive name arguments callback =
  let callback = fun terms ->
  List.map terms (fun t -> if t = c_bottom then None else Some(terminal_thing t)) |> callback
  in extra_primitives := (name,(arguments,callback)) :: !extra_primitives;;


type reduce_result =
  | Stepped of expression
  | NormalForm
  | Blocked of int * expression list (* what wildcard are we blocking on,
                                        and how could we instantiate it? *)

let merge_blocks b b_ =
  match (b,b_) with
  | (Blocked(_,[]),_) -> b_
  | (_,Blocked(_,[])) -> b
  | _ -> b


let try_primitive e =
  let rec walk_left name arity arguments = function
    | Terminal(n,_,_) when arity = 0 && n = name -> Some(arguments)
    | Application(f,x) when arity > 0 -> walk_left name (arity-1) (x::arguments) f
    | _ -> None
  in
  let try_application callback arguments actual_arguments =
    let rec walk_arguments argument actual =
      if List.is_empty argument
      then Stepped(match callback actual_arguments with
      | None -> c_bottom
      | Some(r) -> r)
      else match List.hd_exn actual with
      | Terminal(n,_,_) when n.[0] = '?' ->
        Blocked(int_of_string @@ String.sub n 1 (String.length n - 1),
               List.hd_exn argument)
      | _ -> walk_arguments (List.tl_exn argument) (List.tl_exn actual)
    in walk_arguments arguments actual_arguments
  in
  let rec try_primitives = function
    | [] -> NormalForm
    | (name,(arguments,callback))::prims ->
      match walk_left name (List.length arguments) [] e with
      | None -> try_primitives prims
      | Some(actual_arguments) ->
        try_application callback arguments actual_arguments
  in try_primitives !extra_primitives


let rec reduce_expression = function
  | Terminal(_,_,_) -> NormalForm
  | Application(Terminal(q,_,_),_) when q.[0] = '?' ->
    Blocked(int_of_string @@ String.sub q 1 (String.length q - 1), [])
  (* basis combinators *)
  | Application(Terminal(i,_,_),e) when i = "I" -> Stepped(e)
  | Application(Application(Terminal(k,_,_),e),_) when k = "K" -> Stepped(e)
  | Application(Application(Terminal(k,_,_),_),e) when k = "F" -> Stepped(e)
  | Application(Application(Application(Terminal(b,_,_),f),g),x) when b = "B" ->
    Stepped(Application(f,Application(g,x)))
  | Application(Application(Application(Terminal(c,_,_),f),g),x) when c = "C" ->
    Stepped(Application(Application(f,x),g))
  | Application(Application(Application(Terminal(s,a,b),f),g),x) when s = "S" ->
    (match reduce_expression x with
     | Stepped(y) -> Stepped(Application(Application(Application(Terminal(s,a,b),f),g),y))
     | NormalForm -> Stepped(Application(Application(f,x),Application(g,x)))
     | block -> block)
  (* append *)
  | Application(Application(Terminal(a,_,_),Terminal(n,_,_)),r) when a = "@" && n = "null" ->
    Stepped(r)
  | Application(Application(Terminal(a,_,_),
                            Application(Application(Terminal(c,_,_),x),xs)),ys)
    when a = "@" && c = "cons" ->
    Stepped(Application(Application(c_cons,x),Application(Application(c_append,xs),ys)))
  | Application(Application(Terminal(a,_,_),Terminal(q,_,_)),_)
    when a = "@" && q.[0] = '?' ->
    Blocked(int_of_string @@ String.sub q 1 (String.length q - 1),
            [c_null;Application(Application(c_cons,make_wildcard 1),make_wildcard 2)])
  (* last one *)
  | Application(Terminal(l,_,_), Terminal(q,_,_))
    when l = "last-one" && q.[0] = '?' ->
    Blocked(int_of_string @@ String.sub q 1 (String.length q - 1),
            [Application(Application(c_cons,make_wildcard 1),make_wildcard 2);])
  | Application(Terminal(l,_,_), argument)
    when l = "last-one" -> begin
      match reduce_expression argument with
      | Stepped(new_argument) -> Stepped(Application(c_last_one,new_argument))
      | NormalForm -> begin
          match argument with
          | Application(Application(c,first),rest) -> begin
              assert(string_of_expression c = "cons"); (* c has to be cons if normal form *)
              match rest with
              | Terminal(n,_,_) when n = "null" -> Stepped(first)
              | Terminal(q,_,_) when q.[0] = '?' ->
                Blocked(int_of_string @@ String.sub q 1 (String.length q - 1),
                        [Application(Application(c_cons,make_wildcard 1),make_wildcard 2);c_null;])
              | Application(Application(c_,_),_) ->
                (assert(string_of_expression c_ = "cons");
                 Stepped(Application(c_last_one,rest)))
              | _ -> raise (Failure ("last-one: invalid normal form: "^string_of_expression argument))
            end
          | _ -> raise (Failure ("last-one: invalid normal form(2): "^string_of_expression argument))
        end
      | block -> block
    end
  | Application(f,x) ->
    begin  (* inductive case *)
      match reduce_expression f with
      | Stepped(f_) -> Stepped(Application(f_,x))
      | normal_or_block -> begin
          match reduce_expression x with
          | Stepped(x_) -> Stepped(Application(f,x_))
          | NormalForm ->
            if normal_or_block = NormalForm
            then try_primitive (Application(f,x)) (* see if some other primitive applies *)
            else normal_or_block
          | block ->
            if normal_or_block = NormalForm then block else merge_blocks normal_or_block block
        end
    end
