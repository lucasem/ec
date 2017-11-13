open Core.Std

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

let rec reduce_expression = function
  | Terminal(_,_,_) -> NormalForm
  | Application(Terminal(q,_,_),_) when q.[0] = '?' ->
    Blocked(int_of_string @@ String.sub q ~pos:1 ~len:(String.length q - 1), [])
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
  | Application(f,x) ->
    begin  (* inductive case *)
      match reduce_expression f with
      | Stepped(f_) -> Stepped(Application(f_,x))
      | normal_or_block -> begin
          match reduce_expression x with
          | Stepped(x_) -> Stepped(Application(f,x_))
          | NormalForm -> normal_or_block
          | block ->
            if normal_or_block = NormalForm then block else merge_blocks normal_or_block block
        end
    end
