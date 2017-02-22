open Interface

(* basic string ops *)
let c_up    = Expr.Terminal("upper", T.arrow T.s T.s, Lift.unary Str.uppercase)
let c_low   = Expr.Terminal("lower", T.arrow T.s T.s, Lift.unary Str.lowercase)
let c_cap   = Expr.Terminal("cap", T.arrow T.s T.s, Lift.unary Str.capitalize)
let c_uncap = Expr.Terminal("uncap", T.arrow T.s T.s, Lift.unary Str.uncapitalize)
let c_nth   = Expr.Terminal("nth", T.arrow T.s (T.arrow T.i T.s),
                    Lift.binary (fun s i -> try List.nth (Str.split s ~on:' ') i with Failure exp -> ""))
let c_substr = Expr.Terminal("substr", T.arrow T.s (T.arrow T.i (T.arrow T.i T.s)), Lift.trinary (fun s i j -> Str.sub s ~pos:i ~len:(j - i)))
let c_concat = Expr.Terminal("+", T.arrow T.s (T.arrow T.s T.s), Lift.binary (^))

(* patterns & finding *)
let tc = T.make "char"
let c_find_char = Expr.Terminal("findchar", T.arrow T.s (T.arrow tc T.i), Lift.binary Str.index)
let c_char_spc   = Expr.Terminal("','", tc, Obj.magic (ref ','))
let c_char_comma = Expr.Terminal("','", tc, Obj.magic (ref ','))
let c_char_dot   = Expr.Terminal("'.'", tc, Obj.magic (ref '.'))
let c_char_at    = Expr.Terminal("'@'", tc, Obj.magic (ref '@'))
let c_char_lang  = Expr.Terminal("'<'", tc, Obj.magic (ref '<'))
let c_char_rang  = Expr.Terminal("'>'", tc, Obj.magic (ref '>'))
let c_string_of_char = Expr.Terminal("string-of-char", T.arrow tc T.s, Lift.unary (fun c -> String.make 1 c))

(* ints *)
let c_zero  = Expr.Terminal("0", T.i, Obj.magic (ref 0))
let c_len   = Expr.Terminal("len", T.arrow T.s T.i, Lift.unary Str.length)
let c_incr  = Expr.Terminal("+1", T.arrow T.i T.i, Lift.unary (fun x->x+1))
let c_decr  = Expr.Terminal("-1", T.arrow T.i T.i, Lift.unary (fun x->x-1))

(* advanced string ops *)
let fnth s i f =
  let parts = Str.split ~on:' ' s in
  let newParts = List.mapi (fun j v -> if j == i then f (Some(v)) else Some(v)) parts in
  let unpackedParts = if List.for_all is_some newParts then List.map get_some newParts else parts in
  Str.concat ~sep:" " unpackedParts
let c_fnth = Expr.Terminal("fnth", T.arrow T.s (T.arrow T.i (T.arrow (T.arrow T.s T.s) T.s)), Lift.trinary fnth)
let feach s f =
  let parts = Str.split ~on:' ' s in
  let newParts = List.mapi (fun j v -> f (Some(v))) parts in
  let unpackedParts = if List.for_all is_some newParts then List.map get_some newParts else parts in
  Str.concat ~sep:" " unpackedParts
let c_feach = Expr.Terminal("feach", T.arrow T.s (T.arrow (T.arrow T.s T.s) T.s), Lift.binary feach)

let combs = [C._S;C._B;C._C;C._I;C._K;
  c_up;c_low;c_cap;c_uncap;c_nth;c_substr;c_concat;
  c_find_char;c_char_spc;c_char_comma;c_char_dot;c_char_at;c_char_lang;c_char_rang;c_string_of_char;
  c_zero;c_len;c_incr;c_decr;
  c_fnth;c_feach]

let upper_first_task = task_of_problems [
  { i=Expr.of_str "test"; o="TEST"; };
  { i=Expr.of_str "tests"; o="TESTS"; };
  { i=Expr.of_str "test two"; o="TEST two"; };
  { i=Expr.of_str "yet another test"; o="YET another test"; };
] ~t:(T.arrow T.s T.s) ~name:"upper first"
let cap_last_task = task_of_problems [
  { i=Expr.of_str "test"; o="Test"; };
  { i=Expr.of_str "tests"; o="Tests"; };
  { i=Expr.of_str "test two"; o="test Two"; };
  { i=Expr.of_str "yet another test"; o="yet another Test"; };
] ~t:(T.arrow T.s T.s) ~name:"cap last"

let task01 = task_of_problems [
  { i=Expr.of_str "My name is Richard"; o="Richard"; };
  { i=Expr.of_str "My name is John"; o="John"; };
  { i=Expr.of_str "My name is Josh"; o="Josh"; };
  { i=Expr.of_str "My name is Bill"; o="Bill"; };
  { i=Expr.of_str "My name is Albert"; o="Albert"; };
] ~t:(T.arrow T.s T.s) ~name:"My name is Richard -> Richard"
let task02 = task_of_problems [
  { i=Expr.of_str "thomas"; o="Thomas"; };
  { i=Expr.of_str "james"; o="James"; };
  { i=Expr.of_str "chris"; o="Chris"; };
  { i=Expr.of_str "charles"; o="Charles"; };
  { i=Expr.of_str "paul"; o="Paul"; };
] ~t:(T.arrow T.s T.s) ~name:"thomas -> Thomas"
let task03 = task_of_problems [
  { i=Expr.of_str "Arthur Joe Juan"; o="A.J.J."; };
  { i=Expr.of_str "Donald Steven George"; o="D.S.G."; };
  { i=Expr.of_str "Kevin Jason Matthew"; o="K.J.M."; };
  { i=Expr.of_str "Jose Larry Scott"; o="J.L.S."; };
  { i=Expr.of_str "Raymond Frank Timothy"; o="R.F.T."; };
] ~t:(T.arrow T.s T.s) ~name:"Arthur Joe Juan -> A.J.J."

let task04 = task_of_problems [
  { i=Expr.of_str "ruby.clinton@mit.edu"; o="Ruby Clinton"; };
  { i=Expr.of_str "josh.smith@gmail.com"; o="Josh Smith"; };
  { i=Expr.of_str "matthew.rosman@yahoo.com"; o="Matthew Rosman"; };
  { i=Expr.of_str "brent.harold@hotmail.com"; o="Brent Harold"; };
  { i=Expr.of_str "jim.james@fas.harvard.edu"; o="Jim James"; };
] ~t:(T.arrow T.s T.s) ~name:"ruby.clinton@mit.edu -> Ruby Clinton"

let task06 = task_of_problems [
  { i=Expr.of_str "CHORE BOY HD SC SPNG 1PK"; o="1PK"; };
  { i=Expr.of_str "BTR KRNL WK CORN 15Z"; o="15Z"; };
  { i=Expr.of_str "CAMP DRY DBL NDL 3.6OZ"; o="3.6OZ"; };
  { i=Expr.of_str "O F TOMATO PASTE 6OZ"; o="6OZ"; };
  { i=Expr.of_str "FRENCH WORCESTERSHIRE 5Z"; o="5Z"; };
] ~t:(T.arrow T.s T.s) ~name:"CHORE BOY HD SC SPNG 1PK -> 1PK"

let task08 = task_of_problems [
  { i=Expr.of_str "Principals Of Programming Languages"; o="POPL"; };
  { i=Expr.of_str "Neural Information Processing Systems"; o="NIPS"; };
  { i=Expr.of_str "International Conferences on Software Engineering"; o="ICSE"; };
  { i=Expr.of_str "International Conference on Functional Programming"; o="ICFP"; };
  { i=Expr.of_str "International Business Machines"; o="IBM"; };
] ~t:(T.arrow T.s T.s) ~name:"Principals Of Programming Languages -> POPL"

let task10 = task_of_problems [
  { i=Expr.of_str "5/5/1987"; o="87"; };
  { i=Expr.of_str "1/21/2001"; o="01"; };
  { i=Expr.of_str "21/1/2001"; o="01"; };
  { i=Expr.of_str "22.02.2002"; o="02"; };
  { i=Expr.of_str "2003-23-03"; o="03"; };
] ~t:(T.arrow T.s T.s) ~name:"5/5/1987 -> 87"

let task13 = task_of_problems [
  { i=Expr.of_str "12012011"; o="12/01/2011"; };
  { i=Expr.of_str "11152011"; o="11/15/2011"; };
  { i=Expr.of_str "06222005"; o="06/22/2005"; };
  { i=Expr.of_str "01252010"; o="01/25/2010"; };
  { i=Expr.of_str "01112011"; o="01/11/2011"; };
] ~t:(T.arrow T.s T.s) ~name:"12012011 -> 12/01/2011"

let task15 = task_of_problems [
  { i=Expr.of_str "6/23/15"; o="6.23.2015"; };
  { i=Expr.of_str "4/12/2023"; o="4.12.2023"; };
  { i=Expr.of_str "1/23/2009"; o="1.23.2009"; };
  { i=Expr.of_str "12/32/2013"; o="12.32.2013"; };
  { i=Expr.of_str "7/15/2015"; o="7.15.2015"; };
] ~t:(T.arrow T.s T.s) ~name:"6/23/15 -> 6.23.2015"

let task16 = task_of_problems [
  { i=Expr.of_str "IaN RoDny"; o="Ian Rodny"; };
  { i=Expr.of_str "MELVIN Julian"; o="Melvin Julian"; };
  { i=Expr.of_str "miKe dwIGHT"; o="Mike Dwight"; };
  { i=Expr.of_str "StaNleY TRAVis"; o="Stanley Travis"; };
  { i=Expr.of_str "mary gelman"; o="Mary Gelman"; };
] ~t:(T.arrow T.s T.s) ~name:"IaN RoDny -> Ian Rodny"

let task17 = task_of_problems [
  { i=Expr.of_str "herbert is <2: Marion> morris"; o="(2 marion)"; };
  { i=Expr.of_str "country music <9: refrigerator>"; o="(9 refrigerator)"; };
  { i=Expr.of_str "Three <2: vincent> Jeff"; o="(2 vincent)"; };
  { i=Expr.of_str "Don Kyle <3: ricky sergio> virgil"; o="(3 ricky)"; };
  { i=Expr.of_str "francisco eduardo <1: apples>"; o="(1 apple trees)"; };
] ~t:(T.arrow T.s T.s) ~name:"herbert is <2: Marion> morris -> (2 marion)"

let task18 = task_of_problems [
  { i=Expr.of_str "3 Ames St. Portland OR 02142"; o="Portland"; };
  { i=Expr.of_str "47 Foskett St. #2 Cambridge MA 02144"; o="Cambridge"; };
  { i=Expr.of_str "43 Vassar St. 46-4053 Cambridge MA 02139"; o="Cambridge"; };
  { i=Expr.of_str "3113 Greenfield Ave. Los Angeles CA 90034"; o="Los Angeles"; };
  { i=Expr.of_str "43 St. Margaret St. #1 Dorchester MA 02145"; o="Dorchester"; };
] ~t:(T.arrow T.s T.s) ~name:"3 Ames St. Portland OR 02142 -> Portland"

let task19 = task_of_problems [
  { i=Expr.of_str "Marin Lorentzen"; o="M.L."; };
  { i=Expr.of_str "Annita Nicely"; o="A.N."; };
  { i=Expr.of_str "Joanie Faas"; o="J.F."; };
  { i=Expr.of_str "Oma Cornelison"; o="O.C."; };
  { i=Expr.of_str "Verlene Ottley"; o="V.O."; };
] ~t:(T.arrow T.s T.s) ~name:"Marin Lorentzen -> M.L."

let task21 = task_of_problems [
  { i=Expr.of_str "#include <os.h>"; o="OS"; };
  { i=Expr.of_str "#include <malloc.h>"; o="MALLOC"; };
  { i=Expr.of_str "#include <stdlib.h>"; o="STDLIB"; };
  { i=Expr.of_str "#include <sys.h>"; o="SYS"; };
  { i=Expr.of_str "#include <stdio.h>"; o="STDIO"; };
] ~t:(T.arrow T.s T.s) ~name:"#include <os.h> -> OS"

let task28 = task_of_problems [
  { i=Expr.of_str "August 12 1993"; o="August"; };
  { i=Expr.of_str "January 8"; o="January"; };
  { i=Expr.of_str "December 1990"; o="December"; };
  { i=Expr.of_str "2007 (September)"; o="September"; };
  { i=Expr.of_str "July 4 2015"; o="July"; };
] ~t:(T.arrow T.s T.s) ~name:"August 12 1993 -> August"

let task29 = task_of_problems [
  { i=Expr.of_str "47 Foskett St. #2 Cambridge MA 02144"; o="Foskett"; };
  { i=Expr.of_str "43 Vassar St. 46-4053 Cambridge MA 02139"; o="Vassar"; };
  { i=Expr.of_str "43 St. Margaret St. #1 Dorchester MA 02145"; o="St. Margaret"; };
  { i=Expr.of_str "3113 Greenfield Ave. Los Angeles CA 90034"; o="Greenfield"; };
  { i=Expr.of_str "3 Ames St. Portland OR 02142"; o="Ames"; };
] ~t:(T.arrow T.s T.s) ~name:"47 Foskett St. #2 Cambridge MA 02144 -> Foskett"

let task30 = task_of_problems [
  { i=Expr.of_str "3113 Greenfield Ave. Los Angeles CA 90034"; o="3113"; };
  { i=Expr.of_str "47 Foskett St. #2 Cambridge MA 02144"; o="47"; };
  { i=Expr.of_str "3 Ames St. Portland OR 02142"; o="3"; };
  { i=Expr.of_str "43 Vassar St. 46-4053 Cambridge MA 02139"; o="43"; };
  { i=Expr.of_str "43 St. Margaret St. #1 Dorchester MA 02145"; o="43"; };
] ~t:(T.arrow T.s T.s) ~name:"3113 Greenfield Ave. Los Angeles CA 90034 -> 3113"

let task31 = task_of_problems [
  { i=Expr.of_str "3113 Greenfield Ave. Los Angeles CA 90034"; o="CA"; };
  { i=Expr.of_str "43 St. Margaret St. #1 Dorchester MA 02145"; o="MA"; };
  { i=Expr.of_str "3 Ames St. Portland OR 02142"; o="OR"; };
  { i=Expr.of_str "47 Foskett St. #2 Cambridge MA 02144"; o="MA"; };
  { i=Expr.of_str "43 Vassar St. 46-4053 Cambridge MA 02139"; o="MA"; };
] ~t:(T.arrow T.s T.s) ~name:"3113 Greenfield Ave. Los Angeles CA 90034 -> CA"

let another_task = task_of_problems [
  { i=Expr.of_str "IaN"; o="Ian"; };
  { i=Expr.of_str "MELVIN"; o="Melvin"; };
  { i=Expr.of_str "miKe"; o="Mike"; };
  { i=Expr.of_str "StaNleY"; o="Stanley"; };
  { i=Expr.of_str "mary"; o="Mary"; };
] ~t:(T.arrow T.s T.s) ~name:"IaN -> Ian"

let tasks = [upper_first_task;task02;task02;task03;task04;task06;task08;task10;task13;task15;task16;task17;task18;task19;task21;task29;task29;task30;task31;another_task]
let main () =
  ec combs tasks 16
  ~lambda:1.5
  ~smoothing:1.0
  ~frontier_size:1000
  ~log_prefix:"kn"
;;
main ();;
