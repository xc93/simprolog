(*
 * OCaml implementation of abstract syntax trees of SimProlog programs
 *)

type fact = SimpleFact of string
          | ComplexFact of string * string list
;;

type word = Const of string | Var of string
;;

type predicate = string * word list
;;

type rule = predicate * predicate * predicate list
;;

type inquery = SimpleInq of fact
             | ComplexInq of predicate








