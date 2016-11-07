(*
 * OCaml implementation of abstract syntax trees of SimProlog programs
 * Please refer to README.md.
 *)

(* You are safe to assume that all list contain at least one element *)

type fact = SimpleFact of string
          | CompoundFact of string * string list
;;

(* ConstWord starts with lower case while VarWord starts with upper case *)

type word = ConstWord of string | VarWord of string
;;

(* You might find it is more convenient if we regard facts as predicates *)
(* If that is the case, feel free to not using facts *)
(* I personally find it might be more easy if we regard facts as predicates *)

type predicate = string * word list
;;

(* Also, you might find it is more natural if we regard facts as rules *)
(* The unification process is the same anyway *)

type rule = predicate * predicate list
;;

type inquery = SimpleInq of fact
             | QuantifiedInq of predicate


(* This type corresponds to each statement (line of code) in SimProlog. 
 * They can be either a fact statement, a rule statement, or an inquery. *)

type dec =
  | FactStatement of fact
  | RuleStatement of rule
  | InqueryStatement of inquery
