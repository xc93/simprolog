(* This is a general unification algorithm *)

type word      = Const of string | Var of string

type predicate = string * word list

type substitution = (word * predicate) list
