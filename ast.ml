(*
 * OCaml implementation of abstract syntax trees of SimProlog programs
 * Please refer to README.md.
 *)

type constant = Const of string                  
type variable = Var of string                 
type term = ConstTerm of constant 
          | VarTerm of variable 
          | ComplexTerm of constant * term list
type command = Rule of term * term list | Inquery of term
