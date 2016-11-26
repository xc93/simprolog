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

(* printy-print function for term *)

let rec pp t = match t with
  ConstTerm (Const s) -> print_string s
| VarTerm (Var v) -> print_string v
| ComplexTerm(Const f, ts) -> print_string f; print_string "("; pp_lst ts; print_string ")"
and pp_lst ts = match ts with
  [] -> print_string ""
| [t] -> pp t
| t::ts -> pp t; print_string ", "; pp_lst ts
