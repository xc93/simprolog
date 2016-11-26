open Pervasives
open List


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

let print_var (Var s) = print_string s

(* printy-print function for term *)

let rec pp t = match t with
  ConstTerm (Const s) -> print_string s
| VarTerm (Var v) -> print_string v
| ComplexTerm(Const f, ts) -> print_string f; print_string "("; pp_lst ts; print_string ")"
and pp_lst ts = match ts with
  [] -> print_string ""
| [t] -> pp t
| t::ts -> pp t; print_string ", "; pp_lst ts

let print_term = pp;;

(* print a list *)
let rec print_list lst print_ele (separator : string) =
  match lst
  with  [] -> print_string ""
     |  [x] -> print_ele x
     |  x::xs -> (print_ele x; print_string separator; print_list xs print_ele separator)
;;


(* Unification *)

(* Define type substitution *)

type substitution = (variable * term) list
let identity_subst = []

(* Helper Function for Unification *)
let rec occur v t =
	match t
	with ConstTerm _ -> false
	   | VarTerm (Var s) -> v = s
	   | ComplexTerm (Const s, ts) -> List.exists (occur v) ts
;;

let rec subst_fun subst v = 
	match subst
    with [] -> VarTerm (Var v)
       | (Var s, t) :: substs -> if s = v then t else subst_fun substs v
;;

let rec lift_subst subst t = 
	match t
    with ConstTerm (Const s) -> t
       | VarTerm (Var v) -> subst_fun subst v
       | ComplexTerm (Const s, ts) -> ComplexTerm (Const s, List.map (lift_subst subst) ts)
;;

let lift_subst_term subst (t:term) = lift_subst subst t;;
let lift_subst_term_list subst (ts:term list) = map (lift_subst_term subst) ts;;

let subst_compose (s1:substitution) (s2:substitution) : substitution = s1 @ s2

let print_subst (s:substitution) = print_string "{"; print_list s (fun (v, t) -> print_var v; print_string " -> "; print_term t) ","; print_string "}"
  
  

(* Unify *)
let rec unify eqlst =
	let rec addNewEqs ls1 ls2 acc =
		match ls1, ls2
		with [], [] -> Some acc
		   | (t1 :: tl1), (t2 :: tl2) -> addNewEqs tl1 tl2 ((t1, t2) :: acc)
		   | _ -> None
	in
	match eqlst
		with [] -> Some([])
		   | (s,t) :: eqs -> if s = t then unify eqs
				else (match (s, t)
					with (ComplexTerm ((Const c1), t1), ComplexTerm ((Const c2), t2)) -> 
						if c1 = c2 then (match (addNewEqs t1 t2 eqs) with None -> None | Some l -> unify l)
						else None
					   | (ComplexTerm (c, t), VarTerm (Var v)) -> unify ((VarTerm (Var v), ComplexTerm (c, t)) :: eqs)
					   | (ConstTerm (Const s), VarTerm (Var v)) -> unify ((VarTerm (Var v), ConstTerm (Const s)) :: eqs)
					   | (VarTerm (Var v), t) -> if (occur v t) then None
					   		else let eqs' = List.map (fun (t1, t2) -> (lift_subst [(Var v, t)] t1, lift_subst [(Var v, t)] t2)) eqs
					   		in (match (unify eqs') with None -> None | Some phi -> Some ((Var v, lift_subst phi t) :: phi))
					   | _ -> None
			)
;;


(* Backtracking *)

type rule = term * term list

(* generating fresh variables *)

let (fresh, reset) = 
  let nxt = ref 0 in
  let f () = (let r = Var("$" ^ string_of_int !nxt) in let _ = nxt := !nxt + 1 in r) in
  let r () = nxt := 0 in
  (f, r)

(* generating fresh rule instances *)

let rec ddup l =
  match l
  with  [] -> []
     |  x::xs -> let dxs = ddup xs in
        if mem x dxs then dxs else x::dxs

let rec collect_variables_in_term (t: term) :variable list=
  match t
  with  ConstTerm _ -> []
     |  VarTerm v -> [v]
     |  ComplexTerm(c, ts) -> collect_variables_in_term_list (ts: term list)
and collect_variables_in_term_list (ts: term list) :variable list =
  match ts
  with  [] -> []
     |  t::ts -> ddup ((collect_variables_in_term t) @ (collect_variables_in_term_list ts))

let rec fresh_rule (t,ts) :rule =
  let bound_variables = collect_variables_in_term_list (t::ts) in
  let subst = map (fun (v:variable) -> (v, VarTerm(fresh()))) bound_variables in
  (lift_subst_term subst t, lift_subst_term_list subst ts)

let rec solve qs rs rs_togo s sols k =
  match qs
  with  [] -> k(s::sols)
     |  q1::remq -> 
       (match rs_togo
        with  [] -> k(sols)
           |  r1::remr -> let fresh_r = fresh_rule r1 in
             (match unify [(q1, fst fresh_r)]
              with  None -> solve qs rs remr s sols k
                 |  Some sigma -> solve (lift_subst_term_list sigma (remq @ (snd fresh_r))) rs rs (subst_compose sigma s) sols (fun nsols -> solve qs rs remr s nsols k)))

let print_sol (sols:substitution list) = print_list sols print_subst "\n";;

(* test1 *)
(* 
 * true()
 * bleq(a, b)
 * bleq(b, c)
 * bleq(c, d)
 * leq(X, X) :- true()
 * leq(X, Y) :- bleq(X, Z), leq(Z, Y)
 * ?leq(b, What)
 *)
(*
let rule_true : rule = (ConstTerm(Const "true"),[])
let rule_a_leq_b : rule = (ComplexTerm(Const "bleq", [ConstTerm(Const "a"); ConstTerm(Const "b")]), [])
let rule_b_leq_c : rule = (ComplexTerm(Const "bleq", [ConstTerm(Const "b"); ConstTerm(Const "c")]), [])
let rule_c_leq_d : rule = (ComplexTerm(Const "bleq", [ConstTerm(Const "c"); ConstTerm(Const "d")]), [])
let rule_X_leq_X : rule = (ComplexTerm(Const "leq", [VarTerm(Var "X"); VarTerm(Var "X")]), [ConstTerm(Const "true")])
let rule_X_leq_Y : rule = (ComplexTerm(Const "leq", [VarTerm(Var "X"); VarTerm(Var "Y")]), 
 				[ComplexTerm(Const "bleq", [VarTerm(Var "X"); VarTerm(Var "Z")]);
				 ComplexTerm(Const "leq" , [VarTerm(Var "Z"); VarTerm(Var "Y")])])
let rules_leq : rule list = [rule_true; rule_a_leq_b; rule_b_leq_c; rule_c_leq_d; rule_X_leq_X; rule_X_leq_Y]
let quest_leq : term = ComplexTerm(Const "leq", [ConstTerm(Const "b"); VarTerm(Var "What")]);;

let sols = solve [quest_leq] rules_leq rules_leq identity_subst [] (fun sols -> sols);;
*)

(* test2 *)

(* 
 * true()
 * addeq(X, zero, X) :- true()
 * addeq(X, s(Y), Z) :- addeq(s(X), Y, Z)
 *)

let rule_true : rule = (ConstTerm(Const "true"),[])
let rule_base : rule = (ComplexTerm(Const "addeq", [VarTerm(Var "X"); ConstTerm(Const "zero"); VarTerm(Var "X")]), [ConstTerm(Const "true")])
let rule_indu : rule = (ComplexTerm(Const "addeq", [VarTerm(Var "X"); ComplexTerm(Const "s", [VarTerm(Var "Y")]); VarTerm(Var "Z")]),
                        [ComplexTerm(Const "addeq", [ComplexTerm(Const "s", [VarTerm(Var "X")]); VarTerm(Var "Y"); VarTerm(Var "Z")])]);;

let rules = [rule_true; rule_base; rule_indu];;

let quest : term = ComplexTerm(Const "addeq", [ComplexTerm(Const "s", [ConstTerm(Const "zero")]);
					       ComplexTerm(Const "s", [ConstTerm(Const "zero")]);
                                               VarTerm(Var "What")])
let sols = solve [quest] rules rules identity_subst [] (fun sols -> sols);;


  






