(* Unification *)

open Ast

(* Define type substitution *)

type substitution = (variable * term) list

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
