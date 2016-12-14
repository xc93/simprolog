open Pervasives
open List

(* Abstract Data Structures *)
type constant = Const of string                  
type variable = Var of string                 
type term = ConstTerm of constant 
          | VarTerm of variable 
          | ComplexTerm of constant * term list
type rule = term * term list
type command = Rule of term * term list | Inquery of term | ClearComm | ShowComm

(* Unification and Substitution *)
type substitution = (variable * term) list
let identity_subst : substitution = []

(* helper functions *)
let rec occur (varname: string) (t: term) : bool =
  match t with
| ConstTerm _ -> false
| VarTerm (Var s) -> varname = s
| ComplexTerm (Const s, ts) -> exists (occur varname) ts
;;

let rec subst_fun (subst: substitution) (varname: string) = 
  match subst with 
| [] -> 
    VarTerm (Var varname)
| (Var varname', t) :: substs -> 
    if varname' = varname then t else subst_fun substs varname
;;

let rec lift_subst_term (subst: substitution) (t: term) = 
  match t with 
| ConstTerm (Const s) -> t
| VarTerm (Var varname) -> subst_fun subst varname
| ComplexTerm (Const s, ts) -> 
    ComplexTerm (Const s, map (lift_subst_term subst) ts)
;;

let lift_subst_term_list subst (ts:term list) = map (lift_subst_term subst) ts;;

(* Cited from MPs and MLs. *)
let subst_compose (s2: substitution) (s1: substitution) : substitution =
  (filter (fun (tv,_) -> not(mem_assoc tv s1)) s2) @ 
  (map (fun (tv,residue) -> (tv, lift_subst_term s2 residue)) s1)

(* Pick the subset of substitution @s that contains variables in @vars. *)
let rec pick_subst (s: substitution) (vars: variable list) : substitution =
  match s with
| [] -> []
| (v, t) :: rem -> 
    if mem v vars then (v, t) :: pick_subst rem vars
                  else pick_subst rem vars

(* Unification *)
let rec unify (eqlst: (term * term) list) =
  let rec addNewEqs ls1 ls2 acc =
    match ls1, ls2 with
  | [], [] -> Some acc
  | (t1 :: tl1), (t2 :: tl2) -> addNewEqs tl1 tl2 ((t1, t2) :: acc)
  | _ -> None
  in match eqlst with
   | [] -> Some([])
   (*Delet*)
   | (s,t) :: eqs -> if s = t then unify eqs
   (*Decompose*)
  else (match (s, t) with 
     | (ComplexTerm ((Const c1), t1), ComplexTerm ((Const c2), t2)) ->
          if c1 = c2 then (match (addNewEqs t1 t2 eqs) with 
                         | None -> None 
                         | Some l -> unify l)
          else None
     (*Orient*)
     | (ComplexTerm (c, t), VarTerm (Var v)) -> 
          unify ((VarTerm (Var v), ComplexTerm (c, t)) :: eqs)
     (*Oirent*)
     | (ConstTerm (Const s), VarTerm (Var v)) -> 
          unify ((VarTerm (Var v), ConstTerm (Const s)) :: eqs)
     (*Eliminate*)
     | (VarTerm (Var v), t) -> if (occur v t) then None
          else let eqs' = 
            map (fun (t1, t2) -> 
                  (lift_subst_term [(Var v, t)] t1, lift_subst_term [(Var v, t)] t2))
                eqs
               in (match (unify eqs') with 
                   | None -> None 
                   | Some phi -> 
                       Some (subst_compose [(Var v, lift_subst_term phi t)] phi))
      | _ -> None)
;;


(* Backtracking *)

(* generating fresh variables and rules *)

let (fresh, reset) = 
  let nxt = ref 0 in
  let f () = (let r = Var("$" ^ string_of_int !nxt) in let _ = nxt := !nxt + 1 in r) in
  let r () = nxt := 0 in
  (f, r)

(* de-duplicate a list *)
let rec ddup l =
  match l with
| [] -> []
|  x::xs -> let dxs = ddup xs in
   if mem x dxs then dxs else x::dxs

let rec collect_variables_in_term (t: term) : variable list=
  match t
  with  ConstTerm _ -> []
     |  VarTerm v -> [v]
     |  ComplexTerm(c, ts) -> collect_variables_in_term_list (ts: term list)
and collect_variables_in_term_list (ts: term list) : variable list =
  match ts
  with  [] -> []
     |  t::ts -> ddup ((collect_variables_in_term t) @
                       (collect_variables_in_term_list ts))

(* generating a fresh instance of an implicit quantified rule by
   replacing all variables by fresh variables *)
(* this prevents incorrect variable capturing *)
let rec fresh_rule (t,ts) : rule =
  let bound_variables = collect_variables_in_term_list (t::ts) in
  let subst = map (fun (v:variable) -> (v, VarTerm(fresh()))) 
                  bound_variables in
  (lift_subst_term subst t, lift_subst_term_list subst ts)


(* find all solutions for a list of inquery @qs using backtracking.
 * @rs_togo is the list of rules that haven't been tried to resolve
 * the first inquery. @s is the substitution on-the-fly so fat and
 * @sols contains all found solutions.
 * @k is the continuation: yes, I use CPS style.
 *)
let rec solve (qs: term list)
              (rs: rule list) 
              (rs_togo: rule list) 
              (s: substitution) 
              (sols: substitution list) 
              (k: substitution list -> 'a) =
  match qs with
| [] -> k(s::sols) (* no more question to solve: done and @s is the last solution. *)
| q1::remq -> (match rs_togo with
  | [] -> k(sols)  (* no more rules togo: done and abandon @s. *)
  | r1::remr -> let fresh_r = fresh_rule r1 in
                (match unify [(q1, fst fresh_r)] with
    | None -> solve qs rs remr s sols k     (* cannot use the first rule_togo: try the rest rules_to_go. *)
    | Some sigma -> solve (lift_subst_term_list sigma (remq @ (snd fresh_r)))   (* solve the new sub-problems, and after that ... *)
                          rs
                          rs 
                          (subst_compose sigma s) 
                          sols 
                          (fun nsols -> solve qs rs remr s nsols k)))           (* collect solutions and continue solving the current problem *)

(* Prettyprinting *)
(* printy-print function for term *)



let rec pp t = match t with
  ConstTerm (Const s) -> print_string s
| VarTerm (Var v) -> print_string v
| ComplexTerm(Const f, ts) -> print_string f; print_string "("; pp_lst ts; print_string ")"
and pp_lst ts = match ts with
  [] -> print_string ""
| [t] -> pp t
| t::ts -> pp t; print_string ", "; pp_lst ts

(* print a list *)
let rec print_list lst print_ele (separator : string) =
  match lst
  with  [] -> print_string ""
     |  [x] -> print_ele x
     |  x::xs -> (print_ele x; print_string separator; print_list xs print_ele separator)
;;


(* De-duplicate substitution list *)
let rec is_same_subst subst subst' =
  match subst
  with [] -> (
    match subst'
    with [] -> true
       | _ -> false
  )
     | x :: substs -> if List.exists ((=) x) subst' then is_same_subst substs (snd(List.partition ((=) x) subst')) else false
;;



let rec contain_subst subst l =
  match l
  with [] -> false
     | subst' :: ls -> if is_same_subst subst subst' then true else contain_subst subst ls
;;

let rec dd_subst (sols:substitution list) = 
  match sols
  with [] -> []
     | subst1 :: ls -> if contain_subst subst1 ls then dd_subst ls else subst1 :: (dd_subst ls)
;;


let print_term = pp;;
let print_term_list ts = print_list ts print_term ", "

let print_var (Var s) = print_string s
let print_subst (s:substitution) = print_string "{"; print_list s (fun (v, t) -> print_var v; print_string " -> "; print_term t) ","; print_string "}"

let print_sol (sols:substitution list) = print_list (dd_subst sols) print_subst "\n";;
let print_sols = print_sol;;






(* print a command *)

let print_command comm = 
  match comm
  with  Rule(t, ts) -> print_term t; print_string ":-"; print_term_list ts
     |  Inquery t -> print_term t
     |  ClearComm -> print_string "Clearing command"
     |  ShowComm -> print_string "Showing command"


let print_rule (r:rule) = let (t,ts) = r in
  print_term t; print_string " :- "; print_term_list ts

(* print a rule list *)
let print_rules (rs: rule list) =
  print_list rs print_rule "\n"
;;




  






