(* This is the core of simprolog's unification process *)

type word = Const of string | Var of string
type pred = string * word list
type rule = pred * pred list
type subst = (string * word) list

let fst = fun (x, y) -> x;;
let snd = fun (x, y) -> y;;
let hd = fun (l:'a list) -> match l with [] -> raise (Failure "List is empty.") | x::xs -> x
let tl = fun (l:'a list) -> match l with [] -> raise (Failure "List is empty.") | x::xs -> xs


let (fresh, reset) = 
  let nxt = ref 0 in
  let f () = (let r = Var("$" ^ string_of_int !nxt) in let _ = nxt := !nxt + 1 in r) in
  let r () = nxt := 0 in
  (f, r)
;;

let rec word_lift_subst (s: subst) (w: word) = 
  match w with Const _ -> w | Var v -> (match s with [] -> w | p::ps ->
  if v = fst p then snd p
  else word_lift_subst ps w)
;;

let rec wordlist_lift_subst (s: subst) (wl: word list) = 
  List.map (word_lift_subst s) wl
;;

let pred_lift_subst (s: subst) (p: pred) =
  let (f, ws) = p in
  (f, List.map (word_lift_subst s) ws)
;;

let instantiate (p: pred) =
  let rec freshlist wordlist = 
    match wordlist with [] -> [] | w::ws ->
    (match w with Const _ -> w :: freshlist ws | Var v ->
    if String.contains v '$' then w :: freshlist ws
    else let freshVar = fresh () in
    freshVar :: freshlist (List.map (word_lift_subst [(v, freshVar)]) ws)) in
  (fst p, freshlist (snd p))
;;


let rec unify_wordlist (wl1: word list) (wl2: word list) =
  if List.length wl1 <> List.length wl2 then None else if wl1 = [] then Some [] else
  let (w, ws, v, vs) = (hd wl1, tl wl1, hd wl2, tl wl2) in
  match (w, v) with
    (Const a, Const b) -> if a = b then unify_wordlist ws vs else None
  | (Const a, Var b) -> let s = [(b, Const a)] in
                        (match unify_wordlist (wordlist_lift_subst s ws) (wordlist_lift_subst s vs) with
                        None -> None | Some phi -> Some (s @ phi))
  | (Var a, Const b) -> let s = [(a, Const b)] in 
                        (match unify_wordlist (wordlist_lift_subst s ws) (wordlist_lift_subst s vs) with
                        None -> None | Some phi -> Some (s @ phi))
  | (Var a, Var b) -> let s = [(b, Var a)] in
                      (match unify_wordlist (wordlist_lift_subst s ws) (wordlist_lift_subst s vs) with
                      None -> None | Some phi -> Some (s @ phi))
;;

let unify_pred (p: pred) (q: pred) =
  if fst p <> fst q then None else unify_wordlist (snd p) (snd q)
;;



let s = [("X", Const "10"); ("Y", Const "23"); ("Z", Var "W")];;
let p = ("foo", [Var "X"; Var "Y"; Const "fun"; Const "happy"]);;
let q = ("foo", [Const "hey"; Var "F"; Const "fun"; Const "H"]);;
pred_lift_subst s p;;
