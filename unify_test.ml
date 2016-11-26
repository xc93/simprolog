(* unify.ml test file *)
(* unify{ foo(X, Y)  = foo(fun(a, Y), fun(b, c)),
          fun(fun(Y)) = Z }
 *)

open Unify

let left1 = ComplexTerm(Const "foo", [VarTerm (Var "X"); VarTerm (Var "Y")]);;
let right1 = ComplexTerm(Const "foo",
               [ComplexTerm(Const "fun", [ConstTerm (Const "a"); VarTerm (Var "Y")]);
                ComplexTerm(Const "fun", [ConstTerm (Const "b"); ConstTerm (Const "c")])]);;
let left2 = ComplexTerm(Const "fun", [ComplexTerm(Const "fun", [VarTerm (Var "Y")])]);;
let right2 = VarTerm (Var "Z");;


