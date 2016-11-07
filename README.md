# simprolog
Implementation of a simple version of Prolog in OCaml. This project includes a grammar, lexer, parser, semantics, evaluator (including back-tracking).


The simple version that is considered in this OCaml implementation is called SimProlog. 

The basic building blocks in SimProlog contains:

Facts
  party 
  maryIsHappy
  eats(fred, oranges) 
  likes(john, mary) 
  age(ian)

Rules
  mortal(X) :- human(X)
  fun(X) :- red(X), car(X)
  fun(X) :- blue(X), bike(X)
  fun(X) :- blue(X), bike(it)
  fun(X, c) :- blue(X, c), bike(c), next(d)

// We might want to unify rules and facts.

Inqueries and execution samples

?- party.

> yes.

?- eats(fred, oranges).

> yes.

?- eats(fred, What).

> with What = oranges
> yes.

?- likes(Who, Whom).

> with Who = john, Whom = mary
> yes.

?- likes(mary, Who).

> no.

// The following shows how unification works in SimProlog.

// Let's first define some facts and rules.

true                                // define true to be a fact, so that true is always true
leq(a, b).                          // a <= b
leq(b, c).                          // b <= c
leq(c, d).                          // c <= d
leq(X, X) :- true.                  // X <= X if true
leq(X, Y) :- leq(X, Z), leq(Z, Y).  // X <= Y if X <= Z and Z <= Y

?- leq(a, d).

// Here we show how the unification works.
// As the unification goes, we construct a proof tree
// just as what we do in type inference.

solving leq(a, d) ...

unifying leq(a, d) = true ...
failed.

unifying leq(a, d) = leq(a, b) ...
unifying a = a, d = b ...
unifying d = b ...
failed.

...

// Note how we change names for variables in rules.
// The reason that we do so will be clearer later.
unifying leq(a, d) = leq(X$1, X$1)
failed.

unifying leq(a, d) = leq(X$1, Y$1)
successed with X$1 -> a, Y$1 -> d

proof tree
  subst X$1 -> a, Y$1 -> d
  leq(a, d) holds by rule
  leq(X$1, Y$1) :- leq(X$1, Z$1), leq(Z$1, Y$1)
    leq(a, Z$1) 
    leq(Z$1, d)

Note that two sub-inqueries are inserted in the tree. And we recursively
finds proof for them. While doing so, the tree gets bigger and bigger, until
we finish the proof or fail to prove.

In the following we show how proof tree is built when we process unification
and substitution.

proof tree:
  subst X$1 -> a, Y$1 -> d, Z$1 -> b
  leq(a, d) holds by rule
  leq(X$1, Y$1) :- leq(X$1, Z$1), leq(Z$1, Y$1)
    leq(a, Z$1) holds by fact
      leq(a, b)    
    leq(Z$1, c)


