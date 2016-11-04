# simprolog
Implementation of a simple version of Prolog in OCaml. This project includes a grammar, lexer, parser, semantics, evaluator (including back-tracking).


The simple version that is considered in this OCaml implementation is called SimProlog. 

The basic building blocks in SimProlog contains:

Facts
  simple facts: party. maryIsHappy.
  compound facts: eats(fred, oranges). likes(john, mary). age(ian, 2).
Rules
  mortal(X) :- human(X).
  fun(X) :- red(X), car(X).
  fun(X) :- blue(X), bike(X).
  fun(ice_cream).
