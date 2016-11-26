(*
 * This file provides ocaml data structure for tokens of SimProlog.
 * This file will be used as a intermediate between the lexer and
 * the parser. The lexer needs this file to, when digesting SimProlog
 * programs, build tokens in OCaml. The parser will take use of the
 * results and construct abstract syntax trees from strings of tokens.
 *)

type token =
  | EOF
  | COMMA
  | CDASH               (* :- *)
  | QDASH
  | LPAREN
  | RPAREN
  | DOT
  | LBRAC
  | RBRAC
  | SEMICOLON
  | LWORD of (string)
  | UWORD of (string)

