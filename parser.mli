type token =
  | LWORD of (string)
  | UWORD of (string)
  | COMMA
  | CDASH
  | QDASH
  | LPAREN
  | RPAREN
  | DOT
  | SHOW
  | CLEAR

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Main.command