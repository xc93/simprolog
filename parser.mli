type token =
  | LWORD of (string)
  | UWORD of (string)
  | EOF
  | COMMA
  | CDASH
  | QDASH
  | LPAREN
  | RPAREN
  | DOT

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.command
