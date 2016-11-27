{ open Parser  (* The type token is defined in parser.mli *)
  exception Eof
}

let digit       = ['0'-'9']
let lcase       = ['a'-'z']
let ucase       = ['A'-'Z']
let wchar       = digit | lcase | ucase | '_'
let lword       = lcase wchar*
let uword       = ucase wchar*
let space       = [' ' '\t' '\n']

rule token = parse
  | space         { token lexbuf } (* skip over whitespace *)
  | ":-"          { CDASH }
  | "?-"          { QDASH }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '.'           { DOT }
  | ','           { COMMA }
  | "clear!"      { CLEAR }
  | "show!"       { SHOW }
  | lword as w    { LWORD w }
  | uword as w    { UWORD w }
  | eof		  { raise Eof }
