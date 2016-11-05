(*
 * This file defines regular expressions for lexing tokens in
 * SimProlog. For a complete definition of tokens, please refer
 * to "token.ml". 
 *)

{
  open Token;;
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
  | eof           { EOF }
  | ":-"          { CDASH }
  | "?-"          { QDASH }
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '.'           { DOT }
  | ','           { COMMA }
  | lword as w    { LWORD w }
  | uword as w    { UWORD w } 

{
let lextest s = token(Lexing.from_string s)
;;

let get_all_tokens s =
  let b = Lexing.from_string (s^"\n") in
  let rec g () =
    match token b
    with  EOF -> []
       |  t   -> t :: g()
  in g ()
;;

let try_get_all_tokens s =
  try (Some(get_all_tokens s), true)
  with Failure "unmatched open comment" -> (None, true)
     | Failure "unmatched closed comment" -> (None, false)
;;

let get_all_tokens_options s =
  let b = Lexing.from_string (s^"\n") in
  let rec g () =
    match (try Some (token b) with _ -> None) with
      Some EOF -> []
    | None -> [None]
    | t -> t :: g()
  in
  g ()
;;
}
