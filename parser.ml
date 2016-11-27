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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Main 
# 18 "parser.ml"
let yytransl_const = [|
  259 (* COMMA *);
  260 (* CDASH *);
  261 (* QDASH *);
  262 (* LPAREN *);
  263 (* RPAREN *);
  264 (* DOT *);
  265 (* SHOW *);
  266 (* CLEAR *);
    0|]

let yytransl_block = [|
  257 (* LWORD *);
  258 (* UWORD *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\001\000\002\000\002\000\003\000\004\000\
\004\000\004\000\004\000\005\000\005\000\006\000\007\000\000\000"

let yylen = "\002\000\
\002\000\002\000\001\000\001\000\001\000\003\000\002\000\001\000\
\001\000\003\000\004\000\001\000\003\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\014\000\015\000\000\000\004\000\003\000\016\000\
\000\000\000\000\000\000\000\000\009\000\007\000\001\000\002\000\
\000\000\000\000\000\000\006\000\010\000\000\000\000\000\011\000\
\013\000"

let yydgoto = "\002\000\
\008\000\009\000\010\000\019\000\020\000\012\000\013\000"

let yysindex = "\002\000\
\255\254\000\000\000\000\000\000\017\255\000\000\000\000\000\000\
\007\255\014\255\019\255\018\255\000\000\000\000\000\000\000\000\
\017\255\004\255\022\255\000\000\000\000\020\255\017\255\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\021\255\009\255\000\000\000\000\000\000\000\000\
\000\000\000\000\013\255\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\009\000\240\255\000\000\000\000"

let yytablesize = 29
let yytable = "\003\000\
\004\000\022\000\001\000\005\000\003\000\004\000\025\000\006\000\
\007\000\011\000\021\000\008\000\008\000\014\000\015\000\008\000\
\008\000\003\000\004\000\012\000\012\000\016\000\017\000\018\000\
\023\000\000\000\024\000\000\000\005\000"

let yycheck = "\001\001\
\002\001\018\000\001\000\005\001\001\001\002\001\023\000\009\001\
\010\001\001\000\007\001\003\001\004\001\005\000\008\001\007\001\
\008\001\001\001\002\001\007\001\008\001\008\001\004\001\006\001\
\003\001\255\255\007\001\255\255\008\001"

let yynames_const = "\
  COMMA\000\
  CDASH\000\
  QDASH\000\
  LPAREN\000\
  RPAREN\000\
  DOT\000\
  SHOW\000\
  CLEAR\000\
  "

let yynames_block = "\
  LWORD\000\
  UWORD\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rule) in
    Obj.repr(
# 12 "parser.mly"
                                     ( _1 )
# 103 "parser.ml"
               : Main.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'inquery) in
    Obj.repr(
# 13 "parser.mly"
                                     ( _1 )
# 110 "parser.ml"
               : Main.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 14 "parser.mly"
                  ( ClearComm )
# 116 "parser.ml"
               : Main.command))
; (fun __caml_parser_env ->
    Obj.repr(
# 15 "parser.mly"
                 ( ShowComm )
# 122 "parser.ml"
               : Main.command))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 18 "parser.mly"
                                     ( Rule(_1, []) )
# 129 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 19 "parser.mly"
                                     ( Rule(_1, _3) )
# 137 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 22 "parser.mly"
                                     ( Inquery _2 )
# 144 "parser.ml"
               : 'inquery))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'lword) in
    Obj.repr(
# 25 "parser.mly"
                                     ( ConstTerm _1 )
# 151 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'uword) in
    Obj.repr(
# 26 "parser.mly"
                                     ( VarTerm _1 )
# 158 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'lword) in
    Obj.repr(
# 27 "parser.mly"
                                     ( ComplexTerm(_1, []) )
# 165 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'lword) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'term_list) in
    Obj.repr(
# 28 "parser.mly"
                                     ( ComplexTerm(_1, _3) )
# 173 "parser.ml"
               : 'term))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'term) in
    Obj.repr(
# 31 "parser.mly"
                                     ( [_1] )
# 180 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'term) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'term_list) in
    Obj.repr(
# 32 "parser.mly"
                                     ( _1::_3 )
# 188 "parser.ml"
               : 'term_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 35 "parser.mly"
                                     ( Const _1 )
# 195 "parser.ml"
               : 'lword))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 38 "parser.mly"
                                     ( Var _1 )
# 202 "parser.ml"
               : 'uword))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Main.command)
