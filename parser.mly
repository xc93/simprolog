%{ open Main %}

%token <string> LWORD
%token <string> UWORD
%token COMMA CDASH QDASH LPAREN RPAREN DOT SHOW CLEAR

%start main
%type <Main.command> main
%%

main:
  | rule DOT                         { $1 }
  | inquery DOT                      { $1 }
  | CLEAR 			     { ClearComm }
  | SHOW 			     { ShowComm }

rule:
  | term                             { Rule($1, []) }
  | term CDASH term_list             { Rule($1, $3) }

inquery:
  | QDASH term                       { Inquery $2 }

term:
  | lword                            { ConstTerm $1 }
  | uword                            { VarTerm $1 }
  | lword LPAREN RPAREN              { ComplexTerm($1, []) }
  | lword LPAREN term_list RPAREN    { ComplexTerm($1, $3) }

term_list:
  | term                             { [$1] }
  | term COMMA term_list             { $1::$3 }

lword:
  | LWORD                            { Const $1 }

uword: 
  | UWORD                            { Var $1 }
