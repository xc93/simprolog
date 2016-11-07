%{
  open Ast;;
%}

%token <string> LWORD
%token <string> UWORD

%token EOF COMMA CDASH QDASH LPAREN RPAREN DOT

%start main
%type <Ast.dec> main
%%

main:
  | fact DOT
    { FactStatement $1 }
  | rule DOT
    { RuleStatement $1 }
  | inquery DOT
    { InqueryStatement $1 }


fact:
  | lword                             { SimpleFact $1 }
  | lword LPAREN lword_list RPAREN    { CompoundFact($1, $3) }








lword_list:
  | lword                        { [ $1 ] }
  | lword COMMA lword_list       { $1 :: $3 }

lword:
  | LWORD { ConstWord $1 }
uword: 
  | UWORD { Varword $1 }
