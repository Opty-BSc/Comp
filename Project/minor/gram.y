%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"

int yylex();
int yyerror(char *s);
%}

%union {
    int i;      /* Integer */
    char c;     /* Character */
    char *s;    /* Symbol or Text Chain */
    Node *n;    /* Node Pointer */
};

%right ASSIGN
%left '|'
%left '&'
%nonassoc '~'
%left NE '='
%left '<' '>' LE GE
%left '+' '-'
%left '*' '/' '%'
%right '^'
%nonassoc ADDR UMINUS '?'
%nonassoc '(' ')' '[' ']'

%token <i> INT
%token <c> CHAR
%token <s> ID STR
%token PROGRAM MODULE END START
%token VOID CONST NUMBER ARRAY STRING FUNCTION PUBLIC FORWARD
%token IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN
%%

file        : program
            | module
            ;

program     : PROGRAM optdeclseq START body END
            ;

module      : MODULE optdeclseq END
            ;

optdeclseq  :
            | declseq
            ;

declseq     : decl
            | declseq ';' decl
            ;

decl        : function
            | qualifier optconst variable optassign
            ;

qualifier   :
            | PUBLIC
            | FORWARD
            ;

variable    : type ID optarray
            ;

type        : NUMBER
            | ARRAY
            | STRING
            ;

optarray    :
            | '[' INT ']'
            ;

optconst    :
            | CONST
            ;

optassign   :
            | ASSIGN literalseq
            ;

literalseq  : literal
            | literalseq ',' literal
            ;

literal     : STR chain
            | INT chain
            | CHAR chain
            ;

chain       :
            | chain STR
            | chain INT
            | chain CHAR
            ;

function    : FUNCTION qualifier functype ID funcargs funcbody
            ;

functype    : type
            | VOID
            ;

funcargs    :
            | varseqlist
            ;

varseqlist  : variable
            | varseqlist ';' variable
            ;

funcbody    : DONE
            | DO body
            ;

body        : varseq instrblock
            ;

varseq      :
            | varseq variable ';'
            ;

instr       : IF rvalue THEN instrblock instrelif instrelse FI
            | FOR rvalue UNTIL rvalue STEP rvalue DO instrblock DONE
            | rvalue rsugar
            | lvalue '#' rvalue ';'
            ;

instrelif   :
            | ELIF rvalue THEN instrblock
            ;

instrelse   :
            | ELSE instrblock
            ;

rsugar      : ';'
            | '!'
            ;

endinstr    :
            | REPEAT
            | STOP
            | RETURN optrvalue
            ;

instrblock  : instrseq endinstr
            ;

instrseq    :
            | instrseq instr
            ;

optrvalue   :
            | rvalue
            ;

lvalue      : ID
            | lvalue '[' rvalue ']'
            ;

rvalue      : lvalue
            | literal
            | '(' rvalue ')'
            | rvalue '(' rargs ')'
            | '?'
            | '&' lvalue %prec ADDR
            | '-' rvalue %prec UMINUS
            | rvalue '^' rvalue
            | rvalue '*' rvalue
            | rvalue '/' rvalue
            | rvalue '%' rvalue
            | rvalue '+' rvalue
            | rvalue '-' rvalue
	        | rvalue '<' rvalue
            | rvalue '>' rvalue
            | rvalue LE rvalue
            | rvalue GE rvalue
            | rvalue NE rvalue
            | rvalue '=' rvalue
            | '~' rvalue
            | rvalue '&' rvalue
            | rvalue '|' rvalue
            | lvalue ASSIGN rvalue
            ;

rargs       : rvalue
            | rargs ',' rvalue
            ;

%%
char **yynames =
#if YYDEBUG > 0
    (char **)yyname;
#else
    NULL;
#endif
