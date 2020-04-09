%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#define A_TYPE 0
#define I_TYPE 1
#define S_TYPE 2
#define V_TYPE 3
#define VARnew(v,c) IDnew(v->info+c*3, LEFT_CHILD(v)->value.s, 0)

int yylex();
int yyerror(char *s);

int nblck = 0;
int ncicl = 0;
int retType = I_TYPE;
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

%type <n> program module dSEQOPT dSEQ declaration
%type <n> variable vDimOPT vInitOPT literal literals integers
%type <n> function fArgsOPT fArgs fBody body vSEQ
%type <n> iBlock iSEQ instruction iElifSEQ iElif iElse iSugar iLast
%type <n> rValueOPT lValue rValue rArgs
%type <i> qualifier constant type fType

%token NIL DECL DECLS VAR VARS DIM INIT LITERALS INTS
%token CONDITION ELIFS ELSES INSTRS BLOCK EXPR
%token BODY FUNCTION_ARGS FUNCTION_ID LOAD CALL INDEX EXPR_ARGS
%%
file        : program     { printNode($1, 0, yynames); }
            | module      { printNode($1, 0, yynames); }
            ;

program     : PROGRAM dSEQOPT START body END    { $$ = binNode(PROGRAM, $2, $4); }
            ;

module      : MODULE dSEQOPT END    { $$ = uniNode(MODULE, $2); }
            ;

dSEQOPT     :                       { $$ = nilNode(NIL); }
            | dSEQ                  { $$ = $1; }
            ;

dSEQ        : declaration           { $$ = binNode(DECLS, $1, nilNode(NIL)); }
            | dSEQ ';' declaration  { $$ = binNode(DECLS, $3, $1); }
            ;

declaration : function                              { $$ = $1; }
            | qualifier constant variable vInitOPT  { VARnew($3, $2); $$ = binNode(DECL, $3, $4);}
            ;

qualifier   :                     { $$ = 0; }
            | PUBLIC              { $$ = 1; }
            | FORWARD             { $$ = 2; }
            ;

constant    :                     { $$ = 0; }
            | CONST               { $$ = 1; }
            ;

variable    : type ID vDimOPT     { $$ = binNode(VAR, strNode(ID, $2), $3); $$->info = $1; }
            ;

type        : ARRAY               { $$ = A_TYPE; }
            | NUMBER              { $$ = I_TYPE; }
            | STRING              { $$ = S_TYPE; }
            ;

vDimOPT     :                     { $$ = nilNode(NIL); }
            | '[' INT ']'         { $$ = intNode(DIM, $2); }
            ;

vInitOPT    :                     { $$ = nilNode(NIL); }
            | ASSIGN literal      { $$ = uniNode(INIT, $2); }
            | ASSIGN literals     { $$ = uniNode(INIT, $2); }
            | ASSIGN integers     { $$ = uniNode(INIT, $2); }
            ;

literal     : STR                 { $$ = strNode(STR, $1); $$->info = S_TYPE; }
            | INT                 { $$ = intNode(INT, $1); $$->info = I_TYPE; }
            | CHAR                { $$ = intNode(CHAR, $1); $$->info = I_TYPE; }
            ;

literals    : literal literal     { $$ = binNode(LITERALS, $2, binNode(LITERALS, $1, nilNode(NIL)));
                                    RIGHT_CHILD($$)->info = $$->info = S_TYPE; }
            | literals literal    { $$ = binNode(LITERALS, $1, $2); $$->info = S_TYPE; }
            ;

integers    : INT ',' INT         { $$ = binNode(INTS, intNode(INT, $3), binNode(INTS, intNode(INT, $1), nilNode(NIL)));
                                    RIGHT_CHILD($$)->info = $$->info = A_TYPE; }
            | integers ',' INT    { $$ = binNode(INTS, intNode(INT, $3), $1); $$->info = A_TYPE; }
            ;

function    : FUNCTION qualifier fType ID   { IDnew($3+6, $4, 0); retType = $3; IDpush(); }
              fArgsOPT fBody                { IDpop(); retType = I_TYPE; $$ = binNode(FUNCTION, binNode(FUNCTION_ID, strNode(ID, $4), $6), $7); }
            ;

fType       : type                { $$ = $1; }
            | VOID                { $$ = V_TYPE; }
            ;

fArgsOPT    :                     { $$ = nilNode(NIL); }
            | fArgs               { $$ = $1; }
            ;

fArgs       : variable            { VARnew($1, 0); $$ = binNode(FUNCTION_ARGS, $1, nilNode(NIL)); }
            | fArgs ';' variable  { VARnew($3, 0); $$ = binNode(FUNCTION_ARGS, $3, $1); }
            ;

fBody       : DONE                { $$ = nilNode(DONE); }
            | DO body             { $$ = uniNode(DO, $2); }
            ;

body        : vSEQ iBlock         { $$ = binNode(BODY, $1, $2); }
            ;

vSEQ        :                     { $$ = nilNode(NIL); }
            | vSEQ variable ';'   { VARnew($2, 0); $$ = binNode(VARS, $2, $1); }
            ;

iBlock      : { IDpush(); nblck++; } iSEQ iLast { nblck--; IDpop(); $$ = binNode(BLOCK, $2, $3); }
            ;

iSEQ        :                     { $$ = nilNode(NIL); }
            | iSEQ instruction    { $$ = binNode(INSTRS, $2, $1); }
            ;

instruction : rValue iSugar                             { $$ = binNode(EXPR, $1, $2); }
            | lValue '#' rValue ';'                     { $$ = binNode('#', $1, $3); }
            | IF rValue THEN iBlock iElifSEQ iElse FI   { $$ = binNode(CONDITION, binNode(IF, $2, uniNode(THEN, $4)), binNode(ELSES, $5, $6)); }
            | FOR rValue UNTIL rValue STEP rValue
              DO { ncicl++; } iBlock { ncicl--; } DONE  { $$ = binNode(FOR, $2, binNode(UNTIL, $4, binNode(STEP, $6, uniNode(DO, $9)))); }
            ;

iElifSEQ    :                           { $$ = nilNode(NIL); }
            | iElifSEQ iElif            { $$ = binNode(ELIFS, $2, $1); }
            ;

iElif       : ELIF rValue THEN iBlock   { $$ = binNode(ELIF, $2, uniNode(THEN, $4)); }
            ;

iElse       :                           { $$ = nilNode(NIL); }
            | ELSE iBlock               { $$ = uniNode(ELSE, $2); }
            ;

iSugar      : ';'                       { $$ = nilNode(';'); }
            | '!'                       { $$ = nilNode('!'); }
            ;

iLast       :                           { $$ = nilNode(NIL); }
            | REPEAT                    { if (!ncicl) yyerror("REPEAT: Outside of a Cicle"); $$ = nilNode(REPEAT); }
            | STOP                      { if (!ncicl) yyerror("STOP: Outside of a Cicle"); $$ = nilNode(STOP); }
            | RETURN rValueOPT          { if (!nblck) yyerror("RETURN: On Main Block");
                                          else if (!checkType(retType, $2->info)) yyerror("RETURN: Invalid Type");
                                          $$ = uniNode(RETURN, $2); }
            ;

rValueOPT   :                           { $$ = nilNode(NIL); $$->info = V_TYPE; }
            | rValue                    { $$ = $1; }
            ;

lValue      : ID                        { $$ = IDNode($1); }
            | ID '[' rValue ']'         { $$ = IDidx($1, $3); }
            ;

rValue      : lValue                    { $$ = uniNode(LOAD, $1); $$->info = $1->info; }
            | literal                   { $$ = $1; }
            | literals                  { $$ = $1; }
            | '(' rValue ')'            { $$ = $2; }
            | rValue '(' rArgs ')'      { $$ = binNode(CALL, $1, $3); $$->info = $1->info; }
            | '?'                       { $$ = nilNode('?'); $$->info = I_TYPE; }
            | '&' lValue %prec ADDR     { $$ = uniNode(ADDR, $2); $$->info = $2->info; }
            | '-' rValue %prec UMINUS   { $$ = uniNode(UMINUS, $2); $$->info = $2->info; }
            | rValue '^' rValue         { $$ = binNode('^', $1, $3); $$->info = I_TYPE; }
            | rValue '*' rValue         { $$ = binNode('*', $1, $3); $$->info = I_TYPE; }
            | rValue '/' rValue         { $$ = binNode('/', $1, $3); $$->info = I_TYPE; }
            | rValue '%' rValue         { $$ = binNode('%', $1, $3); $$->info = I_TYPE; }
            | rValue '+' rValue         { $$ = binNode('+', $1, $3); $$->info = I_TYPE; }
            | rValue '-' rValue         { $$ = binNode('-', $1, $3); $$->info = I_TYPE; }
            | rValue '<' rValue         { $$ = binNode('<', $1, $3); $$->info = I_TYPE; }
            | rValue '>' rValue         { $$ = binNode('>', $1, $3); $$->info = I_TYPE; }
            | rValue LE rValue          { $$ = binNode(LE, $1, $3); $$->info = I_TYPE; }
            | rValue GE rValue          { $$ = binNode(GE, $1, $3); $$->info = I_TYPE; }
            | rValue NE rValue          { $$ = binNode(NE, $1, $3); $$->info = I_TYPE; }
            | rValue '=' rValue         { $$ = binNode('=', $1, $3); $$->info = I_TYPE; }
            | '~' rValue                { $$ = uniNode('~', $2); $$->info = $2->info; }
            | rValue '&' rValue         { $$ = binNode('&', $1, $3); $$->info = I_TYPE; }
            | rValue '|' rValue         { $$ = binNode('|', $1, $3); $$->info = I_TYPE; }
            | lValue ASSIGN rValue      { $$ = binNode(ASSIGN, $1, $3); $$->info = $3->info; }
            ;

rArgs       : rValue                    { $$ = binNode(EXPR_ARGS, $1, nilNode(NIL)); }
            | rArgs ',' rValue          { $$ = binNode(EXPR_ARGS, $3, $1); }
            ;
%%
char **yynames =
#if YYDEBUG > 0
  (char **)yyname;
#else
  NULL;
#endif

int checkType(int gen, int spc) {

  if (gen > 8 || spc > 8) return gen == spc;

  return ((gen == A_TYPE) && (spc == I_TYPE)) || ((gen % 3) == (spc % 3));
}

Node *IDNode(char *id) {

  int typ = IDfind(id, NULL);
  if (typ < 0) yyerror("ID is Undefined\n");

  Node *s = strNode(ID, id);
  s->info = typ;
  return s;
}

Node *IDidx(char *id, Node *expr) {

  if (!checkType(expr->info, I_TYPE)) yyerror("INDEX: Expression Type != Number");
  int typ = IDfind(id, NULL);
  if (typ < 0) yyerror("INDEX: ID is Undefined");
  if (typ > 5) yyerror("INDEX: ID is a Function");
  if (!checkType(typ, A_TYPE) && !checkType(typ, S_TYPE)) yyerror("INDEX: Variable can not be Indexed");

  Node *s = strNode(ID, id);
  s->info = A_TYPE;
  Node *b = binNode(INDEX, s, expr);
  b->info = I_TYPE;
  return b;
}
