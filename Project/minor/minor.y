%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#define YYDEBUG 1
/*
0 ARRAY
1 INT
2 STR
3 VOID
4 CONST ARRAY
5 CONST INT
6 CONST STR
7 CONST VOID
8 FORWARD ARRAY
9 FORWARD INT
10 FORWARD STR
11 FORWARD VOID
12 FORWARD CONST ARRAY
13 FORWARD CONST INT
14 FORWARD CONST STR
15 FORWARD CONST VOID
16 FUNCTION ARRAY
17 FUNCTION INT
18 FUNCTION STR
19 FUNCTION VOID
24 FORWARD FUNCTION ARRAY
25 FORWARD FUNCTION INT
26 FORWARD FUNCTION STR
27 FORWARD FUNCTION VOID
*/
#define _ARRAY 0
#define _INT 1
#define _STR 2
#define _VOID 3
#define NAK_TYP(a) (a % 4)
#define EQU_TYP(a,v) (NAK_TYP(a) == NAK_TYP(PLACE(v)) || IS_NULL(v))
#define IS_NULL(v) (OP_LABEL(v) == INT && v->value.i == 0)
#define isArray(v) (NAK_TYP(PLACE(v)) == 0)
#define isInt(v) (NAK_TYP(PLACE(v)) == 1)
#define isStr(v) (NAK_TYP(PLACE(v)) == 2)
#define isVoid(v) (NAK_TYP(PLACE(v)) == 3)
#define _CONST 4
#define _FORWARD 8
#define _PUBLIC 0
#define _FUNCTION 16
#define isConst(a) ((a % 8) > 3)
#define isForward(a) ((a % 16) > 7)
#define isFunction(a) (a > 15)
/* Declarations */
int yyparse();
int yyerror(char *s);
extern int yylex();
extern int yyselect(Node *);
extern int errors;
/* Global Variabes */
int inMain = 0;
int alen = 0;
int blck = 0;
int cicl = 0;
int retType = _INT;
char buf[120] = "";
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
%type <n> variable vDimOPT vInitOPT literal literalSEQ literals integerSEQ
%type <n> function fParamsOPT fParams fBody body vSEQ
%type <n> iBlock iSEQOPT iSEQ instruction iElifSEQ iElif iElse iSugar iLast
%type <n> rValueOPT lValue rValue rArgs
%type <n> qualifier constant type fType

%token NIL DECL DECLS LITERALS INTS
%token VAR V_PRIVACY V_RDONLY V_TYPE V_ID V_DIM INIT F_PRIVACY F_TYPE F_ID PARAMS
%token CONDITION ELIFS ELSES INSTRS BLOCK EXPR ARGS
%token BODY VARS FETCH LOAD CALL PRIORITY ERROR
%%

file        : { IDpush(); } program { IDpop(); if (!errors) { printNode($2, 0, yynames); yyselect($2); } freeNode($2); }
            | { IDpush(); } module  { IDpop(); if (!errors) { printNode($2, 0, yynames); yyselect($2); } freeNode($2); }
            ;

program     : PROGRAM dSEQOPT START { IDpush(); inMain = 1; } body { inMain = 0; IDpop(); } END { $$ = binNode(PROGRAM, $2, $5); }
            ;

module      : MODULE dSEQOPT END    { $$ = uniNode(MODULE, $2); }
            ;

dSEQOPT     :                       { $$ = nilNode(NIL); }
            | dSEQ                  { $$ = $1; }
            ;

dSEQ        : declaration           { $$ = binNode(DECLS, nilNode(NIL), $1); }
            | dSEQ ';' declaration  { $$ = binNode(DECLS, $1, $3); }
            | error START           { $$ = binNode(DECLS, nilNode(NIL), nilNode(ERROR)); }
            | error END             { $$ = binNode(DECLS, nilNode(NIL), nilNode(ERROR)); }
            | dSEQ ';' error ';'    { $$ = binNode(DECLS, $1, nilNode(ERROR)); }
            ;

declaration : function                              { $$ = $1; }
            | qualifier constant variable vInitOPT  { $$ = VARNode($1, $2, $3, $4); }
            ;

qualifier   :                       { $$ = nilNodeT(NIL, 0); }
            | PUBLIC                { $$ = nilNodeT(PUBLIC, _PUBLIC); }
            | FORWARD               { $$ = nilNodeT(FORWARD, _FORWARD); }
            ;

constant    :                       { $$ = nilNodeT(NIL, 0); }
            | CONST                 { $$ = nilNodeT(CONST, _CONST); }
            ;

variable    : type ID vDimOPT       { $$ = binNodeT(V_TYPE, $1, binNode(V_ID, strNode(ID, $2), $3), PLACE($1));
                                      if (OP_LABEL($1) != ARRAY && OP_LABEL($3) != NIL)
                                      yyerror("[Invalid Variable Type to specify its dimension]"); }
            ;

type        : ARRAY                 { $$ = nilNodeT(ARRAY, _ARRAY); }
            | NUMBER                { $$ = nilNodeT(NUMBER, _INT); }
            | STRING                { $$ = nilNodeT(STRING, _STR); }
            ;

vDimOPT     :                       { $$ = nilNode(NIL); }
            | '[' INT ']'           { $$ = intNode(V_DIM, $2);
                                      if ($2 == 0) yyerror("[Array dimension must be > 0]"); }
            ;

vInitOPT    :                               { $$ = uniNodeT(INIT, nilNode(NIL), _VOID); alen = 0; }
            | ASSIGN literals               { $$ = uniNodeT(INIT, $2, PLACE($2)); }
            | ASSIGN integerSEQ ',' INT     { $$ = uniNodeT(INIT, binNodeT(INTS, $2, intNode(INT, $4), _ARRAY), _ARRAY); alen++; }
            ;

literal     : INT                   { $$ = intNode(INT, $1); PLACE($$) = _INT; }
            | CHAR                  { $$ = intNode(CHAR, $1); PLACE($$) = _INT; }
            | STR                   { $$ = strNode(STR, $1); PLACE($$) = _STR; }
            ;

literalSEQ  : literal               { $$ = binNodeT(LITERALS, nilNode(NIL), $1, _STR); }
            | literalSEQ literal    { $$ = binNodeT(LITERALS, $1, $2, _STR); }
            ;

literals    : literal               { $$ = $1; alen = 1; }
            | literalSEQ literal    { $$ = binNodeT(LITERALS, $1, $2, _STR); }
            ;

integerSEQ  : INT                   { $$ = binNodeT(INTS, nilNode(NIL), intNode(INT, $1), _ARRAY); alen = 1; }
            | integerSEQ ',' INT    { $$ = binNodeT(INTS, $1, intNode(INT, $3), _ARRAY); alen++; }
            ;

function    : FUNCTION qualifier fType ID   { retType = PLACE($3); IDpush(); }
              fParamsOPT                    { FUNput(PLACE($2) + retType + _FUNCTION, $4, $6); }
              fBody                         { IDpop(); retType = _INT;
                                              $$ = binNode(FUNCTION, binNode(F_PRIVACY, $2, binNode(F_TYPE, $3, binNode(F_ID, strNode(ID, $4), $6))), $8);
                                              if (OP_LABEL($2) == FORWARD && OP_LABEL($8) != DONE) yyerror("[Forward Function must not have a Body]");
                                              else if (OP_LABEL($2) != FORWARD && OP_LABEL($8) == DONE) yyerror("[Function with empty Body must be Forward]"); }
            ;

fType       : type                  { $$ = $1; }
            | VOID                  { $$ = nilNodeT(VOID, _VOID); }
            ;

fParamsOPT  :                       { $$ = nilNode(NIL); }
            | fParams               { $$ = $1; }
            ;

fParams     : variable              { VARput($1); $$ = binNode(PARAMS, nilNode(NIL), $1); }
            | fParams ';' variable  { VARput($3); $$ = binNode(PARAMS, $1, $3); }
            ;

fBody       : DONE                  { $$ = nilNode(DONE); }
            | DO body               { $$ = uniNode(DO, $2); }
            ;

body        : vSEQ iSEQOPT iLast    { $$ = binNode(BODY, $1, binNode(BLOCK, $2, $3)); }
            ;

vSEQ        :                       { $$ = nilNode(NIL); }
            | vSEQ variable ';'     { VARput($2); $$ = binNode(VARS, $1, $2); }
            | vSEQ error ';'        { $$ = binNode(VARS, $1, nilNode(ERROR)); }
            ;

iBlock      : { blck++; } iSEQOPT iLast { blck--; $$ = binNode(BLOCK, $2, $3); }
            ;

iSEQOPT     :                       { $$ = nilNode(NIL); }
            | iSEQ                  { $$ = $1; }
            ;

iSEQ        : instruction           { $$ = binNode(INSTRS, nilNode(NIL), $1); }
            | iSEQ instruction      { $$ = binNode(INSTRS, $1, $2); }
            | iSEQ error iSugar     { $$ = binNode(INSTRS, $1, nilNode(ERROR)); }
            | iSEQ error FI         { $$ = binNode(INSTRS, $1, nilNode(ERROR)); }
            | iSEQ error DONE       { $$ = binNode(INSTRS, $1, nilNode(ERROR)); }
            ;

instruction : rValue iSugar                             { $$ = binNode(EXPR, $1, $2);
                                                          if (isVoid($1) && OP_LABEL($2) == '!') yyerror("[Void Expression can not be printed]"); }
            | lValue '#' rValue ';'                     { $$ = binNode('#', $1, $3);
                                                          if (!isLV($1)) yyerror("['#' Left-value must not be a Function]");
                                                          else if (isConst(PLACE($1))) yyerror("['#' Left-value must not be a Constant]");
                                                          else if (isInt($1)) yyerror("['#' Left-value Type must be a Pointer]");
                                                          else if (!isInt($3)) yyerror("['#' Expression Type must be an Integer]"); }
            | IF rValue                                 { if (!isInt($2)) yyerror("['if' Condition Type must be an Integer]"); }
              THEN iBlock iElifSEQ iElse FI             { $$ = binNode(CONDITION, binNode(IF, $2, uniNode(THEN, $5)), binNode(ELSES, $6, $7)); }
            | FOR rValue UNTIL rValue                   { if (!isInt($4)) yyerror("['until' Condition Type must be an Integer]"); }
              STEP rValue
              DO { cicl++; } iBlock { cicl--; } DONE    { $$ = binNode(FOR, $2, binNode(UNTIL, $4, binNode(STEP, $7, uniNode(DO, $10)))); }
            ;

iElifSEQ    :                           { $$ = nilNode(NIL); }
            | iElifSEQ iElif            { $$ = binNode(ELIFS, $1, $2); }
            ;

iElif       : ELIF rValue               { if (!isInt($2)) yyerror("['elif' Condition Type must be an Integer]"); }
              THEN iBlock               { $$ = binNode(ELIF, $2, uniNode(THEN, $5)); }
            ;

iElse       :                           { $$ = nilNode(NIL); }
            | ELSE iBlock               { $$ = uniNode(ELSE, $2); }
            ;

iSugar      : ';'                       { $$ = nilNode(';'); }
            | '!'                       { $$ = nilNode('!'); }
            ;

iLast       :                           { $$ = nilNode(NIL); }
            | REPEAT                    { $$ = nilNode(REPEAT);
                                          if (!cicl) yyerror("[Repeat must appear inside of a cicle]"); }
            | STOP                      { $$ = nilNode(STOP);
                                          if (!cicl) yyerror("[Stop must appear inside of a cicle]"); }
            | RETURN rValueOPT          { $$ = uniNode(RETURN, $2);
                                          if (!blck && (inMain || retType == _VOID)) yyerror("[Return must appear inside of a sub-block]");
                                          else if (!EQU_TYP(retType, $2)) yyerror("[Funciton Type != Return Type]"); }
            ;

rValueOPT   :                           { $$ = nilNodeT(NIL, _VOID); }
            | rValue                    { $$ = $1; }
            ;

lValue      : ID                        { $$ = findID($1, (void **)IDtest);
                                          if (isFunction(PLACE($$))) { freeNode($$); $$ = CALLNode($1, nilNode(NIL)); } }
            | ID '[' rValue ']'         { $$ = findID($1, (void **)IDtest);
                                          if (isFunction(PLACE($$))) { freeNode($$); $$ = idxNode(CALLNode($1, nilNode(NIL)), $3); }
                                          else $$ = idxNode($$, $3); }
            ;

rValue      : lValue                    { if (!isLV($1)) $$ = $1;
                                          else $$ = uniNodeT(LOAD, $1, NAK_TYP(PLACE($1))); }
            | literals                  { $$ = $1; }
            | ID '(' rArgs ')'          { $$ = CALLNode($1, $3); }
            | '(' rValue ')'            { $$ = uniNodeT(PRIORITY, $2, PLACE($2)); }
            | '?'                       { $$ = nilNodeT('?', _INT); }
            | '&' lValue %prec ADDR     { $$ = uniNode(ADDR, $2);
                                          if (!isLV($2)) yyerror("[Functions can not be located '&']");
                                          else if (!isInt($2)) yyerror("[Only Integers can be located '&']");
                                          else PLACE($$) = _ARRAY; }
            | '-' rValue %prec UMINUS   { $$ = uniNode(UMINUS, $2);
                                          if (isInt($2)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Type to (Symmetrical) '-']"); }
            | rValue '^' rValue         { $$ = binNode('^', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '^']"); }
            | rValue '*' rValue         { $$ = binNode('*', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '*']"); }
            | rValue '/' rValue         { $$ = binNode('/', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '/']"); }
            | rValue '%' rValue         { $$ = binNode('%', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '%%']"); }
            | rValue '+' rValue         { $$ = binNode('+', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = _INT;
                                          else if (isInt($1) && isArray($3) ||
                                            isArray($1) && isInt($3)) PLACE($$) = _ARRAY;
                                          else yyerror("[Invalid Types to '+']"); }
            | rValue '-' rValue         { $$ = binNode('-', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isArray($1) && isArray($3)) PLACE($$) = _INT;
                                          else if (isInt($1) && isArray($3) ||
                                            isArray($1) && isInt($3)) PLACE($$) = _ARRAY;
                                          else yyerror("[Invalid Types to '-']"); }
            | rValue '<' rValue         { $$ = binNode('<', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '<']"); }
            | rValue '>' rValue         { $$ = binNode('>', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '>']"); }
            | rValue LE rValue          { $$ = binNode(LE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '<=']"); }
            | rValue GE rValue          { $$ = binNode(GE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '>=']"); }
            | rValue NE rValue          { $$ = binNode(NE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '~=']"); }
            | rValue '=' rValue         { $$ = binNode('=', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to '=']"); }
            | '~' rValue                { $$ = uniNode('~', $2);
                                          if (isInt($2)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Type to '~']"); }
            | rValue '&' rValue         { $$ = binNode('&', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to (AND) '&']"); }
            | rValue '|' rValue         { $$ = binNode('|', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = _INT;
                                          else yyerror("[Invalid Types to (OR) '|']"); }
            | lValue ASSIGN rValue      { $$ = binNode(ASSIGN, $1, $3);
                                          if (!isLV($1)) yyerror("[Functions can not be assigned ':=']");
                                          else if (isConst(PLACE($1))) yyerror("[Constants can not be assigned ':=']");
                                          else if (EQU_TYP(PLACE($1), $3)) PLACE($$) = PLACE($1);
                                          else yyerror("[Invalid Types to ':=']"); }
            ;

rArgs       : rValue                    { $$ = binNode(ARGS, nilNode(NIL), $1); }
            | rArgs ',' rValue          { $$ = binNode(ARGS, $1, $3); }
            ;
%%

static Node *nilNodeT(int tok, int info) {

    Node *n = nilNode(tok);
    PLACE(n) = info;
    return n;
}

static Node *uniNodeT(int tok, Node *left, int info) {

    Node *n = uniNode(tok, left);
    PLACE(n) = info;
    return n;
}

static Node *binNodeT(int tok, Node *left, Node *right, int info) {

    Node *n = binNode(tok, left, right);
    PLACE(n) = info;
    return n;
}

static void VARput(Node *var) {

    char *id = LEFT_CHILD(RIGHT_CHILD(var))->value.s;
    int typ = IDsearch(id, (void **)IDtest, 0, 1);

    if (typ == -1) {
        IDadd(PLACE(var), id, 0);
    } else {
        if (isFunction(typ)) {
            sprintf(buf, "[Function named '%s' already declared]", id);
            yyerror(buf);
        } else if (!isForward(typ)) {
            sprintf(buf, "[Variable '%s' already defined]", id);
            yyerror(buf);
        } else if (isConst(typ) != isConst(PLACE(var)) || NAK_TYP(typ) != NAK_TYP(PLACE(var))) {
            sprintf(buf, "[Variable '%s' already declared with a different Type]", id);
            yyerror(buf);
        } else {
            IDreplace(PLACE(var), id, 0);
        }
    }
}

static Node *VARNode(Node *qual, Node *cons, Node *var, Node *init) {

    char *id = LEFT_CHILD(RIGHT_CHILD(var))->value.s;
    int typV = PLACE(qual) + PLACE(cons) + PLACE(var);

    if (!isVoid(init)) {
        if (isForward(typV)) {
            sprintf(buf, "[Forward Variable '%s' can not be initialized]", id);
            yyerror(buf);
        } else if (!(EQU_TYP(typV, init) || (typV == _ARRAY) && isInt(init))) {
            sprintf(buf, "[Invalid Variable '%s' initialization Type]", id);
            yyerror(buf);
        } else if (isArray(var)) {
            Node *dim = RIGHT_CHILD(RIGHT_CHILD(var));
            if (OP_LABEL(dim) == NIL) {
                sprintf(buf, "[Array '%s' dimension must be specified when initialized]", id);
                yyerror(buf);
            } else if (dim->value.i < alen) {
                sprintf(buf, "[Invalid Array '%s' dimension: %d < %d]", id, dim->value.i, alen);
                yyerror(buf);
            }
        }
    } else if (!isForward(typV) && isConst(typV)) {
        sprintf(buf, "[Non Forward Constant '%s' is not initialized]", id);
        yyerror(buf);
    }

    PLACE(var) = typV;
    VARput(var);
    return binNode(VAR, binNode(V_PRIVACY, qual, binNode(V_RDONLY, cons, var)), init);
}

static void checkArgs(char *id, Node *params, Node *args) {

    if (OP_LABEL(params) == NIL && OP_LABEL(args) == NIL) return;

    while (OP_LABEL(params) != NIL && OP_LABEL(args) != NIL) {
        
        if (!EQU_TYP(PLACE(RIGHT_CHILD(params)), RIGHT_CHILD(args))) {
            sprintf(buf, "[Invalid Parameter Types to Function '%s']", id);
            yyerror(buf);
            return;
        }
        params = LEFT_CHILD(params);
        args = LEFT_CHILD(args);

        if (OP_LABEL(params) == NIL && OP_LABEL(args) == NIL) return;
    }
    
    sprintf(buf, "[Invalid Parameters to Function '%s']", id);
    yyerror(buf);
}

static void FUNput(int typF, char *id, Node *params) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("Out of Memory"); exit(2); }
    int typ = IDsearch(id, (void **)p, 1, 1);

    if (typ == -1) {
        IDinsert(IDlevel() - 1, typF, id, params);
    } else {
        if (!isFunction(typ)) {
            sprintf(buf, "[Variable named '%s' already declared]", id);
            yyerror(buf);
        } else if (!isForward(typ)) {
            sprintf(buf, "[Function '%s' already defined]", id);
            yyerror(buf);
        } else if (NAK_TYP(typ) != NAK_TYP(typF)) {
            sprintf(buf, "[Function '%s' already declared with a different Type]", id);
            yyerror(buf);
        } else {
            checkArgs(id, params, *p);
            IDchange(typF, id, params, 1);
        }
    }
    free(p);
}

static Node *findID(char *id, void **attr) {

    int typ = IDfind(id, attr);
    if (typ == -1) {
        sprintf(buf, "[Identifier '%s' is undefined]", id);
        yyerror(buf);
    }
    return uniNodeT(FETCH, strNode(ID, id), typ);
}

static Node *idxNode(Node *ptr, Node *expr) {

    if (isInt(ptr)) yyerror("[Number can not be Indexed]");
    else if (!isInt(expr)) yyerror("[Index Expression must be an Integer]");

    return binNodeT('[', ptr, expr, _INT);
}

static int isLV(Node *ptr) {

    return OP_LABEL(ptr) == FETCH || OP_LABEL(ptr) == '[';
}

static Node *CALLNode(char *id, Node *args) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("[Out of Memory]"); exit(2); }
    Node *idN = findID(id, (void **)p);

    if (isFunction(PLACE(idN))) {
        checkArgs(id, *p, args);
    } else {
        sprintf(buf, "[The Identifier '%s' must be a Function]", id);
        yyerror(buf);
    }
    free(p);
    return binNodeT(CALL, idN, args, NAK_TYP(PLACE(idN)));
}

char **yynames =
#if YYDEBUG > 0
    (char **)yyname;
#else
    NULL;
#endif
