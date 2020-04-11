%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#define YYDEBUG 1
#define A_TYPE 0
#define I_TYPE 1
#define S_TYPE 2
#define V_TYPE 3
#define P_TYPE 1
#define F_TYPE 2
#define C_TYPE 1
#define isArr(a) ((PLACE(a) % 4) == A_TYPE)
#define isInt(i) ((PLACE(i) % 4) == I_TYPE)
#define isStr(s) ((PLACE(s) % 4) == S_TYPE)
#define isVoid(v) ((PLACE(v) % 4) == V_TYPE)
#define toFunc(n) (n + 24)
#define isFunc(i) (i >= 24)
#define isForw(i) ((i % 12) >= 8)
#define isCons(i) ((i % 24) >= 12)
#define sameType(a,b) ((a % 4) == (b % 4))
#define checkType(g,s) (sameType(g,s) || ((g % 4 == A_TYPE) && (s % 4 == I_TYPE)))
#define VType(q,c,t) (t + q * 4 + c * 12)
#define FType(q) (retType + q * 4 + 24)
int yylex();
int yyerror(char *s);
extern int errors;
int cnt = 0;
int blck = 0;
int cicl = 0;
int retType = I_TYPE;
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
%nonassoc ID
%nonassoc '[' ']' '(' ')'

%token <i> INT
%token <c> CHAR
%token <s> ID STR
%token PROGRAM MODULE END START
%token VOID CONST NUMBER ARRAY STRING FUNCTION PUBLIC FORWARD
%token IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN

%type <n> program module dSEQOPT dSEQ declaration
%type <n> variable vDimOPT vInitOPT literal literals integers
%type <n> function fParamsOPT fParams fBody body vSEQ
%type <n> iBlock iSEQ instruction iElifSEQ iElif iElse iSugar iLast
%type <n> rValueOPT lValue rValue rArgs
%type <i> qualifier constant type fType

%token NIL DECL DECLS VAR VARS DIM INIT LITERALS INTS
%token CONDITION ELIFS ELSES INSTRS BLOCK EXPR
%token BODY PARAMS ATTRIB FETCH LOAD CALL PRIORITY ERROR
%%

file        : { IDpush(); } program { IDpop(); if (!errors) printNode($2, 0, yynames); freeNode($2); }
            | { IDpush(); } module  { IDpop(); if (!errors) printNode($2, 0, yynames); freeNode($2); }
            ;

program     : PROGRAM dSEQOPT START body END    { $$ = binNode(PROGRAM, $2, $4); }
            ;

module      : MODULE dSEQOPT END    { $$ = uniNode(MODULE, $2); }
            ;

dSEQOPT     :                       { $$ = nilNode(NIL); }
            | dSEQ                  { $$ = $1; }
            ;

dSEQ        : declaration           { $$ = binNode(DECLS, $1, nilNode(NIL)); }
            | error                 { $$ = binNode(DECLS, nilNode(ERROR), nilNode(NIL)); }
            | dSEQ ';' declaration  { $$ = binNode(DECLS, $3, $1); }
            | dSEQ ';' error        { $$ = binNode(DECLS, nilNode(ERROR), $1); }
            ;

declaration : function                              { $$ = $1; }
            | qualifier constant variable vInitOPT  { $$ = VARNode($1, $2, $3, $4); }
            ;

qualifier   :                   { $$ = 0; }
            | PUBLIC            { $$ = P_TYPE; }
            | FORWARD           { $$ = F_TYPE; }
            ;

constant    :                   { $$ = 0; }
            | CONST             { $$ = C_TYPE; }
            ;

variable    : type ID vDimOPT   { $$ = binNodeT(VAR, strNode(ID, $2), $3, $1);
                                  if ($1 != A_TYPE && OP_LABEL($3) != NIL)
                                  yyerror("[Invalid Variable Type to specify its dimension]"); }
            ;

type        : ARRAY             { $$ = A_TYPE; }
            | NUMBER            { $$ = I_TYPE; }
            | STRING            { $$ = S_TYPE; }
            ;

vDimOPT     :                   { $$ = nilNode(NIL); }
            | '[' INT ']'       { $$ = intNode(DIM, $2);
                                  if ($2 == 0) yyerror("[Array dimension must be > 0]"); }
            ;

vInitOPT    :                   { $$ = nilNodeT(NIL, V_TYPE); cnt = 0; }
            | ASSIGN literal    { $$ = uniNodeT(INIT, $2, PLACE($2)); }
            | ASSIGN literals   { $$ = uniNodeT(INIT, $2, PLACE($2));}
            | ASSIGN integers   { $$ = uniNodeT(INIT, $2, PLACE($2));}
            ;

literal     : STR               { $$ = strNode(STR, $1); PLACE($$) = S_TYPE; cnt = 1; }
            | INT               { $$ = intNode(INT, $1); PLACE($$) = I_TYPE; cnt = 1; }
            | CHAR              { $$ = intNode(CHAR, $1); PLACE($$) = I_TYPE; cnt = 1; }
            ;

literals    : literal literal   { $$ = binNodeT(LITERALS, $2,
                                  binNodeT(LITERALS, $1, nilNode(NIL), S_TYPE), S_TYPE); cnt = 2; }
            | literals literal  { $$ = binNodeT(LITERALS, $1, $2, S_TYPE); cnt++; }
            ;

integers    : INT ',' INT       { $$ = binNodeT(INTS, intNode(INT, $3),
                                  binNodeT(INTS, intNode(INT, $1), nilNode(NIL), A_TYPE), A_TYPE); cnt = 2; }
            | integers ',' INT  { $$ = binNodeT(INTS, intNode(INT, $3), $1, A_TYPE); cnt++; }
            ;

function    : FUNCTION qualifier fType ID   { retType = $3; IDpush(); }
              fParamsOPT                    { FUNCput($2, $4, $6); }
              fBody                         { IDpop(); retType = I_TYPE;
                                              $$ = binNode(FUNCTION, binNode(ATTRIB, strNode(ID, $4), $6), $8);
                                              if ($2 == F_TYPE && OP_LABEL($8) != DONE) yyerror("[Forward Function must not have a Body]");
                                              else if ($2 != F_TYPE && OP_LABEL($8) == DONE) yyerror("[Function with empty Body must be Forward]"); }
            ;

fType       : type                  { $$ = $1; }
            | VOID                  { $$ = V_TYPE; }
            ;

fParamsOPT  :                       { $$ = nilNode(NIL); }
            | fParams               { $$ = $1; }
            ;

fParams     : variable              { VARput(0, 0, $1); $$ = binNode(PARAMS, $1, nilNode(NIL)); }
            | fParams ';' variable  { VARput(0, 0, $3); $$ = binNode(PARAMS, $3, $1); }
            ;

fBody       : DONE                  { $$ = nilNode(DONE); }
            | DO body               { $$ = uniNode(DO, $2); }
            ;

body        : vSEQ iSEQ iLast       { $$ = binNode(BODY, $1, binNode(BLOCK, $2, $3)); }
            ;

vSEQ        :                       { $$ = nilNode(NIL); }
            | vSEQ variable ';'     { VARput(0, 0, $2); $$ = binNode(VARS, $2, $1); }
            | vSEQ error ';'        { $$ = binNode(VARS, nilNode(ERROR), $1); }
            ;

iBlock      : { blck++; } iSEQ iLast    { blck--; $$ = binNode(BLOCK, $2, $3); }
            ;

iSEQ        :                       { $$ = nilNode(NIL); }
            | iSEQ instruction      { $$ = binNode(INSTRS, $2, $1); }
            ;

instruction : rValue iSugar                             { $$ = binNode(EXPR, $1, $2);
                                                          if (isVoid($1) && OP_LABEL($2) == '!') yyerror("[Void Expression can not be printed]"); }
            | lValue '#' rValue ';'                     { $$ = binNode('#', $1, $3);
                                                          if (isFunc(PLACE($1))) yyerror("['#' Left-value must not be a Function]");
                                                          else if (isInt($1)) yyerror("['#' Left-value Type must be a Pointer]");
                                                          else if (!isInt($3)) yyerror("['#' Expression Type must be an Integer]"); }
            | IF rValue                                 { if (!isInt($2)) yyerror("['if' Condition Type must be an Integer]"); }
              THEN iBlock iElifSEQ iElse FI             { $$ = binNode(CONDITION, binNode(IF, $2, uniNode(THEN, $5)), binNode(ELSES, $6, $7)); }
            | FOR rValue UNTIL rValue                   { if (!isInt($4)) yyerror("['until' Condition Type must be an Integer]"); }
              STEP rValue
              DO { cicl++; } iBlock { cicl--; } DONE    { $$ = binNode(FOR, $2, binNode(UNTIL, $4, binNode(STEP, $7, uniNode(DO, $10)))); }
            ;

iElifSEQ    :                           { $$ = nilNode(NIL); }
            | iElifSEQ iElif            { $$ = binNode(ELIFS, $2, $1); }
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
                                          if (!blck && (IDlevel() == 1 || retType == V_TYPE)) yyerror("[Return must appear inside of a sub-block]");
                                          else if (!checkType(retType, PLACE($2))) yyerror("[Funciton Type != Return Type]"); }
            ;

rValueOPT   :                           { $$ = nilNodeT(NIL, V_TYPE); }
            | rValue                    { $$ = $1; }
            ;

lValue      : ID                        { $$ = findID($1, (void **)IDtest);
                                          if (isFunc(PLACE($$))) { freeNode($$); $$ = CALLNode($1, nilNode(NIL)); } }
            | ID '[' rValue ']'         { $$ = findID($1, (void **)IDtest);
                                          if (isFunc(PLACE($$))) { freeNode($$); $$ = idxNode(CALLNode($1, nilNode(NIL)), $3); }
                                          else $$ = idxNode($$, $3); }
            ;

rValue      : lValue                    { if (isFunc(PLACE($1))) $$ = $1;
                                          else $$ = uniNodeT(LOAD, $1, PLACE($1)); }
            | literal                   { $$ = $1; }
            | literals                  { $$ = $1; }
            | ID '(' rArgs ')'          { $$ = CALLNode($1, $3); }
            | rValue '[' rValue ']'     { $$ = idxNode($1, $3); }
            | '(' rValue ')'            { $$ = uniNodeT(PRIORITY, $2, PLACE($2)); }
            | '?'                       { $$ = nilNodeT('?', I_TYPE); }
            | '&' lValue %prec ADDR     { $$ = uniNode(ADDR, $2);
                                          if (isFunc(PLACE($2))) yyerror("[Functions can not be located '&']");
                                          else if (!isInt($2)) yyerror("[Only Integers can be located '&']");
                                          else PLACE($$) = I_TYPE; }
            | '-' rValue %prec UMINUS   { $$ = uniNode(UMINUS, $2);
                                          if (isInt($2)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Type to (Symmetrical) '-']"); }
            | rValue '^' rValue         { $$ = binNode('^', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '^']"); }
            | rValue '*' rValue         { $$ = binNode('*', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '*']"); }
            | rValue '/' rValue         { $$ = binNode('/', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '/']"); }
            | rValue '%' rValue         { $$ = binNode('%', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '%%']"); }
            | rValue '+' rValue         { $$ = binNode('+', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else if (isInt($1) && isArr($3) ||
                                            isArr($1) && isInt($3)) PLACE($$) = A_TYPE;
                                          else yyerror("[Invalid Types to '+']"); }
            | rValue '-' rValue         { $$ = binNode('-', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isArr($1) && isArr($3)) PLACE($$) = I_TYPE;
                                          else if (isInt($1) && isArr($3) ||
                                            isArr($1) && isInt($3)) PLACE($$) = A_TYPE;
                                          else yyerror("[Invalid Types to '-']"); }
            | rValue '<' rValue         { $$ = binNode('<', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '<']"); }
            | rValue '>' rValue         { $$ = binNode('>', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '>']"); }
            | rValue LE rValue          { $$ = binNode(LE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '<=']"); }
            | rValue GE rValue          { $$ = binNode(GE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '>=']"); }
            | rValue NE rValue          { $$ = binNode(NE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '~=']"); }
            | rValue '=' rValue         { $$ = binNode('=', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to '=']"); }
            | '~' rValue                { $$ = uniNode('~', $2);
                                          if (isInt($2)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Type to '~']"); }
            | rValue '&' rValue         { $$ = binNode('&', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to (AND) '&']"); }
            | rValue '|' rValue         { $$ = binNode('|', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("[Invalid Types to (OR) '|']"); }
            | lValue ASSIGN rValue      { $$ = binNode(ASSIGN, $1, $3);
                                          if (isFunc(PLACE($1))) yyerror("[Functions can not be assigned ':=']");
                                          else if (isCons(PLACE($1))) yyerror("[Constants can not be assigned ':=']");
                                          else if (OP_LABEL($3) == INT && $3->value.i == 0) PLACE($$) = PLACE($1);
                                          else if (checkType(PLACE($1), PLACE($3))) PLACE($$) = PLACE($1);
                                          else yyerror("[Invalid Types to ':=']"); }
            ;

rArgs       : rValue                    { $$ = binNode(PARAMS, $1, nilNode(NIL)); }
            | rArgs ',' rValue          { $$ = binNode(PARAMS, $3, $1); }
            ;
%%

Node *nilNodeT(int tok, int info) {

    Node *r = nilNode(tok);
    PLACE(r) = info;
    return r;
}

Node *uniNodeT(int tok, Node *left, int info) {

    Node *r = uniNode(tok, left);
    PLACE(r) = info;
    return r;
}

Node *binNodeT(int tok, Node *left, Node *right, int info) {

    Node *r = binNode(tok, left, right);
    PLACE(r) = info;
    return r;
}

void VARput(int qual, int cons, Node *var) {

    int typ = IDfind(LEFT_CHILD(var)->value.s, (void **)IDtest, 0);
    char *id = LEFT_CHILD(var)->value.s;

    if (typ == -1) {
        IDadd(VType(qual, cons, PLACE(var)), LEFT_CHILD(var)->value.s, 0);
    } else {
        if (isFunc(typ)) {
            sprintf(buf, "[Function named '%s' already declared]", id);
            yyerror(buf);
        } else if (!isForw(typ)) {
            sprintf(buf, "[Variable '%s' already defined]", id);
            yyerror(buf);
        } else if (isCons(typ) != (cons == C_TYPE) || !sameType(typ, PLACE(var))) {
            sprintf(buf, "[Variable '%s' already declared with a different Type]", id);
            yyerror(buf);
        } else {
            IDreplace(VType(qual, cons, PLACE(var)), LEFT_CHILD(var)->value.s, 0);
        }
    }
}

Node *VARNode(int qual, int cons, Node *var, Node *init) {

    char *id = LEFT_CHILD(var)->value.s;

    if (!isVoid(init)) {
        if (qual == F_TYPE) {
            sprintf(buf, "[Forward Variable '%s' can not be initialized]", id);
            yyerror(buf);
        } else if (!checkType(PLACE(var), PLACE(init))) {
            sprintf(buf, "[Invalid Variable '%s' initialization Type]", id);
            yyerror(buf);
        } else if (PLACE(var) == A_TYPE) {
            if (OP_LABEL(RIGHT_CHILD(var)) == NIL) {
                sprintf(buf, "[Array '%s' dimension must be specified when initialized]", id);
                yyerror(buf);
            } else if (RIGHT_CHILD(var)->value.i < cnt) {
                sprintf(buf, "[Invalid Array '%s' dimension: %d < %d]", id, RIGHT_CHILD(var)->value.i, cnt);
                yyerror(buf);
            }
        }
    } else if (qual != F_TYPE && cons == C_TYPE) {
        sprintf(buf, "[Non Forward Constant '%s' is not initialized]", id);
        yyerror(buf);
    }
    VARput(qual, cons, var);
    return binNode(DECL, var, init);
}

void checkArgs(char *id, Node *p, Node *args) {

    if (OP_LABEL(p) != NIL && OP_LABEL(args) != NIL) {
        do {
            if (!sameType(PLACE(LEFT_CHILD(p)), PLACE(LEFT_CHILD(args)))) {
                sprintf(buf, "[Invalid Parameter Types to Function '%s']", id);
                yyerror(buf);
                break;
            }
            p = RIGHT_CHILD(p);
            args = RIGHT_CHILD(args);
            if (OP_LABEL(p) != OP_LABEL(args)) {
                sprintf(buf, "[Invalid Parameters to Function '%s']", id);
                yyerror(buf);
                break;
            }
        } while (OP_LABEL(p) != NIL && OP_LABEL(args) != NIL);

    } else if (OP_LABEL(p) != OP_LABEL(args)) {
        sprintf(buf, "[Invalid Parameters to Function '%s']", id);
        yyerror(buf);
    }
}

void FUNCput(int qual, char *id, Node *params) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("Out of Memory"); exit(2); }
    int typ = IDfind(id, (void **)p, 1);

    if (typ == -1) {
        IDinsert(IDlevel() - 1, FType(qual), id, params);
    } else {
        if (!isFunc(typ)) {
            sprintf(buf, "[Variable named '%s' already declared]", id);
            yyerror(buf);
        } else if (!isForw(typ)) {
            sprintf(buf, "[Function '%s' already defined]", id);
            yyerror(buf);
        } else if (!sameType(typ, retType)) {
            sprintf(buf, "[Function '%s' already declared with different Parameter Types]", id);
            yyerror(buf);
        } else {
            checkArgs(id, *p, params);
            IDchange(FType(qual), id, params, 1);
        }
    }
    free(p);
}

Node *findID(char *id, void **attr) {

    int typ = IDfind(id, attr, 0);
    if (typ == -1) {
        sprintf(buf, "[Identifier '%s' is undefined]", id);
        yyerror(buf);
    }
    return uniNodeT(FETCH, strNode(ID, id), typ);
}

Node *idxNode(Node *ptr, Node *expr) {

    if (isInt(ptr)) yyerror("[Number can not be Indexed]");
    else if (!isInt(expr)) yyerror("[Index Expression must be an Integer]");

    int typ = OP_LABEL(ptr) == CALL ? toFunc(I_TYPE) : I_TYPE;

    return binNodeT('[', ptr, expr, typ);
}

Node *CALLNode(char *id, Node *args) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("[Out of Memory]"); exit(2); }
    Node *idN = findID(id, (void **)p);

    if (isFunc(PLACE(idN))) {
        checkArgs(id, *p, args);
    } else {
        sprintf(buf, "[The Identifier '%s' must be a Function]", id);
        yyerror(buf);
    }
    free(p);
    return binNodeT(CALL, idN, args, PLACE(idN));
}

char **yynames =
#if YYDEBUG > 0
    (char **)yyname;
#else
    NULL;
#endif
