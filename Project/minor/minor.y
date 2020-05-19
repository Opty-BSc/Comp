%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#include "postfix.h"
#include "minor.h"
#define YYDEBUG 1
/* Declarations */
int yyparse();
int yyerror(char *);
extern int yylex();
extern int errors;
extern void decVar(int, char *, Node *, Node *);
void doMain(int, Node *);
void doFunc(int, char *, int, Node *);
/* Global Variabes */
static int inMain;
static int alen;
static int blck;
static int cicl;
static int enter;
static int retType = _INT;
static char buf[120] = "";
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
%token VAR V_LBL V_RDONLY V_TYPE V_ID V_DIM INIT FUNC F_HEAD F_LBL F_ID PARAMS F_BODY
%token CONDITION ELIFS ELSES INSTRS BLOCK EXPR ARGS LINDEX RINDEX
%token BODY VARS FETCH LOAD CALL PRIORITY ERROR
%%

file        : { IDpush(); } program { IDpop(); if (!errors) { printNode($2, 0, yynames); } freeNode($2); }
            | { IDpush(); } module  { IDpop(); if (!errors) { printNode($2, 0, yynames); } freeNode($2); }
            ;

program     : PROGRAM dSEQOPT START { IDpush(); inMain = 1; }
              body { doMain(enter, $5); inMain = 0; IDpop(); }
              END { $$ = binNode(PROGRAM, $2, $5); }
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
            | qualifier constant variable vInitOPT  { $$ = varNode($1, $2, $3, $4); }
            ;

qualifier   :                       { $$ = nilNodeT(NIL, 0); }
            | PUBLIC                { $$ = nilNodeT(PUBLIC, _PUBLIC); }
            | FORWARD               { $$ = nilNodeT(FORWARD, _FORWARD); }
            ;

constant    :                       { $$ = nilNodeT(NIL, 0); }
            | CONST                 { $$ = nilNodeT(CONST, _CONST); }
            ;

variable    : type ID vDimOPT       { $$ = binNodeT(V_TYPE, $1, binNode(V_ID, strNode(ID, $2), $3), INFO($1));
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

vInitOPT    :                               { $$ = nilNodeT(NIL, _VOID); alen = 0; }
            | ASSIGN literals               { $$ = $2; }
            | ASSIGN integerSEQ ',' INT     { $$ = binNodeT(INTS, $2, intNode(INT, $4), _ARRAY); alen++; }
            ;

literal     : INT                   { $$ = intNode(INT, $1); INFO($$) = _INT; }
            | CHAR                  { $$ = intNode(CHAR, $1); INFO($$) = _INT; }
            | STR                   { $$ = strNode(STR, $1); INFO($$) = _STR; }
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

function    : FUNCTION qualifier fType ID   { retType = INFO($3); IDpush(); }
              fParamsOPT                    { funcPut(INFO($2) + retType + _FUNCTION, $4, $6); }
              fBody                         { IDpop(); retType = _INT;
                                              $$ = binNode(FUNC, binNode(F_HEAD, binNode(F_LBL, $2, binNode(F_ID, $3, strNode(ID, $4))), $6), $8);
                                              if (OP_LABEL($2) == FORWARD && OP_LABEL($8) != DONE) yyerror("[Forward Function must not have a Body]");
                                              else if (OP_LABEL($2) != FORWARD && OP_LABEL($8) == DONE) yyerror("[Function with empty Body must be Forward]");
                                              else doFunc(INFO($2) + INFO($3), $4, enter, $8); }
            ;

fType       : type                  { $$ = $1; }
            | VOID                  { $$ = nilNodeT(VOID, _VOID); }
            ;

fParamsOPT  :                       { $$ = nilNode(NIL); }
            | fParams               { $$ = $1; }
            ;

fParams     : variable              { varPut($1); $$ = binNode(PARAMS, nilNode(NIL), $1); }
            | fParams ';' variable  { varPut($3); $$ = binNode(PARAMS, $1, $3); }
            ;

fBody       : DONE                  { $$ = nilNode(DONE); enter = 0; }
            | DO body               { $$ = uniNode(F_BODY, $2); }
            ;

body        : vSEQ iSEQOPT iLast    { $$ = binNode(BODY, $1, binNode(BLOCK, $2, $3)); }
            ;

vSEQ        :                       { $$ = nilNode(NIL); enter = 0; }
            | vSEQ variable ';'     { varPut($2); $$ = binNode(VARS, $1, $2); enter += isInt($2) ? 4 : pfWORD; }
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
                                                          else if (isConst(INFO($1))) yyerror("['#' Left-value must not be a Constant]");
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
                                          else if (!SAME_TYPE(retType, $2)) yyerror("[Funciton Type != Return Type]"); }
            ;

rValueOPT   :                           { $$ = nilNodeT(NIL, _VOID); }
            | rValue                    { $$ = $1; }
            ;

lValue      : ID                        { $$ = findID($1, (void **)IDtest);
                                          if (isFunction(INFO($$))) { freeNode($$); $$ = callNode($1, nilNode(NIL)); } }
            | ID '[' rValue ']'         { $$ = findID($1, (void **)IDtest);
                                          if (isFunction(INFO($$))) { freeNode($$); $$ = idxNode(callNode($1, nilNode(NIL)), $3); }
                                          else $$ = idxNode($$, $3); }
            ;

rValue      : lValue                    { if (isLV($1)) $$ = uniNodeT(LOAD, $1, NAKED_TYPE(INFO($1)));
                                          else $$ = $1; }
            | literals                  { $$ = $1; }
            | ID '(' rArgs ')'          { $$ = callNode($1, $3); }
            | '(' rValue ')'            { $$ = uniNodeT(PRIORITY, $2, INFO($2)); }
            | '?'                       { $$ = nilNodeT('?', _INT); }
            | '&' lValue %prec ADDR     { $$ = uniNode(ADDR, $2);
                                          if (!isLV($2)) yyerror("[Functions can not be located '&']");
                                          else if (!isInt($2)) yyerror("[Only Integers can be located '&']");
                                          else INFO($$) = _ARRAY; }
            | '-' rValue %prec UMINUS   { $$ = uniNode(UMINUS, $2);
                                          if (isInt($2)) INFO($$) = _INT;
                                          else yyerror("[Invalid Type to (Symmetrical) '-']"); }
            | rValue '^' rValue         { $$ = binNode('^', $1, $3);
                                          if (isInt($1) && isInt($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '^']"); }
            | rValue '*' rValue         { $$ = binNode('*', $1, $3);
                                          if (isInt($1) && isInt($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '*']"); }
            | rValue '/' rValue         { $$ = binNode('/', $1, $3);
                                          if (isInt($1) && isInt($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '/']"); }
            | rValue '%' rValue         { $$ = binNode('%', $1, $3);
                                          if (isInt($1) && isInt($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '%%']"); }
            | rValue '+' rValue         { $$ = binNode('+', $1, $3);
                                          if (isInt($1) && isInt($3)) INFO($$) = _INT;
                                          else if (isInt($1) && isArray($3) ||
                                            isArray($1) && isInt($3)) INFO($$) = _ARRAY;
                                          else yyerror("[Invalid Types to '+']"); }
            | rValue '-' rValue         { $$ = binNode('-', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isArray($1) && isArray($3)) INFO($$) = _INT;
                                          else if (isInt($1) && isArray($3) ||
                                            isArray($1) && isInt($3)) INFO($$) = _ARRAY;
                                          else yyerror("[Invalid Types to '-']"); }
            | rValue '<' rValue         { $$ = binNode('<', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '<']"); }
            | rValue '>' rValue         { $$ = binNode('>', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '>']"); }
            | rValue LE rValue          { $$ = binNode(LE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '<=']"); }
            | rValue GE rValue          { $$ = binNode(GE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '>=']"); }
            | rValue NE rValue          { $$ = binNode(NE, $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '~=']"); }
            | rValue '=' rValue         { $$ = binNode('=', $1, $3);
                                          if (isInt($1) && isInt($3) ||
                                            isStr($1) && isStr($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to '=']"); }
            | '~' rValue                { $$ = uniNode('~', $2);
                                          if (isInt($2)) INFO($$) = _INT;
                                          else yyerror("[Invalid Type to '~']"); }
            | rValue '&' rValue         { $$ = binNode('&', $1, $3);
                                          if (isInt($1) && isInt($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to (AND) '&']"); }
            | rValue '|' rValue         { $$ = binNode('|', $1, $3);
                                          if (isInt($1) && isInt($3)) INFO($$) = _INT;
                                          else yyerror("[Invalid Types to (OR) '|']"); }
            | lValue ASSIGN rValue      { $$ = binNode(ASSIGN, $1, $3);
                                          if (!isLV($1)) yyerror("[Functions can not be assigned ':=']");
                                          else if (isConst(INFO($1))) yyerror("[Constants can not be assigned ':=']");
                                          else if (SAME_TYPE(INFO($1), $3)) INFO($$) = INFO($1);
                                          else yyerror("[Invalid Types to ':=']"); }
            ;

rArgs       : rValue                    { $$ = binNode(ARGS, nilNode(NIL), $1); }
            | rArgs ',' rValue          { $$ = binNode(ARGS, $1, $3); }
            ;
%%

static Node *nilNodeT(int tok, int info) {

    Node *n = nilNode(tok);
    INFO(n) = info;
    return n;
}

static Node *uniNodeT(int tok, Node *left, int info) {

    Node *n = uniNode(tok, left);
    INFO(n) = info;
    return n;
}

static Node *binNodeT(int tok, Node *left, Node *right, int info) {

    Node *n = binNode(tok, left, right);
    INFO(n) = info;
    return n;
}

static void varPut(Node *var) {

    char *id = LEFT_CHILD(RIGHT_CHILD(var))->value.s;
    int typ = IDsearch(id, (void **)IDtest, 0, 1);

    if (typ == -1) {
        IDadd(INFO(var), id, 0);
    } else {
        if (isFunction(typ)) {
            sprintf(buf, "[Function named '%s' already declared]", id);
            yyerror(buf);
        } else if (!isForw(typ)) {
            sprintf(buf, "[Variable '%s' already defined]", id);
            yyerror(buf);
        } else if (isConst(typ) != isConst(INFO(var)) || NAKED_TYPE(typ) != NAKED_TYPE(INFO(var))) {
            sprintf(buf, "[Variable '%s' already declared with a different Type]", id);
            yyerror(buf);
        } else {
            IDreplace(INFO(var), id, 0);
        }
    }
}

static Node *varNode(Node *qual, Node *cons, Node *var, Node *value) {

    char *id = LEFT_CHILD(RIGHT_CHILD(var))->value.s;
    int typ = INFO(qual) + INFO(cons) + INFO(var);
    Node *sz = RIGHT_CHILD(RIGHT_CHILD(var));

    if (!isVoid(value)) {
        if (isForw(typ)) {
            sprintf(buf, "[Forward Variable '%s' can not be initialized]", id);
            yyerror(buf);
        } else if (!SAME_TYPE(typ, value) && !(NAKED_TYPE(typ) == _ARRAY && isInt(value))) {
            sprintf(buf, "[Invalid Variable '%s' initialization Type]", id);
            yyerror(buf);
        } else if (isArray(var)) {
            if (OP_LABEL(sz) == NIL) {
                sprintf(buf, "[Array '%s' dimension must be specified when initialized]", id);
                yyerror(buf);
            } else if (sz->value.i < alen) {
                sprintf(buf, "[Invalid Array '%s' dimension: %d < %d]", id, sz->value.i, alen);
                yyerror(buf);
            }
        }
    } else if (!isForw(typ) && isConst(typ)) {
        sprintf(buf, "[Non Forward Constant '%s' is not initialized]", id);
        yyerror(buf);
    }

    INFO(var) = typ;
    varPut(var);
    decVar(typ, id, sz, value); 
    return binNode(VAR, binNode(V_LBL, qual, binNode(V_RDONLY, cons, var)), value);
}

static void checkArgs(char *id, Node *params, Node *args) {

    if (OP_LABEL(params) == NIL && OP_LABEL(args) == NIL) return;

    while (OP_LABEL(params) != NIL && OP_LABEL(args) != NIL) {
        
        if (!SAME_TYPE(INFO(RIGHT_CHILD(params)), RIGHT_CHILD(args))) {
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

static void funcPut(int typF, char *id, Node *params) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("Out of Memory"); exit(2); }
    int typ = IDsearch(id, (void **)p, 1, 1);

    if (typ == -1) {
        IDinsert(IDlevel() - 1, typF, id, params);
    } else {
        if (!isFunction(typ)) {
            sprintf(buf, "[Variable named '%s' already declared]", id);
            yyerror(buf);
        } else if (!isForw(typ)) {
            sprintf(buf, "[Function '%s' already defined]", id);
            yyerror(buf);
        } else if (NAKED_TYPE(typ) != NAKED_TYPE(typF)) {
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

    return binNodeT(OP_LABEL(ptr) == FETCH ? LINDEX : RINDEX, ptr, expr, _INT);
}

static int isLV(Node *ptr) {

    return OP_LABEL(ptr) == FETCH || OP_LABEL(ptr) == LINDEX;
}

static Node *callNode(char *id, Node *args) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("[Out of Memory]"); exit(2); }
    Node *idN = findID(id, (void **)p);

    if (isFunction(INFO(idN))) {
        checkArgs(id, *p, args);
    } else {
        sprintf(buf, "[The Identifier '%s' must be a Function]", id);
        yyerror(buf);
    }
    free(p);
    return binNodeT(CALL, idN, args, NAKED_TYPE(INFO(idN)));
}

char **yynames =
#if YYDEBUG > 0
    (char **)yyname;
#else
    NULL;
#endif
