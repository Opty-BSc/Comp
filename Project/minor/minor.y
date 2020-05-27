%{
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include "node.h"
#include "tabid.h"
#include "minor.h"
#define YYDEBUG 1
/* Declarations */
extern int trace;
extern int errors;
extern int yyerror(char *s);
extern int yyparse();
extern int yylex();
extern int yyselres;
extern void doMain(int enter, Node *body);
extern void doFunc(int typ, char *id, int enter, Node *body);
extern void decVar(int typ, char *id, Node *sz, Node *val);
extern void decExterns();
/* In File Declarations */
static Node *nilNodeT(int tok, int info);
static Node *uniNodeT(int tok, Node *left, int info);
static Node *binNodeT(int tok, Node *left, Node *right, int info);
static void varPut(Node *var);
static void checkArgs(char *id, Node *params, Node *args);
static void funcPut(int typF, char *id, Node *params);
static Node *findID(char *id, void **attrib);
static Node *idxNode(Node *ptr, Node *expr);
static int isLV(Node *ptr);
static Node *callNode(char *id, Node *args);
static void evaluate(Node *p);
/* Global Variabes */
static int inMain;
static int alen;
static int blck;
static int cicl;
static int pos;
static int retType = _INT;
static char buf[120] = "";
static int posbuf[80];
static int poscnt;
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
%nonassoc PTR UMINUS '?'
%nonassoc '(' ')' '[' ']'

%token <i> INT
%token <c> CHAR
%token <s> ID STR
%token PROGRAM MODULE END START
%token VOID CONST NUMBER ARRAY STRING FUNCTION PUBLIC FORWARD
%token IF THEN ELSE ELIF FI FOR UNTIL STEP DO DONE REPEAT STOP RETURN WHILE

%type <n> program module dSEQOPT dSEQ declaration
%type <n> variable vDimOPT vInitOPT literal literalSEQ integerSEQ
%type <n> function fParamsOPT fParams fBody body vSEQ
%type <n> iBlock iSEQOPT iSEQ instruction iElifSEQ iElif iElse iSugar iLast
%type <n> rValueOPT lValue rValue rArgs
%type <n> qualifier constant type fType

%token NIL DECL DECLS INTS LITERALS LITSEQ JMP DOW DOW_BODY
%token VAR V_LBL V_RDONLY V_TYPE V_ID V_DIM INIT FUNC F_HEAD F_LBL F_ID PARAMS F_BODY
%token CONDITION ELIFS ELSES INSTRS BLOCK EXPR ARGS LINDEX RINDEX
%token BODY VARS FETCH LOCAL ADDR LOAD CALL ERROR
%%

file        : { IDpush(); } program { IDpop(); evaluate($2); freeNode($2); }
            | { IDpush(); } module  { IDpop(); evaluate($2); freeNode($2); }
            ;

program     : PROGRAM dSEQOPT START { IDpush(); inMain = 1; }
              body { inMain = 0; doMain(-pos, $5); poscnt = pos = 0; IDpop(); }
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

variable    : type ID vDimOPT       {
    $$ = binNodeT(V_TYPE, $1, binNode(V_ID, strNode(ID, $2), $3), INFO($1));
    if (OP_LABEL($1) != ARRAY && OP_LABEL($3) != NIL) yyerror("[Invalid Variable Type to specify its dimension]");
}           ;

type        : ARRAY                 { $$ = nilNodeT(ARRAY, _ARRAY); }
            | NUMBER                { $$ = nilNodeT(NUMBER, _INT); }
            | STRING                { $$ = nilNodeT(STRING, _STR); }
            ;

vDimOPT     :                       { $$ = nilNode(NIL); }
            | '[' INT ']'           { $$ = intNode(V_DIM, $2);
                                      if ($2 == 0) yyerror("[Array dimension must be > 0]"); }
            ;

vInitOPT    :                               { $$ = nilNodeT(NIL, _VOID); alen = 0; }
            | ASSIGN literal                { $$ = $2; alen = 1; }
            | ASSIGN literalSEQ literal     { $$ = binNodeT(LITERALS, $2, $3, _STR); }
            | ASSIGN integerSEQ ',' INT     { $$ = binNodeT(INTS, $2, intNode(INT, $4), _ARRAY); alen++; }
            ;

literal     : INT                   { $$ = intNode(INT, $1); INFO($$) = _INT; }
            | CHAR                  { $$ = intNode(CHAR, $1); INFO($$) = _INT; }
            | STR                   { $$ = strNode(STR, $1); INFO($$) = _STR; }
            ;

literalSEQ  : literal               { $$ = binNodeT(LITERALS, nilNode(NIL), $1, _STR); }
            | literalSEQ literal    { $$ = binNodeT(LITERALS, $1, $2, _STR); }
            ;

integerSEQ  : INT                   { $$ = binNodeT(INTS, nilNode(NIL), intNode(INT, $1), _ARRAY); alen = 1; }
            | integerSEQ ',' INT    { $$ = binNodeT(INTS, $1, intNode(INT, $3), _ARRAY); alen++; }
            ;

function    : FUNCTION qualifier fType ID   { retType = INFO($3); IDpush(); }
              fParamsOPT                    { funcPut(INFO($2) + retType + _FUNCTION, $4, $6); }
              fBody                         {
    $$ = binNode(FUNC, binNode(F_HEAD, binNode(F_LBL, $2, binNode(F_ID, $3, strNode(ID, $4))), $6), $8);
    if (OP_LABEL($2) == FORWARD && OP_LABEL($8) != DONE) yyerror("[Forward Function must not have a Body]");
    else if (OP_LABEL($2) != FORWARD && OP_LABEL($8) == DONE) yyerror("[Function with empty Body must be Forward]");
    else doFunc(INFO($2) + INFO($3), $4, -pos, $8);
    poscnt = pos = 0; retType = _INT; IDpop();
}           ;

fType       : type                  { $$ = $1; }
            | VOID                  { $$ = nilNodeT(VOID, _VOID); }
            ;

fParamsOPT  :                       { $$ = nilNode(NIL); }
            | fParams               { $$ = uniNode(PARAMS, $1); }
            ;

fParams     : variable              { pos = 8; varPut($1); pos += typeBytes(INFO($1)); $$ = binNode(VARS, $1, nilNode(NIL)); }
            | fParams ';' variable  { varPut($3); pos += typeBytes(INFO($3)); $$ = binNode(VARS, $3, $1); }
            ;

fBody       : DONE                  { $$ = nilNode(DONE); }
            | DO body               { $$ = $2; }
            ;

body        : vSEQ iSEQOPT iLast    { $$ = binNode(BODY, $1, binNode(BLOCK, $2, $3)); }
            ;

vSEQ        :                       { pos = 0; $$ = nilNodeT(NIL, pos); }
            | vSEQ variable ';'     { pos -= typeBytes(INFO($2)); varPut($2); $$ = binNode(VARS, $1, $2); }
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

instruction : IF rValue                                 { if (!isInt($2)) yyerror("['if' Condition Type must be an Integer]"); }
              THEN iBlock iElifSEQ iElse FI             { $$ = binNode(CONDITION, binNode(IF, binNode(JMP, nilNode(START), $2), uniNode(THEN, $5)), binNode(ELSES, $6, $7)); }
            | FOR rValue UNTIL rValue                   { if (!isInt($4)) yyerror("['until' Condition Type must be an Integer]"); }
              STEP rValue
              DO { cicl++; } iBlock { cicl--; } DONE    { $$ = binNode(FOR, $2, binNode(STEP, binNode(DO, binNode(UNTIL, nilNode(START), $4), $10), $7)); }
            | DO { cicl++; } iBlock { cicl--; }
              WHILE rValue ';'      {
    if (!isInt($6)) yyerror("['while' Condition Type must be an Integer]");
    $$ = binNode(DOW, nilNode(START), binNode(DOW_BODY, $3, $6));
}           | rValue iSugar         {
    $$ = binNode(EXPR, $1, $2);
    if (isVoid($1) && OP_LABEL($2) == '!') yyerror("[Void Expression can not be printed]");
}           | lValue '#' rValue ';' {
    $$ = binNode('#', $3, $1);
    if (!isLV($1)) yyerror("['#' Left-value must not be a Function]");
    else if (isConst(INFO($1))) yyerror("['#' Left-value must not be a Constant]");
    else if (isInt($1)) yyerror("['#' Left-value Type must be a Pointer]");
    else if (!isInt($3)) yyerror("['#' Expression Type must be an Integer]");
}           ;

iElifSEQ    :                   { $$ = nilNodeT(NIL, -1); }
            | iElifSEQ iElif    { $$ = binNodeT(ELIFS, $1, $2, INFO($1) + 1); }
            ;

iElif       : ELIF rValue       { if (!isInt($2)) yyerror("['elif' Condition Type must be an Integer]"); }
              THEN iBlock       { $$ = binNode(ELIF, binNode(JMP, nilNode(START), $2), uniNode(THEN, $5)); }
            ;

iElse       :                   { $$ = nilNode(NIL); }
            | ELSE iBlock       { $$ = uniNode(ELSE, $2); }
            ;

iSugar      : ';'               { $$ = nilNode(';'); }
            | '!'               { $$ = nilNode('!'); }
            ;

iLast       :                   { $$ = nilNode(NIL); }
            | REPEAT            {
    $$ = nilNode(REPEAT);
    if (!cicl) yyerror("[Repeat must appear inside of a cicle]");
}           | STOP              {
    $$ = nilNode(STOP);
    if (!cicl) yyerror("[Stop must appear inside of a cicle]");
}           | RETURN rValueOPT  {
    $$ = uniNode(RETURN, $2);
    if (!blck && inMain) yyerror("[Return must appear inside of a sub-block]");
    else if (retType == _VOID && OP_LABEL($2) != NIL) yyerror("[Return Expression not allowed in Void Functions]");
    else if (!SAME_TYPE(retType, $2)) yyerror("[Funciton Type != Return Type]");
}           ;

rValueOPT   :                   { $$ = nilNodeT(NIL, _VOID); }
            | rValue            { $$ = $1; }
            ;

lValue      : ID                {
    $$ = findID($1, NULL);
    if (isFunc(INFO($$))) { freeNode($$); $$ = callNode($1, nilNode(NIL)); }
}           | ID '[' rValue ']' {
    $$ = findID($1, NULL);
    if (isFunc(INFO($$))) { freeNode($$); $$ = idxNode(callNode($1, nilNode(NIL)), $3); }
    else $$ = idxNode($$, $3);
}           ;

rValue      : lValue                    { if (isLV($1)) $$ = uniNodeT(LOAD, $1, NAKED_TYPE(INFO($1)));
                                          else $$ = $1; }
            | literal                   { $$ = $1; }
            | literalSEQ literal        { $$ = binNodeT(LITSEQ, nilNode(START), binNodeT(LITERALS, $1, $2, _STR), _STR); }
            | ID '(' rArgs ')'          { $$ = callNode($1, $3); }
            | '(' rValue ')'            { $$ = $2; }
            | '?'                       { $$ = nilNodeT('?', _INT); }
            | '&' lValue %prec PTR      { $$ = uniNode(PTR, $2);
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
                                          else if (isArray($1) && isInt($3)) INFO($$) = _ARRAY;
                                          else if (isInt($1) && isArray($3)) {
                                            free($$); $$ = binNodeT('+', $3, $1, _ARRAY); }
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
            | lValue ASSIGN rValue      { $$ = binNode(ASSIGN, $3, $1);
                                          if (!isLV($1)) yyerror("[Functions can not be assigned ':=']");
                                          else if (isConst(INFO($1))) yyerror("[Constants can not be assigned ':=']");
                                          else if (SAME_TYPE(INFO($1), $3)) INFO($$) = INFO($1);
                                          else yyerror("[Invalid Types to ':=']"); }
            ;

rArgs       : rValue                    { $$ = binNode(ARGS, $1, nilNode(NIL)); }
            | rArgs ',' rValue          { $$ = binNode(ARGS, $3, $1); }
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
        if (pos) posbuf[poscnt] = pos;
        IDadd(INFO(var), id, pos ? posbuf + poscnt++ : 0);
    } else {
        if (isFunc(typ)) {
            sprintf(buf, "[Function named '%s' already declared]", id);
            yyerror(buf);
        } else if (!isForw(typ)) {
            sprintf(buf, "[Variable '%s' already defined]", id);
            yyerror(buf);
        } else if (isConst(typ) != isConst(INFO(var)) || NAKED_TYPE(typ) != NAKED_TYPE(INFO(var))) {
            sprintf(buf, "[Variable '%s' already declared with a different Type]", id);
            yyerror(buf);
        } else {
            if (pos) posbuf[poscnt] = pos;
            IDreplace(INFO(var), id, pos ? posbuf + poscnt++ : 0);
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
            if (OP_LABEL(sz) != NIL && sz->value.i < alen) {
                sprintf(buf, "[Invalid Array '%s' dimension: %d < %d]", id, sz->value.i, alen);
                yyerror(buf);
            }
        }
    }
    INFO(var) = typ;
    varPut(var);
    decVar(typ, id, sz, value); 
    return binNode(VAR, binNode(V_LBL, qual, binNode(V_RDONLY, cons, var)), value);
}

static void checkArgs(char *id, Node *params, Node *args) {

    if (OP_LABEL(params) == NIL && OP_LABEL(args) == NIL) return;

    while (OP_LABEL(params) != NIL && OP_LABEL(args) != NIL) {

        if (!SAME_TYPE(INFO(LEFT_CHILD(params)), LEFT_CHILD(args))) {
            sprintf(buf, "[Invalid Parameter Types to Function '%s']", id);
            yyerror(buf);
            return;
        }
        params = RIGHT_CHILD(params);
        args = RIGHT_CHILD(args);

        if (OP_LABEL(params) == NIL && OP_LABEL(args) == NIL) return;
    }

    sprintf(buf, "[Invalid Parameters to Function '%s']", id);
    yyerror(buf);
}

static void funcPut(int typF, char *id, Node *params) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("Out of Memory"); exit(2); }
    int typ = IDsearch(id, (void **)p, 1, 1);
    if (OP_LABEL(params) != NIL) params = LEFT_CHILD(params);

    if (typ == -1) {
        IDinsert(IDlevel() - 1, typF, id, params);
    } else {
        if (!isFunc(typ)) {
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

static Node *findID(char *id, void **attrib) {
    
    void **attr = attrib ? attrib : (void **)malloc(sizeof(void *));
    if (!attr) { yyerror("[Out of Memory]"); exit(2); }

    int typ = IDfind(id, attr);
    if (typ == -1) {
        sprintf(buf, "[Identifier '%s' is undefined]", id);
        yyerror(buf);
    }
    int tok = isFunc(typ) ? FETCH : (*attr ? LOCAL : ADDR);
    if (!attrib) free(attr);
    return uniNodeT(tok, strNode(ID, id), typ);
}

static Node *idxNode(Node *ptr, Node *expr) {

    if (isInt(ptr)) yyerror("[Number can not be Indexed]");
    else if (!isInt(expr)) yyerror("[Index Expression must be an Integer]");

    return binNodeT(isLV(ptr) ? LINDEX : RINDEX, ptr, expr, _INT);
}

static int isLV(Node *ptr) {

    switch (OP_LABEL(ptr)) {
        case LOCAL:
        case ADDR:
        case LINDEX:
            return 1;
        default:
            return 0;
    }
}

static Node *callNode(char *id, Node *args) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("[Out of Memory]"); exit(2); }
    Node *idN = findID(id, (void **)p);

    if (isFunc(INFO(idN))) {
        checkArgs(id, *p, args);
    } else {
        sprintf(buf, "[The Identifier '%s' must be a Function]", id);
        yyerror(buf);
    }
    free(p);
    return binNodeT(CALL, idN, args, NAKED_TYPE(INFO(idN)));
}

char **yynames =
#ifdef YYDEBUG
    (char**)yyname;
#else
    0;
#endif

static void evaluate(Node *p) {
    decExterns();
    if (!errors && trace) {
        printNode(p, stdout, yynames);
        if (!yyselres) printf("selection successful\n");
    }
}
