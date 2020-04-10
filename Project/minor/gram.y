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
#define P_TYPE 1
#define F_TYPE 2
#define C_TYPE 1
#define isArr(a) ((PLACE(a) % 4) == A_TYPE)
#define isInt(i) ((PLACE(i) % 4) == I_TYPE)
#define isStr(s) ((PLACE(s) % 4) == S_TYPE)
#define isVoid(v) ((PLACE(v) % 4) == V_TYPE)
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
%type <n> function fParamsOPT fParams fBody body vSEQ
%type <n> iBlock iSEQ instruction iElifSEQ iElif iElse iSugar iLast
%type <n> rValueOPT lValue rValue rArgs
%type <i> qualifier constant type fType

%token NIL DECL DECLS VAR VARS DIM INIT LITERALS INTS
%token CONDITION ELIFS ELSES INSTRS BLOCK EXPR
%token BODY FUNCTION_PARAMS FUNCTION_ATTR LOAD CALL INDEX
%%

file        : program   { if (!errors) printNode($1, 0, yynames); }
            | module    { if (!errors) printNode($1, 0, yynames); }
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
                                  if ($1 != A_TYPE && OP_LABEL($3) != NIL) yyerror("VARIABLE DIMEMSION: Invalid Type"); }
            ;

type        : ARRAY             { $$ = A_TYPE; }
            | NUMBER            { $$ = I_TYPE; }
            | STRING            { $$ = S_TYPE; }
            ;

vDimOPT     :                   { $$ = nilNode(NIL); }
            | '[' INT ']'       { $$ = intNode(DIM, $2); }
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

literals    : literal literal   { $$ = binNodeT(LITERALS, $2, binNodeT(LITERALS, $1, nilNode(NIL), S_TYPE), S_TYPE); cnt = 2; }
            | literals literal  { $$ = binNodeT(LITERALS, $1, $2, S_TYPE); cnt++; }
            ;

integers    : INT ',' INT       { $$ = binNodeT(INTS, intNode(INT, $3), binNodeT(INTS, intNode(INT, $1), nilNode(NIL), A_TYPE), A_TYPE); cnt = 2; }
            | integers ',' INT  { $$ = binNodeT(INTS, intNode(INT, $3), $1, A_TYPE); cnt++; }
            ;

function    : FUNCTION qualifier fType ID   { retType = $3; IDpush(); blck++; }
              fParamsOPT                    { FUNCput($2, $4, $6); }
              fBody                         { blck--; IDpop(); retType = I_TYPE;
                                              $$ = binNode(FUNCTION, binNode(FUNCTION_ATTR, strNode(ID, $4), $6), $8);
                                              if ($2 == F_TYPE && OP_LABEL($8) != DONE) yyerror("DECLARATION: Forward Function does not have a Body"); }
            ;

fType       : type                  { $$ = $1; }
            | VOID                  { $$ = V_TYPE; }
            ;

fParamsOPT  :                       { $$ = nilNode(NIL); }
            | fParams               { $$ = $1; }
            ;

fParams     : variable              { VARput(0, 0, $1); $$ = binNode(FUNCTION_PARAMS, $1, nilNode(NIL)); }
            | fParams ';' variable  { VARput(0, 0, $3); $$ = binNode(FUNCTION_PARAMS, $3, $1); }
            ;

fBody       : DONE                  { $$ = nilNode(DONE); }
            | DO vSEQ iSEQ iLast    { $$ = uniNode(DO, binNode(BODY, $2, binNode(BLOCK, $3, $4))); }
            ;

body        : vSEQ iBlock           { $$ = binNode(BODY, $1, $2); }
            ;

vSEQ        :                       { $$ = nilNode(NIL); }
            | vSEQ variable ';'     { VARput(0, 0, $2); $$ = binNode(VARS, $2, $1); }
            ;

iBlock      : { IDpush(); blck++; } iSEQ iLast { blck--; IDpop(); $$ = binNode(BLOCK, $2, $3); }
            ;

iSEQ        :                       { $$ = nilNode(NIL); }
            | iSEQ instruction      { $$ = binNode(INSTRS, $2, $1); }
            ;

instruction : rValue iSugar                             { $$ = binNode(EXPR, $1, $2); }
            | lValue '#' rValue ';'                     { $$ = binNode('#', $1, $3);
                                                          if (!isInt($3)) yyerror("STACK RESERVE: Invalid Expression Type"); }
            | IF rValue                                 { if (!isInt($2)) yyerror("IF: Invalid Condition Type"); }
              THEN iBlock iElifSEQ iElse FI             { $$ = binNode(CONDITION, binNode(IF, $2, uniNode(THEN, $5)), binNode(ELSES, $6, $7)); }
            | FOR rValue UNTIL rValue                   { if (!isInt($4)) yyerror("FOR: Invalid Until Expression Type"); }
              STEP rValue
              DO { cicl++; } iBlock { cicl--; } DONE    { $$ = binNode(FOR, $2, binNode(UNTIL, $4, binNode(STEP, $7, uniNode(DO, $10)))); }
            ;

iElifSEQ    :                           { $$ = nilNode(NIL); }
            | iElifSEQ iElif            { $$ = binNode(ELIFS, $2, $1); }
            ;

iElif       : ELIF rValue               { if (!isInt($2)) yyerror("ELIF: Invalid Condition Type"); }
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
                                          if (!cicl) yyerror("REPEAT: Outside of a Cicle"); }
            | STOP                      { $$ = nilNode(STOP);
                                          if (!cicl) yyerror("STOP: Outside of a Cicle"); }
            | RETURN rValueOPT          { $$ = uniNode(RETURN, $2);
                                          if (!blck) yyerror("RETURN: On Main Block");
                                          else if (!checkType(retType, PLACE($2))) yyerror("RETURN: Invalid Type"); }
            ;

rValueOPT   :                           { $$ = nilNodeT(NIL, V_TYPE); }
            | rValue                    { $$ = $1; }
            ;

lValue      : ID                        { $$ = IDNode($1); }
            | ID '[' rValue ']'         { $$ = idxNode($1, $3); }
            ;

rValue      : lValue                    { if (isFunc(PLACE($1))) $$ = $1;
                                          else $$ = uniNodeT(LOAD, $1, PLACE($1)); }
            | literal                   { $$ = $1; }
            | literals                  { $$ = $1; }
            | '(' rValue ')'            { $$ = $2; }
            | ID '(' rArgs ')'          { $$ = FUNCNode($1, $3); }
            | '?'                       { $$ = nilNodeT('?', I_TYPE); }
            | '&' lValue %prec ADDR     { $$ = uniNodeT(ADDR, $2, I_TYPE); }
            | '-' rValue %prec UMINUS   { $$ = uniNode(UMINUS, $2);
                                          if (isInt($2)) PLACE($$) = I_TYPE;
                                          else yyerror("UMINUS: Invalid Type"); }
            | rValue '^' rValue         { $$ = binNode('^', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("POWER: Invalid Types"); }
            | rValue '*' rValue         { $$ = binNode('*', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("MULTIPLICATION: Invalid Types"); }
            | rValue '/' rValue         { $$ = binNode('/', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("DIVISION: Invalid Types"); }
            | rValue '%' rValue         { $$ = binNode('%', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("POWER: Invalid Types"); }
            | rValue '+' rValue         { $$ = binNode('+', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else if (isInt($1) && isArr($3) || isArr($1) && isInt($3)) PLACE($$) = A_TYPE;
                                          else yyerror("ADDITION: Invalid Types"); }
            | rValue '-' rValue         { $$ = binNode('-', $1, $3);
                                          if (isInt($1) && isInt($3) || isArr($1) && isArr($3)) PLACE($$) = I_TYPE;
                                          else if (isInt($1) && isArr($3) || isArr($1) && isInt($3)) PLACE($$) = A_TYPE;
                                          else yyerror("SUBTRACTION: Invalid Types"); }
            | rValue '<' rValue         { $$ = binNode('<', $1, $3);
                                          if (isInt($1) && isInt($3) || isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("LESS: Invalid Types"); }
            | rValue '>' rValue         { $$ = binNode('>', $1, $3);
                                          if (isInt($1) && isInt($3) || isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("GREATER: Invalid Types"); }
            | rValue LE rValue          { $$ = binNode(LE, $1, $3);
                                          if (isInt($1) && isInt($3) || isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("LESS OR EQUAL: Invalid Types"); }
            | rValue GE rValue          { $$ = binNode(GE, $1, $3);
                                          if (isInt($1) && isInt($3) || isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("GREATER OR EQUAL: Invalid Types"); }
            | rValue NE rValue          { $$ = binNode(NE, $1, $3);
                                          if (isInt($1) && isInt($3) || isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("NOT EQUAL: Invalid Types"); }
            | rValue '=' rValue         { $$ = binNode('=', $1, $3);
                                          if (isInt($1) && isInt($3) || isStr($1) && isStr($3)) PLACE($$) = I_TYPE;
                                          else yyerror("EQUAL: Invalid Types"); }
            | '~' rValue                { $$ = uniNode('~', $2);
                                          if (isInt($2)) PLACE($$) = I_TYPE;
                                          else yyerror("POWER: Invalid Type"); }
            | rValue '&' rValue         { $$ = binNode('&', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("POWER: Invalid Types"); }
            | rValue '|' rValue         { $$ = binNode('|', $1, $3);
                                          if (isInt($1) && isInt($3)) PLACE($$) = I_TYPE;
                                          else yyerror("POWER: Invalid Types"); }
            | lValue ASSIGN rValue      { $$ = binNode(ASSIGN, $1, $3);
                                          if (isFunc(PLACE($1))) yyerror("ASSIGN: Functions can not be assigned");
                                          else if (isCons(PLACE($1))) yyerror("ASSIGN: Constants can not be assigned");
                                          else if (OP_LABEL($3) == INT && $3->value.i == 0) PLACE($$) = PLACE($1);
                                          else if (checkType(PLACE($1), PLACE($3))) PLACE($$) = PLACE($1);
                                          else yyerror("ASSIGN: Invalid Types"); }
            ;

rArgs       : rValue                    { $$ = binNode(FUNCTION_PARAMS, $1, nilNode(NIL)); }
            | rArgs ',' rValue          { $$ = binNode(FUNCTION_PARAMS, $3, $1); }
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

Node *findID(char *id, void **attr) {

    int typ = IDfind(id, attr, 0);
    Node *s = strNode(ID, id);
    PLACE(s) = typ;
    return s;
}

void VARput(int qual, int cons, Node *var) {

    int typ = IDfind(LEFT_CHILD(var)->value.s, (void **)IDtest, 0);

    if (typ == -1) {
        IDadd(VType(qual, cons, PLACE(var)), LEFT_CHILD(var)->value.s, 0);
    } else {
        if (isFunc(typ)) {
            yyerror("DECLARATION: Function already defined with the same ID");
        } else if (!isForw(typ)) {
            yyerror("DECLARATION: Variable already defined with the same ID");
        } else if (isCons(typ) != (cons == C_TYPE) || !sameType(typ, PLACE(var))) {
            yyerror("DECLARATION: Incompatible Variable Types");
        }
        IDreplace(VType(qual, cons, PLACE(var)), LEFT_CHILD(var)->value.s, 0);
    }
}

Node *VARNode(int qual, int cons, Node *var, Node *init) {

    if (!isVoid(init)) {
        if (qual == F_TYPE) {
            yyerror("DECLARATION: Forward Variable can not be initialized");
        } else if (!checkType(PLACE(var), PLACE(init))) {
            yyerror("DECLARATION: Invalid Variable Type");
        } else if (OP_LABEL(RIGHT_CHILD(var)) != NIL && cnt > RIGHT_CHILD(var)->value.i) {
            yyerror("DECLARATION: Invalid Variable init Dimension");
        }
    } else if (qual != F_TYPE && cons == C_TYPE) {
        yyerror("DECLARATION: Constant is not initialized!");
    }
    VARput(qual, cons, var);
    return binNode(DECL, var, init);
}

void checkArgs(Node *p, Node *args) {

    if (OP_LABEL(p) != NIL && OP_LABEL(args) != NIL) {
        do {
            if (!sameType(PLACE(LEFT_CHILD(p)), PLACE(LEFT_CHILD(args)))) {
                yyerror("FUNCTION: Invalid Types of Arguments");
                break;
            }
            p = RIGHT_CHILD(p);
            args = RIGHT_CHILD(args);
            if (OP_LABEL(p) != OP_LABEL(args)) {
                yyerror("FUNCTION: Invalid Number of Arguments");
                break;
            }
        } while (OP_LABEL(p) != NIL && OP_LABEL(args) != NIL);

    } else if (OP_LABEL(p) != OP_LABEL(args)) {
        yyerror("FUNCTION: Invalid Number of Arguments");
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
            yyerror("DECLARATION: Variable already defined with the same ID");
        } else if (!isForw(typ)) {
            yyerror("DECLARATION: Function already defined with the same ID");
        } else if (!sameType(typ, retType)) {
            yyerror("DECLARATION: Incompatible Function Types");
        }
        checkArgs(*p, params);
        IDchange(FType(qual), id, params, 1);
    }
    free(p);
}

Node *idxNode(char *id, Node *expr) {

    Node *s = findID(id, (void **)IDtest);
    if (isFunc(PLACE(s))) {
        yyerror("INDEXING: The Identifier is not a Variable");
    }
    else if (!isInt(expr)) {
        yyerror("INDEXING: Index Expression is not an Integer");
    }
    else if (!isArr(s) && !isStr(s)) {
        yyerror("INDEXING: Variable can not be Indexed");
    }
    return binNodeT(INDEX, s, expr, I_TYPE);
}

Node *IDNode(char *id) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("Out of Memory"); exit(2); }
    Node *s = findID(id, (void **)p);

    if (!isFunc(PLACE(s))) { free(p); return s; }

    checkArgs(*p, nilNode(NIL));
    free(p);
    return binNodeT(CALL, s, nilNode(NIL), PLACE(s));
}

Node *FUNCNode(char *id, Node *args) {

    Node **p = (Node **)malloc(sizeof(Node *));
    if (p == NULL) { yyerror("Out of Memory"); exit(2); }
    Node *s = findID(id, (void **)p);

    if (!isFunc(PLACE(s))) {
        yyerror("FUNCTION CALL: The Identifier is not Function");
    }
    checkArgs(*p, args);
    free(p);
    return binNodeT(CALL, s, args, PLACE(s));
}

char **yynames =
#if YYDEBUG > 0
    (char **)yyname;
#else
    NULL;
#endif
