#define _ARRAY 0
#define _INT 1
#define _STR 2
#define _VOID 3
#define NAKED_TYPE(a) (a % 4)
#define SAME_TYPE(a, p) (NAKED_TYPE(a) == NAKED_TYPE(PLACE(p)) || isNull(p))
#define isNull(p) (OP_LABEL(p) == INT && p->value.i == 0)
#define isArray(p) (NAKED_TYPE(PLACE(p)) == 0)
#define isInt(p) (NAKED_TYPE(PLACE(p)) == 1)
#define isStr(p) (NAKED_TYPE(PLACE(p)) == 2)
#define isVoid(p) (NAKED_TYPE(PLACE(p)) == 3)
#define _CONST 4
#define _FORWARD 8
#define _PUBLIC 0
#define _FUNCTION 16
#define isConst(a) ((a % 8) > 3)
#define isForward(a) ((a % 16) > 7)
#define isFunction(a) (a > 15)
#define UC(c) ((unsigned char)c)

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
