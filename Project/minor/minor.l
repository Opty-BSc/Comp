%{
#include <string.h>
#include <limits.h>
#include "node.h"
#include "y.tab.h"

int strtoi(char *s, int b);
char *dupstr(const char *s);
int yyerror(char *s);
%}
IDENTIFIER  [A-Za-z][A-Za-z0-9_]*
DECIMAL     (0|[1-9][0-9]*)
HEXA        [A-Fa-f0-9]+
BINARY      [01]+
OCTAL       [0-7]+
H           [A-Fa-f0-9]
%x CZ
%x PT
%x FIN
%%

<INITIAL,CZ>^"program"      BEGIN CZ; return PROGRAM;
<INITIAL,CZ>^"module"       BEGIN CZ; return MODULE;
<INITIAL>.|\n               ;/* Non Program / Module Content */

<CZ>^"end"                  BEGIN FIN; return END;
<FIN>.|\n                   return 0;

<CZ>"$$".*\n        ;/* Explanatory Comments */
<CZ>"$"[^$]*"$"     ;/* Operational Comments */

<CZ>"start"         return START;
<CZ>"void"          return VOID;
<CZ>"const"         return CONST;
<CZ>"number"        return NUMBER;
<CZ>"array"         return ARRAY;
<CZ>"string"        return STRING;
<CZ>"function"      return FUNCTION;
<CZ>"public"        return PUBLIC;
<CZ>"forward"       return FORWARD;
<CZ>"if"            return IF;
<CZ>"then"          return THEN;
<CZ>"else"          return ELSE;
<CZ>"elif"          return ELIF;
<CZ>"fi"            return FI;
<CZ>"for"           return FOR;
<CZ>"until"         return UNTIL;
<CZ>"step"          return STEP;
<CZ>"do"            return DO;
<CZ>"done"          return DONE;
<CZ>"repeat"        return REPEAT;
<CZ>"stop"          return STOP;
<CZ>"return"        return RETURN;
<CZ>"<="            return LE;
<CZ>">="            return GE;
<CZ>"~="            return NE;
<CZ>":="            return ASSIGN;

<CZ>{IDENTIFIER}    { yylval.s = dupstr(yytext);         return ID; }
<CZ>{DECIMAL}       { yylval.i = strtoi(yytext, 10);     return INT; }
<CZ>0x{HEXA}        { yylval.i = strtoi(yytext + 2, 16); return INT; }
<CZ>0b{BINARY}      { yylval.i = strtoi(yytext + 2, 2);  return INT; }
<CZ>0{OCTAL}        { yylval.i = strtoi(yytext + 1, 8);  return INT; }

<CZ>\"              BEGIN PT;
<PT>\"              { yytext[0] = 0; yylval.s = dupstr(yytext); BEGIN CZ; return STR; }
<PT>\\[n]           { yytext[0] = '\n'; yytext[1] = 0; yymore(); }
<PT>\\[t]           { yytext[0] = '\t'; yytext[1] = 0; yymore(); }
<PT>\\[r]           { yytext[0] = '\r'; yytext[1] = 0; yymore(); }
<PT>\\[\\"]         { yytext[0] = yytext[1]; yytext[1] = 0; yymore(); }
<CZ>\\{H}{H}        { yytext[0] = strtoi(yytext + 1, 16); yytext[1] = yytext[2] = 0; yymore(); }
<CZ>\\{H}           { yytext[0] = strtoi(yytext + 1, 16); yytext[1] = 0; yymore();}
<PT>.|\n            { yymore(); }

<CZ>\'\\[n]\'       { yylval.c = '\n'; return CHAR; }
<CZ>\'\\[t]\'       { yylval.c = '\t'; return CHAR; }
<CZ>\'\\[r]\'       { yylval.c = '\r'; return CHAR; }
<CZ>\'(\\[\\']|[^\\])\' { yylval.c = yytext[yyleng-2];       return CHAR; }
<CZ>\'\\{H}{H}?\'       { yylval.c = strtoi(yytext + 2, 16); return CHAR; }

<CZ>[-+*/%^:=<>~|&~?#\[\]();:!,]        return yytext[0];
<CZ>[\ \n\r\t]+     /* Whitespaces */   ;
<CZ>.               /*  Something  */   { yyerror("Unknown Character"); }

%%
int yywrap() {
    if (YYSTATE == CZ) yyerror("Unterminated Code Zone");
    if (YYSTATE == PT) yyerror("Unterminated Literal String");
    return 1;
}

char *getyytext() {
    return yytext;
}

int strtoi(char *s, int b) {
    long int l = strtol(s, NULL, b);
    if (l > INT_MAX) yyerror("Integer Overflow");
    return l;
}
