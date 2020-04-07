%{
#include <string.h>
#include <limits.h>
#include "node.h"
#include "y.tab.h"

int strtoi(char *s, int b);
char *dupstr(const char *s);
int yyerror(char *s);
%}
DECIMAL     (0|[1-9][0-9]*)
HEXA        [A-Fa-f0-9]+
BINARY      [01]+
OCTAL       [0-7]+
IDENTIFIER  [A-Za-z][A-Za-z0-9_]*
%x CODE
%x TEXT
%%

<INITIAL>^"program" BEGIN CODE; return PROGRAM;
<INITIAL>^"module"  BEGIN CODE; return MODULE;
<INITIAL>.|\n       ;/* Non Program / Module Content */

<CODE>^"end"        BEGIN INITIAL; return END;
<CODE>"$$".*\n      ;/* Explanatory Comments */
<CODE>"$"[^$]*"$"   ;/* Operational Comments */

<CODE>"start"       return START;
<CODE>"void"        return VOID;
<CODE>"const"       return CONST;
<CODE>"number"      return NUMBER;
<CODE>"array"       return ARRAY;
<CODE>"string"      return STRING;
<CODE>"function"    return FUNCTION;
<CODE>"public"      return PUBLIC;
<CODE>"forward"     return FORWARD;
<CODE>"if"          return IF;
<CODE>"then"        return THEN;
<CODE>"else"        return ELSE;
<CODE>"elif"        return ELIF;
<CODE>"fi"          return FI;
<CODE>"for"         return FOR;
<CODE>"until"       return UNTIL;
<CODE>"step"        return STEP;
<CODE>"do"          return DO;
<CODE>"done"        return DONE;
<CODE>"repeat"      return REPEAT;
<CODE>"stop"        return STOP;
<CODE>"return"      return RETURN;
<CODE>"<="          return LE;
<CODE>">="          return GE;
<CODE>"~="          return NE;
<CODE>":="          return ASSIGN;

<CODE>{IDENTIFIER}  { yylval.s = dupstr(yytext);         return ID; }
<CODE>{DECIMAL}     { yylval.i = strtoi(yytext, 10);     return INT; }
<CODE>0x{HEXA}      { yylval.i = strtoi(yytext + 2, 16); return INT; }
<CODE>0b{BINARY}    { yylval.i = strtoi(yytext + 2, 2);  return INT; }
<CODE>0{OCTAL}      { yylval.i = strtoi(yytext + 1, 8);  return INT; }

<CODE>\"        BEGIN TEXT;
<TEXT>\"        { yytext[yyleng-1] = 0; yylval.s = dupstr(yytext); BEGIN CODE; return STR; }
<TEXT>\\[n]     { yytext[yyleng-2] = '\n'; yytext[yyleng-1] = 0; yymore(); }
<TEXT>\\[t]     { yytext[yyleng-2] = '\t'; yytext[yyleng-1] = 0; yymore(); }
<TEXT>\\[r]     { yytext[yyleng-2] = '\r'; yytext[yyleng-1] = 0; yymore(); }
<TEXT>\\[\\"]   { yytext[yyleng-2] = yytext[yyleng-1]; yytext[yyleng-1] = 0; yymore(); }
<TEXT>[^\\]     { yymore(); }
<TEXT>.         { yyerror("Unknown Character"); }

<CODE>\'\\[n]\'     { yylval.c = '\n'; return CHAR; }
<CODE>\'\\[t]\'     { yylval.c = '\t'; return CHAR; }
<CODE>\'\\[r]\'     { yylval.c = '\r'; return CHAR; }
<CODE>\'(\\[\\']|[^\\])\'               { yylval.c = yytext[yyleng-2];       return CHAR; }
<CODE>\'\\[A-Fa-f0-9][A-Fa-f0-9]?\'     { yylval.c = strtoi(yytext + 2, 16); return CHAR; }

<CODE>[-+*/%^:=<>~|&~?#\[\]();:!,]      return *yytext;
<CODE>[\ \n\r\t]+   /* Whitespaces */   ;
<CODE>.             /*  Something  */   { yyerror("Unknown Character"); }

%%
int yywrap() {
    if (YYSTATE == CODE) yyerror("Unterminated Code Zone");
    if (YYSTATE == TEXT) yyerror("Unterminated String");
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
