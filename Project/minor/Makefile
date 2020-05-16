LANG=minor
LIB=lib
EXS=exs
UTIL=util
LEX=flex
YACC=byacc
BURG=pburg
CC=gcc
CFLAGS=-g -DYYDEBUG

$(LANG): minor.l minor.y minor.brg
	make -C $(LIB)
	$(LEX) -dl minor.l
	$(YACC) -dv minor.y
	$(BURG) -T minor.brg
	$(CC) $(CFLAGS) -o $(LANG) -I$(LIB) lex.yy.c y.tab.c yyselect.c -L$(LIB) -l$(UTIL)

clean::
	make -C $(LIB) clean
	rm -f $(LANG) lex.yy.c y.tab.c y.tab.h y.output yyselect.c *.out *.asm *~ exs/*.out exs/*.asm exs/*~
