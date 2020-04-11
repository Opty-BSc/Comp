LANG=minor
LIB=lib
EXS=exs
UTIL=util
LEX=flex
YACC=byacc
CC=gcc
CFLAGS=-g -DYYDEBUG

$(LANG): scan.l gram.y
	make -C $(LIB)
	$(LEX) -dl scan.l
	$(YACC) -dv gram.y
	$(CC) $(CFLAGS) -o $(LANG) -I$(LIB) lex.yy.c y.tab.c -L$(LIB) -l$(UTIL)

clean::
	make -C $(LIB) clean
	rm -f $(LANG) lex.yy.c y.tab.c y.tab.h y.output yyselect.c *.out *.asm *~ exs/*.out exs/*.asm exs/*~
