LANG=minor
LIB=lib
EXS=exs
UTIL=util
LEX=flex
YACC=byacc
CC=gcc
CFLAGS=-DYYDEBUG

$(LANG): scan.l gram.y
	make -C $(LIB)
	$(LEX) -dl scan.l
	$(YACC) -dv gram.y
	$(CC) -o $(LANG) -I$(LIB) lex.yy.c y.tab.c -L$(LIB) -l$(UTIL) $(CFLAGS)

clean::
	make -C $(LIB) clean
	rm -f $(LANG) lex.yy.c y.tab.c y.tab.h y.output yyselect.c *.out *.asm *~
