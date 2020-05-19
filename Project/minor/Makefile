LANG=minor
LIB=lib
EXS=exs
UTIL=util
RUN=run
LEX=flex
YACC=byacc
BURG=pburg
CC=gcc
CFLAGS=-g -DYYDEBUG
AS=nasm -felf32
LD=ld -melf_i386
LDLIB=run/lib$(LANG).a

$(LANG): minor.l minor.y minor.brg
	make -C $(LIB)
	make -C $(RUN)
	$(LEX) -dl minor.l
	$(YACC) -dv minor.y
	$(BURG) -T minor.brg
	$(CC) $(CFLAGS) -o $(LANG) -I$(LIB) lex.yy.c y.tab.c yyselect.c -L$(LIB) -l$(UTIL)
	rm -f lex.yy.c y.tab.c y.tab.h y.output yyselect.c

%: %.asm
	$(AS) $*.asm
	$(LD) -o $@ $*.o $(LDLIB)
	rm -f $*.o

clean::
	make -C $(LIB) clean
	make -C $(RUN) clean
	rm -f $(LANG) *.asm *~ $(EXS)/*.asm $(EXS)/*~ lex.yy.c y.tab.c y.tab.h y.output yyselect.c *.o out
