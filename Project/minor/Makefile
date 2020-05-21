LANG=minor
EXT=min# File Extension: .$(EXT)
LIB=lib# Compiler Library Directory
UTIL=util# Compiler Library: lib$(LIB).a
RUN=run# Runtime Directory
EXS=exs# Examples Directory
CC=gcc
CFLAGS=-g -DYYDEBUG
AS=nasm -felf32
LD=ld -melf_i386
LDLIBS=lib$(LANG).a

$(LANG): $(LANG).y $(LANG).l $(LANG).brg
	make -C $(LIB)
	byacc -dv $(LANG).y
	flex -dl $(LANG).l
	pburg -T $(LANG).brg
	$(LINK.c) -o $(LANG) $(ARCH) -I$(LIB) lex.yy.c y.tab.c yyselect.c -L$(LIB) -l$(UTIL)
	make -C $(RUN)
	-cp $(RUN)/$(LDLIBS) .

%: %.asm
	$(AS) $*.asm
	$(LD) $*.o $(LDLIBS) -o out
	./out

clean::
	make -C $(LIB) clean
	make -C $(RUN) clean
	rm -f $(LANG) $(LDLIBS) *.o *.asm *~ lex.yy.c y.tab.c y.tab.h y.output yyselect.c out
	rm -f $(EXS)/*.o $(EXS)/*.asm $(EXS)/*~
