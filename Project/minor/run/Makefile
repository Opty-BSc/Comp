.SUFFIXES: .asm
LIB=minor
CC=gcc -m32 # Produce elf32 Code in 64-bit Machines
CFLAGS=-g -Dunderscore -fno-stack-protector # When _stack_chk_fail in Ubuntu
RUNTIME=lib$(LIB).a # Stand-Alone Runtime
AS=nasm -felf32 # Assembler Command

all:: $(RUNTIME)

$(RUNTIME): dbl.o linux.o lib.o sys.o
	ar crl $@ $^

.asm.o:
	$(AS) $< -o $@

install:: $(RUNTIME)
	cp $(RUNTIME) ..

clean::
	rm -f $(RUNTIME) *.o a.out core
