LIB=minor
OS=linux.o lib.o sys.o wrap.o printlns.o
CC=gcc -m32# Produce elf32 in 64 bit Machines
LD=ld -melf_i386# Produce elf32 in 64 bit Machines
CFLAGS=-g -Dunderscore -fno-stack-protector# When __stack_chk_fail in Ubuntu
BOOT=$(LIB).o# Bootstrap Runtime for a C Compiler
RUNTIME=lib$(LIB).a# Stand-Alone Runtime
AS=nasm -felf32

all: $(RUNTIME) $(BOOT)

$(RUNTIME): dbl.o char.o $(OS)
	ar crl $@ $?

$(BOOT): dbl.o char.o $(OS)
	$(LD) -r -o  $@ $^

dbl.o: dbl.c
	$(CC) $(CFLAGS) -DPOSTFIX -c $<

char.o: char.c
	$(CC) $(CFLAGS) -DPOSTFIX -c $<

.SUFFIXES: .asm

.asm.o:
	$(AS) $< -o $@

clean::
	rm -f $(RUNTIME) $(BOOT) *.o
