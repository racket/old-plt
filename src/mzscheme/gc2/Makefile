
XFORM = mzscheme -r xform.ss
XSRCDIR = xsrc
SRCDIR = ../src
OBJDIR = objects/sys_i386_linux

XMAKE = make

OBJS =  $(OBJDIR)/salloc.o \
	$(OBJDIR)/bignum.o \
	$(OBJDIR)/bool.o \
	$(OBJDIR)/char.o \
	$(OBJDIR)/complex.o \
	$(OBJDIR)/dynext.o \
	$(OBJDIR)/env.o \
	$(OBJDIR)/error.o \
	$(OBJDIR)/eval.o \
	$(OBJDIR)/file.o \
	$(OBJDIR)/fun.o \
	$(OBJDIR)/hash.o \
	$(OBJDIR)/image.o \
	$(OBJDIR)/list.o \
	$(OBJDIR)/number.o \
        $(OBJDIR)/object.o \
	$(OBJDIR)/port.o \
	$(OBJDIR)/print.o \
	$(OBJDIR)/process.o \
	$(OBJDIR)/promise.o \
        $(OBJDIR)/rational.o \
	$(OBJDIR)/read.o \
	$(OBJDIR)/regexp.o \
	$(OBJDIR)/sema.o \
	$(OBJDIR)/setjmpup.o \
	$(OBJDIR)/string.o \
	$(OBJDIR)/struct.o \
	$(OBJDIR)/symbol.o \
	$(OBJDIR)/syntax.o \
	$(OBJDIR)/type.o \
        $(OBJDIR)/unit.o \
	$(OBJDIR)/vector.o

XSRCS = $(XSRCDIR)/salloc.c \
	$(XSRCDIR)/bignum.c \
	$(XSRCDIR)/bool.c \
	$(XSRCDIR)/char.c \
	$(XSRCDIR)/complex.c \
	$(XSRCDIR)/dynext.c \
	$(XSRCDIR)/env.c \
	$(XSRCDIR)/error.c \
	$(XSRCDIR)/eval.c \
	$(XSRCDIR)/file.c \
	$(XSRCDIR)/fun.c \
	$(XSRCDIR)/hash.c \
	$(XSRCDIR)/image.c \
	$(XSRCDIR)/list.c \
	$(XSRCDIR)/number.c \
        $(XSRCDIR)/object.c \
	$(XSRCDIR)/port.c \
	$(XSRCDIR)/print.c \
	$(XSRCDIR)/process.c \
	$(XSRCDIR)/promise.c \
        $(XSRCDIR)/rational.c \
	$(XSRCDIR)/read.c \
	$(XSRCDIR)/regexp.c \
	$(XSRCDIR)/sema.c \
	$(XSRCDIR)/setjmpup.c \
	$(XSRCDIR)/string.c \
	$(XSRCDIR)/struct.c \
	$(XSRCDIR)/symbol.c \
	$(XSRCDIR)/syntax.c \
	$(XSRCDIR)/type.c \
        $(XSRCDIR)/unit.c \
	$(XSRCDIR)/vector.c

all: objects $(OBJDIR)
	$(XMAKE) xobjects

xsrcs: $(XSRCS)

$(OBJDIR):
	mkdir $(OBJDIR)
objects:
	mkdir objects

xobjects: $(OBJS)

$(XSRCDIR)/salloc.c: $(SRCDIR)/$(OBJDIR)/salloc.o
	$(XFORM) $(SRCDIR)/salloc.c $(XSRCDIR)/salloc.c
$(XSRCDIR)/bignum.c: $(SRCDIR)/$(OBJDIR)/bignum.o
	$(XFORM) $(SRCDIR)/bignum.c $(XSRCDIR)/bignum.c
$(XSRCDIR)/bool.c: $(SRCDIR)/$(OBJDIR)/bool.o
	$(XFORM) $(SRCDIR)/bool.c $(XSRCDIR)/bool.c
$(XSRCDIR)/char.c: $(SRCDIR)/$(OBJDIR)/char.o
	$(XFORM) $(SRCDIR)/char.c $(XSRCDIR)/char.c
$(XSRCDIR)/complex.c: $(SRCDIR)/$(OBJDIR)/complex.o
	$(XFORM) $(SRCDIR)/complex.c $(XSRCDIR)/complex.c
$(XSRCDIR)/dynext.c: $(SRCDIR)/$(OBJDIR)/dynext.o
	$(XFORM) $(SRCDIR)/dynext.c $(XSRCDIR)/dynext.c
$(XSRCDIR)/env.c: $(SRCDIR)/$(OBJDIR)/env.o
	$(XFORM) $(SRCDIR)/env.c $(XSRCDIR)/env.c
$(XSRCDIR)/error.c: $(SRCDIR)/$(OBJDIR)/error.o
	$(XFORM) $(SRCDIR)/error.c $(XSRCDIR)/error.c
$(XSRCDIR)/eval.c: $(SRCDIR)/$(OBJDIR)/eval.o
	$(XFORM) $(SRCDIR)/eval.c $(XSRCDIR)/eval.c
$(XSRCDIR)/file.c: $(SRCDIR)/$(OBJDIR)/file.o
	$(XFORM) $(SRCDIR)/file.c $(XSRCDIR)/file.c
$(XSRCDIR)/fun.c: $(SRCDIR)/$(OBJDIR)/fun.o
	$(XFORM) $(SRCDIR)/fun.c $(XSRCDIR)/fun.c
$(XSRCDIR)/hash.c: $(SRCDIR)/$(OBJDIR)/hash.o
	$(XFORM) $(SRCDIR)/hash.c $(XSRCDIR)/hash.c
$(XSRCDIR)/image.c: $(SRCDIR)/$(OBJDIR)/image.o
	$(XFORM) $(SRCDIR)/image.c $(XSRCDIR)/image.c
$(XSRCDIR)/list.c: $(SRCDIR)/$(OBJDIR)/list.o
	$(XFORM) $(SRCDIR)/list.c $(XSRCDIR)/list.c
$(XSRCDIR)/number.c: $(SRCDIR)/$(OBJDIR)/number.o
	$(XFORM) $(SRCDIR)/number.c $(XSRCDIR)/number.c
$(XSRCDIR)/object.c: $(SRCDIR)/$(OBJDIR)/object.o
	$(XFORM) $(SRCDIR)/object.c $(XSRCDIR)/object.c
$(XSRCDIR)/port.c: $(SRCDIR)/$(OBJDIR)/port.o
	$(XFORM) $(SRCDIR)/port.c $(XSRCDIR)/port.c
$(XSRCDIR)/print.c: $(SRCDIR)/$(OBJDIR)/print.o
	$(XFORM) $(SRCDIR)/print.c $(XSRCDIR)/print.c
$(XSRCDIR)/process.c: $(SRCDIR)/$(OBJDIR)/process.o
	$(XFORM) $(SRCDIR)/process.c $(XSRCDIR)/process.c
$(XSRCDIR)/promise.c: $(SRCDIR)/$(OBJDIR)/promise.o
	$(XFORM) $(SRCDIR)/promise.c $(XSRCDIR)/promise.c
$(XSRCDIR)/rational.c: $(SRCDIR)/$(OBJDIR)/rational.o
	$(XFORM) $(SRCDIR)/rational.c $(XSRCDIR)/rational.c
$(XSRCDIR)/read.c: $(SRCDIR)/$(OBJDIR)/read.o
	$(XFORM) $(SRCDIR)/read.c $(XSRCDIR)/read.c
$(XSRCDIR)/regexp.c: $(SRCDIR)/$(OBJDIR)/regexp.o
	$(XFORM) $(SRCDIR)/regexp.c $(XSRCDIR)/regexp.c
$(XSRCDIR)/sema.c: $(SRCDIR)/$(OBJDIR)/sema.o
	$(XFORM) $(SRCDIR)/sema.c $(XSRCDIR)/sema.c
$(XSRCDIR)/setjmpup.c: $(SRCDIR)/$(OBJDIR)/setjmpup.o
	$(XFORM) $(SRCDIR)/setjmpup.c $(XSRCDIR)/setjmpup.c
$(XSRCDIR)/string.c: $(SRCDIR)/$(OBJDIR)/string.o
	$(XFORM) $(SRCDIR)/string.c $(XSRCDIR)/string.c -I$(OBJDIR)
$(XSRCDIR)/struct.c: $(SRCDIR)/$(OBJDIR)/struct.o
	$(XFORM) $(SRCDIR)/struct.c $(XSRCDIR)/struct.c
$(XSRCDIR)/symbol.c: $(SRCDIR)/$(OBJDIR)/symbol.o
	$(XFORM) $(SRCDIR)/symbol.c $(XSRCDIR)/symbol.c
$(XSRCDIR)/syntax.c: $(SRCDIR)/$(OBJDIR)/syntax.o
	$(XFORM) $(SRCDIR)/syntax.c $(XSRCDIR)/syntax.c
$(XSRCDIR)/type.c: $(SRCDIR)/$(OBJDIR)/type.o
	$(XFORM) $(SRCDIR)/type.c $(XSRCDIR)/type.c
$(XSRCDIR)/unit.c: $(SRCDIR)/$(OBJDIR)/unit.o
	$(XFORM) $(SRCDIR)/unit.c $(XSRCDIR)/unit.c
$(XSRCDIR)/vector.c: $(SRCDIR)/$(OBJDIR)/vector.o
	$(XFORM) $(SRCDIR)/vector.c $(XSRCDIR)/vector.c


$(OBJDIR)/salloc.o: $(XSRCDIR)/salloc.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/salloc.c -o $(OBJDIR)/salloc.o
$(OBJDIR)/bignum.o: $(XSRCDIR)/bignum.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/bignum.c -o $(OBJDIR)/bignum.o
$(OBJDIR)/bool.o: $(XSRCDIR)/bool.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/bool.c -o $(OBJDIR)/bool.o
$(OBJDIR)/char.o: $(XSRCDIR)/char.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/char.c -o $(OBJDIR)/char.o
$(OBJDIR)/complex.o: $(XSRCDIR)/complex.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/complex.c -o $(OBJDIR)/complex.o
$(OBJDIR)/dynext.o: $(XSRCDIR)/dynext.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/dynext.c -o $(OBJDIR)/dynext.o
$(OBJDIR)/env.o: $(XSRCDIR)/env.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/env.c -o $(OBJDIR)/env.o
$(OBJDIR)/error.o: $(XSRCDIR)/error.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/error.c -o $(OBJDIR)/error.o
$(OBJDIR)/eval.o: $(XSRCDIR)/eval.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/eval.c -o $(OBJDIR)/eval.o
$(OBJDIR)/file.o: $(XSRCDIR)/file.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/file.c -o $(OBJDIR)/file.o
$(OBJDIR)/fun.o: $(XSRCDIR)/fun.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/fun.c -o $(OBJDIR)/fun.o
$(OBJDIR)/hash.o: $(XSRCDIR)/hash.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/hash.c -o $(OBJDIR)/hash.o
$(OBJDIR)/image.o: $(XSRCDIR)/image.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/image.c -o $(OBJDIR)/image.o
$(OBJDIR)/list.o: $(XSRCDIR)/list.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/list.c -o $(OBJDIR)/list.o
$(OBJDIR)/number.o: $(XSRCDIR)/number.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/number.c -o $(OBJDIR)/number.o
$(OBJDIR)/object.o: $(XSRCDIR)/object.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/object.c -o $(OBJDIR)/object.o
$(OBJDIR)/port.o: $(XSRCDIR)/port.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/port.c -o $(OBJDIR)/port.o
$(OBJDIR)/print.o: $(XSRCDIR)/print.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/print.c -o $(OBJDIR)/print.o
$(OBJDIR)/process.o: $(XSRCDIR)/process.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/process.c -o $(OBJDIR)/process.o
$(OBJDIR)/promise.o: $(XSRCDIR)/promise.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/promise.c -o $(OBJDIR)/promise.o
$(OBJDIR)/rational.o: $(XSRCDIR)/rational.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/rational.c -o $(OBJDIR)/rational.o
$(OBJDIR)/read.o: $(XSRCDIR)/read.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/read.c -o $(OBJDIR)/read.o
$(OBJDIR)/regexp.o: $(XSRCDIR)/regexp.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/regexp.c -o $(OBJDIR)/regexp.o
$(OBJDIR)/sema.o: $(XSRCDIR)/sema.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/sema.c -o $(OBJDIR)/sema.o
$(OBJDIR)/setjmpup.o: $(XSRCDIR)/setjmpup.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/setjmpup.c -o $(OBJDIR)/setjmpup.o
$(OBJDIR)/string.o: $(XSRCDIR)/string.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/string.c -o $(OBJDIR)/string.o
$(OBJDIR)/struct.o: $(XSRCDIR)/struct.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/struct.c -o $(OBJDIR)/struct.o
$(OBJDIR)/symbol.o: $(XSRCDIR)/symbol.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/symbol.c -o $(OBJDIR)/symbol.o
$(OBJDIR)/syntax.o: $(XSRCDIR)/syntax.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/syntax.c -o $(OBJDIR)/syntax.o
$(OBJDIR)/type.o: $(XSRCDIR)/type.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/type.c -o $(OBJDIR)/type.o
$(OBJDIR)/unit.o: $(XSRCDIR)/unit.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/unit.c -o $(OBJDIR)/unit.o
$(OBJDIR)/vector.o: $(XSRCDIR)/vector.c
	$(CC) $(CFLAGS) -c $(XSRCDIR)/vector.c -o $(OBJDIR)/vector.o

ctok: lex.yy.c
	gcc -g -o ctok lex.yy.c

lex.yy.c: ctok.lex
	lex ctok.lex
