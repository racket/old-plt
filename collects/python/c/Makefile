
MZC=/home/daniel/plt/bin/mzc
CC=$(MZC)
CFLAGS=++ccf -I. ++ccf -g --cc

OBJECTS=pscm.o listobject.o stringobject.o intobject.o longobject.o floatobject.o getargs.o cobject.o dictobject.o tupleobject.o object.o typeobject.o classobject.o pystate.o descrobject.o spy-modsupport.o abstract.o funcobject.o methodobject.o weakrefobject.o iterobject.o sliceobject.o structmember.o cellobject.o


fake-program: fake-program.c cpy-bindings.so
	gcc -o fake-program -I/home/daniel/plt/include fake-program.c cpy-bindings.so ~/plt/lib/libmzscheme.a ~/plt/lib/mzdyn.o ~/plt/lib/libmzgc.a -lm -ldl

cpy-bindings.so: $(OBJECTS)
	$(MZC) --ld cpy-bindings.so $(OBJECTS)

.c.o:
	$(CC) $(CFLAGS) $<

