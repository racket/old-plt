## Makefile for PLT MzScheme

TARGET = mzscheme
APPNAME = "PLT MzScheme"
APPID = "MzSc"

OBJS = $(TARGET).o
LIBS =

CC = m68k-palmos-coff-gcc

CFLAGS = -Wall -g -O2

PILRC = pilrc
OBJRES = m68k-palmos-coff-obj-res
NM = m68k-palmos-coff-nm
BUILDPRC = build-prc
PILOTXFER = pilot-xfer

MZOBJS = salloc.o \
         bignum.o \
         bool.o \
         char.o \
         complex.o \
         dynext.o \
         env.o \
         error.o \
         eval.o \
         file.o \
         fun.o \
         hash.o \
         image.o \
         list.o \
         port.o \
         print.o \
         process.o \
         promise.o \
         rational.o \
         read.o \
         regexp.o \
         sema.o \
         setjmpup.o \
         string.o \
         struct.o \
         symbol.o \
         syntax.o \
         type.o \
         unit.o \
         vector.o

all: $(TARGET).prc

.S.o:
	$(CC) $(TARGETFLAGS) -c $<

.c.s:
	$(CC) $(CSFLAGS) $<

$(TARGET).prc: code0000.$(TARGET).grc code0001.$(TARGET).grc data0000.$(TARGET).grc pref0000.$(TARGET).grc rloc0000.$(TARGET).grc bin.res
	$(BUILDPRC) $(TARGET).prc $(APPNAME) $(APPID) code0001.$(TARGET).grc code0000.$(TARGET).grc data0000.$(TARGET).grc *.bin pref0000.$(TARGET).grc rloc0000.$(TARGET).grc

code0000.$(TARGET).grc: $(TARGET)
	$(OBJRES) $(TARGET)

code0001.$(TARGET).grc: code0000.$(TARGET).grc

data0000.$(TARGET).grc: code0000.$(TARGET).grc

pref0000.$(TARGET).grc: code0000.$(TARGET).grc

rloc0000.$(TARGET).grc: code0000.$(TARGET).grc

bin.res: $(TARGET).rcp $(TARGET).pbm
	$(PILRC) $(TARGET).rcp .
	touch bin.res

$(TARGET): $(OBJS) $(MZOBJS)
	$(CC) $(CFLAGS) $(OBJS) $(MZOBJS) -o $(TARGET) $(LIBS)
	! $(NM) -u $(TARGET) | grep .

send: $(TARGET).prc
	$(PILOTXFER) -p /dev/cuaa0 -i $(TARGET).prc

depend:
	makedepend -Y -I. *.c

clean:
	-rm -f *.[oa] $(TARGET) *.bin bin.res *.grc Makefile.bak

veryclean: clean
	-rm -f $(TARGET).prc pilot.ram pilot.scratch

salloc.o: ../src/salloc.c
	$(CC) -I../include -I../src -c $<
bignum.o: ../src/bignum.c
	$(CC) -I../include -I../src -c $<
bool.o: ../src/bool.c
	$(CC) -I../include -I../src -c $<
char.o: ../src/char.c
	$(CC) -I../include -I../src -c $<
complex.o: ../src/complex.c
	$(CC) -I../include -I../src -c $<
dynext.o: ../src/dynext.c
	$(CC) -I../include -I../src -c $<
env.o: ../src/env.c
	$(CC) -I../include -I../src -c $<
error.o: ../src/error.c
	$(CC) -I../include -I../src -c $<
eval.o: ../src/eval.c
	$(CC) -I../include -I../src -c $<
file.o: ../src/file.c
	$(CC) -I../include -I../src -c $<
fun.o: ../src/fun.c
	$(CC) -I../include -I../src -c $<
hash.o: ../src/hash.c
	$(CC) -I../include -I../src -c $<
image.o: ../src/image.c
	$(CC) -I../include -I../src -c $<
list.o: ../src/list.c
	$(CC) -I../include -I../src -c $<
number.o: ../src/number.c
	$(CC) -I../include -I../src -c $<
object.o: ../src/object.c
	$(CC) -I../include -I../src -c $<
port.o: ../src/port.c
	$(CC) -I../include -I../src -c $<
print.o: ../src/print.c
	$(CC) -I../include -I../src -c $<
process.o: ../src/process.c
	$(CC) -I../include -I../src -c $<
promise.o: ../src/promise.c
	$(CC) -I../include -I../src -c $<
rational.o: ../src/rational.c
	$(CC) -I../include -I../src -c $<
read.o: ../src/read.c
	$(CC) -I../include -I../src -c $<
regexp.o: ../src/regexp.c
	$(CC) -I../include -I../src -c $<
sema.o: ../src/sema.c
	$(CC) -I../include -I../src -c $<
setjmpup.o: ../src/setjmpup.c
	$(CC) -I../include -I../src -c $<
string.o: ../src/string.c
	$(CC) -I../include -I../src -c $<
struct.o: ../src/struct.c
	$(CC) -I../include -I../src -c $<
symbol.o: ../src/symbol.c
	$(CC) -I../include -I../src -c $<
syntax.o: ../src/syntax.c
	$(CC) -I../include -I../src -c $<
type.o: ../src/type.c
	$(CC) -I../include -I../src -c $<
unit.o: ../src/unit.c
	$(CC) -I../include -I../src -c $<
vector.o: ../src/vector.c
	$(CC) -I../include -I../src -c $<

