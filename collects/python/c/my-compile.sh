COMPILE="/home/dsilva/plt/bin/mzc  ++ccf -I. --cc" && $COMPILE pscm.c && $COMPILE stringobject.c && $COMPILE getargs.c && ~/plt/bin/mzc --ld stringobject.so stringobject.o pscm.o getargs.o

