# Symantec C++ makefile for the DIB library
# NOTE that peripheral libraries are now dealt in main wxWindows makefile.

WXDIR = $(WXWIN)
WXLIB = $(WXDIR)\lib\wx.lib
INCDIR = $(WXDIR)\include
MSWINC = $(INCDIR)\msw
BASEINC = $(INCDIR)\base

DIBDIR = $(WXDIR)\utils\dib
DIBINC = $(DIBDIR)
DIBLIB = $(DIBDIR)\dib.lib

CC=sc
CFLAGS = -o -ml -W -Dwx_msw

INCLUDE=$(BASEINC);$(MSWINC)

OBJS = dib.obj

.cc.obj:
	*$(CC) -c $(CFLAGS) -I$(INCLUDE) $<

$(DIBLIB): $(OBJS)
 	-del $(DIBLIB)
	*lib $(DIBLIB) y $(OBJS), nul;

$(OBJS): $(DIBINC)\dib.h

clean:
        -del *.obj
	-del $(DIBLIB)
