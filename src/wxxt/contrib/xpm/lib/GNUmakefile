#
#  Copyright (C) 1989-95 GROUPE BULL
#
#  Permission is hereby granted, free of charge, to any person obtaining a copy
#  of this software and associated documentation files (the "Software"), to
#  deal in the Software without restriction, including without limitation the
#  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
#  sell copies of the Software, and to permit persons to whom the Software is
#  furnished to do so, subject to the following conditions:
#
#  The above copyright notice and this permission notice shall be included in
#  all copies or substantial portions of the Software.
#
#  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
#  GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
#  AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#  Except as contained in this notice, the name of GROUPE BULL shall not be
#  used in advertising or otherwise to promote the sale, use or other dealings
#  in this Software without prior written authorization from GROUPE BULL.
#

PLTSRCDIR = ../../../..
include ../../../src/xtmake.env

XPMCC = $(CC)

XPM_CC = $(XPMCC) $(CFLAGS) -DZPIPE -c

OBJDIR = objects/sys$(MZPLATFORMSUFFIX)
LIBDIR = lib/sys$(MZPLATFORMSUFFIX)

OBJS= $(OBJDIR)/data.o $(OBJDIR)/create.o $(OBJDIR)/misc.o $(OBJDIR)/rgb.o \
      $(OBJDIR)/scan.o $(OBJDIR)/parse.o $(OBJDIR)/hashtab.o \
      $(OBJDIR)/WrFFrP.o $(OBJDIR)/RdFToP.o $(OBJDIR)/CrPFrDat.o $(OBJDIR)/CrDatFrP.o \
      $(OBJDIR)/WrFFrI.o $(OBJDIR)/RdFToI.o $(OBJDIR)/CrIFrDat.o $(OBJDIR)/CrDatFrI.o \
      $(OBJDIR)/CrIFrBuf.o $(OBJDIR)/CrPFrBuf.o $(OBJDIR)/CrBufFrI.o $(OBJDIR)/CrBufFrP.o \
      $(OBJDIR)/RdFToDat.o $(OBJDIR)/WrFFrDat.o \
      $(OBJDIR)/Attrib.o $(OBJDIR)/CrIFrP.o $(OBJDIR)/CrPFrI.o $(OBJDIR)/Image.o \
      $(OBJDIR)/Info.o $(OBJDIR)/RdFToBuf.o $(OBJDIR)/WrFFrBuf.o

all: $(LIBDIR)/libXpm.a

clean: 
	$(RM) $(OBJDIR)/*.o $(LIBDIR)/libXpm.a

$(LIBDIR)/libXpm.a: $(OBJS)
	$(AR) $(ARFLAGS) $(LIBDIR)/libXpm.a $(OBJS)
	$(RANLIB) $(LIBDIR)/libXpm.a

# Other dependencies.
$(OBJDIR)/CrBufFrI.o:
	$(XPM_CC) CrBufFrI.c -o $(OBJDIR)/CrBufFrI.o
$(OBJDIR)/CrBufFrP.o:
	$(XPM_CC) CrBufFrP.c -o $(OBJDIR)/CrBufFrP.o
$(OBJDIR)/CrDatFrI.o:
	$(XPM_CC) CrDatFrI.c -o $(OBJDIR)/CrDatFrI.o
$(OBJDIR)/CrDatFrP.o:
	$(XPM_CC) CrDatFrP.c -o $(OBJDIR)/CrDatFrP.o
$(OBJDIR)/CrDatFI.o:
	$(XPM_CC) CrDatFI.c -o $(OBJDIR)/CrDatFI.o
$(OBJDIR)/CrDatFP.o:
	$(XPM_CC) CrDatFP.c -o $(OBJDIR)/CrDatFP.o
$(OBJDIR)/CrIFrBuf.o:
	$(XPM_CC) CrIFrBuf.c -o $(OBJDIR)/CrIFrBuf.o
$(OBJDIR)/CrIFrDat.o:
	$(XPM_CC) CrIFrDat.c -o $(OBJDIR)/CrIFrDat.o
$(OBJDIR)/CrPFrBuf.o:
	$(XPM_CC) CrPFrBuf.c -o $(OBJDIR)/CrPFrBuf.o
$(OBJDIR)/CrPFrDat.o:
	$(XPM_CC) CrPFrDat.c -o $(OBJDIR)/CrPFrDat.o
$(OBJDIR)/RdFToDat.o:
	$(XPM_CC) RdFToDat.c -o $(OBJDIR)/RdFToDat.o
$(OBJDIR)/RdFToI.o:
	$(XPM_CC) RdFToI.c -o $(OBJDIR)/RdFToI.o
$(OBJDIR)/RdFToP.o:
	$(XPM_CC) RdFToP.c -o $(OBJDIR)/RdFToP.o
$(OBJDIR)/WrFFrDat.o:
	$(XPM_CC) WrFFrDat.c -o $(OBJDIR)/WrFFrDat.o
$(OBJDIR)/WrFFrI.o:
	$(XPM_CC) WrFFrI.c -o $(OBJDIR)/WrFFrI.o
$(OBJDIR)/WrFFrP.o:
	$(XPM_CC) WrFFrP.c -o $(OBJDIR)/WrFFrP.o
$(OBJDIR)/create.o:
	$(XPM_CC) create.c -o $(OBJDIR)/create.o
$(OBJDIR)/data.o:
	$(XPM_CC) data.c -o $(OBJDIR)/data.o
$(OBJDIR)/hashtab.o:
	$(XPM_CC) hashtab.c -o $(OBJDIR)/hashtab.o
$(OBJDIR)/misc.o:
	$(XPM_CC) misc.c -o $(OBJDIR)/misc.o
$(OBJDIR)/parse.o:
	$(XPM_CC) parse.c -o $(OBJDIR)/parse.o
$(OBJDIR)/rgb.o:
	$(XPM_CC) rgb.c -o $(OBJDIR)/rgb.o
$(OBJDIR)/scan.o:
	$(XPM_CC) scan.c -o $(OBJDIR)/scan.o
$(OBJDIR)/Attrib.o:
	$(XPM_CC) Attrib.c -o $(OBJDIR)/Attrib.o
$(OBJDIR)/CrIFrP.o:
	$(XPM_CC) CrIFrP.c -o $(OBJDIR)/CrIFrP.o
$(OBJDIR)/CrPFrI.o:
	$(XPM_CC) CrPFrI.c -o $(OBJDIR)/CrPFrI.o
$(OBJDIR)/Image.o:
	$(XPM_CC) Image.c -o $(OBJDIR)/Image.o
$(OBJDIR)/Info.o:
	$(XPM_CC) Info.c -o $(OBJDIR)/Info.o
$(OBJDIR)/RdFToBuf.o:
	$(XPM_CC) RdFToBuf.c -o $(OBJDIR)/RdFToBuf.o
$(OBJDIR)/WrFFrBuf.o:
	$(XPM_CC) WrFFrBuf.c -o $(OBJDIR)/WrFFrBuf.o
