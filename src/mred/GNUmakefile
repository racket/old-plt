
WXDIR=../wxxt

PLTSRCDIR = ..
include $(WXDIR)/src/xtmake.env

# system name for installation
INSTALLSYS = `../../bin/archsys`

OBJDIR=objects/sys$(MZPLATFORMSUFFIX)$(GUISUFFIX)
MZOBJDIR=objects/sys$(MZPLATFORMSUFFIX)
LIBDIR=lib/sys$(MZPLATFORMSUFFIX)
MZLIBDIR=$(LIBDIR)$(MZTHREADSUFFIX)

MZSCHEMELDFLAGS = -L../mzscheme/$(LIBDIR)/
MZSCHEMELDLIBS = -lmzscheme -lgc

WXSCHEMELDFLAGS = -Lwxs/$(OBJDIR)
WXSCHEMELDLIBS = -lwxscheme

WXMELDFLAGS = -Lwxme/$(OBJDIR)
WXMELDLIBS = -lwxme

MREDLDFLAGS = $(LDFLAGS) $(MZSCHEMELDFLAGS) $(WXSCHEMELDFLAGS) $(WXMELDFLAGS)
MREDLDLIBS = $(MZSCHEMELDLIBS) $(WXSCHEMELDLIBS) $(WXMELDLIBS) -lwx_xt -lXpm -limage_xt $(LDLIBS) $(LDNONSTATICLIBS)

MREDOBJECTS = $(OBJDIR)/mred.o $(OBJDIR)/mredx.o $(OBJDIR)/wxGC.o

MRED = $(MRBUILDDIR)mred
MZSCHEME = ../mzscheme/$(MZLIBDIR)/libmzscheme.a ../mzscheme/$(MZLIBDIR)/libgc.a
WXSCHEME = wxs/$(OBJDIR)/libwxscheme.a
WXME = wxme/$(OBJDIR)/libwxme.a

INSTALLDEST=/home/scheme/plt

MRSTATIC = 
MRSTATIC_STUB =

MRCHECK =

WXINCDEP = $(WXDIR)/src/Windows/Window.h

# For SGIs and gcc 2.7.2, I needed a hack
# (not needed with CC)
SGILINKHACK =
# SGILINKHACK = $(OBJDIR)/sgilinkhack.o

EXPORT = CC='$(CC)' CXX='$(CXX)' AS='$(AS)' RANLIB='$(RANLIB)' DEBUGFLAGS='$(DEBUGFLAGS)' OPTIONS='$(OPTIONS)' THREADFLAGS='$(THREADFLAGS)' WARN='$(WARN)' OPT='$(OPT)' $(MZEXTRAEXPORT)

# Used for distribution
HIDEDIR = /home/mflatt/mredhide
FTPDIR = /home/scheme/FTP/plt/staging

all:  objects $(OBJDIR)
	cd ../mzscheme; $(MAKE) $(EXPORT) mzschemelib
	$(MAKE) wx
	if [ -f wrap/wrap.ss ] ; then $(MAKE) mred-wrap ; fi
	cd wxs; $(MAKE)
	cd wxme; $(MAKE)
	$(MAKE) $(MRED) MRCHECK='$(MRCHECK)'

mred-wrap:
	cd wrap; $(MAKE)

bin:
	$(MAKE) $(MRED)

checked:
	$(MAKE) all MRCHECK='$(OBJDIR)/checkm.o'

$(OBJDIR):
	mkdir $(OBJDIR)
objects:
	mkdir objects

$(MRED) : $(WXME) $(WXSCHEME) $(MZSCHEME) $(MREDOBJECTS) \
          $(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a \
          $(WXDIR)/utils/image/$(LIBDIR)/libimage_xt.a \
          $(MRSTATIC_STUB) $(MRCHECK)
	$(LINKER) $(MREDLDFLAGS) $(MRSTATIC) -o $(MRED) $(LDSTARTERS) $(MREDOBJECTS) $(MRCHECK) $(MREDLDLIBS) $(MRSTATIC_STUB) $(SGILINKHACK) $(LDENDERS)

ee-app: $(MRED) $(OBJDIR)/mred_ee.o
	if [ "$(EEAPP)" = '' ] ; then echo "ERROR: You must specify EEAPP" ; else $(LINKER) $(MREDLDFLAGS) $(MRSTATIC) -o $(EEAPP) $(LDSTARTERS) $(OBJDIR)/mred_ee.o $(OBJDIR)/mredx.o $(EEOBJECTS) $(MRCHECK) $(MREDLDLIBS) $(MRSTATIC_STUB) $(SGILINKHACK) $(LDENDERS) ; fi

sunos4-static: $(OBJDIR)/dl_stub.o
	$(MAKE) $(MRED) MRSTATIC=-static  MRSTATIC_STUB='$(OBJDIR)/dl_stub.o' LDNONSTATICLIBS=''

solaris-static:
	$(MAKE) $(MRED) MRSTATIC="-Wl,-Bstatic" LDNONSTATICLIBS='-Wl,-Bdynamic -ldl -Wl,-Bstatic'

linux-static:
	$(MAKE) $(MRED) MRSTATIC=--static LDNONSTATICLIBS='-ldl'

$(OBJDIR)/checkm.o: misc/checkm.c
	$(CC) $(CFLAGS) $(LOCALFLAGS) -c misc/checkm.c -o $(OBJDIR)/checkm.o

wx:
	cd $(WXDIR)/src/x; $(MAKE)

$(OBJDIR)/mred.o : mred.cxx wxme/wx_media.h wxme/wx_medad.h wxs/wxscheme.h ../mzscheme/cmdline.inc mred.h \
                   ../mzscheme/include/scheme.h ../mzscheme/src/schvers.h $(WXINCDEP) $(WXDIR)/src/Windows/Frame.h \
                   $(WXDIR)/src/DataStructures/Object.h ../wxcommon/wxGC.h ../wxcommon/wx_list.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c mred.cxx -o $(OBJDIR)/mred.o

$(OBJDIR)/mred_ee.o : $(OBJDIR)/mred.o
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -DSTANDALONE_WITH_EMBEDDED_EXTENSION -c mred.cxx -o $(OBJDIR)/mred_ee.o

ee-main:
	$(OBJDIR)/mred_ee.o

$(OBJDIR)/mredx.o : mredx.cxx ../mzscheme/include/scheme.h mred.h $(WXDIR)/src/Windows/Frame.h \
                    $(WXDIR)/src/DataStructures/Object.h ../wxcommon/wxGC.h ../wxcommon/wx_list.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c mredx.cxx -o $(OBJDIR)/mredx.o

$(OBJDIR)/wxGC.o : ../wxcommon/wxGC.cxx ../wxcommon/wxGC.h
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c ../wxcommon/wxGC.cxx -o $(OBJDIR)/wxGC.o

$(OBJDIR)/dl_stub.o: misc/dl_stub.c
	$(CC) $(CPPFLAGS) $(LOCALFLAGS) -c misc/dl_stub.c -o $(OBJDIR)/dl_stub.o

$(OBJDIR)/sgilinkhack.o:
	$(CXX) $(CPPFLAGS) $(LOCALFLAGS) -c misc/sgilinkhack.cxx -o $(OBJDIR)/sgilinkhack.o

$(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a:
	echo Updating $(WXDIR)/$(LIBDIR)/libwx$(GUISUFFIX).a
	$(MAKE) wx

depend:
	cd wxs; $(MAKE) depend
	cd ../mzscheme; $(MAKE) depend

TILDES=*~ ../collects/mrdemo/*~ demo/turtles/*~ demo/morph/*~ system/*~ collects/mred/*~ tests/*~ misc/*~ windows/*~ ../*~

clean-mred:
	cd $(WXDIR)/src/x; $(MAKE) clean
	rm -rf objects/*/*.o $(TILDES) core
	cd wxme; $(MAKE) clean
	cd wxs; $(MAKE) clean

clean:
	$(MAKE) clean-mred
	cd ../mzscheme; $(MAKE) clean

install:
	if [ ! -d ../../.bin ] ; then mkdir ../../.bin ; fi
	if [ ! -d ../../.bin/$(INSTALLSYS) ] ; then mkdir ../../.bin/$(INSTALLSYS) ; fi
	rm -f ../../.bin/$(INSTALLSYS)/mred
	cp $(MRED) ../../.bin/$(INSTALLSYS)/mred


################################
#          Precise GC          #
################################

XFORM = ../mzscheme/$(MZBUILDDIR)mzscheme -rq ../mzscheme/gc2/xform.ss "gcc -E -I../mzscheme/gc2 $(OPTIONS) $(GUI) $(INC) -I$(WXDIR)/src/XWidgets -I$(WXDIR)/src $(XINCLUDE)"

gc2/xxx: 
	gcc -E -H $(OPTIONS) $(GUI) -I../mzscheme/gc2 $(INC) -I$(WXDIR)/src/XWidgets -I$(WXDIR)/src $(XINCLUDE) -o gc2/xxx wxme/wx_media.cxx

gc2/Window.cc: $(WXDIR)/src/Windows/Window.cc
	$(XFORM) $< $@
gc2/Button.cc: $(WXDIR)/src/Windows/Button.cc
	$(XFORM) $< $@
gc2/Layout.cc: $(WXDIR)/src/Windows/Layout.cc
	$(XFORM) $< $@
gc2/MenuBar.cc: $(WXDIR)/src/Windows/MenuBar.cc
	$(XFORM) $< $@
gc2/RadioBox.cc: $(WXDIR)/src/Windows/RadioBox.cc
	$(XFORM) $< $@ 
gc2/Choice.cc: $(WXDIR)/src/Windows/Choice.cc
	$(XFORM) $< $@
gc2/Gauge.cc: $(WXDIR)/src/Windows/Gauge.cc
	$(XFORM) $< $@  
gc2/ListBox.cc: $(WXDIR)/src/Windows/ListBox.cc
	$(XFORM) $< $@
gc2/Message.cc: $(WXDIR)/src/Windows/Message.cc
	$(XFORM) $< $@
gc2/Slider.cc: $(WXDIR)/src/Windows/Slider.cc
	$(XFORM) $< $@
gc2/Canvas.cc: $(WXDIR)/src/Windows/Canvas.cc
	$(XFORM) $< $@
gc2/DialogBox.cc: $(WXDIR)/src/Windows/DialogBox.cc
	$(XFORM) $< $@ 
gc2/Item.cc: $(WXDIR)/src/Windows/Item.cc
	$(XFORM) $< $@
gc2/Menu.cc: $(WXDIR)/src/Windows/Menu.cc
	$(XFORM) $< $@
gc2/Panel.cc: $(WXDIR)/src/Windows/Panel.cc
	$(XFORM) $< $@

gc2/DC.cc: $(WXDIR)/src/DeviceContexts/DC.cc
	$(XFORM) $< $@
gc2/WindowDC.cc: $(WXDIR)/src/DeviceContexts/WindowDC.cc
	$(XFORM) $< $@
gc2/MemoryDC.cc: $(WXDIR)/src/DeviceContexts/MemoryDC.cc
	$(XFORM) $< $@
gc2/PSDC.cc: $(WXDIR)/src/DeviceContexts/PSDC.cc
	$(XFORM) $< $@

gc2/Object.cc: $(WXDIR)/src/DataStructures/Object.cc
	$(XFORM) $< $@
gc2/TypeTree.cc: $(WXDIR)/src/DataStructures/TypeTree.cc
	$(XFORM) $< $@
gc2/wb_list.cc: $(WXDIR)/src/DataStructures/wb_list.cc
	$(XFORM) $< $@
gc2/wb_hash.cc: $(WXDIR)/src/DataStructures/wb_hash.cc
	$(XFORM) $< $@

gc2/AppMain.cc: $(WXDIR)/src/Application/AppMain.cc
	$(XFORM) $< $@
gc2/GlobalData.cc: $(WXDIR)/src/Application/GlobalData.cc
	$(XFORM) $< $@

gc2/FileDialog.cc: $(WXDIR)/src/Dialogs/FileDialog.cc
	$(XFORM) $< $@
gc2/MessageBox.cc: $(WXDIR)/src/Dialogs/MessageBox.cc
	$(XFORM) $< $@

gc2/EvtHandler.cc: $(WXDIR)/src/EventHandling/EvtHandler.cc
	$(XFORM) $< $@
gc2/wb_sysev.cc: $(WXDIR)/src/EventHandling/wb_sysev.cc
	$(XFORM) $< $@
gc2/wb_stdev.cc: $(WXDIR)/src/EventHandling/wb_stdev.cc
	$(XFORM) $< $@

gc2/Bitmap.cc: $(WXDIR)/src/GDI-Classes/Bitmap.cc
	$(XFORM) $< $@
gc2/Colour.cc: $(WXDIR)/src/GDI-Classes/Colour.cc
	$(XFORM) $< $@
gc2/Font.cc: $(WXDIR)/src/GDI-Classes/Font.cc
	$(XFORM) $< $@
gc2/FontDirectory.cc: $(WXDIR)/src/GDI-Classes/FontDirectory.cc
	$(XFORM) $< $@
gc2/Pen+Brush.cc: $(WXDIR)/src/GDI-Classes/Pen+Brush.cc
	$(XFORM) $< $@

gc2/Clipboard.cc: $(WXDIR)/src/Misc/Clipboard.cc
	$(XFORM) $< $@
gc2/Timer.cc: $(WXDIR)/src/Misc/Timer.cc
	$(XFORM) $< $@

gc2/Assert.cc: $(WXDIR)/src/Utilities/Assert.cc
	$(XFORM) $< $@
gc2/BusyCursor.cc: $(WXDIR)/src/Utilities/BusyCursor.cc
	$(XFORM) $< $@
gc2/Date+Time.cc: $(WXDIR)/src/Utilities/Date+Time.cc
	$(XFORM) $< $@
gc2/Dierctory.cc: $(WXDIR)/src/Utilities/Directory.cc
	$(XFORM) $< $@
gc2/Error.cc: $(WXDIR)/src/Utilities/Error.cc
	$(XFORM) $< $@
gc2/GDI.cc: $(WXDIR)/src/Utilities/GDI.cc
	$(XFORM) $< $@
gc2/Home.cc: $(WXDIR)/src/Utilities/Home.cc
	$(XFORM) $< $@
gc2/Misc.cc: $(WXDIR)/src/Utilities/Misc.cc
	$(XFORM) $< $@
gc2/Net.cc: $(WXDIR)/src/Utilities/Net.cc
	$(XFORM) $< $@
gc2/Path.cc: $(WXDIR)/src/Utilities/Path.cc
	$(XFORM) $< $@
gc2/Resources.cc: $(WXDIR)/src/Utilities/Resources.cc
	$(XFORM) $< $@
gc2/String.cc: $(WXDIR)/src/Utilities/String.cc
	$(XFORM) $< $@
gc2/TempFile.cc: $(WXDIR)/src/Utilities/TempFile.cc
	$(XFORM) $< $@

gc2/wx_image.cc: $(WXDIR)/utils/image/src/wx_image.cc
	$(XFORM) $< $@
gc2/wx_img.cc: $(WXDIR)/utils/image/src/wx_img.cc
	$(XFORM) $< $@
gc2/wx_24to8.cc: $(WXDIR)/utils/image/src/wx_24to8.cc
	$(XFORM) $< $@
gc2/wx_bmp.cc: $(WXDIR)/utils/image/src/wx_bmp.cc
	$(XFORM) $< $@
gc2/wx_gam.cc: $(WXDIR)/utils/image/src/wx_gam.cc
	$(XFORM) $< $@
gc2/wx_gif.cc: $(WXDIR)/utils/image/src/wx_gif.cc
	$(XFORM) $< $@
gc2/wx_gifwr.cc: $(WXDIR)/utils/image/src/wx_gifwr.cc
	$(XFORM) $< $@
gc2/wx_xbm.cc: $(WXDIR)/utils/image/src/wx_xbm.cc
	$(XFORM) $< $@

gc2/wx_media.cc: wxme/wx_media.cxx
	$(XFORM) $< $@

gc2/wx_mpriv.cc: wxme/wx_mpriv.cxx
	$(XFORM) $< $@

gc2/wx_snip.cc: wxme/wx_snip.cxx
	$(XFORM) $< $@

gc2/wx_msnip.cc: wxme/wx_msnip.cxx
	$(XFORM) $< $@

gc2/wx_mbuf.cc: wxme/wx_mbuf.cxx
	$(XFORM) $< $@

gc2/wx_mpbrd.cc: wxme/wx_mpbrd.cxx
	$(XFORM) $< $@

gc2/wx_keym.cc: wxme/wx_keym.cxx
	$(XFORM) $< $@

gc2/wx_medio.cc: wxme/wx_medio.cxx
	$(XFORM) $< $@

gc2/wx_style.cc: wxme/wx_style.cxx
	$(XFORM) $< $@

gc2/wx_mline.cc: wxme/wx_mline.cxx
	$(XFORM) $< $@


gc2/wxscheme.cc: wxs/wxscheme.cxx
	$(XFORM) $< $@

gc2/xcglue.c: ../mzscheme/utils/xcglue.c
	$(XFORM) $< $@


gc2/mred.cc: mred.cxx
	$(XFORM) $< $@

gc2/mredx.cc: mredx.cxx
	$(XFORM) $< $@


gc2/$(OBJDIR)/%.o: gc2/%.cc
	$(CXX) -DGC2_JUST_MACROS -include ../mzscheme/gc2/gc2.h -c $(CXXFLAGS) -o $@ $<

gc2/$(OBJDIR)/%.o: gc2/%.c
	$(CC) -DGC2_JUST_MACROS -include ../mzscheme/gc2/gc2.h -c $(CFLAGS) -o $@ $<

gc2/$(OBJDIR)/wxs_%.o: wxs/wxs_%.cxx
	$(CXX) -DMZ_PRECISE_GC -I../mzscheme/gc2/ $(CPPFLAGS) $(LOCALFLAGS) -c -o $@ $<

gc2/$(OBJDIR)/wxGC.o: ../wxcommon/wxGC.cxx
	$(CXX) -DMZ_PRECISE_GC -I../mzscheme/gc2/ $(CPPFLAGS) $(LOCALFLAGS) -c -o $@ $<

XSRCS = gc2/Window.cc \
  gc2/Button.cc \
  gc2/Layout.cc \
  gc2/MenuBar.cc \
  gc2/RadioBox.cc \
  gc2/Choice.cc \
  gc2/Gauge.cc \
  gc2/ListBox.cc \
  gc2/Message.cc \
  gc2/Slider.cc \
  gc2/Canvas.cc \
  gc2/DialogBox.cc \
  gc2/Item.cc \
  gc2/Menu.cc \
  gc2/Panel.cc \
\
  gc2/DC.cc \
  gc2/WindowDC.cc \
  gc2/MemoryDC.cc \
  gc2/PSDC.cc \
\
  gc2/Object.cc \
  gc2/TypeTree.cc \
  gc2/wb_list.cc \
  gc2/wb_hash.cc \
\
  gc2/AppMain.cc \
  gc2/GlobalData.cc \
\
  gc2/FileDialog.cc \
  gc2/MessageBox.cc \
\
  gc2/EvtHandler.cc \
  gc2/wb_sysev.cc \
  gc2/wb_stdev.cc \
\
  gc2/Bitmap.cc \
  gc2/Colour.cc \
  gc2/Font.cc \
  gc2/FontDirectory.cc \
  gc2/Pen+Brush.cc \
\
  gc2/Clipboard.cc \
  gc2/Timer.cc \
\
  gc2/Assert.cc \
  gc2/BusyCursor.cc \
  gc2/Date+Time.cc \
  gc2/Dierctory.cc \
  gc2/Error.cc \
  gc2/GDI.cc \
  gc2/Home.cc \
  gc2/Misc.cc \
  gc2/Net.cc \
  gc2/Path.cc \
  gc2/Resources.cc \
  gc2/String.cc \
  gc2/TempFile.cc \
\
  gc2/wx_image.cc \
  gc2/wx_img.cc \
  gc2/wx_24to8.cc \
  gc2/wx_bmp.cc \
  gc2/wx_gam.cc \
  gc2/wx_gif.cc \
  gc2/wx_gifwr.cc \
  gc2/wx_xbm.cc \
\
  gc2/wx_media.cc \
  gc2/wx_mpriv.cc \
  gc2/wx_snip.cc \
  gc2/wx_msnip.cc \
  gc2/wx_mbuf.cc \
  gc2/wx_mpbrd.cc \
  gc2/wx_keym.cc \
  gc2/wx_medio.cc \
  gc2/wx_style.cc \
  gc2/wx_mline.cc \
\
  gc2/wxscheme.cc \
  gc2/xcglue.c \
\
  gc2/mred.cc \
  gc2/mredx.cc

XOBJS = ${XSRCS:gc2/%.cc=gc2/$(OBJDIR)/%.o} \
  gc2/$(OBJDIR)/xcglue.o \
  gc2/$(OBJDIR)/wxGC.o \
  gc2/$(OBJDIR)/wxs_bmap.o \
  gc2/$(OBJDIR)/wxs_butn.o \
  gc2/$(OBJDIR)/wxs_chce.o \
  gc2/$(OBJDIR)/wxs_ckbx.o \
  gc2/$(OBJDIR)/wxs_cnvs.o \
  gc2/$(OBJDIR)/wxs_dc.o \
  gc2/$(OBJDIR)/wxs_evnt.o \
  gc2/$(OBJDIR)/wxs_fram.o \
  gc2/$(OBJDIR)/wxs_gage.o \
  gc2/$(OBJDIR)/wxs_gdi.o \
  gc2/$(OBJDIR)/wxs_glob.o \
  gc2/$(OBJDIR)/wxs_item.o \
  gc2/$(OBJDIR)/wxs_lbox.o \
  gc2/$(OBJDIR)/wxs_madm.o \
  gc2/$(OBJDIR)/wxs_mede.o \
  gc2/$(OBJDIR)/wxs_medi.o \
  gc2/$(OBJDIR)/wxs_menu.o \
  gc2/$(OBJDIR)/wxs_mio.o \
  gc2/$(OBJDIR)/wxs_misc.o \
  gc2/$(OBJDIR)/wxs_mpb.o \
  gc2/$(OBJDIR)/wxs_obj.o \
  gc2/$(OBJDIR)/wxs_panl.o \
  gc2/$(OBJDIR)/wxs_rado.o \
  gc2/$(OBJDIR)/wxs_slid.o \
  gc2/$(OBJDIR)/wxs_snip.o \
  gc2/$(OBJDIR)/wxs_styl.o \
  gc2/$(OBJDIR)/wxs_win.o

MZOBJS = ../mzscheme/gc2/$(MZOBJDIR)/salloc.o \
	../mzscheme/gc2/$(MZOBJDIR)/bignum.o \
	../mzscheme/gc2/$(MZOBJDIR)/bool.o \
	../mzscheme/gc2/$(MZOBJDIR)/char.o \
	../mzscheme/gc2/$(MZOBJDIR)/complex.o \
	../mzscheme/gc2/$(MZOBJDIR)/dynext.o \
	../mzscheme/gc2/$(MZOBJDIR)/env.o \
	../mzscheme/gc2/$(MZOBJDIR)/error.o \
	../mzscheme/gc2/$(MZOBJDIR)/eval.o \
	../mzscheme/gc2/$(MZOBJDIR)/file.o \
	../mzscheme/gc2/$(MZOBJDIR)/fun.o \
	../mzscheme/gc2/$(MZOBJDIR)/hash.o \
	../mzscheme/gc2/$(MZOBJDIR)/image.o \
	../mzscheme/gc2/$(MZOBJDIR)/list.o \
	../mzscheme/gc2/$(MZOBJDIR)/number.o \
        ../mzscheme/gc2/$(MZOBJDIR)/object.o \
	../mzscheme/gc2/$(MZOBJDIR)/port.o \
	../mzscheme/gc2/$(MZOBJDIR)/print.o \
	../mzscheme/gc2/$(MZOBJDIR)/process.o \
	../mzscheme/gc2/$(MZOBJDIR)/promise.o \
        ../mzscheme/gc2/$(MZOBJDIR)/rational.o \
	../mzscheme/gc2/$(MZOBJDIR)/read.o \
	../mzscheme/gc2/$(MZOBJDIR)/regexp.o \
	../mzscheme/gc2/$(MZOBJDIR)/sema.o \
	../mzscheme/gc2/$(MZOBJDIR)/setjmpup.o \
	../mzscheme/gc2/$(MZOBJDIR)/string.o \
	../mzscheme/gc2/$(MZOBJDIR)/struct.o \
	../mzscheme/gc2/$(MZOBJDIR)/symbol.o \
	../mzscheme/gc2/$(MZOBJDIR)/syntax.o \
	../mzscheme/gc2/$(MZOBJDIR)/type.o \
        ../mzscheme/gc2/$(MZOBJDIR)/unit.o \
	../mzscheme/gc2/$(MZOBJDIR)/vector.o \
	../mzscheme/gc2/$(MZOBJDIR)/gc2.o

XWOBJS = ../wxxt/src/XWidgets/$(MZOBJDIR)/xwArrow.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwCommon.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwHScrollb.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwRowCol.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwSlider2.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwVScrollb.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwBoard.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwEnforcer.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwLabel.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwScrollText.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwTabString.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwscroll.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwButton.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwFrame.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwMenu.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwScrollWin.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwToggle.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwCanvas.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwGroup.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwMultiList.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwScrollbar.o \
    ../wxxt/src/XWidgets/$(MZOBJDIR)/xwTools3d.o

xform: $(XSRCS)

mred2k: $(XOBJS)
	$(LINKER) $(MREDLDFLAGS) -o $(MRBUILDDIR)mred2k $(LDSTARTERS) $(XOBJS) $(MZOBJS) $(XWOBJS) -lXpm $(LDLIBS) $(LDENDERS)
