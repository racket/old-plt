
################################
#          Precise GC          #
################################
# included in ../GNUmakefile

MZOBJDIR=objects/sys$(MZPLATFORMSUFFIX)
WXOBJDIR=objects/sys$(MZPLATFORMSUFFIX)

XFORM = ../mzscheme/$(MZBUILDDIR)mzscheme2k -rq ../mzscheme/gc2/xform.ss "gcc -E -I../mzscheme/gc2 $(OPTIONS) $(GUI) $(INC) -I$(WXDIR)/src/XWidgets -I$(WXDIR)/src $(XINCLUDE)"
XFORMDEP = ../mzscheme/gc2/xform.ss

gc2/xsrc/Window.cc: $(WXDIR)/src/Windows/Window.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Window.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Button.cc: $(WXDIR)/src/Windows/Button.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Button.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Frame.cc: $(WXDIR)/src/Windows/Frame.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Frame.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Layout.cc: $(WXDIR)/src/Windows/Layout.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Layout.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/MenuBar.cc: $(WXDIR)/src/Windows/MenuBar.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/MenuBar.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/RadioBox.cc: $(WXDIR)/src/Windows/RadioBox.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/RadioBox.o $(XFORMDEP)
	$(XFORM) $< $@ 
gc2/xsrc/CheckBox.cc: $(WXDIR)/src/Windows/CheckBox.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/CheckBox.o $(XFORMDEP)
	$(XFORM) $< $@ 
gc2/xsrc/Choice.cc: $(WXDIR)/src/Windows/Choice.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Choice.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Gauge.cc: $(WXDIR)/src/Windows/Gauge.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Gauge.o $(XFORMDEP)
	$(XFORM) $< $@  
gc2/xsrc/ListBox.cc: $(WXDIR)/src/Windows/ListBox.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/ListBox.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Message.cc: $(WXDIR)/src/Windows/Message.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Message.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Slider.cc: $(WXDIR)/src/Windows/Slider.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Slider.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Canvas.cc: $(WXDIR)/src/Windows/Canvas.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Canvas.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/DialogBox.cc: $(WXDIR)/src/Windows/DialogBox.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/DialogBox.o $(XFORMDEP)
	$(XFORM) $< $@ 
gc2/xsrc/Item.cc: $(WXDIR)/src/Windows/Item.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Item.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Menu.cc: $(WXDIR)/src/Windows/Menu.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Menu.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Panel.cc: $(WXDIR)/src/Windows/Panel.cc $(WXDIR)/src/Windows/$(WXOBJDIR)/Panel.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/DC.cc: $(WXDIR)/src/DeviceContexts/DC.cc $(WXDIR)/src/DeviceContexts/$(WXOBJDIR)/DC.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/WindowDC.cc: $(WXDIR)/src/DeviceContexts/WindowDC.cc $(WXDIR)/src/DeviceContexts/$(WXOBJDIR)/WindowDC.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/MemoryDC.cc: $(WXDIR)/src/DeviceContexts/MemoryDC.cc $(WXDIR)/src/DeviceContexts/$(WXOBJDIR)/MemoryDC.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/PSDC.cc: $(WXDIR)/src/DeviceContexts/PSDC.cc $(WXDIR)/src/DeviceContexts/$(WXOBJDIR)/PSDC.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/Object.cc: $(WXDIR)/src/DataStructures/Object.cc $(WXDIR)/src/DataStructures/$(WXOBJDIR)/Object.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/TypeTree.cc: $(WXDIR)/src/DataStructures/TypeTree.cc $(WXDIR)/src/DataStructures/$(WXOBJDIR)/TypeTree.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wb_list.cc: $(WXDIR)/src/DataStructures/wb_list.cc $(WXDIR)/src/DataStructures/$(WXOBJDIR)/wb_list.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wb_hash.cc: $(WXDIR)/src/DataStructures/wb_hash.cc $(WXDIR)/src/DataStructures/$(WXOBJDIR)/wb_hash.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/AppMain.cc: $(WXDIR)/src/Application/AppMain.cc $(WXDIR)/src/Application/$(WXOBJDIR)/AppMain.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/GlobalData.cc: $(WXDIR)/src/Application/GlobalData.cc $(WXDIR)/src/Application/$(WXOBJDIR)/GlobalData.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/FileDialog.cc: $(WXDIR)/src/Dialogs/FileDialog.cc $(WXDIR)/src/Dialogs/$(WXOBJDIR)/FileDialog.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/MessageBox.cc: $(WXDIR)/src/Dialogs/MessageBox.cc $(WXDIR)/src/Dialogs/$(WXOBJDIR)/MessageBox.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/EvtHandler.cc: $(WXDIR)/src/EventHandling/EvtHandler.cc $(WXDIR)/src/EventHandling/$(WXOBJDIR)/EvtHandler.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wb_sysev.cc: $(WXDIR)/src/EventHandling/wb_sysev.cc $(WXDIR)/src/EventHandling/$(WXOBJDIR)/wb_sysev.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wb_stdev.cc: $(WXDIR)/src/EventHandling/wb_stdev.cc $(WXDIR)/src/EventHandling/$(WXOBJDIR)/wb_stdev.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/Bitmap.cc: $(WXDIR)/src/GDI-Classes/Bitmap.cc $(WXDIR)/src/GDI-Classes/$(WXOBJDIR)/Bitmap.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Colour.cc: $(WXDIR)/src/GDI-Classes/Colour.cc $(WXDIR)/src/GDI-Classes/$(WXOBJDIR)/Colour.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Font.cc: $(WXDIR)/src/GDI-Classes/Font.cc $(WXDIR)/src/GDI-Classes/$(WXOBJDIR)/Font.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/FontDirectory.cc: $(WXDIR)/src/GDI-Classes/FontDirectory.cc $(WXDIR)/src/GDI-Classes/$(WXOBJDIR)/FontDirectory.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Pen+Brush.cc: $(WXDIR)/src/GDI-Classes/Pen+Brush.cc $(WXDIR)/src/GDI-Classes/$(WXOBJDIR)/Pen+Brush.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/Clipboard.cc: $(WXDIR)/src/Misc/Clipboard.cc $(WXDIR)/src/Misc/$(WXOBJDIR)/Clipboard.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Timer.cc: $(WXDIR)/src/Misc/Timer.cc $(WXDIR)/src/Misc/$(WXOBJDIR)/Timer.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/Application.cc: $(WXDIR)/src/Utilities/Application.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Application.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Assert.cc: $(WXDIR)/src/Utilities/Assert.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Assert.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/BusyCursor.cc: $(WXDIR)/src/Utilities/BusyCursor.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/BusyCursor.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Date+Time.cc: $(WXDIR)/src/Utilities/Date+Time.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Date+Time.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Directory.cc: $(WXDIR)/src/Utilities/Directory.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Directory.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Error.cc: $(WXDIR)/src/Utilities/Error.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Error.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/GDI.cc: $(WXDIR)/src/Utilities/GDI.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/GDI.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Home.cc: $(WXDIR)/src/Utilities/Home.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Home.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Misc.cc: $(WXDIR)/src/Utilities/Misc.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Misc.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Net.cc: $(WXDIR)/src/Utilities/Net.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Net.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Path.cc: $(WXDIR)/src/Utilities/Path.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Path.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/Resources.cc: $(WXDIR)/src/Utilities/Resources.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/Resources.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/String.cc: $(WXDIR)/src/Utilities/String.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/String.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/TempFile.cc: $(WXDIR)/src/Utilities/TempFile.cc $(WXDIR)/src/Utilities/$(WXOBJDIR)/TempFile.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_image.cc: $(WXDIR)/utils/image/src/wx_image.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_image.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wx_img.cc: $(WXDIR)/utils/image/src/wx_img.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_img.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wx_24to8.cc: $(WXDIR)/utils/image/src/wx_24to8.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_24to8.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wx_bmp.cc: $(WXDIR)/utils/image/src/wx_bmp.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_bmp.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wx_gam.cc: $(WXDIR)/utils/image/src/wx_gam.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_gam.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wx_gif.cc: $(WXDIR)/utils/image/src/wx_gif.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_gif.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wx_gifwr.cc: $(WXDIR)/utils/image/src/wx_gifwr.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_gifwr.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wx_xbm.cc: $(WXDIR)/utils/image/src/wx_xbm.cc $(WXDIR)/utils/image/src/$(OBJDIR)/wx_xbm.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_media.cc: wxme/wx_media.cxx wxme/$(OBJDIR)/wx_media.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_mpriv.cc: wxme/wx_mpriv.cxx wxme/$(OBJDIR)/wx_mpriv.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_snip.cc: wxme/wx_snip.cxx wxme/$(OBJDIR)/wx_snip.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_msnip.cc: wxme/wx_msnip.cxx wxme/$(OBJDIR)/wx_msnip.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_mbuf.cc: wxme/wx_mbuf.cxx wxme/$(OBJDIR)/wx_mbuf.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_mpbrd.cc: wxme/wx_mpbrd.cxx wxme/$(OBJDIR)/wx_mpbrd.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_keym.cc: wxme/wx_keym.cxx wxme/$(OBJDIR)/wx_keym.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_medio.cc: wxme/wx_medio.cxx wxme/$(OBJDIR)/wx_medio.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_medad.cc: wxme/wx_medad.cxx wxme/$(OBJDIR)/wx_medad.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_style.cc: wxme/wx_style.cxx wxme/$(OBJDIR)/wx_style.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_mline.cc: wxme/wx_mline.cxx wxme/$(OBJDIR)/wx_mline.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/wx_cgrec.cc: wxme/wx_cgrec.cxx wxme/$(OBJDIR)/wx_cgrec.o $(XFORMDEP)
	$(XFORM) $< $@


gc2/xsrc/wxscheme.cc: wxs/wxscheme.cxx wxs/$(OBJDIR)/wxscheme.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/xsrc/xcglue.c: ../mzscheme/utils/xcglue.c wxs/$(OBJDIR)/xcglue.o $(XFORMDEP)
	$(XFORM) $< $@


gc2/xsrc/mred.cc: mred.cxx
	$(XFORM) $< $@

gc2/xsrc/mredx.cc: mredx.cxx
	$(XFORM) $< $@

gc2/xsrc/wxs_bmap.cc: wxs/wxs_bmap.cxx wxs/$(OBJDIR)/wxs_bmap.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_butn.cc: wxs/wxs_butn.cxx wxs/$(OBJDIR)/wxs_butn.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_chce.cc: wxs/wxs_chce.cxx wxs/$(OBJDIR)/wxs_chce.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_ckbx.cc: wxs/wxs_ckbx.cxx wxs/$(OBJDIR)/wxs_ckbx.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_cnvs.cc: wxs/wxs_cnvs.cxx wxs/$(OBJDIR)/wxs_cnvs.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_dc.cc: wxs/wxs_dc.cxx wxs/$(OBJDIR)/wxs_dc.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_evnt.cc: wxs/wxs_evnt.cxx wxs/$(OBJDIR)/wxs_evnt.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_fram.cc: wxs/wxs_fram.cxx wxs/$(OBJDIR)/wxs_fram.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_gage.cc: wxs/wxs_gage.cxx wxs/$(OBJDIR)/wxs_gage.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_gdi.cc: wxs/wxs_gdi.cxx wxs/$(OBJDIR)/wxs_gdi.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_glob.cc: wxs/wxs_glob.cxx wxs/$(OBJDIR)/wxs_glob.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_item.cc: wxs/wxs_item.cxx wxs/$(OBJDIR)/wxs_item.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_lbox.cc: wxs/wxs_lbox.cxx wxs/$(OBJDIR)/wxs_lbox.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_madm.cc: wxs/wxs_madm.cxx wxs/$(OBJDIR)/wxs_madm.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_mede.cc: wxs/wxs_mede.cxx wxs/$(OBJDIR)/wxs_mede.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_medi.cc: wxs/wxs_medi.cxx wxs/$(OBJDIR)/wxs_medi.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_menu.cc: wxs/wxs_menu.cxx wxs/$(OBJDIR)/wxs_menu.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_mio.cc: wxs/wxs_mio.cxx wxs/$(OBJDIR)/wxs_mio.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_misc.cc: wxs/wxs_misc.cxx wxs/$(OBJDIR)/wxs_misc.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_mpb.cc: wxs/wxs_mpb.cxx wxs/$(OBJDIR)/wxs_mpb.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_obj.cc: wxs/wxs_obj.cxx wxs/$(OBJDIR)/wxs_obj.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_panl.cc: wxs/wxs_panl.cxx wxs/$(OBJDIR)/wxs_panl.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_rado.cc: wxs/wxs_rado.cxx wxs/$(OBJDIR)/wxs_rado.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_slid.cc: wxs/wxs_slid.cxx wxs/$(OBJDIR)/wxs_slid.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_snip.cc: wxs/wxs_snip.cxx wxs/$(OBJDIR)/wxs_snip.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_styl.cc: wxs/wxs_styl.cxx wxs/$(OBJDIR)/wxs_styl.o $(XFORMDEP)
	$(XFORM) $< $@
gc2/xsrc/wxs_win.cc: wxs/wxs_win.cxx wxs/$(OBJDIR)/wxs_win.o $(XFORMDEP)
	$(XFORM) $< $@

gc2/$(OBJDIR)/%.o: gc2/xsrc/%.cc
	$(CXX) -DGC2_JUST_MACROS -include ../mzscheme/gc2/gc2.h -c $(CXXFLAGS) -o $@ $<

gc2/$(OBJDIR)/%.o: gc2/xsrc/%.c
	$(CC) -DGC2_JUST_MACROS -include ../mzscheme/gc2/gc2.h -c $(CFLAGS) -o $@ $<

gc2/$(OBJDIR)/wxGC.o: ../wxcommon/wxGC.cxx
	$(CXX) -DMZ_PRECISE_GC -I../mzscheme/gc2/ $(CPPFLAGS) -c -o $@ $<

XSRCS = gc2/xsrc/Window.cc \
  gc2/xsrc/Button.cc \
  gc2/xsrc/Frame.cc \
  gc2/xsrc/Layout.cc \
  gc2/xsrc/MenuBar.cc \
  gc2/xsrc/RadioBox.cc \
  gc2/xsrc/CheckBox.cc \
  gc2/xsrc/Choice.cc \
  gc2/xsrc/Gauge.cc \
  gc2/xsrc/ListBox.cc \
  gc2/xsrc/Message.cc \
  gc2/xsrc/Slider.cc \
  gc2/xsrc/Canvas.cc \
  gc2/xsrc/DialogBox.cc \
  gc2/xsrc/Item.cc \
  gc2/xsrc/Menu.cc \
  gc2/xsrc/Panel.cc \
\
  gc2/xsrc/DC.cc \
  gc2/xsrc/WindowDC.cc \
  gc2/xsrc/MemoryDC.cc \
  gc2/xsrc/PSDC.cc \
\
  gc2/xsrc/Object.cc \
  gc2/xsrc/TypeTree.cc \
  gc2/xsrc/wb_list.cc \
  gc2/xsrc/wb_hash.cc \
\
  gc2/xsrc/AppMain.cc \
  gc2/xsrc/GlobalData.cc \
\
  gc2/xsrc/FileDialog.cc \
  gc2/xsrc/MessageBox.cc \
\
  gc2/xsrc/EvtHandler.cc \
  gc2/xsrc/wb_sysev.cc \
  gc2/xsrc/wb_stdev.cc \
\
  gc2/xsrc/Bitmap.cc \
  gc2/xsrc/Colour.cc \
  gc2/xsrc/Font.cc \
  gc2/xsrc/FontDirectory.cc \
  gc2/xsrc/Pen+Brush.cc \
\
  gc2/xsrc/Clipboard.cc \
  gc2/xsrc/Timer.cc \
\
  gc2/xsrc/Application.cc \
  gc2/xsrc/Assert.cc \
  gc2/xsrc/BusyCursor.cc \
  gc2/xsrc/Date+Time.cc \
  gc2/xsrc/Directory.cc \
  gc2/xsrc/Error.cc \
  gc2/xsrc/GDI.cc \
  gc2/xsrc/Home.cc \
  gc2/xsrc/Misc.cc \
  gc2/xsrc/Net.cc \
  gc2/xsrc/Path.cc \
  gc2/xsrc/Resources.cc \
  gc2/xsrc/String.cc \
  gc2/xsrc/TempFile.cc \
\
  gc2/xsrc/wx_image.cc \
  gc2/xsrc/wx_img.cc \
  gc2/xsrc/wx_24to8.cc \
  gc2/xsrc/wx_bmp.cc \
  gc2/xsrc/wx_gam.cc \
  gc2/xsrc/wx_gif.cc \
  gc2/xsrc/wx_gifwr.cc \
  gc2/xsrc/wx_xbm.cc \
\
  gc2/xsrc/wx_media.cc \
  gc2/xsrc/wx_mpriv.cc \
  gc2/xsrc/wx_snip.cc \
  gc2/xsrc/wx_msnip.cc \
  gc2/xsrc/wx_mbuf.cc \
  gc2/xsrc/wx_mpbrd.cc \
  gc2/xsrc/wx_keym.cc \
  gc2/xsrc/wx_medio.cc \
  gc2/xsrc/wx_medad.cc \
  gc2/xsrc/wx_style.cc \
  gc2/xsrc/wx_mline.cc \
  gc2/xsrc/wx_cgrec.cc \
\
  gc2/xsrc/wxscheme.cc \
\
  gc2/xsrc/wxs_bmap.cc \
  gc2/xsrc/wxs_butn.cc \
  gc2/xsrc/wxs_chce.cc \
  gc2/xsrc/wxs_ckbx.cc \
  gc2/xsrc/wxs_cnvs.cc \
  gc2/xsrc/wxs_dc.cc \
  gc2/xsrc/wxs_evnt.cc \
  gc2/xsrc/wxs_fram.cc \
  gc2/xsrc/wxs_gage.cc \
  gc2/xsrc/wxs_gdi.cc \
  gc2/xsrc/wxs_glob.cc \
  gc2/xsrc/wxs_item.cc \
  gc2/xsrc/wxs_lbox.cc \
  gc2/xsrc/wxs_madm.cc \
  gc2/xsrc/wxs_mede.cc \
  gc2/xsrc/wxs_medi.cc \
  gc2/xsrc/wxs_menu.cc \
  gc2/xsrc/wxs_mio.cc \
  gc2/xsrc/wxs_misc.cc \
  gc2/xsrc/wxs_mpb.cc \
  gc2/xsrc/wxs_obj.cc \
  gc2/xsrc/wxs_panl.cc \
  gc2/xsrc/wxs_rado.cc \
  gc2/xsrc/wxs_slid.cc \
  gc2/xsrc/wxs_snip.cc \
  gc2/xsrc/wxs_styl.cc \
  gc2/xsrc/wxs_win.cc \
\
  gc2/xsrc/mred.cc \
  gc2/xsrc/mredx.cc

XOBJS = ${XSRCS:gc2/xsrc/%.cc=gc2/$(OBJDIR)/%.o} \
  gc2/$(OBJDIR)/xcglue.o \
  gc2/$(OBJDIR)/wxGC.o


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

xform: $(XSRCS) gc2/xcglue.c

$(MRBUILDDIR)mred2k: $(XOBJS) $(MZOBJS) $(XWOBJS)
	$(LINKER) $(MREDLDFLAGS) -o $(MRBUILDDIR)mred2k $(LDSTARTERS) $(XOBJS) $(MZOBJS) $(XWOBJS) -lXpm $(LDLIBS) $(LDNONSTATICLIBS) $(LDENDERS)

gc2/xsrc:
	mkdir gc2/xsrc

gc2/objects:
	mkdir gc2/objects

gc2/$(OBJDIR):
	mkdir gc2/$(OBJDIR)

2k: gc2/xsrc gc2/objects gc2/$(OBJDIR)
	cd ../mzscheme; ./mzmake 2k
	$(MAKE) all
	$(MAKE) $(MRBUILDDIR)mred2k
