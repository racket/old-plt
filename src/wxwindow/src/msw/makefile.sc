# Symantec C++ makefile for the msw objects
# called from src\makefile.sc

# configuration section (see src\makefile.sc) ###########################

WXDIR = $(WXWIN)
INCDIR = $(WXDIR)\include
MSWINC = $(INCDIR)\msw
BASEINC = $(INCDIR)\base

# default values overridden by src\makefile.sc

CC=sc
CFLAGS = -o -ml -W -Dwx_msw

INCLUDE=$(BASEINC);$(MSWINC);$(WXDIR)\contrib\fafa;$(WXDIR)\contrib\itsybits

OPTIONS=

# end of configuration section ##########################################

.cc.obj:
	*$(CC) -c $(CFLAGS) -I$(INCLUDE) $(OPTIONS) $<

OBJS = wx_win.obj wx_frame.obj wx_panel.obj wx_utils.obj wx_main.obj \
wx_item.obj wx_text.obj wx_gdi.obj wx_dialg.obj wx_canvs.obj wx_dc.obj \
wx_mf.obj wx_ipc.obj wx_timer.obj wx_clipb.obj wx_scrol.obj wx_vlbox.obj \
wx_stat.obj wx_buttn.obj wx_messg.obj wx_check.obj wx_choic.obj wx_rbox.obj wx_lbox.obj \
wx_group.obj wx_gauge.obj wx_txt.obj wx_mtxt.obj wx_slidr.obj wx_menu.obj wx_db.obj\
wx_cmdlg.obj

all: $(OBJS)

wx_obj.obj: $(BASEINC)\wx_obj.h

wx_win.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h \
$(BASEINC)\wx_obj.h $(BASEINC)\wx_utils.h wx_win.cc \
$(MSWINC)\wx_gdi.h $(MSWINC)\wx_privt.h

wx_main.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(MSWINC)\wx_frame.h $(BASEINC)\wx_utils.h

wx_frame.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(MSWINC)\wx_frame.h wx_frame.cc \
$(BASEINC)\wx_stdev.h $(MSWINC)\wx_privt.h

wx_panel.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(MSWINC)\wx_frame.h $(MSWINC)\wx_panel.h \
wx_panel.cc $(BASEINC)\wx_stdev.h $(MSWINC)\wx_privt.h

wx_text.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(MSWINC)\wx_frame.h $(MSWINC)\wx_text.h \
wx_text.cc $(BASEINC)\wx_stdev.h $(MSWINC)\wx_privt.h

wx_canvs.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(MSWINC)\wx_frame.h $(MSWINC)\wx_canvs.h \
wx_canvs.cc $(BASEINC)\wx_stdev.h $(MSWINC)\wx_gdi.h $(MSWINC)\wx_dc.h \
$(MSWINC)\wx_privt.h

wx_dc.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(MSWINC)\wx_frame.h $(MSWINC)\wx_canvs.h wx_dc.cc \
$(BASEINC)\wx_stdev.h $(MSWINC)\wx_gdi.h $(MSWINC)\wx_dc.h \
$(MSWINC)/wx_dccan.h $(MSWINC)/wx_dcmem.h

wx_mf.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h $(BASEINC)\wx_obj.h \
wx_mf.cc $(BASEINC)\wx_stdev.h $(MSWINC)\wx_gdi.h $(MSWINC)\wx_mf.h

wx_item.obj: $(BASEINC)\common.h $(MSWINC)\wx_win.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(MSWINC)\wx_frame.h $(MSWINC)\wx_item.h \
wx_item.cc $(BASEINC)\wx_stdev.h $(MSWINC)\wx_privt.h

wx_utils.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h wx_utils.cc

wx_ipc.obj: $(BASEINC)\common.h $(BASEINC)\wx_obj.h \
$(BASEINC)\wx_utils.h $(MSWINC)\wx_ipc.h wx_ipc.cc

wx_gdi.obj: $(BASEINC)\common.h $(MSWINC)\wx_gdi.h $(BASEINC)\wx_utils.h \
wx_gdi.cc

wx_dialg.obj: $(BASEINC)\common.h wx_dialg.cc $(MSWINC)\wx_dialg.h \
$(MSWINC)\wx_win.h $(BASEINC)\wx_utils.h $(MSWINC)\wx_panel.h \
$(MSWINC)\wx_privt.h

wx_timer.obj: $(BASEINC)\common.h wx_timer.cc $(MSWINC)\wx_timer.h

wx_clipb.obj: $(BASEINC)\common.h wx_clipb.cc $(MSWINC)\wx_clipb.h

wx_stat.obj: wx_stat.cc $(MSWINC)\wx_stat.h

wx_scrol.obj: wx_scrol.cc $(MSWINC)\wx_scrol.h

wx_vlbox.obj: wx_vlbox.cc $(MSWINC)\wx_vlbox.h

wx_buttn.obj: wx_buttn.cc $(MSWINC)\wx_buttn.h

wx_messg.obj: wx_messg.cc $(MSWINC)\wx_messg.h

wx_check.obj: wx_check.cc $(MSWINC)\wx_check.h

wx_choic.obj: wx_choic.cc $(MSWINC)\wx_choic.h

wx_rbox.obj: wx_rbox.cc $(MSWINC)\wx_rbox.h

wx_lbox.obj: wx_lbox.cc $(MSWINC)\wx_lbox.h

wx_group.obj: wx_group.cc $(MSWINC)\wx_group.h

wx_gauge.obj: wx_gauge.cc $(MSWINC)\wx_gauge.h

wx_txt.obj: wx_txt.cc $(MSWINC)\wx_txt.h

wx_mtxt.obj: wx_mtxt.cc $(MSWINC)\wx_mtxt.h

wx_slidr.obj: wx_slidr.cc $(MSWINC)\wx_sldir.h

wx_menu.obj: wx_menu.cc $(MSWINC)\wx_menu.h

wx_db.obj: wx_db.cc $(MSWINC)\wx_db.h

wx_cmdlg.obj: wx_cmdlg.cc $(MSWINC)\wx_cmdlg.h

$(MSWINC)/wx_win.h:  $(BASEINC)/wb_win.h
$(MSWINC)/wx_main.h:  $(BASEINC)/wb_main.h
$(MSWINC)/wx_frame.h:  $(BASEINC)/wb_frame.h
$(MSWINC)/wx_panel.h:  $(BASEINC)/wb_panel.h
$(MSWINC)/wx_text.h:  $(BASEINC)/wb_text.h
$(MSWINC)/wx_dialg.h:  $(BASEINC)/wb_dialg.h
$(MSWINC)/wx_ipc.h:  $(BASEINC)/wb_ipc.h
$(MSWINC)/wx_gdi.h:  $(BASEINC)/wb_gdi.h
$(MSWINC)/wx_event.h:  $(BASEINC)/wb_event.h
$(MSWINC)/wx_canvs.h:  $(BASEINC)/wb_canvs.h
$(MSWINC)/wx_mf.h:  $(BASEINC)/wb_mf.h
$(MSWINC)/wx_item.h:  $(BASEINC)/wb_item.h
$(MSWINC)/wx_buttn.h:  $(BASEINC)/wb_buttn.h
$(MSWINC)/wx_messg.h:  $(BASEINC)/wb_messg.h
$(MSWINC)/wx_choic.h:  $(BASEINC)/wb_choic.h
$(MSWINC)/wx_check.h:  $(BASEINC)/wb_check.h
$(MSWINC)/wx_lbox.h:  $(BASEINC)/wb_lbox.h
$(MSWINC)/wx_txt.h:  $(BASEINC)/wb_txt.h
$(MSWINC)/wx_mtxt.h:  $(BASEINC)/wb_mtxt.h
$(MSWINC)/wx_slidr.h:  $(BASEINC)/wb_slidr.h
$(MSWINC)/wx_menu.h:  $(BASEINC)/wb_menu.h


clean:
	-del *.obj
