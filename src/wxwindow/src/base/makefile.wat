#
# Makefile for WATCOM C++
#
# Edwin Thaler	July 1994
# Dmitri Chubraev Nov. 1994
#
#.SUFFIXES
#.SUFFIXES : .obj .cc

WXDIR = ..\..

!include $(WXDIR)\src\makewat.env

WXLIB = $(WXDIR)\lib

DOCDIR = $(WXDIR)\docs
DOCUTILSDIR = $(WXDIR)\utils\tex2rtf\src
THISDIR = $(WXDIR)\src\base

ITEMS =  $(WXBASEINC)\wb_item.h $(WXBASEINC)\wb_buttn.h  &
	$(WXBASEINC)\wb_check.h $(WXBASEINC)\wb_choic.h  &
	$(WXBASEINC)\wb_menu.h $(WXBASEINC)\wb_messg.h  &
	$(WXBASEINC)\wb_txt.h $(WXBASEINC)\wb_mtxt.h  &
	$(WXBASEINC)\wb_lbox.h $(WXBASEINC)\wb_slidr.h &
	$(WXBASEINC)\wb_rbox.h $(WXBASEINC)\wx_setup.h
	

OBJECTS = wb_win.obj wb_frame.obj wb_panel.obj wb_utils.obj  &
	wb_main.obj wb_item.obj wb_list.obj wb_obj.obj &
	wb_text.obj wb_gdi.obj wb_dialg.obj wb_canvs.obj &
	wb_dc.obj wb_mf.obj wb_ps.obj wx_enhdg.obj &
	wb_hash.obj wb_ipc.obj wb_form.obj wb_timer.obj &
	wb_help.obj wb_sysev.obj wb_stdev.obj wb_types.obj &
	wb_mgstr.obj wxstring.obj wb_data.obj wb_scrol.obj wb_vlbox.obj &
        wb_stat.obj wx_lay.obj wx_doc.obj wb_res.obj wx_tbar.obj wx_bbar.obj &
        wb_print.obj wx_mem.obj wx_date.obj wb_cmdlg.obj wx_time.obj wx_frac.obj


all : $(OBJECTS)
	-echo "OBJECTs done" >all

clean:   .SYMBOLIC
    -erase *.obj *.bak *.err *.lbc

#$(OBJECTS) : $(ITEMS)
