# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

!IF "$(CFG)" == ""
CFG=wxwin - Win32 Release
!MESSAGE No configuration specified.  Defaulting to wxwin - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "wxwin - Win32 Release" && "$(CFG)" != "wxwin - Win32 Debug" &&\
 "$(CFG)" != "wxwin - Win32 SGC"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxwin.mak" CFG="wxwin - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxwin - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wxwin - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE "wxwin - Win32 SGC" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "wxwin - Win32 Debug"
CPP=cl.exe

!IF  "$(CFG)" == "wxwin - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\wxwin.lib"

CLEAN : 
	-@erase ".\Release\vc40.pdb"
	-@erase ".\Release\wxwin.lib"
	-@erase ".\Release\WB_MF.obj"
	-@erase ".\Release\WX_CANVS.obj"
	-@erase ".\Release\WX_MEM.obj"
	-@erase ".\Release\WB_PANEL.obj"
	-@erase ".\Release\WX_UTILS.obj"
	-@erase ".\Release\WX_SCROL.obj"
	-@erase ".\Release\WX_MF.obj"
	-@erase ".\Release\WX_ITEM.obj"
	-@erase ".\Release\WX_LBOX.obj"
	-@erase ".\Release\WB_DC.obj"
	-@erase ".\Release\WB_FORM.obj"
	-@erase ".\Release\WB_LIST.obj"
	-@erase ".\Release\WB_DIALG.obj"
	-@erase ".\Release\WB_TEXT.obj"
	-@erase ".\Release\WB_PS.obj"
	-@erase ".\Release\WX_TBAR.obj"
	-@erase ".\Release\WX_DC.obj"
	-@erase ".\Release\WX_TXT.obj"
	-@erase ".\Release\WX_DATE.obj"
	-@erase ".\Release\wximgfil.obj"
	-@erase ".\Release\WB_MGSTR.obj"
	-@erase ".\Release\WB_FRAME.obj"
	-@erase ".\Release\WX_MENU.obj"
	-@erase ".\Release\WX_GAUGE.obj"
	-@erase ".\Release\WB_HELP.obj"
	-@erase ".\Release\WX_TIMER.obj"
	-@erase ".\Release\WX_BBAR.obj"
	-@erase ".\Release\WB_PRINT.obj"
	-@erase ".\Release\WB_SYSEV.obj"
	-@erase ".\Release\WB_MAIN.obj"
	-@erase ".\Release\WX_GROUP.obj"
	-@erase ".\Release\WX_BUTTN.obj"
	-@erase ".\Release\WX_FRAME.obj"
	-@erase ".\Release\WX_MTXT.obj"
	-@erase ".\Release\WX_RBOX.obj"
	-@erase ".\Release\WB_STDEV.obj"
	-@erase ".\Release\WB_CMDLG.obj"
	-@erase ".\Release\WX_DB.obj"
	-@erase ".\Release\WB_VLBOX.obj"
	-@erase ".\Release\WX_TIME.obj"
	-@erase ".\Release\wx_pdf.obj"
	-@erase ".\Release\WB_TYPES.obj"
	-@erase ".\Release\WX_WIN.obj"
	-@erase ".\Release\WB_RES.obj"
	-@erase ".\Release\WX_MESSG.obj"
	-@erase ".\Release\WXSTRING.obj"
	-@erase ".\Release\WX_SLIDR.obj"
	-@erase ".\Release\WX_CMDLG.obj"
	-@erase ".\Release\WX_IPC.obj"
	-@erase ".\Release\WX_VLBOX.obj"
	-@erase ".\Release\WX_PANEL.obj"
	-@erase ".\Release\WX_GDI.obj"
	-@erase ".\Release\WX_TEXT.obj"
	-@erase ".\Release\WB_OBJ.obj"
	-@erase ".\Release\WB_ITEM.obj"
	-@erase ".\Release\WB_HASH.obj"
	-@erase ".\Release\WB_TIMER.obj"
	-@erase ".\Release\WX_STAT.obj"
	-@erase ".\Release\WX_DOC.obj"
	-@erase ".\Release\WX_LAY.obj"
	-@erase ".\Release\WB_DATA.obj"
	-@erase ".\Release\WB_WIN.obj"
	-@erase ".\Release\WX_CHECK.obj"
	-@erase ".\Release\WX_FRAC.obj"
	-@erase ".\Release\WX_CHOIC.obj"
	-@erase ".\Release\WX_CLIPB.obj"
	-@erase ".\Release\WB_IPC.obj"
	-@erase ".\Release\WX_ENHDG.obj"
	-@erase ".\Release\WB_STAT.obj"
	-@erase ".\Release\WB_GDI.obj"
	-@erase ".\Release\WX_MAIN.obj"
	-@erase ".\Release\WB_CANVS.obj"
	-@erase ".\Release\WB_UTILS.obj"
	-@erase ".\Release\WX_DIALG.obj"
	-@erase ".\Release\WB_SCROL.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ".\..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "NDEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /YX"wx.h" /c
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ".\..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "NDEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /Fp"$(INTDIR)/wxwin.pch" /YX"wx.h" /Fo"$(INTDIR)/"\
 /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxwin.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxwin.lib" 
LIB32_OBJS= \
	".\Release\WB_MF.obj" \
	".\Release\WX_CANVS.obj" \
	".\Release\WX_MEM.obj" \
	".\Release\WB_PANEL.obj" \
	".\Release\WX_UTILS.obj" \
	".\Release\WX_SCROL.obj" \
	".\Release\WX_MF.obj" \
	".\Release\WX_ITEM.obj" \
	".\Release\WX_LBOX.obj" \
	".\Release\WB_DC.obj" \
	".\Release\WB_FORM.obj" \
	".\Release\WB_LIST.obj" \
	".\Release\WB_DIALG.obj" \
	".\Release\WB_TEXT.obj" \
	".\Release\WB_PS.obj" \
	".\Release\WX_TBAR.obj" \
	".\Release\WX_DC.obj" \
	".\Release\WX_TXT.obj" \
	".\Release\WX_DATE.obj" \
	".\Release\wximgfil.obj" \
	".\Release\WB_MGSTR.obj" \
	".\Release\WB_FRAME.obj" \
	".\Release\WX_MENU.obj" \
	".\Release\WX_GAUGE.obj" \
	".\Release\WB_HELP.obj" \
	".\Release\WX_TIMER.obj" \
	".\Release\WX_BBAR.obj" \
	".\Release\WB_PRINT.obj" \
	".\Release\WB_SYSEV.obj" \
	".\Release\WB_MAIN.obj" \
	".\Release\WX_GROUP.obj" \
	".\Release\WX_BUTTN.obj" \
	".\Release\WX_FRAME.obj" \
	".\Release\WX_MTXT.obj" \
	".\Release\WX_RBOX.obj" \
	".\Release\WB_STDEV.obj" \
	".\Release\WB_CMDLG.obj" \
	".\Release\WX_DB.obj" \
	".\Release\WB_VLBOX.obj" \
	".\Release\WX_TIME.obj" \
	".\Release\wx_pdf.obj" \
	".\Release\WB_TYPES.obj" \
	".\Release\WX_WIN.obj" \
	".\Release\WB_RES.obj" \
	".\Release\WX_MESSG.obj" \
	".\Release\WXSTRING.obj" \
	".\Release\WX_SLIDR.obj" \
	".\Release\WX_CMDLG.obj" \
	".\Release\WX_IPC.obj" \
	".\Release\WX_VLBOX.obj" \
	".\Release\WX_PANEL.obj" \
	".\Release\WX_GDI.obj" \
	".\Release\WX_TEXT.obj" \
	".\Release\WB_OBJ.obj" \
	".\Release\WB_ITEM.obj" \
	".\Release\WB_HASH.obj" \
	".\Release\WB_TIMER.obj" \
	".\Release\WX_STAT.obj" \
	".\Release\WX_DOC.obj" \
	".\Release\WX_LAY.obj" \
	".\Release\WB_DATA.obj" \
	".\Release\WB_WIN.obj" \
	".\Release\WX_CHECK.obj" \
	".\Release\WX_FRAC.obj" \
	".\Release\WX_CHOIC.obj" \
	".\Release\WX_CLIPB.obj" \
	".\Release\WB_IPC.obj" \
	".\Release\WX_ENHDG.obj" \
	".\Release\WB_STAT.obj" \
	".\Release\WB_GDI.obj" \
	".\Release\WX_MAIN.obj" \
	".\Release\WB_CANVS.obj" \
	".\Release\WB_UTILS.obj" \
	".\Release\WX_DIALG.obj" \
	".\Release\WB_SCROL.obj"

"$(OUTDIR)\wxwin.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\wxwin.lib"

CLEAN : 
	-@erase ".\Debug\wxwin.lib"
	-@erase ".\Debug\WX_MAIN.obj"
	-@erase ".\Debug\WX_DB.obj"
	-@erase ".\Debug\WX_IPC.obj"
	-@erase ".\Debug\WB_TYPES.obj"
	-@erase ".\Debug\WX_GDI.obj"
	-@erase ".\Debug\WX_SLIDR.obj"
	-@erase ".\Debug\WX_CMDLG.obj"
	-@erase ".\Debug\WX_VLBOX.obj"
	-@erase ".\Debug\WX_PANEL.obj"
	-@erase ".\Debug\WB_WIN.obj"
	-@erase ".\Debug\WB_TIMER.obj"
	-@erase ".\Debug\WB_IPC.obj"
	-@erase ".\Debug\WB_GDI.obj"
	-@erase ".\Debug\WB_FRAME.obj"
	-@erase ".\Debug\WX_ITEM.obj"
	-@erase ".\Debug\WX_LBOX.obj"
	-@erase ".\Debug\WX_TIMER.obj"
	-@erase ".\Debug\WX_ENHDG.obj"
	-@erase ".\Debug\WB_PRINT.obj"
	-@erase ".\Debug\WB_FORM.obj"
	-@erase ".\Debug\WB_LIST.obj"
	-@erase ".\Debug\WX_DIALG.obj"
	-@erase ".\Debug\WX_MEM.obj"
	-@erase ".\Debug\WB_TEXT.obj"
	-@erase ".\Debug\WB_SCROL.obj"
	-@erase ".\Debug\WB_CMDLG.obj"
	-@erase ".\Debug\WB_RES.obj"
	-@erase ".\Debug\WB_VLBOX.obj"
	-@erase ".\Debug\WX_TBAR.obj"
	-@erase ".\Debug\WX_CANVS.obj"
	-@erase ".\Debug\WB_PANEL.obj"
	-@erase ".\Debug\WX_DATE.obj"
	-@erase ".\Debug\WX_UTILS.obj"
	-@erase ".\Debug\WB_MF.obj"
	-@erase ".\Debug\WX_MESSG.obj"
	-@erase ".\Debug\WXSTRING.obj"
	-@erase ".\Debug\WB_OBJ.obj"
	-@erase ".\Debug\WX_SCROL.obj"
	-@erase ".\Debug\WX_MENU.obj"
	-@erase ".\Debug\WB_HELP.obj"
	-@erase ".\Debug\WX_BBAR.obj"
	-@erase ".\Debug\WX_DOC.obj"
	-@erase ".\Debug\WX_LAY.obj"
	-@erase ".\Debug\WB_MAIN.obj"
	-@erase ".\Debug\WX_MF.obj"
	-@erase ".\Debug\WX_MTXT.obj"
	-@erase ".\Debug\WX_RBOX.obj"
	-@erase ".\Debug\WB_DC.obj"
	-@erase ".\Debug\WX_CHECK.obj"
	-@erase ".\Debug\WX_TXT.obj"
	-@erase ".\Debug\WX_TIME.obj"
	-@erase ".\Debug\WX_CHOIC.obj"
	-@erase ".\Debug\WX_CLIPB.obj"
	-@erase ".\Debug\WB_DIALG.obj"
	-@erase ".\Debug\WB_PS.obj"
	-@erase ".\Debug\wximgfil.obj"
	-@erase ".\Debug\WB_CANVS.obj"
	-@erase ".\Debug\WB_UTILS.obj"
	-@erase ".\Debug\WX_DC.obj"
	-@erase ".\Debug\WB_MGSTR.obj"
	-@erase ".\Debug\WX_TEXT.obj"
	-@erase ".\Debug\WX_GAUGE.obj"
	-@erase ".\Debug\WB_ITEM.obj"
	-@erase ".\Debug\WB_HASH.obj"
	-@erase ".\Debug\WX_STAT.obj"
	-@erase ".\Debug\WB_SYSEV.obj"
	-@erase ".\Debug\WB_DATA.obj"
	-@erase ".\Debug\WX_GROUP.obj"
	-@erase ".\Debug\WX_BUTTN.obj"
	-@erase ".\Debug\WX_FRAME.obj"
	-@erase ".\Debug\WX_FRAC.obj"
	-@erase ".\Debug\wx_pdf.obj"
	-@erase ".\Debug\WB_STDEV.obj"
	-@erase ".\Debug\WX_WIN.obj"
	-@erase ".\Debug\WB_STAT.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ".\..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /YX"wx.h" /c
CPP_PROJ=/nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ".\..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /Fp"$(INTDIR)/wxwin.pch" /YX"wx.h" /Fo"$(INTDIR)/"\
 /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxwin.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxwin.lib" 
LIB32_OBJS= \
	".\Debug\WX_MAIN.obj" \
	".\Debug\WX_DB.obj" \
	".\Debug\WX_IPC.obj" \
	".\Debug\WB_TYPES.obj" \
	".\Debug\WX_GDI.obj" \
	".\Debug\WX_SLIDR.obj" \
	".\Debug\WX_CMDLG.obj" \
	".\Debug\WX_VLBOX.obj" \
	".\Debug\WX_PANEL.obj" \
	".\Debug\WB_WIN.obj" \
	".\Debug\WB_TIMER.obj" \
	".\Debug\WB_IPC.obj" \
	".\Debug\WB_GDI.obj" \
	".\Debug\WB_FRAME.obj" \
	".\Debug\WX_ITEM.obj" \
	".\Debug\WX_LBOX.obj" \
	".\Debug\WX_TIMER.obj" \
	".\Debug\WX_ENHDG.obj" \
	".\Debug\WB_PRINT.obj" \
	".\Debug\WB_FORM.obj" \
	".\Debug\WB_LIST.obj" \
	".\Debug\WX_DIALG.obj" \
	".\Debug\WX_MEM.obj" \
	".\Debug\WB_TEXT.obj" \
	".\Debug\WB_SCROL.obj" \
	".\Debug\WB_CMDLG.obj" \
	".\Debug\WB_RES.obj" \
	".\Debug\WB_VLBOX.obj" \
	".\Debug\WX_TBAR.obj" \
	".\Debug\WX_CANVS.obj" \
	".\Debug\WB_PANEL.obj" \
	".\Debug\WX_DATE.obj" \
	".\Debug\WX_UTILS.obj" \
	".\Debug\WB_MF.obj" \
	".\Debug\WX_MESSG.obj" \
	".\Debug\WXSTRING.obj" \
	".\Debug\WB_OBJ.obj" \
	".\Debug\WX_SCROL.obj" \
	".\Debug\WX_MENU.obj" \
	".\Debug\WB_HELP.obj" \
	".\Debug\WX_BBAR.obj" \
	".\Debug\WX_DOC.obj" \
	".\Debug\WX_LAY.obj" \
	".\Debug\WB_MAIN.obj" \
	".\Debug\WX_MF.obj" \
	".\Debug\WX_MTXT.obj" \
	".\Debug\WX_RBOX.obj" \
	".\Debug\WB_DC.obj" \
	".\Debug\WX_CHECK.obj" \
	".\Debug\WX_TXT.obj" \
	".\Debug\WX_TIME.obj" \
	".\Debug\WX_CHOIC.obj" \
	".\Debug\WX_CLIPB.obj" \
	".\Debug\WB_DIALG.obj" \
	".\Debug\WB_PS.obj" \
	".\Debug\wximgfil.obj" \
	".\Debug\WB_CANVS.obj" \
	".\Debug\WB_UTILS.obj" \
	".\Debug\WX_DC.obj" \
	".\Debug\WB_MGSTR.obj" \
	".\Debug\WX_TEXT.obj" \
	".\Debug\WX_GAUGE.obj" \
	".\Debug\WB_ITEM.obj" \
	".\Debug\WB_HASH.obj" \
	".\Debug\WX_STAT.obj" \
	".\Debug\WB_SYSEV.obj" \
	".\Debug\WB_DATA.obj" \
	".\Debug\WX_GROUP.obj" \
	".\Debug\WX_BUTTN.obj" \
	".\Debug\WX_FRAME.obj" \
	".\Debug\WX_FRAC.obj" \
	".\Debug\wx_pdf.obj" \
	".\Debug\WB_STDEV.obj" \
	".\Debug\WX_WIN.obj" \
	".\Debug\WB_STAT.obj"

"$(OUTDIR)\wxwin.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "wxwin___"
# PROP BASE Intermediate_Dir "wxwin___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "SGC"
# PROP Intermediate_Dir "SGC"
# PROP Target_Dir ""
OUTDIR=.\SGC
INTDIR=.\SGC

ALL : "$(OUTDIR)\wxwin.lib"

CLEAN : 
	-@erase ".\SGC\vc40.pdb"
	-@erase ".\SGC\wxwin.lib"
	-@erase ".\SGC\WB_MF.obj"
	-@erase ".\SGC\WB_FRAME.obj"
	-@erase ".\SGC\WX_GAUGE.obj"
	-@erase ".\SGC\WX_TIMER.obj"
	-@erase ".\SGC\WB_PRINT.obj"
	-@erase ".\SGC\WX_TEXT.obj"
	-@erase ".\SGC\WX_GROUP.obj"
	-@erase ".\SGC\WX_BUTTN.obj"
	-@erase ".\SGC\WX_MF.obj"
	-@erase ".\SGC\WX_ITEM.obj"
	-@erase ".\SGC\WX_LBOX.obj"
	-@erase ".\SGC\WX_GDI.obj"
	-@erase ".\SGC\WB_FORM.obj"
	-@erase ".\SGC\WB_CMDLG.obj"
	-@erase ".\SGC\WB_VLBOX.obj"
	-@erase ".\SGC\WB_LIST.obj"
	-@erase ".\SGC\WX_CANVS.obj"
	-@erase ".\SGC\WB_PANEL.obj"
	-@erase ".\SGC\WX_UTILS.obj"
	-@erase ".\SGC\WB_TEXT.obj"
	-@erase ".\SGC\WB_TYPES.obj"
	-@erase ".\SGC\WB_STAT.obj"
	-@erase ".\SGC\WX_MESSG.obj"
	-@erase ".\SGC\WX_TBAR.obj"
	-@erase ".\SGC\WB_PS.obj"
	-@erase ".\SGC\WXSTRING.obj"
	-@erase ".\SGC\WX_SCROL.obj"
	-@erase ".\SGC\WX_SLIDR.obj"
	-@erase ".\SGC\WX_DC.obj"
	-@erase ".\SGC\WB_GDI.obj"
	-@erase ".\SGC\WX_PANEL.obj"
	-@erase ".\SGC\WX_MENU.obj"
	-@erase ".\SGC\WB_HELP.obj"
	-@erase ".\SGC\WX_BBAR.obj"
	-@erase ".\SGC\WB_MAIN.obj"
	-@erase ".\SGC\WX_CHECK.obj"
	-@erase ".\SGC\wx_pdf.obj"
	-@erase ".\SGC\WX_CHOIC.obj"
	-@erase ".\SGC\WX_RBOX.obj"
	-@erase ".\SGC\WX_CLIPB.obj"
	-@erase ".\SGC\WB_DIALG.obj"
	-@erase ".\SGC\WX_WIN.obj"
	-@erase ".\SGC\WB_RES.obj"
	-@erase ".\SGC\WX_ENHDG.obj"
	-@erase ".\SGC\WX_DB.obj"
	-@erase ".\SGC\WX_IPC.obj"
	-@erase ".\SGC\WX_TIME.obj"
	-@erase ".\SGC\wximgfil.obj"
	-@erase ".\SGC\WB_CANVS.obj"
	-@erase ".\SGC\WB_UTILS.obj"
	-@erase ".\SGC\WB_MGSTR.obj"
	-@erase ".\SGC\WX_DIALG.obj"
	-@erase ".\SGC\WB_OBJ.obj"
	-@erase ".\SGC\WB_SCROL.obj"
	-@erase ".\SGC\WX_DOC.obj"
	-@erase ".\SGC\WB_DC.obj"
	-@erase ".\SGC\WX_LAY.obj"
	-@erase ".\SGC\WB_SYSEV.obj"
	-@erase ".\SGC\WB_WIN.obj"
	-@erase ".\SGC\WB_ITEM.obj"
	-@erase ".\SGC\WX_FRAME.obj"
	-@erase ".\SGC\WB_HASH.obj"
	-@erase ".\SGC\WX_STAT.obj"
	-@erase ".\SGC\WB_IPC.obj"
	-@erase ".\SGC\WB_STDEV.obj"
	-@erase ".\SGC\WX_DATE.obj"
	-@erase ".\SGC\WB_DATA.obj"
	-@erase ".\SGC\WX_TXT.obj"
	-@erase ".\SGC\WX_FRAC.obj"
	-@erase ".\SGC\WX_MAIN.obj"
	-@erase ".\SGC\WX_CMDLG.obj"
	-@erase ".\SGC\WX_VLBOX.obj"
	-@erase ".\SGC\WX_MEM.obj"
	-@erase ".\SGC\WX_MTXT.obj"
	-@erase ".\SGC\WB_TIMER.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Z7 /Od /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\mzscheme\gc" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /YX"wx.h" /c
# ADD CPP /nologo /MTd /W3 /Zi /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ".\..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT" /YX"wx.h" /c
CPP_PROJ=/nologo /MTd /W3 /Zi /Od /I "..\..\mzscheme\sgc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ".\..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT"\
 /Fp"$(INTDIR)/wxwin.pch" /YX"wx.h" /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\SGC/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxwin.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxwin.lib" 
LIB32_OBJS= \
	".\SGC\WB_MF.obj" \
	".\SGC\WB_FRAME.obj" \
	".\SGC\WX_GAUGE.obj" \
	".\SGC\WX_TIMER.obj" \
	".\SGC\WB_PRINT.obj" \
	".\SGC\WX_TEXT.obj" \
	".\SGC\WX_GROUP.obj" \
	".\SGC\WX_BUTTN.obj" \
	".\SGC\WX_MF.obj" \
	".\SGC\WX_ITEM.obj" \
	".\SGC\WX_LBOX.obj" \
	".\SGC\WX_GDI.obj" \
	".\SGC\WB_FORM.obj" \
	".\SGC\WB_CMDLG.obj" \
	".\SGC\WB_VLBOX.obj" \
	".\SGC\WB_LIST.obj" \
	".\SGC\WX_CANVS.obj" \
	".\SGC\WB_PANEL.obj" \
	".\SGC\WX_UTILS.obj" \
	".\SGC\WB_TEXT.obj" \
	".\SGC\WB_TYPES.obj" \
	".\SGC\WB_STAT.obj" \
	".\SGC\WX_MESSG.obj" \
	".\SGC\WX_TBAR.obj" \
	".\SGC\WB_PS.obj" \
	".\SGC\WXSTRING.obj" \
	".\SGC\WX_SCROL.obj" \
	".\SGC\WX_SLIDR.obj" \
	".\SGC\WX_DC.obj" \
	".\SGC\WB_GDI.obj" \
	".\SGC\WX_PANEL.obj" \
	".\SGC\WX_MENU.obj" \
	".\SGC\WB_HELP.obj" \
	".\SGC\WX_BBAR.obj" \
	".\SGC\WB_MAIN.obj" \
	".\SGC\WX_CHECK.obj" \
	".\SGC\wx_pdf.obj" \
	".\SGC\WX_CHOIC.obj" \
	".\SGC\WX_RBOX.obj" \
	".\SGC\WX_CLIPB.obj" \
	".\SGC\WB_DIALG.obj" \
	".\SGC\WX_WIN.obj" \
	".\SGC\WB_RES.obj" \
	".\SGC\WX_ENHDG.obj" \
	".\SGC\WX_DB.obj" \
	".\SGC\WX_IPC.obj" \
	".\SGC\WX_TIME.obj" \
	".\SGC\wximgfil.obj" \
	".\SGC\WB_CANVS.obj" \
	".\SGC\WB_UTILS.obj" \
	".\SGC\WB_MGSTR.obj" \
	".\SGC\WX_DIALG.obj" \
	".\SGC\WB_OBJ.obj" \
	".\SGC\WB_SCROL.obj" \
	".\SGC\WX_DOC.obj" \
	".\SGC\WB_DC.obj" \
	".\SGC\WX_LAY.obj" \
	".\SGC\WB_SYSEV.obj" \
	".\SGC\WB_WIN.obj" \
	".\SGC\WB_ITEM.obj" \
	".\SGC\WX_FRAME.obj" \
	".\SGC\WB_HASH.obj" \
	".\SGC\WX_STAT.obj" \
	".\SGC\WB_IPC.obj" \
	".\SGC\WB_STDEV.obj" \
	".\SGC\WX_DATE.obj" \
	".\SGC\WB_DATA.obj" \
	".\SGC\WX_TXT.obj" \
	".\SGC\WX_FRAC.obj" \
	".\SGC\WX_MAIN.obj" \
	".\SGC\WX_CMDLG.obj" \
	".\SGC\WX_VLBOX.obj" \
	".\SGC\WX_MEM.obj" \
	".\SGC\WX_MTXT.obj" \
	".\SGC\WB_TIMER.obj"

"$(OUTDIR)\wxwin.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "wxwin - Win32 Release"
# Name "wxwin - Win32 Debug"
# Name "wxwin - Win32 SGC"

!IF  "$(CFG)" == "wxwin - Win32 Release"

!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_CMDLG.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_CM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_WB_CM=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_CMDLG.obj" : $(SOURCE) $(DEP_CPP_WB_CM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_CM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_WB_CM=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_CMDLG.obj" : $(SOURCE) $(DEP_CPP_WB_CM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_CM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	

"$(INTDIR)\WB_CMDLG.obj" : $(SOURCE) $(DEP_CPP_WB_CM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_DATA.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_DA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_DA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_DATA.obj" : $(SOURCE) $(DEP_CPP_WB_DA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_DA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_DA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_DATA.obj" : $(SOURCE) $(DEP_CPP_WB_DA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_DA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WB_DATA.obj" : $(SOURCE) $(DEP_CPP_WB_DA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_DC.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_DC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\Wxwindow\Src\Base\XFSPLINE.cxx"\
	".\..\..\Wxwindow\Src\base\wxspline.cxx"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_DC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_DC.obj" : $(SOURCE) $(DEP_CPP_WB_DC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_DC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\Wxwindow\Src\Base\XFSPLINE.cxx"\
	".\..\..\Wxwindow\Src\base\wxspline.cxx"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_DC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_DC.obj" : $(SOURCE) $(DEP_CPP_WB_DC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_DC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\Wxwindow\Src\Base\XFSPLINE.cxx"\
	".\..\..\Wxwindow\Src\base\wxspline.cxx"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WB_DC.obj" : $(SOURCE) $(DEP_CPP_WB_DC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_DIALG.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_DI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	
NODEP_CPP_WB_DI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_DIALG.obj" : $(SOURCE) $(DEP_CPP_WB_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_DI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	
NODEP_CPP_WB_DI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_DIALG.obj" : $(SOURCE) $(DEP_CPP_WB_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_DI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	

"$(INTDIR)\WB_DIALG.obj" : $(SOURCE) $(DEP_CPP_WB_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_FORM.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_FO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_form.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WB_FO=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_FORM.obj" : $(SOURCE) $(DEP_CPP_WB_FO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_FO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_form.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WB_FO=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_FORM.obj" : $(SOURCE) $(DEP_CPP_WB_FO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_FO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_form.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	

"$(INTDIR)\WB_FORM.obj" : $(SOURCE) $(DEP_CPP_WB_FO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_FRAME.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_FR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	
NODEP_CPP_WB_FR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_FRAME.obj" : $(SOURCE) $(DEP_CPP_WB_FR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_FR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	
NODEP_CPP_WB_FR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_FRAME.obj" : $(SOURCE) $(DEP_CPP_WB_FR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_FR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	

"$(INTDIR)\WB_FRAME.obj" : $(SOURCE) $(DEP_CPP_WB_FR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_GDI.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_GD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_GDI.obj" : $(SOURCE) $(DEP_CPP_WB_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_GD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_GDI.obj" : $(SOURCE) $(DEP_CPP_WB_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WB_GDI.obj" : $(SOURCE) $(DEP_CPP_WB_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_HASH.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_HA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	
NODEP_CPP_WB_HA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_HASH.obj" : $(SOURCE) $(DEP_CPP_WB_HA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_HA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WB_HA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_HASH.obj" : $(SOURCE) $(DEP_CPP_WB_HA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_HA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WB_HASH.obj" : $(SOURCE) $(DEP_CPP_WB_HA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_HELP.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_HE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_help.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	
NODEP_CPP_WB_HE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_HELP.obj" : $(SOURCE) $(DEP_CPP_WB_HE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_HE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_help.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	
NODEP_CPP_WB_HE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_HELP.obj" : $(SOURCE) $(DEP_CPP_WB_HE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_HE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_help.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	

"$(INTDIR)\WB_HELP.obj" : $(SOURCE) $(DEP_CPP_WB_HE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_IPC.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_IP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	
NODEP_CPP_WB_IP=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_IPC.obj" : $(SOURCE) $(DEP_CPP_WB_IP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_IP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	
NODEP_CPP_WB_IP=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_IPC.obj" : $(SOURCE) $(DEP_CPP_WB_IP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_IP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	

"$(INTDIR)\WB_IPC.obj" : $(SOURCE) $(DEP_CPP_WB_IP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_ITEM.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_IT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_IT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_ITEM.obj" : $(SOURCE) $(DEP_CPP_WB_IT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_IT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_IT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_ITEM.obj" : $(SOURCE) $(DEP_CPP_WB_IT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_IT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WB_ITEM.obj" : $(SOURCE) $(DEP_CPP_WB_IT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_LIST.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_LI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WB_LI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_LIST.obj" : $(SOURCE) $(DEP_CPP_WB_LI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_LI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WB_LI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_LIST.obj" : $(SOURCE) $(DEP_CPP_WB_LI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_LI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	

"$(INTDIR)\WB_LIST.obj" : $(SOURCE) $(DEP_CPP_WB_LI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_MAIN.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_MA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_MA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_MAIN.obj" : $(SOURCE) $(DEP_CPP_WB_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_MA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_MA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_MAIN.obj" : $(SOURCE) $(DEP_CPP_WB_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_MA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WB_MAIN.obj" : $(SOURCE) $(DEP_CPP_WB_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_MF.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_MF=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_MF=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_MF.obj" : $(SOURCE) $(DEP_CPP_WB_MF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_MF=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_MF=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_MF.obj" : $(SOURCE) $(DEP_CPP_WB_MF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_MF=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WB_MF.obj" : $(SOURCE) $(DEP_CPP_WB_MF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_MGSTR.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_MG=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WB_MG=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_MGSTR.obj" : $(SOURCE) $(DEP_CPP_WB_MG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_MG=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WB_MG=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_MGSTR.obj" : $(SOURCE) $(DEP_CPP_WB_MG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_MG=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WB_MGSTR.obj" : $(SOURCE) $(DEP_CPP_WB_MG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_OBJ.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_OB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	
NODEP_CPP_WB_OB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_OBJ.obj" : $(SOURCE) $(DEP_CPP_WB_OB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_OB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	
NODEP_CPP_WB_OB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_OBJ.obj" : $(SOURCE) $(DEP_CPP_WB_OB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_OB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	

"$(INTDIR)\WB_OBJ.obj" : $(SOURCE) $(DEP_CPP_WB_OB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_PANEL.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_PA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	
NODEP_CPP_WB_PA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_PANEL.obj" : $(SOURCE) $(DEP_CPP_WB_PA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_PA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	
NODEP_CPP_WB_PA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_PANEL.obj" : $(SOURCE) $(DEP_CPP_WB_PA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_PA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	

"$(INTDIR)\WB_PANEL.obj" : $(SOURCE) $(DEP_CPP_WB_PA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_PRINT.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_PR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_WB_PR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_PRINT.obj" : $(SOURCE) $(DEP_CPP_WB_PR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_PR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_WB_PR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_PRINT.obj" : $(SOURCE) $(DEP_CPP_WB_PR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_PR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	

"$(INTDIR)\WB_PRINT.obj" : $(SOURCE) $(DEP_CPP_WB_PR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_PS.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_PS=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	
NODEP_CPP_WB_PS=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_PS.obj" : $(SOURCE) $(DEP_CPP_WB_PS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_PS=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	
NODEP_CPP_WB_PS=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_PS.obj" : $(SOURCE) $(DEP_CPP_WB_PS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_PS=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	

"$(INTDIR)\WB_PS.obj" : $(SOURCE) $(DEP_CPP_WB_PS) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_RES.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_RE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	
NODEP_CPP_WB_RE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_RES.obj" : $(SOURCE) $(DEP_CPP_WB_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_RE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	
NODEP_CPP_WB_RE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_RES.obj" : $(SOURCE) $(DEP_CPP_WB_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_RE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	

"$(INTDIR)\WB_RES.obj" : $(SOURCE) $(DEP_CPP_WB_RE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_SCROL.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_SC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	
NODEP_CPP_WB_SC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_SCROL.obj" : $(SOURCE) $(DEP_CPP_WB_SC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_SC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	
NODEP_CPP_WB_SC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_SCROL.obj" : $(SOURCE) $(DEP_CPP_WB_SC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_SC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	

"$(INTDIR)\WB_SCROL.obj" : $(SOURCE) $(DEP_CPP_WB_SC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_STAT.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	
NODEP_CPP_WB_ST=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_STAT.obj" : $(SOURCE) $(DEP_CPP_WB_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	
NODEP_CPP_WB_ST=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_STAT.obj" : $(SOURCE) $(DEP_CPP_WB_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	

"$(INTDIR)\WB_STAT.obj" : $(SOURCE) $(DEP_CPP_WB_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_STDEV.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_STD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_STD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_STDEV.obj" : $(SOURCE) $(DEP_CPP_WB_STD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_STD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_STD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_STDEV.obj" : $(SOURCE) $(DEP_CPP_WB_STD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_STD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WB_STDEV.obj" : $(SOURCE) $(DEP_CPP_WB_STD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_SYSEV.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_SY=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_SY=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_SYSEV.obj" : $(SOURCE) $(DEP_CPP_WB_SY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_SY=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WB_SY=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_SYSEV.obj" : $(SOURCE) $(DEP_CPP_WB_SY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_SY=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WB_SYSEV.obj" : $(SOURCE) $(DEP_CPP_WB_SY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_TEXT.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_TE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_TE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_TEXT.obj" : $(SOURCE) $(DEP_CPP_WB_TE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_TE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_TE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_TEXT.obj" : $(SOURCE) $(DEP_CPP_WB_TE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_TE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WB_TEXT.obj" : $(SOURCE) $(DEP_CPP_WB_TE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_TIMER.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_TI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	
NODEP_CPP_WB_TI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_TIMER.obj" : $(SOURCE) $(DEP_CPP_WB_TI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_TI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	
NODEP_CPP_WB_TI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_TIMER.obj" : $(SOURCE) $(DEP_CPP_WB_TI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_TI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	

"$(INTDIR)\WB_TIMER.obj" : $(SOURCE) $(DEP_CPP_WB_TI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_TYPES.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_TY=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WB_TY=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_TYPES.obj" : $(SOURCE) $(DEP_CPP_WB_TY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_TY=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WB_TY=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_TYPES.obj" : $(SOURCE) $(DEP_CPP_WB_TY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_TY=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WB_TYPES.obj" : $(SOURCE) $(DEP_CPP_WB_TY) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_UTILS.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_UT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\Wxwindow\Src\Base\glob.inc"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	
NODEP_CPP_WB_UT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_UTILS.obj" : $(SOURCE) $(DEP_CPP_WB_UT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_UT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\Wxwindow\Src\Base\glob.inc"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	
NODEP_CPP_WB_UT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_UTILS.obj" : $(SOURCE) $(DEP_CPP_WB_UT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_UT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\Wxwindow\Src\Base\glob.inc"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	

"$(INTDIR)\WB_UTILS.obj" : $(SOURCE) $(DEP_CPP_WB_UT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_VLBOX.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_VL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_vlbox.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	".\..\..\wxwindow\include\base\wb_vlbox.h"\
	
NODEP_CPP_WB_VL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_VLBOX.obj" : $(SOURCE) $(DEP_CPP_WB_VL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_VL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_vlbox.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	".\..\..\wxwindow\include\base\wb_vlbox.h"\
	
NODEP_CPP_WB_VL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_VLBOX.obj" : $(SOURCE) $(DEP_CPP_WB_VL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_VL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_vlbox.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	".\..\..\wxwindow\include\base\wb_vlbox.h"\
	

"$(INTDIR)\WB_VLBOX.obj" : $(SOURCE) $(DEP_CPP_WB_VL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_WIN.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_WI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_WI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_WIN.obj" : $(SOURCE) $(DEP_CPP_WB_WI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_WI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WB_WI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_WIN.obj" : $(SOURCE) $(DEP_CPP_WB_WI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_WI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WB_WIN.obj" : $(SOURCE) $(DEP_CPP_WB_WI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_BBAR.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_BB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_bbar.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_tbar.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_BB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_BBAR.obj" : $(SOURCE) $(DEP_CPP_WX_BB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_BB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_bbar.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_tbar.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_BB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_BBAR.obj" : $(SOURCE) $(DEP_CPP_WX_BB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_BB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_bbar.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_tbar.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_BBAR.obj" : $(SOURCE) $(DEP_CPP_WX_BB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_DATE.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_DA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_date.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WX_DA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DATE.obj" : $(SOURCE) $(DEP_CPP_WX_DA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_DA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_date.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WX_DA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DATE.obj" : $(SOURCE) $(DEP_CPP_WX_DA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_DA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_date.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WX_DATE.obj" : $(SOURCE) $(DEP_CPP_WX_DA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_DOC.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_DO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_doc.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_DO=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DOC.obj" : $(SOURCE) $(DEP_CPP_WX_DO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_DO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_doc.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_DO=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DOC.obj" : $(SOURCE) $(DEP_CPP_WX_DO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_DO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_doc.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_print.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	

"$(INTDIR)\WX_DOC.obj" : $(SOURCE) $(DEP_CPP_WX_DO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_ENHDG.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_EN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_enhdg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	
NODEP_CPP_WX_EN=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_ENHDG.obj" : $(SOURCE) $(DEP_CPP_WX_EN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_EN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_enhdg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	
NODEP_CPP_WX_EN=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_ENHDG.obj" : $(SOURCE) $(DEP_CPP_WX_EN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_EN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wx_enhdg.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	

"$(INTDIR)\WX_ENHDG.obj" : $(SOURCE) $(DEP_CPP_WX_EN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_FRAC.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_FR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_frac.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WX_FR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_FRAC.obj" : $(SOURCE) $(DEP_CPP_WX_FR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_FR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_frac.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WX_FR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_FRAC.obj" : $(SOURCE) $(DEP_CPP_WX_FR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_FR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_frac.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	

"$(INTDIR)\WX_FRAC.obj" : $(SOURCE) $(DEP_CPP_WX_FR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_LAY.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_LA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_LA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_LAY.obj" : $(SOURCE) $(DEP_CPP_WX_LA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_LA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_LA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_LAY.obj" : $(SOURCE) $(DEP_CPP_WX_LA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_LA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	

"$(INTDIR)\WX_LAY.obj" : $(SOURCE) $(DEP_CPP_WX_LA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_MEM.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_ME=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_ME=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MEM.obj" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_ME=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_ME=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MEM.obj" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_ME=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_MEM.obj" : $(SOURCE) $(DEP_CPP_WX_ME) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_TBAR.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_TB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_tbar.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WX_TB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TBAR.obj" : $(SOURCE) $(DEP_CPP_WX_TB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_TB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_tbar.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WX_TB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TBAR.obj" : $(SOURCE) $(DEP_CPP_WX_TB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_TB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_tbar.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	

"$(INTDIR)\WX_TBAR.obj" : $(SOURCE) $(DEP_CPP_WX_TB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WX_TIME.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_TI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_time.h"\
	".\..\..\wxwindow\include\base\wx_date.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WX_TI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TIME.obj" : $(SOURCE) $(DEP_CPP_WX_TI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_TI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_time.h"\
	".\..\..\wxwindow\include\base\wx_date.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	
NODEP_CPP_WX_TI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TIME.obj" : $(SOURCE) $(DEP_CPP_WX_TI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_TI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_time.h"\
	".\..\..\wxwindow\include\base\wx_date.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	

"$(INTDIR)\WX_TIME.obj" : $(SOURCE) $(DEP_CPP_WX_TI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WXSTRING.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WXSTR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wxstring.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxregex.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxstrgnu.h"\
	
NODEP_CPP_WXSTR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	".\..\..\wxwindow\include\base\wxregex.h"\
	

"$(INTDIR)\WXSTRING.obj" : $(SOURCE) $(DEP_CPP_WXSTR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WXSTR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wxstring.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxregex.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxstrgnu.h"\
	
NODEP_CPP_WXSTR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	".\..\..\wxwindow\include\base\wxregex.h"\
	

"$(INTDIR)\WXSTRING.obj" : $(SOURCE) $(DEP_CPP_WXSTR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WXSTR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wxstring.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxregex.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxstrgnu.h"\
	
NODEP_CPP_WXSTR=\
	".\..\..\wxwindow\include\base\wxregex.h"\
	

"$(INTDIR)\WXSTRING.obj" : $(SOURCE) $(DEP_CPP_WXSTR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Base\WB_CANVS.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WB_CA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	
NODEP_CPP_WB_CA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_CANVS.obj" : $(SOURCE) $(DEP_CPP_WB_CA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WB_CA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	
NODEP_CPP_WB_CA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WB_CANVS.obj" : $(SOURCE) $(DEP_CPP_WB_CA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WB_CA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	

"$(INTDIR)\WB_CANVS.obj" : $(SOURCE) $(DEP_CPP_WB_CA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_WIN.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_WI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\itsybits\itsybits.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_WI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_WIN.obj" : $(SOURCE) $(DEP_CPP_WX_WI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_WI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\itsybits\itsybits.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_WI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_WIN.obj" : $(SOURCE) $(DEP_CPP_WX_WI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_WI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\itsybits\itsybits.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_WIN.obj" : $(SOURCE) $(DEP_CPP_WX_WI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CANVS.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_CA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WX_CA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CANVS.obj" : $(SOURCE) $(DEP_CPP_WX_CA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_CA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	
NODEP_CPP_WX_CA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CANVS.obj" : $(SOURCE) $(DEP_CPP_WX_CA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_CA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	

"$(INTDIR)\WX_CANVS.obj" : $(SOURCE) $(DEP_CPP_WX_CA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CHECK.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_CH=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_CH=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CHECK.obj" : $(SOURCE) $(DEP_CPP_WX_CH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_CH=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_CH=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CHECK.obj" : $(SOURCE) $(DEP_CPP_WX_CH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_CH=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_CHECK.obj" : $(SOURCE) $(DEP_CPP_WX_CH) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CHOIC.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_CHO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	
NODEP_CPP_WX_CHO=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CHOIC.obj" : $(SOURCE) $(DEP_CPP_WX_CHO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_CHO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	
NODEP_CPP_WX_CHO=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CHOIC.obj" : $(SOURCE) $(DEP_CPP_WX_CHO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_CHO=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	

"$(INTDIR)\WX_CHOIC.obj" : $(SOURCE) $(DEP_CPP_WX_CHO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CLIPB.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_CL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WX_CL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CLIPB.obj" : $(SOURCE) $(DEP_CPP_WX_CL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_CL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WX_CL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CLIPB.obj" : $(SOURCE) $(DEP_CPP_WX_CL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_CL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WX_CLIPB.obj" : $(SOURCE) $(DEP_CPP_WX_CL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_CMDLG.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_CM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_CM=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CMDLG.obj" : $(SOURCE) $(DEP_CPP_WX_CM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_CM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_CM=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_CMDLG.obj" : $(SOURCE) $(DEP_CPP_WX_CM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_CM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	

"$(INTDIR)\WX_CMDLG.obj" : $(SOURCE) $(DEP_CPP_WX_CM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_DB.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_DB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wxstring.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_db.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxregex.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxstrgnu.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	
NODEP_CPP_WX_DB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	".\..\..\wxwindow\include\base\wxregex.h"\
	

"$(INTDIR)\WX_DB.obj" : $(SOURCE) $(DEP_CPP_WX_DB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_DB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wxstring.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_db.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxregex.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxstrgnu.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	
NODEP_CPP_WX_DB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	".\..\..\wxwindow\include\base\wxregex.h"\
	

"$(INTDIR)\WX_DB.obj" : $(SOURCE) $(DEP_CPP_WX_DB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_DB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wxstring.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_db.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxregex.h"\
	".\..\..\wxwindow\include\base\..\\..\\contrib\\wxstring\\wxstrgnu.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	
NODEP_CPP_WX_DB=\
	".\..\..\wxwindow\include\base\wxregex.h"\
	

"$(INTDIR)\WX_DB.obj" : $(SOURCE) $(DEP_CPP_WX_DB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_DC.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_DC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_DC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DC.obj" : $(SOURCE) $(DEP_CPP_WX_DC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_DC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_DC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DC.obj" : $(SOURCE) $(DEP_CPP_WX_DC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_DC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_DC.obj" : $(SOURCE) $(DEP_CPP_WX_DC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_DIALG.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_DI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_DI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DIALG.obj" : $(SOURCE) $(DEP_CPP_WX_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_DI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_DI=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_DIALG.obj" : $(SOURCE) $(DEP_CPP_WX_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_DI=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_DIALG.obj" : $(SOURCE) $(DEP_CPP_WX_DI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_FRAME.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_FRA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\itsybits\itsybits.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_FRA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_FRAME.obj" : $(SOURCE) $(DEP_CPP_WX_FRA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_FRA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\itsybits\itsybits.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_FRA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_FRAME.obj" : $(SOURCE) $(DEP_CPP_WX_FRA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_FRA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\itsybits\itsybits.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_FRAME.obj" : $(SOURCE) $(DEP_CPP_WX_FRA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_GAUGE.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_GA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_GA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_GAUGE.obj" : $(SOURCE) $(DEP_CPP_WX_GA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_GA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_GA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_GAUGE.obj" : $(SOURCE) $(DEP_CPP_WX_GA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_GA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_GAUGE.obj" : $(SOURCE) $(DEP_CPP_WX_GA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_GDI.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\wxwindow\include\msw\wximgfil.h"\
	".\..\..\wxwindow\include\msw\wximgxbm.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	
NODEP_CPP_WX_GD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_GDI.obj" : $(SOURCE) $(DEP_CPP_WX_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\wxwindow\include\msw\wximgfil.h"\
	".\..\..\wxwindow\include\msw\wximgxbm.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	
NODEP_CPP_WX_GD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_GDI.obj" : $(SOURCE) $(DEP_CPP_WX_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_GD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\wxwindow\include\msw\wximgfil.h"\
	".\..\..\wxwindow\include\msw\wximgxbm.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\WX_GDI.obj" : $(SOURCE) $(DEP_CPP_WX_GD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_GROUP.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_GR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_GR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_GROUP.obj" : $(SOURCE) $(DEP_CPP_WX_GR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_GR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_GR=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_GROUP.obj" : $(SOURCE) $(DEP_CPP_WX_GR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_GR=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_GROUP.obj" : $(SOURCE) $(DEP_CPP_WX_GR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_IPC.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_IP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_IP=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_IPC.obj" : $(SOURCE) $(DEP_CPP_WX_IP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_IP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_IP=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_IPC.obj" : $(SOURCE) $(DEP_CPP_WX_IP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_IP=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_ipc.h"\
	".\..\..\wxwindow\include\msw\wx_ipcob.h"\
	".\..\..\wxwindow\include\base\wb_ipcob.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_IPC.obj" : $(SOURCE) $(DEP_CPP_WX_IP) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_ITEM.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_IT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	
NODEP_CPP_WX_IT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_ITEM.obj" : $(SOURCE) $(DEP_CPP_WX_IT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_IT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	
NODEP_CPP_WX_IT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_ITEM.obj" : $(SOURCE) $(DEP_CPP_WX_IT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_IT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	

"$(INTDIR)\WX_ITEM.obj" : $(SOURCE) $(DEP_CPP_WX_IT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_LBOX.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_LB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_LB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_LBOX.obj" : $(SOURCE) $(DEP_CPP_WX_LB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_LB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_LB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_LBOX.obj" : $(SOURCE) $(DEP_CPP_WX_LB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_LB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_LBOX.obj" : $(SOURCE) $(DEP_CPP_WX_LB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MAIN.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_MA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_MA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MAIN.obj" : $(SOURCE) $(DEP_CPP_WX_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_MA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	
NODEP_CPP_WX_MA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MAIN.obj" : $(SOURCE) $(DEP_CPP_WX_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_MA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	

"$(INTDIR)\WX_MAIN.obj" : $(SOURCE) $(DEP_CPP_WX_MA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MENU.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_MEN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_MEN=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MENU.obj" : $(SOURCE) $(DEP_CPP_WX_MEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_MEN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_MEN=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MENU.obj" : $(SOURCE) $(DEP_CPP_WX_MEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_MEN=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_MENU.obj" : $(SOURCE) $(DEP_CPP_WX_MEN) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MESSG.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_MES=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_MES=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MESSG.obj" : $(SOURCE) $(DEP_CPP_WX_MES) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_MES=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_MES=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MESSG.obj" : $(SOURCE) $(DEP_CPP_WX_MES) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_MES=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_MESSG.obj" : $(SOURCE) $(DEP_CPP_WX_MES) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MF.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_MF=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_MF=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MF.obj" : $(SOURCE) $(DEP_CPP_WX_MF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_MF=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_MF=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MF.obj" : $(SOURCE) $(DEP_CPP_WX_MF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_MF=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_mf.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_mf.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_MF.obj" : $(SOURCE) $(DEP_CPP_WX_MF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_MTXT.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_MT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_MT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MTXT.obj" : $(SOURCE) $(DEP_CPP_WX_MT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_MT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_MT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_MTXT.obj" : $(SOURCE) $(DEP_CPP_WX_MT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_MT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_MTXT.obj" : $(SOURCE) $(DEP_CPP_WX_MT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_PANEL.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_PA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	
NODEP_CPP_WX_PA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_PANEL.obj" : $(SOURCE) $(DEP_CPP_WX_PA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_PA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	
NODEP_CPP_WX_PA=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_PANEL.obj" : $(SOURCE) $(DEP_CPP_WX_PA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_PA=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	

"$(INTDIR)\WX_PANEL.obj" : $(SOURCE) $(DEP_CPP_WX_PA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_RBOX.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_RB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_RB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_RBOX.obj" : $(SOURCE) $(DEP_CPP_WX_RB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_RB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_RB=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_RBOX.obj" : $(SOURCE) $(DEP_CPP_WX_RB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_RB=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_RBOX.obj" : $(SOURCE) $(DEP_CPP_WX_RB) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_SCROL.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_SC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_SC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_SCROL.obj" : $(SOURCE) $(DEP_CPP_WX_SC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_SC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_SC=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_SCROL.obj" : $(SOURCE) $(DEP_CPP_WX_SC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_SC=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_SCROL.obj" : $(SOURCE) $(DEP_CPP_WX_SC) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_SLIDR.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_SL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_SL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_SLIDR.obj" : $(SOURCE) $(DEP_CPP_WX_SL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_SL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_SL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_SLIDR.obj" : $(SOURCE) $(DEP_CPP_WX_SL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_SL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_SLIDR.obj" : $(SOURCE) $(DEP_CPP_WX_SL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_STAT.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	
NODEP_CPP_WX_ST=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_STAT.obj" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	
NODEP_CPP_WX_ST=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_STAT.obj" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_ST=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_stat.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_stat.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	

"$(INTDIR)\WX_STAT.obj" : $(SOURCE) $(DEP_CPP_WX_ST) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_TEXT.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_TE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_TE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TEXT.obj" : $(SOURCE) $(DEP_CPP_WX_TE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_TE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_TE=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TEXT.obj" : $(SOURCE) $(DEP_CPP_WX_TE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_TE=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_clipb.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	

"$(INTDIR)\WX_TEXT.obj" : $(SOURCE) $(DEP_CPP_WX_TE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_TIMER.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_TIM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_TIM=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TIMER.obj" : $(SOURCE) $(DEP_CPP_WX_TIM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_TIM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	
NODEP_CPP_WX_TIM=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TIMER.obj" : $(SOURCE) $(DEP_CPP_WX_TIM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_TIM=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	

"$(INTDIR)\WX_TIMER.obj" : $(SOURCE) $(DEP_CPP_WX_TIM) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_TXT.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_TX=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_TX=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TXT.obj" : $(SOURCE) $(DEP_CPP_WX_TX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_TX=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_TX=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_TXT.obj" : $(SOURCE) $(DEP_CPP_WX_TX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_TX=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_TXT.obj" : $(SOURCE) $(DEP_CPP_WX_TX) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_UTILS.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_UT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WX_UT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_UTILS.obj" : $(SOURCE) $(DEP_CPP_WX_UT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_UT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	
NODEP_CPP_WX_UT=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_UTILS.obj" : $(SOURCE) $(DEP_CPP_WX_UT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_UT=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	

"$(INTDIR)\WX_UTILS.obj" : $(SOURCE) $(DEP_CPP_WX_UT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_VLBOX.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_VL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_vlbox.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_vlbox.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	
NODEP_CPP_WX_VL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_VLBOX.obj" : $(SOURCE) $(DEP_CPP_WX_VL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_VL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_vlbox.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_vlbox.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	
NODEP_CPP_WX_VL=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_VLBOX.obj" : $(SOURCE) $(DEP_CPP_WX_VL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_VL=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_vlbox.h"\
	".\..\..\wxwindow\include\msw\wx_scrol.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_vlbox.h"\
	".\..\..\wxwindow\include\base\wb_scrol.h"\
	

"$(INTDIR)\WX_VLBOX.obj" : $(SOURCE) $(DEP_CPP_WX_VL) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\WX_BUTTN.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_BU=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_BU=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_BUTTN.obj" : $(SOURCE) $(DEP_CPP_WX_BU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_BU=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	
NODEP_CPP_WX_BU=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\WX_BUTTN.obj" : $(SOURCE) $(DEP_CPP_WX_BU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_BU=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	

"$(INTDIR)\WX_BUTTN.obj" : $(SOURCE) $(DEP_CPP_WX_BU) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\wx_pdf.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WX_PD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	
NODEP_CPP_WX_PD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\wx_pdf.obj" : $(SOURCE) $(DEP_CPP_WX_PD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WX_PD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	
NODEP_CPP_WX_PD=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\wx_pdf.obj" : $(SOURCE) $(DEP_CPP_WX_PD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WX_PD=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_pdf.h"\
	".\..\..\wxwindow\include\msw\wx_timer.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_timer.h"\
	{$(INCLUDE)}"\sys\TIMEB.H"\
	

"$(INTDIR)\wx_pdf.obj" : $(SOURCE) $(DEP_CPP_WX_PD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\WXWINDOW\SRC\MSW\wximgfil.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WXIMG=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\msw\wximgfil.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	
NODEP_CPP_WXIMG=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\wximgfil.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WXIMG=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\msw\wximgfil.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wx_dcps.h"\
	".\..\..\wxwindow\include\msw\wx_panel.h"\
	".\..\..\wxwindow\include\msw\wx_dcpan.h"\
	".\..\..\wxwindow\include\msw\wx_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_check.h"\
	".\..\..\wxwindow\include\msw\wx_choic.h"\
	".\..\..\wxwindow\include\msw\wx_messg.h"\
	".\..\..\wxwindow\include\msw\wx_lbox.h"\
	".\..\..\wxwindow\include\msw\wx_rbox.h"\
	".\..\..\wxwindow\include\msw\wx_txt.h"\
	".\..\..\wxwindow\include\msw\wx_mtxt.h"\
	".\..\..\wxwindow\include\msw\wx_slidr.h"\
	".\..\..\wxwindow\include\msw\wx_group.h"\
	".\..\..\wxwindow\include\msw\wx_gauge.h"\
	".\..\..\wxwindow\include\msw\wx_menu.h"\
	".\..\..\wxwindow\include\msw\wx_text.h"\
	".\..\..\wxwindow\include\msw\wx_main.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\wxwindow\include\msw\wx_dialg.h"\
	".\..\..\wxwindow\include\base\wx_utils.h"\
	".\..\..\wxwindow\include\msw\wx_cmdlg.h"\
	".\..\..\wxwindow\include\base\wx_res.h"\
	".\..\..\wxwindow\include\base\wx_lay.h"\
	".\..\..\wxwindow\include\msw\wx_wmgr.h"\
	".\..\..\wxwindow\include\msw\wx_privt.h"\
	".\..\..\wxwindow\include\msw\wx_itemp.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_panel.h"\
	".\..\..\wxwindow\include\base\wb_dcpan.h"\
	".\..\..\wxwindow\include\base\wb_buttn.h"\
	".\..\..\wxwindow\include\msw\wx_item.h"\
	".\..\..\wxwindow\include\base\wb_item.h"\
	".\..\..\wxwindow\include\base\wb_check.h"\
	".\..\..\wxwindow\include\base\wb_choic.h"\
	".\..\..\wxwindow\include\base\wb_messg.h"\
	".\..\..\wxwindow\include\base\wb_lbox.h"\
	".\..\..\wxwindow\include\base\wb_rbox.h"\
	".\..\..\wxwindow\include\base\wb_txt.h"\
	".\..\..\wxwindow\include\base\wb_mtxt.h"\
	".\..\..\wxwindow\include\base\wb_slidr.h"\
	".\..\..\wxwindow\include\base\wb_group.h"\
	".\..\..\wxwindow\include\base\wb_gauge.h"\
	".\..\..\wxwindow\include\base\wb_menu.h"\
	".\..\..\wxwindow\include\msw\wx_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_mnuit.h"\
	".\..\..\wxwindow\include\base\wb_text.h"\
	".\..\..\wxwindow\include\base\wx_mgstr.h"\
	".\..\..\wxwindow\include\base\wb_main.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_dialg.h"\
	".\..\..\wxwindow\include\base\wb_cmdlg.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\base\wb_dc.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	
NODEP_CPP_WXIMG=\
	".\..\..\mzscheme\gc\libgc_globals.h"\
	

"$(INTDIR)\wximgfil.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WXIMG=\
	".\..\..\wxwindow\include\base\wx.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\msw\wx_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dc.h"\
	".\..\..\wxwindow\include\msw\wx_dcmem.h"\
	".\..\..\wxwindow\include\msw\wximgfil.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\base\wx_mem.h"\
	".\..\..\wxwindow\include\msw\wx_win.h"\
	".\..\..\wxwindow\include\msw\wx_frame.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_win.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_stdev.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	".\..\..\wxwindow\include\base\wx_sysev.h"\
	".\..\..\wxwindow\include\base\wx_types.h"\
	".\..\..\wxwindow\include\base\wx_hash.h"\
	".\..\..\wxwindow\include\base\wb_frame.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wb_canvs.h"\
	".\..\..\wxwindow\include\msw\wx_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dccan.h"\
	".\..\..\wxwindow\include\base\wb_dcmem.h"\
	

"$(INTDIR)\wximgfil.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
