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
!MESSAGE NMAKE /f "wxutils.mak" CFG="wxwin - Win32 Release"
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

ALL : "$(OUTDIR)\wxutils.lib"

CLEAN : 
	-@erase ".\Release\vc40.pdb"
	-@erase ".\Release\wxutils.lib"
	-@erase ".\Release\Cont.obj"
	-@erase ".\Release\Dialog.obj"
	-@erase ".\Release\Dumfafa.obj"
	-@erase ".\Release\Crbuffri.obj"
	-@erase ".\Release\Wrffrp.obj"
	-@erase ".\Release\Simx.obj"
	-@erase ".\Release\Parse.obj"
	-@erase ".\Release\Draw.obj"
	-@erase ".\Release\Check.obj"
	-@erase ".\Release\wximgxbm.obj"
	-@erase ".\Release\Scan.obj"
	-@erase ".\Release\Zyz3d.obj"
	-@erase ".\Release\DIB.obj"
	-@erase ".\Release\Misc.obj"
	-@erase ".\Release\Wrffrdat.obj"
	-@erase ".\Release\Crifrdat.obj"
	-@erase ".\Release\Data.obj"
	-@erase ".\Release\Button.obj"
	-@erase ".\Release\Fafa.obj"
	-@erase ".\Release\Rdftodat.obj"
	-@erase ".\Release\Wrffri.obj"
	-@erase ".\Release\Hashtab.obj"
	-@erase ".\Release\Zyzgauge.obj"
	-@erase ".\Release\Static.obj"
	-@erase ".\Release\Rdftoi.obj"
	-@erase ".\Release\Create.obj"
	-@erase ".\Release\Rgb.obj"
	-@erase ".\Release\Crifrbuf.obj"
	-@erase ".\Release\Crdatfri.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "NDEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "NDEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\Release/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxutils.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxutils.lib" 
LIB32_OBJS= \
	".\Release\Cont.obj" \
	".\Release\Dialog.obj" \
	".\Release\Dumfafa.obj" \
	".\Release\Crbuffri.obj" \
	".\Release\Wrffrp.obj" \
	".\Release\Simx.obj" \
	".\Release\Parse.obj" \
	".\Release\Draw.obj" \
	".\Release\Check.obj" \
	".\Release\wximgxbm.obj" \
	".\Release\Scan.obj" \
	".\Release\Zyz3d.obj" \
	".\Release\DIB.obj" \
	".\Release\Misc.obj" \
	".\Release\Wrffrdat.obj" \
	".\Release\Crifrdat.obj" \
	".\Release\Data.obj" \
	".\Release\Button.obj" \
	".\Release\Fafa.obj" \
	".\Release\Rdftodat.obj" \
	".\Release\Wrffri.obj" \
	".\Release\Hashtab.obj" \
	".\Release\Zyzgauge.obj" \
	".\Release\Static.obj" \
	".\Release\Rdftoi.obj" \
	".\Release\Create.obj" \
	".\Release\Rgb.obj" \
	".\Release\Crifrbuf.obj" \
	".\Release\Crdatfri.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

ALL : "$(OUTDIR)\wxutils.lib"

CLEAN : 
	-@erase ".\Debug\wxutils.lib"
	-@erase ".\Debug\Dumfafa.obj"
	-@erase ".\Debug\Draw.obj"
	-@erase ".\Debug\Crbuffri.obj"
	-@erase ".\Debug\Wrffri.obj"
	-@erase ".\Debug\Misc.obj"
	-@erase ".\Debug\Check.obj"
	-@erase ".\Debug\Rdftoi.obj"
	-@erase ".\Debug\DIB.obj"
	-@erase ".\Debug\Cont.obj"
	-@erase ".\Debug\Wrffrdat.obj"
	-@erase ".\Debug\Dialog.obj"
	-@erase ".\Debug\Data.obj"
	-@erase ".\Debug\Button.obj"
	-@erase ".\Debug\Fafa.obj"
	-@erase ".\Debug\Wrffrp.obj"
	-@erase ".\Debug\Simx.obj"
	-@erase ".\Debug\Rdftodat.obj"
	-@erase ".\Debug\Static.obj"
	-@erase ".\Debug\wximgxbm.obj"
	-@erase ".\Debug\Create.obj"
	-@erase ".\Debug\Scan.obj"
	-@erase ".\Debug\Hashtab.obj"
	-@erase ".\Debug\Parse.obj"
	-@erase ".\Debug\Zyzgauge.obj"
	-@erase ".\Debug\Rgb.obj"
	-@erase ".\Debug\Crifrdat.obj"
	-@erase ".\Debug\Crifrbuf.obj"
	-@erase ".\Debug\Zyz3d.obj"
	-@erase ".\Debug\Crdatfri.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /GX /Z7 /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /MTd /W3 /Z7 /Od /I "..\..\mzscheme\gc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /Fo"$(INTDIR)/" /c 
CPP_OBJS=.\Debug/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxutils.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxutils.lib" 
LIB32_OBJS= \
	".\Debug\Dumfafa.obj" \
	".\Debug\Draw.obj" \
	".\Debug\Crbuffri.obj" \
	".\Debug\Wrffri.obj" \
	".\Debug\Misc.obj" \
	".\Debug\Check.obj" \
	".\Debug\Rdftoi.obj" \
	".\Debug\DIB.obj" \
	".\Debug\Cont.obj" \
	".\Debug\Wrffrdat.obj" \
	".\Debug\Dialog.obj" \
	".\Debug\Data.obj" \
	".\Debug\Button.obj" \
	".\Debug\Fafa.obj" \
	".\Debug\Wrffrp.obj" \
	".\Debug\Simx.obj" \
	".\Debug\Rdftodat.obj" \
	".\Debug\Static.obj" \
	".\Debug\wximgxbm.obj" \
	".\Debug\Create.obj" \
	".\Debug\Scan.obj" \
	".\Debug\Hashtab.obj" \
	".\Debug\Parse.obj" \
	".\Debug\Zyzgauge.obj" \
	".\Debug\Rgb.obj" \
	".\Debug\Crifrdat.obj" \
	".\Debug\Crifrbuf.obj" \
	".\Debug\Zyz3d.obj" \
	".\Debug\Crdatfri.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

ALL : "$(OUTDIR)\wxutils.lib"

CLEAN : 
	-@erase ".\SGC\vc40.pdb"
	-@erase ".\SGC\wxutils.lib"
	-@erase ".\SGC\Cont.obj"
	-@erase ".\SGC\Dumfafa.obj"
	-@erase ".\SGC\Static.obj"
	-@erase ".\SGC\Rdftoi.obj"
	-@erase ".\SGC\Create.obj"
	-@erase ".\SGC\Zyzgauge.obj"
	-@erase ".\SGC\Parse.obj"
	-@erase ".\SGC\Crdatfri.obj"
	-@erase ".\SGC\Wrffrdat.obj"
	-@erase ".\SGC\Dialog.obj"
	-@erase ".\SGC\Simx.obj"
	-@erase ".\SGC\Draw.obj"
	-@erase ".\SGC\Scan.obj"
	-@erase ".\SGC\Wrffrp.obj"
	-@erase ".\SGC\Crbuffri.obj"
	-@erase ".\SGC\Misc.obj"
	-@erase ".\SGC\wximgxbm.obj"
	-@erase ".\SGC\Check.obj"
	-@erase ".\SGC\Zyz3d.obj"
	-@erase ".\SGC\Data.obj"
	-@erase ".\SGC\Hashtab.obj"
	-@erase ".\SGC\Crifrdat.obj"
	-@erase ".\SGC\Fafa.obj"
	-@erase ".\SGC\Crifrbuf.obj"
	-@erase ".\SGC\Rgb.obj"
	-@erase ".\SGC\DIB.obj"
	-@erase ".\SGC\Button.obj"
	-@erase ".\SGC\Wrffri.obj"
	-@erase ".\SGC\Rdftodat.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE CPP /nologo /W3 /Z7 /Od /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\mred\mzscheme\gc" /I "..\..\wxWindow\contrib\fafa" /I "..\..\mzscheme\gc" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /YX"wx.h" /c
# ADD CPP /nologo /MTd /W3 /Zi /Od /I "..\..\mzscheme\sgc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa" /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW" /D WX_NORMALIZED_PS_FONTS=1 /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT" /c
# SUBTRACT CPP /YX
CPP_PROJ=/nologo /MTd /W3 /Zi /Od /I "..\..\mzscheme\sgc" /I\
 "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I\
 ":..\..\wxwindow\src\base" /I "..\..\wxwindow\src\msw" /I\
 "..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib" /I "..\..\wxWindow\contrib\fafa"\
 /D "_DEBUG" /D "WINNT" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "FOR_MSW"\
 /D WX_NORMALIZED_PS_FONTS=1 /D "USE_SENORA_GC" /D "USE_WXOBJECT_TRACE_COUNT"\
 /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=.\SGC/
CPP_SBRS=
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/wxutils.bsc" 
BSC32_SBRS=
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
LIB32_FLAGS=/nologo /out:"$(OUTDIR)/wxutils.lib" 
LIB32_OBJS= \
	".\SGC\Cont.obj" \
	".\SGC\Dumfafa.obj" \
	".\SGC\Static.obj" \
	".\SGC\Rdftoi.obj" \
	".\SGC\Create.obj" \
	".\SGC\Zyzgauge.obj" \
	".\SGC\Parse.obj" \
	".\SGC\Crdatfri.obj" \
	".\SGC\Wrffrdat.obj" \
	".\SGC\Dialog.obj" \
	".\SGC\Simx.obj" \
	".\SGC\Draw.obj" \
	".\SGC\Scan.obj" \
	".\SGC\Wrffrp.obj" \
	".\SGC\Crbuffri.obj" \
	".\SGC\Misc.obj" \
	".\SGC\wximgxbm.obj" \
	".\SGC\Check.obj" \
	".\SGC\Zyz3d.obj" \
	".\SGC\Data.obj" \
	".\SGC\Hashtab.obj" \
	".\SGC\Crifrdat.obj" \
	".\SGC\Fafa.obj" \
	".\SGC\Crifrbuf.obj" \
	".\SGC\Rgb.obj" \
	".\SGC\DIB.obj" \
	".\SGC\Button.obj" \
	".\SGC\Wrffri.obj" \
	".\SGC\Rdftodat.obj"

"$(OUTDIR)\wxutils.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
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

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrp.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WRFFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Wrffrp.obj" : $(SOURCE) $(DEP_CPP_WRFFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WRFFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Wrffrp.obj" : $(SOURCE) $(DEP_CPP_WRFFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WRFFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Wrffrp.obj" : $(SOURCE) $(DEP_CPP_WRFFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crdatfri.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_CRDAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crdatfri.obj" : $(SOURCE) $(DEP_CPP_CRDAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_CRDAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crdatfri.obj" : $(SOURCE) $(DEP_CPP_CRDAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_CRDAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Crdatfri.obj" : $(SOURCE) $(DEP_CPP_CRDAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Create.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_CREAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Create.obj" : $(SOURCE) $(DEP_CPP_CREAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_CREAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Create.obj" : $(SOURCE) $(DEP_CPP_CREAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_CREAT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Create.obj" : $(SOURCE) $(DEP_CPP_CREAT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrbuf.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_CRIFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crifrbuf.obj" : $(SOURCE) $(DEP_CPP_CRIFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_CRIFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crifrbuf.obj" : $(SOURCE) $(DEP_CPP_CRIFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_CRIFR=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Crifrbuf.obj" : $(SOURCE) $(DEP_CPP_CRIFR) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crifrdat.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_CRIFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crifrdat.obj" : $(SOURCE) $(DEP_CPP_CRIFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_CRIFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crifrdat.obj" : $(SOURCE) $(DEP_CPP_CRIFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_CRIFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Crifrdat.obj" : $(SOURCE) $(DEP_CPP_CRIFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Data.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_DATA_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Data.obj" : $(SOURCE) $(DEP_CPP_DATA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_DATA_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Data.obj" : $(SOURCE) $(DEP_CPP_DATA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_DATA_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	

"$(INTDIR)\Data.obj" : $(SOURCE) $(DEP_CPP_DATA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Hashtab.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_HASHT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Hashtab.obj" : $(SOURCE) $(DEP_CPP_HASHT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_HASHT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Hashtab.obj" : $(SOURCE) $(DEP_CPP_HASHT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_HASHT=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Hashtab.obj" : $(SOURCE) $(DEP_CPP_HASHT) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Misc.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_MISC_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_MISC_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_MISC_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	{$(INCLUDE)}"\sys\STAT.H"\
	

"$(INTDIR)\Misc.obj" : $(SOURCE) $(DEP_CPP_MISC_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Parse.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_PARSE=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Parse.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_PARSE=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Parse.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_PARSE=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Parse.obj" : $(SOURCE) $(DEP_CPP_PARSE) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftodat.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_RDFTO=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Rdftodat.obj" : $(SOURCE) $(DEP_CPP_RDFTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_RDFTO=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Rdftodat.obj" : $(SOURCE) $(DEP_CPP_RDFTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_RDFTO=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Rdftodat.obj" : $(SOURCE) $(DEP_CPP_RDFTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rdftoi.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_RDFTOI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Rdftoi.obj" : $(SOURCE) $(DEP_CPP_RDFTOI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_RDFTOI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Rdftoi.obj" : $(SOURCE) $(DEP_CPP_RDFTOI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_RDFTOI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Rdftoi.obj" : $(SOURCE) $(DEP_CPP_RDFTOI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Rgb.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_RGB_C=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\rgbtab.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Rgb.obj" : $(SOURCE) $(DEP_CPP_RGB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_RGB_C=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\rgbtab.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Rgb.obj" : $(SOURCE) $(DEP_CPP_RGB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_RGB_C=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\rgbtab.h"\
	

"$(INTDIR)\Rgb.obj" : $(SOURCE) $(DEP_CPP_RGB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Scan.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_SCAN_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Scan.obj" : $(SOURCE) $(DEP_CPP_SCAN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_SCAN_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Scan.obj" : $(SOURCE) $(DEP_CPP_SCAN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_SCAN_=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Scan.obj" : $(SOURCE) $(DEP_CPP_SCAN_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Simx.c
DEP_CPP_SIMX_=\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Simx.obj" : $(SOURCE) $(DEP_CPP_SIMX_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Simx.obj" : $(SOURCE) $(DEP_CPP_SIMX_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Simx.obj" : $(SOURCE) $(DEP_CPP_SIMX_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffrdat.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WRFFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Wrffrdat.obj" : $(SOURCE) $(DEP_CPP_WRFFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WRFFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Wrffrdat.obj" : $(SOURCE) $(DEP_CPP_WRFFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WRFFRD=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Wrffrdat.obj" : $(SOURCE) $(DEP_CPP_WRFFRD) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Wrffri.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_WRFFRI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Wrffri.obj" : $(SOURCE) $(DEP_CPP_WRFFRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_WRFFRI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Wrffri.obj" : $(SOURCE) $(DEP_CPP_WRFFRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_WRFFRI=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Wrffri.obj" : $(SOURCE) $(DEP_CPP_WRFFRI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\Crbuffri.c

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_CRBUF=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crbuffri.obj" : $(SOURCE) $(DEP_CPP_CRBUF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_CRBUF=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	".\..\..\wxwindow\contrib\wxxpm\libxpm.34b\lib\xpm34.h"\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\simx.h"\
	

"$(INTDIR)\Crbuffri.obj" : $(SOURCE) $(DEP_CPP_CRBUF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_CRBUF=\
	".\..\..\Wxwindow\Contrib\Wxxpm\Libxpm.34b\Lib\xpm34p.h"\
	

"$(INTDIR)\Crbuffri.obj" : $(SOURCE) $(DEP_CPP_CRBUF) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Utils\Dib\DIB.cxx

!IF  "$(CFG)" == "wxwin - Win32 Release"

DEP_CPP_DIB_C=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	

"$(INTDIR)\DIB.obj" : $(SOURCE) $(DEP_CPP_DIB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"

DEP_CPP_DIB_C=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\gc\gc_cpp.h"\
	".\..\..\mzscheme\gc\gc.h"\
	

"$(INTDIR)\DIB.obj" : $(SOURCE) $(DEP_CPP_DIB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"

DEP_CPP_DIB_C=\
	".\..\..\wxwindow\include\base\common.h"\
	".\..\..\wxwindow\include\msw\wx_gdi.h"\
	".\..\..\wxwindow\include\base\..\..\utils\dib\dib.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxwindow\include\base\wx_ver.h"\
	".\..\..\wxwindow\include\base\wb_gdi.h"\
	".\..\..\wxwindow\include\base\wx_obj.h"\
	".\..\..\wxwindow\include\base\wx_list.h"\
	".\..\..\mzscheme\sgc\gc_cpp.h"\
	".\..\..\mzscheme\sgc\sgc.h"\
	

"$(INTDIR)\DIB.obj" : $(SOURCE) $(DEP_CPP_DIB_C) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyz3d.c
DEP_CPP_ZYZ3D=\
	".\..\..\Wxwindow\Contrib\Gauge\zyz3d.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Zyz3d.obj" : $(SOURCE) $(DEP_CPP_ZYZ3D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Zyz3d.obj" : $(SOURCE) $(DEP_CPP_ZYZ3D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Zyz3d.obj" : $(SOURCE) $(DEP_CPP_ZYZ3D) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Gauge\Zyzgauge.c
DEP_CPP_ZYZGA=\
	".\..\..\Wxwindow\Contrib\Gauge\zyz3d.h"\
	".\..\..\wxwindow\include\base\..\..\contrib\gauge\zyzgauge.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Zyzgauge.obj" : $(SOURCE) $(DEP_CPP_ZYZGA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Zyzgauge.obj" : $(SOURCE) $(DEP_CPP_ZYZGA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Zyzgauge.obj" : $(SOURCE) $(DEP_CPP_ZYZGA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Static.c
DEP_CPP_STATI=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Static.obj" : $(SOURCE) $(DEP_CPP_STATI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Static.obj" : $(SOURCE) $(DEP_CPP_STATI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Static.obj" : $(SOURCE) $(DEP_CPP_STATI) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Check.c
DEP_CPP_CHECK=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Check.obj" : $(SOURCE) $(DEP_CPP_CHECK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Check.obj" : $(SOURCE) $(DEP_CPP_CHECK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Check.obj" : $(SOURCE) $(DEP_CPP_CHECK) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Cont.c
DEP_CPP_CONT_=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Cont.obj" : $(SOURCE) $(DEP_CPP_CONT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Cont.obj" : $(SOURCE) $(DEP_CPP_CONT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Cont.obj" : $(SOURCE) $(DEP_CPP_CONT_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Dialog.c
DEP_CPP_DIALO=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Dialog.obj" : $(SOURCE) $(DEP_CPP_DIALO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Dialog.obj" : $(SOURCE) $(DEP_CPP_DIALO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Dialog.obj" : $(SOURCE) $(DEP_CPP_DIALO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Draw.c
DEP_CPP_DRAW_=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Draw.obj" : $(SOURCE) $(DEP_CPP_DRAW_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Dumfafa.c
DEP_CPP_DUMFA=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Dumfafa.obj" : $(SOURCE) $(DEP_CPP_DUMFA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Dumfafa.obj" : $(SOURCE) $(DEP_CPP_DUMFA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Dumfafa.obj" : $(SOURCE) $(DEP_CPP_DUMFA) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Fafa.c
DEP_CPP_FAFA_=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxwindow\include\base\wx_setup.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Fafa.obj" : $(SOURCE) $(DEP_CPP_FAFA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Fafa.obj" : $(SOURCE) $(DEP_CPP_FAFA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Fafa.obj" : $(SOURCE) $(DEP_CPP_FAFA_) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Contrib\Fafa\Button.c
DEP_CPP_BUTTO=\
	".\..\..\wxWindow\contrib\fafa\fafapriv.h"\
	".\..\..\wxWindow\contrib\fafa\fafa.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\Button.obj" : $(SOURCE) $(DEP_CPP_BUTTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\Button.obj" : $(SOURCE) $(DEP_CPP_BUTTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\Button.obj" : $(SOURCE) $(DEP_CPP_BUTTO) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=..\..\Wxwindow\Src\Msw\wximgxbm.cxx
DEP_CPP_WXIMG=\
	".\..\..\wxwindow\include\msw\wximgxbm.h"\
	

!IF  "$(CFG)" == "wxwin - Win32 Release"


"$(INTDIR)\wximgxbm.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 Debug"


"$(INTDIR)\wximgxbm.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "wxwin - Win32 SGC"


"$(INTDIR)\wximgxbm.obj" : $(SOURCE) $(DEP_CPP_WXIMG) "$(INTDIR)"
   $(CPP) $(CPP_PROJ) $(SOURCE)


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
