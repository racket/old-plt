# Microsoft Developer Studio Generated NMAKE File, Based on wxme.dsp
!IF "$(CFG)" == ""
CFG=WXME - WIN32 RELEASE
!MESSAGE No configuration specified. Defaulting to WXME - WIN32 RELEASE.
!ENDIF 

!IF "$(CFG)" != "wxme - Win32 Release" && "$(CFG)" != "wxme - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxme.mak" CFG="WXME - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxme - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wxme - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "wxme - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\wxme.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WX_CGREC.obj"
	-@erase "$(INTDIR)\WX_KEYM.obj"
	-@erase "$(INTDIR)\WX_MBUF.obj"
	-@erase "$(INTDIR)\WX_MEDAD.obj"
	-@erase "$(INTDIR)\WX_MEDIA.obj"
	-@erase "$(INTDIR)\WX_MEDIO.obj"
	-@erase "$(INTDIR)\WX_MLINE.obj"
	-@erase "$(INTDIR)\WX_MPBRD.obj"
	-@erase "$(INTDIR)\WX_MPRIV.obj"
	-@erase "$(INTDIR)\WX_MSNIP.obj"
	-@erase "$(INTDIR)\WX_SNIP.obj"
	-@erase "$(INTDIR)\WX_STYLE.obj"
	-@erase "$(INTDIR)\wxGC.obj"
	-@erase "$(INTDIR)\wxJPEG.obj"
	-@erase "$(INTDIR)\xcglue.obj"
	-@erase "$(OUTDIR)\wxme.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxcommon\jpeg" /I "..\jpeg" /I "..\..\wxcommon\zlib" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /Fp"$(INTDIR)\wxme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxme.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxme.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WX_CGREC.obj" \
	"$(INTDIR)\WX_KEYM.obj" \
	"$(INTDIR)\WX_MBUF.obj" \
	"$(INTDIR)\WX_MEDAD.obj" \
	"$(INTDIR)\WX_MEDIA.obj" \
	"$(INTDIR)\WX_MEDIO.obj" \
	"$(INTDIR)\WX_MLINE.obj" \
	"$(INTDIR)\WX_MPBRD.obj" \
	"$(INTDIR)\WX_MPRIV.obj" \
	"$(INTDIR)\WX_MSNIP.obj" \
	"$(INTDIR)\WX_SNIP.obj" \
	"$(INTDIR)\WX_STYLE.obj" \
	"$(INTDIR)\wxGC.obj" \
	"$(INTDIR)\wxJPEG.obj" \
	"$(INTDIR)\xcglue.obj"

"$(OUTDIR)\wxme.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ELSEIF  "$(CFG)" == "wxme - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\wxme.lib"


CLEAN :
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\WX_CGREC.obj"
	-@erase "$(INTDIR)\WX_KEYM.obj"
	-@erase "$(INTDIR)\WX_MBUF.obj"
	-@erase "$(INTDIR)\WX_MEDAD.obj"
	-@erase "$(INTDIR)\WX_MEDIA.obj"
	-@erase "$(INTDIR)\WX_MEDIO.obj"
	-@erase "$(INTDIR)\WX_MLINE.obj"
	-@erase "$(INTDIR)\WX_MPBRD.obj"
	-@erase "$(INTDIR)\WX_MPRIV.obj"
	-@erase "$(INTDIR)\WX_MSNIP.obj"
	-@erase "$(INTDIR)\WX_SNIP.obj"
	-@erase "$(INTDIR)\WX_STYLE.obj"
	-@erase "$(INTDIR)\wxGC.obj"
	-@erase "$(INTDIR)\wxJPEG.obj"
	-@erase "$(INTDIR)\xcglue.obj"
	-@erase "$(OUTDIR)\wxme.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /GX /Zi /Od /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxcommon\jpeg" /I "..\jpeg" /I "..\..\wxcommon\zlib" /D "DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /Fp"$(INTDIR)\wxme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\wxme.bsc" 
BSC32_SBRS= \
	
LIB32=link.exe -lib
LIB32_FLAGS=/nologo /out:"$(OUTDIR)\wxme.lib" 
LIB32_OBJS= \
	"$(INTDIR)\WX_CGREC.obj" \
	"$(INTDIR)\WX_KEYM.obj" \
	"$(INTDIR)\WX_MBUF.obj" \
	"$(INTDIR)\WX_MEDAD.obj" \
	"$(INTDIR)\WX_MEDIA.obj" \
	"$(INTDIR)\WX_MEDIO.obj" \
	"$(INTDIR)\WX_MLINE.obj" \
	"$(INTDIR)\WX_MPBRD.obj" \
	"$(INTDIR)\WX_MPRIV.obj" \
	"$(INTDIR)\WX_MSNIP.obj" \
	"$(INTDIR)\WX_SNIP.obj" \
	"$(INTDIR)\WX_STYLE.obj" \
	"$(INTDIR)\wxGC.obj" \
	"$(INTDIR)\wxJPEG.obj" \
	"$(INTDIR)\xcglue.obj"

"$(OUTDIR)\wxme.lib" : "$(OUTDIR)" $(DEF_FILE) $(LIB32_OBJS)
    $(LIB32) @<<
  $(LIB32_FLAGS) $(DEF_FLAGS) $(LIB32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("wxme.dep")
!INCLUDE "wxme.dep"
!ELSE 
!MESSAGE Warning: cannot find "wxme.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "wxme - Win32 Release" || "$(CFG)" == "wxme - Win32 Debug"
SOURCE=..\..\mred\Wxme\WX_CGREC.cxx

"$(INTDIR)\WX_CGREC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_KEYM.cxx

"$(INTDIR)\WX_KEYM.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MBUF.cxx

"$(INTDIR)\WX_MBUF.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MEDAD.cxx

"$(INTDIR)\WX_MEDAD.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MEDIA.cxx

"$(INTDIR)\WX_MEDIA.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MEDIO.cxx

"$(INTDIR)\WX_MEDIO.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MLINE.cxx

"$(INTDIR)\WX_MLINE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MPBRD.cxx

"$(INTDIR)\WX_MPBRD.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MPRIV.cxx

"$(INTDIR)\WX_MPRIV.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_MSNIP.cxx

"$(INTDIR)\WX_MSNIP.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_SNIP.cxx

"$(INTDIR)\WX_SNIP.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\Wxme\WX_STYLE.cxx

"$(INTDIR)\WX_STYLE.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\wxcommon\wxGC.cxx

"$(INTDIR)\wxGC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\wxcommon\wxJPEG.cxx

"$(INTDIR)\wxJPEG.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\utils\xcglue.c

"$(INTDIR)\xcglue.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

