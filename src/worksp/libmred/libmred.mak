# Microsoft Developer Studio Generated NMAKE File, Based on libmred.dsp
!IF "$(CFG)" == ""
CFG=LIBMRED - WIN32 RELEASE
!MESSAGE No configuration specified. Defaulting to LIBMRED - WIN32 RELEASE.
!ENDIF 

!IF "$(CFG)" != "libmred - Win32 Release" && "$(CFG)" != "libmred - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmred.mak" CFG="LIBMRED - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmred - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libmred - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libmred - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

!IF "$(RECURSE)" == "0" 

ALL : "..\..\..\libmredxxxxxxx.dll"

!ELSE 

ALL : "zlib - Win32 Release" "png - Win32 Release" "wxme - Win32 Release" "libmzsch - Win32 Release" "libmzgc - Win32 Release" "wxwin - Win32 Release" "wxutils - Win32 Release" "wxs - Win32 Release" "jpeg - Win32 Release" "..\..\..\libmredxxxxxxx.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"jpeg - Win32 ReleaseCLEAN" "wxs - Win32 ReleaseCLEAN" "wxutils - Win32 ReleaseCLEAN" "wxwin - Win32 ReleaseCLEAN" "libmzgc - Win32 ReleaseCLEAN" "libmzsch - Win32 ReleaseCLEAN" "wxme - Win32 ReleaseCLEAN" "png - Win32 ReleaseCLEAN" "zlib - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\MRED.obj"
	-@erase "$(INTDIR)\MREDMSW.obj"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\libmredxxxxxxx.exp"
	-@erase "$(OUTDIR)\libmredxxxxxxx.lib"
	-@erase "$(OUTDIR)\libmredxxxxxxx.pdb"
	-@erase "..\..\..\libmredxxxxxxx.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMRED_EXPORTS" /D "GC_DLL" /Fp"$(INTDIR)\libmred.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libmred.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=../libmzsch/release/libmzschxxxxxxx.lib ../libmzgc/release/libmzgcxxxxxxx.lib ../wxs/release/wxs.lib ../wxme/release/wxme.lib ../wxutils/release/wxutils.lib ../jpeg/release/jpeg.lib ../png/release/png.lib ../zlib/release/zlib.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib glu32.lib opengl32.lib winmm.lib comctl32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\libmredxxxxxxx.pdb" /debug /machine:I386 /out:"../../../libmredxxxxxxx.dll" /implib:"$(OUTDIR)\libmredxxxxxxx.lib" 
LINK32_OBJS= \
	"$(INTDIR)\MRED.obj" \
	"$(INTDIR)\MREDMSW.obj" \
	"$(INTDIR)\uniplt.obj" \
	"..\jpeg\Release\jpeg.lib" \
	"..\wxs\Release\wxs.lib" \
	"..\wxutils\Release\wxutils.lib" \
	"..\wxwin\Release\wxwin.lib" \
	"..\libmzgc\Release\libmzgcxxxxxxx.lib" \
	"..\libmzsch\Release\libmzschxxxxxxx.lib" \
	"..\wxme\Release\wxme.lib" \
	"..\png\Release\png.lib" \
	"..\zlib\Release\zlib.lib"

"..\..\..\libmredxxxxxxx.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

!IF "$(RECURSE)" == "0" 

ALL : "..\..\..\libmredxxxxxxx.dll"

!ELSE 

ALL : "zlib - Win32 Debug" "png - Win32 Debug" "wxme - Win32 Debug" "libmzsch - Win32 Debug" "libmzgc - Win32 Debug" "wxwin - Win32 Debug" "wxutils - Win32 Debug" "wxs - Win32 Debug" "jpeg - Win32 Debug" "..\..\..\libmredxxxxxxx.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"jpeg - Win32 DebugCLEAN" "wxs - Win32 DebugCLEAN" "wxutils - Win32 DebugCLEAN" "wxwin - Win32 DebugCLEAN" "libmzgc - Win32 DebugCLEAN" "libmzsch - Win32 DebugCLEAN" "wxme - Win32 DebugCLEAN" "png - Win32 DebugCLEAN" "zlib - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\MRED.obj"
	-@erase "$(INTDIR)\MREDMSW.obj"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\libmredxxxxxxx.exp"
	-@erase "$(OUTDIR)\libmredxxxxxxx.lib"
	-@erase "$(OUTDIR)\libmredxxxxxxx.pdb"
	-@erase "..\..\..\libmredxxxxxxx.dll"
	-@erase "..\..\..\libmredxxxxxxx.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /GX /Zi /Od /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "WIN32" /D "DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMRED_EXPORTS" /D "GC_DLL" /Fp"$(INTDIR)\libmred.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libmred.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=../libmzsch/debug/libmzschxxxxxxx.lib ../libmzgc/debug/libmzgcxxxxxxx.lib ../wxs/debug/wxs.lib ../wxme/debug/wxme.lib ../wxutils/debug/wxutils.lib ../jpeg/debug/jpeg.lib ../png/debug/png.lib ../zlib/debug/zlib.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib glu32.lib opengl32.lib winmm.lib comctl32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\libmredxxxxxxx.pdb" /debug /machine:I386 /out:"../../../libmredxxxxxxx.dll" /implib:"$(OUTDIR)\libmredxxxxxxx.lib" 
LINK32_OBJS= \
	"$(INTDIR)\MRED.obj" \
	"$(INTDIR)\MREDMSW.obj" \
	"$(INTDIR)\uniplt.obj" \
	"..\jpeg\Debug\jpeg.lib" \
	"..\wxs\Debug\wxs.lib" \
	"..\wxutils\Debug\wxutils.lib" \
	"..\wxwin\Debug\wxwin.lib" \
	"..\libmzgc\Debug\libmzgcxxxxxxx.lib" \
	"..\libmzsch\Debug\libmzschxxxxxxx.lib" \
	"..\wxme\Debug\wxme.lib" \
	"..\png\Debug\png.lib" \
	"..\zlib\Debug\zlib.lib"

"..\..\..\libmredxxxxxxx.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

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

MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 

!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("libmred.dep")
!INCLUDE "libmred.dep"
!ELSE 
!MESSAGE Warning: cannot find "libmred.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libmred - Win32 Release" || "$(CFG)" == "libmred - Win32 Debug"
SOURCE=..\..\mred\MRED.cxx

"$(INTDIR)\MRED.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mred\MREDMSW.cxx

"$(INTDIR)\MREDMSW.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\mzscheme\uniplt.c

"$(INTDIR)\uniplt.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "libmred - Win32 Release"

"jpeg - Win32 Release" : 
   cd "..\jpeg"
   $(MAKE) /$(MAKEFLAGS) /F .\jpeg.mak CFG="jpeg - Win32 Release" 
   cd "..\libmred"

"jpeg - Win32 ReleaseCLEAN" : 
   cd "..\jpeg"
   $(MAKE) /$(MAKEFLAGS) /F .\jpeg.mak CFG="jpeg - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"jpeg - Win32 Debug" : 
   cd "..\jpeg"
   $(MAKE) /$(MAKEFLAGS) /F .\jpeg.mak CFG="jpeg - Win32 Debug" 
   cd "..\libmred"

"jpeg - Win32 DebugCLEAN" : 
   cd "..\jpeg"
   $(MAKE) /$(MAKEFLAGS) /F .\jpeg.mak CFG="jpeg - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"wxs - Win32 Release" : 
   cd "..\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Release" 
   cd "..\libmred"

"wxs - Win32 ReleaseCLEAN" : 
   cd "..\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"wxs - Win32 Debug" : 
   cd "..\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Debug" 
   cd "..\libmred"

"wxs - Win32 DebugCLEAN" : 
   cd "..\wxs"
   $(MAKE) /$(MAKEFLAGS) /F .\wxs.mak CFG="wxs - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"wxutils - Win32 Release" : 
   cd "..\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Release" 
   cd "..\libmred"

"wxutils - Win32 ReleaseCLEAN" : 
   cd "..\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"wxutils - Win32 Debug" : 
   cd "..\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Debug" 
   cd "..\libmred"

"wxutils - Win32 DebugCLEAN" : 
   cd "..\wxutils"
   $(MAKE) /$(MAKEFLAGS) /F .\wxutils.mak CFG="wxutils - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"wxwin - Win32 Release" : 
   cd "..\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Release" 
   cd "..\libmred"

"wxwin - Win32 ReleaseCLEAN" : 
   cd "..\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"wxwin - Win32 Debug" : 
   cd "..\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Debug" 
   cd "..\libmred"

"wxwin - Win32 DebugCLEAN" : 
   cd "..\wxwin"
   $(MAKE) /$(MAKEFLAGS) /F .\wxwin.mak CFG="wxwin - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"libmzgc - Win32 Release" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Release" 
   cd "..\libmred"

"libmzgc - Win32 ReleaseCLEAN" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"libmzgc - Win32 Debug" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Debug" 
   cd "..\libmred"

"libmzgc - Win32 DebugCLEAN" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"libmzsch - Win32 Release" : 
   cd "..\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Release" 
   cd "..\libmred"

"libmzsch - Win32 ReleaseCLEAN" : 
   cd "..\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"libmzsch - Win32 Debug" : 
   cd "..\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Debug" 
   cd "..\libmred"

"libmzsch - Win32 DebugCLEAN" : 
   cd "..\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"wxme - Win32 Release" : 
   cd "..\wxme"
   $(MAKE) /$(MAKEFLAGS) /F .\wxme.mak CFG="wxme - Win32 Release" 
   cd "..\libmred"

"wxme - Win32 ReleaseCLEAN" : 
   cd "..\wxme"
   $(MAKE) /$(MAKEFLAGS) /F .\wxme.mak CFG="wxme - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"wxme - Win32 Debug" : 
   cd "..\wxme"
   $(MAKE) /$(MAKEFLAGS) /F .\wxme.mak CFG="wxme - Win32 Debug" 
   cd "..\libmred"

"wxme - Win32 DebugCLEAN" : 
   cd "..\wxme"
   $(MAKE) /$(MAKEFLAGS) /F .\wxme.mak CFG="wxme - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"png - Win32 Release" : 
   cd "..\png"
   $(MAKE) /$(MAKEFLAGS) /F .\png.mak CFG="png - Win32 Release" 
   cd "..\libmred"

"png - Win32 ReleaseCLEAN" : 
   cd "..\png"
   $(MAKE) /$(MAKEFLAGS) /F .\png.mak CFG="png - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"png - Win32 Debug" : 
   cd "..\png"
   $(MAKE) /$(MAKEFLAGS) /F .\png.mak CFG="png - Win32 Debug" 
   cd "..\libmred"

"png - Win32 DebugCLEAN" : 
   cd "..\png"
   $(MAKE) /$(MAKEFLAGS) /F .\png.mak CFG="png - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 

!IF  "$(CFG)" == "libmred - Win32 Release"

"zlib - Win32 Release" : 
   cd "..\zlib"
   $(MAKE) /$(MAKEFLAGS) /F .\zlib.mak CFG="zlib - Win32 Release" 
   cd "..\libmred"

"zlib - Win32 ReleaseCLEAN" : 
   cd "..\zlib"
   $(MAKE) /$(MAKEFLAGS) /F .\zlib.mak CFG="zlib - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmred"

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

"zlib - Win32 Debug" : 
   cd "..\zlib"
   $(MAKE) /$(MAKEFLAGS) /F .\zlib.mak CFG="zlib - Win32 Debug" 
   cd "..\libmred"

"zlib - Win32 DebugCLEAN" : 
   cd "..\zlib"
   $(MAKE) /$(MAKEFLAGS) /F .\zlib.mak CFG="zlib - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmred"

!ENDIF 


!ENDIF 

