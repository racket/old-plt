# Microsoft Developer Studio Generated NMAKE File, Based on mred.dsp
!IF "$(CFG)" == ""
CFG=mred - Win32 Release
!MESSAGE No configuration specified. Defaulting to mred - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mred - Win32 Release" && "$(CFG)" != "mred - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mred.mak" CFG="mred - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mred - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "mred - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "mred - Win32 Release"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\MrEd.exe"

!ELSE 

ALL : "libmred - Win32 Release" "$(OUTDIR)\MrEd.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libmred - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Mred.res"
	-@erase "$(INTDIR)\mrmain.obj"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\MrEd.exe"
	-@erase "$(OUTDIR)\MrEd.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /Fp"$(INTDIR)\mred.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Mred.res" /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mred.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\libmred\Release\libmredxxxxxxx.lib ..\libmzsch\Release\libmzschxxxxxxx.lib ..\libmzgc\Release\libmzgcxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib glu32.lib opengl32.lib comctl32.lib /nologo /subsystem:windows /incremental:no /pdb:"$(OUTDIR)\MrEd.pdb" /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"$(OUTDIR)\MrEd.exe" 
LINK32_OBJS= \
	"$(INTDIR)\mrmain.obj" \
	"$(INTDIR)\uniplt.obj" \
	"$(INTDIR)\Mred.res" \
	"$(OUTDIR)\src\worksp\libmred\Release\libmredxxxxxxx.lib"

"$(OUTDIR)\MrEd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

!IF "$(RECURSE)" == "0" 

ALL : "..\..\..\MrEd.exe"

!ELSE 

ALL : "libmred - Win32 Debug" "..\..\..\MrEd.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libmred - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Mred.res"
	-@erase "$(INTDIR)\mrmain.obj"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\MrEd.pdb"
	-@erase "..\..\..\MrEd.exe"
	-@erase "..\..\..\MrEd.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Zi /Od /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /Fp"$(INTDIR)\mred.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\Mred.res" /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mred.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\libmred\Debug\libmredxxxxxxx.lib ..\libmzsch\Debug\libmzschxxxxxxx.lib ..\libmzgc\Debug\libmzgcxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib glu32.lib opengl32.lib comctl32.lib /nologo /subsystem:windows /incremental:yes /pdb:"$(OUTDIR)\MrEd.pdb" /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"..\..\..\MrEd.exe" 
LINK32_OBJS= \
	"$(INTDIR)\mrmain.obj" \
	"$(INTDIR)\uniplt.obj" \
	"$(INTDIR)\Mred.res" \
	"..\libmred\Debug\libmredxxxxxxx.lib"

"..\..\..\MrEd.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mred.dep")
!INCLUDE "mred.dep"
!ELSE 
!MESSAGE Warning: cannot find "mred.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "mred - Win32 Release" || "$(CFG)" == "mred - Win32 Debug"
SOURCE=.\Mred.rc

"$(INTDIR)\Mred.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=..\..\mred\mrmain.cxx

"$(INTDIR)\mrmain.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\mzscheme\uniplt.c

"$(INTDIR)\uniplt.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "mred - Win32 Release"

"libmred - Win32 Release" : 
   cd "..\libmred"
   $(MAKE) /$(MAKEFLAGS) /F .\libmred.mak CFG="libmred - Win32 Release" 
   cd "..\mred"

"libmred - Win32 ReleaseCLEAN" : 
   cd "..\libmred"
   $(MAKE) /$(MAKEFLAGS) /F .\libmred.mak CFG="libmred - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mred"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

"libmred - Win32 Debug" : 
   cd "..\libmred"
   $(MAKE) /$(MAKEFLAGS) /F .\libmred.mak CFG="libmred - Win32 Debug" 
   cd "..\mred"

"libmred - Win32 DebugCLEAN" : 
   cd "..\libmred"
   $(MAKE) /$(MAKEFLAGS) /F .\libmred.mak CFG="libmred - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mred"

!ENDIF 


!ENDIF 

