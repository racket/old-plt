# Microsoft Developer Studio Generated NMAKE File, Based on mzscheme.dsp
!IF "$(CFG)" == ""
CFG=mzscheme - Win32 Release
!MESSAGE No configuration specified. Defaulting to mzscheme - Win32 Release.
!ENDIF 

!IF "$(CFG)" != "mzscheme - Win32 Release" && "$(CFG)" != "mzscheme - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mzscheme.mak" CFG="mzscheme - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mzscheme - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "mzscheme - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "mzscheme - Win32 Release"

OUTDIR=.\..\..\..\..\plt
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

!IF "$(RECURSE)" == "0" 

ALL : "$(OUTDIR)\MzScheme.exe"

!ELSE 

ALL : "libmzgc - Win32 Release" "libmzsch - Win32 Release" "$(OUTDIR)\MzScheme.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libmzsch - Win32 ReleaseCLEAN" "libmzgc - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Main.obj"
	-@erase "$(INTDIR)\mzscheme.res"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\MzScheme.exe"
	-@erase "$(OUTDIR)\MzScheme.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "..\..\mzscheme\include" /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "GC_DLL" /Fp"$(INTDIR)\mzscheme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzscheme.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\libmzgc\Release\libmzgcxxxxxxx.lib ..\libmzsch\Release\libmzschxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\MzScheme.pdb" /debug /machine:I386 /out:"$(OUTDIR)\MzScheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Main.obj" \
	"$(INTDIR)\uniplt.obj" \
	"$(INTDIR)\mzscheme.res" \
	"$(OUTDIR)\src\worksp\libmzsch\Release\libmzschxxxxxxx.lib" \
	"$(OUTDIR)\src\worksp\libmzgc\Release\libmzgcxxxxxxx.lib"

"$(OUTDIR)\MzScheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

# Begin Custom Macros
OutDir=.\..\..\..\..\plt
# End Custom Macros

$(DS_POSTBUILD_DEP) : "libmzgc - Win32 Release" "libmzsch - Win32 Release" "$(OUTDIR)\MzScheme.exe"
   cd ..\..\mzscheme\dynsrc
	mkmzdyn.bat
	cd ..\..\worksp\mzscheme
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

!IF "$(RECURSE)" == "0" 

ALL : "..\..\..\MzScheme.exe"

!ELSE 

ALL : "libmzgc - Win32 Debug" "libmzsch - Win32 Debug" "..\..\..\MzScheme.exe"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libmzsch - Win32 DebugCLEAN" "libmzgc - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Main.obj"
	-@erase "$(INTDIR)\mzscheme.res"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\MzScheme.pdb"
	-@erase "..\..\..\MzScheme.exe"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /GX /Zi /Od /I "..\..\mzscheme\include" /D "WIN32" /D "DEBUG" /D "_CONSOLE" /D "GC_DLL" /Fp"$(INTDIR)\mzscheme.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\mzscheme.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=..\libmzgc\Debug\libmzgcxxxxxxx.lib ..\libmzsch\Debug\libmzschxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /subsystem:console /incremental:no /pdb:"$(OUTDIR)\MzScheme.pdb" /debug /machine:I386 /nodefaultlib:"kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib" /out:"..\..\..\MzScheme.exe" 
LINK32_OBJS= \
	"$(INTDIR)\Main.obj" \
	"$(INTDIR)\uniplt.obj" \
	"$(INTDIR)\mzscheme.res" \
	"..\libmzsch\Debug\libmzschxxxxxxx.lib" \
	"..\libmzgc\Debug\libmzgcxxxxxxx.lib"

"..\..\..\MzScheme.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

SOURCE="$(InputPath)"
DS_POSTBUILD_DEP=$(INTDIR)\postbld.dep

ALL : $(DS_POSTBUILD_DEP)

$(DS_POSTBUILD_DEP) : "libmzgc - Win32 Debug" "libmzsch - Win32 Debug" "..\..\..\MzScheme.exe"
   cd ..\..\mzscheme\dynsrc
	mkmzdyn.bat
	cd ..\..\worksp\mzscheme
	echo Helper for Post-build step > "$(DS_POSTBUILD_DEP)"

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

RSC_PROJ=/l 0x409 /fo"$(INTDIR)\mzscheme.res" /d "NDEBUG" 

!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("mzscheme.dep")
!INCLUDE "mzscheme.dep"
!ELSE 
!MESSAGE Warning: cannot find "mzscheme.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "mzscheme - Win32 Release" || "$(CFG)" == "mzscheme - Win32 Debug"
SOURCE=..\..\Mzscheme\Main.c

"$(INTDIR)\Main.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=.\mzscheme.rc

"$(INTDIR)\mzscheme.res" : $(SOURCE) "$(INTDIR)"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=.\uniplt.c

"$(INTDIR)\uniplt.obj" : $(SOURCE) "$(INTDIR)"


!IF  "$(CFG)" == "mzscheme - Win32 Release"

"libmzsch - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Release" 
   cd "..\mzscheme"

"libmzsch - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

"libmzsch - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Debug" 
   cd "..\mzscheme"

"libmzsch - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\libmzsch"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzsch.mak CFG="libmzsch - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ENDIF 

!IF  "$(CFG)" == "mzscheme - Win32 Release"

"libmzgc - Win32 Release" : 
   cd "\Matthew\plt\src\worksp\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Release" 
   cd "..\mzscheme"

"libmzgc - Win32 ReleaseCLEAN" : 
   cd "\Matthew\plt\src\worksp\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Release" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ELSEIF  "$(CFG)" == "mzscheme - Win32 Debug"

"libmzgc - Win32 Debug" : 
   cd "\Matthew\plt\src\worksp\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Debug" 
   cd "..\mzscheme"

"libmzgc - Win32 DebugCLEAN" : 
   cd "\Matthew\plt\src\worksp\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\mzscheme"

!ENDIF 


!ENDIF 

