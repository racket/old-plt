# Microsoft Developer Studio Generated NMAKE File, Based on MzCOM.dsp
!IF "$(CFG)" == ""
CFG=MzCOM - Win32 Debug
!MESSAGE No configuration specified. Defaulting to MzCOM - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "MzCOM - Win32 Debug" && "$(CFG)" != "MzCOM - Win32 Release"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "MzCOM.mak" CFG="MzCOM - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "MzCOM - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "MzCOM - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\MzCOM.dll" ".\MzCOM.h" ".\Debug\regsvr32.trg"


CLEAN :
	-@erase "$(INTDIR)\mzcom.obj"
	-@erase "$(INTDIR)\MzCOM.res"
	-@erase "$(INTDIR)\mzobj.obj"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\MzCOM.dll"
	-@erase "$(OUTDIR)\MzCOM.exp"
	-@erase "$(OUTDIR)\MzCOM.ilk"
	-@erase "$(OUTDIR)\MzCOM.lib"
	-@erase "$(OUTDIR)\MzCOM.pdb"
	-@erase ".\MzCOM.h"
	-@erase ".\MzCOM.tlb"
	-@erase ".\MzCOM_i.c"
	-@erase ".\Debug\regsvr32.trg"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\collects\mzscheme\include" /I "../../mzcom" /I "." /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "_ATL_STATIC_REGISTRY" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
MTL_PROJ=
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\MzCOM.res" /i "../../mzcom" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MzCOM.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib mzsrc.lib gc.lib wsock32.lib /nologo /subsystem:windows /dll /incremental:yes /pdb:"$(OUTDIR)\MzCOM.pdb" /debug /machine:I386 /nodefaultlib:"LIBCMT" /def:".\MzCOM.def" /out:"$(OUTDIR)\MzCOM.dll" /implib:"$(OUTDIR)\MzCOM.lib" /pdbtype:sept /libpath:"..\mzsrc\Release" /libpath:"..\gc\Release" 
DEF_FILE= \
	".\MzCOM.def"
LINK32_OBJS= \
	"$(INTDIR)\MzCOM.res" \
	"$(INTDIR)\mzcom.obj" \
	"$(INTDIR)\mzobj.obj" \
	"$(INTDIR)\stdafx.obj"

"$(OUTDIR)\MzCOM.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\Debug
TargetPath=.\Debug\MzCOM.dll
InputPath=.\Debug\MzCOM.dll
SOURCE="$(InputPath)"

"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat 
	@echo off 
	regsvr32 /s /c "$(TargetPath)" 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
<< 
	

!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\MzCOM.dll" ".\MzCOM.h" ".\Release\regsvr32.trg"


CLEAN :
	-@erase "$(INTDIR)\mzcom.obj"
	-@erase "$(INTDIR)\MzCOM.res"
	-@erase "$(INTDIR)\mzobj.obj"
	-@erase "$(INTDIR)\stdafx.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\MzCOM.dll"
	-@erase "$(OUTDIR)\MzCOM.exp"
	-@erase "$(OUTDIR)\MzCOM.lib"
	-@erase ".\MzCOM.h"
	-@erase ".\MzCOM.tlb"
	-@erase ".\MzCOM_i.c"
	-@erase ".\Release\regsvr32.trg"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /O1 /I "..\..\..\collects\mzscheme\include" /I "..\..\src\mzcom" /I "." /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "_ATL_STATIC_REGISTRY" /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
MTL_PROJ=
RSC=rc.exe
RSC_PROJ=/l 0x409 /fo"$(INTDIR)\MzCOM.res" /i "../../mzcom" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\MzCOM.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib mzsrc.lib gc.lib wsock32.lib /nologo /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\MzCOM.pdb" /machine:I386 /def:".\MzCOM.def" /out:"$(OUTDIR)\MzCOM.dll" /implib:"$(OUTDIR)\MzCOM.lib" /libpath:"..\mzsrc\Release" /libpath:"..\gc\Release" 
DEF_FILE= \
	".\MzCOM.def"
LINK32_OBJS= \
	"$(INTDIR)\MzCOM.res" \
	"$(INTDIR)\mzcom.obj" \
	"$(INTDIR)\mzobj.obj" \
	"$(INTDIR)\stdafx.obj"

"$(OUTDIR)\MzCOM.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

OutDir=.\Release
TargetPath=.\Release\MzCOM.dll
InputPath=.\Release\MzCOM.dll
SOURCE="$(InputPath)"

"$(OUTDIR)\regsvr32.trg" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	<<tempfile.bat 
	@echo off 
	regsvr32 /s /c "$(TargetPath)" 
	echo regsvr32 exec. time > "$(OutDir)\regsvr32.trg" 
<< 
	

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("MzCOM.dep")
!INCLUDE "MzCOM.dep"
!ELSE 
!MESSAGE Warning: cannot find "MzCOM.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "MzCOM - Win32 Debug" || "$(CFG)" == "MzCOM - Win32 Release"
SOURCE=..\..\mzcom\mzcom.cxx

"$(INTDIR)\mzcom.obj" : $(SOURCE) "$(INTDIR)" ".\MzCOM_i.c"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzcom\MzCOM.idl

!IF  "$(CFG)" == "MzCOM - Win32 Debug"

MTL_SWITCHES=/tlb ".\MzCOM.tlb" /h "MzCOM.h" /iid "MzCOM_i.c" /Oicf 

".\MzCOM.tlb"	".\MzCOM.h"	".\MzCOM_i.c" : $(SOURCE) "$(INTDIR)"
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ELSEIF  "$(CFG)" == "MzCOM - Win32 Release"

MTL_SWITCHES=/tlb ".\MzCOM.tlb" /h "MzCOM.h" /iid "MzCOM_i.c" /Oicf 

".\MzCOM.tlb"	".\MzCOM.h"	".\MzCOM_i.c" : $(SOURCE) "$(INTDIR)"
	$(MTL) @<<
  $(MTL_SWITCHES) $(SOURCE)
<<


!ENDIF 

SOURCE=.\MzCOM.rc

"$(INTDIR)\MzCOM.res" : $(SOURCE) "$(INTDIR)" ".\MzCOM.tlb"
	$(RSC) $(RSC_PROJ) $(SOURCE)


SOURCE=..\..\mzcom\mzobj.cxx

"$(INTDIR)\mzobj.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzcom\stdafx.cxx

"$(INTDIR)\stdafx.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

