# Microsoft Developer Studio Generated NMAKE File, Based on libmzgc.dsp
!IF "$(CFG)" == ""
CFG=LIBMZGC - WIN32 RELEASE
!MESSAGE No configuration specified. Defaulting to LIBMZGC - WIN32 RELEASE.
!ENDIF 

!IF "$(CFG)" != "libmzgc - Win32 Release" && "$(CFG)" != "libmzgc - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmzgc.mak" CFG="LIBMZGC - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmzgc - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libmzgc - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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

!IF  "$(CFG)" == "libmzgc - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : "..\..\..\libmzgcxxxxxxx.dll"


CLEAN :
	-@erase "$(INTDIR)\Allchblk.obj"
	-@erase "$(INTDIR)\Alloc.obj"
	-@erase "$(INTDIR)\Blacklst.obj"
	-@erase "$(INTDIR)\Dyn_load.obj"
	-@erase "$(INTDIR)\Finalize.obj"
	-@erase "$(INTDIR)\Headers.obj"
	-@erase "$(INTDIR)\Mach_dep.obj"
	-@erase "$(INTDIR)\Malloc.obj"
	-@erase "$(INTDIR)\Mallocx.obj"
	-@erase "$(INTDIR)\Mark.obj"
	-@erase "$(INTDIR)\Mark_rts.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\New_hblk.obj"
	-@erase "$(INTDIR)\Obj_map.obj"
	-@erase "$(INTDIR)\Os_dep.obj"
	-@erase "$(INTDIR)\Reclaim.obj"
	-@erase "$(INTDIR)\Stubborn.obj"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\win32_threads.obj"
	-@erase "$(OUTDIR)\libmzgcxxxxxxx.exp"
	-@erase "$(OUTDIR)\libmzgcxxxxxxx.lib"
	-@erase "$(OUTDIR)\libmzgcxxxxxxx.pdb"
	-@erase "..\..\..\libmzgcxxxxxxx.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /GX /Zi /O2 /I "../../mzscheme/gc/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "GC_BUILD" /D "SILENT" /D "OLD_BLOCK_ALLOC" /D "LARGE_CONFIG" /D "ATOMIC_UNCOLLECTABLE" /D INITIAL_MARK_STACK_SIZE=8192 /D "GC_DLL" /Fp"$(INTDIR)\libmzgc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libmzgc.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\libmzgcxxxxxxx.pdb" /debug /machine:I386 /out:"../../../libmzgcxxxxxxx.dll" /implib:"$(OUTDIR)\libmzgcxxxxxxx.lib" 
LINK32_OBJS= \
	"$(INTDIR)\Allchblk.obj" \
	"$(INTDIR)\Alloc.obj" \
	"$(INTDIR)\Blacklst.obj" \
	"$(INTDIR)\Dyn_load.obj" \
	"$(INTDIR)\Finalize.obj" \
	"$(INTDIR)\Headers.obj" \
	"$(INTDIR)\Mach_dep.obj" \
	"$(INTDIR)\Malloc.obj" \
	"$(INTDIR)\Mallocx.obj" \
	"$(INTDIR)\Mark.obj" \
	"$(INTDIR)\Mark_rts.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\New_hblk.obj" \
	"$(INTDIR)\Obj_map.obj" \
	"$(INTDIR)\Os_dep.obj" \
	"$(INTDIR)\Reclaim.obj" \
	"$(INTDIR)\Stubborn.obj" \
	"$(INTDIR)\uniplt.obj" \
	"$(INTDIR)\win32_threads.obj"

"..\..\..\libmzgcxxxxxxx.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libmzgc - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "..\..\..\libmzgcxxxxxxx.dll"


CLEAN :
	-@erase "$(INTDIR)\Allchblk.obj"
	-@erase "$(INTDIR)\Alloc.obj"
	-@erase "$(INTDIR)\Blacklst.obj"
	-@erase "$(INTDIR)\Dyn_load.obj"
	-@erase "$(INTDIR)\Finalize.obj"
	-@erase "$(INTDIR)\Headers.obj"
	-@erase "$(INTDIR)\Mach_dep.obj"
	-@erase "$(INTDIR)\Malloc.obj"
	-@erase "$(INTDIR)\Mallocx.obj"
	-@erase "$(INTDIR)\Mark.obj"
	-@erase "$(INTDIR)\Mark_rts.obj"
	-@erase "$(INTDIR)\Misc.obj"
	-@erase "$(INTDIR)\New_hblk.obj"
	-@erase "$(INTDIR)\Obj_map.obj"
	-@erase "$(INTDIR)\Os_dep.obj"
	-@erase "$(INTDIR)\Reclaim.obj"
	-@erase "$(INTDIR)\Stubborn.obj"
	-@erase "$(INTDIR)\uniplt.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\win32_threads.obj"
	-@erase "$(OUTDIR)\libmzgcxxxxxxx.exp"
	-@erase "$(OUTDIR)\libmzgcxxxxxxx.lib"
	-@erase "$(OUTDIR)\libmzgcxxxxxxx.pdb"
	-@erase "..\..\..\libmzgcxxxxxxx.dll"
	-@erase "..\..\..\libmzgcxxxxxxx.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /GX /Zi /Od /I "../../mzscheme/gc/include" /D "WIN32" /D "DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "GC_BUILD" /D "MD_LIB_MAIN" /D "SILENT" /D "OLD_BLOCK_ALLOC" /D "LARGE_CONFIG" /D "ATOMIC_UNCOLLECTABLE" /D INITIAL_MARK_STACK_SIZE=8192 /D "GC_DLL" /Fp"$(INTDIR)\libmzgc.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libmzgc.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\libmzgcxxxxxxx.pdb" /debug /machine:I386 /nodefaultlib:"kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib" /out:"../../../libmzgcxxxxxxx.dll" /implib:"$(OUTDIR)\libmzgcxxxxxxx.lib" 
LINK32_OBJS= \
	"$(INTDIR)\Allchblk.obj" \
	"$(INTDIR)\Alloc.obj" \
	"$(INTDIR)\Blacklst.obj" \
	"$(INTDIR)\Dyn_load.obj" \
	"$(INTDIR)\Finalize.obj" \
	"$(INTDIR)\Headers.obj" \
	"$(INTDIR)\Mach_dep.obj" \
	"$(INTDIR)\Malloc.obj" \
	"$(INTDIR)\Mallocx.obj" \
	"$(INTDIR)\Mark.obj" \
	"$(INTDIR)\Mark_rts.obj" \
	"$(INTDIR)\Misc.obj" \
	"$(INTDIR)\New_hblk.obj" \
	"$(INTDIR)\Obj_map.obj" \
	"$(INTDIR)\Os_dep.obj" \
	"$(INTDIR)\Reclaim.obj" \
	"$(INTDIR)\Stubborn.obj" \
	"$(INTDIR)\uniplt.obj" \
	"$(INTDIR)\win32_threads.obj"

"..\..\..\libmzgcxxxxxxx.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("libmzgc.dep")
!INCLUDE "libmzgc.dep"
!ELSE 
!MESSAGE Warning: cannot find "libmzgc.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libmzgc - Win32 Release" || "$(CFG)" == "libmzgc - Win32 Debug"
SOURCE=..\..\Mzscheme\Gc\Allchblk.c

"$(INTDIR)\Allchblk.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Alloc.c

"$(INTDIR)\Alloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Blacklst.c

"$(INTDIR)\Blacklst.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Dyn_load.c

"$(INTDIR)\Dyn_load.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Finalize.c

"$(INTDIR)\Finalize.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Headers.c

"$(INTDIR)\Headers.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mach_dep.c

"$(INTDIR)\Mach_dep.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Malloc.c

"$(INTDIR)\Malloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mallocx.c

"$(INTDIR)\Mallocx.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mark.c

"$(INTDIR)\Mark.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Mark_rts.c

"$(INTDIR)\Mark_rts.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Misc.c

"$(INTDIR)\Misc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\New_hblk.c

"$(INTDIR)\New_hblk.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Obj_map.c

"$(INTDIR)\Obj_map.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Os_dep.c

"$(INTDIR)\Os_dep.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Reclaim.c

"$(INTDIR)\Reclaim.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Gc\Stubborn.c

"$(INTDIR)\Stubborn.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\mzscheme\uniplt.c

"$(INTDIR)\uniplt.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\gc\win32_threads.c

"$(INTDIR)\win32_threads.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

