# Microsoft Developer Studio Generated NMAKE File, Based on libmzsch.dsp
!IF "$(CFG)" == ""
CFG=LIBMZSCH - WIN32 RELEASE
!MESSAGE No configuration specified. Defaulting to LIBMZSCH - WIN32 RELEASE.
!ENDIF 

!IF "$(CFG)" != "libmzsch - Win32 Release" && "$(CFG)" != "libmzsch - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmzsch.mak" CFG="LIBMZSCH - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmzsch - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libmzsch - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
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

!IF  "$(CFG)" == "libmzsch - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

!IF "$(RECURSE)" == "0" 

ALL : "..\..\..\libmzschxxxxxxx.dll"

!ELSE 

ALL : "libmzgc - Win32 Release" "..\..\..\libmzschxxxxxxx.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libmzgc - Win32 ReleaseCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Bignum.obj"
	-@erase "$(INTDIR)\Bool.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\Char.obj"
	-@erase "$(INTDIR)\Complex.obj"
	-@erase "$(INTDIR)\Dynext.obj"
	-@erase "$(INTDIR)\Env.obj"
	-@erase "$(INTDIR)\Error.obj"
	-@erase "$(INTDIR)\Eval.obj"
	-@erase "$(INTDIR)\ffi.obj"
	-@erase "$(INTDIR)\File.obj"
	-@erase "$(INTDIR)\foreign.obj"
	-@erase "$(INTDIR)\Fun.obj"
	-@erase "$(INTDIR)\gmp.obj"
	-@erase "$(INTDIR)\Hash.obj"
	-@erase "$(INTDIR)\image.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\module.obj"
	-@erase "$(INTDIR)\mzsj86.obj"
	-@erase "$(INTDIR)\network.obj"
	-@erase "$(INTDIR)\numarith.obj"
	-@erase "$(INTDIR)\Number.obj"
	-@erase "$(INTDIR)\numcomp.obj"
	-@erase "$(INTDIR)\numstr.obj"
	-@erase "$(INTDIR)\Port.obj"
	-@erase "$(INTDIR)\portfun.obj"
	-@erase "$(INTDIR)\prep_cif.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Rational.obj"
	-@erase "$(INTDIR)\Read.obj"
	-@erase "$(INTDIR)\Regexp.obj"
	-@erase "$(INTDIR)\Salloc.obj"
	-@erase "$(INTDIR)\Sema.obj"
	-@erase "$(INTDIR)\Setjmpup.obj"
	-@erase "$(INTDIR)\String.obj"
	-@erase "$(INTDIR)\Struct.obj"
	-@erase "$(INTDIR)\stxobj.obj"
	-@erase "$(INTDIR)\Symbol.obj"
	-@erase "$(INTDIR)\Syntax.obj"
	-@erase "$(INTDIR)\thread.obj"
	-@erase "$(INTDIR)\Type.obj"
	-@erase "$(INTDIR)\types.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\vector.obj"
	-@erase "$(INTDIR)\win32.obj"
	-@erase "$(OUTDIR)\libmzschxxxxxxx.exp"
	-@erase "$(OUTDIR)\libmzschxxxxxxx.lib"
	-@erase "$(OUTDIR)\libmzschxxxxxxx.pdb"
	-@erase "..\..\..\libmzschxxxxxxx.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MT /W3 /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /I "..\..\mzscheme\src" /I "..\..\foreign\libffi_msvc" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "__STDC__" /D "_USRDLL" /D "GC_DLL" /Fp"$(INTDIR)\libmzsch.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libmzsch.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=../libmzgc/Release/libmzgcxxxxxxx.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /dll /incremental:no /pdb:"$(OUTDIR)\libmzschxxxxxxx.pdb" /debug /machine:I386 /out:"../../../libmzschxxxxxxx.dll" /implib:"$(OUTDIR)\libmzschxxxxxxx.lib" 
LINK32_OBJS= \
	"$(INTDIR)\Bignum.obj" \
	"$(INTDIR)\Bool.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\Char.obj" \
	"$(INTDIR)\Complex.obj" \
	"$(INTDIR)\Dynext.obj" \
	"$(INTDIR)\Env.obj" \
	"$(INTDIR)\Error.obj" \
	"$(INTDIR)\Eval.obj" \
	"$(INTDIR)\ffi.obj" \
	"$(INTDIR)\File.obj" \
	"$(INTDIR)\foreign.obj" \
	"$(INTDIR)\Fun.obj" \
	"$(INTDIR)\gmp.obj" \
	"$(INTDIR)\Hash.obj" \
	"$(INTDIR)\image.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\module.obj" \
	"$(INTDIR)\mzsj86.obj" \
	"$(INTDIR)\network.obj" \
	"$(INTDIR)\numarith.obj" \
	"$(INTDIR)\Number.obj" \
	"$(INTDIR)\numcomp.obj" \
	"$(INTDIR)\numstr.obj" \
	"$(INTDIR)\Port.obj" \
	"$(INTDIR)\portfun.obj" \
	"$(INTDIR)\prep_cif.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Rational.obj" \
	"$(INTDIR)\Read.obj" \
	"$(INTDIR)\Regexp.obj" \
	"$(INTDIR)\Salloc.obj" \
	"$(INTDIR)\Sema.obj" \
	"$(INTDIR)\Setjmpup.obj" \
	"$(INTDIR)\String.obj" \
	"$(INTDIR)\Struct.obj" \
	"$(INTDIR)\stxobj.obj" \
	"$(INTDIR)\Symbol.obj" \
	"$(INTDIR)\Syntax.obj" \
	"$(INTDIR)\thread.obj" \
	"$(INTDIR)\Type.obj" \
	"$(INTDIR)\types.obj" \
	"$(INTDIR)\vector.obj" \
	"$(INTDIR)\win32.obj" \
	"..\libmzgc\Release\libmzgcxxxxxxx.lib"

"..\..\..\libmzschxxxxxxx.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libmzsch - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

!IF "$(RECURSE)" == "0" 

ALL : "..\..\..\libmzschxxxxxxx.dll"

!ELSE 

ALL : "libmzgc - Win32 Debug" "..\..\..\libmzschxxxxxxx.dll"

!ENDIF 

!IF "$(RECURSE)" == "1" 
CLEAN :"libmzgc - Win32 DebugCLEAN" 
!ELSE 
CLEAN :
!ENDIF 
	-@erase "$(INTDIR)\Bignum.obj"
	-@erase "$(INTDIR)\Bool.obj"
	-@erase "$(INTDIR)\builtin.obj"
	-@erase "$(INTDIR)\Char.obj"
	-@erase "$(INTDIR)\Complex.obj"
	-@erase "$(INTDIR)\Dynext.obj"
	-@erase "$(INTDIR)\Env.obj"
	-@erase "$(INTDIR)\Error.obj"
	-@erase "$(INTDIR)\Eval.obj"
	-@erase "$(INTDIR)\ffi.obj"
	-@erase "$(INTDIR)\File.obj"
	-@erase "$(INTDIR)\foreign.obj"
	-@erase "$(INTDIR)\Fun.obj"
	-@erase "$(INTDIR)\gmp.obj"
	-@erase "$(INTDIR)\Hash.obj"
	-@erase "$(INTDIR)\image.obj"
	-@erase "$(INTDIR)\List.obj"
	-@erase "$(INTDIR)\module.obj"
	-@erase "$(INTDIR)\mzsj86.obj"
	-@erase "$(INTDIR)\network.obj"
	-@erase "$(INTDIR)\numarith.obj"
	-@erase "$(INTDIR)\Number.obj"
	-@erase "$(INTDIR)\numcomp.obj"
	-@erase "$(INTDIR)\numstr.obj"
	-@erase "$(INTDIR)\Port.obj"
	-@erase "$(INTDIR)\portfun.obj"
	-@erase "$(INTDIR)\prep_cif.obj"
	-@erase "$(INTDIR)\Print.obj"
	-@erase "$(INTDIR)\Rational.obj"
	-@erase "$(INTDIR)\Read.obj"
	-@erase "$(INTDIR)\Regexp.obj"
	-@erase "$(INTDIR)\Salloc.obj"
	-@erase "$(INTDIR)\Sema.obj"
	-@erase "$(INTDIR)\Setjmpup.obj"
	-@erase "$(INTDIR)\String.obj"
	-@erase "$(INTDIR)\Struct.obj"
	-@erase "$(INTDIR)\stxobj.obj"
	-@erase "$(INTDIR)\Symbol.obj"
	-@erase "$(INTDIR)\Syntax.obj"
	-@erase "$(INTDIR)\thread.obj"
	-@erase "$(INTDIR)\Type.obj"
	-@erase "$(INTDIR)\types.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(INTDIR)\vector.obj"
	-@erase "$(INTDIR)\win32.obj"
	-@erase "$(OUTDIR)\libmzschxxxxxxx.exp"
	-@erase "$(OUTDIR)\libmzschxxxxxxx.lib"
	-@erase "$(OUTDIR)\libmzschxxxxxxx.pdb"
	-@erase "..\..\..\libmzschxxxxxxx.dll"
	-@erase "..\..\..\libmzschxxxxxxx.ilk"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

CPP_PROJ=/nologo /MTd /W3 /Zi /Od /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /I "..\..\mzscheme\src" /I "..\..\foreign\libffi_msvc" /D "WIN32" /D "DEBUG" /D "_WINDOWS" /D "__STDC__" /D "_USRDLL" /D "GC_DLL" /Fp"$(INTDIR)\libmzsch.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\libmzsch.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=../libmzgc/Debug/libmzgcxxxxxxx.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /dll /incremental:yes /pdb:"$(OUTDIR)\libmzschxxxxxxx.pdb" /debug /machine:I386 /out:"../../../libmzschxxxxxxx.dll" /implib:"$(OUTDIR)\libmzschxxxxxxx.lib" 
LINK32_OBJS= \
	"$(INTDIR)\Bignum.obj" \
	"$(INTDIR)\Bool.obj" \
	"$(INTDIR)\builtin.obj" \
	"$(INTDIR)\Char.obj" \
	"$(INTDIR)\Complex.obj" \
	"$(INTDIR)\Dynext.obj" \
	"$(INTDIR)\Env.obj" \
	"$(INTDIR)\Error.obj" \
	"$(INTDIR)\Eval.obj" \
	"$(INTDIR)\ffi.obj" \
	"$(INTDIR)\File.obj" \
	"$(INTDIR)\foreign.obj" \
	"$(INTDIR)\Fun.obj" \
	"$(INTDIR)\gmp.obj" \
	"$(INTDIR)\Hash.obj" \
	"$(INTDIR)\image.obj" \
	"$(INTDIR)\List.obj" \
	"$(INTDIR)\module.obj" \
	"$(INTDIR)\mzsj86.obj" \
	"$(INTDIR)\network.obj" \
	"$(INTDIR)\numarith.obj" \
	"$(INTDIR)\Number.obj" \
	"$(INTDIR)\numcomp.obj" \
	"$(INTDIR)\numstr.obj" \
	"$(INTDIR)\Port.obj" \
	"$(INTDIR)\portfun.obj" \
	"$(INTDIR)\prep_cif.obj" \
	"$(INTDIR)\Print.obj" \
	"$(INTDIR)\Rational.obj" \
	"$(INTDIR)\Read.obj" \
	"$(INTDIR)\Regexp.obj" \
	"$(INTDIR)\Salloc.obj" \
	"$(INTDIR)\Sema.obj" \
	"$(INTDIR)\Setjmpup.obj" \
	"$(INTDIR)\String.obj" \
	"$(INTDIR)\Struct.obj" \
	"$(INTDIR)\stxobj.obj" \
	"$(INTDIR)\Symbol.obj" \
	"$(INTDIR)\Syntax.obj" \
	"$(INTDIR)\thread.obj" \
	"$(INTDIR)\Type.obj" \
	"$(INTDIR)\types.obj" \
	"$(INTDIR)\vector.obj" \
	"$(INTDIR)\win32.obj" \
	"..\libmzgc\Debug\libmzgcxxxxxxx.lib"

"..\..\..\libmzschxxxxxxx.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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
!IF EXISTS("libmzsch.dep")
!INCLUDE "libmzsch.dep"
!ELSE 
!MESSAGE Warning: cannot find "libmzsch.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "libmzsch - Win32 Release" || "$(CFG)" == "libmzsch - Win32 Debug"
SOURCE=..\..\Mzscheme\Src\Bignum.c

"$(INTDIR)\Bignum.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Bool.c

"$(INTDIR)\Bool.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\builtin.c

"$(INTDIR)\builtin.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Char.c

"$(INTDIR)\Char.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Complex.c

"$(INTDIR)\Complex.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Dynext.c

"$(INTDIR)\Dynext.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Env.c

"$(INTDIR)\Env.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Error.c

"$(INTDIR)\Error.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Eval.c

"$(INTDIR)\Eval.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\foreign\libffi_msvc\ffi.c

"$(INTDIR)\ffi.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\File.c

"$(INTDIR)\File.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\foreign\foreign.c

"$(INTDIR)\foreign.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Fun.c

"$(INTDIR)\Fun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\gmp\gmp.c

"$(INTDIR)\gmp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Hash.c

"$(INTDIR)\Hash.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\MZSCHEME\SRC\image.c

"$(INTDIR)\image.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\List.c

"$(INTDIR)\List.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\module.c

"$(INTDIR)\module.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\mzsj86.c

"$(INTDIR)\mzsj86.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\network.c

"$(INTDIR)\network.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\numarith.c

"$(INTDIR)\numarith.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Number.c

"$(INTDIR)\Number.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\numcomp.c

"$(INTDIR)\numcomp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\numstr.c

"$(INTDIR)\numstr.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Port.c

"$(INTDIR)\Port.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\portfun.c

"$(INTDIR)\portfun.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\foreign\libffi_msvc\prep_cif.c

"$(INTDIR)\prep_cif.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Print.c

"$(INTDIR)\Print.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Rational.c

"$(INTDIR)\Rational.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Read.c

"$(INTDIR)\Read.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Regexp.c

"$(INTDIR)\Regexp.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Salloc.c

"$(INTDIR)\Salloc.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Sema.c

"$(INTDIR)\Sema.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Setjmpup.c

"$(INTDIR)\Setjmpup.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\String.c

"$(INTDIR)\String.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Struct.c

"$(INTDIR)\Struct.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\stxobj.c

"$(INTDIR)\stxobj.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Symbol.c

"$(INTDIR)\Symbol.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Syntax.c

"$(INTDIR)\Syntax.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\thread.c

"$(INTDIR)\thread.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\Mzscheme\Src\Type.c

"$(INTDIR)\Type.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\foreign\libffi_msvc\types.c

"$(INTDIR)\types.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\mzscheme\src\vector.c

"$(INTDIR)\vector.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=..\..\foreign\libffi_msvc\win32.c

"$(INTDIR)\win32.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


!IF  "$(CFG)" == "libmzsch - Win32 Release"

"libmzgc - Win32 Release" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Release" 
   cd "..\libmzsch"

"libmzgc - Win32 ReleaseCLEAN" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Release" RECURSE=1 CLEAN 
   cd "..\libmzsch"

!ELSEIF  "$(CFG)" == "libmzsch - Win32 Debug"

"libmzgc - Win32 Debug" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Debug" 
   cd "..\libmzsch"

"libmzgc - Win32 DebugCLEAN" : 
   cd "..\libmzgc"
   $(MAKE) /$(MAKEFLAGS) /F .\libmzgc.mak CFG="libmzgc - Win32 Debug" RECURSE=1 CLEAN 
   cd "..\libmzsch"

!ENDIF 


!ENDIF 

