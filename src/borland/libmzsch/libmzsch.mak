# ---------------------------------------------------------------------------
!if !$d(BCB)
BCB = $(MAKEDIR)\..
!endif

# ---------------------------------------------------------------------------
# IDE SECTION
# ---------------------------------------------------------------------------
# The following section of the project makefile is managed by the BCB IDE.
# It is recommended to use the IDE to change any of the values in this
# section.
# ---------------------------------------------------------------------------

VERSION = BCB.05.03
# ---------------------------------------------------------------------------
PROJECT = ..\..\..\libmzsch.dll
OBJFILES = ..\..\Mzscheme\Src\Bignum.obj ..\..\Mzscheme\Src\Bool.obj \
    ..\..\mzscheme\src\builtin.obj ..\..\Mzscheme\Src\Char.obj \
    ..\..\Mzscheme\Src\Complex.obj ..\..\Mzscheme\Src\Dynext.obj \
    ..\..\Mzscheme\Src\Env.obj ..\..\Mzscheme\Src\Error.obj \
    ..\..\Mzscheme\Src\Eval.obj ..\..\Mzscheme\Src\File.obj \
    ..\..\Mzscheme\Src\Fun.obj ..\..\mzscheme\src\gmp\gmp.obj \
    ..\..\Mzscheme\Src\Hash.obj ..\..\MZSCHEME\SRC\image.obj \
    ..\..\Mzscheme\Src\List.obj ..\..\mzscheme\src\module.obj \
    ..\..\Mzscheme\Src\mzsj86.obj ..\..\mzscheme\src\network.obj \
    ..\..\mzscheme\src\numarith.obj ..\..\Mzscheme\Src\Number.obj \
    ..\..\mzscheme\src\numcomp.obj ..\..\mzscheme\src\numstr.obj \
    ..\..\Mzscheme\Src\Port.obj ..\..\mzscheme\src\portfun.obj \
    ..\..\Mzscheme\Src\Print.obj ..\..\Mzscheme\Src\Rational.obj \
    ..\..\Mzscheme\Src\Read.obj ..\..\Mzscheme\Src\Regexp.obj \
    ..\..\Mzscheme\Src\Salloc.obj ..\..\Mzscheme\Src\Sema.obj \
    ..\..\Mzscheme\Src\Setjmpup.obj ..\..\Mzscheme\Src\String.obj \
    ..\..\Mzscheme\Src\Struct.obj ..\..\mzscheme\src\stxobj.obj \
    ..\..\Mzscheme\Src\Symbol.obj ..\..\Mzscheme\Src\Syntax.obj \
    ..\..\mzscheme\src\thread.obj ..\..\Mzscheme\Src\Type.obj \
    ..\..\mzscheme\src\vector.obj
RESFILES = 
MAINSOURCE = libmzsch.bpf
RESDEPEN = $(RESFILES)
LIBFILES = ..\..\..\libmzgc.lib
IDLFILES = 
IDLGENFILES = 
LIBRARIES = 
PACKAGES = 
SPARELIBS = 
DEFFILE = 
# ---------------------------------------------------------------------------
PATHCPP = .;..\..\Mzscheme\Src;..\..\mzscheme\src\gmp
PATHASM = .;
PATHPAS = .;
PATHRC = .;
DEBUGLIBPATH = 
RELEASELIBPATH = 
USERDEFINES = NO_INLINE_KEYWORD=1;GC_DLL=1;_DEBUG
SYSDEFINES = NO_STRICT;_VIS_NOLIB;_NO_VCL
INCLUDEPATH = ..\..\mzscheme\src\gmp;..\..\Mzscheme\Src;..\..\mzscheme\include;$(BCB)\include
LIBPATH = ..\..\mzscheme\src\gmp;..\..\Mzscheme\Src;..\..\worksp;$(BCB)\lib\obj;$(BCB)\lib
WARNINGS= -w-8065 -w-par -w-8027 -w-8026 -w-aus
# ---------------------------------------------------------------------------
CFLAG1 = -WD -Od -Vx -Ve -X- -r- -a8 -b- -k -y -v -vi- -tWD -tWM -c
IDLCFLAGS = -I..\..\mzscheme\src\gmp -I..\..\Mzscheme\Src -I..\..\mzscheme\include \
    -I"d:\apps\borland\cbuilder5\include\." -src_suffix cpp \
    -DNO_INLINE_KEYWORD=1 -DGC_DLL=1 -D_DEBUG -boa
PFLAGS = -$Y+ -$W -$O- -v -JPHN -M
RFLAGS = 
AFLAGS = /mx /w2 /zi
LFLAGS = -D"" -aa -Tpd -x -Gn -Gi -v
# ---------------------------------------------------------------------------
ALLOBJ = c0d32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cw32mt.lib
# ---------------------------------------------------------------------------
!ifdef IDEOPTIONS

[Version Info]
IncludeVerInfo=0
AutoIncBuild=0
MajorVer=1
MinorVer=0
Release=0
Build=0
Debug=0
PreRelease=0
Special=0
Private=0
DLL=0

[Version Info Keys]
CompanyName=
FileDescription=
FileVersion=1.0.0.0
InternalName=
LegalCopyright=
LegalTrademarks=
OriginalFilename=
ProductName=
ProductVersion=1.0.0.0
Comments=

[Debugging]
DebugSourceDirs=$(BCB)\source\vcl

!endif





# ---------------------------------------------------------------------------
# MAKE SECTION
# ---------------------------------------------------------------------------
# This section of the project file is not used by the BCB IDE.  It is for
# the benefit of building from the command-line using the MAKE utility.
# ---------------------------------------------------------------------------

.autodepend
# ---------------------------------------------------------------------------
!if "$(USERDEFINES)" != ""
AUSERDEFINES = -d$(USERDEFINES:;= -d)
!else
AUSERDEFINES =
!endif

!if !$d(BCC32)
BCC32 = bcc32
!endif

!if !$d(CPP32)
CPP32 = cpp32
!endif

!if !$d(DCC32)
DCC32 = dcc32
!endif

!if !$d(TASM32)
TASM32 = tasm32
!endif

!if !$d(LINKER)
LINKER = ilink32
!endif

!if !$d(BRCC32)
BRCC32 = brcc32
!endif


# ---------------------------------------------------------------------------
!if $d(PATHCPP)
.PATH.CPP = $(PATHCPP)
.PATH.C   = $(PATHCPP)
!endif

!if $d(PATHPAS)
.PATH.PAS = $(PATHPAS)
!endif

!if $d(PATHASM)
.PATH.ASM = $(PATHASM)
!endif

!if $d(PATHRC)
.PATH.RC  = $(PATHRC)
!endif
# ---------------------------------------------------------------------------
$(PROJECT): $(IDLGENFILES) $(OBJFILES) $(RESDEPEN) $(DEFFILE)
    $(BCB)\BIN\$(LINKER) @&&!
    $(LFLAGS) -L$(LIBPATH) +
    $(ALLOBJ), +
    $(PROJECT),, +
    $(ALLLIB), +
    $(DEFFILE), +
    $(ALLRES)
!
# ---------------------------------------------------------------------------
.pas.hpp:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.pas.obj:
    $(BCB)\BIN\$(DCC32) $(PFLAGS) -U$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -O$(INCLUDEPATH) --BCB {$< }

.cpp.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.obj:
    $(BCB)\BIN\$(BCC32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) {$< }

.c.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.cpp.i:
    $(BCB)\BIN\$(CPP32) $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n. {$< }

.asm.obj:
    $(BCB)\BIN\$(TASM32) $(AFLAGS) -i$(INCLUDEPATH:;= -i) $(AUSERDEFINES) -d$(SYSDEFINES:;= -d) $<, $@

.rc.res:
    $(BCB)\BIN\$(BRCC32) $(RFLAGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -fo$@ $<
# ---------------------------------------------------------------------------




