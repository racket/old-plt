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
PROJECT = ..\..\Monolithic.exe
OBJFILES = ..\mzscheme\main.obj ..\mzscheme\gc\allchblk.obj ..\Mzscheme\Gc\Alloc.obj \
    ..\Mzscheme\Gc\Blacklst.obj ..\Mzscheme\Gc\Dyn_load.obj \
    ..\Mzscheme\Gc\Finalize.obj ..\Mzscheme\Gc\Headers.obj \
    ..\Mzscheme\Gc\Mach_dep.obj ..\Mzscheme\Gc\Malloc.obj \
    ..\Mzscheme\Gc\Mark.obj ..\Mzscheme\Gc\Mark_rts.obj \
    ..\Mzscheme\Gc\Misc.obj ..\Mzscheme\Gc\New_hblk.obj \
    ..\Mzscheme\Gc\Obj_map.obj ..\Mzscheme\Gc\Os_dep.obj \
    ..\Mzscheme\Gc\Reclaim.obj ..\Mzscheme\Gc\Stubborn.obj \
    ..\mzscheme\gc\mallocx.obj ..\Mzscheme\Src\Bignum.obj \
    ..\Mzscheme\Src\Bool.obj ..\mzscheme\src\builtin.obj \
    ..\Mzscheme\Src\Char.obj ..\Mzscheme\Src\Complex.obj \
    ..\Mzscheme\Src\Dynext.obj ..\Mzscheme\Src\Env.obj \
    ..\Mzscheme\Src\Error.obj ..\Mzscheme\Src\Eval.obj \
    ..\Mzscheme\Src\File.obj ..\Mzscheme\Src\Fun.obj \
    ..\mzscheme\src\gmp\gmp.obj ..\Mzscheme\Src\Hash.obj \
    ..\MZSCHEME\SRC\image.obj ..\Mzscheme\Src\List.obj \
    ..\mzscheme\src\module.obj ..\Mzscheme\Src\mzsj86.obj \
    ..\mzscheme\src\network.obj ..\mzscheme\src\numarith.obj \
    ..\Mzscheme\Src\Number.obj ..\mzscheme\src\numcomp.obj \
    ..\mzscheme\src\numstr.obj ..\Mzscheme\Src\Port.obj \
    ..\mzscheme\src\portfun.obj ..\Mzscheme\Src\Print.obj \
    ..\Mzscheme\Src\Rational.obj ..\Mzscheme\Src\Read.obj \
    ..\Mzscheme\Src\Regexp.obj ..\Mzscheme\Src\Salloc.obj \
    ..\Mzscheme\Src\Sema.obj ..\Mzscheme\Src\Setjmpup.obj \
    ..\Mzscheme\Src\String.obj ..\Mzscheme\Src\Struct.obj \
    ..\mzscheme\src\stxobj.obj ..\Mzscheme\Src\Symbol.obj \
    ..\Mzscheme\Src\Syntax.obj ..\mzscheme\src\thread.obj \
    ..\Mzscheme\Src\Type.obj ..\mzscheme\src\vector.obj
RESFILES = 
MAINSOURCE = Monolithic.bpf
RESDEPEN = $(RESFILES)
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = 
PACKAGES = 
SPARELIBS = 
DEFFILE = 
# ---------------------------------------------------------------------------
PATHCPP = .;..\mzscheme;..\mzscheme\gc;..\Mzscheme\Src;..\mzscheme\src\gmp
PATHASM = .;
PATHPAS = .;
PATHRC = .;
DEBUGLIBPATH = $(BCB)\lib\debug
RELEASELIBPATH = $(BCB)\lib\release
USERDEFINES = SCHEME_EMBEDDED_NO_DLL;NO_INLINE_KEYWORD=1;_DEBUG
SYSDEFINES = NO_STRICT;_VIS_NOLIB;_NO_VCL;_RTLDLL
INCLUDEPATH = ..\mzscheme\include;..\mzscheme\src\gmp;..\mzscheme\src;..\mzscheme\gc;..\mzscheme\gc\include;..\mzscheme;$(BCB)\include
LIBPATH = $(BCB)\lib\debug;..\mzscheme\src\gmp;..\mzscheme\src;..\mzscheme\gc;..\mzscheme;d:\apps\borland\CBuilder5\;$(BCB)\lib\obj;$(BCB)\lib
WARNINGS= -w-8065 -w-par -w-8027 -w-8026 -w-aus
# ---------------------------------------------------------------------------
CFLAG1 = -Od -Vx -Ve -X- -r- -a8 -b- -k -y -v -vi- -tWC -tWM -c
IDLCFLAGS = -I..\mzscheme\include -I..\mzscheme\src\gmp -I..\mzscheme\src \
    -I..\mzscheme\gc -I..\mzscheme\gc\include -I..\mzscheme -I$(BCB)\include \
    -src_suffix cpp -DSCHEME_EMBEDDED_NO_DLL -DNO_INLINE_KEYWORD=1 -D_DEBUG -boa
PFLAGS = -$Y+ -$W -$O- -v -JPHN -M
RFLAGS = 
AFLAGS = /mx /w2 /zi
LFLAGS = -D"" -ap -Tpe -x -Gn -v
# ---------------------------------------------------------------------------
ALLOBJ = c0x32.obj $(OBJFILES)
ALLRES = $(RESFILES)
ALLLIB = $(LIBFILES) $(LIBRARIES) import32.lib cw32mti.lib
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
..\mzscheme\src\mzsj86.obj: ..\mzscheme\src\mzsj86.c
	$(BCB)\BIN\$(BCC32) -S $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) ..\mzscheme\src\mzsj86.c
	ml /Zm /c /Cx /Dmodel=\; ..\mzscheme\src\mzsj86.asm
..\mzscheme\gc\mach_dep.obj: ..\mzscheme\gc\mach_dep.c
	$(BCB)\BIN\$(BCC32) -S $(CFLAG1) $(WARNINGS) -I$(INCLUDEPATH) -D$(USERDEFINES);$(SYSDEFINES) -n$(@D) ..\mzscheme\gc\mach_dep.c
	ml /Zm /c /Cx /Dmodel=\; ..\mzscheme\gc\mach_dep.asm
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
