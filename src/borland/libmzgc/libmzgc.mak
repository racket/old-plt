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
PROJECT = ..\..\..\libmzgc.dll
OBJFILES = ..\..\Mzscheme\Gc\Allchblk.obj ..\..\Mzscheme\Gc\Alloc.obj \
    ..\..\Mzscheme\Gc\Blacklst.obj ..\..\Mzscheme\Gc\Dyn_load.obj \
    ..\..\Mzscheme\Gc\Finalize.obj ..\..\Mzscheme\Gc\Headers.obj \
    ..\..\Mzscheme\Gc\Mach_dep.obj ..\..\Mzscheme\Gc\Malloc.obj \
    ..\..\Mzscheme\Gc\Mark.obj ..\..\Mzscheme\Gc\Mark_rts.obj \
    ..\..\Mzscheme\Gc\Misc.obj ..\..\Mzscheme\Gc\New_hblk.obj \
    ..\..\Mzscheme\Gc\Obj_map.obj ..\..\Mzscheme\Gc\Os_dep.obj \
    ..\..\Mzscheme\Gc\Reclaim.obj ..\..\Mzscheme\Gc\Stubborn.obj \
    ..\..\mzscheme\gc\mallocx.obj
RESFILES = 
MAINSOURCE = libmzgc.bpf
RESDEPEN = $(RESFILES)
LIBFILES = 
IDLFILES = 
IDLGENFILES = 
LIBRARIES = 
PACKAGES = 
SPARELIBS = 
DEFFILE = 
# ---------------------------------------------------------------------------
PATHCPP = .;..\..\Mzscheme\Gc
PATHASM = .;
PATHPAS = .;
PATHRC = .;
DEBUGLIBPATH = .
RELEASELIBPATH = .
USERDEFINES = GC_DLL;GC_BUILD;_DEBUG
SYSDEFINES = NO_STRICT;_VIS_NOLIB;_NO_VCL
INCLUDEPATH = ..\gc;..\..\Mzscheme\Gc;..\..\mzscheme\gc\include;..\..\mzscheme\include;$(BCB)\include
LIBPATH = ..\..\Mzscheme\Gc;..\gc;..\..\worksp;$(BCB)\lib\obj;$(BCB)\lib
WARNINGS= -w-8065 -w-par -w-8027 -w-8026 -w-aus
# ---------------------------------------------------------------------------
CFLAG1 = -WD -Od -Vx -Ve -X- -r- -a8 -b- -k -y -v -vi- -tWD -tWM -c
IDLCFLAGS = -I..\gc -I..\..\Mzscheme\Gc -I..\..\mzscheme\gc\include \
    -I..\..\mzscheme\include -I$(BCB)\include -src_suffix cpp -DGC_DLL \
    -DGC_BUILD -D_DEBUG -boa
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




