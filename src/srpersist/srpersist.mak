# srpersist.mak - Windows makefile for SrPersist
 
# see README for information on building SrPersist

# may have to change for non-MS driver manager
ODBCVER=0x0351

# will have to change if non-MS driver manager
ODBC_LIBS=odbc32.lib odbccp32.lib 

# change for your installation
MZC="C:\Program Files\PLT\mzc"

all : srpmain.dll

clean :
	-@erase srpersist.obj
	-@erase srptypes.obj
	-@erase srpbuffer.obj
	-@erase srpersist.dll

CPP=cl.exe
CPP_FLAGS=/I"../../collects/mzscheme/include" /ML /W3 /GX /O2 /D ODBCVER=$(ODBCVER) /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /c

.cxx.obj::
   $(CPP) $(CPP_FLAGS) $< 

LINK32=$(MZC)
LINK32_FLAGS=
LINK32_LIBS= \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib \
	comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib \
	uuid.lib $(ODBC_LIBS)
LINK32_OBJS= \
	srpersist.obj srptypes.obj srpbuffer.obj

all : srpmain.dll

srpmain.dll : $(DEF_FILE) $(LINK32_OBJS)
	$(LINK32) $(LINK32_FLAGS) --ld srpmain.dll $(LINK32_OBJS) $(LINK32_LIBS)

install : srpmain.dll
	@ -255 mkdir ..\..\collects\srpersist\lib
	@ -255 mkdir ..\..\collects\srpersist\lib\win32
	@ -255 mkdir ..\..\collects\srpersist\lib\win32\i386
	copy srpmain.dll ..\..\collects\srpersist\lib\win32\i386

srpersist.obj : srpersist.cxx srpersist.h srptypes.h srpprims.tbl srpconsts.tbl srpinfo.tbl srpstructs.tbl srpexns.tbl

srptypes.obj : srptypes.cxx srpersist.h srptypes.h

srpbuffer.obj : srpbuffer.cxx srpersist.h srpbuffer.h

