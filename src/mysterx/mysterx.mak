# mysterx.mak

all : mysterx.dll

clean :
        -@erase comtypes.obj
        -@erase event.obj
        -@erase htmlutil.obj
        -@erase mysterx.obj
	-@erase mysterx.dll

CPP=cl.exe
CPP_FLAGS=/I"$(PLTHOME)/src/mzscheme/include" /I"./myspage" /ML /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /c 

.cxx.obj::
   $(CPP) $(CPP_FLAGS) $< 

MZC="C:\Program Files\PLT\mzc"
        
LINK32=$(MZC)
LINK32_FLAGS=
LINK32_LIBS= \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
        odbccp32.lib mapi32.lib
LINK32_OBJS= \
        mysterx.obj comtypes.obj event.obj htmlutil.obj

mysterx.dll : $(DEF_FILE) $(LINK32_OBJS)
	$(LINK32) $(LINK32_FLAGS) --ld mysterx.dll $(LINK32_OBJS) $(LINK32_LIBS)

comtypes.obj : comtypes.cxx mysterx.h

event.obj : event.cxx mysterx.h

htmlutil.obj : htmlutil.cxx mysterx.h

mysterx.obj : mysterx.cxx mysterx.h

