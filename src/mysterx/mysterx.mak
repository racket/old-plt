# mysterx.mak

all : mysterx.dll

clean :
        -@erase comtypes.obj
        -@erase htmlevent.obj
        -@erase htmlutil.obj
        -@erase array.obj
        -@erase mysterx.obj
	-@erase mysterx.dll

HTMLHELP=C:\Program Files\HTML Help Workshop 
SHELL32=F:\SBN

CPP=cl.exe
CPP_FLAGS=/I"../mzscheme/include" /I"./myspage" /I"./mysc" /I"./myssink" /I"$(SHELL32)\Include" \
	/I"$(HTMLHELP)\include" /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /c 

.cxx.obj::
   $(CPP) $(CPP_FLAGS) $< 

MZC="D:\plt\mzc"
        
LINK32=$(MZC)
LINK32_LIBS= \
	kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib $(SHELL32)\LIB\shell32.lib ole32.lib oleaut32.lib \
	uuid.lib odbc32.lib \
        odbccp32.lib mapi32.lib "$(HTMLHELP)\lib\htmlhelp.lib" \
	mysc\mysc.lib 

LINK32_OBJS= \
        mysterx.obj array.obj comtypes.obj htmlevent.obj htmlutil.obj

mysterx.dll : $(DEF_FILE) $(LINK32_OBJS)
	$(LINK32) --ld mysterx.dll $(LINK32_OBJS) $(LINK32_LIBS)
	copy mysterx.dll ..\..\collects\mysterx\compiled\native
	
comtypes.obj : comtypes.cxx mysterx.h stdafx.h

htmlevent.obj : htmlevent.cxx mysterx.h stdafx.h

htmlutil.obj : htmlutil.cxx mysterx.h stdafx.h

array.obj : array.cxx mysterx.h stdafx.h

mysterx.obj : mysterx.cxx mysterx.h stdafx.h


