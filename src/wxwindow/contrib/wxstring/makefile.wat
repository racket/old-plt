WXDIR = ..\.. 

!include $(WXDIR)\src\makewat.env

LIBTARGET = wxstring.lib

OBJECTS = wxstrgnu.obj wxregex.obj

all: $(OBJECTS) $(LIBTARGET)

$(LIBTARGET): $(OBJECTS)
	wlib /b /c /n /P=256 $(LIBTARGET) $(OBJECTS)

clean:   .SYMBOLIC
    -erase *.obj *.bak *.err *.pch *.lib *.lbc

