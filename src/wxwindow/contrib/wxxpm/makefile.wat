WXDIR   = ..\..

all: .SYMBOLIC
    cd libxpm.34b\lib
    wmake -f makefile.wat

clean: .SYMBOLIC
    cd libxpm.34b\lib
    wmake -f makefile.wat clean
