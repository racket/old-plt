# Makefile for mysterx.dll and myspage.dll

# If you get errors about a missing file atlbase.h,
# check the value of the environment variable INCLUDE.
# The subdirectory ATL beneath the Visual C++ directory
# needs to be in the list of INCLUDE directories.
# For example, if Visual Studio 6 is installed at the root of
# the C: drive, that directory would be
#   C:\Microsoft Visual Studio\VC98\ATL

all : 
	cd myspage
        nmake /f myspage.mak
	cd ..
        nmake /f mysterx.mak

