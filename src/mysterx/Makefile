# Makefile for mysterx.dll, myspage.dll, myssink.dll

# See README for compilation instructions

MZC=..\..\mzc
HTMLHELP="C:\Program Files\HTML Help Workshop"
SHELL32="C:\Program Files\Microsoft Visual Studio\VC98"
NEWMSHTML=1

all : 
	cd myspage
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 /f myspage.mak 
	cd ../mysc
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 /f mysc.mak
	cd ../myssink
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 /f myssink.mak
	cd ..
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 NEWMSHTML=$(NEWMSHTML) \
	  /f mysterx.mak

clean :
	cd myspage
	nmake clean /f myspage.mak
	cd ../mysc
	nmake clean /f mysc.mak
	cd ../myssink
	nmake clean /f myssink.mak
	cd ..
	nmake clean /f mysterx.mak
