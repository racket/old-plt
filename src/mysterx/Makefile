# Makefile for mysterx.dll, myspage.dll, myssink.dll

# See README for compilation instructions

MZC="..\..\mzc"
HTMLHELP="C:\Program Files\HTML Help Workshop"
SHELL32="F:\SBN"
REGSVR32="C:\WINNT\System32\REGSVR32"
NEWMSHTML=0

all : 
	@ -255 mkdir ..\..\collects\mysterx\dlls
	cd myspage
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) /f myspage.mak 
	cd ..\mysc
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) /f mysc.mak
	cd ..\myssink
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) /f myssink.mak
	cd ..
	nmake MZC=$(MZC) HTMLHELP=$(HTMLHELP) SHELL32=$(SHELL32) \
	 REGSVR32=$(REGSVR32) NEWMSHTML=$(NEWMSHTML) /f mysterx.mak

clean :
	cd myspage
	nmake clean /f myspage.mak
	cd ..\mysc
	nmake clean /f mysc.mak
	cd ..\myssink
	nmake clean /f myssink.mak
	cd ..
	nmake clean /f mysterx.mak
