cl -O2 -I../include /c mzdyn.c
lib -def:mzdyn.def -out:mzdyn.lib
copy mzdyn.exp ..\..\..\collects\mzscheme\lib\win32\i386\msvc\
copy mzdyn.obj ..\..\..\collects\mzscheme\lib\win32\i386\msvc\

