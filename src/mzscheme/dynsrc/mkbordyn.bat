bcc32 -I../include -omzdynb.obj -c mzdyn.c
mkdir ..\..\..\collects\mzscheme\lib
mkdir ..\..\..\collects\mzscheme\lib\win32
mkdir ..\..\..\collects\mzscheme\lib\win32\i386
mkdir ..\..\..\collects\mzscheme\lib\win32\i386\bcc
copy mzdynb.obj ..\..\..\collects\mzscheme\lib\win32\i386\bcc
copy mzdynb.def ..\..\..\collects\mzscheme\lib\win32\i386\bcc

