cl /MT -O2 -I../include /c mzdyn.c
lib -def:mzdyn.def -out:mzdyn.lib
mkdir ..\..\..\lib
mkdir ..\..\..\lib\msvc
copy mzdyn.exp ..\..\..\lib\msvc
copy mzdyn.obj ..\..\..\lib\msvc
copy ..\..\worksp\libmzsch\release\libmzschxxxxxxx.lib ..\..\..\lib\msvc
copy ..\..\worksp\libmzgc\release\libmzgcxxxxxxx.lib ..\..\..\lib\msvc

