# Microsoft Developer Studio Project File - Name="libmzsch" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=LIBMZSCH - WIN32 RELEASE
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libmzsch.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmzsch.mak" CFG="LIBMZSCH - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmzsch - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libmzsch - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libmzsch - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMZSCH_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I ".." /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /I "..\..\mzscheme\src" /I "..\..\foreign\libffi_msvc" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "__STDC__" /D "_USRDLL" /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 ../libmzgc/Release/libmzgcxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /dll /debug /machine:I386 /out:"../../../libmzschxxxxxxx.dll"
# SUBTRACT LINK32 /pdb:none /nodefaultlib

!ELSEIF  "$(CFG)" == "libmzsch - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "libmzsch___Win32_Debug"
# PROP BASE Intermediate_Dir "libmzsch___Win32_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /Zi /O2 /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "__STDC__" /D "_USRDLL" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Zi /Od /I ".." /I "..\..\mzscheme\include" /I "..\..\mzscheme\gc" /I "..\..\mzscheme\src" /I "..\..\foreign\libffi_msvc" /D "WIN32" /D "DEBUG" /D "_WINDOWS" /D "__STDC__" /D "_USRDLL" /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 ../libmzgc/Release/libmzgc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /dll /debug /machine:I386 /out:"../../../libmzsch.dll"
# ADD LINK32 ../libmzgc/Debug/libmzgcxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib /nologo /dll /incremental:yes /debug /machine:I386 /nodefaultlib:"kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib" /out:"../../../libmzschxxxxxxx.dll"
# SUBTRACT LINK32 /pdb:none /nodefaultlib

!ENDIF 

# Begin Target

# Name "libmzsch - Win32 Release"
# Name "libmzsch - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Bignum.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Bool.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\builtin.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Char.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Complex.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Dynext.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Env.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Error.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Eval.c
# End Source File
# Begin Source File

SOURCE=..\..\foreign\libffi_msvc\ffi.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\File.c
# End Source File
# Begin Source File

SOURCE=..\..\foreign\foreign.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Fun.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\gmp\gmp.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Hash.c
# End Source File
# Begin Source File

SOURCE=..\..\MZSCHEME\SRC\image.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\List.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\module.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\mzsj86.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\network.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\numarith.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Number.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\numcomp.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\numstr.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Port.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\portfun.c
# End Source File
# Begin Source File

SOURCE=..\..\foreign\libffi_msvc\prep_cif.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Print.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Rational.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Read.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Regexp.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Salloc.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Sema.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Setjmpup.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\String.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Struct.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\stxobj.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Symbol.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Syntax.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\thread.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Src\Type.c
# End Source File
# Begin Source File

SOURCE=..\..\foreign\libffi_msvc\types.c
# End Source File
# Begin Source File

SOURCE=..\mzscheme\uniplt.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\src\vector.c
# End Source File
# Begin Source File

SOURCE=..\..\foreign\libffi_msvc\win32.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
