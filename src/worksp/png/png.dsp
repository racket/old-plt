# Microsoft Developer Studio Project File - Name="png" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=png - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "png.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "png.mak" CFG="png - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "png - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "png - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "png - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../../wxcommon/zlib" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /D "GC_DLL" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "png - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "png___Win32_Debug"
# PROP BASE Intermediate_Dir "png___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /I "../png" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /GX /Od /I "../../wxcommon/zlib" /D "WIN32" /D "DEBUG" /D "_MBCS" /D "_LIB" /D "GC_DLL" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "png - Win32 Release"
# Name "png - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\wxcommon\libpng\png.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngerror.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pnggccrd.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngget.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngmem.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngpread.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngread.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngrio.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngrtran.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngrutil.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngset.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngtest.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngtrans.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngvcrd.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngwio.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngwrite.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngwtran.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\libpng\pngwutil.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
