# Microsoft Developer Studio Project File - Name="libmred" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=LIBMRED - WIN32 RELEASE
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libmred.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmred.mak" CFG="LIBMRED - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmred - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libmred - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libmred - Win32 Release"

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
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMRED_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMRED_EXPORTS" /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 ../libmzsch/release/libmzschxxxxxxx.lib ../libmzgc/release/libmzgcxxxxxxx.lib ../wxs/release/wxs.lib ../wxme/release/wxme.lib ../wxutils/release/wxutils.lib ../jpeg/release/jpeg.lib ../png/release/png.lib ../zlib/release/zlib.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib glu32.lib opengl32.lib winmm.lib comctl32.lib /nologo /dll /debug /machine:I386 /out:"../../../libmredxxxxxxx.dll"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "libmred - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "libmred___Win32_Debug"
# PROP BASE Intermediate_Dir "libmred___Win32_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMRED_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /GX /Zi /Od /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /D "WIN32" /D "DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMRED_EXPORTS" /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 ../libmzsch/release/libmzsch.lib ../libmzgc/release/libmzgc.lib ../wxs/release/wxs.lib ../wxme/release/wxme.lib ../wxutils/release/wxutils.lib ../jpeg/release/jpeg.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib glu32.lib opengl32.lib winmm.lib /nologo /dll /debug /machine:I386 /out:"../../../libmred.dll"
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 ../libmzsch/debug/libmzschxxxxxxx.lib ../libmzgc/debug/libmzgcxxxxxxx.lib ../wxs/debug/wxs.lib ../wxme/debug/wxme.lib ../wxutils/debug/wxutils.lib ../jpeg/debug/jpeg.lib ../png/debug/png.lib ../zlib/debug/zlib.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib glu32.lib opengl32.lib winmm.lib comctl32.lib /nologo /dll /incremental:yes /debug /machine:I386 /out:"../../../libmredxxxxxxx.dll"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "libmred - Win32 Release"
# Name "libmred - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\mred\MRED.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\MREDMSW.cxx
# End Source File
# Begin Source File

SOURCE=..\mzscheme\uniplt.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
