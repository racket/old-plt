# Microsoft Developer Studio Project File - Name="wxme" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=WXME - WIN32 RELEASE
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "wxme.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "wxme.mak" CFG="WXME - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "wxme - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "wxme - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "wxme - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxcommon\jpeg" /I "..\jpeg" /I "..\..\wxcommon\zlib" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "wxme - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "wxme___Win32_Debug"
# PROP BASE Intermediate_Dir "wxme___Win32_Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxcommon\jpeg" /I "..\jpeg" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /GX /Zi /Od /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxcommon\jpeg" /I "..\jpeg" /I "..\..\wxcommon\zlib" /D "DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /YX /FD /c
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

# Name "wxme - Win32 Release"
# Name "wxme - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_CGREC.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_KEYM.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MBUF.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MEDAD.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MEDIA.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MEDIO.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MLINE.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MPBRD.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MPRIV.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_MSNIP.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_SNIP.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\WX_STYLE.cxx
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\wxGC.cxx
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\wxJPEG.cxx
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\utils\xcglue.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
