# Microsoft Developer Studio Project File - Name="mred" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=mred - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "mred.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "mred.mak" CFG="mred - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "mred - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "mred - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 1
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "mred - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Release"
# PROP BASE Intermediate_Dir ".\Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\..\..\plt"
# PROP Intermediate_Dir ".\Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /MT /W3 /Zi /O2 /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 ..\libmred\Release\libmredxxxxxxx.lib ..\libmzsch\Release\libmzschxxxxxxx.lib ..\libmzgc\Release\libmzgcxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib glu32.lib opengl32.lib comctl32.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"..\..\..\MrEd.exe"
# SUBTRACT LINK32 /incremental:yes

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "mred___Win32_Debug"
# PROP BASE Intermediate_Dir "mred___Win32_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /Zi /O2 /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "NDEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /Zi /Od /I ".." /I "..\..\mzscheme\gc" /I "..\..\wxwindow\include\base" /I "..\..\wxwindow\include\msw" /I "..\..\mzscheme\include" /I "..\..\mred\wxme" /I "..\..\mzscheme\utils" /I "..\..\wxwindow\contrib\fafa" /D "DEBUG" /D "__STDC__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "NDEBUG"
# ADD RSC /l 0x409 /i "..\..\wxwindow\include\msw" /i "..\..\wxwindow\contrib\fafa" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 ..\libmred\Release\libmred.lib ..\libmzsch\Release\libmzsch.lib ..\libmzgc\Release\libmzgc.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib glu32.lib opengl32.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"..\..\..\MrEd.exe"
# SUBTRACT BASE LINK32 /incremental:yes
# ADD LINK32 ..\libmred\Debug\libmredxxxxxxx.lib ..\libmzsch\Debug\libmzschxxxxxxx.lib ..\libmzgc\Debug\libmzgcxxxxxxx.lib unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib glu32.lib opengl32.lib comctl32.lib /nologo /subsystem:windows /incremental:yes /debug /machine:I386 /nodefaultlib:"libcd.lib" /out:"..\..\..\MrEd.exe"

!ENDIF 

# Begin Target

# Name "mred - Win32 Release"
# Name "mred - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\Mred.rc

!IF  "$(CFG)" == "mred - Win32 Release"

!ELSEIF  "$(CFG)" == "mred - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\mred\mrmain.cxx
# End Source File
# Begin Source File

SOURCE=..\mzscheme\uniplt.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=..\..\mred\wxme\edjr.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\mred.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\Wx_cgrec.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\Wx_keym.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\Wx_medad.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\wxme\wx_media.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\wxme\wx_medio.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\wx_mline.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\wx_mpriv.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\Wxme\Wx_snip.h
# End Source File
# Begin Source File

SOURCE=..\..\mred\wxme\wx_style.h
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\utils\xcglue.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=.\BULLSEYE.CUR
# End Source File
# Begin Source File

SOURCE=.\child.ico
# End Source File
# Begin Source File

SOURCE=.\fafa.ico
# End Source File
# Begin Source File

SOURCE=.\HAND.CUR
# End Source File
# Begin Source File

SOURCE=.\mdi.ico
# End Source File
# Begin Source File

SOURCE=.\mred.ico
# End Source File
# Begin Source File

SOURCE=.\std.ico
# End Source File
# Begin Source File

SOURCE=.\WATCH1.CUR
# End Source File
# End Group
# End Target
# End Project
