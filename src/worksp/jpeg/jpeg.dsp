# Microsoft Developer Studio Project File - Name="jpeg" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=jpeg - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "jpeg.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "jpeg.mak" CFG="jpeg - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "jpeg - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "jpeg - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "jpeg - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /O2 /I "../jpeg" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ELSEIF  "$(CFG)" == "jpeg - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "." /I "../jpeg" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo

!ENDIF 

# Begin Target

# Name "jpeg - Win32 Release"
# Name "jpeg - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcapimin.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcapistd.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jccoefct.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jccolor.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcdctmgr.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jchuff.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcinit.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcmainct.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcmarker.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcmaster.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcomapi.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcparam.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcphuff.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcprepct.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jcsample.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jctrans.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdapimin.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdapistd.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdatadst.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdatasrc.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdcoefct.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdcolor.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jddctmgr.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdhuff.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdinput.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdmainct.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdmarker.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdmaster.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdmerge.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdphuff.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdpostct.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdsample.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jdtrans.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jerror.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jfdctflt.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jfdctfst.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jfdctint.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jidctflt.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jidctfst.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jidctint.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jidctred.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jmemmgr.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jmemnobs.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jpegtran.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jquant1.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jquant2.c
# End Source File
# Begin Source File

SOURCE=..\..\wxcommon\jpeg\jutils.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
