# Microsoft Developer Studio Project File - Name="libmzgc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=LIBMZGC - WIN32 RELEASE
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libmzgc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmzgc.mak" CFG="LIBMZGC - WIN32 RELEASE"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmzgc - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libmzgc - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libmzgc - Win32 Release"

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
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "LIBMZGC_EXPORTS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /Zi /O2 /I "../../mzscheme/gc/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "GC_BUILD" /D "SILENT" /D "OLD_BLOCK_ALLOC" /D "LARGE_CONFIG" /D "ATOMIC_UNCOLLECTABLE" /D INITIAL_MARK_STACK_SIZE=8192 /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /machine:I386
# ADD LINK32 unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"../../../libmzgcxxxxxxx.dll"
# SUBTRACT LINK32 /pdb:none /nodefaultlib /force

!ELSEIF  "$(CFG)" == "libmzgc - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "libmzgc___Win32_Debug"
# PROP BASE Intermediate_Dir "libmzgc___Win32_Debug"
# PROP BASE Ignore_Export_Lib 0
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /Zi /O2 /I "../../mzscheme/gc/include" /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "GC_BUILD" /D "USE_MSVC_MD_LIBRARY" /D "MD_LIB_MAIN" /D "SILENT" /D "OLD_BLOCK_ALLOC" /D "LARGE_CONFIG" /D "ATOMIC_UNCOLLECTABLE" /D INITIAL_MARK_STACK_SIZE=8192 /YX /FD /c
# ADD CPP /nologo /MTd /W3 /GX /Zi /Od /I "../../mzscheme/gc/include" /D "WIN32" /D "DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /D "GC_BUILD" /D "MD_LIB_MAIN" /D "SILENT" /D "OLD_BLOCK_ALLOC" /D "LARGE_CONFIG" /D "ATOMIC_UNCOLLECTABLE" /D INITIAL_MARK_STACK_SIZE=8192 /D "GC_DLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /debug /machine:I386 /out:"../../../libmzgc.dll"
# SUBTRACT BASE LINK32 /pdb:none /force
# ADD LINK32 unicows.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /dll /incremental:yes /debug /machine:I386 /nodefaultlib:"kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib" /out:"../../../libmzgcxxxxxxx.dll"
# SUBTRACT LINK32 /pdb:none /nodefaultlib /force

!ENDIF 

# Begin Target

# Name "libmzgc - Win32 Release"
# Name "libmzgc - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Allchblk.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Alloc.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Blacklst.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Dyn_load.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Finalize.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Headers.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mach_dep.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Malloc.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mallocx.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mark.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Mark_rts.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Misc.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\New_hblk.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Obj_map.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Os_dep.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Reclaim.c
# End Source File
# Begin Source File

SOURCE=..\..\Mzscheme\Gc\Stubborn.c
# End Source File
# Begin Source File

SOURCE=..\mzscheme\uniplt.c
# End Source File
# Begin Source File

SOURCE=..\..\mzscheme\gc\win32_threads.c
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
