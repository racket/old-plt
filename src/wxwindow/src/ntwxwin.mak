#
# File:		ntwxwin.env
# Author:	Ulrich Leodolter
# Created:	Wed May 17 08:36:42 1995
# Updated:
#
#	Windows NT specific, using SDK
#
!include <ntwin32.mak>
OS_SPEC=/nologo -DWINNT -DMS_WINDOWS -Dwx_msw
ALLFLAGS=$(OS_SPEC) $(cflags) $(cvars) $(cdebug)
CPPFLAGS=$(ALLFLAGS) $(INC)

# Change this to your WXWIN directory
WXDIR=i:\nt\wx162

WXLIB=$(WXDIR)\lib
WXSRC=$(WXDIR)\src\msw
WXINC=$(WXDIR)\include\msw
WXBASESRC=$(WXDIR)\src\base
WXBASEINC=$(WXDIR)\include\base

LIBWX=$(WXLIB)\wx.lib


