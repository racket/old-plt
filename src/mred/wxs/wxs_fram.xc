
@INCLUDE prefix.xci

#include "wx_frame.h"
#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_xt
#define NOT_XT 0
#define HAS_GET_MENU_BAR 1
#define GET_THE_MENU_BAR(f) (f)->GetMenuBar()
#else
#define HAS_GET_MENU_BAR 0
#define NOT_XT 1
#define GET_THE_MENU_BAR(f) (f)->wx_menu_bar
#endif

#ifdef wx_motif
#define wxALLOW_AUTO_RESIZE wxPUSH_PIN
#else
#define wxALLOW_AUTO_RESIZE 0
#endif

#define NO_GET_MENU_BAR !HAS_GET_MENU_BAR

@MACRO CHECKHASMENU[log] = if (<log>GET_THE_MENU_BAR(((wxFrame *)((Scheme_Class_Object *)obj)->primdata))) return scheme_void;

@BEGINSYMBOLS frameStyle
@SYM "caption" : wxCAPTION
@SYM "iconize" : wxICONIZE
@SYM "minimize" : wxMINIMIZE
@SYM "maximize" : wxMAXIMIZE
@SYM "sdi" : wxSDI
@SYM "mdi-parent" : wxMDI_PARENT
@SYM "mdi-child" : wxMDI_CHILD
@SYM "thick-frame" : wxTHICK_FRAME
@SYM "system-menu" : wxSYSTEM_MENU
@SYM "minimize-box" : wxMINIMIZE_BOX
@SYM "maximize-box" : wxMAXIMIZE_BOX
@SYM "resize-border" : wxRESIZE_BORDER
@ENDSYMBOLS

@BEGINSYMBOLS orientation
@SYM "both" : wxBOTH
@SYM "horizontal" : wxHORIZONTAL
@SYM "vertical" :  wxVERTICAL
@ENDSYMBOLS


@CLASSBASE wxFrame "wx:frame":"wx:window"

@CLASSID wxTYPE_FRAME

@CREATOR (wxFrame^, string, int = -1, int = -1, int = -1, int = -1, SYM[frameStyle] = wxDEFAULT_FRAME, string = "frame") : : /NOZERO[4]|NOZERO[5]/

@MACRO CHECKICONOK[p] = if (x<p> && !x<p>->Ok()) return scheme_void;

@ "get-title" : string GetTitle();
@ "set-title" : void SetTitle(string);
@ "iconize" : void Iconize(bool);
@ "set-icon" : void SetIcon(wxIcon!); : : /CHECKICONOK[0]
@ "set-menu-bar" : void SetMenuBar(wxMenuBar!) : : /CHECKHASMENU[ ]
@IVAR r "menu-bar" : wxMenuBar^ wx_menu_bar ## NO_GET_MENU_BAR
@ "get-menu-bar" : wxMenuBar^ GetMenuBar() ## HAS_GET_MENU_BAR
@ "set-tool-bar" : void SetToolBar(wxToolBar^) ## USE_TOOLBAR 
@ "get-tool-bar" : wxToolBar^ GetToolBar() ## USE_TOOLBAR
@ "set-status-text" : void SetStatusText(string)
@ "iconized?" : bool Iconized();
@ "status-line-exists?" : bool StatusLineExists();
@ "maximize" : void Maximize(bool)
@ "load-accelerators" : void LoadAccelerators(string);
@ "create-status-line" : void CreateStatusLine(int = 1, string = "status_line")

@SETMARK f = d
@INCLUDE wxs_fram.xci

@ v "command" : void Command(int);
@ v "on-menu-command" : void OnMenuCommand(int)
@ v "on-menu-select" : void OnMenuSelect(int) : : CHECKHASMENU[!]

@SETMARK w = d
@INCLUDE wxs_win.xci

@CONSTANT "wx:const-default-frame" : SYM[frameStyle] wxDEFAULT_FRAME

@END
