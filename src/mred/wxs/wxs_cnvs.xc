
@INCLUDE prefix.xci

#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_frame.h"

@INCLUDE wxs.xci

@HEADER

#ifdef wx_xt
# include "wx_types.h"
# define CHECK_FOR_PANEL(x) !wxSubType((x)->__type, wxTYPE_CANVAS)
#else
# define CHECK_FOR_PANEL(x) 0
#endif

static void FillZero(int *a, int *b) {
  *a = *b = 0;
}

@BEGINSYMBOLS canvasStyle
@SYM "border" : wxBORDER
@SYM "vscroll" : wxVSCROLL
@SYM "hscroll" : wxHSCROLL
@ENDSYMBOLS

/* Handle cases in Xt that are a problem because a wxPanel isn't really a wxCanvas */
@MACRO PANELREDIRECT[x] = if (CHECK_FOR_PANEL((wxObject *)((Scheme_Class_Object *)obj)->primdata)) { <x>; }

@INCLUDE wxs_drws.xci

@CLASSBASE wxCanvas "wx:canvas":"wx:window"

@CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1,SYM[canvasStyle]=0,string="canvas") : : /NOZERO[3]|NOZERO[4]/ <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1,SYM[canvasStyle]=0,string="canvas") : : /NOZERO[3]|NOZERO[4]/ <> panel

@ "allow-double-click" : void AllowDoubleClick(bool);

@INCLUDE wxs_char.xci

@SETMARK c = d
@INCLUDE wxs_cnvs.xci

@ "popup-menu" : bool PopupMenu(wxMenu!, float, float);

@ "get-dc" : wxCanvasDC! GetDC();

// @ "get-scroll-units" : void GetScrollUnitsPerPage(int*,int*); : : / PANELREDIRECT[ FillZero(x0,x1); return scheme_void]
@ "get-virtual-size" : void GetVirtualSize(int*,int*); : : / PANELREDIRECT[FillZero(x0,x1); return scheme_void]
@ "set-scrollbars" : void SetScrollbars(int,int,int,int,int,int,int=0,int=0,bool=TRUE);  : : / PANELREDIRECT[return scheme_void]
@ "view-start" : void ViewStart(int*,int*); : : / PANELREDIRECT[FillZero(x0,x1); return scheme_void]
@ "warp-pointer" : void WarpPointer(int,int);  : : / PANELREDIRECT[return scheme_void]

@ "scroll" : void Scroll(int,int);
@ "get-scroll-pos" : int GetScrollPos(int);
@ "get-scroll-range" : int GetScrollRange(int);
@ "get-scroll-page" : int GetScrollPage(int);

@ v "on-scroll" : void OnScroll(wxCommandEvent%); : : / PANELREDIRECT[return scheme_void]

@SETMARK w = d
@INCLUDE wxs_win.xci

#define DrawsForCanvas
@INCLUDE wxs_draw.xci

@END
