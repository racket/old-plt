
@INCLUDE prefix.xci

#include "wx_panel.h"
#include "wx_enhdg.h"
#include "wx_dialg.h"
#include "wx_types.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define INTERACT_METHODS 1
#else
#define INTERACT_METHODS 0
#endif

@BEGINSYMBOLS panelFlags
@SYM "border" : wxBORDER
@ENDSYMBOLS

@INCLUDE wxs_ornt.xci

/* The derivation wx:panel -> wx:canvas is a lie for Xt */
@CLASSBASE wxPanel "wx:panel":"wx:canvas"

@CLASSID wxTYPE_PANEL

@CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1,SYM[panelFlags]=0,string="panel") : : /NOZERO[3]|NOZERO[4] <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1,SYM[panelFlags]=0,string="panel") : : /NOZERO[3]|NOZERO[4] <> panel parent

@ "fit" : void Fit();
@ "get-default-item" : wxButton! GetDefaultItem()
@ "get-item-cursor" : void GetCursor(int*,int*);
@ "set-item-cursor" : void SetItemCursor(int,int);

@SETMARK p = v
@INCLUDE wxs_panl.xci

@INCLUDE wxs_cnvs.xci

@ "set-label-position" : void SetLabelPosition(SYM[orientation]);
@ "get-label-position" : SYM[orientation] GetLabelPosition();

@ "get-horizontal-spacing" : int GetHorizontalSpacing();
@ "get-vertical-spacing" : int GetVerticalSpacing();
@ "set-horizontal-spacing" : void SetHorizontalSpacing(int);
@ "set-vertical-spacing" : void SetVerticalSpacing(int)

@ "new-line" : void NewLine(); <> no argument
@ "new-line" : void NewLine(int); <> tab amount

@ "get-panel-dc" : wxDC! GetDC();

@INCLUDE wxs_ifnt.xci
@INCLUDE wxs_icol.xci

@ "tab" : void Tab(); <> no argument
@ "tab" : void Tab(int); <> tab amount

@END


@BEGINSYMBOLS dialogFlags
@SYM "caption" : wxCAPTION
@SYM "thick-frame" : wxTHICK_FRAME
@SYM "system-menu" : wxSYSTEM_MENU
@SYM "resize-border" : wxRESIZE_BORDER
@ENDSYMBOLS

@CLASSBASE wxDialogBox "wx:dialog-box" : "wx:panel"

@CLASSID wxTYPE_DIALOG_BOX

@INCLUDE wxs_dorf.xci

@CREATOR (wxWindow^,nstring,bool=FALSE,int=300,int=300,int=500,int=500,SYM[dialogFlags]=wxDEFAULT_DIALOG_STYLE,string="dialogBox"); : : /DLGORFRAME[0."wx:dialog-box%::initialization"]|NOZERO[5]|NOZERO[6]

@CONSTANT "default-dialog-style" : long wxDEFAULT_DIALOG_STYLE

@SETMARK f = d
@INCLUDE wxs_fram.xci

@SETMARK p = d
@INCLUDE wxs_panl.xci

@END
