
@INCLUDE prefix.xci

#if 0

#include "wx_item.h"
#include "wx_txt.h"
#include "wx_mtxt.h"
#include "wx_text.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS textStyle
@SYM "process-enter" : wxPROCESS_ENTER
@SYM "password" : wxPASSWORD
@SYM "readonly" : wxREADONLY      
@ENDSYMBOLS

@CLASSBASE wxText "wx:text":"wx:item"

@CLASSID wxTYPE_TEXT

@SET CALLBACK_CLASS = wxText
@SET CALLBACK_CLASS_USER = "wx:text%::initialization"
@INCLUDE cb_start.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,nstring,string="",int=-1,int=-1,int=-1,int=-1,SYM[textStyle]=0,string="text"); : : ubCallbackSetup/NOZERO[6]|NOZERO[7]//ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

@ "get-value" : string GetValue();
@ "set-value" : void SetValue(string);

@ "copy" : void Copy();
@ "cut" : void Cut()
@ "paste" : void Paste();
@ "set-editable" : void SetEditable(bool);

@SETMARK c = d
@INCLUDE wxs_char.xci

@END

@INCLUDE cb_end.xci

@BEGINSYMBOLS multiTextStyle
@SYM "process-enter" : wxPROCESS_ENTER
@SYM "password" : wxPASSWORD
@SYM "readonly" : wxREADONLY
@SYM "hscroll" : wxHSCROLL
@ENDSYMBOLS

@CLASSBASE wxMultiText "wx:multi-text":"wx:text"

@CLASSID wxTYPE_MULTI_TEXT

@SET CALLBACK_CLASS = wxMultiText
@SET CALLBACK_CLASS_USER = "wx:multi-text%::initialization"
@INCLUDE cb_start.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,nstring,string="",int=-1,int=-1,int=-1,int=-1,SYM[multiTextStyle]=0,string="text"); : : ubCallbackSetup/NOZERO[6]|NOZERO[7]//ubCallbackCreatorFinish

@ "get-value" : string GetValue();

@SETMARK c = d
@INCLUDE wxs_char.xci

@END

@INCLUDE cb_end.xci

@BEGINSYMBOLS textWinStyle
@SYM "readonly" : wxREADONLY
@ENDSYMBOLS

@CLASSBASE wxTextWindow "wx:text-window" : "wx:window"

@CLASSID wxTYPE_TEXT_WINDOW

@CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1,SYM[textWinStyle]=0,string="textWindow");: : /NOZERO[3]|NOZERO[4] <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1,SYM[textWinStyle]=0,string="textWindow");: : /NOZERO[3]|NOZERO[4] <> panel

@ "popup-menu" : bool PopupMenu(wxMenu!, float, float);

@ "clear" : void Clear();
@ "copy" : void Copy();
@ "cut" : void Cut();
@ "paste" : void Paste();
@ "discard-edits" : void DiscardEdits();
@ "get-contents" : string GetContents();
@ "get-insertion-point" : long GetInsertionPoint();
@ "get-last-position" : long GetLastPosition();
@ "get-line-length" : long GetLineLength(int);
@ "get-number-of-lines" : int GetNumberOfLines();
@ "set-selection" : void SetSelection(long,long);
@ "load-file" : bool LoadFile(pathname);
@ "save-file" : bool SaveFile(pathname);
@ "modified?" : bool Modified();
@ "position-to-x-y" : void PositionToXY(long,long*,long*);
@ "x-y-to-position" : long XYToPosition(long,long);
@ "remove" : void Remove(long,long);
@ "replace" : void Replace(long,long,string);
@ "set-font" : void SetFont(wxFont!);
@ "set-insertion-point" : void SetInsertionPoint(long);
@ "set-insertion-point-end" : void SetInsertionPointEnd();
@ "show-position" : void ShowPosition(long);
@ "write-text" : void WriteText(string);

@SETMARK w = d
@INCLUDE wxs_win.xci

@SETMARK c = d
@INCLUDE wxs_char.xci

@END

#endif
