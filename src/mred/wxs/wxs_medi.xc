
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

static void *cconvert(wxMediaBuffer *b, double x, double y, int todc)
{
  double dcx, dcy;
  wxMediaAdmin *admin;
  Scheme_Object *a[2];

  admin = b->GetAdmin();
  if (admin) {
    float dx, dy;
    admin->GetDC(&dx, &dy);
    if (!todc) {
      dcx = dx + x;
      dcy = dy + y;
    } else {
      dcx = x - dx;
      dcy = y - dy;
    }
  } else {
    dcx = x;
    dcy = y;
  }

  a[0] = objscheme_bundle_double(dcx);
  a[1] = objscheme_bundle_double(dcy);

  return scheme_values(2, a);
}

static void *wxbBufferToDC(wxMediaBuffer *b, double x, double y)
{
  return cconvert(b, x, y, 1);
}

static void *wxbDCToBuffer(wxMediaBuffer *b, double x, double y)
{
  return cconvert(b, x, y, 0);
}

@MACRO rNULL = return NULL;
@MACRO rFALSE = return FALSE;
@MACRO rZERO = return 0;

@INCLUDE wxs_fcs.xci

@BEGINSYMBOLS editOp > ONE
@SYM "undo" : wxEDIT_UNDO
@SYM "redo" : wxEDIT_REDO
@SYM "clear" : wxEDIT_CLEAR
@SYM "cut" : wxEDIT_CUT
@SYM "copy" : wxEDIT_COPY
@SYM "paste" : wxEDIT_PASTE
@SYM "kill" : wxEDIT_KILL
@SYM "insert-text-box" : wxEDIT_INSERT_TEXT_BOX
@SYM "insert-pasteboard-box" : wxEDIT_INSERT_GRAPHIC_BOX
@SYM "insert-image" : wxEDIT_INSERT_IMAGE
@SYM "select-all" : wxEDIT_SELECT_ALL
@ENDSYMBOLS

@BEGINSYMBOLS printMethod > ONE
@SYM "standard" : 0
@SYM "postscript" : 1
@ENDSYMBOLS

@CLASSBASE wxMediaBuffer "editor" : "object"
@INTERFACE "editor"

@CLASSID wxTYPE_MEDIA_BUFFER

@IVAR "buffer-type" : SYM[bufferType] bufferType

@SETMARK Y = V
@SETMARK Z = v
@INCLUDE wxs_mbuf.xci

// X are Methods not intended to be overriden by the user,
// but acutally are implemented with virtual
@SETMARK X = D

// These don't use `pathname' because they expand internally
@ X "load-file" : bool LoadFile(nstring=NULL,SYM[fileType]=wxMEDIA_FF_GUESS,bool=TRUE);
@ X "save-file" : bool SaveFile(nstring=NULL,SYM[fileType]=wxMEDIA_FF_SAME,bool=TRUE);
@ X "insert-file" : bool InsertFile(string,SYM[fileType]=wxMEDIA_FF_GUESS,bool=TRUE);

@ X "get-extent" : void GetExtent(float?,float?);
@ X "get-descent" : float GetDescent(); : : : : XrZERO
@ X "get-space" : float GetSpace(); : : : : XrZERO

@ X "get-max-width" : float GetMaxWidth(); : : : : XrZERO
@ X "get-min-width" : float GetMinWidth(); : : : : XrZERO
@ X "set-max-width" : void SetMaxWidth(float);
@ X "set-min-width" : void SetMinWidth(float);
@ X "get-max-height" : float GetMaxHeight(); : : : : XrZERO
@ X "get-min-height" : float GetMinHeight(); : : : : XrZERO
@ X "set-max-height" : void SetMaxHeight(float);
@ X "set-min-height" : void SetMinHeight(float);

@ X "read-from-file" : bool ReadFromFile(wxMediaStreamIn%); : : : : XrZERO
@ X "write-to-file" : bool WriteToFile(wxMediaStreamOut%); : : : : XrZERO

@ X "style-has-changed" : void StyleHasChanged(wxStyle^);

@ X "begin-edit-sequence" : void BeginEditSequence(bool=TRUE);
@ X "end-edit-sequence" : void EndEditSequence();
@ X "refresh-delayed?" : bool RefreshDelayed();

@ X "get-snip-location" : bool GetSnipLocation(wxSnip!,float?=NULL,float?=NULL,bool=FALSE); : : : : XrZERO

@ X "scroll-line-location" : float ScrollLineLocation(long); : : : : XrZERO
@ X "num-scroll-lines" : long NumScrollLines(); : : : : XrZERO
@ X "find-scroll-line" : long FindScrollLine(float); : : : : XrZERO

@ X "print-to-dc" : void PrintToDC(wxDC!); : : /CHECKOK[0."editor<%>::print-to-dc"]

@ X "get-admin" : wxMediaAdmin^ GetAdmin(); : : : rNULL
@ X "set-admin" : void SetAdmin(wxMediaAdmin^);


@ "global-to-local" : void GlobalToLocal(float*,float*);
@ "local-to-global" : void LocalToGlobal(float*,float*);

@ v "get-dc" : wxDC^ GetDC();
@ v "get-view-size" : void GetViewSize(float?,float?);

@ "clear" : void Clear();
@ "select-all" : void SelectAll();

@ "undo" :  void Undo();
@ "redo": void Redo()
@ "clear-undos" : void ClearUndos();

@ "set-max-undo-history" : void SetMaxUndoHistory(int);
@ "get-max-undo-history" : int GetMaxUndoHistory();

@ "do-edit-operation" : void DoEdit(SYM[editOp],bool=TRUE,long=0);

@ "set-keymap" : void SetKeymap(wxKeymap^=NULL);
@ "get-keymap" : wxKeymap^ GetKeymap();

@ "get-style-list" : wxStyleList! GetStyleList();
@ "set-style-list" : void SetStyleList(wxStyleList!);

@ "set-load-overwrites-styles" : void SetLoadOverwritesStyles(bool)
@ "get-load-overwrites-styles" : bool GetLoadOverwritesStyles();

@ "set-cursor" : void SetCursor(wxCursor^,bool=TRUE); : : /CHECKVOIDABLEOK[0]

@ "lock" : void Lock(bool);
@ "modified?" : bool Modified();

@ "get-filename" : nstring GetFilename(bool?=NULL);

@ "insert-box" : void InsertBox(SYM[bufferType]=wxEDIT_BUFFER);
@ "insert-image" : void InsertImage(nstring=NULL,long=-1,bool=FALSE,bool=TRUE);

@ "print" : void Print(nstring=NULL,bool=TRUE,bool=FALSE,SYM[printMethod]=0);

@ "begin-write-header-footer-to-file" : bool BeginWriteHeaderFooterToFile(wxMediaStreamOut%,string,long*);
@ "end-write-header-footer-to-file" : bool EndWriteHeaderFooterToFile(wxMediaStreamOut%,long);

@ "get-focus-snip" : wxSnip^ GetFocusSnip();

@ "get-inactive-caret-threshold" : int GetInactiveCaretThreshold();
@ "set-inactive-caret-threshold" : void SetInactiveCaretThreshold(int);

@MACRO bundleAny = ((Scheme_Object *){x})
 
@ m "buffer-location-to-dc-location" : void*/bundleAny wxbBufferToDC(double, double);
@ m "dc-location-to-buffer-location" : void*/bundleAny wxbDCToBuffer(double, double);

@END

@GLOBAL wxMediaGlobal

@ "get-editor-print-margin" : void wxGetMediaPrintMargin(long?=NULL,long?=NULL);
@ "set-editor-print-margin" : void wxSetMediaPrintMargin(long=-1,long=-1);

@ "read-editor-global-header" : bool wxReadMediaGlobalHeader(wxMediaStreamIn%);
@ "read-editor-global-footer" : bool wxReadMediaGlobalFooter(wxMediaStreamIn%);
@ "write-editor-global-header" : bool wxWriteMediaGlobalHeader(wxMediaStreamOut%);
@ "write-editor-global-footer" : bool wxWriteMediaGlobalFooter(wxMediaStreamOut%);

@ "add-editor-keymap-functions" : void wxAddMediaBufferFunctions(wxKeymap!);
@ "add-text-editor-keymap-functions" : void wxAddMediaEditorFunctions(wxKeymap!);
@ "add-pasteboard-editor-keymap-functions" : void wxAddMediaPasteboardFunctions(wxKeymap!);

@ "editor-set-x-selection-mode" : void wxMediaSetXSelectionMode(bool);

@ "get-the-snip-class-list" : wxSnipClassList! wxGetTheSnipClassList()
@ "get-the-buffer-data-class-list" : wxBufferDataClassList! wxGetTheBufferDataClassList()

@END
