
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

@INCLUDE wxs_eop.xci

@BEGINSYMBOLS printMethod > ONE
@SYM "standard" : 0
@SYM "postscript" : 1
@ENDSYMBOLS

@INCLUDE wxs_bmt.xci

@CLASSBASE wxMediaBuffer "editor" : "object"
@INTERFACE "editor"

@CLASSID wxTYPE_MEDIA_BUFFER

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

@ X "get-extent" : void GetExtent(nnfloat?,nnfloat?);
@ X "get-descent" : float GetDescent(); : : : : XrZERO
@ X "get-space" : float GetSpace(); : : : : XrZERO

@ X "get-max-width" : nnfs[none] GetMaxWidth(); : : : : XrZERO
@ X "get-min-width" : nnfs[none] GetMinWidth(); : : : : XrZERO
@ X "set-max-width" : void SetMaxWidth(nnfs[none]);
@ X "set-min-width" : void SetMinWidth(nnfs[none]);
@ X "get-max-height" : nnfs[none] GetMaxHeight(); : : : : XrZERO
@ X "get-min-height" : nnfs[none] GetMinHeight(); : : : : XrZERO
@ X "set-max-height" : void SetMaxHeight(nnfs[none]);
@ X "set-min-height" : void SetMinHeight(nnfs[none]);

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

@ X "print-to-dc" : void PrintToDC(wxDC!); : : /CHECKOK[0.METHODNAME("editor<%>","print-to-dc")]

@ X "get-admin" : wxMediaAdmin^ GetAdmin(); : : : rNULL
@ X "set-admin" : void SetAdmin(wxMediaAdmin^);


@ "global-to-local" : void GlobalToLocal(float?,float?);
@ "local-to-global" : void LocalToGlobal(float?,float?);

@ v "get-dc" : wxDC^ GetDC();
@ v "get-view-size" : void GetViewSize(nnfloat?,nnfloat?);

@ "clear" : void Clear();
@ "select-all" : void SelectAll();

@ "undo" :  void Undo();
@ "redo": void Redo()
@ "clear-undos" : void ClearUndos();

@ "set-max-undo-history" : void SetMaxUndoHistory(rint[0|100000]);
@ "get-max-undo-history" : int GetMaxUndoHistory();

@ "do-edit-operation" : void DoEdit(SYM[editOp],bool=TRUE,ExactLong=0);

@ "set-keymap" : void SetKeymap(wxKeymap^=NULL);
@ "get-keymap" : wxKeymap^ GetKeymap();

@ "get-style-list" : wxStyleList! GetStyleList();
@ "set-style-list" : void SetStyleList(wxStyleList!);

@ "set-load-overwrites-styles" : void SetLoadOverwritesStyles(bool)
@ "get-load-overwrites-styles" : bool GetLoadOverwritesStyles();

@ "set-cursor" : void SetCursor(wxCursor^,bool=TRUE); : : /CHECKVOIDABLEOK[0]

@ "lock" : void Lock(bool);
@ "is-locked?" : bool IsLocked();
@ "is-modified?" : bool Modified();

@ "get-filename" : nstring GetFilename(bool?=NULL);

@ "insert-box" : void InsertBox(SYM[bufferType]=wxEDIT_BUFFER);
@ "insert-image" : void InsertImage(nstring=NULL,SYM[bitmapType]=0,bool=FALSE,bool=TRUE);

@ "print" : void Print(bool=TRUE,bool=TRUE,SYM[printMethod]=0);

@ "begin-write-header-footer-to-file" : bool BeginWriteHeaderFooterToFile(wxMediaStreamOut%,string,long*);
@ "end-write-header-footer-to-file" : bool EndWriteHeaderFooterToFile(wxMediaStreamOut%,long);

@ "get-focus-snip" : wxSnip^ GetFocusSnip();

@ "get-inactive-caret-threshold" : SYM[caret] GetInactiveCaretThreshold();
@ "set-inactive-caret-threshold" : void SetInactiveCaretThreshold(SYM[caret]);

@MACRO bundleAny = ((Scheme_Object *){x})
 
@ m "editor-location-to-dc-location" : void*/bundleAny wxbBufferToDC(double, double);
@ m "dc-location-to-editor-location" : void*/bundleAny wxbDCToBuffer(double, double);

@END

@GLOBAL wxMediaGlobal

@ "get-editor-print-margin" : void wxGetMediaPrintMargin(nnlong?,nnlong?);
@ "set-editor-print-margin" : void wxSetMediaPrintMargin(nnlong,nnlong);

@ "read-editor-global-header" : bool wxReadMediaGlobalHeader(wxMediaStreamIn%);
@ "read-editor-global-footer" : bool wxReadMediaGlobalFooter(wxMediaStreamIn%);
@ "write-editor-global-header" : bool wxWriteMediaGlobalHeader(wxMediaStreamOut%);
@ "write-editor-global-footer" : bool wxWriteMediaGlobalFooter(wxMediaStreamOut%);

@ "add-editor-keymap-functions" : void wxAddMediaBufferFunctions(wxKeymap!);
@ "add-text-keymap-functions" : void wxAddMediaEditorFunctions(wxKeymap!);
@ "add-pasteboard-keymap-functions" : void wxAddMediaPasteboardFunctions(wxKeymap!);

@ "editor-set-x-selection-mode" : void wxMediaSetXSelectionMode(bool);

@ "get-the-snip-class-list" : wxSnipClassList! wxGetTheSnipClassList()
@ "get-the-editor-data-class-list" : wxBufferDataClassList! wxGetTheBufferDataClassList()

@END
