
@INCLUDE prefix.xci

#include "wx_media.h"
#include "wx_frame.h"
#include "wx_panel.h"

@INCLUDE wxs.xci

@HEADER

#include "wxs_obj.h"

@MACRO rNULL = return NULL;
@MACRO rFALSE = return FALSE;
@MACRO rZERO = return 0;

@BEGINSYMBOLS style
@SYM "no-hscroll" : wxMCANVAS_NO_H_SCROLL
@SYM "no-vscroll" : wxMCANVAS_NO_V_SCROLL
@SYM "hide-hscroll" : wxMCANVAS_HIDE_H_SCROLL
@SYM "hide-vscroll" : wxMCANVAS_HIDE_V_SCROLL
@ENDSYMBOLS

@INCLUDE wxs_fcs.xci

static void *DoCAPOCallback(void *data)
{
  jmp_buf savebuf;
  void *r;

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf))
    r = (void *)scheme_apply_multi((Scheme_Object *)data, 0, NULL);
  else
    r = (void *)scheme_false;

  COPY_JMPBUF(scheme_error_buf, savebuf);

  return r;
}

typedef void *(*CAPOFunc)(void*);

@CLASSBASE wxMediaCanvas "editor-canvas" : "canvas"

// @CREATOR (wxFrame!,int=-1,int=-1,int=-1,int=-1, string="",SYM[style]=0,int=100,wxMediaBuffer^=NULL); : : /NOZERO[3]|NOZERO[4] <> frame
@CREATOR (wxPanel!,int=-1,int=-1,int=-1,int=-1, string="",SYM[style]=0,int=100,wxMediaBuffer^=NULL); : : /NOZERO[3]|NOZERO[4] <> panel

@CLASSID wxTYPE_MEDIA_CANVAS

@ "set-editor" : void SetMedia(wxMediaBuffer^,bool=TRUE);
@ "get-editor" : wxMediaBuffer^ GetMedia();

@ v "on-set-focus" : void OnSetFocus();
@ v "on-kill-focus" : void OnKillFocus();

@ "is-focus-on?" : bool IsFocusOn();

@ "force-display-focus" : void ForceDisplayFocus(bool);

@ "allow-scroll-to-last" : void AllowScrollToLast(bool);
@ "scroll-with-bottom-base" : void ScrollWithBottomBase(bool);

@ "get-lazy-refresh" : bool GetLazyRefresh();
@ "set-lazy-refresh" : void SetLazyRefresh(bool);

@MACRO CastToSO = (Scheme_Object*){x}
@MACRO ubTestFunc = DoCAPOCallback
@MACRO ubData = p[0]
@MACRO spAnything = _
@MACRO spCAPOProc = (-> _)

@ "call-as-primary-owner" : void[]/CastToSO//spAnything CallAsPrimaryOwner(CAPOFunc//ubTestFunc///spCAPOProc, -void[]//ubData);

@SETMARK w = d
@INCLUDE wxs_win.xci

@SETMARK c = d
@INCLUDE wxs_cnvs.xci

@END


@CLASSBASE wxMediaAdmin "editor-admin":"object"

@CREATOR ();

@CLASSID wxTYPE_MEDIA_ADMIN

@SETMARK A = V
@INCLUDE wxs_madm.xci

@END

// #define FIXCMA os_wxCanvasMediaAdmin() : wxCanvasMediaAdmin(NULL) {}
// @CLASSBASE wxCanvasMediaAdmin "canvas-media-admin":"media-admin"
// @CLASSID wxTYPE_CANVAS_MEDIA_ADMIN
// @VAR FIXCMA
// @ "get-canvas" : wxMediaCanvas! GetCanvas()
// @END

#define FIXMSMA os_wxMediaSnipMediaAdmin() : wxMediaSnipMediaAdmin(NULL) {}

@CLASSBASE wxMediaSnipMediaAdmin "editor-snip-editor-admin":"editor-admin"

@CLASSID wxTYPE_MEDIA_SNIP_MEDIA_ADMIN

@VAR FIXMSMA

@ "get-snip" : wxMediaSnip! GetSnip()

@END


@CLASSBASE wxSnipAdmin "snip-admin":"object"

@CREATOR ();

@CLASSID wxTYPE_MEDIA_SNIP_ADMIN

@ V "get-editor" : wxMediaBuffer^ GetMedia(); : : : rNULL
@ V "get-dc" : wxDC^ GetDC(); : : : rNULL
@ V "get-view-size" : void GetViewSize(float?, float?);
@ V "get-view" : void GetView(float?, float?, float?, float?, wxSnip^=NULL);
@ V "scroll-to" : bool ScrollTo(wxSnip!, float,float,float,float, bool,int=0); : : : rFALSE
@ V "set-caret-owner" : void SetCaretOwner(wxSnip!,SYM[focus]);
@ V "resized" : void Resized(wxSnip!, bool);
@ V "recounted" : bool Recounted(wxSnip!, bool); : : : rFALSE
@ V "needs-update" : void NeedsUpdate(wxSnip!, float,float,float,float);
@ V "release-snip" : bool ReleaseSnip(wxSnip!); : : : rFALSE
@ V "update-cursor" : void UpdateCursor();

@END



@CLASSBASE wxSnipClass "snip-class" : "object"

@CREATOR ();

@CLASSID wxTYPE_SNIP_CLASS

@IVAR "classname" : string classname
@IVAR "version" : int version

@ V "read" : wxSnip^ Read(wxMediaStreamIn%); : : : rNULL
@ v "read-header" : bool ReadHeader(wxMediaStreamIn%);
@ v "read-done" : void ReadDone();
@ v "write-header" : bool WriteHeader(wxMediaStreamOut%);
@ v "write-done" : void WriteDone();

@END


@CLASSBASE wxSnipClassList "snip-class-list" : "object"

@CLASSID wxTYPE_SNIP_CLASS_LIST

@ "find" : wxSnipClass^ Find(string);
@ "find-position" : short FindPosition(wxSnipClass!);
@ "add" : void Add(wxSnipClass!);
@ "number" : int Number();
@ "nth" : wxSnipClass^ Nth(int);
@ "reading-version" : int ReadingVersion(wxSnipClass!);

@END


@CLASSBASE wxKeymap "keymap":"object"

typedef Scheme_Object KeymapCallbackToSchemeRec;
#define kctsr(o) o

static int KeyCallbackToScheme(UNKNOWN_OBJ, wxKeyEvent &, KeymapCallbackToSchemeRec *data);
static int MouseCallbackToScheme(UNKNOWN_OBJ, wxMouseEvent &, KeymapCallbackToSchemeRec *data);
static int GrabKeyCallbackToScheme(char *s, wxKeymap *km, UNKNOWN_OBJ, wxKeyEvent &, KeymapCallbackToSchemeRec *data);
static int GrabMouseCallbackToScheme(char *s, wxKeymap *km, UNKNOWN_OBJ, wxMouseEvent &, KeymapCallbackToSchemeRec *data);
static void ErrorCallbackToScheme(KeymapCallbackToSchemeRec *data, char *str);
static void BreakSequenceCallbackToScheme(KeymapCallbackToSchemeRec *data);

@MACRO bCallback =
@MACRO ubSetup = KeymapCallbackToSchemeRec *cb;

@MACRO ubCallbackKey = (wxKeyFunction)KeyCallbackToScheme
@MACRO ubCallbackMouse = (wxMouseFunction)MouseCallbackToScheme
@MACRO ubCallbackGrabKey = (wxGrabKeyFunction)GrabKeyCallbackToScheme
@MACRO ubCallbackGrabMouse = (wxGrabMouseFunction)GrabMouseCallbackToScheme
@MACRO ubCallbackError = (wxKeyErrorFunction)ErrorCallbackToScheme
@MACRO ubCallbackBreak = (wxBreakSequenceFunction)BreakSequenceCallbackToScheme

@MACRO spCallbackKey = (wxObject-object wxKeyEvent-object -> bool)
@MACRO spCallbackMouse = (wxObject-object wxMousrEvent-object -> bool)
@MACRO spCallbackGrabKey = (str wxKeymap-object wxObject-object wxKeyEvent-object -> bool)
@MACRO spCallbackGrabMouse = (str wxKeymap-object wxObject-object wxMouseEvent-object -> bool)
@MACRO spCallbackError = (str -> void)
@MACRO spCallbackBreak = (-> void)

@MACRO ubData = NULL

@MACRO ubSetData[n.m] = kctsr(cb) = p[<n>]; x<m> = (void *)cb;

@CREATOR ();

@CLASSID wxTYPE_KEYMAP

@ "get-double-click-interval" : long GetDoubleClickInterval();
@ "set-double-click-interval" : void SetDoubleClickInterval(long);

@MACRO bAnythingFromVoid = ((Scheme_Object *){x})
@MACRO ubAnythingToVoid = ((void *){x})
@MACRO cAnything = 1

@ v "handle-key-event" : bool HandleKeyEvent(UNKNOWN_OBJ/bAnythingFromVoid/ubAnythingToVoid/cAnything,wxKeyEvent%);
@ v "handle-mouse-event" : bool HandleMouseEvent(UNKNOWN_OBJ/bAnythingFromVoid/ubAnythingToVoid/cAnything,wxMouseEvent%);
@ "break-sequence" : void BreakSequence();
@ "map-function" : void MapFunction(string,string);
@ "implies-shift" : void ImpliesShift(string);
@ "add-key-function" : void AddKeyFunction(string,wxKeyFunction/bCallback/ubCallbackKey/cCallback//spCallbackKey,-unknown#void*=NULL); : : ubSetup / ubSetData[1.2]
@ "set-grab-key-function" : void SetGrabKeyFunction(wxGrabKeyFunction/bCallback/ubCallbackGrabKey/cCallback//spCallbackGrabKey,-unknown#void*=NULL); : : ubSetup / ubSetData[0.1]
@ "remove-grab-key-function" : void RemoveGrabKeyFunction()
@ "add-mouse-function" : void AddMouseFunction(string,wxMouseFunction/bCallback/ubCallbackMouse/cCallback2//spCallbackMouse,-unknown#void*=NULL); : : ubSetup / ubSetData[1.2]
@ "set-grab-mouse-function" : void SetGrabMouseFunction(wxGrabMouseFunction/bCallback/ubCallbackGrabMouse/cCallback//spCallbackGrabMouse,-unknown#void*=NULL); : : ubSetup / ubSetData[0.1]
@ "remove-grab-mouse-function" : void RemoveGrabMouseFunction()
@ "call-function" : bool CallFunction(string,UNKNOWN_OBJ/bAnythingFromVoid/ubAnythingToVoid/cAnything,wxKeyEvent%,bool=FALSE); <> key-event%
@ "call-function" : bool CallFunction(string,UNKNOWN_OBJ/bAnythingFromVoid/ubAnythingToVoid/cAnything,wxMouseEvent%,bool=FALSE); <> mouse-event%
@ "set-error-callback" : void SetErrorCallback(wxKeyErrorFunction/bCallback/ubCallbackError/cCallback//spCallbackError,-unknown#void*=NULL); : : ubSetup / ubSetData[0.1]
@ "set-break-sequence-callback" : void SetBreakSequenceCallback(wxBreakSequenceFunction/bCallback/ubCallbackBreak/cCallback//spCallbackBreak,-unknown#void*=NULL); : : ubSetup / ubSetData[0.1]
@ "chain-to-keymap" : void ChainToKeymap(wxKeymap!,bool);
@ "remove-chained-keymap" : void RemoveChainedKeymap(wxKeymap!);

@END

static Bool KeyCallbackToScheme(UNKNOWN_OBJ media, wxKeyEvent &event, 
			       KeymapCallbackToSchemeRec *data)
{
  extern Scheme_Object *objscheme_bundle_wxKeyEvent(wxKeyEvent *);
  Scheme_Object *p[2], *obj;
  Bool retval;
  jmp_buf savebuf;

  p[0] = (Scheme_Object *)media;
  p[1] = objscheme_bundle_wxKeyEvent(&event);

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    obj = scheme_apply(kctsr(data), 2, p);
    retval = objscheme_unbundle_bool(obj, "Scheme key callback");
  } else
    retval = 0;

  COPY_JMPBUF(scheme_error_buf, savebuf);
 
  return retval;
}

static Bool GrabKeyCallbackToScheme(char *s, wxKeymap *km,
				    UNKNOWN_OBJ media, wxKeyEvent &event, 
				    KeymapCallbackToSchemeRec *data)
{
  extern Scheme_Object *objscheme_bundle_wxKeyEvent(wxKeyEvent *);
  Scheme_Object *p[4], *obj;
  Bool retval;
  jmp_buf savebuf;

  p[0] = objscheme_bundle_string(s);
  p[1] = objscheme_bundle_wxKeymap(km);
  p[2] = (Scheme_Object *)media;
  p[3] = objscheme_bundle_wxKeyEvent(&event);

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    obj = scheme_apply(kctsr(data), 4, p);
    retval = objscheme_unbundle_bool(obj, "Scheme grab-key callback");
  } else
    retval = 0;

  COPY_JMPBUF(scheme_error_buf, savebuf);
 
  return retval;
}

static Bool MouseCallbackToScheme(UNKNOWN_OBJ media, wxMouseEvent &event, 
				 KeymapCallbackToSchemeRec *data)
{
  extern Scheme_Object *objscheme_bundle_wxMouseEvent(wxMouseEvent *);
  Scheme_Object *p[2], *obj;
  Bool retval;
  jmp_buf savebuf;

  p[0] = (Scheme_Object *)media;
  p[1] = objscheme_bundle_wxMouseEvent(&event);

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    obj = scheme_apply(kctsr(data), 2, p);
    retval = objscheme_unbundle_bool(obj, "Scheme mouse callback");
  } else
    retval = 0;

  COPY_JMPBUF(scheme_error_buf, savebuf);
  
  return retval;
}

static Bool GrabMouseCallbackToScheme(char *s, wxKeymap *km,
				      UNKNOWN_OBJ media, wxMouseEvent &event, 
				      KeymapCallbackToSchemeRec *data)
{
  extern Scheme_Object *objscheme_bundle_wxMouseEvent(wxMouseEvent *);
  Scheme_Object *p[3], *obj;
  Bool retval;
  jmp_buf savebuf;

  p[0] = objscheme_bundle_string(s);
  p[1] = objscheme_bundle_wxKeymap(km);
  p[2] = (Scheme_Object *)media;
  p[3] = objscheme_bundle_wxMouseEvent(&event);

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    obj = scheme_apply(kctsr(data), 4, p);
    retval = objscheme_unbundle_bool(obj, "Scheme grab-mouse callback");
  } else
    retval = 0;

  COPY_JMPBUF(scheme_error_buf, savebuf);
 
  return retval;
}

static void ErrorCallbackToScheme(KeymapCallbackToSchemeRec *data, char *err)
{
  Scheme_Object *p[1];
  jmp_buf savebuf;

  p[0] = objscheme_bundle_string(err);

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    scheme_apply_multi(kctsr(data), 1, p);
  }

  COPY_JMPBUF(scheme_error_buf, savebuf);
}

static void BreakSequenceCallbackToScheme(KeymapCallbackToSchemeRec *data)
{
  jmp_buf savebuf;

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    scheme_apply_multi(kctsr(data), 0, NULL);
  }

  COPY_JMPBUF(scheme_error_buf, savebuf);
}

@INCLUDE wxs_bkt.xci

@CLASSBASE wxMediaWordbreakMap "editor-wordbreak-map" : "object"

@CREATOR ()

@CLASSID wxTYPE_WORDBREAK_MAP

@ "set-map" : void SetMap(uchar,SYM[breakType]);
@ "get-map" : SYM[breakType] GetMap(uchar);

@CONSTANT "the-editor-wordbreak-map" : wxMediaWordbreakMap% wxTheMediaWordbreakMap

@END

