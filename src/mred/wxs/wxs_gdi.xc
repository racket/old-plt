
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_list.h"
#include "wx_gdi.h"
#ifdef wx_xt
# include "wx_dc.h"
#endif
#include "wx_rgn.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS family > ONE
@SYM "default" : wxDEFAULT
@SYM "decorative" : wxDECORATIVE
@SYM "roman" : wxROMAN
@SYM "script" : wxSCRIPT
@SYM "swiss" : wxSWISS
@SYM "modern" : wxMODERN
@SYM "system" : wxSYSTEM
@SYM "symbol" : wxSYMBOL
@ENDSYMBOLS

@BEGINSYMBOLS weight > ONE > PRED
@SYM "normal" : wxNORMAL
@SYM "light" : wxLIGHT
@SYM "bold" : wxBOLD
@ENDSYMBOLS

@BEGINSYMBOLS style > ONE > PRED
@SYM "normal" : wxNORMAL
@SYM "italic" : wxITALIC
@SYM "slant" : wxSLANT
@ENDSYMBOLS

@BEGINSYMBOLS smoothing > ONE > PRED
@SYM "default" : wxSMOOTHING_DEFAULT
@SYM "partly-smoothed" : wxSMOOTHING_PARTIAL
@SYM "smoothed" : wxSMOOTHING_ON
@SYM "unsmoothed" : wxSMOOTHING_OFF
@ENDSYMBOLS

/* Not used, anyway: */
#if defined(wx_mac) || defined(wx_xt)
# define COLORMAP_CREATE 0
#else
# define COLORMAP_CREATE 1
#endif

@MACRO CHECKMUT[TYPE.what.who] = if (!((<TYPE> *)((Scheme_Class_Object *)THEOBJ)->primdata)->IsMutable()) WITH_VAR_STACK(scheme_signal_error("%s: this %s%% object is locked (in use by a dc<%%> object or in a list of %s constants)", <who>, <what>, <what>));

@CLASSBASE wxFont "font":"object"

@CREATOR (); <> no argument
@CREATOR (rint[1|255],SYM[family],SYM[style],SYM[weight],bool=0,SYM[smoothing]=wxSMOOTHING_DEFAULT,bool=0) <> family
@CREATOR (rint[1|255],cstring,SYM[family],SYM[style],SYM[weight],bool=0,SYM[smoothing]=wxSMOOTHING_DEFAULT,bool=0) <> font name

@ "get-family" : SYM[family] GetFamily();
@ "get-face" : nstring GetFaceString();
@ "get-style" : SYM[style] GetStyle();
@ "get-point-size" : int GetPointSize();
@ "get-weight" : SYM[weight] GetWeight();
@ "get-smoothing" : SYM[smoothing] GetSmoothing();
@ "get-underlined" : bool GetUnderlined();
@ "get-size-in-pixels" : bool GetSizeInPixels();

@ "get-font-id" : int GetFontId();

@END

@CLASSBASE wxFontList "font-list":"object"

@CREATOR ();

@ "find-or-create-font" : wxFont! FindOrCreateFont(rint[1|255],SYM[family],SYM[style],SYM[weight],bool=0,SYM[smoothing]=wxSMOOTHING_DEFAULT,bool=0) <> family id
@ "find-or-create-font" : wxFont! FindOrCreateFont(rint[1|255],cstring,SYM[family],SYM[style],SYM[weight],bool=0,SYM[smoothing]=wxSMOOTHING_DEFAULT,bool=0) <> font name

@END


@CLASSBASE wxColour "color" : "object"

@CREATOR (); <> no argument
@CREATOR (ubyte,ubyte,ubyte); <> rgb values
@CREATOR (string); <> color name

@ "copy-from" : wxColour! CopyFrom(wxColour!);  : : /CHECKMUT[wxColour."color".METHODNAME("color%","copy-from")]
// @ "get" : void Get(ubyte*,ubyte*,ubyte*);
@ "ok?" : bool Ok();
@ "set" : void Set(ubyte,ubyte,ubyte);   : : /CHECKMUT[wxColour."color".METHODNAME("color%","set")]

@ "red" : ubyte Red();
@ "green" : ubyte Green();
@ "blue" : ubyte Blue();

@END


#ifdef wx_mac
#define _KEY_TYPE KeyType
#else
#define _KEY_TYPE int
#endif

@CLASSBASE wxColourDatabase "color-database" : "object"
@INTERFACE "color-database"

@ "find-color" : wxColour^ FindColour(string);

@END


@CLASSBASE wxPoint "point" : "object" / nofnl

@CREATOR (); <> no argument
@CREATOR (float,float); <> xy values

@IVAR "x" : float x
@IVAR "y" : float y

@END

@BEGINSYMBOLS brushStyle > ONE > PRED
@SYM "transparent" : wxTRANSPARENT
@SYM "solid" : wxSOLID
@SYM "opaque" : wxSTIPPLE
@SYM "xor" : wxXOR
@SYM "hilite" : wxCOLOR
@SYM "bdiagonal-hatch" : wxBDIAGONAL_HATCH
@SYM "crossdiag-hatch" : wxCROSSDIAG_HATCH
@SYM "fdiagonal-hatch" : wxFDIAGONAL_HATCH
@SYM "cross-hatch" : wxCROSS_HATCH
@SYM "horizontal-hatch" : wxHORIZONTAL_HATCH
@SYM "vertical-hatch" : wxVERTICAL_HATCH
@SYM "panel" : wxPANEL_PATTERN
@ENDSYMBOLS

@CLASSBASE wxBrush "brush" : "object"

@CREATOR (); <> no argument
@CREATOR (wxColour!,SYM[brushStyle]); <> color%
@CREATOR (string,SYM[brushStyle]); <> color name

@ "get-color" : wxColour! GetColour();
@ "set-color" : void SetColour(wxColour!); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-color")] <> color%
@ "set-color" : void SetColour(string); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-color")] <> color name
@ "set-color" : void SetColour(ubyte,ubyte,ubyte); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-color")] <> rgb values

@ "get-stipple" : wxBitmap! GetStipple();
@ "set-stipple" : void SetStipple(wxBitmap^); : : /CHECKOK[0.METHODNAME("brush%","set-stipple")]|CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-stipple")]

@ "get-style" : SYM[brushStyle] GetStyle();
@ "set-style" : void SetStyle(SYM[brushStyle]); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-style")]

@END

@CLASSBASE wxBrushList "brush-list" : "object"

@CREATOR ();

@ "find-or-create-brush" : wxBrush! FindOrCreateBrush(wxColour!,SYM[brushStyle]); <> color%
@ "find-or-create-brush" : wxBrush^ FindOrCreateBrush(string,SYM[brushStyle]); <> color name

@END

@BEGINSYMBOLS penStyle > ONE > PRED
@SYM "transparent" : wxTRANSPARENT
@SYM "solid" : wxSOLID
@SYM "xor" : wxXOR
@SYM "hilite" : wxCOLOR
@SYM "dot" : wxDOT
@SYM "long-dash" : wxLONG_DASH
@SYM "short-dash" : wxSHORT_DASH
@SYM "dot-dash" : wxDOT_DASH
@SYM "xor-dot" : wxXOR_DOT
@SYM "xor-long-dash" : wxXOR_LONG_DASH
@SYM "xor-short-dash" : wxXOR_SHORT_DASH
@SYM "xor-dot-dash" : wxXOR_DOT_DASH
@ENDSYMBOLS

@BEGINSYMBOLS join > ONE > PRED
@SYM "bevel" : wxJOIN_BEVEL
@SYM "miter" : wxJOIN_MITER
@SYM "round" : wxJOIN_ROUND
@ENDSYMBOLS

@BEGINSYMBOLS cap > ONE > PRED
@SYM "round" : wxCAP_ROUND
@SYM "projecting" : wxCAP_PROJECTING
@SYM "butt" : wxCAP_BUTT
@ENDSYMBOLS

@MACRO CHECKBW[p.who] = if (x<p> && (x<p>->GetDepth() != 1)) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "bitmap is not monochrome: ", p[POFFSET+<p>]));
@MACRO CHECKEIGHT[p.who] = if (x<p> && ((x<p>->GetWidth() != 8) || (x<p>->GetHeight() != 8))) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "bitmap is not 8x8: ", p[POFFSET+<p>]));

@CLASSBASE wxPen "pen" : "object"

@CREATOR (); <> no argument
@CREATOR (wxColour!,rfloat[0|255],SYM[penStyle]); <> color%
@CREATOR (string,rfloat[0|255],SYM[penStyle]); <> color name

@ "get-width" : float GetWidthF();
@ "set-width" : void SetWidth(rfloat[0|255]);
@ "get-cap" : SYM[cap] GetCap();
@ "set-cap" : void SetCap(SYM[cap]);
@ "get-join" : SYM[join] GetJoin();
@ "set-join" : void SetJoin(SYM[join]);

@ "get-color" : wxColour! GetColour();
@ "set-color" : void SetColour(wxColour!);  : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-color")] <> color%
@ "set-color" : void SetColour(string);  : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-color")] <> color name
@ "set-color" : void SetColour(ubyte,ubyte,ubyte);  : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-color")] <> rgb values

@ "get-stipple" : wxBitmap! GetStipple();
@ "set-stipple" : void SetStipple(wxBitmap^); : : /CHECKBW[0.METHODNAME("pen%","set-stipple")]|CHECKEIGHT[0.METHODNAME("pen%","set-stipple")]|CHECKOK[0.METHODNAME("pen%","set-stipple")]|CHECKMUT[wxPen."pen".METHODNAME("pen%","set-stipple")]

@ "get-style" : SYM[penStyle] GetStyle();
@ "set-style" : void SetStyle(SYM[penStyle]); : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-style")]

@END


@CLASSBASE wxPenList "pen-list" : "object"

@CREATOR ();

@ "find-or-create-pen" : wxPen! FindOrCreatePen(wxColour!,rfloat[0|255],SYM[penStyle]); <> color%
@ "find-or-create-pen" : wxPen^ FindOrCreatePen(string,rfloat[0|255],SYM[penStyle]); <> color name

@END

@BEGINSYMBOLS cursor > ONE > BUNDLE
@SYM "arrow" : wxCURSOR_ARROW
@SYM "bullseye" : wxCURSOR_BULLSEYE
// @SYM "char" : wxCURSOR_CHAR
@SYM "cross" : wxCURSOR_CROSS
@SYM "hand" : wxCURSOR_HAND
@SYM "ibeam" : wxCURSOR_IBEAM
@SYM "size-n/s" : wxCURSOR_SIZENS
@SYM "size-e/w" : wxCURSOR_SIZEWE
@SYM "size-ne/sw" : wxCURSOR_SIZENESW
@SYM "size-nw/se" : wxCURSOR_SIZENWSE
// @SYM "left-button" : wxCURSOR_LEFT_BUTTON
// @SYM "magnifier" : wxCURSOR_MAGNIFIER
// @SYM "middle-button" : wxCURSOR_MIDDLE_BUTTON
// @SYM "no-entry" : wxCURSOR_NO_ENTRY
// @SYM "paint-brush" : wxCURSOR_PAINT_BRUSH
// @SYM "pencil" : wxCURSOR_PENCIL
// @SYM "point-left" : wxCURSOR_POINT_LEFT
// @SYM "point-right" : wxCURSOR_POINT_RIGHT
// @SYM "question-arrow" : wxCURSOR_QUESTION_ARROW
// @SYM "right-button" : wxCURSOR_RIGHT_BUTTON
// @SYM "sizing" : wxCURSOR_SIZING
// @SYM "spraycan" : wxCURSOR_SPRAYCAN
// @SYM "wait" : wxCURSOR_WAIT
@SYM "watch" : wxCURSOR_WATCH
@ENDSYMBOLS

@MACRO CHECKSIXTEEN[p.who] = { if (x<p>->GetDepth() != 1) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "bitmap is not monochrome: ", p[POFFSET+<p>]));if ((x<p>->GetWidth() != 16) || (x<p>->GetHeight() != 16)) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "bitmap is not 16 pixels by 16 pixels: ", p[POFFSET+<p>])); }

@CLASSBASE wxCursor "cursor" : "object"

@CREATOR (SYM[cursor]); <> symbolic name
@CREATOR (wxBitmap!,wxBitmap!,rint[0|15]=0,rint[0|15]=0); : : /CHECKOK[0.METHODNAME("cursor","initialization")]|CHECKSIXTEEN[0.METHODNAME("cursor","initialization")]|CHECKOK[1.METHODNAME("cursor","initialization")]|CHECKSIXTEEN[1.METHODNAME("cursor","initialization")] <> bitmap

@ "ok?" : bool Ok();

@END

static void *RgnBoundingBox(wxRegion *r)
{
  float x, y, w, h;
  Scheme_Object *a[4];
  void *rt;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 4);

  a[0] = a[1] = a[2] = a[3] = NULL;

  WITH_VAR_STACK(r->BoundingBox(&x, &y, &w, &h));
  a[0] = WITH_VAR_STACK(scheme_make_double(x));
  a[1] = WITH_VAR_STACK(scheme_make_double(y));
  a[2] = WITH_VAR_STACK(scheme_make_double(w));
  a[3] = WITH_VAR_STACK(scheme_make_double(h));
  rt = WITH_VAR_STACK(scheme_values(4, a));
  READY_TO_RETURN;
  return rt;
}

@SET TYPE = wxPoint
@SET INDIRECT = 1
@SET POINTWISE = 1
@INCLUDE list.xci

@INCLUDE wxs_drws.xci

@MACRO CheckRgn[p.who] = if (x<p>->GetDC() != ((wxRegion *)((Scheme_Class_Object *)THEOBJ)->primdata)->GetDC()) scheme_arg_mismatch(METHODNAME("region<%>",<who>), "provided region's dc does not match this region's dc: ", p[POFFSET+<p>]);
@MACRO CheckRgnLock[who] = if (((wxRegion *)((Scheme_Class_Object *)THEOBJ)->primdata)->locked) scheme_arg_mismatch(METHODNAME("region<%>",<who>), "cannot mutate region, because it is currently installed as its dc's clipping region: ", THEOBJ);

@CLASSBASE wxRegion "region" : "object"

@CREATOR (wxDC!)
@ARGNAMES dc

@ "get-dc" : wxDC! GetDC()

@ "set-rectangle" : void SetRectangle(float, float, nnfloat, nnfloat); : : /CheckRgnLock["set-rectangle"]
@ "set-rounded-rectangle" : void SetRoundedRectangle(float, float, nnfloat, nnfloat, float=20.0); : : /CheckRgnLock["set-rounded-rectangle"]
@ "set-ellipse" : void SetEllipse(float, float, nnfloat, nnfloat); : : /CheckRgnLock["set-ellipse"]
@ "set-polygon" : void SetPolygon(-int,wxPoint!/bList/ubList/cList,float=0,float=0,SYM[fillKind]=wxODDEVEN_RULE); : / methListSet[wxPoint.0.1.0]// : /CheckRgnLock["set-polygon"]|glueListSet[wxPoint.0.1.0.METHODNAME("region%","set-polygon")]//
@ "set-arc" : void SetArc(float, float, nnfloat, nnfloat, float, float); : : /CheckRgnLock["set-arc"]

@ "union" : void Union(wxRegion!);  : : /CheckRgnLock["union"]|CheckRgn[0."union"]
@ "intersect" : void Intersect(wxRegion!);  : : /CheckRgnLock["intersect"]|CheckRgn[0."intersect"]
@ "subtract" : void Subtract(wxRegion!);  : : /CheckRgnLock["subtract"]|CheckRgn[0."subtract"]

@MACRO bundleAny = ((Scheme_Object *){x})
 
@ m "get-bounding-box" : void*/bundleAny RgnBoundingBox();

@ "is-empty?" : bool Empty();

@END


static inline int Identity(wxFontNameDirectory *, int v)
{
  return v;
}

@CLASSBASE wxFontNameDirectory "font-name-directory":"object"
@INTERFACE "font-name-directory"

@ "get-screen-name" : nstring GetScreenName(int,SYM[weight],SYM[style]);
@ "get-post-script-name" : nstring GetPostScriptName(int,SYM[weight],SYM[style]);

@ "set-screen-name" : void SetScreenName(int,SYM[weight],SYM[style],string);
@ "set-post-script-name" : void SetPostScriptName(int,SYM[weight],SYM[style],string);

@ "get-font-id" : int GetFontId(string,SYM[family]);
@ "get-face-name" : nstring GetFontName(int);
@ "get-family" : SYM[family] GetFamily(int);

@ "find-or-create-font-id" : int FindOrCreateFontId(cstring,SYM[family]);
@ m "find-family-default-font-id" : int Identity(SYM[family]);

@END

static wxColourDatabase* wxGetTheColourDatabase()
{
 return wxTheColourDatabase;
}

static wxBrushList* wxGetTheBrushList()
{
 return wxTheBrushList;
}

static wxPenList* wxGetThePenList()
{
 return wxThePenList;
}

static wxFontList* wxGetTheFontList()
{
 return wxTheFontList;
}

static wxFontNameDirectory* wxGetTheFontNameDirectory()
{
 return wxTheFontNameDirectory;
}

@GLOBAL wxGDIGlobal 

@ "get-the-color-database" : wxColourDatabase! wxGetTheColourDatabase()
@ "get-the-brush-list" : wxBrushList! wxGetTheBrushList()
@ "get-the-pen-list" : wxPenList! wxGetThePenList()
@ "get-the-font-list" : wxFontList! wxGetTheFontList()
@ "get-the-font-name-directory" : wxFontNameDirectory! wxGetTheFontNameDirectory()

@END
