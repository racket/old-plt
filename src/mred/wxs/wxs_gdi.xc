
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_list.h"
#include "wx_gdi.h"
#ifdef wx_xt
# include "wx_dc.h"
#endif

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_bmt.xci

@BEGINSYMBOLS family > ONE
@SYM "default" : wxDEFAULT
@SYM "decorative" : wxDECORATIVE
@SYM "roman" : wxROMAN
@SYM "script" : wxSCRIPT
@SYM "swiss" : wxSWISS
@SYM "modern" : wxMODERN
@SYM "teletype" : wxTELETYPE
@SYM "system" : wxSYSTEM
@ENDSYMBOLS

@BEGINSYMBOLS weight > ONE
@SYM "normal" : wxNORMAL
@SYM "light" : wxLIGHT
@SYM "bold" : wxBOLD
@ENDSYMBOLS

@BEGINSYMBOLS style > ONE
@SYM "normal" : wxNORMAL
@SYM "italic" : wxITALIC
@SYM "slant" : wxSLANT
@ENDSYMBOLS

#define USE_FONT_NAME_DIRECTORY 1

/* Not used, anyway: */
#if defined(wx_mac) || defined(wx_xt)
# define COLORMAP_CREATE 0
#else
# define COLORMAP_CREATE 1
#endif

@MACRO CHECKMUT[TYPE.what.who] = if (!((<TYPE> *)((Scheme_Class_Object *)obj)->primdata)->IsMutable()) scheme_signal_error("%s: this %s%% object is locked (in use by a dc%% or in a list of %s constants)", <who>, <what>, <what>);

@CLASSBASE wxFont "font":"object"

@CREATOR (); <> no argument
@CREATOR (nnint,SYM[family],SYM[style],SYM[weight],bool=0) <> family
@CREATOR (nnint,cstring,SYM[family],SYM[style],SYM[weight],bool=0) <> font name

@ "get-family" : SYM[family] GetFamily();
@ "get-face" : nstring GetFaceString();
@ "get-style" : SYM[style] GetStyle();
@ "get-point-size" : int GetPointSize();
@ "get-weight" : SYM[weight] GetWeight();
@ "get-underlined" : bool GetUnderlined();

@ "get-font-id" : int GetFontId();

@END

@CLASSBASE wxFontList "font-list":"object"

@CREATOR ();

@ "find-or-create-font" : wxFont! FindOrCreateFont(nnint,SYM[family],SYM[style],SYM[weight],bool=0) <> family id
@ "find-or-create-font" : wxFont! FindOrCreateFont(nnint,cstring,SYM[family],SYM[style],SYM[weight],bool=0) <> font name ## USE_FONT_NAME_DIRECTORY

@CONSTANT "the-font-list" : wxFontList! wxTheFontList

@END


@CLASSBASE wxColour "color" : "object"

@CREATOR (); <> no argument
@CREATOR (ubyte,ubyte,ubyte); <> rgb values
@CREATOR (string); <> color name

@ "copy-from" : wxColour% operator=(wxColour%);  : : /CHECKMUT[wxColour."color".METHODNAME("color%","copy-from")]
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

// Since we don't allow creating this anymore, need a Mac fix:
#if defined(wx_mac)
#define CDB_FIX os_wxColourDatabase(_KEY_TYPE x) : wxColourDatabase(x) {}
#else
#define CDB_FIX 
#endif

@CLASSBASE wxColourDatabase "color-database" : "object"
@INTERFACE "color-database"

@VAR CDB_FIX

@ "find-color" : wxColour^ FindColour(string);

@CONSTANT "the-color-database" : wxColourDatabase! wxTheColourDatabase

@END


@CLASSBASE wxPoint "point" : "object"

@CREATOR (); <> no argument
@CREATOR (float,float); <> xy values

@IVAR "x" : float x
@IVAR "y" : float y

@END

@BEGINSYMBOLS brushStyle > ONE
@SYM "transparent" : wxTRANSPARENT
@SYM "solid" : wxSOLID
@SYM "bdiagonal-hatch" : wxBDIAGONAL_HATCH
@SYM "crossdiag-hatch" : wxCROSSDIAG_HATCH
@SYM "fdiagonal-hatch" : wxFDIAGONAL_HATCH
@SYM "cross-hatch" : wxCROSS_HATCH
@SYM "horizontal-hatch" : wxHORIZONTAL_HATCH
@SYM "vertical-hatch" : wxVERTICAL_HATCH
@SYM "stipple" : wxSTIPPLE
@SYM "opaque-stipple" : wxOPAQUE_STIPPLE
@ENDSYMBOLS

@CLASSBASE wxBrush "brush" : "object"

@CREATOR (); <> no argument
@CREATOR (wxColour%,SYM[brushStyle]); <> color%
@CREATOR (string,SYM[brushStyle]); <> color name

@ "get-color" : wxColour% GetColour();
@ "set-color" : void SetColour(wxColour%); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-colour")] <> color%
@ "set-color" : void SetColour(string); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-colour")] <> color name
@ "set-color" : void SetColour(int,int,int); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-colour")] <> rgb values

@ "get-stipple" : wxBitmap! GetStipple();
@ "set-stipple" : void SetStipple(wxBitmap^); : : /CHECKVOIDABLEOK[0]|CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-stipple")]

@ "get-style" : SYM[brushStyle] GetStyle();
@ "set-style" : void SetStyle(SYM[brushStyle]); : : /CHECKMUT[wxBrush."brush".METHODNAME("brush%","set-style")]

@END

@CLASSBASE wxBrushList "brush-list" : "object"

@CREATOR ();

@ "find-or-create-brush" : wxBrush! FindOrCreateBrush(wxColour!,SYM[brushStyle]); <> color%
@ "find-or-create-brush" : wxBrush^ FindOrCreateBrush(string,SYM[brushStyle]); <> color name

@CONSTANT "the-brush-list" : wxBrushList! wxTheBrushList

@END

@BEGINSYMBOLS penStyle > ONE
@SYM "transparent" : wxTRANSPARENT
@SYM "solid" : wxSOLID
@SYM "dot" : wxDOT
@SYM "long-dash" : wxLONG_DASH
@SYM "short-dash" : wxSHORT_DASH
@SYM "dot-dash" : wxDOT_DASH
@SYM "stipple" : wxSTIPPLE
@ENDSYMBOLS

@BEGINSYMBOLS join > ONE
@SYM "bevel" : wxJOIN_BEVEL
@SYM "miter" : wxJOIN_MITER
@SYM "round" : wxJOIN_ROUND
@ENDSYMBOLS

@BEGINSYMBOLS cap > ONE
@SYM "round" : wxCAP_ROUND
@SYM "projecting" : wxCAP_PROJECTING
@SYM "butt" : wxCAP_BUTT
@ENDSYMBOLS

@CLASSBASE wxPen "pen" : "object"

@CREATOR (); <> no argument
@CREATOR (wxColour%,nnint,SYM[penStyle]); <> color%
@CREATOR (string,nnint,SYM[penStyle]); <> color name

@ "get-width" : int GetWidth();
@ "set-width" : void SetWidth(int);
@ "get-cap" : SYM[cap] GetCap();
@ "set-cap" : void SetCap(SYM[cap]);
@ "get-join" : SYM[join] GetJoin();
@ "set-join" : void SetJoin(SYM[join]);

@ "get-color" : wxColour% GetColour();
@ "set-color" : void SetColour(wxColour%);  : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-colour")] <> color%
@ "set-color" : void SetColour(string);  : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-colour")] <> color name
@ "set-color" : void SetColour(int,int,int);  : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-colour")] <> rgb values

@ "get-stipple" : wxBitmap! GetStipple();
@ "set-stipple" : void SetStipple(wxBitmap^); : : /CHECKVOIDABLEOK[0]|CHECKMUT[wxPen."pen".METHODNAME("pen%","set-stipple")]

@ "get-style" : SYM[penStyle] GetStyle();
@ "set-style" : void SetStyle(SYM[penStyle]); : : /CHECKMUT[wxPen."pen".METHODNAME("pen%","set-style")]

@END


@CLASSBASE wxPenList "pen-list" : "object"

@CREATOR ();

@ "find-or-create-pen" : wxPen! FindOrCreatePen(wxColour!,nnint,SYM[penStyle]); <> color%
@ "find-or-create-pen" : wxPen^ FindOrCreatePen(string,nnint,SYM[penStyle]); <> color name

@CONSTANT "the-pen-list" : wxPenList! wxThePenList

@END

@BEGINSYMBOLS cursor > ONE
@SYM "arrow" : wxCURSOR_ARROW
@SYM "bullseye" : wxCURSOR_BULLSEYE
// @SYM "char" : wxCURSOR_CHAR
@SYM "cross" : wxCURSOR_CROSS
@SYM "hand" : wxCURSOR_HAND
@SYM "ibeam" : wxCURSOR_IBEAM
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
// @SYM "sizenesw" : wxCURSOR_SIZENESW
// @SYM "sizens" : wxCURSOR_SIZENS
// @SYM "sizenwse" : wxCURSOR_SIZENWSE
// @SYM "sizewe" : wxCURSOR_SIZEWE
// @SYM "sizing" : wxCURSOR_SIZING
// @SYM "spraycan" : wxCURSOR_SPRAYCAN
// @SYM "wait" : wxCURSOR_WAIT
@SYM "watch" : wxCURSOR_WATCH
@ENDSYMBOLS

@CLASSBASE wxCursor "cursor" : "object"

@CREATOR (string,SYM[bitmapType]=0,int=0,int=0); <> cursor name
@CREATOR (SYM[cursor]); <> cursor id

@ "ok?" : bool Ok();

@END


#if USE_FONT_NAME_DIRECTORY

static inline int Identity(wxFontNameDirectory *, int v)
{
  return v;
}

@CLASSBASE wxFontNameDirectory "font-name-directory":"object"
@INTERFACE "font-name-directory"

@ "get-screen-name" : nstring GetScreenName(int,SYM[weight],SYM[style]);
@ "get-post-script-name" : nstring GetPostScriptName(int,SYM[weight],SYM[style]);
@ "get-afm-name" : nstring GetAFMName(int,SYM[weight],SYM[style]);

@ "get-new-font-id" :   int GetNewFontId()
@ "initialize" : void Initialize(int,SYM[family],string);

@ "get-font-id" : int GetFontId(string);
@ "get-face-name" : nstring GetFontName(int);
@ "get-family" : SYM[family] GetFamily(int);

@ "find-or-create-font-id" : int FindOrCreateFontId(cstring,SYM[family]);
@ m "find-family-default-font-id" : int Identity(SYM[family]);

@CONSTANT "the-font-name-directory" : wxFontNameDirectory% wxTheFontNameDirectory

@END

#endif
