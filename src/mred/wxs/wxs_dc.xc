
@INCLUDE prefix.xci

#include "wx_dccan.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"
#ifdef wx_msw
#include "wx_mf.h"
#endif
#include "wx_types.h"
#ifdef wx_mac
#include "wx_dcpr.h"
#endif
#include "wx_rgn.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS textMode > ONE > PRED
@SYM "transparent" : wxTRANSPARENT
@SYM "solid" : wxSOLID
@ENDSYMBOLS

@BEGINSYMBOLS bitmapDrawStyle > ONE > PRED BUNDLE
@SYM "solid" : wxSOLID
@SYM "opaque" : wxSTIPPLE
@SYM "xor" : wxXOR
@ENDSYMBOLS

@INCLUDE wxs_drws.xci

@MACRO CastToSO = (Scheme_Object*){x}
@MACRO spAnything = _

static wxColour* dcGetTextBackground(wxDC *dc)
{
  wxColour *c = NULL, *bg = NULL;
  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, dc);
  VAR_STACK_PUSH(1, c);

  c = WITH_VAR_STACK(new wxColour());
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(c->gcInit_wxColour());
#endif
  bg = WITH_VAR_STACK(dc->GetTextBackground());
  WITH_VAR_STACK(c->CopyFrom(bg));
  READY_TO_RETURN;
  return c;
}

static wxColour* dcGetTextForeground(wxDC *dc)
{
  wxColour *c = NULL, *fg = NULL;
  SETUP_VAR_STACK(2);
  VAR_STACK_PUSH(0, dc);
  VAR_STACK_PUSH(1, c);

  c = WITH_VAR_STACK(new wxColour());
#ifdef MZ_PRECISE_GC
  WITH_VAR_STACK(c->gcInit_wxColour());
#endif
  fg = WITH_VAR_STACK(dc->GetTextForeground());
  WITH_VAR_STACK(c->CopyFrom(fg));
  READY_TO_RETURN;
  return c;
}

static Bool DrawBitmap(wxDC *dc, wxBitmap *bm, float x, float y, int mode, wxColour *c, wxBitmap* mask)
{
  REMEMBER_VAR_STACK();
  if (bm->Ok()) {
    return WITH_REMEMBERED_STACK(dc->Blit(x, y, bm->GetWidth(), bm->GetHeight(), bm, 0, 0, mode, c, mask));
  } else
    return FALSE;
}

static Bool DrawBitmapRegion(wxDC *dc, wxBitmap *bm, float x, float y, float dx, float dy, float dw, float dh, int mode, wxColour *c, wxBitmap* mask)
{
  REMEMBER_VAR_STACK();
  if (bm->Ok()) {
    return WITH_REMEMBERED_STACK(dc->Blit(x, y, dw, dh, bm, dx, dy, mode, c, mask));
  } else
    return FALSE;
}

static void* MyTextExtent(wxDC *dc, char *s, wxFont *f, Bool big, int offset)
{
  float w, h, d, asc;
  Scheme_Object *a[4];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 4);

  a[0] = a[1] = a[2] = a[3] = NULL;

  WITH_VAR_STACK(dc->GetTextExtent(s, &w, &h, &d, &asc, f, big, offset));
    
  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));
  a[2] = WITH_VAR_STACK(scheme_make_double(d));
  a[3] = WITH_VAR_STACK(scheme_make_double(asc));

  r = WITH_VAR_STACK(scheme_values(4, a));

  READY_TO_RETURN;

  return r;
}

static void* MyGetSize(wxDC *dc)
{
  float w, h;
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  dc->GetSize(&w, &h); /* no GC possible */

  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));

  r = WITH_VAR_STACK(scheme_values(2, a));

  READY_TO_RETURN;

  return r;
}

static void* MyGetScale(wxDC *dc)
{
  float w, h;
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  dc->GetUserScale(&w, &h); /* no GC possible */

  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));

  r = WITH_VAR_STACK(scheme_values(2, a));

  READY_TO_RETURN;

  return r;
}

static void* MyGetOrigin(wxDC *dc)
{
  float w, h;
  Scheme_Object *a[2];
  void *r;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH_ARRAY(0, a, 2);

  a[0] = a[1] = NULL;

  dc->GetDeviceOrigin(&w, &h); /* no GC possible */

  a[0] = WITH_VAR_STACK(scheme_make_double(w));
  a[1] = WITH_VAR_STACK(scheme_make_double(h));

  r = WITH_VAR_STACK(scheme_values(2, a));

  READY_TO_RETURN;

  return r;
}

static void dcGetARGBPixels(wxMemoryDC *dc, float x, float y, int w, int h, char *s)
{
  int i, j, p;
  unsigned char *ss = (unsigned char *)s;
  wxColour *c;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, ss);
  VAR_STACK_PUSH(1, c);
  VAR_STACK_PUSH(2, dc);

  c = new wxColour();
  
  p = 0;

  for (i = 0; i < w; i++) {
    for (j = 0; j < h; j++) {
      WITH_VAR_STACK(dc->GetPixel(x + i, y + j, c));
      ss[p++] = 255; /* alpha */
      ss[p++] = c->Red();
      ss[p++] = c->Green();
      ss[p++] = c->Blue();
    }
  }

  READY_TO_RETURN;
}

static void dcSetARGBPixels(wxMemoryDC *dc, float x, float y, int w, int h, char *s)
{
  int i, j, p;
  unsigned char *ss = (unsigned char *)s;
  wxColour *c;
  SETUP_VAR_STACK(3);
  VAR_STACK_PUSH(0, ss);
  VAR_STACK_PUSH(1, c);
  VAR_STACK_PUSH(2, dc);

  c = new wxColour();
  
  p = 0;

  for (i = 0; i < w; i++) {
    for (j = 0; j < h; j++) {
      WITH_VAR_STACK(c->Set(ss[p+1], ss[p+2], ss[p+3]));
      WITH_VAR_STACK(dc->SetPixel(x + i, y + j, c));
      p += 4;
    }
  }

  READY_TO_RETURN;
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static wxBitmap *dc_target(Scheme_Object *obj)
{
  wxDC *dc;
  dc = (wxDC *)((Scheme_Class_Object *)obj)->primdata;
  if (dc->__type == wxTYPE_DC_MEMORY) {
    wxBitmap *bm;
    bm = ((wxMemoryDC *)dc)->GetObject();
    if (bm)
      return bm;
  }
  return (wxBitmap *)0x1; /* dont't return NULL because that matches unspecified mask */
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

@MACRO CheckStringIndex[n.s.i] = if (x<i> > SCHEME_STRLEN_VAL(p[POFFSET+<s>])) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("dc<%>",<n>), "string index too large: ", p[POFFSET+<i>]));

@CLASSBASE wxDC "dc":"object"
@INTERFACE "dc"

@CLASSID wxTYPE_DC

@SETMARK Q = U
@INCLUDE wxs_draw.xci

// Also in wxWindow:
@ m "get-text-extent" : void[]/CastToSO//spAnything MyTextExtent(string,wxFont^=NULL,bool=FALSE,nnint=0); : : /CheckStringIndex["get-text-extent".0.3]
@ Q "get-char-height" : float GetCharHeight();
@ Q "get-char-width" : float GetCharWidth();

@MACRO rZERO = return 0;
@MACRO rFALSE = return FALSE;

#ifndef wx_mac
#define HIDETHISSTATEMENT(x) x
#else
#define HIDETHISSTATEMENT(x) 
#endif

@MACRO IFNOTMAC = HIDETHISSTATEMENT(
@MACRO ENDIF = )

#ifndef wx_mac
#define CHECKTHISONE(x) x
#else
#define CHECKTHISONE(x) 1
#endif

@MACRO CheckBMOk[p.who] = if (x<p> && !(x<p>->Ok())) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap is not ok: ", p[POFFSET+<p>]));
@MACRO old_CheckBW[p.who] = if (x<p> && (x<p>->GetDepth() != 1)) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap is not monochrome: ", p[POFFSET+<p>]));
@MACRO CheckBW[p.who] = 
@MACRO CheckSizes[p.m.who] = if (x<m> && ((x<p>->GetWidth() != x<m>->GetWidth()) || (x<p>->GetHeight() != x<m>->GetHeight()))) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap size does not match bitmap to draw: ", p[POFFSET+<p>]));
@MACRO CheckNotSame[p.m.who] = if (WITH_VAR_STACK(dc_target(THEOBJ)) == x<p>) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "source bitmap is the same as the destination: ", p[POFFSET+<p>])); if (WITH_VAR_STACK(dc_target(THEOBJ)) == x<m>) WITH_VAR_STACK(scheme_arg_mismatch(<who>, "mask bitmap is the same as the destination: ", p[POFFSET+<m>]));

@ m "draw-bitmap-section" : bool DrawBitmapRegion(wxBitmap!,float,float,float,float,nnfloat,nnfloat,SYM[bitmapDrawStyle]=wxSOLID,wxColour!=NULL,wxBitmap^=NULL); : : /CheckBMOk[9.METHODNAME("dc<%>","draw-bitmap-section")]|CheckBW[9.METHODNAME("dc<%>","draw-bitmap-section")]|CheckSizes[0.9.METHODNAME("dc<%>","draw-bitmap-section")]|CheckOk[METHODNAME("dc<%>","draw-bitmap-section")]|CheckNotSame[0.9.METHODNAME("dc<%>","draw-bitmap-section")] : : rFALSE <> with size
@ m "draw-bitmap" : bool DrawBitmap(wxBitmap!,float,float,SYM[bitmapDrawStyle]=wxSOLID,wxColour!=NULL,wxBitmap^=NULL); : : /CheckBMOk[5.METHODNAME("dc<%>","draw-bitmap")]|CheckBW[5.METHODNAME("dc<%>","draw-bitmap")]|CheckSizes[0.5.METHODNAME("dc<%>","draw-bitmap")]|CheckOk[METHODNAME("dc<%>","draw-bitmap")]|CheckNotSame[0.5.METHODNAME("dc<%>","draw-bitmap")]

@ Q "try-color" : void TryColour(wxColour!,wxColour!); : : /CheckOk[METHODNAME("dc<%>","try-color")]

@ Q "set-text-mode" : void SetBackgroundMode(SYM[textMode]); :  : /CheckOk[METHODNAME("dc<%>","set-text-mode")]
@ Q "set-scale" : void SetUserScale(nnfloat,nnfloat); : : /CheckOk[METHODNAME("dc<%>","set-scale")]
@ Q "set-origin" : void SetDeviceOrigin(float,float); : : /CheckOk[METHODNAME("dc<%>","set-origin")]

@ m "get-scale" : void[]/CastToSO//spAnything MyGetScale(); : : /CheckOk[METHODNAME("dc<%>","get-scale")]
@ m "get-origin" : void[]/CastToSO//spAnything MyGetOrigin(); : : /CheckOk[METHODNAME("dc<%>","get-origin")]

@ q "get-background" : wxColour! GetBackground(); : : /CheckOk[METHODNAME("dc<%>","get-background")]
@ q "get-text-mode" : SYM[textMode] GetBackgroundMode(); : : /CheckOk[METHODNAME("dc<%>","get-text-mode")]
@ q "get-brush" : wxBrush! GetBrush(); : : /CheckOk[METHODNAME("dc<%>","get-brush")]
@ q "get-font" : wxFont! GetFont(); : : /CheckOk[METHODNAME("dc<%>","get-font")]
@ q "get-pen" : wxPen! GetPen(); : : /CheckOk[METHODNAME("dc<%>","get-pen")]
@ m "get-text-background" : wxColour! dcGetTextBackground(); : : /CheckOk[METHODNAME("dc<%>","get-text-background")]
@ m "get-text-foreground" : wxColour! dcGetTextForeground(); : : /CheckOk[METHODNAME("dc<%>","get-text-foreground")]

@ m "get-size" : void[]/CastToSO//spAnything MyGetSize(); : : /CheckOk[METHODNAME("dc<%>","get-size")]

@ q "ok?" : bool Ok();

@ Q "start-doc" : bool StartDoc(string); : : /CheckOk[METHODNAME("dc<%>","start-doc")] : rFALSE
@ Q "start-page" : void StartPage(); : : /CheckOk[METHODNAME("dc<%>","start-page")]
@ Q "end-doc" : void EndDoc(); : : /CheckOk[METHODNAME("dc<%>","end-doc-line")]
@ Q "end-page" : void EndPage(); : : /CheckOk[METHODNAME("dc<%>","end-page")]

@END

@MACRO STRINGENOUGH[who] = if (SCHEME_STRTAG_VAL(p[4+POFFSET]) < (x2 * x3 * 4)) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap%",<who>), "string too short: ", p[4+POFFSET]));

@CLASSBASE wxMemoryDC "bitmap-dc":"dc"

@CLASSID wxTYPE_DC_MEMORY

@CREATOR ()
@ARGNAMES

@ "get-pixel" : bool GetPixel(float,float,wxColour^) : : /CheckOk[METHODNAME("memory-dc%","get-pixel")]
@ "set-pixel" : void SetPixel(float,float,wxColour^) : : /CheckOk[METHODNAME("memory-dc%","set-pixel")]

@ m "get-argb-pixels" : void dcGetARGBPixels(float,float,rint[0|10000],rint[0|10000],string) : : /CheckOk[METHODNAME("memory-dc%","get-argb-pixels")]|STRINGENOUGH["get-argb-pixels"]
@ m "set-argb-pixels" : void dcSetARGBPixels(float,float,rint[0|10000],rint[0|10000],string) : : /CheckOk[METHODNAME("memory-dc%","set-argb-pixels")]|STRINGENOUGH["set-argb-pixels"]

@ "set-bitmap" : void SelectObject(wxBitmap^);  : : /CHECKOKFORDC[0.METHODNAME("memory-dc%","set-bitmap")]
@ "get-bitmap" : wxBitmap^ GetObject();

@END

@CLASSBASE wxPostScriptDC "post-script-dc":"dc"

@CLASSID wxTYPE_DC_POSTSCRIPT

@INCLUDE wxs_dorf.xci

@CREATOR (bool=TRUE,wxWindow^=NULL,bool=FALSE) : : /DLGORFRAME[1.METHODNAME("post-script-dc%","initialization")]
@ARGNAMES [interactive? #t] [parent #f] [use-paper-bbox? #f]

@END

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

#ifdef wx_x

class basePrinterDC : public wxObject
{
public:
  basePrinterDC(wxWindow *w);
};

basePrinterDC::basePrinterDC(wxWindow *)
{
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "%s", 
		   METHODNAME("printer-dc%","initialization")": not supported for X Windows");
}

#else

class basePrinterDC : public wxPrinterDC
{
public:
  basePrinterDC(wxWindow *w);
};

basePrinterDC::basePrinterDC(wxWindow *w) 
: wxPrinterDC( )
{
}

#endif

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

@CLASSBASE basePrinterDC "printer-dc":"dc"

@CLASSID wxTYPE_DC_PRINTER

@CREATOR (wxWindow^=NULL); : : /DLGORFRAME[0.METHODNAME("printer-dc%","initialization")]
@ARGNAMES [parent #f]

@END




#if 0

#ifdef wx_msw

class baseMetaFileDC : public wxMetaFileDC {
public:
  baseMetaFileDC(char *s = NULL);

  baseMetaFile* baseClose() { return (baseMetaFile *)Close(); }
};

baseMetaFileDC::baseMetaFileDC(char *s)
    : wxMetaFileDC(s)
{
}

#else

class baseMetaFileDC : public wxObject 
{
public:
  baseMetaFileDC(char * = NULL) {
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "%s", 
		     METHODNAME("meta-file-dc%","initialization")": only supported for Windows");
  }

  baseMetaFile* baseClose() { return NULL; }
};

#endif

@CLASSBASE baseMetaFileDC "meta-file-dc":"dc"

@CLASSID wxTYPE_DC_METAFILE

@CREATOR (string=NULL);
@ARGNAMES [filename #f]

@ "close" : baseMetaFile! baseClose()

@END

#endif
