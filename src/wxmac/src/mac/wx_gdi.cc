///////////////////////////////////////////////////////////////////////////////
// File:	wx_gdi.cc (Macintosh version)
// Purpose:	GDI (Graphics Device Interface) objects and functions
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_list.h"
#include "wx_utils.h"
#include "wx_gdi.h"
#include "wx_dcmem.h"
#ifndef OS_X
# include <Strings.h>
# include <Resources.h>
# include <QDOffscreen.h>
#endif
#if USE_XPM_IN_MAC
# define FOR_MAC
# include "xpm34.h"
#endif
#if USE_IMAGE_LOADING_IN_MAC
# include "wx_image.h"
#endif

CGrafPtr gMacFontGrafPort = NULL; // mac platform only

#define PLAIN_MALLOC_FOR_XPM

#if 1
# define RECORD(w, p) p
#else
static void *RECORD(const char *where, void *p)
{
	printf("%s %lx\n", where, (long)p);
	return p;
}
#endif

void *XpmMalloc(size_t size)
{
#ifdef PLAIN_MALLOC_FOR_XPM
  return RECORD("m", malloc(size));
#else
  return new char[size];
#endif
}

void *XpmMallocA(size_t size)
{
#ifdef PLAIN_MALLOC_FOR_XPM
  return RECORD("m", malloc(size));
#else
  return new WXGC_ATOMIC char[size];
#endif
}

static void *DoXpmRealloc(void *(*alloc)(size_t), void *ptr, size_t size)
{
#ifdef PLAIN_MALLOC_FOR_XPM
  return RECORD("r", realloc(ptr, size));
#else
  void *naya;
  size_t osize;
  
  naya = alloc(size);
  
  osize = GC_size(ptr);
  if (size < osize)
    osize = size;
  
  memcpy(naya, ptr, osize);
  
  return naya;
#endif
}

void *XpmRealloc(void *ptr, size_t size)
{
  return DoXpmRealloc(XpmMalloc, ptr, size);
}

void *XpmReallocA(void *ptr, size_t size)
{
  return DoXpmRealloc(XpmMallocA, ptr, size);
}

void *XpmCallocA(size_t nelem, size_t elsize)
{
#ifdef PLAIN_MALLOC_FOR_XPM
  return RECORD("c", calloc(nelem, elsize));
#else
  void *v = XpmMallocA(nelem * elsize);
  memset(v, 0, nelem * elsize);
  return v;
#endif
}

void *XpmCalloc(size_t nelem, size_t elsize)
{
#ifdef PLAIN_MALLOC_FOR_XPM
  return RECORD("c", calloc(nelem, elsize));
#else
  return XpmMalloc((nelem) * (elsize))
#endif
}

void XpmFree(void *ptr)
{
#ifdef PLAIN_MALLOC_FOR_XPM
  free(RECORD("f", ptr));
#else
  /* Do nothing */
#endif
}

#ifdef wx_xview

/* These cursors courtesy of xfig
 */

static unsigned short    bull_cursor_array[16] = {
  0x0F00, 0x30C0, 0x4020, 0x4020, 0x8010, 0x8610, 0x8610, 0x8010,
  0x4020, 0x4020, 0x30C0, 0x0F00, 0x0000, 0x0000, 0x0000, 0x0000
};

static unsigned short    char_cursor_data[16] = {
  0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00,
  0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00, 0xFF00,
};

static unsigned short    crosshair_cursor_data[16] = {
  0x1000, 0x1000, 0x1000, 0xFE00, 0x1000, 0x1000, 0x1000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
};

static unsigned short    magnifier_cursor_array[16] = {
  0x0F80, 0x3060, 0x4010, 0x4010, 0x8008, 0x8008, 0x8008, 0x8008,
  0x8008, 0x4010, 0x4010, 0x3078, 0x0F9C, 0x000E, 0x0007, 0x0003,
};

static unsigned short    pencil_cursor_array[16] = {
  0x0000, 0x0018, 0x0024, 0x0075, 0x009B, 0x0117, 0x022E, 0x045C,
  0x08B8, 0x1170, 0x22E0, 0x25C0, 0x7B80, 0x6700, 0x8600, 0x0800,
};

static unsigned short    vbar_cursor_array[16] = {
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
};

static unsigned short hand_cursor_array[] =
{
  0x0C00,0x1200,0x1200,0x1380,0x1240,0x7270,0x9248,0x924E,
  0x9249,0x9249,0x9009,0x8001,0x4002,0x4002,0x2004,0x2004
};
#endif

//-----------------------------------------------------------------------------
wxFont::wxFont(void)
{
  Create(10, wxDEFAULT, 
	 wxDEFAULT, 
	 wxNORMAL, wxNORMAL, FALSE);
}

//-----------------------------------------------------------------------------
// Constructor for a font. Note that the real construction is done
// in wxDC::SetFont, when information is available about scaling etc.
//-----------------------------------------------------------------------------
wxFont::wxFont(int PointSize, int FontOrFamilyId, int Style, int Weight, Bool Underlined)
{
  Create(PointSize, FontOrFamilyId, 
	 wxTheFontNameDirectory->GetFamily(FontOrFamilyId), 
	 Style, Weight, Underlined);
}

wxFont::wxFont(int PointSize, const char *Face, int Family, int Style, int Weight, 
	       Bool underlined)
{
  int id = wxTheFontNameDirectory->FindOrCreateFontId(Face, Family);
  int fam = wxTheFontNameDirectory->GetFamily(id);
  
  Create(PointSize, id, fam, Style, Weight, underlined);
}

void wxFont::Create(int PointSize, int Font, int Family, int Style, int Weight, 
		    Bool Underlined)
{
  fontid = Font;
  family = Family;
  style = Style;
  weight = Weight;
  point_size = PointSize;
  underlined = Underlined;

  int tried_once = 0;

  while (1) {
    char *name = wxTheFontNameDirectory->GetScreenName(Font, Weight, Style);
    Str255 buffer;
    
    if (!strcmp(name, "systemfont")) {
      macFontId = GetSysFont();
      break;
    } else if (!strcmp(name, "applicationfont")) {
      macFontId = GetAppFont();
      break;
    } else {
      CopyCStringToPascal(name,buffer);
      ::GetFNum((ConstStr255Param)buffer, &macFontId);

      if (macFontId || tried_once)
	break;
      else {
	/* Try again with family... */
	Font = Family;
	tried_once = 1;
      }
    }
  }

#if !WXGARBAGE_COLLECTION_ON
  wxTheFontList->Append(this);
#endif
}

//-----------------------------------------------------------------------------
wxFont::~wxFont()
{
#if !WXGARBAGE_COLLECTION_ON
  wxTheFontList->DeleteObject(this);
#endif
}

static long TextFontInfo(int font, int size, int face, FontInfo *finfo, char *str)
{
  static int fn, sz = -1, fc;
  static FontInfo fontInfo;
  long result = 0;
  int isdiff = ((fn != font)
                || (sz != size)
                || (fc != face));
  
  if (str || isdiff) {
    CGrafPtr savep;
    GDHandle savegd;
    
    ::GetGWorld(&savep, &savegd);  
    ::SetGWorld(gMacFontGrafPort, wxGetGDHandle());

    if (isdiff) {
      ::TextFont(fn = font);
      ::TextSize(sz = size);
      ::TextFace(fc = face);
    }
    
    ::GetFontInfo(&fontInfo);
    
    if (str)
      result = TextWidth(str, 0, strlen(str));
    
    ::SetGWorld(savep, savegd);
  }
  
  memcpy(finfo, &fontInfo, sizeof(FontInfo));
  
  return result;
}

//-----------------------------------------------------------------------------
float wxFont::GetCharHeight(void)
{
  FontInfo fontInfo;
  ::TextFontInfo(GetMacFontNum(),
		 point_size,
		 GetMacFontStyle(),
		 &fontInfo, NULL);
  return fontInfo.ascent + fontInfo.descent;
}

//-----------------------------------------------------------------------------
float wxFont::GetCharWidth(void)
{
  FontInfo fontInfo;
  ::TextFontInfo(GetMacFontNum(),
		 point_size,
		 GetMacFontStyle(),
		 &fontInfo, NULL);
  return fontInfo.widMax;
}

//-----------------------------------------------------------------------------
void wxFont::GetTextExtent(char* string, float* x, float* y,
			   float* descent, float* externalLeading, Bool use16)
{
  FontInfo fontInfo;
  *x = ::TextFontInfo(GetMacFontNum(),
		      point_size,
		      GetMacFontStyle(),
		      &fontInfo, string);
  *y = fontInfo.ascent + fontInfo.descent; // height
  if (descent) *descent = fontInfo.descent;
  if (externalLeading) *externalLeading = fontInfo.leading;
}

//-----------------------------------------------------------------------------
int wxFont::GetMacFontNum(void) // mac platform only
{
  return macFontId;
}

//-----------------------------------------------------------------------------
Style wxFont::GetMacFontStyle(void) // mac platform only
{
  Style result = 0;
  if (weight == wxBOLD)
    result |= bold;
  if (style == wxITALIC || style == wxSLANT) 
    result |= italic;
  if (underlined) 
    result |= underline;
  return result;
}



/*
 * Colour map
 *
 */

//-----------------------------------------------------------------------------
wxColourMap::wxColourMap(void)
{
#ifdef wx_x
  cmap = 0;
#endif
}

//-----------------------------------------------------------------------------
wxColourMap::~wxColourMap(void)
{
}


// Pens

//-----------------------------------------------------------------------------
wxPen::wxPen(void)
{
  wxColour *c;
  
  c = new wxColour(wxBLACK);
  c->Lock(1);
  colour = c;
  
  stipple = NULL ;
  style = wxSOLID;
  join = wxJOIN_ROUND ;
  cap = wxCAP_ROUND ;
  nb_dash = 0 ;
  dash = NULL ;
  width = 1;

#if !WXGARBAGE_COLLECTION_ON
  wxThePenList->AddPen(this);
#endif
}

//-----------------------------------------------------------------------------
wxPen::~wxPen()
{
#if !WXGARBAGE_COLLECTION_ON
  wxThePenList->RemovePen(this);
#endif
}

//-----------------------------------------------------------------------------
wxPen::wxPen(wxColour *col, float Width, int Style):
wxbPen(col, Width, Style)
{
  wxColour *c;
  
  c = new wxColour(col);
  c->Lock(1);
  colour = c;
  
  stipple = NULL ;
  width = Width;
  style = Style;
  join = wxJOIN_ROUND ;
  cap = wxCAP_ROUND ;
  nb_dash = 0 ;
  dash = NULL ;
}

//-----------------------------------------------------------------------------
wxPen::wxPen(char *col, float Width, int Style):
wxbPen(col, Width, Style)
{
  wxColour *c;
  
  c = new wxColour(col);
  c->Lock(1);
  colour = c;
  
  stipple = NULL ;
  width = Width;
  style = Style;
  join = wxJOIN_ROUND ;
  cap = wxCAP_ROUND ;
  nb_dash = 0 ;
  dash = NULL ;
}

// Brushes

//-----------------------------------------------------------------------------
wxBrush::wxBrush(void)
{
  wxColour *c;
  
  c = new wxColour(wxBLACK);
  c->Lock(1);
  
  colour = c;
  style = wxSOLID;
  stipple = NULL ;
}

//-----------------------------------------------------------------------------
wxBrush::~wxBrush()
{
}

//-----------------------------------------------------------------------------
wxBrush::wxBrush(wxColour *col, int Style)
{
  wxColour *c;
  
  c = new wxColour(col);
  c->Lock(1);
  colour = c;
  
  style = Style;
  stipple = NULL ;
}

//-----------------------------------------------------------------------------
wxBrush::wxBrush(char *col, int Style)
{
  wxColour *c;
  
  c = new wxColour(col);
  c->Lock(1);
  colour = c;
  
  style = Style;
  stipple = NULL ;
}

// Cursors

//-----------------------------------------------------------------------------
wxCursor::wxCursor(void)
{
  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;
}

//-----------------------------------------------------------------------------
wxCursor::wxCursor(char bits[], int width, int height, int depth)
{
  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;
}

//-----------------------------------------------------------------------------
static wxMemoryDC *temp_mdc;
static wxMemoryDC *temp_mask_mdc;

wxCursor::wxCursor(wxBitmap *mask, wxBitmap *bm, int hotSpotX, int hotSpotY)
{
  int w, h, bw, bh, i, j, bit;
  unsigned char r, g, b;
  wxColour *c;
  wxMemoryDC *mask_dc, *bitmap_dc;

  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;
  cMacCustomCursor = NULL;

  /* Get the allowed size for cursors: */
  w = 16;
  h = 16;

  bw = bm->GetWidth();
  bh = bm->GetHeight();

  /* If the given cursor doesn't fit exactly, give up. (MrEd constrains the
     bitmap to be 16x16, which surely will fit.) */
  if ((bw > w) || (bh > h))
    return;

  if ((hotSpotX > w) || (hotSpotY > h)) 
    return;

  /* Make read-only DCs for reading bits from the bitmaps: */
  if (!temp_mdc) {
    wxREGGLOB(temp_mdc);
    wxREGGLOB(temp_mask_mdc);
    temp_mdc = new wxMemoryDC(1);
    temp_mask_mdc = new wxMemoryDC(1);
  }
  
  bitmap_dc = temp_mdc;
  
  bitmap_dc->SelectObject(bm);
  /* Might fail, so we double-check: */
  if (!bitmap_dc->GetObject())
    return;
  /* If bm and mask are the same, use one DC (since re-selecting
     will fail, anyway). */
  if (mask == bm) {
    mask_dc = bitmap_dc;
  } else {
    mask_dc = temp_mask_mdc;
    mask_dc->SelectObject(mask);
    if (!mask_dc->GetObject()) {
      bitmap_dc->SelectObject(NULL);
      return;
    }
  }

  c = new wxColour(); /* to recieve bit values */

  cMacCustomCursor = new Cursor;

  /* Init arrays */
  for (i = 0; i < h; i++) {
    cMacCustomCursor->data[i] = 0;
    cMacCustomCursor->mask[i] = 0;
  }

  /* Read bits from mask and bm and set the corresponding bits */
  for (j = 0; j < bh; j++) {
    bit = 0x8000;
    for (i = 0; i < w; i++) {
      if (i < bw) {
	
        // transfer bm pixel
        bitmap_dc->GetPixel(i, j, c);
	c->Get(&r, &g, &b);

        if (!r && !g && !b) {
	  cMacCustomCursor->data[j] += bit;
        }
        
        mask_dc->GetPixel(i, j, c);
        c->Get(&r, &g, &b);
        
        if (!r && !g && !b) {
	  cMacCustomCursor->mask[j] += bit;
        }
      }
      bit = bit >> 1;
    }
  }

  cMacCustomCursor->hotSpot.h = hotSpotX;
  cMacCustomCursor->hotSpot.v = hotSpotY;
  
  /* Clean up */
  bitmap_dc->SelectObject(NULL);
  mask_dc->SelectObject(NULL);

}

//-----------------------------------------------------------------------------
// Cursors by stock number
//-----------------------------------------------------------------------------
wxCursor::wxCursor(int cursor_type)
{
  __type = wxTYPE_CURSOR;
  cMacCursor = NULL;
  cMacCustomCursor = NULL;

  switch (cursor_type)
    {
    case wxCURSOR_WAIT:
    case wxCURSOR_WATCH:
      {
	cMacCursor = GetCursor(watchCursor);
	break;
      }
    case wxCURSOR_CROSS:
      {
	cMacCursor = GetCursor(crossCursor);
	break;
      }
    case wxCURSOR_CHAR:
      {
	break;
      }
    case wxCURSOR_HAND:
      {
	break;
      }
    case wxCURSOR_BULLSEYE:
      {
	cMacCursor = GetCursor(128);
	break;
      }
    case wxCURSOR_PENCIL:
      {
	break;
      }
    case wxCURSOR_MAGNIFIER:
      {
	break;
      }
    case wxCURSOR_IBEAM:
      {
	cMacCursor = GetCursor(iBeamCursor);
	break;
      }
    case wxCURSOR_NO_ENTRY:
      {
	break;
      }

    case wxCURSOR_LEFT_BUTTON:
      {
	break;
      }
    case wxCURSOR_RIGHT_BUTTON:
      {
	break;
      }
    case wxCURSOR_MIDDLE_BUTTON:
      {
	break;
      }
    case wxCURSOR_QUESTION_ARROW:
      {
	break;
      }
    case wxCURSOR_SIZING:
      {
	break;
      }
    case wxCURSOR_SPRAYCAN:
      {
	break;
      }
    case wxCURSOR_PAINT_BRUSH:
      {
	break;
      }
    case wxCURSOR_SIZENWSE:
    case wxCURSOR_SIZENESW:
      {
	break;
      }
    case wxCURSOR_SIZEWE:
      {
	break;
      }
    case wxCURSOR_SIZENS:
      {
	break;
      }
    case wxCURSOR_POINT_LEFT:
      {
	break;
      }
    case wxCURSOR_POINT_RIGHT:
      {
	break;
      }
    default:
    case wxCURSOR_ARROW:
      {
	cMacCursor = (Cursor **)0x1;
	break;
      }
    case wxCURSOR_BLANK:
      {
	break ;
      }
    }
}

//-----------------------------------------------------------------------------
wxCursor::~wxCursor(void)
{
  if (cMacCustomCursor) {
    delete cMacCustomCursor;
  }
}

//-----------------------------------------------------------------------------
Bool wxCursor::Ok(void)
{
  return (!!cMacCursor || !!cMacCustomCursor);
}

//-----------------------------------------------------------------------------
// Global cursor setting
//-----------------------------------------------------------------------------

static wxCursor *curCursor = NULL;

void wxRegisterCurCursor();

void wxRegisterCurCursor()
{
  wxREGGLOB(curCursor);
}

static Cursor arrow_c;
static int arrow_c_inited;

void wxSetCursor(wxCursor *cursor)
{
  if (cursor != curCursor) {
    /* 0x1 is the arrow cursor */
    if (cursor) {
      if (cursor->cMacCustomCursor) {
	::SetCursor(cursor->cMacCustomCursor);
      } else {
	if (cursor->cMacCursor && (cursor->cMacCursor != (Cursor **)0x1))
	  ::SetCursor(*(cursor->cMacCursor));
	else {
	  if (!arrow_c_inited) {
	    GetQDGlobalsArrow(&arrow_c);
	    arrow_c_inited = 1;
	  }
	  ::SetCursor(&arrow_c);
	}
      }
    }
    curCursor = cursor;
  }
  wxFlushEvents();
}

// Misc. functions

//-----------------------------------------------------------------------------
// Return TRUE if we have a colour display
//-----------------------------------------------------------------------------
Bool wxColourDisplay(void)
{
  return wxDisplayDepth() > 1;
}

//-----------------------------------------------------------------------------
// Returns depth of screen
//-----------------------------------------------------------------------------
int wxDisplayDepth(void)
{
#if USE_XPM_IN_MAC
  return XDefaultDepth(NULL, NULL); // Args are not used for Mac
#else
  // code COPIED from XPM package
  int d, b;
  PixMapHandle pmap;
  GDHandle dev;
  dev = GetMainDevice();
  pmap = (**dev).gdPMap;
  b = (**pmap).pixelSize;
  return (b);
#endif
}

//-----------------------------------------------------------------------------
// Get size of display
//-----------------------------------------------------------------------------
void wxDisplaySize(int *width, int *height)
{
  BitMap screenBits;
  GetQDGlobalsScreenBits(&screenBits);
  *width = screenBits.bounds.right - screenBits.bounds.left;
  *height = screenBits.bounds.bottom - screenBits.bounds.top - GetMBarHeight();
}

/* NOTE: we rely on no garbage collection between GetGWorld()... SetGWorld()
   pairs. Otherwise, the temporarily unset GWorldPtr could be freed before
   the restoring SetGWorld(). */

static void FreeGWorld(GWorldPtr x_pixmap)
{
  /* Is this GWorld the current context? */
  GDHandle savegd;
  CGrafPtr saveport;
  GetGWorld(&saveport, &savegd);
  if (saveport == x_pixmap) {
    SetGWorld(wxGetGrafPtr(), wxGetGDHandle());
  }

  DisposeGWorld(x_pixmap);
}

//------------------ BitMaps ------------------------------------------
/*
   on the Mac, the wxBitMap needs to be a structure that will allow
   us to redraw. Externally a wxBitmap is a picture (file or resource).
   Internally, its an offscreen GWorld (and its pixmap).
   */
wxBitmap::wxBitmap(void)
{
  __type = wxTYPE_BITMAP;
  ok = FALSE;
  width = 0;
  height = 0;
  depth = 0;
  x_pixmap = NULL;
  selectedInto = NULL;
  WXGC_IGNORE(this, selectedInto);
}

//-----------------------------------------------------------------------------
wxBitmap::wxBitmap(char bits[], int the_width, int the_height)
{
  __type = wxTYPE_BITMAP;
  depth = 1;
  width = the_width;
  height = the_height;
  //Rect bounds = {0, 0, the_height, the_width};
  GDHandle savegd;
  CGrafPtr saveport;
  Create(the_width, the_height, 1);
  if (ok) {
    GetGWorld(&saveport, &savegd);
    SetGWorld(x_pixmap, 0);
    int i, j, p = 0;
    char byte;
    RGBColor	cpix;
    // look in contrib/wxwxpm/simx.c for a clue on finishing this 
    
    GetForeColor(&cpix);
    for (i = 0; i < the_height; i++) {
      for (j = 0; j < the_width; j += 8, p++) {
	byte = bits[p];
	for (int k = 0; k < 8; k++) {
	  if (byte & 1) {			
	    ::SetCPixel(j + k, i, &cpix);
	  }
	  byte = byte >> 1;
	}
      }
    }
    
    SetGWorld(saveport, savegd);
  }
  //ok = TRUE;
}

//-----------------------------------------------------------------------------
wxBitmap::wxBitmap(char *bitmap_file, long flags)
{
  __type = wxTYPE_BITMAP;
  selectedInto = NULL;
  WXGC_IGNORE(this, selectedInto);

  if (flags & wxBITMAP_TYPE_PICT_RESOURCE)	{ 
    // look for a 'PICT' resource with the given name
    Str255 resname;
    PicHandle	h;
    CopyCStringToPascal(bitmap_file,resname);
    h = (PicHandle)::GetNamedResource('PICT', resname);
    if (h) {
      depth =  wxDisplayDepth();
      width = (*h)->picFrame.right;
      height = (*h)->picFrame.bottom;
      GDHandle savegd;
      CGrafPtr saveport;
      Rect bounds = {0, 0, height, width};
      Create(width, height, depth);
      GetGWorld(&saveport, &savegd);
      SetGWorld(x_pixmap, 0);
      DrawPicture( h, &bounds);
      ::ReleaseResource((Handle)h);
      ::SetGWorld(saveport, savegd);
      return;
    }
  }
  // we also get here if we asked for a resource but it wasn't found
  x_pixmap = NULL;
  if (LoadFile(bitmap_file, flags) == FALSE) {
    char t[200];
    sprintf(t, "Could not load Bitmap: %s", bitmap_file);
    // mflatt: This is not a fatal error
    // wxFatalError(t);
    // wxError(t);
  }
  
}

//-----------------------------------------------------------------------------
// Create a new bitmap of a given size and depth
//-----------------------------------------------------------------------------
wxBitmap::wxBitmap(int w, int h, Bool bandw)
{
  Create(w, h, bandw ? 1 : -1);
}

//-----------------------------------------------------------------------------
wxBitmap::~wxBitmap(void)
{
  if (selectedInto)
    selectedInto->SelectObject(NULL);

  if (x_pixmap) {
    FreeGWorld(x_pixmap);
    x_pixmap = NULL;
  }
}

Bool wxBitmap::Create(int wid, int hgt, int deep)
{
  if (!__type) {
    __type = wxTYPE_BITMAP;
    selectedInto = NULL;
    WXGC_IGNORE(this, selectedInto);
  }
  width = wid;
  height = hgt;
  depth = deep;
  Rect bounds = {0, 0, height, width};
  // Build a offscreen GWorld to draw the Picture in
  GDHandle savegw;
  CGrafPtr saveport;
  QDErr err;
  GWorldPtr	newGWorld = NULL;
  err = NewGWorld(&newGWorld, (deep == -1) ? 32 : deep, &bounds, NULL, NULL, 0);
  if (err == noErr) {
    GetGWorld(&saveport, &savegw);
    ::LockPixels(::GetGWorldPixMap(newGWorld));
    SetGWorld(newGWorld, 0);
    if (depth < 1)
      depth = wxDisplayDepth();
    ::EraseRect(&bounds);
    ok = TRUE;
    x_pixmap = newGWorld;
    SetGWorld(saveport, savegw);
  }
  else {
    ok = FALSE;
    x_pixmap = NULL;
  }
  return ok;
}

#if USE_XPM_IN_MAC
// Load a bitmap with xpm data (compiled in)
wxBitmap::wxBitmap(char **data, wxItem *anItem)
{
  __type = wxTYPE_BITMAP;
  width = 0;
  height = 0;
  depth = 0;
  freePixmap = FALSE;
  XImage	*ximage;
  XpmAttributes xpmAttr;

  ok = FALSE;

  xpmAttr.valuemask = XpmReturnInfos;	/* nothing yet, but get infos back */
  int  ErrorStatus = XpmCreateImageFromData(NULL,	// don't have a Display dpy
					    data,
					    &ximage,
					    NULL,				// don't want a shapemask 
					    &xpmAttr);

  if (ErrorStatus == XpmSuccess) {
    // Set attributes
    width=xpmAttr.width;
    height = xpmAttr.height;
    depth = wxDisplayDepth();
    XpmFreeAttributes(&xpmAttr);
    ok = TRUE;
    x_pixmap = ximage->bitmap;	// Actually a GWorldPtr!
    XImageFree(ximage);			// does not delete the GWorld
  } else {
    // XpmDebugError(ErrorStatus, NULL);
    ok = False;
  }
}
#endif

extern int wxsGetImageType(char *);

/*
   if USE_XPM_IN_MAC and USE_IMAGE_LOADING_IN_MAC are not defined in wx_setup.h
   then the only thing we can load is a PICT file.
   USE_XPM_IN_MAC requires that the flags arg has the wxBITMAP_TYPE_XPM bit set.
   USE_IMAGE_LOADING... does NOT look at the flags, instead it looks at file
   extensions. This deserves a proper cleanup.
   */
Bool wxBitmap::LoadFile(char *name, long flags)
{
  if (selectedIntoDC) return FALSE;
  
  if (x_pixmap) {
    FreeGWorld(x_pixmap);
    x_pixmap = NULL;
  }
  wxColourMap *colourmap;
  ok = FALSE;

  if (!flags)
    flags = wxsGetImageType(name);

  if (flags & wxBITMAP_TYPE_XPM) {
    XImage	*ximage;
    XpmAttributes xpmAttr;
    
    xpmAttr.valuemask = XpmReturnInfos;	/* nothing yet, but get infos back */
    int ErrorStatus = XpmReadFileToImage(NULL,	// don't have a Display dpy
					 name,
					 &ximage,							// we get this back
					 NULL,							// don't want a shapemask
					 &xpmAttr);						// where to put the attributes

    if (ErrorStatus == XpmSuccess) {
      // Set attributes
      width=xpmAttr.width;
      height = xpmAttr.height;
      depth = wxDisplayDepth();
      XpmFreeAttributes(&xpmAttr);
      ok = TRUE;
      x_pixmap = ximage->bitmap;	// Actually a GWorldPtr!
      XImageFree(ximage);			// does not delete the GWorld
    }
    return ok;
  }

  if (flags & wxBITMAP_TYPE_GIF) {
    ok = wxLoadGifIntoBitmap(name, this, &colourmap);
    if (ok) SetDepth(wxDisplayDepth());
  } else if (flags & wxBITMAP_TYPE_PICT) {
    ok = wxLoadPICTIntoBitmap(name, this, &colourmap);
  } else if (flags & wxBITMAP_TYPE_XBM) {
    ok = wxLoadXBMIntoBitmap(name, this, &colourmap);
  } else if (flags & wxBITMAP_TYPE_BMP) {
    ok = wxLoadBMPIntoBitmap(name, this, &colourmap);
    if (ok) SetDepth(wxDisplayDepth());
  } else if (flags & wxBITMAP_TYPE_JPEG) {
    ok = read_JPEG_file(name, this);
    if (!ok) {
      if (x_pixmap) {
	FreeGWorld(x_pixmap);
	x_pixmap = NULL;
      }
      ok = FALSE;
    } else {
      SetDepth(wxDisplayDepth());
    }	
  } else {
    ok = FALSE;
  }
  return ok;
}

Bool wxBitmap::SaveFile(char *name, int type, wxColourMap *cmap)
{
  Bool ok = FALSE;

  if (type & wxBITMAP_TYPE_XBM) {
    ok = wxSaveXBMFromBitmap(name, this, NULL);
  } else if (type & wxBITMAP_TYPE_XPM) {
    if (!Ok()) return FALSE;
    
    XImage ximage;
    
    GDHandle savegw;
    CGrafPtr saveport;
    GetGWorld(&saveport, &savegw);
    
    SetGWorld(x_pixmap, 0);
    // LockPixels(GetGWorldPixMap(x_pixmap));
    
    ximage.width = GetWidth(); 
    ximage.height = GetHeight();
    ximage.depth = GetDepth(); 
    ximage.bitmap = NULL;
    
    int errorStatus = XpmWriteFileFromImage(NULL, name,
					    &ximage, (XImage *)NULL, 
					    (XpmAttributes *)NULL);

    // UnlockPixels(GetGWorldPixMap(x_pixmap));
    SetGWorld(saveport, savegw);

    ok = (errorStatus == XpmSuccess);
  }
  
  return ok;
}

void wxBitmap::SetColourMap(wxColourMap *cmap)
{
}

wxColourMap* wxBitmap::GetColourMap(void)
{
  return NULL;
}

//  --------------- Some Mac extensions ---- should only be used inside
//		wxwindows, like from wx_button, wxDC, wxMemoryDC, etc.
//		we ASSUME that SetMacDC() is set to the proper destination port.
void wxBitmap::DrawMac(void)
{
  DrawMac(0, 0);
}

void wxBitmap::DrawMac(int x, int y, int mode)
{
  if (x_pixmap) {
    Rect sbounds = {0, 0, height, width};
    Rect dbounds = {y, x, height+y, width+x};
    OffsetRect(&dbounds,SetOriginX,SetOriginY);
    CGrafPtr portNow;
    GDHandle deviceNow;
    ::GetGWorld(&portNow,&deviceNow);

    const BitMap *srcbm = GetPortBitMapForCopyBits(x_pixmap);
    const BitMap *dstbm = GetPortBitMapForCopyBits(portNow);

    ::CopyBits(srcbm, dstbm, &sbounds, &dbounds, mode, NULL);
  }
}
