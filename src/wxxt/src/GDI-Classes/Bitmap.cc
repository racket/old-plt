/*								-*- C++ -*-
 *
 * Purpose: bitmap classes to implement pixmaps, icons, and cursors
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "Bitmap.h"
#endif

#define  Uses_XLib
#define  Uses_wxBitmap
#define  Uses_wxColour
#define  Uses_wxMemoryDC
#include "wx.h"

#if USE_XPM
#	include "../../contrib/xpm/lib/xpm.h"
#endif
#if USE_IMAGE_LOADING_IN_X
#define WXI_SKIP_WX_INCLUDES
#       include WXIMAGE_INCLUDE
#endif
#include <X11/cursorfont.h>

extern "C" { 
#include "XWidgets/wxAllocColor.h"
};
extern Colormap wx_default_colormap;

extern int read_JPEG_file(char * filename, wxBitmap *bm);
extern int write_JPEG_file(char * filename, wxBitmap *bm, int quality_val);

extern int wx_read_png(char *file_name, wxBitmap *bm, int w_mask, wxColour *bg);
extern int wx_write_png(char *file_name, wxBitmap *bm);

// hints for what to free in wxBitmap::Destroy()
enum {
    __BITMAP_NORMAL,	// <-- no special data
#if USE_XPM
    __BITMAP_XPM	// <-- XpmAttributes
#endif
};

class wxBitmap_Xintern {
public:
    int          type;			// what is the type of the bitmap
    unsigned int width, height, depth;	// dimensions of bitmap
    int          x_hot, y_hot;		// hotspot of bitmap
    Pixmap       x_pixmap;		// the displayable pixmap
#ifdef WX_USE_XRENDER
    long      picture;
#endif
    // Optional stuff for different bitmaps
    XpmAttributes* xpm;		// for XPM pixmaps
    void         *account;
};

class wxCursor_Xintern {
public:
    Cursor x_cursor;
};

//-----------------------------------------------------------------------------
// wxBitmap
//-----------------------------------------------------------------------------

// create nothing
wxBitmap::wxBitmap(void)
{
    __type = wxTYPE_BITMAP;

    Xbitmap = NULL;
    cmap    = wxAPP_COLOURMAP;
}

// create bitmap from bitmap-data
wxBitmap::wxBitmap(char bits[], int w, int h)
{
    __type = wxTYPE_BITMAP;

    Xbitmap = new wxBitmap_Xintern;
    cmap    = wxAPP_COLOURMAP;

    // set bitmap specific data
    Xbitmap->type   = __BITMAP_NORMAL;
    Xbitmap->width  = w;
    Xbitmap->height = h;
    Xbitmap->depth  = 1; // don't know what to do if depth > 1 !!!
    Xbitmap->x_hot  = 0;
    Xbitmap->y_hot  = 0;

    // create pixmap with depth 1
    Xbitmap->x_pixmap = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, bits, w, h);
    if (Xbitmap->x_pixmap == None) {
      // create failed
      DELETE_OBJ Xbitmap;
      Xbitmap = NULL;
    }
    Xbitmap->account = GC_malloc_accounting_shadow(w * h * 4);

    WXGC_IGNORE(this, selectedTo);
}

// create bitmap from file
wxBitmap::wxBitmap(char *bitmap_file, long flags, wxColour *bg)
{
    __type = wxTYPE_BITMAP;

    Xbitmap = NULL;
    cmap    = wxAPP_COLOURMAP;

    // use load method
    (void)LoadFile(bitmap_file, flags, bg);

    WXGC_IGNORE(this, selectedTo);
}

#if USE_XPM

// create bitmap from xpm-data
wxBitmap::wxBitmap(char **data, wxItem *WXUNUSED(anItem)) // anItem used for MOTIF
{
    int ErrorStatus;

    __type = wxTYPE_BITMAP;

    cmap    = wxAPP_COLOURMAP;
    Xbitmap = new wxBitmap_Xintern;
    // what I want to get from XPM
#ifdef MZ_PRECISE_GC
    {
      XpmAttributes *attr;
      attr = (XpmAttributes *)GC_malloc_atomic(sizeof(XpmAttributes));
      Xbitmap->xpm = attr;
    }
#else
    Xbitmap->xpm = new WXGC_ATOMIC XpmAttributes;
#endif
    Xbitmap->xpm->valuemask = (XpmReturnInfos | XpmReturnPixels | XpmCloseness
			       | XpmVisual | XpmDepth | XpmColormap);
    Xbitmap->xpm->closeness = 40000;
    Xbitmap->xpm->visual = wxAPP_VISUAL;
    Xbitmap->xpm->depth = wx_visual_depth;
    Xbitmap->xpm->colormap = wx_default_colormap;
    // create pixmap
    ErrorStatus = XpmCreatePixmapFromData(wxAPP_DISPLAY, wxAPP_ROOT,
					  data, &(Xbitmap->x_pixmap),
					  (Pixmap*)NULL, Xbitmap->xpm);
    if (ErrorStatus == XpmSuccess) {
	int sdummy; unsigned int udummy; Window wdummy;
	// create pixmap successful
	Xbitmap->type   = __BITMAP_XPM;
	Xbitmap->width  = Xbitmap->xpm->width;
	Xbitmap->height = Xbitmap->xpm->height;
	Xbitmap->x_hot  = Xbitmap->xpm->x_hotspot;
	Xbitmap->y_hot  = Xbitmap->xpm->y_hotspot;
	// get depth of pixmap
	XGetGeometry(wxAPP_DISPLAY, Xbitmap->x_pixmap, &wdummy, &sdummy, &sdummy,
		     &udummy, &udummy, &udummy, &(Xbitmap->depth));
	Xbitmap->account = GC_malloc_accounting_shadow(Xbitmap->width * Xbitmap->height * 4);
    } else {
	// create failed: free all memory
	XpmFreeAttributes(Xbitmap->xpm);
	DELETE_VAL Xbitmap->xpm;
	DELETE_OBJ Xbitmap;
	Xbitmap = NULL;
    }

    WXGC_IGNORE(this, selectedTo);
}

#endif

// create bitmap of given size
wxBitmap::wxBitmap(int w, int h, Bool b_and_w)
{
    __type = wxTYPE_BITMAP;

    Xbitmap = NULL;
    cmap    = wxAPP_COLOURMAP;

    // use create method
    (void)Create(w, h, b_and_w ? 1 : -1);

    WXGC_IGNORE(this, selectedTo);
}

// destroy bitmap
wxBitmap::~wxBitmap(void)
{
    // free pixmap and infos
    Destroy();

    selectedTo = NULL;
}

static int errorFlagged;
static int FlagError(Display*, XErrorEvent*)
{
  errorFlagged = 1;
  return 0;
}

typedef int (*X_Err_Handler)(Display*, XErrorEvent*);

// create empty bitmap with dimensions w,h,d
Bool wxBitmap::Create(int w, int h, int d)
{
    X_Err_Handler old_handler;

    Destroy(); // destroy old bitmap if any

    Xbitmap = new wxBitmap_Xintern;
    // set pixmap specific data
    Xbitmap->type   = __BITMAP_NORMAL;
    Xbitmap->width  = w;
    Xbitmap->height = h;
    if (d < 1) {
      Xbitmap->depth = wxDisplayDepth();
    } else {
      Xbitmap->depth = d;
    }
    Xbitmap->x_hot  = 0;
    Xbitmap->y_hot  = 0;
    // create pixmap

    old_handler = XSetErrorHandler(FlagError);
    errorFlagged = 0;

    Xbitmap->x_pixmap = XCreatePixmap(wxAPP_DISPLAY, wxAPP_ROOT, w, h, Xbitmap->depth);
    
    XSync(wxAPP_DISPLAY, FALSE);

    if (errorFlagged)
      Xbitmap->x_pixmap = None;

    XSetErrorHandler(old_handler);

    if (Xbitmap->x_pixmap == None) {
      // create failed!
      DELETE_OBJ Xbitmap;
      Xbitmap = NULL;
    } else {
      Xbitmap->account = GC_malloc_accounting_shadow((w * h * ((Xbitmap->depth == 1) ? 1 : 32)) >> 3);
    }

    return Ok();
}

// destroy bitmap
void wxBitmap::Destroy(void)
{
  if (Xbitmap) {
    XFreePixmap(wxAPP_DISPLAY, Xbitmap->x_pixmap); // free pixmap
    GC_free_accounting_shadow(Xbitmap->account);
    Xbitmap->account = NULL;
# ifdef WX_USE_XRENDER
    if (Xbitmap->picture) {
      wxFreePicture(Xbitmap->picture);
    }
# endif
    
    switch (Xbitmap->type) { // free type specific data
    case __BITMAP_XPM:
      {
	// free XPM data
	Colormap cm;
	cm = *((Colormap*)(cmap->GetHandle()));
	XFreeColors(wxAPP_DISPLAY, cm, Xbitmap->xpm->pixels, Xbitmap->xpm->npixels, 0);
	XpmFreeAttributes(Xbitmap->xpm);
	DELETE_VAL Xbitmap->xpm;
      }
      break;
    default:
      break; // no other formats so far
    }
    DELETE_OBJ Xbitmap;
  }
  // contains no pixmap
  Xbitmap = NULL;
}

extern int wxsGetImageType(char *);

// load bitmaps
Bool wxBitmap::LoadFile(char *fname, long flags, wxColour *bg)
{
  int getMask;

  if (selectedIntoDC)
    return FALSE;

  Destroy(); // destroy old pixmap if any

  if (flags & wxBITMAP_TYPE_MASK)
    getMask = 1;
  else
    getMask = 0;

  if (!flags || (flags == wxBITMAP_TYPE_MASK))
    flags = wxsGetImageType(fname);

  /* MATTHEW: move "Xbitmap = new wxBitmap_Xintern" into
     two appropriate cases. */

  if (flags & wxBITMAP_TYPE_XBM) { // XBM file format
    Xbitmap = new wxBitmap_Xintern;

    if (XReadBitmapFile(wxAPP_DISPLAY, wxAPP_ROOT, fname,
			&(Xbitmap->width), &(Xbitmap->height),
			&(Xbitmap->x_pixmap), 
			&(Xbitmap->x_hot), &(Xbitmap->y_hot))
	== BitmapSuccess)
      {
	Xbitmap->type  = __BITMAP_NORMAL;
	Xbitmap->depth = 1;
	Xbitmap->account = GC_malloc_accounting_shadow((Xbitmap->width * Xbitmap->height) >> 3);
      } else {
	DELETE_OBJ Xbitmap;
	Xbitmap = NULL;
      }
  }
  else if (flags & wxBITMAP_TYPE_JPEG) {
    if (!read_JPEG_file(fname, this)) {
      Destroy();
    }
  }
  else if (flags & wxBITMAP_TYPE_PNG) {
    if (!wx_read_png(fname, this, getMask, bg)) {
      Destroy();
    }
  }
#if USE_XPM
else if (flags & wxBITMAP_TYPE_XPM) { // XPM file format
  Xbitmap = new wxBitmap_Xintern;

  // what I want to get
#ifdef MZ_PRECISE_GC
  {
    XpmAttributes *attr;
    attr = (XpmAttributes *)GC_malloc_atomic(sizeof(XpmAttributes));
    Xbitmap->xpm = attr;
  }
#else
  Xbitmap->xpm = new WXGC_ATOMIC XpmAttributes;
#endif
  Xbitmap->xpm->valuemask = XpmReturnInfos | XpmReturnPixels | XpmCloseness | XpmDepth;
  Xbitmap->xpm->closeness = 40000;
  Xbitmap->xpm->depth = DefaultDepth(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));
  
  if (XpmReadFileToPixmap(wxAPP_DISPLAY, wxAPP_ROOT, fname,
			  &(Xbitmap->x_pixmap), (Pixmap*)NULL, Xbitmap->xpm)
      == XpmSuccess)
    {
      // read pixmap ok!
      int sdummy; unsigned int udummy; Window wdummy;
      Xbitmap->type   = __BITMAP_XPM;
      Xbitmap->width  = Xbitmap->xpm->width;
      Xbitmap->height = Xbitmap->xpm->height;
      Xbitmap->x_hot  = Xbitmap->xpm->x_hotspot;
      Xbitmap->y_hot  = Xbitmap->xpm->y_hotspot;
      XGetGeometry(wxAPP_DISPLAY, Xbitmap->x_pixmap, &wdummy, &sdummy, &sdummy,
		   &udummy, &udummy, &udummy, &(Xbitmap->depth));
      Xbitmap->account = GC_malloc_accounting_shadow(Xbitmap->width * Xbitmap->height * 4);
    } else {
      // read failed: free all memory
      XpmFreeAttributes(Xbitmap->xpm);
      DELETE_VAL Xbitmap->xpm;
      DELETE_OBJ Xbitmap;
      Xbitmap = NULL;
    }
}
#endif
#if USE_IMAGE_LOADING_IN_X
else if ((flags & wxBITMAP_TYPE_ANY) || (flags & wxBITMAP_TYPE_BMP) ||
	 (flags & wxBITMAP_TYPE_GIF))
  {
    wxColourMap *map = NULL;
    Bool success = FALSE;

    if (flags & wxBITMAP_DISCARD_COLOURMAP)
      success = wxLoadIntoBitmap(fname, this, NULL, getMask);
    else {
      wxColourMap *cm;
      success = wxLoadIntoBitmap(fname, this, &cm, getMask);
      cmap = cm;
    }

    if (!success && map) {
      DELETE_OBJ map;
      map = NULL;
    }

    if (map)
      cmap = map;
  }
#endif

  return Ok();
}

static int write_pixmap_as_bitmap(Display *display, Pixmap pm, char *fname, 
				   int width, int height)
{
  char *data;
  int rw, ok, i, j, pos;
  XImage *img;
  Pixmap bm;

  img = XGetImage(display, pm, 0, 0, width, height, AllPlanes, ZPixmap);

  rw = ((width + 1) >> 3);

  data = new char[rw * height];

  pos = 0;
  for (j = 0; j < height; j++, pos += rw) {
    int bit = 0x01, v = 0, count = 0;
    int row = pos;

    for (i = 0; i < width; i++) {
      XColor xcol;
      unsigned long pixel;

      pixel = XGetPixel(img, i, j);
      if (xcol.pixel != pixel) {
	xcol.pixel = pixel;
	
	wxQueryColor(display, 
		     wx_default_colormap,
		     &xcol);
      }

      if ((xcol.red >> 8) != 255
	  || (xcol.green >> 8) != 255
	  || (xcol.blue >> 8) != 255)
	v += bit;

      bit = bit << 1;
      count++;
      if (count == 8) {
	data[row++] = v;
	v = 0;
	bit = 0x01;
	count = 0;
      }
    }
    if (bit != 0x01)
      data[row] = v;
  }

  bm = XCreateBitmapFromData(display, pm, data, width, height);

  ok = (XWriteBitmapFile(display, fname, bm, width, height, 0, 0)
	== BitmapSuccess);

  XFreePixmap(display, bm);

  XDestroyImage(img);

  return ok;
} 

// save bitmaps
Bool wxBitmap::SaveFile(char *fname, int type, int quality, wxColourMap *WXUNUSED(cmap))
{
  if (Xbitmap) {
    if (selectedTo)
      selectedTo->EndSetPixel();
    
    switch (type) {
    case wxBITMAP_TYPE_XBM:
      if (Xbitmap->depth == 1)
	return (XWriteBitmapFile(wxAPP_DISPLAY, fname, Xbitmap->x_pixmap,
				 Xbitmap->width, Xbitmap->height,
				 Xbitmap->x_hot, Xbitmap->y_hot)
		== BitmapSuccess);
      else {
	return write_pixmap_as_bitmap(wxAPP_DISPLAY, Xbitmap->x_pixmap,  fname, 
				      Xbitmap->width, Xbitmap->height);
      }
      break; // write failed or depth != 1
    case wxBITMAP_TYPE_XPM:
      return (XpmWriteFileFromPixmap(wxAPP_DISPLAY, fname, Xbitmap->x_pixmap,
				     (Pixmap)NULL, (XpmAttributes*)NULL)
	      == XpmSuccess);
      break; // write failed
    case wxBITMAP_TYPE_JPEG:
      return write_JPEG_file(fname, this, quality);
      break; // write failed
    case wxBITMAP_TYPE_PNG:
      return wx_write_png(fname, this);
      break; // write failed
    default:
      break; // no other save methods so far
    }
  }
  return FALSE;
}

// retrieve infos
int   wxBitmap::GetDepth(void)  { return (Xbitmap ? Xbitmap->depth : 0); }
int   wxBitmap::GetHeight(void) { return (Xbitmap ? Xbitmap->height : 0); }
int   wxBitmap::GetWidth(void)  { return (Xbitmap ? Xbitmap->width : 0); }
void  wxBitmap::GetHotSpot(int *x, int *y)
{
    if (Xbitmap) { *x = Xbitmap->x_hot; *y = Xbitmap->y_hot; }
    else         { *x = *y = 0; }
}

void* wxBitmap::GetHandle(void) {
  return (Xbitmap ? &(Xbitmap->x_pixmap) : NULL);
}

#ifdef WX_USE_XRENDER
long wxBitmap::GetPicture(void) { 
  if (Xbitmap) {
    if (!Xbitmap->picture) {
      long p;
      p = wxMakePicture(Xbitmap->x_pixmap, Xbitmap->depth != 1);
      Xbitmap->picture = p;
    }
    return Xbitmap->picture;
  } else
    return 0;
}
#endif

//-----------------------------------------------------------------------------
// wxCursor
//-----------------------------------------------------------------------------

/* wxCursor is a subclass of wxBitmap for historical reasons. It
   doesn't make any sense. */

static XColor black = { 0, 0, 0, 0, 0, 0 };
static XColor white = { 0, 65535, 65535, 65535, DoRed | DoGreen | DoBlue, 0 };

wxCursor::wxCursor(void) : wxBitmap()
{
    __type = wxTYPE_CURSOR;

    Xcursor = NULL;
}

wxCursor::wxCursor(wxBitmap *bm, wxBitmap *mask, int x, int y) : wxBitmap()
{
    __type = wxTYPE_CURSOR;

    Xcursor = NULL;

    if (bm->Ok() && mask->Ok()
	&& (bm->GetDepth() == 1)
	&& (mask->GetDepth() == 1)
	&& (bm->GetWidth() == mask->GetWidth())
	&& (bm->GetHeight() == mask->GetHeight())) {
      Xcursor = new wxCursor_Xintern;
      Xcursor->x_cursor
	= XCreatePixmapCursor(wxAPP_DISPLAY,
			      bm->Xbitmap->x_pixmap, mask->Xbitmap->x_pixmap,
			      &black, &white,
			      x, y);
    }
}

static unsigned int x_cursor_id[] = { // same order as wxCURSOR_...
	XC_left_ptr,	        XC_based_arrow_down,	XC_based_arrow_up,
	XC_target,		XC_crosshair,		XC_cross_reverse,
	XC_double_arrow,	XC_hand2,		XC_xterm,
	XC_leftbutton,		XC_sizing,		XC_middlebutton,
	XC_pirate,		XC_spraycan,		XC_pencil,
	XC_sb_left_arrow,	XC_sb_right_arrow,	XC_question_arrow,
	XC_rightbutton,		XC_circle,		XC_sb_v_double_arrow,
	XC_circle,		XC_sb_h_double_arrow,	XC_sizing,
	XC_spraycan,		XC_watch,		XC_watch,
      };

static char char_data[32] = { // bits for char pointer
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
	0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00,
    };

static char blank_data[32] = { // bits for blank pointer
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

static char nwse_bits[32] = {
 0x00,0x00,0x00,0x00,0xf8,0x00,0x38,0x00,0x58,0x00,0xa8,0x00,0x48,0x01,0x80,
 0x02,0x00,0x25,0x00,0x2a,0x00,0x34,0x00,0x38,0x00,0x3e,0x00,0x00,0x00,0x00,
 0x00,0x00};

static char swne_bits[32] = {
 0x00,0x00,0x00,0x00,0x00,0x3e,0x00,0x38,0x00,0x34,0x00,0x2a,0x00,0x25,0x80,
 0x02,0x48,0x01,0xa8,0x00,0x58,0x00,0x38,0x00,0xf8,0x00,0x00,0x00,0x00,0x00,
 0x00,0x00};


wxCursor::wxCursor(int cursor_type) : wxBitmap()
{
    Pixmap pixmap;
    
    __type = wxTYPE_CURSOR;

    Xcursor = new wxCursor_Xintern;

    switch (cursor_type) {
    case wxCURSOR_BLANK:
      pixmap = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, blank_data, 16, 16);
      Xcursor->x_cursor = XCreatePixmapCursor(wxAPP_DISPLAY, pixmap, pixmap, &black, &black, 8, 8);
      XFreePixmap(wxAPP_DISPLAY, pixmap);
      break;
    case wxCURSOR_CHAR:
      pixmap = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, char_data, 16, 16);
      Xcursor->x_cursor = XCreatePixmapCursor(wxAPP_DISPLAY, pixmap, pixmap, &black, &black, 0, 13);
      XFreePixmap(wxAPP_DISPLAY, pixmap);
      break;
    case wxCURSOR_SIZENESW:
      pixmap = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, swne_bits, 16, 16);
      Xcursor->x_cursor = XCreatePixmapCursor(wxAPP_DISPLAY, pixmap, pixmap, &black, &black, 0, 13);
      XFreePixmap(wxAPP_DISPLAY, pixmap);
      break;
    case wxCURSOR_SIZENWSE:
      pixmap = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, nwse_bits, 16, 16);
      Xcursor->x_cursor = XCreatePixmapCursor(wxAPP_DISPLAY, pixmap, pixmap, &black, &black, 0, 13);
      XFreePixmap(wxAPP_DISPLAY, pixmap);
      break;
    default:
      if (wxFIRST_X11_CURSOR <= cursor_type && cursor_type <= wxLAST_X11_CURSOR) {
	Xcursor->x_cursor = XCreateFontCursor(wxAPP_DISPLAY, x_cursor_id[cursor_type]);
      }
      break;
    }
    if (!Xcursor->x_cursor) {
      DELETE_OBJ Xcursor;
      Xcursor = NULL;
    }
}

wxCursor::~wxCursor(void)
{
    if (Xcursor) {
      DELETE_OBJ Xcursor;
      Xcursor = NULL;
    }
}

void* wxCursor::GetHandle(void) { return (Xcursor ? &(Xcursor->x_cursor) : NULL); }
