/*								-*- C++ -*-
 * $Id: Bitmap.cc,v 1.3 1998/03/12 01:01:02 mflatt Exp $
 *
 * Purpose: bitmap classes to implement pixmaps, icons, and cursors
 *
 * Authors: Markus Holzem and Julian Smart
 *
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
#include "wx.h"

#if USE_XPM
#	include "../../contrib/xpm/lib/xpm.h"
#endif
#if USE_IMAGE_LOADING_IN_X
#define WXI_SKIP_WX_INCLUDES
#       include WXIMAGE_INCLUDE
#endif
#include <X11/cursorfont.h>

// hints for what to free in wxBitmap::Destroy()
enum {
    __BITMAP_NORMAL,	// <-- no special data
#if USE_XPM
    __BITMAP_XPM	// <-- XpmAttributes
#endif
};

IMPLEMENT_DYNAMIC_CLASS(wxBitmap, wxObject)
IMPLEMENT_DYNAMIC_CLASS(wxCursor, wxBitmap)
IMPLEMENT_DYNAMIC_CLASS(wxIcon, wxBitmap)
IMPLEMENT_DYNAMIC_CLASS(wxGDIList, wxList)

class wxBitmap_Xintern {
public:
    int          type;			// what is the type of the bitmap
    unsigned int width, height, depth;	// dimensions of bitmap
    int          x_hot, y_hot;		// hotspot of bitmap
    Pixmap       x_pixmap;		// the displayable pixmap
    // Optional stuff for different bitmaps
    union {
#if USE_XPM
	XpmAttributes* xpm;		// for XPM pixmaps
#endif
	int dummy;			// for savety
    } ext;
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

#if !WXGARBAGE_COLLECTION_ON
    wxTheBitmapList->Append(this);
#endif
}

// create bitmap from bitmap-data
wxBitmap::wxBitmap(char bits[], int w, int h, int d)
{
    __type = wxTYPE_BITMAP;

    Xbitmap = new wxBitmap_Xintern;
    cmap    = wxAPP_COLOURMAP;

    // set bitmap specific data
    Xbitmap->type   = __BITMAP_NORMAL;
    Xbitmap->width  = w;
    Xbitmap->height = h;
    Xbitmap->depth  = d = 1; // don't know what to do if depth > 1 !!!
    Xbitmap->x_hot  = 0;
    Xbitmap->y_hot  = 0;

    // create pixmap with depth 1
    if (((Xbitmap->x_pixmap
	  = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, bits, w, h)))
	== None)
    {
	// create failed
	delete Xbitmap;
	Xbitmap = NULL;
    }
#if !WXGARBAGE_COLLECTION_ON
    wxTheBitmapList->Append(this);
#endif
}

// create bitmap from file
wxBitmap::wxBitmap(char *bitmap_file, long flags)
{
    __type = wxTYPE_BITMAP;

    Xbitmap = NULL;
    cmap    = wxAPP_COLOURMAP;

    // use load method
    (void)LoadFile(bitmap_file, flags);

#if !WXGARBAGE_COLLECTION_ON
    wxTheBitmapList->Append(this);
#endif
}

#if USE_XPM

// create bitmap from xpm-data
wxBitmap::wxBitmap(char **data, wxItem *WXUNUSED(anItem)) // anItem used for MOTIF
{
    __type = wxTYPE_BITMAP;

    cmap    = wxAPP_COLOURMAP;
    Xbitmap = new wxBitmap_Xintern;
    // what I want to get from XPM
    Xbitmap->ext.xpm = new XpmAttributes;
    Xbitmap->ext.xpm->valuemask = XpmReturnInfos | XpmReturnPixels | XpmCloseness;
    Xbitmap->ext.xpm->closeness = 40000;
    // create pixmap
    int ErrorStatus
	= XpmCreatePixmapFromData(wxAPP_DISPLAY, wxAPP_ROOT,
				  data, &(Xbitmap->x_pixmap),
				  (Pixmap*)NULL, Xbitmap->ext.xpm);
    if (ErrorStatus == XpmSuccess) {
	// create pixmap successful
	Xbitmap->type   = __BITMAP_XPM;
	Xbitmap->width  = Xbitmap->ext.xpm->width;
	Xbitmap->height = Xbitmap->ext.xpm->height;
	Xbitmap->x_hot  = Xbitmap->ext.xpm->x_hotspot;
	Xbitmap->y_hot  = Xbitmap->ext.xpm->y_hotspot;
	// get depth of pixmap
	int sdummy; unsigned int udummy; Window wdummy;
	XGetGeometry(wxAPP_DISPLAY, Xbitmap->x_pixmap, &wdummy, &sdummy, &sdummy,
		     &udummy, &udummy, &udummy, &(Xbitmap->depth));
    } else {
	// create failed: free all memory
	XpmFreeAttributes(Xbitmap->ext.xpm);
	delete Xbitmap->ext.xpm;
	delete Xbitmap;
	Xbitmap = NULL;
    }
#if !WXGARBAGE_COLLECTION_ON
    wxTheBitmapList->Append(this);
#endif
}

#endif

// create bitmap of given size
wxBitmap::wxBitmap(int w, int h, int d)
{
    __type = wxTYPE_BITMAP;

    Xbitmap = NULL;
    cmap    = wxAPP_COLOURMAP;

    // use create method
    (void)Create(w, h, d);

#if !WXGARBAGE_COLLECTION_ON
    wxTheBitmapList->Append(this);
#endif
}

// destroy bitmap
wxBitmap::~wxBitmap(void)
{
    // free pixmap and infos
    Destroy();
    // remove Bitmap form List
#if !WXGARBAGE_COLLECTION_ON
    wxTheBitmapList->DeleteObject(this);
#endif
}

static int errorFlagged;
static int FlagError(Display*, XErrorEvent*)
{
  errorFlagged = 1;
  return 0;
}

// create empty bitmap with dimensions w,h,d
Bool wxBitmap::Create(int w, int h, int d)
{
    Destroy(); // destroy old bitmap if any

    /* MATTHEW: [5] */
    if (d > 1) {
      int c, *depths, n;

      n = ScreenCount(wxAPP_DISPLAY);
      while (n--) {
	int i;
	depths = XListDepths(wxAPP_DISPLAY, n, &c);

	for (i = 0; i < c; i++)
	  if (depths[i] == d)
	    break; /* good depth */

	if (i >= c)
	  break; /* bad depth */
      }

      if (n >= 0)
	d = -1; /* bad depth */
    }

    Xbitmap = new wxBitmap_Xintern;
    // set pixmap specific data
    Xbitmap->type   = __BITMAP_NORMAL;
    Xbitmap->width  = w;
    Xbitmap->height = h;
    Xbitmap->depth  = d < 1 ? wxDisplayDepth() : d;
    Xbitmap->x_hot  = 0;
    Xbitmap->y_hot  = 0;
    // create pixmap

    int (*old_handler)(Display*, XErrorEvent*);
    old_handler = XSetErrorHandler(FlagError);
    errorFlagged = 0;

    Xbitmap->x_pixmap = XCreatePixmap(wxAPP_DISPLAY, wxAPP_ROOT, w, h, Xbitmap->depth);

    XSync(wxAPP_DISPLAY, FALSE);

    if (errorFlagged)
      Xbitmap->x_pixmap = None;

    XSetErrorHandler(old_handler);

    if (Xbitmap->x_pixmap == None) {
      // create failed!
      delete Xbitmap;
      Xbitmap = NULL;
    }

    return Ok();
}

// destroy bitmap
void wxBitmap::Destroy(void)
{
    if (Xbitmap) {
	XFreePixmap(wxAPP_DISPLAY, Xbitmap->x_pixmap); // free pixmap
	switch (Xbitmap->type) { // free type specific data
#if USE_XPM
	case __BITMAP_XPM:
	    // free XPM data
	    XFreeColors(wxAPP_DISPLAY, *((Colormap*)(cmap->GetHandle())),
			Xbitmap->ext.xpm->pixels, Xbitmap->ext.xpm->npixels, 0);
	    XpmFreeAttributes(Xbitmap->ext.xpm);
	    delete Xbitmap->ext.xpm;
	    break;
#endif
	default:
	    break; // no other formats so far
	}
	delete Xbitmap;
    }
    // contains no pixmap
    Xbitmap = NULL;
}

// load bitmaps
Bool wxBitmap::LoadFile(char *fname, long flags)
{
    if (selectedIntoDC)
      return FALSE;

    Destroy(); // destroy old pixmap if any

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
	} else {
	    delete Xbitmap;
	    Xbitmap = NULL;
	}
    }
#if USE_XPM
    else if (flags & wxBITMAP_TYPE_XPM) { // XPM file format
      Xbitmap = new wxBitmap_Xintern;

	// what I want to get
	Xbitmap->ext.xpm = new XpmAttributes;
	Xbitmap->ext.xpm->valuemask = XpmReturnInfos | XpmReturnPixels | XpmCloseness | XpmDepth;
	Xbitmap->ext.xpm->closeness = 40000;
        Xbitmap->ext.xpm->depth = DefaultDepth(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY));

	if (XpmReadFileToPixmap(wxAPP_DISPLAY, wxAPP_ROOT, fname,
				&(Xbitmap->x_pixmap), (Pixmap*)NULL, Xbitmap->ext.xpm)
	    == XpmSuccess)
	{
	    // read pixmap ok!
	    Xbitmap->type   = __BITMAP_XPM;
	    Xbitmap->width  = Xbitmap->ext.xpm->width;
	    Xbitmap->height = Xbitmap->ext.xpm->height;
	    Xbitmap->x_hot  = Xbitmap->ext.xpm->x_hotspot;
	    Xbitmap->y_hot  = Xbitmap->ext.xpm->y_hotspot;
	    int sdummy; unsigned int udummy; Window wdummy;
	    XGetGeometry(wxAPP_DISPLAY, Xbitmap->x_pixmap, &wdummy, &sdummy, &sdummy,
			 &udummy, &udummy, &udummy, &(Xbitmap->depth));
	} else {
	    // read failed: free all memory
	    XpmFreeAttributes(Xbitmap->ext.xpm);
	    delete Xbitmap->ext.xpm;
	    delete Xbitmap;
	    Xbitmap = NULL;
	}
    }
#endif
#if USE_IMAGE_LOADING_IN_X
  /* MATTHEW */
  else if ((flags & wxBITMAP_TYPE_ANY) || (flags & wxBITMAP_TYPE_BMP) ||
           (flags & wxBITMAP_TYPE_GIF))
  {
    wxColourMap *map = NULL;
    Bool success = FALSE;

    if (flags & wxBITMAP_DISCARD_COLOURMAP)
      success = wxLoadIntoBitmap(fname, this);
    else
      success = wxLoadIntoBitmap(fname, this, &cmap);

    if (!success && map) {
      delete map;
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
  char *data, *pos;
  int rw, ok, i, j;

  XImage *img = XGetImage(display, pm, 0, 0, width, height, AllPlanes, ZPixmap);

  rw = ((width + 1) >> 3);

  data = new char[rw * height];

  pos = data;
  for (j = 0; j < height; j++, pos += rw) {
    int bit = 0x01, v = 0, count = 0;
    char *row = pos;

    for (i = 0; i < width; i++) {
      XColor xcol;
      unsigned long pixel;

      pixel = XGetPixel(img, i, j);
      if (xcol.pixel != pixel) {
	xcol.pixel = pixel;
	
	XQueryColor(display, 
		    DefaultColormapOfScreen(DefaultScreenOfDisplay(display)), 
		    &xcol);
      }

      if ((xcol.red >> 8) != 255
	  || (xcol.green >> 8) != 255
	  || (xcol.blue >> 8) != 255)
	v += bit;

      bit = bit << 1;
      count++;
      if (count == 8) {
	*(row++) = v;
	v = 0;
	bit = 0x01;
	count = 0;
      }
    }
    if (bit != 0x01)
      *row = v;
  }

  Pixmap bm = XCreateBitmapFromData(display, pm, data, width, height);

  ok = (XWriteBitmapFile(display, fname, bm, width, height, 0, 0)
	== BitmapSuccess);

  XFreePixmap(display, bm);

  XDestroyImage(img);

  return ok;
} 

// save bitmaps
Bool wxBitmap::SaveFile(char *fname, int type, wxColourMap *WXUNUSED(cmap))
{
    if (Xbitmap) {
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
#if USE_XPM
	case wxBITMAP_TYPE_XPM:
	    return (XpmWriteFileFromPixmap(wxAPP_DISPLAY, fname, Xbitmap->x_pixmap,
					   (Pixmap)NULL, (XpmAttributes*)NULL)
		    == XpmSuccess);
	    break; // write failed
#endif
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
void* wxBitmap::GetHandle(void) { return (Xbitmap ? &(Xbitmap->x_pixmap) : NULL); }

//-----------------------------------------------------------------------------
// wxCursor
//-----------------------------------------------------------------------------

static XColor black = { 0, 0, 0, 0, 0, 0 };

wxCursor::wxCursor(void) : wxBitmap()
{
    __type = wxTYPE_CURSOR;

    Xcursor = NULL;
}

wxCursor::wxCursor(char *name, long flags, int x, int y) : wxBitmap(name, flags)
{
    __type = wxTYPE_CURSOR;

    Xcursor = NULL;

    /* MATTHEW: [5] check Ok() */
    if (Xbitmap && Xbitmap->depth == 1 && Ok()) { // only possible for bitmaps
        Xbitmap->x_hot = x; Xbitmap->y_hot = y;

	Xcursor = new wxCursor_Xintern;
	Xcursor->x_cursor
	    = XCreatePixmapCursor(wxAPP_DISPLAY,
				  Xbitmap->x_pixmap, Xbitmap->x_pixmap,
				  &black, &black,
				  Xbitmap->x_hot, Xbitmap->y_hot);
	Destroy(); // destroy bitmap
    }
}

wxCursor::wxCursor(char bits[], int width, int height /* , int x, int y */)
    : wxBitmap(bits, width, height, 1)
{
    __type = wxTYPE_CURSOR;

    /* MATTHEW: [5] check Ok() */
    if (Xbitmap && Xbitmap->depth == 1 && Ok()) { // only possible for bitmaps
      /* Xbitmap->x_hot = x; Xbitmap->y_hot = y; */
	Xcursor = new wxCursor_Xintern;
	Xcursor->x_cursor
	    = XCreatePixmapCursor(wxAPP_DISPLAY,
				  Xbitmap->x_pixmap, Xbitmap->x_pixmap,
				  &black, &black,
				  Xbitmap->x_hot, Xbitmap->y_hot);
	Destroy(); // destroy bitmap
    }
}

wxCursor::wxCursor(int cursor_type) : wxBitmap()
{
    static unsigned int x_cursor_id[] = { // same order as wxCURSOR_...
	XC_top_left_arrow,	XC_based_arrow_down,	XC_based_arrow_up,
	XC_target,		XC_crosshair,		XC_cross_reverse,
	XC_double_arrow,	XC_hand1,		XC_xterm,
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

    __type = wxTYPE_CURSOR;

    Pixmap pixmap;
    
    Xcursor = new wxCursor_Xintern;

    switch (cursor_type) {
    case wxCURSOR_BLANK:
	pixmap = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, blank_data, 16, 16);
	Xcursor->x_cursor
	    = XCreatePixmapCursor(wxAPP_DISPLAY, pixmap, pixmap, &black, &black, 8, 8);
	XFreePixmap(wxAPP_DISPLAY, pixmap);
	break;
    case wxCURSOR_CHAR:
	pixmap = XCreateBitmapFromData(wxAPP_DISPLAY, wxAPP_ROOT, char_data, 16, 16);
	Xcursor->x_cursor
	    = XCreatePixmapCursor(wxAPP_DISPLAY, pixmap, pixmap, &black, &black, 0, 13);
	XFreePixmap(wxAPP_DISPLAY, pixmap);
	break;
    default:
	if (wxFIRST_X11_CURSOR <= cursor_type && cursor_type <= wxLAST_X11_CURSOR)
	    Xcursor->x_cursor
		= XCreateFontCursor(wxAPP_DISPLAY, x_cursor_id[cursor_type]);
	break;
    }
    if (!Xcursor->x_cursor) {
	delete Xcursor;
	Xcursor = NULL;
    }
}

wxCursor::~wxCursor(void)
{
    if (Xcursor) delete Xcursor;
}

void* wxCursor::GetHandle(void) { return (Xcursor ? &(Xcursor->x_cursor) : NULL); }

//-----------------------------------------------------------------------------
// wxIcon
//-----------------------------------------------------------------------------

wxIcon::wxIcon(void) : wxBitmap()
{
    __type = wxTYPE_ICON;
}

wxIcon::wxIcon(char bits[], int width, int height, int depth)
    : wxBitmap(bits, width, height, depth)
{
    __type = wxTYPE_ICON;
}

wxIcon::wxIcon(int width, int height, int depth)
    : wxBitmap(width, height, depth)
{
    __type = wxTYPE_ICON;
}

#if USE_XPM
wxIcon::wxIcon(char **data)
    : wxBitmap(data)
{
    __type = wxTYPE_ICON;
}
#endif

wxIcon::wxIcon(char *name, long flags)
    : wxBitmap(name, flags)
{
    __type = wxTYPE_ICON;
}

//-----------------------------------------------------------------------------
// GDIList
//-----------------------------------------------------------------------------

wxGDIList::wxGDIList(void) : wxList()
{
}

wxGDIList::~wxGDIList (void)
{
    wxNode *node = First();
    while (node) {
	wxObject *object = (wxObject*)node->Data();
	wxNode *next = node->Next();
	delete object;
	node = next;
    }
}
