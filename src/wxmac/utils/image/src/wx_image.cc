/*
 * File:     wx_image.cc
 * Purpose:  
 *
 *                       wxWindows 1.50
 * Copyright (c) 1993 Artificial Intelligence Applications Institute,
 *                   The University of Edinburgh
 *
 *                     Author: Julian Smart
 *                        Date: 7-9-93
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice, author statement and this permission
 * notice appear in all copies of this software and related documentation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, EXPRESS,
 * IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL THE ARTIFICIAL INTELLIGENCE APPLICATIONS INSTITUTE OR THE
 * UNIVERSITY OF EDINBURGH BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY OF
 * DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#ifdef wx_xview
#include <stdlib.h>
#endif

#ifdef wx_motif
#include <stdlib.h>
#endif

#include "wx.h"
#include "wx_image.h"

#ifdef wx_x

#include <X11/X.h>
#ifdef wx_xview
#include <xview/screen.h>
#include <xview/cursor.h>
#include <xview/svrimage.h>
extern Xv_Server xview_server;
#endif
#ifdef wx_motif
#endif
#ifdef wx_mac
#include "wximgfil.h"
#  if USE_XPM_IN_MAC
#    define FOR_MAC
#    include "xpm34.h"
#  endif
#endif

/* file types that can be read */
#define UNKNOWN 0
#define GIF     1
#define PM      2
#define PBM     3
#define XBM     4
#define BMP     5
#define PCX     6

static unsigned long rootbg, rootfg;  /* fg/bg for root border */
static int    autoquit = 0;     /* quit after loading first pic to rootW */
static int    autogamma = 0;    /* perform gamma correction by default */
static int    rootPattern = 0;  /* pattern used for root border */
static char   initpath[500];
#endif

far byte r[256], g[256], b[256];

// Save (device dependent) wxBitmap as a DIB
Bool wxSaveBitmap(char *filename, wxBitmap *bitmap, wxColourMap *colourmap)
{
	return FALSE;
}


Bool wxLoadPICTIntoBitmap(char *, wxBitmap *, wxColourMap **);

Bool wxLoadPICTIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
	FILE *fp = fopen(fileName,"rb");
	if (fp) {
		// I don't know why we skip 512 bytes. I would have
		// thought fopen only processes the data fork. I suppose
		// it could be the "Mac Draw" header block (IM-V, pg 88)
		fseek(fp, 0, SEEK_END);
		int fsize = ftell(fp) - 512;
		fseek(fp, 512, SEEK_SET);	// 0 didn't work
		PicHandle ph = (PicHandle)NewHandle(fsize);
		CheckMemOK(ph);
		int rsize = fread((char *)*ph, 1, fsize, fp);
		fclose(fp);
		// width = (*ph)->picFrame.right;
		bm->SetWidth((*ph)->picFrame.right);
		// bm->height = (*ph)->picFrame.bottom;
		bm->SetHeight((*ph)->picFrame.bottom);
		// bm->depth = wxDisplayDepth();
		bm->SetDepth(wxDisplayDepth());
		GDHandle savegd;
		CGrafPtr saveport;
		GetGWorld(&saveport, &savegd);
		QDErr err;
		GWorldPtr	newGWorld;
		Rect	bounds = {0, 0, bm->GetHeight(), bm->GetWidth()};
		err = NewGWorld(&bm->x_pixmap, 0, &bounds, NULL, NULL, noNewDevice);
		if (!err) {
		  SetGWorld(bm->x_pixmap, 0);
		  DrawPicture(ph, &bounds);
		  DisposeHandle((Handle)ph);
		  SetGWorld(saveport, savegd);
		  return TRUE;
	    } else {
	      bm->x_pixmap = NULL;
	      return FALSE;
	    }
	} else
		return FALSE;
}


Bool wxLoadXPMIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
#if USE_XPM_IN_MAC
	return bm->LoadFile(fileName,wxBITMAP_TYPE_XPM);
#else
    wxImage *img = new(wxImage);
  	img->LoadPM(fileName, 0);	// nc arg - what does it do ??
  	// Convert the image to a Bitmap
  	delete img;
#endif
 } 

void Mac_FixupFileName(char * dest, char * src);

void Mac_FixupFileName(char * dest, char * src) {
	char *slpos = strchr(src, '/');
	char *colpos = strchr(src, ':');
	if (colpos) {
		// Assume that any colon indicates a mac path
		strcpy(dest, src);
	} else if (slpos) {
		if (slpos == src) {
			// its an abs path - can't cope that
			*dest = '\0';
			return;
		}
		// create a relative path;
		*dest++ = ':';
		int len = colpos - src;
		strncpy(dest, src, len);
		dest += len;
		*dest++ = ':';
		strcpy(dest, slpos+1);	// CJC - FIXME - really should iterate over the rest 
	} else {
		// probably just a file name
		strcpy(dest, src);
	}
}

// Load into existing bitmap;
//Bool *wxLoadIntoBitmap(char *fileName, wxBitmap *bitmap, wxColourMap **pal);

Bool wxLoadIntoBitmap(char *infile, wxBitmap *bitmap, wxColourMap **pal)
{
  Bool stats = FALSE;
  char fileName[255];
  Mac_FixupFileName(fileName, infile);
  if (wxMatchWild("*.gif",fileName)) {
    stats = wxLoadGifIntoBitmap(fileName, bitmap, pal);
  }
  else if (wxMatchWild("*.pict", fileName)) {
    stats = wxLoadPICTIntoBitmap(fileName, bitmap, pal);
  }
  else if (wxMatchWild("*.xpm", fileName)) {
	stats = wxLoadXPMIntoBitmap(fileName, bitmap, pal);
 }
  if (stats)
    return TRUE;
  else
  {
    delete bitmap;
    return 0;
  }
  return 0;
}

wxBitmap *wxLoadBitmap(char *fileName, wxColourMap **pal)
{
  Bool stats;
  wxBitmap *bitmap = new wxBitmap;

  if (wxLoadIntoBitmap(fileName, bitmap, pal))
  	return bitmap;
  else
  	return NULL;
}

 
wxImage::wxImage(void)
{
	gifpic = NULL;
	dEBUG = 0;
}

wxImage::~wxImage(void)
{
	if (gifpic)
		delete gifpic;
}

Bool wxImage::Load(char *file)
{
  gifpic = new wxGIF(file);
  if (gifpic)
  	return TRUE;
  else
  	return FALSE;
}

Bool wxImage::Destroy(void)
{
  return FALSE;
}

void wxImage::Draw(wxCanvas *canvas, int x, int y, int width, int height)
{
}

void wxImage::Resize(int width, int height)
{
}

void wxImage::GetSize(int *width, int *height)
{
  *width = 0;
  *height = 0;
}

wxColourMap *wxImage::GetColourMap(void)
{
  return NULL;
}
