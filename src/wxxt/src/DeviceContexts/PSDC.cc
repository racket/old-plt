/*
 * File:      wb_ps.cc
 * Purpose:     Device context implementation (PostScript)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: PSDC.cc,v 1.23 1999/07/14 00:20:00 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

/* This file is the same for all three version of wxWindows from
   PLT. */

/* static const char sccsid[] = "@(#)wb_ps.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef wx_xt
# ifdef __GNUG__
# pragma implementation "PSDC.h"
# endif

# define  Uses_XLib
# define  Uses_wxList
# define  Uses_wxWindowDC
# define  Uses_wxMemoryDC
# define  Uses_wxPostScriptDC
# define  Uses_wxPrintSetup
# define  Uses_wxFontNameDirectory
# define  Uses_wxDialogBox
# define  Uses_wxButton
# define  Uses_wxRadioBox
# define  Uses_wxText
# define  Uses_wxChoice
# define  Uses_wxCheckBox
# include "wx.h"

# include <math.h>
# include <string.h>

#else

# ifdef __GNUG__
# pragma implementation "wx_dcps.h"
# pragma implementation
# pragma interface
#endif

# include "common.h"
# include "wx_frame.h"
# include "wx_dcps.h"
# include "wx_dcmem.h"
# include "wx_utils.h"
# include "wx_dialg.h"
# include "wx_cmdlg.h"
# include "wx_main.h"
# include "wx_lbox.h"
# include "wx_rbox.h"
# include "wx_buttn.h"
# include "wx_choic.h"
# include "wx_check.h"
# include "wx_messg.h"
# include "wx_txt.h"
# include "wx_mtxt.h"

#endif
#endif

#include "wx_rgn.h"

#if USE_POSTSCRIPT

# define YSCALE(y) ((paper_h) - ((y) * user_scale_y + device_origin_y))
# define XSCALE(x) ((x) * user_scale_x + device_origin_x)
# define YOFFSET(y) ((paper_h) - ((y) + device_origin_y))
# define XOFFSET(x) ((x) + device_origin_x)
# define YSCALEREL(dy) ((dy) * user_scale_y)
# define XSCALEREL(dx) ((dx) * user_scale_x)
# define XSCALEBND(dx) (XSCALEREL(dx) + device_origin_x)
# define YSCALEBND(dy) (YSCALEREL(dy) + device_origin_y)

# define ASCALEREL(a) ((a) * ascale)

# define PIE pie

#define RESET_FONT 0x1
#define RESET_COLOR 0x2

static double pie = 0.0;

#ifndef WXUNUSED
# define WXUNUSED(x) x
#endif
#ifndef IMPLEMENT_DYNAMIC_CLASS
# define IMPLEMENT_DYNAMIC_CLASS(x, y) /* empty */
#endif

#ifdef wx_xt
# define WXXTUNUSED(c) /* empty */
#else
# define WXXTUNUSED(x) x
#endif

#ifndef wx_mac
# define OFSTREAM_HAS_TELLP_AND_SEEKP 1
#endif

#define DEFAULT_PAPER "Letter 8 1/2 x 11 in"

class wxCanvas;

#ifndef wx_xt
#include "wx_privt.h"
#endif

#include <iostream.h>
#include <fstream.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
// #include <time.h>
#include <limits.h>
#include <assert.h>

static char *default_afm_path = NULL;

Bool XPrinterDialog (wxWindow *parent);

// Determine the Default Postscript Previewer
// available on the platform
#if defined(sun) && defined(wx_xview)
// OpenWindow/NeWS's Postscript Previewer
# define PS_VIEWER_PROG "pageview"
#elif defined(VMS)
#define PS_VIEWER_PROG "view/format=ps/select=x_display"
#elif defined(__sgi)
// SGI's Display Postscript Previewer
# define PS_VIEWER_PROG "dps"
#elif defined(wx_x)
// Front-end to ghostscript 
# define PS_VIEWER_PROG "ghostview"
#else
// Windows ghostscript/ghostview
# define PS_VIEWER_PROG NULL
#endif

#ifdef wx_mac
wxPrintPaperDatabase *wxThePrintPaperDatabase;
#endif

/*
static char *wx_preview_command = copystring(PS_VIEWER_PROG);
static Bool wx_printer_orientation = PS_PORTRAIT;
static int wx_printer_mode = PS_PREVIEW;

#ifdef VMS
static char *wx_printer_command = copystring("print");
static char *wx_printer_flags = copystring("/nonotify/queue=psqueue");
static char *wx_afm_path = copystring("sys$ps_font_metrics:");
#endif
#ifdef wx_msw
static char *wx_printer_command = copystring("print");
static char *wx_afm_path = copystring("c:\\windows\\system\\");
static char *wx_printer_flags = NULL;
#endif
#if !defined(VMS) && !defined(wx_msw)
static char *wx_printer_command = copystring("lpr");
static char *wx_printer_flags = NULL;
static char *wx_afm_path = NULL;
#endif
*/

#define _MAXPATHLEN 500

/* See "wxspline.cc" and "xfspline.cc" */
#if USE_XFIG_SPLINE_CODE
static const char *wxPostScriptHeaderSpline = " \
/DrawSplineSection {\n\
	/y3 exch def\n\
	/x3 exch def\n\
	/y2 exch def\n\
	/x2 exch def\n\
	/y1 exch def\n\
	/x1 exch def\n\
	/xa x1 x2 x1 sub 0.666667 mul add def\n\
	/ya y1 y2 y1 sub 0.666667 mul add def\n\
	/xb x3 x2 x3 sub 0.666667 mul add def\n\
	/yb y3 y2 y3 sub 0.666667 mul add def\n\
	x1 y1 lineto\n\
	xa ya xb yb x3 y3 curveto\n\
	} def\n\
";
#else
// No extra PS header for this spline implementation.
static const char *wxPostScriptHeaderSpline = NULL;

#endif /* USE_XFIG_SPLINE_CODE */

// steve, 05.09.94
// VMS has a bug in the ofstream class.
// the buffering doesn't work correctly. therefore
// we will allocate (temporarily) a very big buffer (1MB), so
// that a buffer overflow will not occur.
#ifdef VMS
#define VMS_BUFSIZ (1024L*1024L)
static char *fileBuffer = NULL;
#endif

#ifndef wx_xt
# define current_font font
#else
# define current_bk_mode current_text_bgmode
# define current_text_foreground current_text_fg
# define current_text_background current_text_bg
#endif

class PSStream : public wxObject {
  FILE *f;
  int int_width;

 public:

  PSStream(char *file) {
    f = fopen(file, "w");
    int_width = 0;
  }
  ~PSStream(void) {
    if (f) fclose(f);
  }

  int good(void) {
    return !!f;
  }

  PSStream& operator<<(char s) {
    fprintf(f, "%c", s);
    return *this;
  }
  PSStream& operator<<(const char *s) {
    fwrite(s, strlen(s), 1, f);
    return *this;
  }
  PSStream& operator<<(float n);
  PSStream& operator<<(double d) {
    return *this << (float)d;
  }
  PSStream& operator<<(long l);
  PSStream& operator<<(int i) {
    return *this << (long)i;
  }

  long tellp(void) {
    return ftell(f);
  }
  void seekp(long pos) {
    fseek(f, pos, 0);
  }

  void width(int w) {
    int_width = w;
  }
};

PSStream& PSStream::operator<<(float n) {
  if (int_width > 0) {
    if ((float)(long)n == n)
      return *this << (long)n;
  }
  fprintf(f, "%f", n);
  return *this;
}

PSStream& PSStream::operator<<(long l) {
  if (int_width > 0) {
    char buffer[50];
    sprintf(buffer, "%%%d.%dld", int_width, int_width);
    fprintf(f, buffer, l);
    int_width = 0;
  } else
    fprintf(f, "%ld", l);
  return *this;
}

IMPLEMENT_DYNAMIC_CLASS(wxPostScriptDC, wxDC)

wxPostScriptDC::wxPostScriptDC (void)
{
  Create(TRUE);
}

wxPostScriptDC::wxPostScriptDC (Bool interactive)
{
  Create(interactive);
}

Bool wxPostScriptDC::Create(Bool interactive)
{
  if (!pie)
    pie = 2 * asin((double)1.0);

  __type = wxTYPE_DC_POSTSCRIPT;
#ifndef wx_xt
  wx_interactive = interactive;
#endif
  current_font = wxNORMAL_FONT;
  device = wxDEVICE_EPS;
  clipping = NULL;

#ifndef wx_xt
  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  min_x = 10000.0;
  min_y = 10000.0;
  max_x = -10000.0;
  max_y = -10000.0;

  current_pen = NULL;
  current_brush = NULL;
  current_background_color = *wxWHITE;

  current_text_foreground = *wxBLACK;

  mapping_mode = MM_TEXT;
#else
  current_pen = wxBLACK_PEN;
  current_pen->Lock(1);
  current_brush = wxWHITE_BRUSH;
  current_brush->Lock(1);
  current_background_color = *wxWHITE;
#endif

  title = NULL;

  filename = NULL;

  pstream = NULL;

  /* MATTHEW: [9] */
  clipx = 0;
  clipy = 0;
  clipw = -1;
  cliph = -1;

  if ((ok = PrinterDialog(interactive)) == FALSE)
    return FALSE;

  currentRed = 0;
  currentGreen = 0;
  currentBlue = 0;

  Colour = TRUE;
  
  wxPrintSetupData *wxThePrintSetupData = wxGetThePrintSetupData();

  level2ok = wxThePrintSetupData->GetLevel2();
  afm_path = wxThePrintSetupData->GetAFMPath();

  char *paperType = wxThePrintSetupData->GetPaperName();
  if (!paperType)
    paperType = DEFAULT_PAPER;

  wxPrintPaperType *paper = wxThePrintPaperDatabase->FindPaperType(paperType);
  if (!paper)
    paper = wxThePrintPaperDatabase->FindPaperType(DEFAULT_PAPER);
  if (paper) {
    paper_w = (float)paper->widthPixels;
    paper_h = (float)paper->heightPixels;
  } else {
    paper_w = 1000;
    paper_h = 1000;
  }

  if (wxThePrintSetupData) {
    wxThePrintSetupData->GetPrinterTranslation(&paper_x, &paper_y);
    wxThePrintSetupData->GetPrinterScaling(&paper_x_scale, &paper_y_scale);
    landscape = (wxThePrintSetupData->GetPrinterOrientation() == PS_LANDSCAPE);
  } else {
    paper_x = paper_y = 0;
    paper_x_scale = paper_y_scale = 1;
    landscape = 0;
  }

  if (landscape) {
    float tmp;

    tmp = paper_w;
    paper_w = paper_h;
    paper_h = tmp;
  }

  paper_w /= paper_x_scale;
  if (!paper_w)
    paper_w = 1;
  paper_h /= paper_y_scale;
  if (!paper_h)
    paper_h = 1;

  return ok;
}

wxPostScriptDC::~wxPostScriptDC (void)
{
  if (current_brush) current_brush->Lock(-1);
  if (current_pen) current_pen->Lock(-1);

  if (pstream)
    delete pstream;
  if (filename)
    delete[]filename;
}

Bool wxPostScriptDC::PrinterDialog(Bool interactive)
{
  if (interactive) {
    ok = XPrinterDialog(NULL);
    if (!ok)
      return FALSE;
  } else
    ok = TRUE;

  wxPrintSetupData *wxThePrintSetupData = wxGetThePrintSetupData();

  mode = wxThePrintSetupData->GetPrinterMode();
  preview_cmd = copystring(wxThePrintSetupData->GetPrintPreviewCommand());
  print_cmd = copystring(wxThePrintSetupData->GetPrinterCommand());
  print_opts = copystring(wxThePrintSetupData->GetPrinterOptions());

  if ((mode == PS_PREVIEW) || (mode == PS_PRINTER)) {
    // For PS_PRINTER action this depends on a Unix-style print spooler
    // since the wx_printer_file can be destroyed during a session
    // @@@ TODO: a Windows-style answer for non-Unix
    char userId[256];
    wxGetUserId (userId, sizeof (userId) / sizeof (char));
    char tmp[256];
    strcpy (tmp, "/tmp/preview_");
    strcat (tmp, userId);
    strcat (tmp, ".ps");
    filename = copystring(tmp);
  } else if (mode == PS_FILE) {
    char *file = interactive ? (char *)NULL : wxThePrintSetupData->GetPrinterFile();
    if (!file)
      file = wxSaveFileSelector("PostScript", "ps");
    if (!file) {
      ok = FALSE;
      return FALSE;
    }
    filename = copystring(file);
    ok = TRUE;
  }

  return ok;
}

void wxPostScriptDC::SetClippingRect(float cx, float cy, float cw, float ch)
{
  if (!pstream)
    return;

  wxRegion *r = new wxRegion(this);
  r->SetRectangle(cx, cy, cw, ch);

  SetClippingRegion(r);
}

wxRegion *wxPostScriptDC::GetClippingRegion()
{
  if (clipping)
    return new wxRegion(this, clipping);
  else
    return NULL;
}

void wxPostScriptDC::SetClippingRegion(wxRegion *r)
{
  if (!pstream)
    return;
  if (r && (r->GetDC() != this))
    return;

  if (clipping) {
    clipping = NULL;
    *pstream << "initclip\n";
  }

  if (r) {
    *pstream << "newpath\n";
    if (r->ps) /* => non-empty region */
      *pstream << r->ps->Lift()->GetString();
    *pstream << "clip\n";

    clipping = r;
  }
}

void wxPostScriptDC::Clear(void)
{
  unsigned char red = current_background_color.Red();
  unsigned char blue = current_background_color.Blue();
  unsigned char green = current_background_color.Green();
  float redPS = (float) (((int) red) / 255.0);
  float bluePS = (float) (((int) blue) / 255.0);
  float greenPS = (float) (((int) green) / 255.0);
  
  /* Fill with current background */
  *pstream << "gsave newpath\n";
  *pstream << redPS << " " << greenPS << " " << bluePS << " setrgbcolor\n";
  *pstream << 0 << " " << 0 << " moveto\n";
  *pstream << 0 << " " << paper_h << " lineto\n";
  *pstream << paper_w << " " << paper_h << " lineto\n";
  *pstream << paper_w << " " << 0 << " lineto\n";
  *pstream << "closepath\n";
  *pstream << "fill grestore\n";
}

void wxPostScriptDC::FloodFill(float WXUNUSED(x), float WXUNUSED(y), wxColour * WXUNUSED(col), int WXUNUSED(style))
{
}

Bool wxPostScriptDC::GetPixel(float WXUNUSED(x), float WXUNUSED(y), wxColour * WXUNUSED(col))
{
  return FALSE;
}

void wxPostScriptDC::IntDrawLine (int x1, int y1, int x2, int y2)
{
  DrawLine ((float) x1, (float) y1, (float) x2, (float) y2);
}

void wxPostScriptDC::CrossHair (float x, float y)
{
  DrawLine(0, y, paper_w, y);
  DrawLine(x, 0, x, paper_h);
}

void wxPostScriptDC::DrawLine (float x1, float y1, float x2, float y2)
{
  if (!pstream)
    return;
  if (current_pen)
    SetPen (current_pen);
  *pstream << "newpath\n";
  *pstream << XSCALE(x1) << " " << YSCALE (y1) << " moveto\n";
  *pstream << XSCALE(x2) << " " << YSCALE (y2) << " lineto\n";
  *pstream << "stroke\n";
  CalcBoundingBox(XSCALEBND(x1), YSCALEBND(y1));
  CalcBoundingBox(XSCALEBND(x2), YSCALEBND(y2));
}

void wxPostScriptDC::DrawArc (float x, float y, float w, float h, float start, float end)
{
  if (!pstream)
    return;

  if (start != end) {
    /* Before we scale: */
    CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
    CalcBoundingBox(XSCALEBND(x + w), YSCALEBND(y + h));

    x = XSCALE(x);
    y = XSCALE(y);
    w = XSCALEREL(w);
    h = YSCALEREL(h);

    float radius = (h / 2);
    float xscale = (w / h);

    float a1 = start * (180 / pie);
    float a2 = end * (180 / pie);

    *pstream << "gsave\n";
    *pstream << (x + w/2) << " " << (paper_h - (y + h/2)) << " translate\n";
    *pstream << xscale << " " << 1 << " scale\n";

    if (current_brush && current_brush->GetStyle () != wxTRANSPARENT) {
      SetBrush(current_brush);
      
      *pstream << "newpath\n";
      *pstream << cos(start)*radius << " " << sin(start)*radius << " moveto\n";
      *pstream << "0 0 " << radius << " " << a1 << " " << a2 << " arc\n";

      *pstream << "0 0 lineto\n";

      *pstream << "closepath\n";

      *pstream << "fill\n";
    }
    if (current_pen && current_pen->GetStyle () != wxTRANSPARENT) {
      SetPen(current_pen);

      *pstream << "newpath\n";
      *pstream << cos(start)*radius << " " << sin(start)*radius << " moveto\n";
      *pstream << "0 0 " << radius << " "
	<< a1 << " " << a2 << " arc\n";
      *pstream << "stroke\n";
    }

    *pstream << "grestore\n";
  }
}

void wxPostScriptDC::DrawPoint (float x, float y)
{
  if (!pstream)
    return;
  if (current_pen)
    SetPen (current_pen);
  *pstream << "newpath\n";
  *pstream << XSCALE(x) << " " << YSCALE (y) << " moveto\n";
  *pstream << XSCALE(x+1) << " " << YSCALE (y) << " lineto\n";
  *pstream << "stroke\n";
  CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
}

void wxPostScriptDC::DrawPolygon (int n, wxPoint points[], float xoffset, float yoffset, int fillStyle)
{
  if (!pstream)
    return;
  if (n > 0)
    {
      if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
	{
	  SetBrush (current_brush);
	  *pstream << "newpath\n";

	  float xx = points[0].x + xoffset;
	  float yy = (points[0].y + yoffset);
	  *pstream << XSCALE(xx) << " " << YSCALE(yy) << " moveto\n";
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

	  int i;
	  for (i = 1; i < n; i++)
	    {
	      xx = points[i].x + xoffset;
	      yy = (points[i].y + yoffset);
	      *pstream << XSCALE(xx) << " " << YSCALE(yy) << " lineto\n";
	      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	    }
	  *pstream << ((fillStyle == wxODDEVEN_RULE) ? "eofill\n" : "fill\n");
	}

      if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
	{
	  SetPen (current_pen);
	  *pstream << "newpath\n";

	  float xx = points[0].x + xoffset;
	  float yy = (points[0].y + yoffset);
	  *pstream << XSCALE(xx) << " " << YSCALE(yy) << " moveto\n";
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

	  int i;
	  for (i = 1; i < n; i++)
	    {
	      xx = points[i].x + xoffset;
	      yy = (points[i].y + yoffset);
	      *pstream << XSCALE(xx) << " " << YSCALE(yy) << " lineto\n";
	      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	    }

	  // Close the polygon
	  xx = points[0].x + xoffset;
	  yy = (points[0].y + yoffset);
	  *pstream << XSCALE(xx) << " " << YSCALE(yy) << " lineto\n";

	  // Output the line
	  *pstream << "stroke\n";
	}
    }
}

void wxPostScriptDC::DrawLines (int n, wxIntPoint points[], int xoffset, int yoffset)
{
  if (!pstream)
    return;
  if (n > 0)
    {
      if (current_pen)
	SetPen (current_pen);

      *pstream << "newpath\n";

      float xx = (float) (points[0].x + xoffset);
      float yy = (float) (points[0].y + yoffset);
      *pstream << XSCALE(xx) << " " << YSCALE(yy) << " moveto\n";
      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

      int i;
      for (i = 1; i < n; i++)
	{
	  xx = (float) (points[i].x + xoffset);
	  yy = (float) (points[i].y + yoffset);
	  *pstream << XSCALE(xx) << " " << YSCALE(yy) << " lineto\n";
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	}
      *pstream << "stroke\n";
    }
}

void wxPostScriptDC::DrawLines (int n, wxPoint points[], float xoffset, float yoffset)
{
  if (!pstream)
    return;
  if (n > 0)
    {
      if (current_pen)
	SetPen (current_pen);

      *pstream << "newpath\n";

      float xx = points[0].x + xoffset;
      float yy = (points[0].y + yoffset);
      *pstream << XSCALE(xx) << " " << YSCALE(yy) << " moveto\n";
      CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));

      int i;
      for (i = 1; i < n; i++)
	{
	  xx = points[i].x + xoffset;
	  yy = (points[i].y + yoffset);
	  *pstream << XSCALE(xx) << " " << YSCALE(yy) << " lineto\n";
	  CalcBoundingBox(XSCALEBND(xx), YSCALEBND(yy));
	}
      *pstream << "stroke\n";
    }
}

#ifdef wx_xt

void wxPostScriptDC::DrawLines(wxList *list, float xoffset, float yoffset)
{
  int n = list->Number();
  wxPoint *points = new wxPoint[n];

  int i = 0;
  for(wxNode *node = list->First(); node; node = node->Next()) {
    wxPoint *point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawLines(n, points, xoffset, yoffset);
  delete []points;
}

void wxPostScriptDC::DrawPolygon(wxList *list, float xoffset, float yoffset,int fillStyle)
{
  int n = list->Number();
  wxPoint *points = new wxPoint[n];

  int i = 0;
  for(wxNode *node = list->First(); node; node = node->Next()) {
    wxPoint *point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawPolygon(n, points, xoffset, yoffset,fillStyle);
  delete[] points;
}

#endif

void wxPostScriptDC::DrawRectangle (float x, float y, float width, float height)
{
  if (!pstream)
    return;
  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);

      *pstream << "newpath\n";
      *pstream << XSCALE(x) << " " << YSCALE (y) << " moveto\n";
      *pstream << XSCALE(x + width) << " " << YSCALE (y) << " lineto\n";
      *pstream << XSCALE(x + width) << " " << YSCALE (y + height) << " lineto\n";
      *pstream << XSCALE(x) << " " << YSCALE (y + height) << " lineto\n";
      *pstream << "closepath\n";
      *pstream << "fill\n";

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);

      *pstream << "newpath\n";
      *pstream << XSCALE(x) << " " << YSCALE (y) << " moveto\n";
      *pstream << XSCALE(x + width) << " " << YSCALE (y) << " lineto\n";
      *pstream << XSCALE(x + width) << " " << YSCALE (y + height) << " lineto\n";
      *pstream << XSCALE(x) << " " << YSCALE (y + height) << " lineto\n";
      *pstream << "closepath\n";
      *pstream << "stroke\n";

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width),  YSCALEBND(y + height));
    }
}

void wxPostScriptDC::DrawRoundedRectangle (float x, float y, float width, float height, float radius)
{
  if (!pstream)
    return;

  if (radius < 0.0)
    {
      // Now, a negative radius is interpreted to mean
      // 'the proportion of the smallest X or Y dimension'
      float smallest = 0.0;
      if (width < height)
	smallest = width;
      else
	smallest = height;
      radius = (float) (-radius * smallest);
    }

  float ascale = (user_scale_x < user_scale_y) ? user_scale_x : user_scale_y;

  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);
      // Draw rectangle anticlockwise
      *pstream << "newpath\n";

      *pstream << XSCALE(x) + ASCALEREL(radius) << " " << YSCALE(y) << " moveto\n";

      *pstream << XSCALE(x) + ASCALEREL(radius) << " " 
	<< YSCALE(y) - ASCALEREL(radius) << " " << ASCALEREL(radius) << " 90 180 arc\n";

      *pstream << XSCALE(x) + ASCALEREL(radius) << " " 
	<< YSCALE(y + height) + ASCALEREL(radius) << " " << ASCALEREL(radius) << " 180 270 arc\n";

      *pstream << XSCALE(x + width) - ASCALEREL(radius) << " " 
	<< YSCALE(y + height) + ASCALEREL(radius) << " " << ASCALEREL(radius) << " 270 0 arc\n";

      *pstream << XSCALE(x + width) - ASCALEREL(radius) << " " 
	<< YSCALE(y) - ASCALEREL(radius) << " " << ASCALEREL(radius) << " 0 90 arc\n";

      *pstream << "closepath\n";

      *pstream << "fill\n";

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);
      // Draw rectangle anticlockwise
      *pstream << "newpath\n";

      *pstream << XSCALE(x) + ASCALEREL(radius) << " " << YSCALE(y) << " moveto\n";

      *pstream << XSCALE(x) + ASCALEREL(radius) << " " 
	<< YSCALE(y) - ASCALEREL(radius) << " " << ASCALEREL(radius) << " 90 180 arc\n";

      *pstream << XSCALE(x) + ASCALEREL(radius) << " " 
	<< YSCALE(y + height) + ASCALEREL(radius) << " " << ASCALEREL(radius) << " 180 270 arc\n";

      *pstream << XSCALE(x + width) - ASCALEREL(radius) << " " 
	<< YSCALE(y + height) + ASCALEREL(radius) << " " << ASCALEREL(radius) << " 270 0 arc\n";

      *pstream << XSCALE(x + width) - ASCALEREL(radius) << " " 
	<< YSCALE(y) - ASCALEREL(radius) << " " << ASCALEREL(radius) << " 0 90 arc\n";

      *pstream << "closepath\n";

      *pstream << "stroke\n";

      CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
}

void wxPostScriptDC::DrawEllipse (float x, float y, float width, float height)
{
  if (!pstream)
    return;
  if (current_brush && current_brush->GetStyle () != wxTRANSPARENT)
    {
      SetBrush (current_brush);

      *pstream << "newpath\n";
      *pstream << XSCALE(x + width / 2) << " " << YSCALE(y + height / 2) << " ";
      *pstream << XSCALEREL(width / 2) << " " << YSCALEREL(height / 2) << " 0 360 ellipse\n";
      *pstream << "fill\n";

      CalcBoundingBox(XSCALEBND(x - width), YSCALEBND(y - height));
      CalcBoundingBox(XSCALEBND(x + width), YSCALEBND(y + height));
    }
  if (current_pen && current_pen->GetStyle () != wxTRANSPARENT)
    {
      SetPen (current_pen);

      *pstream << "newpath\n";
      *pstream << XSCALE(x + width / 2) << " " << YSCALE(y + height / 2) << " ";
      *pstream << XSCALEREL(width / 2) << " " << YSCALEREL(height / 2) << " 0 360 ellipse\n";
      *pstream << "stroke\n";

      CalcBoundingBox (XSCALEBND(x - width), YSCALEBND(y - height));
      CalcBoundingBox (XSCALEBND(x + width), YSCALEBND(y + height));
    }
}

void wxPostScriptDC::SetFont (wxFont * the_font)
{
  if (!pstream)
    return;
  if ((current_font == the_font) && !(resetFont & RESET_FONT))
    return;

  resetFont -= (resetFont & RESET_FONT);

  current_font = the_font;
  /* MATTHEW: [2] Use wxTheFontDirectory */
  char *name;
  int Family = current_font->GetFamily ();
  int Style = current_font->GetStyle ();
  int Weight = current_font->GetWeight ();

  name = wxTheFontNameDirectory.GetPostScriptName(Family, Weight, Style);
  if (!name)
    name = "Times-Roman";

  *pstream << "/" << name << " findfont\n";
  *pstream << YSCALEREL(current_font->GetPointSize()) << " scalefont setfont\n";
}

static void set_pattern(wxPostScriptDC *dc, PSStream *pstream, wxBitmap *bm, int rop, wxColour *col)
{
  int width, height;

  width = bm->GetWidth();
  height = bm->GetHeight();

  (*pstream 
   << "8 dict\n"
   << "dup\n"
   << "begin\n"
   << " /PatternType 1 def\n"
   << " /PaintType 1 def\n"
   << " /TilingType 1 def\n"
   << " /BBox [ 0 0 " << width << " " << height << " ] def\n"
   << " /XStep " << width << " def\n"
   << " /YStep " << height << " def\n");

  dc->Blit(0, 0, width, height, bm, 0, 0, -rop - 1, col);

  (*pstream << "end\n"
   << " matrix makepattern setpattern\n");
}

void wxPostScriptDC::SetPen (wxPen * pen)
{
  if (!pstream)
    return;
  wxPen *oldPen = current_pen;

  if (current_pen) current_pen->Lock(-1);
  if (pen) pen->Lock(1);

  if ((current_pen = pen) == NULL)
    return;			/* NIL */

  // Line width
  *pstream << pen->GetWidth () << " setlinewidth\n";

  if (level2ok) {
    wxBitmap *stipple = pen->GetStipple();
    if (stipple && stipple->Ok()) {
      set_pattern(this, pstream, stipple, pen->GetStyle(), &pen->GetColour());
      resetFont |= RESET_COLOR;
      return;
    }
  }

  // Line style - WRONG: 2nd arg is OFFSET
  /*
     Here, I'm afraid you do not conceive meaning of parameters of 'setdash'
     operator correctly. You should look-up this in the Red Book: the 2nd parame-
     ter is not number of values in the array of the first one, but an offset
     into this description of the pattern. I mean a real *offset* not index
     into array. I.e. If the command is [3 4] 1 setdash   is used, then there
     will be first black line *2* units long, then space 4 units, then the
     pattern of *3* units black, 4 units space will be repeated.
   */
  static char *dotted = "[2 5] 2";
  static char *short_dashed = "[4 4] 2";
  static char *long_dashed = "[4 8] 2";
  static char *dotted_dashed = "[6 6 2 6] 4";

  char *psdash = NULL;
  switch (pen->GetStyle ())
    {
    case wxDOT:
      psdash = dotted;
      break;
    case wxSHORT_DASH:
      psdash = short_dashed;
      break;
    case wxLONG_DASH:
      psdash = long_dashed;
      break;
    case wxDOT_DASH:
      psdash = dotted_dashed;
      break;
    case wxSOLID:
    case wxTRANSPARENT:
    default:
      psdash = "[] 0";
      break;
    }
  if (oldPen != pen)
    *pstream << psdash << " setdash\n";

  // Line colour
  unsigned char red = pen->GetColour ().Red ();
  unsigned char blue = pen->GetColour ().Blue ();
  unsigned char green = pen->GetColour ().Green ();

  if (!Colour)
    {
      // Anything not white is black
      if (!(red == (unsigned char) 255 && blue == (unsigned char) 255
	    && green == (unsigned char) 255))
	{
	  red = (unsigned char) 0;
	  green = (unsigned char) 0;
	  blue = (unsigned char) 0;
	}
    }

  if (!(red == currentRed && green == currentGreen && blue == currentBlue)
      || (resetFont & RESET_COLOR)) {
    float redPS = (float) (((int) red) / 255.0);
    float bluePS = (float) (((int) blue) / 255.0);
    float greenPS = (float) (((int) green) / 255.0);

    *pstream << redPS << " " << greenPS << " " << bluePS << " setrgbcolor\n";
    
    currentRed = red;
    currentBlue = blue;
    currentGreen = green;
    resetFont -= (resetFont & RESET_COLOR);
  }
}

static char *ps_brush_hatch[] = { " 0 0 moveto 8 8",
				  " 0 0 moveto 8 8 lineto closepath stroke 8 0 moveto 0 8",
				  " 8 0 moveto 0 8",
				  " 0 4 moveto 8 4 lineto closepath stroke 4 0 moveto 4 8",
				  " 0 4 moveto 8 4",
				  " 4 0 moveto 4 8" };

void wxPostScriptDC::SetBrush(wxBrush * brush)
{
  if (!pstream)
    return;

  if (current_brush) current_brush->Lock(-1);
  if (brush) brush->Lock(1);

  if ((current_brush = brush) == NULL)
    return; 

  if (level2ok) {
    wxBitmap *stipple = brush->GetStipple();
    if (stipple && stipple->Ok()) {
      set_pattern(this, pstream, stipple, brush->GetStyle(), &brush->GetColour());
      resetFont |= RESET_COLOR;
      return;
    }
  }

  // Brush colour
  unsigned char red = brush->GetColour().Red();
  unsigned char blue = brush->GetColour().Blue();
  unsigned char green = brush->GetColour().Green();

  if (!Colour) {
    // Anything not black is white
    if (!(red == (unsigned char) 0 && blue == (unsigned char) 0
	  && green == (unsigned char) 0)) {
      red = (unsigned char) 255;
      green = (unsigned char) 255;
      blue = (unsigned char) 255;
    }
  }

  int hatch_id = -1;
  switch (brush->GetStyle()) {
  case wxBDIAGONAL_HATCH:
    hatch_id = 0;
    break;
  case wxCROSSDIAG_HATCH:
    hatch_id = 1;
    break;
  case wxFDIAGONAL_HATCH:
    hatch_id = 2;
    break;
  case wxCROSS_HATCH:
    hatch_id = 3;
    break;
  case wxHORIZONTAL_HATCH:
    hatch_id = 4;
    break;
  case wxVERTICAL_HATCH:
    hatch_id = 5;
    break;
  }

  float redPS = (float) (((int) red) / 255.0);
  float bluePS = (float) (((int) blue) / 255.0);
  float greenPS = (float) (((int) green) / 255.0);

  if (hatch_id > -1) {
    (*pstream 
     << "7 dict\n"
     << "dup\n"
     << "begin\n"
     << " /PatternType 1 def\n"
     << " /PaintType 1 def\n"
     << " /TilingType 1 def\n"
     << " /BBox [ 0 0 8 8 ] def\n"
     << " /XStep 8 def\n"
     << " /YStep 8 def\n"
     << " /PaintProc { begin gsave \n");

    *pstream << " 0 setlinewidth\n";
    *pstream << " [] 0 setdash\n";
    *pstream << " " << redPS << " " << greenPS << " " << bluePS << " setrgbcolor\n";

    *pstream << " " << ps_brush_hatch[hatch_id] << " lineto closepath stroke \n";
    
    *pstream << "grestore\n } def \n";
    
    *pstream << "end\n" << " matrix makepattern setpattern\n";

    resetFont |= RESET_COLOR;

    return;
  }

  if (!(red == currentRed && green == currentGreen && blue == currentBlue)
      || (resetFont & RESET_COLOR)) {
    *pstream << redPS << " " << greenPS << " " << bluePS << " setrgbcolor\n";
    currentRed = red;
    currentBlue = blue;
    currentGreen = green;
    resetFont -= (resetFont & RESET_COLOR);
  }
}

void wxPostScriptDC::DrawText (DRAW_TEXT_CONST char *text, float x, float y,
			       Bool WXUNUSED(use16))
{
  if (!pstream)
    return;
  if (current_font)
    SetFont (current_font);

  float tw, th;
  GetTextExtent(text, &tw, &th);

  if (current_bk_mode == wxSOLID) {
    unsigned char red = current_text_background.Red();
    unsigned char blue = current_text_background.Blue();
    unsigned char green = current_text_background.Green();
    float redPS = (float) (((int) red) / 255.0);
    float bluePS = (float) (((int) blue) / 255.0);
    float greenPS = (float) (((int) green) / 255.0);

    *pstream << "gsave newpath\n";
    *pstream << redPS << " " << greenPS << " " << bluePS << " setrgbcolor\n";
    *pstream << XSCALE(x) << " " << YSCALE (y) << " moveto\n";
    *pstream << XSCALE(x + tw) << " " << YSCALE (y) << " lineto\n";
    *pstream << XSCALE(x + tw) << " " << YSCALE (y + th) << " lineto\n";
    *pstream << XSCALE(x) << " " << YSCALE (y + th) << " lineto\n";
    *pstream << "closepath\n";
    *pstream << "fill grestore\n";
  }

  if (current_text_foreground.Ok ()) {
    unsigned char red = current_text_foreground.Red ();
    unsigned char blue = current_text_foreground.Blue ();
    unsigned char green = current_text_foreground.Green ();
    
    if (!Colour) {
      // Anything not white is black
      if (!(red == (unsigned char) 255 && blue == (unsigned char) 255
	    && green == (unsigned char) 255))
	{
	  red = (unsigned char) 0;
	  green = (unsigned char) 0;
	  blue = (unsigned char) 0;
	}
    }
    if (!(red == currentRed && green == currentGreen && blue == currentBlue)
	|| (resetFont & RESET_COLOR)) {
      float redPS = (float) (((int) red) / 255.0);
      float bluePS = (float) (((int) blue) / 255.0);
      float greenPS = (float) (((int) green) / 255.0);
      *pstream << redPS << " " << greenPS << " " << bluePS << " setrgbcolor\n";
      
      currentRed = red;
      currentBlue = blue;
      currentGreen = green;
      resetFont -= (resetFont & RESET_COLOR);
    }
  }
  
  int size = 10;
  if (current_font)
    size = current_font->GetPointSize();

  *pstream << XSCALE(x) << " " << YSCALE (y + size) << " moveto\n";

  *pstream << "(";
  int len = strlen (text);
  int i;
  for (i = 0; i < len; i++)
    {
      char ch = text[i];
      if (ch == ')' || ch == '(' || ch == '\\')
	*pstream << "\\";
      *pstream << ch;
    }

  *pstream << ")" << " show\n";

  CalcBoundingBox(XSCALEBND(x), YSCALEBND(y));
  CalcBoundingBox(XSCALEBND(x + tw), YSCALEBND(y + th));
}


void wxPostScriptDC::SetBackground (wxColour * c)
{
  current_background_color = *c;
}

void wxPostScriptDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

void wxPostScriptDC::SetTextBackground(wxColour *col)
{
  current_text_background = *col;  
}

void wxPostScriptDC::SetTextForeground(wxColour *col)
{
  current_text_foreground = *col;
}

void wxPostScriptDC::TryColour(wxColour *src, wxColour *dest)
{
  if (!Colour) {
    if ((src->Red() == 255)
	&& (src->Green() == 255)
	&& (src->Blue() == 255))
      dest->Set(255, 255, 255);
    else
      dest->Set(0, 0, 0);
  } else
    *dest = *src;
}

static const char *wxPostScriptHeaderEllipse = "\
/ellipsedict 8 dict def\n\
ellipsedict /mtrx matrix put\n\
/ellipse\n\
{ ellipsedict begin\n\
  /endangle exch def\n\
  /startangle exch def\n\
  /yrad exch def\n\
  /xrad exch def\n\
  /y exch def\n\
  /x exch def\n\
  /savematrix mtrx currentmatrix def\n\
  x y translate\n\
  xrad yrad scale\n\
  0 0 1 endangle startangle arcn\n\
  savematrix setmatrix\n\
  end\n\
  } def\n\
";

Bool wxPostScriptDC::StartDoc (char *message)
{
  if (device == wxDEVICE_EPS) {
    pstream = new PSStream(filename);

    if (!pstream || !pstream->good()) {
      ok = FALSE;
      pstream = NULL;
      return FALSE;
    }
    ok = TRUE;
  }

  *pstream << "%!PS-Adobe-2.0 EPSF-2.0\n";	/* PostScript magic strings */
  if (title)
    *pstream << "%%Title: " << title << "\n";
  *pstream << "%%Creator: " << "wxWindows (MrEd)" << "\n";
  *pstream << "%%CreationDate: " << wxNow() << "\n";

  // User Id information
  char userID[256];
  if (wxGetEmailAddress(userID, sizeof(userID))) {
    *pstream << "%%For: " << (char *)userID;
    char userName[245];
    if (wxGetUserName(userName, sizeof(userName)))
      *pstream << " (" << (char *)userName << ")";
    *pstream << "\n";
  } else if ( wxGetUserName(userID, sizeof(userID))) {
    *pstream << "%%For: " << (char *)userID << "\n";
  }

  boundingboxpos = pstream->tellp();

  *pstream << "%%BoundingBox: -0000 -0000 -0000 -0000\n";
  *pstream << "%%Pages: -0000\n";
  *pstream << "%%EndComments\n\n";

  *pstream << wxPostScriptHeaderEllipse;

  if (wxPostScriptHeaderSpline)
    *pstream << wxPostScriptHeaderSpline;

  SetBrush(wxWHITE_BRUSH);
  SetPen(wxBLACK_PEN);

  page_number = 1;
  if (message)
    title = copystring (message);

  return TRUE;
}

#ifdef wx_x
extern void wxsExecute(char **);
#endif

void wxPostScriptDC::EndDoc (void)
{
  if (!pstream)
    return;
  if (clipping) {
    clipping = FALSE;
    *pstream << "grestore\n";
  }

  // THE FOLLOWING HAS BEEN CONTRIBUTED BY Andy Fyfe <andy@hyperparallel.com>

  // Compute the bounding box.  Note that it is in the default user
  // coordinate system, thus we have to convert the values.
  float llx;
  float lly;
  float urx;
  float ury;

  // If we're landscape, our sense of "x" and "y" is reversed.
  if (landscape) {
    llx = min_y * paper_y_scale + paper_y;
    lly = min_x * paper_x_scale + paper_x;
    urx = max_y * paper_y_scale + paper_y;
    ury = max_x * paper_x_scale + paper_x;
  } else {
    llx = min_x * paper_x_scale + paper_x;
    lly = paper_h * paper_y_scale - (max_y * paper_y_scale) + paper_y;
    urx = max_x * paper_x_scale + paper_x;
    ury = paper_h * paper_y_scale - (min_y * paper_y_scale) + paper_y;
  }

  // The Adobe specifications call for integers; we round as to make
  // the bounding larger.
  pstream->seekp(boundingboxpos);
  *pstream << "%%BoundingBox: ";
  pstream->width(5);
  *pstream << floor(llx) << " ";
  pstream->width(5);
  *pstream << floor(lly) << " ";
  pstream->width(5);
  *pstream << ceil(urx)  << " ";
  pstream->width(5);
  *pstream << ceil(ury) << "\n";
  *pstream << "%%Pages: ";
  pstream->width(5);
  *pstream << (page_number - 1) << "\n";

  delete pstream;
  pstream = NULL;

#ifdef wx_x
  if (ok /* && wx_interactive */)
    {
      switch (mode) {
	case PS_PREVIEW:
	{
          char *argv[3];
	  argv[0] = preview_cmd;
          argv[1] = filename;
          argv[2] = NULL;
	  wxsExecute (argv);
	}
	break;

	case PS_PRINTER:
	{
          char *argv[4];
          /* MATTHEW: [7] Print - not preview */
          argv[0] = print_cmd;
	  /* MATTHEW: [9] Use options only if it's not "" */
	  int i = 1;
	  char *opts = print_opts;
	  if (opts && *opts)
	    argv[i++] = opts;
	  argv[i++] = filename;
	  argv[i] = NULL;
	  wxsExecute(argv);
	}
	break;

	case PS_FILE:
	  break;
	}
    }
#endif
}

void wxPostScriptDC::StartPage (void)
{
  if (!pstream)
    return;
  *pstream << "%%Page: " << page_number++ << "\n";

  *pstream << (paper_x + (landscape ? (paper_h * paper_y_scale) : 0)) 
    << " " << paper_y << " translate\n";
  if (landscape) {
    *pstream << paper_y_scale << " " << paper_x_scale << " scale\n";
    *pstream << "90 rotate\n";
  } else {
    *pstream << paper_x_scale << " " << paper_y_scale << " scale\n";
  }
  *pstream << "2 setlinecap\n";

  resetFont = RESET_FONT | RESET_COLOR;

  if (clipping)
    SetClippingRegion(clipping);
}

void wxPostScriptDC::EndPage (void)
{
  if (!pstream)
    return;
  *pstream << "showpage\n";
}


static void printhex(PSStream *pstream, int v)
{
  int h, l;
  char s[3];

  s[2] = 0;
  
  h = (v >> 4) & 0xF;
  l = v & 0xF;
  
  if (h <= 9)
    s[0] = '0' + h;
  else
    s[0] = 'a' + (h - 10);
  if (l <= 9)
    s[1] = '0' + l;
  else
    s[1] = 'a' + (l - 10);

  *pstream << s;
}


Bool wxPostScriptDC::
Blit (float xdest, float ydest, float fwidth, float fheight,
      wxMemoryDC *src, float xsrc, float ysrc, int rop, wxColour *dcolor)
{
  if (!pstream)
    return FALSE;

  wxCanvasDC *source = (wxCanvasDC *)src;
  long width, height, x, y;
  Bool asColour = level2ok;

  width = (long)floor(fwidth);
  height = (long)floor(fheight);
  x = (long)floor(xsrc);
  y = (long)floor(ysrc);

  /* Allocate space: */
  (*pstream << "/DataString " 
   << (width * (asColour ? 3 : 1) * ((rop < 0) ? height : 1))
   << " string def\n");

  if (rop < 0) {
    *pstream << " /PaintProc { begin \n";
  }

  /* PostScript setup: */
  *pstream << "gsave\n";
  if (rop >= 0) {
    *pstream << XSCALE(xdest) << " " << YSCALE(ydest) - fheight << " translate\n";
  }
  *pstream << fwidth << " " << fheight << " scale\n";
  *pstream << width << " " << height << " 8 [ ";
  *pstream << width << " 0 0 " << (-height) << " 0 " << height;
  *pstream << " ]\n";
  if (rop >= 0) {
    *pstream << "{\n";
    *pstream << "  currentfile DataString readhexstring pop\n";
    *pstream << "} bind";
  } else {
    *pstream << " { DataString } ";
  }
  if (asColour) {
    *pstream << " false 3 colorimage\n";
  } else {
    *pstream << " image\n";
  }
  
  if (rop < 0) {
    (*pstream << "grestore\n } def \n"
     << " { currentfile DataString readhexstring pop pop } exec\n");
  }

  /* Output data as hex digits: */
  int mono;
  long j, i;
  wxColour c;
  int pixel;
  int pr, pg, pb;

  mono = (src->GetObject()->GetDepth() == 1);

  if (mono && dcolor) {
    pr = dcolor->Red();
    pg = dcolor->Green();
    pb = dcolor->Blue();
  } else
    pr = pg = pb = 0;

  for (j = 0; j < height; j++) {
    for (i = 0; i < width; i++) {
      source->GetPixel(i, j, &c);

      int red, green, blue;
      red = c.Red();
      green = c.Green();
      blue = c.Blue();

      if (mono && !red && !green && !blue) {
	red = pr;
	green = pg;
	blue = pb;
      } else if (mono) {
	if ((rop != wxSOLID) && (rop != (-wxSOLID - 1))) {
	  red = current_background_color.Red();
	  green = current_background_color.Green();
	  blue = current_background_color.Blue();
	}
      }

      if (asColour) {
	printhex(pstream, red);
	printhex(pstream, green);
	printhex(pstream, blue);
      } else {
	float r, g, b;

	r = ((float)(red) / 255);
	g = ((float)(green) / 255);
	b = ((float)(blue) / 255);

	pixel = (int)(255 * sqrt(((r * r) + (g * g) + (b * b)) / 3));
	
	printhex(pstream, pixel);
      }
    }
    *pstream << "\n";
  }

  if (rop >= 0) {
    *pstream << "grestore\n";
  }

  if (rop >= 0) {
    CalcBoundingBox(XSCALEBND(xdest), YSCALEBND(ydest));
    /* Bitmap isn't scaled: */
    CalcBoundingBox(XSCALEBND(xdest) + fwidth, YSCALEBND(ydest) + fheight);
  }

  return TRUE;
}

static wxMemoryDC *temp_mdc;

Bool wxPostScriptDC::Blit (float xdest, float ydest, float fwidth, float fheight,
      wxBitmap *bm, float xsrc, float ysrc, int rop, wxColour *c)
{
  if (!temp_mdc)
    temp_mdc = new wxMemoryDC(1);

  temp_mdc->SelectObject(bm);
  Bool v = Blit(xdest, ydest, fwidth, fheight,
		temp_mdc, xsrc, ysrc, rop, c);
  temp_mdc->SelectObject(NULL);

  return v;
}

float wxPostScriptDC::GetCharHeight (void)
{
  if (current_font)
    return (float) current_font->GetPointSize ();
  else
    return 12.0;
}


float wxPostScriptDC::GetCharWidth (void)
{
  return 0;
}

void wxPostScriptDC::GetTextExtent (const char *string, float *x, float *y,
				    float *descent, float *externalLeading, wxFont *theFont,
				    Bool WXUNUSED(use16))
{
  wxFont *fontToUse = theFont;
  if (!fontToUse)
    fontToUse = current_font;
    
  if (!pstream)
    return;
#if !USE_AFM_FOR_POSTSCRIPT
  // Provide a VERY rough estimate (avoid using it)
  int width = 12;
  int height = 12;

  if (fontToUse)
    {
      height = fontToUse->GetPointSize ();
      width = height;
    }
  *x = (float) strlen (string) * width;
  *y = (float) height;
  if (descent)
    *descent = 0.0;
  if (externalLeading)
    *externalLeading = 0.0;
#else
  // +++++ start of contributed code +++++
  
  // ************************************************************
  // method for calculating string widths in postscript:
  // read in the AFM (adobe font metrics) file for the
  // actual font, parse it and extract the character widths
  // and also the descender. this may be improved, but for now
  // it works well. the AFM file is only read in if the
  // font is changed. this may be chached in the future.
  // calls to GetTextExtent with the font unchanged are rather
  // efficient!!!
  //
  // for each font and style used there is an AFM file necessary.
  // currently i have only files for the roman font family.
  // i try to get files for the other ones!
  //
  // CAVE: the size of the string is currently always calculated
  //       in 'points' (1/72 of an inch). this should later on be
  //       changed to depend on the mapping mode.
  // CAVE: the path to the AFM files must be set before calling this
  //       function. this is usually done by a call like the following:
  //       wxSetAFMPath("d:\\wxw161\\afm\\");
  //
  // example:
  //
  //    wxPostScriptDC dc(NULL, TRUE);
  //    if (dc.Ok()){
  //      wxSetAFMPath("d:\\wxw161\\afm\\");
  //      dc.StartDoc("Test");
  //      dc.StartPage();
  //      float w,h;
  //      dc.SetFont(new wxFont(10, wxROMAN, wxNORMAL, wxNORMAL));
  //      dc.GetTextExtent("Hallo",&w,&h);
  //      dc.EndPage();
  //      dc.EndDoc();
  //    }
  //
  // by steve (stefan.hammes@urz.uni-heidelberg.de)
  // created: 10.09.94
  // updated: 14.05.95

  assert(fontToUse && "void wxPostScriptDC::GetTextExtent: no font defined");
  assert(x && "void wxPostScriptDC::GetTextExtent: x == NULL");
  assert(y && "void wxPostScriptDC::GetTextExtent: y == NULL");

  // these static vars are for storing the state between calls
  static int lastFamily= INT_MIN;
  static int lastSize= INT_MIN;
  static int lastStyle= INT_MIN;
  static int lastWeight= INT_MIN;
  static int lastDescender = INT_MIN;
  static int lastWidths[256]; // widths of the characters

  // get actual parameters
  const int Family = fontToUse->GetFamily();
  const int Size =   fontToUse->GetPointSize();
  const int Style =  fontToUse->GetStyle();
  const int Weight = fontToUse->GetWeight();

  // if we have another font, read the font-metrics
  if(Family!=lastFamily||Size!=lastSize||Style!=lastStyle||Weight!=lastWeight){
    // store actual values
    lastFamily = Family;
    lastSize =   Size;
    lastStyle =  Style;
    lastWeight = Weight;

    // read in new font metrics **************************************

    // 1. construct filename ******************************************
    /* MATTHEW: [2] Use wxTheFontNameDirectory */
    char *name;

    name = wxTheFontNameDirectory.GetAFMName(Family, Weight, Style);
    if (!name)
      name = "unknown";

    // get the directory of the AFM files
    char afmName[1024];
    afmName[0] = 0;
    if (afm_path)
      strncpy(afmName, afm_path, 1024);

    // 2. open and process the file **********************************

    // a short explanation of the AFM format:
    // we have for each character a line, which gives its size
    // e.g.:
    //
    //   C 63 ; WX 444 ; N question ; B 49 -14 395 676 ;
    //
    // that means, we have a character with ascii code 63, and width 
    // (444/1000 * fontSize) points.
    // the other data is ignored for now!
    //
    // when the font has changed, we read in the right AFM file and store the
    // character widths in an array, which is processed below (see point 3.).

    strncat(afmName, "/", 1024);
    strncat(afmName, name, 1024);
    strncat(afmName,".afm", 1024);
    afmName[1023] = 0;
    FILE *afmFile = fopen(afmName,"r");
    if(afmFile==NULL){
      wxDebugMsg("GetTextExtent: can't open AFM file '%s'\n",afmName);
      wxDebugMsg("               using approximate values\n");
      int i;
      for (i=0; i<256; i++) lastWidths[i] = 500; // an approximate value
      lastDescender = -150; // dito.
    }else{
      int i;
      // init the widths array
      for(i=0; i<256; i++) lastWidths[i]= INT_MIN;
      // some variables for holding parts of a line
      char cString[10],semiString[10],WXString[10],descString[20];
      char line[256];
      int ascii,cWidth;
      // read in the file and parse it
      while(fgets(line,sizeof(line),afmFile)!=NULL){
        // A.) check for descender definition
        if(strncmp(line,"Descender",9)==0){
          if((sscanf(line,"%s%d",descString,&lastDescender)!=2)
	     || (strcmp(descString,"Descender")!=0)) {
	    wxDebugMsg("AFM-file '%s': line '%s' has error (bad descender)\n",
		       afmName,line);
          }
	// B.) check for char-width
        }else if(strncmp(line,"C ",2)==0){
          if(sscanf(line,"%s%d%s%s%d",
              cString,&ascii,semiString,WXString,&cWidth)!=5){
             wxDebugMsg("AFM-file '%s': line '%s' has an error (bad character width)\n",afmName,line);
          }
          if(strcmp(cString,"C")!=0 || strcmp(semiString,";")!=0 ||
             strcmp(WXString,"WX")!=0){
             wxDebugMsg("AFM-file '%s': line '%s' has a format error\n",afmName,line);
          }
          //printf("            char '%c'=%d has width '%d'\n",ascii,ascii,cWidth);
          if(ascii>=0 && ascii<256){
            lastWidths[ascii] = cWidth; // store width
          }else{
	    /* MATTHEW: this happens a lot; don't print an error */
            // wxDebugMsg("AFM-file '%s': ASCII value %d out of range\n",afmName,ascii);
          }
        }
        // C.) ignore other entries.
      }
      fclose(afmFile);
    }
  }
  
  // 3. now the font metrics are read in, calc size *******************
  // this is done by adding the widths of the characters in the
  // string. they are given in 1/1000 of the size!

  float widthSum=0.0;
  float height=(float)Size; // by default
  unsigned char *p;
  for(p=(unsigned char *)string; *p; p++){
    if(lastWidths[*p]== INT_MIN){
      wxDebugMsg("GetTextExtent: undefined width for character '%c' (%d)\n",
                 *p,*p);
      widthSum += lastWidths[' ']; // assume space
    }else{
      widthSum += (lastWidths[*p]/1000.0F)*Size;
    }
  }
  // add descender to height (it is usually a negative value)
  if(lastDescender!=INT_MIN){
    height += ((-lastDescender)/1000.0F) * Size; /* MATTHEW: forgot scale */
  }
  
  // return size values
  *x = widthSum;
  *y = height;

  // return other parameters
  if (descent){
    if(lastDescender!=INT_MIN){
      *descent = ((-lastDescender)/1000.0F) * Size; /* MATTHEW: forgot scale */
    }else{
      *descent = 0.0;
    }
  }

  // currently no idea how to calculate this!
  // if (externalLeading) *externalLeading = 0.0;
  if (externalLeading)
    *externalLeading = 0.0;

  // ----- end of contributed code -----
#endif
}

void wxPostScriptDC::SetMapMode (int WXXTUNUSED(mode))
{
#ifndef wx_xt
  mapping_mode = mode;
#endif
  return;
}

void wxPostScriptDC::SetUserScale (float x, float y)
{
  user_scale_x = x;
  user_scale_y = y;
}

float wxPostScriptDC::DeviceToLogicalX(int x)
{
  return (x - device_origin_x) / user_scale_x;
}

float wxPostScriptDC::DeviceToLogicalXRel(int x)
{
  return x / user_scale_x;
}

float wxPostScriptDC::DeviceToLogicalY(int y)
{
  float y2 = -(y - paper_h);
  return (y2 - device_origin_y) / user_scale_y;
}

float wxPostScriptDC::DeviceToLogicalYRel(int y)
{
  return y / user_scale_y;
}

int wxPostScriptDC::LogicalToDeviceX(float x)
{
  return (int)floor(XSCALE(x));
}

int wxPostScriptDC::LogicalToDeviceXRel(float x)
{
  return (int)floor(XSCALEREL(x));
}

int wxPostScriptDC::LogicalToDeviceY(float y)
{
  return (int)floor(YSCALE(y));
}

int wxPostScriptDC::LogicalToDeviceYRel(float y)
{
  return (int)floor(YSCALEREL(y));
}

void wxPostScriptDC::GetSize(float *width, float *height)
{
  if (width)
    *width = paper_w;
  if (height)
    *height = paper_h;
}

void wxPostScriptDC::GetSizeMM(float *WXUNUSED(width), float *WXUNUSED(height))
{
#if 0
  char *paperType = wxThePrintSetupData->GetPaperName();
  if (!paperType)
    paperType = DEFAULT_PAPER;

  wxPrintPaperType *paper = wxThePrintPaperDatabase->FindPaperType(paperType);
  if (!paper)
    paper = wxThePrintPaperDatabase->FindPaperType(DEFAULT_PAPER);
  if (paper) {
    *width = (float)paper->widthMM;
    *height = (float)paper->heightMM;
  }
  else {
    *width = 1000;
    *height = 1000;
  }
#endif
}

extern Bool wxsPrinterDialog(wxWindow *parent);

Bool 
XPrinterDialog (wxWindow *parent)
{
  return wxsPrinterDialog(parent);
}

//-----------------------------------------------------------------------------
// wxPrintSetup implementation
//-----------------------------------------------------------------------------

#define PS_DEFAULT_PAPER  "Letter 8 1/2 x 11 in"

#if defined(sun) && defined(wx_xview)
#	define PS_PREVIEW_COMMAND	"pageview"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#elif defined(VMS)
#	define PS_PREVIEW_COMMAND	"view/format=ps/select=x_display"
#	define PS_PRINTER_COMMAND	"print"
#	define PS_PRINTER_OPTIONS	"/nonotify/queue=psqueue"
#	define PS_AFM_PATH		"sys$ps_font_metrics:"
#elif defined(__sgi)
#	define PS_PREVIEW_COMMAND	"dps"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#elif defined(wx_x)
#	define PS_PREVIEW_COMMAND	"ghostview"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#elif defined(wx_msw) || defined(wx_mac)
#	define PS_PREVIEW_COMMAND	"ghostview"
#	define PS_PRINTER_COMMAND	"print"
#	define PS_PRINTER_OPTIONS	""
#	define PS_AFM_PATH		NULL
#else
#	define PS_PREVIEW_COMMAND	NULL
#	define PS_PRINTER_COMMAND	NULL
#	define PS_PRINTER_OPTIONS	NULL
#	define PS_AFM_PATH		NULL
#endif

IMPLEMENT_DYNAMIC_CLASS(wxPrintSetupData, wxObject)

wxPrintSetupData::wxPrintSetupData(void)
{
    printer_command = PS_PRINTER_COMMAND;
    preview_command = PS_PREVIEW_COMMAND;
    printer_flags = PS_PRINTER_OPTIONS;
    printer_orient = PS_PORTRAIT;
    printer_scale_x = 1.0;
    printer_scale_y = 1.0;
    printer_translate_x = 0.0;
    printer_translate_y = 0.0;
#ifdef wx_x
    printer_mode = PS_PRINTER;
#else
    printer_mode = PS_FILE;
#endif
    afm_path = default_afm_path;
    paper_name = DEFAULT_PAPER;
    print_colour = TRUE;
    print_level_2 = TRUE;
    printer_file = NULL;
    emargin_v = emargin_h = 36;
}

wxPrintSetupData::~wxPrintSetupData(void)
{
    if (printer_command)
	delete[] printer_command;
    if (preview_command)
	delete[] preview_command;
    if (printer_flags)
	delete[] printer_flags;
    if (afm_path)
	delete[] afm_path;
    if (paper_name)
	delete[] paper_name;
    if (printer_file)
	delete[] printer_file;
}

void wxPrintSetupData::SetPrinterCommand(char *cmd)
{
    if (cmd == printer_command)
	return;
    if (printer_command)
	delete[] printer_command;
    if (cmd)
	printer_command = copystring(cmd);
    else
	printer_command = NULL;
}

void wxPrintSetupData::SetPrintPreviewCommand(char *cmd)
{
    if (cmd == preview_command)
	return;
    if (preview_command)
	delete[] preview_command;
    if (cmd)
	preview_command = copystring(cmd);
    else
	preview_command = NULL;
}

void wxPrintSetupData::SetPaperName(char *name)
{
    if (name == paper_name)
	return;
    if (paper_name)
	delete[] paper_name;
    if (name)
	paper_name = copystring(name);
    else
	paper_name = NULL;
}

void wxPrintSetupData::SetPrinterOptions(char *flags)
{
    if (printer_flags == flags)
	return;
    if (printer_flags)
	delete[] printer_flags;
    if (flags)
	printer_flags = copystring(flags);
    else
	printer_flags = NULL;
}

void wxPrintSetupData::SetPrinterFile(char *f)
{
    if (f == printer_file)
	return;
    if (printer_file)
	delete[] printer_file;
    if (f)
	printer_file = copystring(f);
    else
	printer_file = NULL;
}

void wxPrintSetupData::SetPrinterMode(int mode)
{
    printer_mode = PS_FILE;

    if (mode == PS_PREVIEW && preview_command
    ||  mode == PS_PRINTER && printer_command)
	printer_mode = mode;
}

void wxPrintSetupData::SetAFMPath(char *f)
{
    if (f && !default_afm_path)
      default_afm_path = f;
  
    if (f == afm_path)
	return;
    if (afm_path)
	delete[] afm_path;
    if (f)
	afm_path = copystring(f);
    else
	afm_path = NULL;
}

void wxPrintSetupData::operator=(wxPrintSetupData& data)
{
    float x, y;

    SetPrinterCommand(data.GetPrinterCommand());
    SetPrintPreviewCommand(data.GetPrintPreviewCommand());
    SetPrinterOptions(data.GetPrinterOptions());
    SetPrinterOrientation(data.GetPrinterOrientation());
    SetPrinterMode(data.GetPrinterMode());
    SetAFMPath(data.GetAFMPath());
    SetPaperName(data.GetPaperName());
    SetColour(data.GetColour());

    data.GetPrinterTranslation(&x, &y);
    SetPrinterTranslation(x, y);
    data.GetPrinterScaling(&x, &y);
    SetPrinterScaling(x, y);
}

void wxPrintSetupData::copy(wxPrintSetupData& data)
{
  *this = data;
}

//-----------------------------------------------------------------------------
// wxInitializePrintSetupData
//-----------------------------------------------------------------------------

void wxInitializePrintSetupData(Bool /* init */)
{
#ifdef wx_mac
  wxThePrintPaperDatabase = new wxPrintPaperDatabase;
  wxThePrintPaperDatabase->CreateDatabase();
#endif
  
  wxPrintSetupData *wxThePrintSetupData;
  
  wxThePrintSetupData = new wxPrintSetupData;
  
  wxThePrintSetupData->SetPrintPreviewCommand(PS_PREVIEW_COMMAND);
  wxThePrintSetupData->SetPrinterOrientation(PS_PORTRAIT);
  wxThePrintSetupData->SetPrinterMode(PS_PREVIEW);
  wxThePrintSetupData->SetPaperName(PS_DEFAULT_PAPER);
  wxThePrintSetupData->SetPrinterCommand(PS_PRINTER_COMMAND);
  wxThePrintSetupData->SetPrinterOptions(PS_PRINTER_OPTIONS);
  wxThePrintSetupData->SetAFMPath(PS_AFM_PATH);
  
  wxSetThePrintSetupData(wxThePrintSetupData);
}

//-----------------------------------------------------------------------------
// wxPrintPaperType implementation
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxPrintPaperType, wxObject)

wxPrintPaperType::wxPrintPaperType(char *name, int wmm, int hmm, int wp, int hp)
{
    widthMM = wmm;
    heightMM = hmm;
    widthPixels = wp;
    heightPixels = hp;
    pageName = copystring(name);
}

wxPrintPaperType::~wxPrintPaperType(void)
{
    delete[] pageName;
}


//-----------------------------------------------------------------------------
// wxPrintPaperDatabase implementation
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxPrintPaperDatabase, wxList)

wxPrintPaperDatabase::wxPrintPaperDatabase(void) : wxList(wxKEY_STRING)
{
    DeleteContents(TRUE);
}

wxPrintPaperDatabase::~wxPrintPaperDatabase(void)
{
}

void wxPrintPaperDatabase::CreateDatabase(void)
{
    // Need correct values for page size in pixels.
    // Each unit is one 'point' = 1/72 of an inch.
    // NOTE: WE NEED ALSO TO MAKE ADJUSTMENTS WHEN TRANSLATING
    // in wxPostScriptDC code, so we can start from top left.
    // So access this database and translate by appropriate number
    // of points for this paper size. OR IS IT OK ALREADY?
    // Can't remember where the PostScript origin is by default.
    // Heck, someone will know how to make it hunky-dory...
    // JACS 25/5/95
  
    AddPaperType("A4 210 x 297 mm", 210, 297,         595, 842);
    AddPaperType("A3 297 x 420 mm", 297, 420,         842, 1191);
    AddPaperType("Letter 8 1/2 x 11 in", 216, 279,    612, 791);
    AddPaperType("Legal 8 1/2 x 14 in", 216, 356,     612, 1009);
}

void wxPrintPaperDatabase::ClearDatabase(void)
{
    Clear();
}

void wxPrintPaperDatabase::AddPaperType(char *name, int wmm, int hmm,
					int wp, int hp)
{
    Append(name, new wxPrintPaperType(name, wmm, hmm, wp, hp));
}

wxPrintPaperType *wxPrintPaperDatabase::FindPaperType(char *name)
{
  wxNode *node;

    if ((node = Find(name)))
	return (wxPrintPaperType*)node->Data();
    else
	return NULL;
}

#endif
