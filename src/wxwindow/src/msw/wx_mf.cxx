/*
 * File:	wx_mf.cc
 * Purpose:	Metafiles and metafile DCs
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_mf.cxx,v 1.3 1998/09/21 05:21:17 mflatt Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_setup.h"

#endif

#if USE_METAFILE

#include "wx_mf.h"

#if !defined(_MSC_VER)
# include "wx_utils.h"
# include "wx_canvs.h"
# include "wx_main.h"
# include "wx_privt.h"
#endif

#include <stdio.h>

extern Bool wxClipboardIsOpen;

/*
 * Metafiles - Windows 3.1 only
 * Currently, the only purpose for making a metafile is to put
 * it on the clipboard.
 */

IMPLEMENT_DYNAMIC_CLASS(wxMetaFile, wxObject)

wxMetaFile::wxMetaFile(char *file)
{
  metafile = NULL;
  if (file)
  {
    metafile = GetMetaFile(file);
  }
}

wxMetaFile::~wxMetaFile(void)
{
  if (metafile)
    { DeleteMetaFile((HMETAFILE)metafile); metafile = NULL; }
}

Bool wxMetaFile::SetClipboard(int width, int height)
{
  HANDLE data = GlobalAlloc(GHND, sizeof(METAFILEPICT) + 1);
  METAFILEPICT *mf = (METAFILEPICT *)GlobalLock(data);

  mf->mm = MM_TEXT;
  mf->mm = MM_ANISOTROPIC; // steve, 27.09.94
// ABOVE MOD (TEXT->ANISOTROPIC) YET TO BE TESTED BY JACS.
  mf->xExt = width;
  mf->yExt = height;
  mf->hMF = (HMETAFILE)metafile;
  GlobalUnlock(data);
  metafile = NULL;

  wxFrame *frame = wxTheApp->wx_frame;
  if (!frame)
    return FALSE;

  wxWnd *wnd = (wxWnd *)frame->handle;
  Bool alreadyOpen = wxClipboardIsOpen;
  if (!alreadyOpen)
  {
    OpenClipboard(wnd->handle);
    EmptyClipboard();
  }
  HANDLE success = SetClipboardData(CF_METAFILEPICT, data);
  if (!alreadyOpen)
    CloseClipboard();
  return (Bool)success;
}

Bool wxMetaFile::Play(wxDC *dc)
{
  HDC hDC = 0;
  wxWnd *wnd = NULL;
  if (dc->canvas) wnd = (wxWnd *)dc->canvas->handle;
  if (dc->cdc)
    hDC = dc->cdc;
  else if (wnd)
    hDC = GetDC(wnd->handle);

  if (hDC && metafile)
    PlayMetaFile(hDC, (HMETAFILE)metafile);
  if (!dc->cdc)
    ReleaseDC(wnd->handle, hDC);

  return TRUE;
}

/*
 * Metafile device context
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxMetaFileDC, wxDC)

wxMetaFileDC::wxMetaFileDC(char *file)
{
  __type = wxTYPE_DC_METAFILE;
  old_bitmap = NULL;
  old_pen = NULL;
  old_brush = NULL;
  old_font = NULL;
  old_palette = NULL;

  metafile = NULL;
  wx_interactive = FALSE;
  canvas = NULL;
  font = NULL;

  device = wxDEVICE_WINDOWS;

  cdc = NULL;
  dont_delete = FALSE;
  clipping = FALSE;

  logical_origin_x = 0;
  logical_origin_y = 0;

  device_origin_x = 0;
  device_origin_y = 0;

  logical_scale_x = 1.0;
  logical_scale_y = 1.0;

  user_scale_x = 1.0;
  user_scale_y = 1.0;

  mapping_mode = MM_TEXT;

  min_x = 10000.0;
  min_y = 10000.0;
  max_x = -10000.0;
  max_y = -10000.0;
  title = NULL;

  filename = NULL;

  if (file && FileExists(file)) wxRemoveFile(file);
  cdc = CreateMetaFile(file);

  ok = TRUE;
  SetMapMode(MM_TEXT);

  current_pen = wxBLACK_PEN;
  current_brush = wxBLACK_BRUSH;
  current_background_color = new wxColour(wxWHITE);
  current_text_foreground = new wxColour(wxBLACK);
  current_text_background = new wxColour(wxWHITE);
  Colour = wxColourDisplay();
}

wxMetaFileDC::~wxMetaFileDC(void)
{
  cdc = 0;
}

/* MATTHEW: [2] 16-bit flag */
void wxMetaFileDC::GetTextExtent(const char *string, float *x, float *y,
                                 float *descent, float *externalLeading, 
				 wxFont *theFont, Bool use16bit)
{
  wxFont *fontToUse = theFont;
  if (!fontToUse)
    fontToUse = font;

  HDC dc = GetDC(NULL);

  SIZE sizeRect;
  TEXTMETRIC tm;
  GetTextExtentPoint(dc, string, strlen(string), &sizeRect);
  GetTextMetrics(dc, &tm);

  ReleaseDC(NULL, dc);

  *x = (float)XDEV2LOGREL(sizeRect.cx);
  *y = (float)YDEV2LOGREL(sizeRect.cy);
  if (descent) *descent = (float)tm.tmDescent;
  if (externalLeading) *externalLeading = (float)tm.tmExternalLeading;
}

wxMetaFile *wxMetaFileDC::Close(void)
{
  SelectOldObjects(cdc);
  HANDLE mf = CloseMetaFile(cdc);
  cdc = 0;
  if (mf)
  {
    wxMetaFile *wx_mf = new wxMetaFile;
    wx_mf->metafile = mf;
    return wx_mf;
  }
  return NULL;
}

void wxMetaFileDC::SetMapMode(int mode)
{
  mapping_mode = mode;

//  int pixel_width = 0;
//  int pixel_height = 0;
//  int mm_width = 0;
//  int mm_height = 0;

  float mm2pixelsX = 10.0;
  float mm2pixelsY = 10.0;

  switch (mode)
  {
    case MM_TWIPS:
    {
      logical_scale_x = (float)(twips2mm * mm2pixelsX);
      logical_scale_y = (float)(twips2mm * mm2pixelsY);
      break;
    }
    case MM_POINTS:
    {
      logical_scale_x = (float)(pt2mm * mm2pixelsX);
      logical_scale_y = (float)(pt2mm * mm2pixelsY);
      break;
    }
    case MM_METRIC:
    {
      logical_scale_x = mm2pixelsX;
      logical_scale_y = mm2pixelsY;
      break;
    }
    case MM_LOMETRIC:
    {
      logical_scale_x = (float)(mm2pixelsX/10.0);
      logical_scale_y = (float)(mm2pixelsY/10.0);
      break;
    }
    default:
    case MM_TEXT:
    {
      logical_scale_x = 1.0;
      logical_scale_y = 1.0;
      break;
    }
  }
  window_ext_x = 100;
  window_ext_y = 100;
}

struct mfPLACEABLEHEADER {
	DWORD	key;
	HANDLE	hmf;
	RECT	bbox;
	WORD	inch;
	DWORD	reserved;
	WORD	checksum;
};

/*
 * Pass filename of existing non-placeable metafile, and bounding box.
 * Adds a placeable metafile header, sets the mapping mode to anisotropic,
 * and sets the window origin and extent to mimic the MM_TEXT mapping mode.
 *
 */
 
Bool wxMakeMetaFilePlaceable(char *filename, int x1, int y1, int x2, int y2, float scale)
{
  // I'm not sure if this is the correct way of suggesting a scale
  // to the client application, but it's the only way I can find.
  int unitsPerInch = (int)(576/scale);
  
  mfPLACEABLEHEADER header;
  header.key = 0x9AC6CDD7L;
  header.hmf = 0;
  header.bbox.left = (int)(x1);
  header.bbox.top = (int)(y1);
  header.bbox.right = (int)(x2);
  header.bbox.bottom = (int)(y2);
  header.inch = unitsPerInch;
  header.reserved = 0;

  // Calculate checksum  
  WORD *p;
  mfPLACEABLEHEADER *pMFHead = &header;
  for (p =(WORD *)pMFHead,pMFHead -> checksum = 0;
	p < (WORD *)&pMFHead ->checksum; ++p)
       pMFHead ->checksum ^= *p;

  FILE *fd = fopen(filename, "rb");
  if (!fd) return FALSE;
  
  char tempFileBuf[256];
  wxGetTempFileName("mf", tempFileBuf);
  FILE *fHandle = fopen(tempFileBuf, "wb");
  if (!fHandle)
    return FALSE;
  fwrite((void *)&header, sizeof(char), sizeof(mfPLACEABLEHEADER), fHandle);

  // Calculate origin and extent
  int originX = x1;
  int originY = y1;
  int extentX = x2 - x1;
  int extentY = (y2 - y1);

  // Read metafile header and write
  METAHEADER metaHeader;
  fread((void *)&metaHeader, sizeof(char), sizeof(metaHeader), fd);
  metaHeader.mtSize += 15;
//  metaHeader.mtNoObjects += 3; // No, GDI objects, not records!
  fwrite((void *)&metaHeader, sizeof(char), sizeof(metaHeader), fHandle);

  // Write SetMapMode, SetWindowOrigin and SetWindowExt records
  char modeBuffer[8];
  char originBuffer[10];
  char extentBuffer[10];
  METARECORD *modeRecord = (METARECORD *)&modeBuffer;
  METARECORD *originRecord = (METARECORD *)&originBuffer;
  METARECORD *extentRecord = (METARECORD *)&extentBuffer;

  modeRecord->rdSize = 4;
  modeRecord->rdFunction = META_SETMAPMODE;
  modeRecord->rdParm[0] = MM_ANISOTROPIC;

  originRecord->rdSize = 5;
  originRecord->rdFunction = META_SETWINDOWORG;
  originRecord->rdParm[0] = originY;
  originRecord->rdParm[1] = originX;

  extentRecord->rdSize = 5;
  extentRecord->rdFunction = META_SETWINDOWEXT;
  extentRecord->rdParm[0] = extentY;
  extentRecord->rdParm[1] = extentX;

  fwrite((void *)modeBuffer, sizeof(char), 8, fHandle);
  fwrite((void *)originBuffer, sizeof(char), 10, fHandle);
  fwrite((void *)extentBuffer, sizeof(char), 10, fHandle);
  
  int ch = -2;
  while (ch != EOF)
  {
    ch = getc(fd);
    if (ch != EOF)
    {
      putc(ch, fHandle);
    }
  }
  fclose(fHandle);
  fclose(fd);
  wxRemoveFile(filename);
  wxCopyFile(tempFileBuf, filename);
  wxRemoveFile(tempFileBuf);
  return TRUE;
}

#endif // USE_METAFILE
