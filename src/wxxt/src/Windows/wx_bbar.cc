/*
 * File:	wx_bbar.cc
 * Purpose:	Almost identical to wxToolBar programmer interface, but optimized
 *              for Windows to give better button-press and toggle feedback.
 *              By default, 16x15 pixel bitmaps are required under Windows.
 *              You can change this by calling wxButtonBar::SetDefaultSize,
 *              _before_ you start adding tools. See test.cc.
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

static const char sccsid[] = "%W% %G%";

#ifdef __GNUG__
#pragma implementation "wx_bbar.h"
#endif

#include "common.h"

#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

#if USE_BUTTONBAR && USE_TOOLBAR

#include "wx_bbar.h"
#include "wx_main.h"

IMPLEMENT_DYNAMIC_CLASS(wxButtonBar, wxToolBar)

#ifdef wx_msw

#if !USE_IMAGE_LOADING_IN_MSW
#error If USE_IMAGE_LOADING_IN_MSW is set to 0, then USE_BUTTONBAR must be set to 0 too.
#endif

#include "..\..\utils\dib\dib.h"
#include "wx_privt.h"
static void DrawButton(HDC hdc, int x, int y, int dx, int dy,
      wxToolBarTool *tool, int state);
static HBITMAP CreateMappedBitmap(HINSTANCE hInstance, HBITMAP hBitmap);
static void FreeGlobalObjects(void);
static Bool InitGlobalObjects(void);
#endif

wxButtonBar::wxButtonBar(wxFrame *parent, int x, int y, int w, int h, long style,
                     int direction, int RowsOrColumns, char *name):
  wxToolBar(parent, x, y, w, h, style, direction, RowsOrColumns, name)
{
  __type = wxTYPE_BUTTONBAR;
#ifdef wx_msw
  hbrDither = 0;
  rgbFace = 0;
  rgbShadow = 0;
  rgbHilight = 0;
  rgbFrame = 0;
  hdcMono = NULL;
  hbmMono = NULL;
  hbmDefault = NULL;
#endif
  defaultWidth = DEFAULTBITMAPX;
  defaultHeight = DEFAULTBITMAPY;
#ifdef wx_msw
  InitGlobalObjects();
#endif
}

wxButtonBar::~wxButtonBar(void)
{
#ifdef wx_msw
  FreeGlobalObjects();
#endif
}

void wxButtonBar::SetDefaultSize(float w, float h)
{
  defaultWidth = w; defaultHeight = h;
#ifdef wx_msw
  FreeGlobalObjects();
  InitGlobalObjects();
#endif
}

// The button size is bigger than the bitmap size
float wxButtonBar::GetDefaultButtonWidth(void)
{
#ifdef wx_msw
  return defaultWidth + 8;
#else
  return defaultWidth;
#endif
}

float wxButtonBar::GetDefaultButtonHeight(void)
{
#ifdef wx_msw
  return defaultHeight + 7;
#else
  return defaultWidth;
#endif
}

void wxButtonBar::OnPaint(void)
{
#ifdef wx_msw
  static int wxOnPaintCount = 0;

  // Prevent reentry of OnPaint which would cause
  // wxMemoryDC errors.
  if (wxOnPaintCount > 0)
    return;
  wxOnPaintCount ++;

  wxNode *node = tools.First();
  while (node)
  {
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    int state = wxTBSTATE_ENABLED;
    if (!tool->enabled)
      state = 0;
    if (tool->isToggle && tool->toggleState)
      state |= wxTBSTATE_CHECKED;
    DrawTool(tool, state);
    node = node->Next();
  }
  wxOnPaintCount --;
#else
  wxToolBar::OnPaint();
#endif
}

void wxButtonBar::OnSize(int w, int h)
{
  wxToolBar::OnSize(w, h);
}

// If a Button is disabled, then NO function (besides leaving
// or entering) should be carried out. Therefore the additions
// of 'enabled' testing (Stefan Hammes).
void wxButtonBar::OnEvent(wxMouseEvent& event)
{
#ifdef wx_msw
  static wxToolBarTool *eventCurrentTool = NULL;
  
  if (event.Leaving())
  {
    currentTool = -1;
    if (eventCurrentTool && eventCurrentTool->enabled)
    {
      ::ReleaseCapture();
      int state = wxTBSTATE_ENABLED;
      if (eventCurrentTool->toggleState)
        state |= wxTBSTATE_CHECKED;
      DrawTool(eventCurrentTool, state);
      eventCurrentTool = NULL;
    }
    OnMouseEnter(-1);
    return;
  }

  float x, y;
  event.Position(&x, &y);
  wxToolBarTool *tool = FindToolForPosition(x, y);

  if (!tool)
  {
    if (eventCurrentTool && eventCurrentTool->enabled)
    {
      ::ReleaseCapture();
      
      int state = wxTBSTATE_ENABLED;
      if (eventCurrentTool->toggleState)
        state |= wxTBSTATE_CHECKED;
      DrawTool(eventCurrentTool, state);
      eventCurrentTool = NULL;
    }
    if (currentTool > -1)
    {
      currentTool = -1;
      OnMouseEnter(-1);
    }
    return;
  }
  
  if (!event.Dragging() && !event.IsButton())
  {
    if (tool->index != currentTool)
    {
      OnMouseEnter(tool->index);
      currentTool = tool->index;
      return;
    }
  }
  if (event.Dragging() && tool->enabled)
  {
    if (eventCurrentTool)
    {
      // Might have dragged outside tool
      if (eventCurrentTool != tool)
      {
        int state = wxTBSTATE_ENABLED;
        if (tool->toggleState)
          state |= wxTBSTATE_CHECKED;
        DrawTool(tool, state);
        eventCurrentTool = NULL;
        return;
      }
    }
    else
    {
      if (tool && event.LeftIsDown() && tool->enabled)
      {
        eventCurrentTool = tool;
        ::SetCapture(GetHWND());
        int state = wxTBSTATE_ENABLED|wxTBSTATE_PRESSED;
        if (tool->toggleState)
          state |= wxTBSTATE_CHECKED;
        DrawTool(tool, state);
      }
    }
  }
  if (event.LeftDown() && tool->enabled)
  {
    eventCurrentTool = tool;
    ::SetCapture(GetHWND());
    int state = wxTBSTATE_ENABLED|wxTBSTATE_PRESSED;
    if (tool->toggleState)
      state |= wxTBSTATE_CHECKED;
    DrawTool(tool, state);
  }
  else if (event.LeftUp() && tool->enabled)
  {
    if (eventCurrentTool)
      ::ReleaseCapture();
    if (eventCurrentTool == tool)
    {
      if (tool->isToggle)
      {
        tool->toggleState = !tool->toggleState;
        if (!OnLeftClick(tool->index, tool->toggleState))
        {
          tool->toggleState = !tool->toggleState;
        }
        int state = wxTBSTATE_ENABLED;
        if (tool->toggleState)
          state |= wxTBSTATE_CHECKED;
        DrawTool(tool, state);
      }
      else
      {
        int state = wxTBSTATE_ENABLED;
        if (tool->toggleState)
          state |= wxTBSTATE_CHECKED;
        DrawTool(tool, state);
        OnLeftClick(tool->index, tool->toggleState);
      }
    }
    eventCurrentTool = NULL;
  }
  else if (event.RightDown() && tool->enabled)
  {
    OnRightClick(tool->index, x, y);
  }
#else
  wxToolBar::OnEvent(event);
#endif
}

// This function enables/disables a toolbar tool and redraws it.
// If that would not be done, the enabling/disabling wouldn't be
// visible on the screen.
#ifdef wx_msw
void wxButtonBar::EnableTool(int toolIndex, Bool enable)
{
  wxNode *node = tools.Find((long)toolIndex);
  if (node)
  {
    // at first do the enabling/disabling in the base class
    wxToolBar::EnableTool(toolIndex,enable);
    // then calculate the state of the tool and draw it
    wxToolBarTool *tool = (wxToolBarTool *)node->Data();
    int state = 0;
    if(tool->toggleState) state |= wxTBSTATE_CHECKED;
    if(tool->enabled) state |= wxTBSTATE_ENABLED;
    // how can i access the PRESSED state???
    DrawTool(tool,state);
  }
}
#endif

#ifdef wx_msw
void wxButtonBar::DrawTool(wxToolBarTool *tool, int state)
{
  wxWnd *wnd = (wxWnd *)this->handle;
  HDC dc = wnd->GetHDC();
  DrawButton(dc, (int)tool->x, (int)tool->y, (int)tool->GetWidth(), (int)tool->GetHeight(), tool, state);
  wnd->ReleaseHDC();
}
#endif

void wxButtonBar::DrawTool(wxMemoryDC&
#ifndef wx_msw
 memDC
#endif
, wxToolBarTool *tool)
{
#ifdef wx_msw
  int state = wxTBSTATE_ENABLED;
  if (!tool->enabled)
    state = 0;
  if (tool->toggleState)
    state |= wxTBSTATE_CHECKED;
  DrawTool(tool, state);
#else
  wxToolBar::DrawTool(memDC, tool);
#endif
}

// Only allow toggle if returns TRUE
Bool wxButtonBar::OnLeftClick(int toolIndex, Bool toggleDown)
{
  return wxToolBar::OnLeftClick(toolIndex, toggleDown);
}

// Called when the mouse cursor enters a tool bitmap (no button pressed).
// Argument is -1 if mouse is exiting the toolbar.
void wxButtonBar::OnMouseEnter(int WXUNUSED(toolIndex))
{
}
  
// If pushedBitmap is NULL, a reversed version of bitmap is
// created and used as the pushed/toggled image.
// If toggle is TRUE, the button toggles between the two states.
wxToolBarTool *wxButtonBar::AddTool(int index, wxBitmap *bitmap, wxBitmap *pushedBitmap,
             Bool toggle, float xPos, float yPos, wxObject *clientData)
{
#ifdef wx_msw
  wxBitmap *bitmap2 = new wxBitmap;
  bitmap2->ms_bitmap = CreateMappedBitmap(wxhInstance, bitmap->ms_bitmap);

  wxToolBarTool *tool = new wxToolBarTool(index, bitmap, bitmap2, toggle, this, xPos, yPos);
  tool->clientData = clientData;

  if (xPos > -1)
    tool->x = xPos;
  else
    tool->x = xMargin;

  if (yPos > -1)
    tool->y = yPos;
  else
    tool->y = yMargin;

  tool->deleteSecondBitmap = TRUE;
  tool->SetSize(GetDefaultButtonWidth(), GetDefaultButtonHeight());
  
  // Calculate reasonable max size in case Layout() not called
  if ((tool->x + bitmap->GetWidth() + xMargin) > maxWidth)
    maxWidth = (tool->x + tool->GetWidth() + xMargin);

  if ((tool->y + bitmap->GetHeight() + yMargin) > maxHeight)
    maxHeight = (tool->y + tool->GetHeight() + yMargin);

  tools.Append((long)index, tool);
  return tool;
#else
  return wxToolBar::AddTool(index, bitmap, pushedBitmap, toggle, xPos, yPos, clientData);
#endif
}

#ifdef wx_msw
Bool wxButtonBar::InitGlobalObjects(void)
{
  GetSysColors();      
  if (!CreateDitherBrush())
    return FALSE;

    hdcMono = CreateCompatibleDC(NULL);
    if (!hdcMono)
        return FALSE;

    hbmMono = CreateBitmap((int)GetDefaultButtonWidth(), (int)GetDefaultButtonHeight(), 1, 1, NULL);
    if (!hbmMono)
        return FALSE;

    hbmDefault = SelectObject(hdcMono, hbmMono);

    return TRUE;
}

void wxButtonBar::FreeGlobalObjects(void)
{
    FreeDitherBrush();

    if (hdcMono) {
	if (hbmDefault)
	{
	    SelectObject(hdcMono, hbmDefault);
	    hbmDefault = 0;
	}
	DeleteDC(hdcMono);		// toast the DCs
    }
    hdcMono = NULL;

    if (hbmMono)
	DeleteObject(hbmMono);
    hbmMono = NULL;
}


void wxButtonBar::PatB(HDC hdc,int x,int y,int dx,int dy, DWORD rgb)
{
    RECT    rc;

    rc.left   = x;
    rc.top    = y;
    rc.right  = x + dx;
    rc.bottom = y + dy;

    SetBkColor(hdc,rgb);
    ExtTextOut(hdc,0,0,ETO_OPAQUE,&rc,NULL,0,NULL);
}


// create a mono bitmap mask:
//   1's where color == COLOR_BTNFACE || COLOR_HILIGHT
//   0's everywhere else

void wxButtonBar::CreateMask(HBITMAP hBitmap, int xoffset, int yoffset, int dx, int dy)
{
    HDC globalDC = ::GetDC(NULL);
    HDC hdcGlyphs = CreateCompatibleDC(globalDC);
    ReleaseDC(NULL, globalDC);

    HBITMAP bitmapOld = SelectObject(hdcGlyphs, hBitmap);
    
    // initalize whole area with 1's
    PatBlt(hdcMono, 0, 0, dx, dy, WHITENESS);

    // create mask based on color bitmap
    // convert this to 1's
    SetBkColor(hdcGlyphs, rgbFace);
    BitBlt(hdcMono, xoffset, yoffset, (int)GetDefaultWidth(), (int)GetDefaultHeight(),
        hdcGlyphs, 0, 0, SRCCOPY);
    // convert this to 1's
    SetBkColor(hdcGlyphs, rgbHilight);
    // OR in the new 1's
    BitBlt(hdcMono, xoffset, yoffset, (int)GetDefaultWidth(), (int)GetDefaultHeight(),
        hdcGlyphs, 0, 0, SRCPAINT);

    SelectObject(hdcGlyphs, bitmapOld);
    DeleteDC(hdcGlyphs);
}

void wxButtonBar::DrawBlankButton(HDC hdc, int x, int y, int dx, int dy, WORD state)
{
    // face color
    PatB(hdc, x, y, dx, dy, rgbFace);

    if (state & wxTBSTATE_PRESSED) {
	PatB(hdc, x + 1, y, dx - 2, 1, rgbFrame);
	PatB(hdc, x + 1, y + dy - 1, dx - 2, 1, rgbFrame);
	PatB(hdc, x, y + 1, 1, dy - 2, rgbFrame);
	PatB(hdc, x + dx - 1, y +1, 1, dy - 2, rgbFrame);
	PatB(hdc, x + 1, y + 1, 1, dy-2, rgbShadow);
	PatB(hdc, x + 1, y + 1, dx-2, 1, rgbShadow);
    }
    else {
	PatB(hdc, x + 1, y, dx - 2, 1, rgbFrame);
	PatB(hdc, x + 1, y + dy - 1, dx - 2, 1, rgbFrame);
	PatB(hdc, x, y + 1, 1, dy - 2, rgbFrame);
	PatB(hdc, x + dx - 1, y + 1, 1, dy - 2, rgbFrame);
	dx -= 2;
	dy -= 2;
	PatB(hdc, x + 1, y + 1, 1, dy - 1, rgbHilight);
	PatB(hdc, x + 1, y + 1, dx - 1, 1, rgbHilight);
	PatB(hdc, x + dx, y + 1, 1, dy, rgbShadow);
	PatB(hdc, x + 1, y + dy, dx, 1,   rgbShadow);
	PatB(hdc, x + dx - 1, y + 2, 1, dy - 2, rgbShadow);
	PatB(hdc, x + 2, y + dy - 1, dx - 2, 1,   rgbShadow);
    }
}

void wxButtonBar::DrawButton(HDC hdc, int x, int y, int dx, int dy, wxToolBarTool *tool, int state)
{
    int yOffset;
    HBRUSH hbrOld, hbr;
    BOOL bMaskCreated = FALSE;
    int xButton = 0;		// assume button is down
    int dxFace, dyFace;
    int xCenterOffset;

    dxFace = dx;
    dyFace = dy;

    HBITMAP hBitmap = tool->bitmap1->ms_bitmap;
    HDC globalDC = ::GetDC(NULL);
    HDC hdcGlyphs = CreateCompatibleDC(globalDC);
    ReleaseDC(NULL, globalDC);

    // get the proper button look - up or down.
    if (!(state & (wxTBSTATE_PRESSED | wxTBSTATE_CHECKED))) {
	xButton = dx;	// use 'up' version of button
	dxFace -= 2;	
	dyFace -= 2;	// extents to ignore button highlight
    }

    DrawBlankButton(hdc, x, y, dx, dy, state);


    // move coordinates inside border and away from upper left highlight.
    // the extents change accordingly.
    x += 2;
    y += 2;
    dxFace -= 3;		
    dyFace -= 3;

    HBITMAP bitmapOld = SelectObject(hdcGlyphs, tool->bitmap2->ms_bitmap);
    
//    if (!SelectBM(hdcGlyphs, pTBHdr, ptButton->iBitmap))
//	return;

    // calculate offset of face from (x,y).  y is always from the top,
    // so the offset is easy.  x needs to be centered in face.
    yOffset = 1;
    xCenterOffset = (dxFace - (int)GetDefaultWidth())/2;
    if (state & (wxTBSTATE_PRESSED | wxTBSTATE_CHECKED))
    {
	// pressed state moves down and to the right
	// (x moves automatically as face size grows)
        yOffset++;
    }

    // now put on the face
    if (state & wxTBSTATE_ENABLED) {
        // regular version
        BitBlt(hdc, x+xCenterOffset, y + yOffset, (int)GetDefaultWidth(), (int)GetDefaultHeight(),
            hdcGlyphs, 0, 0, SRCCOPY);
    } else {
        // disabled version (or indeterminate)
	bMaskCreated = TRUE;
        CreateMask(hBitmap, xCenterOffset, yOffset, dxFace, dyFace);

	SetTextColor(hdc, 0L);	 // 0's in mono -> 0 (for ROP)
	SetBkColor(hdc, 0x00FFFFFF); // 1's in mono -> 1

	// draw glyph's white understrike
	if (!(state & wxTBSTATE_INDETERMINATE)) {
	    hbr = CreateSolidBrush(rgbHilight);
	    if (hbr) {
	        hbrOld = SelectObject(hdc, hbr);
	        if (hbrOld) {
	            // draw hilight color where we have 0's in the mask
                    BitBlt(hdc, x + 1, y + 1, dxFace, dyFace, hdcMono, 0, 0, 0x00B8074A);
	            SelectObject(hdc, hbrOld);
	        }
	        DeleteObject(hbr);
	    }
	}

	// gray out glyph
	hbr = CreateSolidBrush(rgbShadow);
	if (hbr) {
	    hbrOld = SelectObject(hdc, hbr);
	    if (hbrOld) {
	        // draw the shadow color where we have 0's in the mask
                BitBlt(hdc, x, y, dxFace, dyFace, hdcMono, 0, 0, 0x00B8074A);
	        SelectObject(hdc, hbrOld);
	    }
	    DeleteObject(hbr);
	}

	if (state & wxTBSTATE_CHECKED) {
	    BitBlt(hdcMono, 1, 1, dxFace - 1, dyFace - 1, hdcMono, 0, 0, SRCAND);
	}
    }

    if (state & (wxTBSTATE_CHECKED | wxTBSTATE_INDETERMINATE)) {

        hbrOld = SelectObject(hdc, hbrDither);
	if (hbrOld) {

	    if (!bMaskCreated)
                CreateMask(hBitmap, xCenterOffset, yOffset, dxFace, dyFace);

	    SetTextColor(hdc, 0L);		// 0 -> 0
	    SetBkColor(hdc, 0x00FFFFFF);	// 1 -> 1

	    // only draw the dither brush where the mask is 1's
            BitBlt(hdc, x, y, dxFace, dyFace, hdcMono, 0, 0, 0x00E20746);
	    
	    SelectObject(hdc, hbrOld);
	}
    }
    SelectObject(hdcGlyphs, bitmapOld);
    DeleteDC(hdcGlyphs);
}

void wxButtonBar::GetSysColors(void)
{
	static COLORREF rgbSaveFace    = 0xffffffffL,
	                rgbSaveShadow  = 0xffffffffL,
	                rgbSaveHilight = 0xffffffffL,
	                rgbSaveFrame   = 0xffffffffL;

	rgbFace    = GetSysColor(COLOR_BTNFACE);
	rgbShadow  = GetSysColor(COLOR_BTNSHADOW);
	rgbHilight = GetSysColor(COLOR_BTNHIGHLIGHT);
	rgbFrame   = GetSysColor(COLOR_WINDOWFRAME);

	if (rgbSaveFace!=rgbFace || rgbSaveShadow!=rgbShadow
		|| rgbSaveHilight!=rgbHilight || rgbSaveFrame!=rgbFrame)
	{
		rgbSaveFace    = rgbFace;
		rgbSaveShadow  = rgbShadow;
		rgbSaveHilight = rgbHilight;
                rgbSaveFrame   = rgbFrame;

		// Update the brush for pushed-in buttons
		CreateDitherBrush();
	}
}

HBITMAP wxButtonBar::CreateDitherBitmap()
{
    PBITMAPINFO pbmi;
    HBITMAP hbm;
    HDC hdc;
    int i;
    long patGray[8];
    DWORD rgb;

//    pbmi = (PBITMAPINFO)LocalAlloc(LPTR, sizeof(BITMAPINFOHEADER) + (sizeof(RGBQUAD) * 16));
    HANDLE hDIB = GlobalAlloc(GHND, (DWORD)(sizeof(BITMAPINFOHEADER) + 
    					16 * sizeof(RGBQUAD)));
    if (!hDIB)
	return(NULL);

    pbmi = (PBITMAPINFO)GlobalLock(hDIB);

    if (!pbmi)
        return NULL;

    pbmi->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    pbmi->bmiHeader.biWidth = 8;
    pbmi->bmiHeader.biHeight = 8;
    pbmi->bmiHeader.biPlanes = 1;
    pbmi->bmiHeader.biBitCount = 1;
    pbmi->bmiHeader.biCompression = BI_RGB;

    rgb = GetSysColor(COLOR_BTNFACE);
    pbmi->bmiColors[0].rgbBlue  = GetBValue(rgb);
    pbmi->bmiColors[0].rgbGreen = GetGValue(rgb);
    pbmi->bmiColors[0].rgbRed   = GetRValue(rgb);
    pbmi->bmiColors[0].rgbReserved = 0;

    rgb = GetSysColor(COLOR_BTNHIGHLIGHT);
    pbmi->bmiColors[1].rgbBlue  = GetBValue(rgb);
    pbmi->bmiColors[1].rgbGreen = GetGValue(rgb);
    pbmi->bmiColors[1].rgbRed   = GetRValue(rgb);
    pbmi->bmiColors[1].rgbReserved = 0;


    /* initialize the brushes */

    for (i = 0; i < 8; i++)
       if (i & 1)
           patGray[i] = 0xAAAA5555L;   //  0x11114444L; // lighter gray
       else
           patGray[i] = 0x5555AAAAL;   //  0x11114444L; // lighter gray

    hdc = ::GetDC(NULL);

    hbm = CreateDIBitmap(hdc, &pbmi->bmiHeader, CBM_INIT, patGray, pbmi, DIB_RGB_COLORS);

    ReleaseDC(NULL, hdc);

//    LocalFree((HANDLE)pbmi);
    GlobalUnlock(hDIB);
    GlobalFree(hDIB);

    return hbm;
}

Bool wxButtonBar::CreateDitherBrush(void)
{
	HBITMAP hbmGray;
	HBRUSH hbrSave;
	if (hbrDither)
	  return TRUE;
	hbmGray = CreateDitherBitmap();
	if (hbmGray)
	{
		hbrSave = hbrDither;
		hbrDither = CreatePatternBrush(hbmGray);
		DeleteObject(hbmGray);
		if (hbrDither)
		{
			if (hbrSave)
			{
				DeleteObject(hbrSave);
			}
			return TRUE;
		}
		else
		{
			hbrDither = hbrSave;
		}
	}

	return FALSE;
}

Bool wxButtonBar::FreeDitherBrush(void)
{
    if (hbrDither)
      DeleteObject(hbrDither);
    hbrDither = NULL;

    return TRUE;
}

typedef struct tagCOLORMAP
{
    COLORREF bgrfrom;
    COLORREF bgrto;
    COLORREF sysColor;
} COLORMAP;

// these are the default colors used to map the dib colors
// to the current system colors

#define BGR_BUTTONTEXT      (RGB(000,000,000))  // black
#define BGR_BUTTONSHADOW    (RGB(128,128,128))  // dark grey
#define BGR_BUTTONFACE      (RGB(192,192,192))  // bright grey
#define BGR_BUTTONHILIGHT   (RGB(255,255,255))  // white
#define BGR_BACKGROUNDSEL   (RGB(255,000,000))  // blue
#define BGR_BACKGROUND      (RGB(255,000,255))  // magenta
#define FlipColor(rgb)      (RGB(GetBValue(rgb), GetGValue(rgb), GetRValue(rgb)))

HBITMAP wxButtonBar::CreateMappedBitmap(HINSTANCE WXUNUSED(hInstance), LPBITMAPINFOHEADER lpBitmapInfo)
{
  HDC			hdc, hdcMem = NULL;
  DWORD FAR		*p;
  LPSTR 		lpBits;
  HBITMAP		hbm = NULL, hbmOld;
  int numcolors, i;
  int wid, hgt;
  static COLORMAP ColorMap[] = {
    {BGR_BUTTONTEXT,    BGR_BUTTONTEXT,    COLOR_BTNTEXT},     // black
    {BGR_BUTTONSHADOW,  BGR_BUTTONSHADOW,  COLOR_BTNSHADOW},   // dark grey
    {BGR_BUTTONFACE,    BGR_BUTTONFACE,    COLOR_BTNFACE},     // bright grey
    {BGR_BUTTONHILIGHT, BGR_BUTTONHILIGHT, COLOR_BTNHIGHLIGHT},// white
    {BGR_BACKGROUNDSEL, BGR_BACKGROUNDSEL, COLOR_HIGHLIGHT},   // blue
    {BGR_BACKGROUND,    BGR_BACKGROUND,    COLOR_WINDOW}       // magenta
  };

  #define NUM_MAPS (sizeof(ColorMap)/sizeof(COLORMAP))

  if (!lpBitmapInfo)
  	return NULL;

  //
  // So what are the new colors anyway ?
  //
  for (i=0; i < NUM_MAPS; i++) {
     ColorMap[i].bgrto = FlipColor(GetSysColor((int)ColorMap[i].sysColor));
  }

  p = (DWORD FAR *)(((LPSTR)lpBitmapInfo) + lpBitmapInfo->biSize);

  /* Replace button-face and button-shadow colors with the current values
   */
  numcolors = 16;

  while (numcolors-- > 0) {
      for (i = 0; i < NUM_MAPS; i++) {
          if (*p == ColorMap[i].bgrfrom) {
          *p = ColorMap[i].bgrto;
	      break;
	  }
      }
      p++;
  }

  /* First skip over the header structure */
  lpBits = (LPSTR)(lpBitmapInfo + 1);

  /* Skip the color table entries, if any */
  lpBits += (1 << (lpBitmapInfo->biBitCount)) * sizeof(RGBQUAD);

  /* Create a color bitmap compatible with the display device */
  i = wid = (int)lpBitmapInfo->biWidth;
  hgt = (int)lpBitmapInfo->biHeight;
  hdc = ::GetDC(NULL);

  hdcMem = CreateCompatibleDC(hdc);
  if (hdcMem) {
    hbm = CreateDiscardableBitmap(hdc, i, hgt);
    if (hbm) {
        hbmOld = SelectObject(hdcMem, hbm);

        // set the main image
        StretchDIBits(hdcMem, 0, 0, wid, hgt, 0, 0, wid, hgt, lpBits,
                   (LPBITMAPINFO)lpBitmapInfo, DIB_RGB_COLORS, SRCCOPY);

        SelectObject(hdcMem, hbmOld);
    }

    DeleteObject(hdcMem);
  }

  ReleaseDC(NULL, hdc);

  return hbm;
}

HBITMAP wxButtonBar::CreateMappedBitmap(HINSTANCE hInstance, HBITMAP hBitmap)
{
  HANDLE hDIB = BitmapToDIB(hBitmap, 0);
  if (hDIB)
  {
    LPBITMAPINFOHEADER lpbmInfoHdr = (LPBITMAPINFOHEADER)GlobalLock(hDIB);
    HBITMAP newBitmap = CreateMappedBitmap(hInstance, lpbmInfoHdr);
    GlobalUnlock(hDIB);
    GlobalFree(hDIB);
    return newBitmap;
  }
  return 0;
}

/*
static HBITMAP CreateMappedBitmap(HINSTANCE hInstance, int idBitmap )
{
  HANDLE h = FindResource(hInstance, MAKEINTRESOURCE(idBitmap), RT_BITMAP);
  if (!h)
      return NULL;

  HANDLE hRes = LoadResource(hInstance, h);

  // Lock the bitmap and get a pointer to the color table.
  LPBITMAPINFOHEADER lpBitmapInfo = (LPBITMAPINFOHEADER)LockResource(hRes);
  if (!lpBitmapInfo)
  	return NULL;

  HBITMAP hbm = CreateMappedBitmap(hInstance, lpBitmapInfo);

  UnlockResource(hRes);
  FreeResource(hRes);

  return hbm;
}
*/

#endif
#endif
