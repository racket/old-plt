/*
 * File:	wx_bbar.h
 * Purpose:	See wx_bbar.cc
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_bbarh
#define wx_bbarh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"

#if USE_BUTTONBAR && USE_TOOLBAR
#include "wx_tbar.h"

class wxButtonBar: public wxToolBar
{
  DECLARE_DYNAMIC_CLASS(wxButtonBar)
 public:
  float defaultWidth;
  float defaultHeight;

#ifdef wx_msw
  HBRUSH hbrDither;
  DWORD  rgbFace;
  DWORD  rgbShadow;
  DWORD  rgbHilight;
  DWORD  rgbFrame;

//
// hdcMono is the DC that holds a mono bitmap, hbmMono
// that is used to create highlights
// of button faces.
// hbmDefault hold the default bitmap if there is one.
//
  HDC     hdcMono;
  HBITMAP hbmMono;
  HBITMAP hbmDefault;
#endif

  /*
   * Public interface
   */

  wxButtonBar(wxWindow *window = NULL, int x = 0, int y = 0, int w = -1, int h = -1,
            long style = 0, int orientation = wxVERTICAL, int RowsOrColumns = 2, char *name = "buttonBar");
  ~wxButtonBar(void);

  // Handle wxWindows events
  void OnPaint(void);
  void OnSize(int w, int h);
  void OnEvent(wxMouseEvent& event);

  // Handle wxButtonBar events

  // Only allow toggle if returns TRUE
  virtual Bool OnLeftClick(int toolIndex, Bool toggleDown);
  virtual void OnRightClick(int WXUNUSED(toolIndex), float WXUNUSED(x), float WXUNUSED(y)) {};

  // Eliminate compiler warnings
  virtual void OnLeftClick(int x, int y, int keys) { wxCanvas::OnLeftClick(x, y, keys); }
  virtual void OnRightClick(int x, int y, int keys) { wxCanvas::OnRightClick(x, y, keys); }

  // Called when the mouse cursor enters a tool bitmap (no button pressed).
  // Argument is -1 if mouse is exiting the toolbar.
  virtual void OnMouseEnter(int toolIndex);
  
  // If pushedBitmap is NULL, a reversed version of bitmap is
  // created and used as the pushed/toggled image.
  // If toggle is TRUE, the button toggles between the two states.
  wxToolBarTool *AddTool(int toolIndex, wxBitmap *bitmap, wxBitmap *pushedBitmap = NULL,
               Bool toggle = FALSE, float xPos = -1, float yPos = -1, wxObject *clientData = NULL);

  void DrawTool(wxMemoryDC& memDc, wxToolBarTool *tool);

  // New members
  // Set default bitmap size
  void SetDefaultSize(float w, float h);
  inline float GetDefaultWidth(void) { return defaultWidth; }
  inline float GetDefaultHeight(void) { return defaultHeight; }
#ifdef wx_msw
  void EnableTool(int toolIndex, Bool enable); // additional drawing on enabling
#endif

  // The button size is bigger than the bitmap size
  float GetDefaultButtonWidth(void);
  float GetDefaultButtonHeight(void);
  protected:
#ifdef wx_msw
  void DrawTool(wxToolBarTool *tool, int state);

  void GetSysColors(void);
  Bool InitGlobalObjects(void);
  void FreeGlobalObjects(void);
  void PatB(HDC hdc,int x,int y,int dx,int dy, DWORD rgb);
  void CreateMask(HBITMAP hBitmap, int xoffset, int yoffset, int dx, int dy);
  void DrawBlankButton(HDC hdc, int x, int y, int dx, int dy, WORD state);
  void DrawButton(HDC hdc, int x, int y, int dx, int dy, wxToolBarTool *tool, int state);
  HBITMAP CreateDitherBitmap();
  Bool CreateDitherBrush(void);
  Bool FreeDitherBrush(void);
  HBITMAP CreateMappedBitmap(HINSTANCE hInstance, LPBITMAPINFOHEADER lpBitmapInfo);
  HBITMAP CreateMappedBitmap(HINSTANCE hInstance, HBITMAP hBitmap);
#endif
};

// #ifdef wx_msw
#define DEFAULTBITMAPX   16
#define DEFAULTBITMAPY   15
#define DEFAULTBUTTONX   24
#define DEFAULTBUTTONY   22
#define DEFAULTBARHEIGHT 27

//
// States (not all of them currently used)
//
#define wxTBSTATE_CHECKED         0x01    // radio button is checked
#define wxTBSTATE_PRESSED         0x02    // button is being depressed (any style)
#define wxTBSTATE_ENABLED         0x04    // button is enabled
#define wxTBSTATE_HIDDEN          0x08    // button is hidden
#define wxTBSTATE_INDETERMINATE   0x10    // button is indeterminate

// #endif // wx_msw

#endif // USE_TOOL/BUTTONBAR
#endif // wx_bbarh
