/*
 * File:	wx_tbar.h
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_tbarh
#define wx_tbarh

#ifdef __GNUG__
#pragma interface
#endif

#include <common.h>

#if USE_TOOLBAR

#include "wx_gdi.h"
#include "wx_list.h"
#include "wx_canvs.h"
#include "wx_dcmem.h"

class wxToolBarTool: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxToolBarTool)
 public:
  wxObject *clientData;
  int index;
  float x;
  float y;
  float width;
  float height;
  Bool toggleState;
  Bool isToggle;
  Bool deleteSecondBitmap;
  Bool enabled;
  wxBitmap *bitmap1;
  wxBitmap *bitmap2;
  
  wxToolBarTool(int theIndex = 0, wxBitmap *bitmap1 = NULL, wxBitmap *bitmap2 = NULL,
                Bool toggle = FALSE, wxCanvas *canvas = NULL, float xPos = -1, float yPos = -1);
  ~wxToolBarTool(void);
  inline void SetSize(float w, float h) { width = w; height = h; }
  inline float GetWidth(void) { return width; }
  inline float GetHeight(void) { return height; }
};

class wxToolBar: public wxCanvas
{
  DECLARE_DYNAMIC_CLASS(wxToolBar)
 protected:
  wxList tools;
  int tilingDirection;
  int rowsOrColumns;
  int currentRowsOrColumns;
  float lastX;
  float lastY;
  float maxWidth;
  float maxHeight;
  float xMargin;
  float yMargin;
  int currentTool;  // Tool where mouse currently currently is
 public:

  /*
   * Public interface
   */

  wxToolBar(wxWindow *parent = NULL, int x = 0, int y = 0, int w = -1, int h = -1,
            long style = 0, int orientation = wxVERTICAL, int RowsOrColumns = 2, char *name = "toolbar");
  ~wxToolBar(void);

  // Handle wxWindows events
  void OnPaint(void);
  void OnSize(int w, int h);
  void OnEvent(wxMouseEvent& event);
  void OnKillFocus(void);

  // Handle wxToolBar events

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
  virtual wxToolBarTool *AddTool(int toolIndex, wxBitmap *bitmap, wxBitmap *pushedBitmap = NULL,
               Bool toggle = FALSE, float xPos = -1, float yPos = -1, wxObject *clientData = NULL);
  virtual void ClearTools(void);

  virtual void DrawTool(wxMemoryDC& memDC, wxToolBarTool *tool);
  virtual void EnableTool(int toolIndex, Bool enable);
  virtual void ToggleTool(int toolIndex, Bool toggle); // toggle is TRUE if toggled on
  virtual void SetToggle(int toolIndex, Bool toggle); // Set this to be togglable (or not)
  virtual wxObject *GetToolClientData(int index);
  inline wxList& GetTools(void) { return tools; }
  virtual void Layout(void);

  // After the toolbar has initialized, this is the size the tools take up
  virtual void GetMaxSize(float *w, float *h);
  virtual Bool GetToolState(int toolIndex);
  virtual Bool GetToolEnabled(int toolIndex);
  virtual void SetMargins(float x, float y);
  virtual wxToolBarTool *FindToolForPosition(float x, float y);
  virtual void SpringUpButton(int index);
};

#endif // USE_TOOLBAR
#endif // wx_tbarh
