/*
 * File:	wx_win.h
 * Purpose:	wxWindow class declaration (X version).
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_win.h	1.2 5/9/94" */

#ifndef wx_winh
#define wx_winh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_list.h"
#include "wb_win.h"

#ifdef wx_motif
#include <Xm/Xm.h>
#endif

#ifdef wx_xview
#include "xview/xview.h"
#endif

/*
 * Base class for frame, panel, canvas, panel items, dialog box.
 *
 */

#ifdef IN_CPROTO
typedef       void    *wxWindow ;
#else
class wxCursor;
class wxBitmap;
class wxWindow: public wxbWindow
{
  DECLARE_DYNAMIC_CLASS(wxWindow)

 public:
  // When doing global cursor changes, the current cursor
  // may need to be saved for each window @@@@
  Cursor currentWindowCursor;
#ifdef wx_xview
  Xv_opaque dropSite;
#endif
#ifdef wx_motif
  // Need to store state of buttons (whether they're still down)
  int sr_width;
  int sr_height;
  int filler; // use in the future
#endif

  // Constructors/Destructors
  wxWindow(void);
  ~wxWindow(void);

  void GetSize(int *width, int *height);
  void GetPosition(int *x, int *y);
  void GetClientSize(int *width, int *height); // Size client can use
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  // Avoid compiler warning
  void SetSize(int w, int h) { wxbWindow::SetSize(w, h); }
  void SetClientSize(int width, int size);
  void ClientToScreen(int *x, int *y);
  void ScreenToClient(int *x, int *y);
  void Refresh(void);
  void SetFocus(void);
  void CaptureMouse(void);
  void ReleaseMouse(void);
  void Enable(Bool enable);
  void InternalEnable(Bool enable);
  void DragAcceptFiles(Bool accept);
  void Fit(void) {};
  void Centre(int /* direction */) {};
  void SetTitle(char * /* title */) {};
  void SetFont(wxFont *f);
  Bool PopupMenu(wxMenu *menu, float x, float y);

  // Sometimes in Motif there are problems popping up a menu
  // (for unknown reasons); use this instead when this happens.
#ifdef wx_motif
  Bool FakePopupMenu(wxMenu *menu, float x, float y);
#endif

  Bool Show(Bool show);
  wxCursor *SetCursor(wxCursor *cursor);
  void SetColourMap(wxColourMap *cmap);

  float GetCharHeight(void);
  float GetCharWidth(void);
  void GetTextExtent(const char *string, float *x, float *y,
     float *descent = NULL, float *externalLeading = NULL, wxFont *theFont = NULL, Bool use16 = FALSE);

#ifdef wx_motif 
  virtual Bool PreResize(void);
  virtual void PostDestroyChildren(void);
  int wxType;
#endif
  // Get the underlying X window
  virtual Window GetXWindow(void);
  virtual Window GetXCursorWindow(void);
  virtual Display *GetXDisplay(void);

  inline void OnScroll(wxScrollEvent& /*  event */) {}
  inline void SetScrollPosX(int /* pos */) {}
  inline void SetScrollPosY(int /* pos */) {}
  inline int GetScrollPos(int /* orient */) { return 0; };
  inline int GetScrollRange(int /* orient */) { return 0; };

  void AddPreHandlers(Widget w, Widget hash_w = 0);
  static void WindowEventHandler(Widget w, Widget hash_w, XEvent *xev, Boolean *continue_to_dispatch_return);

  virtual Bool PreOnChar(wxWindow *win, wxKeyEvent *event);
  Bool CallPreOnChar(wxWindow *win, wxKeyEvent *event);
  virtual Bool PreOnEvent(wxWindow *win, wxMouseEvent *event);
  Bool CallPreOnEvent(wxWindow *win, wxMouseEvent *event);
};

#endif // IN_CPROTO
#endif
