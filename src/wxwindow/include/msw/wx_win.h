/*
 * File:	wx_win.h
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

/*
 * Purpose:  wxWindow class declaration. Base class for all windows and
 *           panel items.
 */


#ifndef wx_winh
#define wx_winh

#include "wb_win.h"

#include <windows.h>

/*
 * Base class for frame, panel, canvas, panel items, dialog box.
 *
 */

#ifdef IN_CPROTO
typedef       void    *wxWindow ;
#else

class wxWindow: public wxbWindow
{
  DECLARE_ABSTRACT_CLASS(wxWindow)

 public:
  HANDLE ms_handle;                   // For menus and hwnds: using 'handle'
                                      // causes too many compiler messages
  int wxWinType;                      // For knowing how to delete the object
  int cxChar;
  int cyChar;
  int windows_id;
  Bool mouseInWindow ;
  Bool winEnabled;
  short internal_gray_disabled;

  RECT updateRect;             // Bounding box for screen damage area
#ifdef WIN32
  HRGN updateRgn;                  // NT allows access to the rectangle list
#endif

  // Caret data
  int caretWidth;
  int caretHeight;
  Bool caretEnabled;
  Bool caretShown;

  wxWindow *focusWindow;

  virtual BOOL MSWCommand(UINT param, WORD id);
  wxWindow *FindItem(int id);
  wxWindow *FindItemByHWND(HWND hWnd);
  virtual void PreDelete(HDC dc);              // Allows system cleanup
  HWND GetHWND(void);

  // Constructors/Destructors
  wxWindow(void);
  virtual ~wxWindow(void);

  virtual Bool Show(Bool show);
  virtual wxCursor *SetCursor(wxCursor *cursor);
  virtual void SetColourMap(wxColourMap *cmap);

  virtual float GetCharHeight(void);
  virtual float GetCharWidth(void);
  /* MATTHEW: [2] 16-bit flag */
  virtual void GetTextExtent(const char *string, float *x, float *y,
      float *descent = NULL, float *externalLeading = NULL, 
			     wxFont *theFont = NULL, Bool use16bit = FALSE);

  void GetSize(int *width, int *height);
  void GetPosition(int *x, int *y);
  void GetClientSize(int *width, int *height); // Size client can use
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxbWindow::SetSize(width, height); }
  void SetClientSize(int width, int size);
  void ClientToScreen(int *x, int *y);
  void ScreenToClient(int *x, int *y);
  void SetFocus(void);
  void CaptureMouse(void);
  void ReleaseMouse(void);
  void Enable(Bool enable);
  void DragAcceptFiles(Bool accept);
  inline void SetTitle(char *WXUNUSED(title)) {};
  inline void Fit(void) {};
  inline void Centre(int WXUNUSED(direction)) {};
  Bool PopupMenu(wxMenu *menu, float x, float y);

  void InternalEnable(Bool enable, Bool gray = FALSE);
  Bool IsGray(void);
  virtual void ChangeToGray(Bool gray);
  void InternalGrayChildren(Bool gray);

  void Refresh(void);

  void OnScroll(wxScrollEvent& event);
  void SetScrollPos(int orient, int pos);
  void SetScrollRange(int orient, int range);
  void SetScrollPage(int orient, int page);
  int GetScrollPos(int orient);
  int GetScrollRange(int orient);
  int GetScrollPage(int orient);

  wxWindow *GetTopLevel();
  void DoEnableWindow(int on);

  // The default implementation sets scroll ranges, if any
  void OnSize(int w, int h);

  // Internal function to update scrollbars
  void DoScroll(wxScrollEvent& event);

  // Calculate scroll increment
  int CalcScrollInc(wxScrollEvent& event);

  Bool CallPreOnEvent(wxWindow *, wxMouseEvent *);
  Bool CallPreOnChar(wxWindow *, wxKeyEvent *);  

  virtual Bool PreOnEvent(wxWindow *, wxMouseEvent *);
  virtual Bool PreOnChar(wxWindow *, wxKeyEvent *);
};

// Window specific (so far)
wxWindow *wxGetActiveWindow(void);

// Allows iteration through damaged rectangles in OnPaint
class wxUpdateIterator
{
  int rects;						// How many rects in Update region
  int current;					        // Current rectangle index
  RECT *rp;						// current rectangle
#ifdef	WIN32
  RGNDATA *rlist;					// Storage for regiondata
#endif

 public:
  wxUpdateIterator(wxWindow* wnd);
  ~wxUpdateIterator(void);

  operator int (void);
  wxUpdateIterator* operator ++(int);
  RECT*	GetMSWRect(void);
  void GetRect(wxRectangle *rect);
  int GetX();
  int GetY();
  int GetW();
  int GetH();
};

int wxCharCodeMSWToWX(int keySym);
int wxCharCodeWXToMSW(int id, Bool *IsVirtual);

#endif // IN_CPROTO
#endif
