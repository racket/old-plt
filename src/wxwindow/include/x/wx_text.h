/*
 * File:	wx_text.h
 * Purpose:	wxTextWindow - simple text subwindow class (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_text.h	1.2 5/9/94" */

#ifndef wx_texth
#define wx_texth

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_frame.h"
#include "wb_text.h"

#ifdef IN_CPROTO
typedef       void    *wxTextWindow ;
#else

class wxTextWindow: public wxbTextWindow
{
  DECLARE_DYNAMIC_CLASS(wxTextWindow)
 private:

#ifdef wx_motif
  long textPosition;
  Bool textModified;
#endif
 public:
#ifdef wx_motif
  XtPointer tempCallbackStruct;
#endif

  wxTextWindow(void);
  wxTextWindow(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
               long style=0, char *name = "textWindow");
  ~wxTextWindow(void);

  Bool Create(wxWindow *window, int x=-1, int y=-1, int width=-1, int height=-1,
               long style=0, char *name = "textWindow");
  Bool LoadFile(char *file);
  Bool SaveFile(char *file);
  void WriteText(char *text);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  // Avoid compiler warning
  void SetSize(int w, int h) { wxWindow::SetSize(w, h); }
  void SetClientSize(int width, int height);
  void Clear(void);
  void DiscardEdits(void);
  Bool Modified(void);
  char *GetContents(void);
  void SetInsertionPoint(long pos);
  void SetInsertionPointEnd(void);
  long GetInsertionPoint(void);
  long GetLastPosition(void);
  long XYToPosition(long x, long y);
  void PositionToXY(long pos, long *x, long *y);
  void ShowPosition(long pos);
  int GetLineLength(long lineNo);
  int GetLineText(long lineNo, char *buf);
  int GetNumberOfLines(void);
  void Replace(long from, long to, char *value);
  void Remove(long from, long to);
  void SetSelection(long from, long to);
  void Copy(void); // Copy selection to clipboard
  void Paste(void); // Paste clipboard into text window
  void Cut(void); // Copy selection to clipboard, then remove selection.
  void SetEditable(Bool editable);

  void OnChar(wxKeyEvent& event);
#ifdef wx_xview
  virtual void DragAcceptFiles(Bool accept = TRUE);
#endif
#ifdef wx_motif
  inline void SetModified(Bool mod) { textModified = mod; }
#endif
};

#endif // IN_CPROTO
#endif // wx_texth
