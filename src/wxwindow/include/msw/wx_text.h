/*
 * File:	wx_text.h
 * Purpose:	wxTextWindow - simple text subwindow class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_texth
#define wx_texth
#include "common.h"
#include "wb_text.h"

/*
 * I would make this also derived from ostream:
 *
 * class wxTextWindow: public wxWindow, public virtual ostream
 *
 * but weirdly, the operator << isn't declared virtual in (isn't even
 * a member of) ostream, so we can't use a wxTextWindow in place of an ostream.
 *
 */

#ifdef IN_CPROTO
typedef       void    *wxTextWindow ;
#else

class wxTextWindow: public wxbTextWindow
{
  DECLARE_DYNAMIC_CLASS(wxTextWindow)

 private:
  long insertionPoint;
 public:
  // Pointer to global memory
  HGLOBAL globalHandle;

  FARPROC oldTextProc; // For subclassing

  // Wojciech's mods
  UINT last_msg;
  WPARAM last_wparam;
  LPARAM last_lparam;

  wxTextWindow(void);
  wxTextWindow(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
               long style=0, char *name = "textWindow");
  ~wxTextWindow(void);

  Bool Create(wxWindow *parent, int x=-1, int y=-1, int width=-1, int height=-1,
               long style=0, char *name = "textWindow");
  Bool LoadFile(char *file);
  Bool SaveFile(char *file);
  void WriteText(char *text);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
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
  void SetFont(wxFont *font);

  // MSW implementation only
  void Synch();
  void Char(WPARAM wParam,BOOL isASCII = FALSE);
  void OnChar(wxKeyEvent& event);
};

#endif // IN_CPROTO
#endif // wx_texth
