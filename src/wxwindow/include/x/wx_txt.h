/*
 * File:	wx_txt.h
 * Purpose:	Declares single-line text panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_txt.h	1.2 5/9/94" */

#ifndef wx_txth
#define wx_txth

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_txt.h"

#ifdef IN_CPROTO
typedef       void    *wxText ;
#else

class wxPanel;

// Single-line text item
class wxText: public wxbText
{
  DECLARE_DYNAMIC_CLASS(wxText)

 public:
  // Text value in case it's a password-type of
  // text widget, where the reall password text must
  // be stored separately.
  char *internalTextValue;
  wxText(void);
  wxText(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = -1,
         long style = 0, char *name = "text");
  ~wxText(void);

  Bool Create(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = -1,
         long style = 0, char *name = "text");
  char *GetValue(void);
  void SetValue(char *value);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }
  void SetFocus(void);

  // Clipboard operations
  void Copy(void);
  void Cut(void);
  void Paste(void);

  void SetEditable(Bool editable);

  void ChangeColour(void) ;
};

#endif // IN_CPROTO
#endif // wx_txth
