/*
 * File:	wx_txt.h
 * Purpose:	Text panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_txth
#define wx_txth

#include "wb_txt.h"

#ifdef IN_CPROTO
typedef       void    *wxText ;
#else

// Single-line text item
class wxText: public wxbText
{
  DECLARE_DYNAMIC_CLASS(wxText)

 public:
  HWND static_label;
  // Pointer to global memory
  HGLOBAL globalHandle;

  wxText(void);
  wxText(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = -1,
         long style = 0, char *name = "text");
  ~wxText(void);

  Bool Create(wxPanel *panel, wxFunction func, char *label, char *value = "",
         int x = -1, int y = -1, int width = -1, int height = -1,
         long style = 0, char *name = "text");
  char *GetValue(void);
  char *GetLabel(void);
  void SetValue(char *value);
  void SetLabel(char *label);
  void GetSize(int *x, int *y);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetPosition(int *x, int *y);
  void SetFocus(void);

  virtual void ChangeToGray(Bool gray);
  
  // Clipboard operations
  void Copy(void);
  void Cut(void);
  void Paste(void);

  BOOL MSWCommand(UINT param, WORD id);

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;

  void SetEditable(Bool editable);
};

#endif // IN_CPROTO
#endif // wx_txth
