/*
 * File:	wx_rbox.h
 * Purpose:	Declares radio box item (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_rbox.h	1.2 5/9/94" */

#ifndef wx_rboxh
#define wx_rboxh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_rbox.h"

#ifdef IN_CPROTO
typedef       void    *wxRadioBox ;
#else

// Radio box item
class wxBitmap ;
class wxPanel;
class wxRadioBox: public wxbRadioBox
{
  DECLARE_DYNAMIC_CLASS(wxRadioBox)

 public:
#ifdef wx_motif
  char **radioButtonLabels;
  Widget *radioButtons;
  Bool *radioEnabled;
#endif
  wxBitmap **buttonBitmap ;

  wxRadioBox(void);
  wxRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             int majorDim=0, long style = wxHORIZONTAL, char *name = "radioBox");
  wxRadioBox(wxPanel *panel, wxFunction func, char *Title,
             int x, int y, int width, int height,
             int N, wxBitmap **Choices,
             int majorDim=0, long style = wxHORIZONTAL, char *name = "radioBox");
  ~wxRadioBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             int majorDim=0, long style = wxHORIZONTAL, char *name = "radioBox");
  Bool Create(wxPanel *panel, wxFunction func, char *Title,
             int x, int y, int width, int height,
             int N, wxBitmap **Choices,
             int majorDim=0, long style = wxHORIZONTAL, char *name = "radioBox");
  int FindString(char *s);
  void SetSelection(int N);
  int GetSelection(void);
  char *GetString(int N);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }
#ifdef wx_motif
  void Enable(Bool enable);
#else
  // Avoid compiler warning
  void Enable(Bool Flag) { wxItem::Enable(Flag); }
#endif
  void Enable(int item, Bool enable);
  void Show(int item, Bool show) ;
  // Avoid compiler warning
  Bool Show(Bool show)
  { return wxItem::Show(show); }
  void SetLabel(int item, char *label) ;
  void SetLabel(int item, wxBitmap *bitmap) ;
  // Avoid compiler warning
  void SetLabel(char *label) { wxItem::SetLabel(label); }
  char *GetLabel(int item) ;
  // Avoid compiler warning
  char *GetLabel(void) { return wxItem::GetLabel(); }

  void ChangeColour(void) ;

  int ButtonFocus(int i);
};

#endif // IN_CPROTO
#endif // wx_rboxh
