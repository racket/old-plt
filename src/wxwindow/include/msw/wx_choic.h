/*
 * File:	wx_choic.h
 * Purpose:	Choice panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_choic.h	1.2 5/9/94" */

#ifndef wx_choich
#define wx_choich

#include "wb_choic.h"

#ifdef IN_CPROTO
typedef       void    *wxChoice ;
#else

// Choice item
class wxChoice: public wxbChoice
{
  DECLARE_DYNAMIC_CLASS(wxChoice)

 public:
  HWND static_label;

  wxChoice(void);
  wxChoice(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice");
  ~wxChoice(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title,
           int x = -1, int y = -1, int width = -1, int height = -1,
           int N = 0, char **Choices = NULL,
           long style = 0, char *name = "choice");
  void Append(char *Item);
  void Clear(void);
  int GetSelection(void);
  void SetSelection(int n);
  int FindString(char *s);
  char *GetString(int n);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetSize(int *x, int *y);
  void GetPosition(int *x, int *y);
  char *GetLabel(void);
  void SetLabel(char *label);

  BOOL MSWCommand(UINT param, WORD id);

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;

  virtual void ChangeToGray(Bool gray);
  
  virtual Bool Show(Bool s);

  inline void SetColumns(int WXUNUSED(n) = 1 ) { /* No effect */ } ;
  inline int GetColumns(void) { return 1 ; };
};

#endif // IN_CPROTO
#endif // wx_choich
