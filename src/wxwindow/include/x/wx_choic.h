/*
 * File:	wx_choic.h
 * Purpose:	Declares wxChoice panel item (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_choic.h	1.2 5/9/94" */

#ifndef wx_choich
#define wx_choich

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_list.h"
#include "wb_choic.h"

#ifdef IN_CPROTO
typedef       void    *wxChoice ;
#else

class wxPanel;

// Choice item
class wxChoice: public wxbChoice
{
  DECLARE_DYNAMIC_CLASS(wxChoice)
 private:
  int no_strings;
 public:
#ifdef wx_motif
  Widget menuWidget;
  Widget buttonWidget;
  Widget *widgetList ;
  Widget rowWidget;
  wxStringList stringList;
#endif
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
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }
  virtual void Append(char *Item);
  virtual void Clear(void);
  virtual int GetSelection(void);
  virtual void SetSelection(int n);
  virtual int FindString(char *s);
  virtual char *GetString(int n);

  void ChangeColour(void) ;

  void SetColumns(int n = 1 );
  int  GetColumns(void);

  int Number();
};

#endif // IN_CPROTO
#endif // wx_choich
