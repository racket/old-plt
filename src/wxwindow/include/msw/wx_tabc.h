/*
 * File:	wx_tabc.h
 * Purpose:	Tab group panel item
 * Author:	Matthew
 * Created:	2002
 * Copyright:	(c) 2002, PLT
 */

#ifndef wx_tabch
#define wx_tabch

#include "wx_item.h"

class wxTabChoice : public wxItem
{
 public:
  wxTabChoice(wxPanel *panel, wxFunction func, char *label, 
              int N, char **Choices);
  ~wxTabChoice();

  int   GetSelection(void);
  int   Number(void);
  void  SetSelection(int n);
  void  Enable(Bool enable);

  void Append(char *s);
  void Delete(int i);

  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);

  BOOL MSWCommand(UINT param, WORD id);
};

#endif // wx_tabch
