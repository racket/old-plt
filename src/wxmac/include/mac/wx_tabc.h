/*
 * File:	wx_tabc.h
 * Purpose:	Tab group panel item
 * Author:	Matthew
 * Created:	2002
 * Copyright:	(c) 2004 PLT Scheme, Inc.
 * Copyright:	(c) 2002, PLT
 */

#ifndef wx_tabch
#define wx_tabch

#include "wx_item.h"

class wxTabChoice : public wxItem
{
 public:
  wxTabChoice(wxPanel *panel, wxFunction func, char *label, 
              int N, char **Choices, int style);
  ~wxTabChoice();

  int   GetSelection(void);
  int   Number(void);
  void  SetSelection(int n);

  void Append(char *s);
  void Delete(int i);

  virtual void DoShow(Bool show);
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);

  virtual char *GetLabel();

  virtual void Refresh(void);

  int tab_count;
  char **tab_labels;

  ControlHandle pane;

  virtual void Activate(Bool gray);
  virtual void MaybeMoveControls();

protected:
  virtual void ChangeToGray(Bool gray);
  virtual void Paint(void);
  virtual void OnEvent(wxMouseEvent *event);
};

#endif // wx_tabch
