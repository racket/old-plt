/*
 * File:	wb_dialg.cc
 * Purpose:	wxDialogBox and miscellaneous dialog functions
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#include "wx.h"

// Dialog box - like panel but doesn't need a frame, and is modal or
// non-modal

wxbDialogBox::wxbDialogBox(void)
{
  __type = wxTYPE_DIALOG_BOX;
  modal = FALSE;
  SetShown(FALSE);
}

wxbDialogBox::wxbDialogBox(wxWindow *WXUNUSED(Parent), char *WXUNUSED(Title), Bool Modal, 
			   int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height), 
			   long style, char *WXUNUSED(name)):
  wxPanel()
{
  __type = wxTYPE_DIALOG_BOX;
  windowStyle = style;
  modal = Modal;
  SetShown(FALSE);
}

Bool wxbDialogBox::Create(wxWindow *Parent, char *WXUNUSED(Title), Bool Modal, 
			  int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), 
			  int WXUNUSED(height), long style, char *WXUNUSED(name))
{
  windowStyle = style;
  modal = Modal;

  context = wxGetContextForFrame();
  WXGC_IGNORE(this, context);

  wxTopLevelWindows(this)->Append(this);
  wxTopLevelWindows(this)->Show(this, FALSE);

  SetShown(FALSE);

  return TRUE;
}

wxbDialogBox::~wxbDialogBox()
{
  wxTopLevelWindows(this)->DeleteObject(this);
}

void wxbDialogBox::Centre(int direction)
{
  int x_offset,y_offset ;
  int display_width, display_height;
  int  width, height, x, y;
  wxFrame *frame ;
  if (direction & wxCENTER_FRAME)
  {
    frame = (wxFrame*)GetParent() ;
    if (frame)
    {
      frame->GetPosition(&x_offset,&y_offset) ;
      frame->GetSize(&display_width,&display_height) ;
    }
  }
  else
    frame = NULL ;

  if (frame==NULL)
  {
    wxDisplaySize(&display_width, &display_height);
    x_offset = 0 ;
    y_offset = 0 ;
  }


  GetSize(&width, &height);
  GetPosition(&x, &y);

  if (direction & wxHORIZONTAL)
    x = (int)((display_width - width)/2);
  if (direction & wxVERTICAL)
    y = (int)((display_height - height)/2);

  SetSize(x+x_offset, y+y_offset, width, height);
}


