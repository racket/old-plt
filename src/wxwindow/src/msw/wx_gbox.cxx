/*
 * File:	wx_gbox.cc
 * Purpose:	Group box item implementation
 * Author:	Matthew Flatt
 * Created:	2003
 * Copyright:	(c) 2004 PLT Scheme, Inc.
 * Copyright:	(c) 2003, PLT
 */

#include "wx.h"
#include "wx_gbox.h"

#define GROUP_CLASS      "wxBUTTON"
#define GROUP_FLAGS      (BS_GROUPBOX|WS_CHILD|WS_VISIBLE)

wxGroupBox::wxGroupBox(wxPanel *panel, char *Title, long _style):
  wxItem(panel)
{
  int x = 0, y = 0, width, height, nid;
  wxWnd *cparent;
  char *the_label;
  HWND the_handle;

  __type = wxTYPE_GROUP_BOX;

  panel->AddChild(this);
  wxWinType = wxTYPE_HWND;

  cparent = (wxWnd *)(panel->handle);

  panel->GetValidPosition(&x, &y);

  the_label = copystring(Title ? Title : "");

  the_handle = cparent->handle;

  nid = NewId(this);

  ms_handle = wxwmCreateWindowEx(0, GROUP_CLASS, the_label,
				 GROUP_FLAGS
				 | ((_style & wxINVISIBLE) ? 0 : WS_VISIBLE),
				 0, 0, 0, 0,
				 cparent->handle, (HMENU)nid,
				 wxhInstance, NULL);

  wxSetWinFont(labelFont, ms_handle);

  SubclassControl((HWND)ms_handle);

  the_handle = cparent->handle;

  if (the_label) {
    float label_width = 0;
    float label_height = 0;
    int char_width, ignored;

    GetTextExtent(wxStripMenuCodes(the_label), &label_width, &label_height, NULL, NULL, labelFont);
    wxGetCharSize((HWND)ms_handle, &char_width, &ignored, labelFont);
    label_width += 3 * char_width; /* space before & after label */
    width = label_width;
    height = label_height + 4;
  } else {
    width = 10;
    height = 4;
  }

  SetSize(x, y, width, height);
  panel->AdvanceCursor(this);

  BringWindowToTop((HWND)ms_handle);

  if (_style & wxINVISIBLE)
    Show(FALSE);
}

wxGroupBox::~wxGroupBox(void)
{
}

void wxGroupBox::SetSize(int x, int y, int width, int height, int WXUNUSED(sizeFlags))
{
  int currentX, currentY;

  GetPosition(&currentX, &currentY);
  if (x == -1)
    x = currentX;
  if (y == -1)
    y = currentY;
  
  MoveWindow((HWND)ms_handle, x, y, width, height, TRUE);
  OnSize(width, height);
}
