/*
 * File:	wb_scrol.cc
 * Purpose: Scrollbar items implementation
 * Author:  Sergey Krasnov (ksa@orgland.ru)
 * Created: 1995
 * Updated:
 * Copyright:
 */

#include "wx.h"

wxbScrollBar::wxbScrollBar(void)
{
    __type = wxTYPE_SCROLL_BAR;
    buttonFont = NULL ;
    labelFont = NULL ;
    backColour = NULL ;
    labelColour = NULL ;
    buttonColour = NULL ;
}

wxbScrollBar::wxbScrollBar(wxPanel *panel, wxFunction func, int direction,
			   int x, int y, int width, int height, long style, char *name)
{
    __type = wxTYPE_SCROLL_BAR;
    if (!panel)
        return;
    window_parent = panel;
    labelPosition = panel->label_position;
    buttonFont = panel->buttonFont ;
    labelFont = panel->labelFont ;
    backColour = panel->backColour ;
    labelColour = panel->labelColour ;
    buttonColour = panel->buttonColour ;
}

wxbScrollBar::~wxbScrollBar(void)
{
}

void wxbScrollBar::Command(wxCommandEvent& event)
{
    SetValue(event.commandInt);
    ProcessCommand(event);
}

void wxbScrollBar::ProcessCommand(wxCommandEvent& event)
{
    if (wxNotifyEvent(event, TRUE))
        return;

    wxFunction fun = callback;
    if (fun) {
        (void)(*(fun))(*this, event);
    }
    wxNotifyEvent(event, FALSE);
}
