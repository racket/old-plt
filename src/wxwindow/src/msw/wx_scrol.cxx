/*
 * File:        wx_scrol.cc
 * Purpose:     Scrolbar items implementation (MSW version)
 * Author:    Sergey Krasnov (ksa@orgland.ru)
 * Created:   1995
 * Updated:
 * Copyright:
 */

// static const char sccsid[] = "%W% %G%";

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "common.h"

#include "wx_scrol.h"
#include "wx_privt.h"
#include "wx_itemp.h"
#include "wx_utils.h"
#include "wx_wmgr.h"

#endif

#if USE_SCROLLBAR

#if CTL3D
#include <ctl3d.h>
#endif

extern wxList wxScrollBarList;
extern void wxFindMaxSize(HWND hwnd, RECT *rect);

// Scrollbar
IMPLEMENT_DYNAMIC_CLASS(wxScrollBar, wxItem)

wxScrollBar::wxScrollBar(void)
{
    wxWinType = wxTYPE_HWND;
    windows_id = 0;
    ms_handle = 0;
    pageSize = 0;
    viewSize = 0;
}

wxScrollBar::wxScrollBar(wxPanel *panel, wxFunction func, int direction,
                      int x, int y, int width, int height, long style, char *name):
    wxbScrollBar(panel, func, direction, x, y, width, height, style, name)
{
    Create(panel, func, direction, x, y, width, height, style, name);
}

Bool wxScrollBar::Create(wxPanel *panel, wxFunction func,
                int direction, int x, int y, int width, int height,
                long style, char *name)
{
    if (!panel)
        return FALSE;
    panel->AddChild(this);
    buttonFont = panel->buttonFont ;
    labelFont = panel->labelFont ;
    backColour = panel->backColour ;
    labelColour = panel->labelColour ;
    buttonColour = panel->buttonColour ;
    wxWinType = wxTYPE_HWND;
    windowStyle = style;
    wxWnd *cparent = NULL;
    cparent = (wxWnd *)(panel->handle);
    window_parent = panel;
    labelPosition = panel->label_position;
    panel->GetValidPosition(&x, &y);

    // Now create scrollbar
    windows_id = (int)NewId();
     DWORD _direction = direction == wxHORIZONTAL ?
                        SBS_HORZ: SBS_VERT;
	 HWND scroll_bar = wxwmCreateWindowEx(0, "SCROLLBAR", "scrollbar",
                         _direction | WS_CHILD | WS_VISIBLE,
                         0, 0, 0, 0, cparent->handle, (HMENU)windows_id,
                         wxhInstance, NULL);
#if CTL3D
    Ctl3dSubclassCtl(scroll_bar);
#endif

    wxScrollBarList.Append((long)scroll_bar, this);
    pageSize = 1;
    viewSize = 1;

    ::SetScrollRange(scroll_bar, SB_CTL, 0, 1, FALSE);
    ::SetScrollPos(scroll_bar, SB_CTL, 0, FALSE);
    ShowWindow(scroll_bar, SW_SHOW);

    ms_handle = (HANDLE)scroll_bar;
    SetSize(x, y, width, height);

    panel->AdvanceCursor(this);
    Callback(func);

    return TRUE;
}

wxScrollBar::~wxScrollBar(void)
{
    wxScrollBarList.DeleteObject(this);
}

// Called from wx_win.cc: wxWnd::OnHScroll, wxWnd::OnVScroll
void wxScrollBarEvent(HWND hbar, WORD wParam, WORD pos)
{
    wxNode *node = (wxNode *)wxScrollBarList.Find((long)hbar);
    if (!node)
      return;

    wxScrollBar *scrollBar = (wxScrollBar *)node->Data();
    int position = GetScrollPos(hbar, SB_CTL);

    int nScrollInc;
    switch ( wParam )
    {
            case SB_LINEUP:
                    nScrollInc = -1;
                    break;

            case SB_LINEDOWN:
                    nScrollInc = 1;
                    break;

            case SB_PAGEUP:
                    nScrollInc = -scrollBar->pageSize;
                    break;

            case SB_PAGEDOWN:
                    nScrollInc = scrollBar->pageSize;;
                    break;

            case SB_THUMBTRACK:
                    nScrollInc = pos - position;
                    break;

            default:
                    nScrollInc = 0;
    }

    if (nScrollInc != 0)
    {
        int new_pos = position + nScrollInc;
        int minPos, maxPos;
        GetScrollRange(hbar, SB_CTL, &minPos, &maxPos);
        if (new_pos < 0)
            new_pos = 0;
        if (new_pos > maxPos)
            new_pos = maxPos;

        scrollBar->SetValue(new_pos);
        wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_SCROLLBAR_COMMAND);
        event->commandInt = new_pos;
        event->eventObject = scrollBar;
        scrollBar->ProcessCommand(*event);
    }
}

void wxScrollBar::SetValue(int viewStart)
{
    ::SetScrollPos((HWND)ms_handle, SB_CTL, viewStart, TRUE);
}

int wxScrollBar::GetValue(void)
{
    return ::GetScrollPos((HWND)ms_handle, SB_CTL);
}

void wxScrollBar::SetPageLength(int pageLength)
{
    pageSize = pageLength;
}

void wxScrollBar::SetObjectLength(int objectLength)
{
    ::SetScrollRange((HWND)ms_handle, SB_CTL, 0, objectLength, TRUE);
}

void wxScrollBar::SetViewLength(int viewLength)
{
    viewSize = viewLength;
}


void wxScrollBar::GetValues(int *viewStart, int *viewLength, int *objectLength,
           int *pageLength)
{
    *viewStart = ::GetScrollPos((HWND)ms_handle, SB_CTL);
    *viewLength = viewSize;
    int minPos;
    ::GetScrollRange((HWND)ms_handle, SB_CTL, &minPos, objectLength);
    *pageLength = pageSize;
}

char *wxScrollBar::GetLabel(void)
{
    return NULL;
}

void wxScrollBar::SetLabel(char *label)
{
}

void wxScrollBar::SetBackgroundColour(wxColour*col)
{
}

void wxScrollBar::SetLabelColour(wxColour*col)
{
}

void wxScrollBar::SetButtonColour(wxColour*col)
{
}

#endif
