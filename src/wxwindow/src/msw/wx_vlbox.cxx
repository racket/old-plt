/*
 * File:        wx_vlboc.cc
 * Purpose:     Scrolbar items implementation (MSW version)
 * Author:     Sergey Krasnov (ksa@orgland.ru)
 * Created:   1995
 * Updated:
 * Copyright:
 */

// static const char sccsid[] = "%W% %G%";

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "common.h"

#endif

#if USE_VLBOX

#include "wx_panel.h"
#include "wx_lbox.h"
#include "wx_vlbox.h"
#include "wx_scrol.h"
#include "wx_privt.h"
#include "wx_utils.h"

void wx_vlbox_listbox_proc(wxListBox& list, wxCommandEvent& event);
void wx_vlbox_scroll_proc(wxScrollBar& scrollBar, wxCommandEvent& event);

void wx_vlbox_listbox_proc(wxListBox& list, wxCommandEvent& event)
{
    wxVirtListBox * vlb = (wxVirtListBox *)list.wxWindow::GetClientData();
    if (event.commandInt != wxERROR && event.commandInt < vlb->GetRowsNumber())
        vlb->SetSelection(event.commandInt + vlb->GetViewStart());
    else
        vlb->HideSelection();
    event.commandInt = vlb->GetSelection();
}

void wx_vlbox_scroll_proc(wxScrollBar& scrollBar, wxCommandEvent& event)
{
    wxVirtListBox * vlb = (wxVirtListBox *)scrollBar.GetClientData();
    vlb->SetViewStart(event.commandInt);
}

IMPLEMENT_DYNAMIC_CLASS(wxVirtListBox, wxItem)

wxVirtListBox::wxVirtListBox(void)
{
    wxWinType = 0;
    windows_id = 0;
	listBoxVOffset = 0;
}

wxVirtListBox::wxVirtListBox(wxPanel * panel, wxFunction func,
                CPFPCPI get_optx, char *garg,
               char *Title, int nrows, Bool Multiple,
               int x, int y, int width, int height,
               long style, char *name):
               wxbVirtListBox(panel, func, get_optx, garg, Title, nrows,
               Multiple, x, y, width, height, style, name)
{
    Create(panel, func, get_optx, garg, Title, nrows,
               Multiple, x, y, width, height, style, name);
}

wxVirtListBox::~wxVirtListBox(void)
{
    ms_handle = NULL;
}

Bool wxVirtListBox::Create(wxPanel * panel, wxFunction func,
                CPFPCPI get_optx, char *garg,
               char *Title, int nrows, Bool Multiple,
               int x, int y, int width, int height,
               long style, char *name)
{
#define DEF_LB_WIDTH   200
#define DEF_LB_HEIGHT  300

    if (!panel)
		return FALSE;
    wxWinType = 0;
    windows_id = 0;
	listBoxVOffset = 0;

    wxWinType = wxERROR;

    if (GetOptx(gArg, 0) == NULL)
        Selection = wxERROR;
    else
       Selection = 0;

    int listbox_w, listbox_h;

    int scrollBarWidth = GetSystemMetrics(SM_CXVSCROLL) + 2;
    if (width < 0)
       listbox_w = DEF_LB_WIDTH - scrollBarWidth;
    else
       listbox_w = width - scrollBarWidth;
    if (height < 0) {
        if (nRows > -1)
            listbox_h = 1000;
        else
            listbox_h = DEF_LB_HEIGHT;
    } else
        listbox_h = height - scrollBarWidth;

    listBox = new wxListBox(panel, (wxFunction)&wx_vlbox_listbox_proc,
                        Title,
                        wxSINGLE | wxNEEDED_SB,
                        x, y, listbox_w, listbox_h, 0, NULL, wxHSCROLL);

    listBox->GetPosition(&x, &y);
    if (Title) {
		RECT rect;
    	wxWindow *parent = listBox->GetParent();
    	GetWindowRect((HWND)listBox->ms_handle, &rect);
    	POINT point;
    	point.x = rect.left;
    	point.y = rect.top;
    	if (parent) {
      		wxWnd *cparent = (wxWnd *)(parent->handle);
      		::ScreenToClient(cparent->handle, &point);
    	}
		listBoxVOffset = point.y - y;
	}

  	listBox->GetSize(&listbox_w, &listbox_h);
  	int listRowHeight = (int)SendMessage((HWND)listBox->ms_handle, LB_GETITEMHEIGHT, 0, 0L);

   	if (nRows > -1)
     	listBox->SetSize(-1, -1, listbox_w, ScrollBarHeight() + listBoxVOffset);
   	else
      	nRows = (listbox_h - listBoxVOffset) / listRowHeight;

   	if (nRows < 2) {
       	nRows = 2;
     	listBox->SetSize(-1, -1, listbox_w, ScrollBarHeight() - listBoxVOffset);
    }

    scrollBar = new wxScrollBar(panel, (wxFunction)&wx_vlbox_scroll_proc, wxVERTICAL,
           x + listbox_w, y + listBoxVOffset, scrollBarWidth, ScrollBarHeight());

    SetLabel(Title);

    listBox->wxWindow::SetClientData((char *) this);
    scrollBar->SetClientData((char *) this);
    Callback (func);
    panel->AdvanceCursor(this);
    ms_handle = listBox->ms_handle;

    scrollBar->SetPageLength(nRows);
    scrollBar->SetViewLength(nRows);
    scrollBar->SetObjectLength(2*nRows);
    nLastViewStart = 0;
    SetViewStart(0, TRUE);
    SetSelection(0);

    return TRUE;
}

void wxVirtListBox::SetRowsNumber(int nRowsNew)
{
    if (nRows == nRowsNew || nRows <= 0)
        return;
    nRows = max(nRowsNew, 2);
    CheckListBoxSize();
    int scrollBarWidth = GetSystemMetrics(SM_CXVSCROLL) + 2;
    scrollBar->SetSize(-1, -1, scrollBarWidth, ScrollBarHeight());
    scrollBar->Show(nItems > nRows || (!WasOnBottom && nItems == nRows));

    scrollBar->SetPageLength(nRows);
    scrollBar->SetViewLength(nRows);

    int w, h;
    GetSize(&w, &h);
    GetEventHandler()->OnSize(w, h);
    Refresh();
}

void wxVirtListBox::SetListSelection(int n, Bool select)
{
    listBox->SetSelection(n, select);
}

int wxVirtListBox::ScrollBarHeight(void)
{
    int listRowHeight = (int)SendMessage((HWND)listBox->ms_handle, LB_GETITEMHEIGHT, 0, 0L);
    return listRowHeight * nRows + 2;
}


// Correction of listbox size when horizontal scrollbar appear/disappear
void wxVirtListBox::CheckListBoxSize(void)
{
    int w, h;
   	listBox->GetClientSize(&w, &h);
  	int listRowWidth = w;
    listBox->GetSize(&w, &h);
    int scrollBarWidth = GetSystemMetrics(SM_CXVSCROLL) + 2;

    int hextent = (int) SendMessage((HWND)listBox->ms_handle, LB_GETHORIZONTALEXTENT, 0, 0L);
    if (hextent >= listRowWidth)
        listBox->SetSize(-1, -1, w, ScrollBarHeight() + scrollBarWidth + listBoxVOffset);
    else if (hextent < listRowWidth)
        listBox->SetSize(-1, -1, w, ScrollBarHeight() + listBoxVOffset);
}

void wxVirtListBox::SetRedraw(Bool redraw)
{
    if (redraw)
         CheckListBoxSize();
    SendMessage((HWND)listBox->ms_handle, WM_SETREDRAW, redraw, 0L);
    if (redraw) {
        InvalidateRect((HWND)listBox->ms_handle,  NULL, FALSE);
        InvalidateRect((HWND)scrollBar->ms_handle,  NULL, FALSE);
	}
}

Bool wxVirtListBox::Show(Bool show)
{
  SetShown(show);
    IsVizible = show;
    scrollBar->Show(show && (nItems > nRows || (!WasOnBottom && nItems == nRows)));
    listBox->Show(show);
    return TRUE;
}

void wxVirtListBox::GetPosition(int *x, int *y)
{
    listBox->GetPosition(x, y);
}

void wxVirtListBox::GetSize(int *w, int *h)
{
    int scrollBarWidth = GetSystemMetrics(SM_CXVSCROLL) + 2;
    listBox->GetSize(w, h);
    *h = ScrollBarHeight() + scrollBarWidth + listBoxVOffset;
    *w += scrollBarWidth;
}

void wxVirtListBox::SetSize(int x, int y, int w, int h, int sizeFlags)
{
	int w1, h1, wl, hl;
    int _nRows;
//    int voffset;
    GetSize(&w1, &h1);
    listBox->GetSize(&wl, &hl);
    int scrollBarWidth = GetSystemMetrics(SM_CXVSCROLL) + 2;
    char buff[256];
	strcpy(buff, listBox->GetLabel());
	listBox->SetLabel(" "); // now the size of listbox doesn't depend on the
							// size of label 
    if (w > 0)
        wl += w - w1;

    listBox->SetSize(x, y, wl, ScrollBarHeight());

    if (h > 0) {
        int listRowHeight = (int)SendMessage((HWND)listBox->ms_handle, LB_GETITEMHEIGHT, 0, 0L);
        _nRows = max((int)((h - scrollBarWidth - listBoxVOffset) / (float) listRowHeight + 0.5), 2);
    } else {
        _nRows = nRows;
    }

    scrollBar->SetSize(x + wl, y + listBoxVOffset, scrollBarWidth, ScrollBarHeight());
    SetLabel(buff);
    SetRowsNumber(_nRows);
    GetSize(&w1, &h1);
    GetEventHandler()->OnSize(w1, h1);
}

void wxVirtListBox::SetLabel(char * label)
{
	if (label && label[0]) {
#if TRUNCATE_LABEL
        float label_w, label_h;
		listBox->GetTextExtent((LPSTR)label, &label_w, &label_h, NULL, NULL, listBox->labelFont);
 		label_w += 10;
		int w, h;
		listBox->GetSize(&w, &h);
		if (labelPosition == wxHORIZONTAL)
			w = w/2;
    	if (label_w > w) {
			strcpy(wxBuffer, label);
			wxBuffer[(int)(strlen(label) * w / label_w)] = 0;
			listBox->SetLabel(wxBuffer);
		} else
			listBox->SetLabel(label);
        CheckListBoxSize();
#else
        listBox->SetLabel(label);
		CheckListBoxSize();
#endif
    }
}

char * wxVirtListBox::GetLabel(void)
{
    return listBox->GetLabel();
}

void wxVirtListBox::SetBackgroundColour(wxColour*col)
{
}

void wxVirtListBox::SetLabelColour(wxColour*col)
{
}

void wxVirtListBox::SetButtonColour(wxColour*col)
{
}

#endif
