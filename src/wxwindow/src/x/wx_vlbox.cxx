/*
 * File:	wx_vlbox.cc
 * Purpose:	Virtual listbox items implementation (X version)
 * Author:  Sergey Krasnov (ksa@orgland.ru)
 * Created:	1994
 * Updated:
 * Copyright:	(c) 1994
 */

static const char sccsid[] = "%W% %G%";

#include "common.h"

#if USE_VLBOX

#ifdef __GNUG__
#pragma implementation
#endif

#include "wx_panel.h"
#include "wx_lbox.h"
#include "wx_scrol.h"
#include "wx_vlbox.h"
#include "wx_utils.h"

#ifdef wx_xview
#include <xview/scrollbar.h>
#include <xview/panel.h>
#endif

#ifdef wx_motif
#include <Xm/List.h>
#endif

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
#ifdef wx_motif
  labelWidget = NULL;
#endif
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
}

Bool wxVirtListBox::Create(wxPanel * panel, wxFunction func,
                CPFPCPI get_optx, char *garg,
             	char *Title, int nrows, Bool Multiple,
             	int x, int y, int width, int height,
             	long style, char *name)
{
#define DEF_LB_WIDTH	200
#define DEF_LB_HEIGHT	300

    if (!panel)
        return FALSE;

	if (GetOptx(gArg, 0) == NULL)
		Selection = wxERROR;
	else
		Selection = 0;

    int scrollbar_x, scrollbar_y, scrollbar_h, scrollbar_w;
	int listbox_w, listbox_h;

    scrollBar = new wxScrollBar(panel, (wxFunction)&wx_vlbox_scroll_proc,
                    wxVERTICAL);
    scrollBar->GetSize(&scrollbar_w, &scrollbar_h);

	if (width < 0)
        listbox_w = DEF_LB_WIDTH - scrollbar_w;
	else
        listbox_w = width - scrollbar_w;
	if (height < 0) {
		if (nRows > -1)
		   listbox_h = 1000;
		else
		   listbox_h = DEF_LB_HEIGHT;
  	} else
  		listbox_h = height;

#ifdef wx_motif
	listbox_w -= 4;
    itemOrientation = labelPosition;
    listBox = new wxListBox(panel, (wxFunction)&wx_vlbox_listbox_proc,
                    Title,  wxSINGLE | wxALWAYS_SB,
                    x, y, listbox_w, listbox_h, 0, NULL);

    Widget listBoxWidget = (Widget)listBox->GetHandle();
	listBoxLeftOffcet = wxERROR;
	SetLabel(Title);
  	XtVaSetValues(listBoxWidget,
  					XmNselectionPolicy, XmBROWSE_SELECT,
 					XmNscrollBarDisplayPolicy, XmAS_NEEDED,
					NULL);

	if (nRows > -1)
    	XtVaSetValues(listBoxWidget, XmNvisibleItemCount, nRows, NULL) ;
	else
		XtVaGetValues(listBoxWidget, XmNvisibleItemCount, &nRows, NULL) ;
	if (nRows < 2) {
	 	nRows = 2;
    	XtVaSetValues(listBoxWidget, XmNvisibleItemCount, nRows, NULL) ;
	}

    // Calculating itemHeight. I don't now another simple way to do it
    Position xx, yy, yy1;
    Dimension w, h, hh , ww;
    listBox->Append("1");
	listBox->Append("1");
    XmListPosToBounds(listBoxWidget, 1, &xx, &yy, &ww, &hh);
    XmListPosToBounds(listBoxWidget, 2, &xx, &yy1, &ww, &hh);
	itemHeight = yy1 - yy;
	listBoxItemTopOffcet = yy;
	listBox->Clear();

 	XtVaGetValues(listBox->formWidget,
	                XmNx, &xx,
	                XmNy, &yy,
	                XmNwidth, &w,
	                XmNheight, &h,
					NULL);

	listBoxLeftOffcet = w - ww + 4;
    listBoxTopOffcet = (labelPosition == wxVERTICAL) ?
            (h - ScrollBarHeight()) /2 : 0;
    scrollbar_x = xx + w + 4;
    scrollbar_y = yy + listBoxTopOffcet;

#endif
#ifdef wx_xview

    listBox = new wxListBox(panel, (wxFunction)&wx_vlbox_listbox_proc,
                        NULL, wxSINGLE | wxALWAYS_SB,
						x, y, listbox_w, listbox_h, 0, NULL);

   	Panel_item x_listBox =  (Panel_item)(listBox->GetHandle());
	itemHeight = (int) xv_get (x_listBox, PANEL_LIST_ROW_HEIGHT);

    SetLabel(Title);

	if (nRows > -1)
		xv_set(x_listBox, PANEL_LIST_DISPLAY_ROWS, nRows, NULL);
	else
		nRows =  xv_get(x_listBox, PANEL_LIST_DISPLAY_ROWS);

	if (nRows < 2) {
	 	nRows = 2;
		xv_set(x_listBox, PANEL_LIST_DISPLAY_ROWS, nRows, NULL);
	}
	xv_set(x_listBox, PANEL_CHOOSE_NONE,  TRUE, NULL);

	Scrollbar x_lbScrollBar = (Scrollbar) xv_get(x_listBox, PANEL_LIST_SCROLLBAR);
    scrollbar_x = (int) xv_get(x_lbScrollBar, XV_X) + scrollbar_w;
    scrollbar_y = (int) xv_get(x_lbScrollBar, XV_Y);
    xv_set(x_lbScrollBar, XV_SHOW, FALSE, NULL);

#endif

	scrollbar_h = ScrollBarHeight();
    scrollBar->SetSize(scrollbar_x, scrollbar_y, scrollbar_w, scrollbar_h);

  	listBox->wxWindow::SetClientData((char *) this);
  	scrollBar->SetClientData((char *) this);
	Callback (func);
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

#ifdef wx_motif

   	XtVaSetValues((Widget)(listBox->GetHandle()),
			XmNvisibleItemCount, nRows, NULL) ;
	scrollBar->SetSize(-1, -1, -1, ScrollBarHeight());

#endif
#ifdef wx_xview

	xv_set((Panel_item)(listBox->GetHandle()),
		PANEL_LIST_DISPLAY_ROWS, nRows, NULL);
	xv_set((Scrollbar)(scrollBar->GetHandle()),

            XV_HEIGHT, ScrollBarHeight(), NULL);

#endif
	scrollBar->Show(nItems > nRows || (!WasOnBottom && nItems == nRows));
  	scrollBar->SetPageLength(nRows);
	scrollBar->SetViewLength(nRows);
	Refresh();
	int w, h;
	GetSize(&w, &h);
    GetEventHandler()->OnSize(w, h);
}

void wxVirtListBox::SetListSelection(int n, Bool select)
{
#ifdef wx_xview
    xv_set((Panel_item) listBox->GetHandle(),
                PANEL_CHOOSE_NONE, !select, NULL);
#endif
    listBox->SetSelection(n, select);
}

int wxVirtListBox::ScrollBarHeight(void)
{
#ifdef wx_motif
    return nRows * itemHeight + 2 * listBoxItemTopOffcet + 3;
#endif
#ifdef wx_xview
  	Scrollbar x_lbScrollBar = (Scrollbar) xv_get((Panel_item) listBox->handle,
  				 PANEL_LIST_SCROLLBAR);
	return (int) xv_get(x_lbScrollBar, XV_HEIGHT);
#endif
}

void wxVirtListBox::CheckListBoxSize(void)
{
#ifdef wx_motif
    Widget listBoxWidget = (Widget)listBox->GetHandle();
	int _nRows;
	XtVaGetValues(listBoxWidget, XmNvisibleItemCount, &_nRows, NULL) ;
	if (_nRows != nRows) {
		// Horizontal scrollbar appear or disappear
		XtVaSetValues(listBoxWidget, XmNvisibleItemCount, nRows, NULL) ;
		scrollBar->SetSize(-1,-1, -1, ScrollBarHeight());
	}
#endif
}

void wxVirtListBox::SetRedraw(Bool redraw)
{
    if (redraw)
         CheckListBoxSize();

    if (IsVizible) {
        Show(redraw);
        IsVizible = TRUE;
    }
}

Bool wxVirtListBox::Show(Bool show)
{
    SetShown(show);

    IsVizible = show;
    scrollBar->Show(show && (nItems > nRows || (!WasOnBottom && nItems == nRows)));
    listBox->Show(show);
#ifdef wx_xview
	Panel_item x_listBox =  (Panel_item)(listBox->GetHandle());
	Scrollbar x_lbScrollBar = (Scrollbar) xv_get(x_listBox, PANEL_LIST_SCROLLBAR);
    xv_set(x_lbScrollBar, XV_SHOW, FALSE, NULL);
#endif
    return TRUE;
}

void wxVirtListBox::GetPosition(int *x, int *y)
{
	listBox->GetPosition(x, y);
}

void wxVirtListBox::GetSize(int *w, int *h)
{
 	int xl, yl, xs, ys, ws, hs;
 	listBox->GetPosition(&xl, &yl);
 	listBox->GetSize(w, h);
#ifdef wx_motif
	*h = ScrollBarHeight() + itemHeight + listBoxTopOffcet;
#endif
 	scrollBar->GetPosition(&xs, &ys);
 	scrollBar->GetSize(&ws, &hs);
 	*w = xs - xl + ws;
}

void wxVirtListBox::SetSize(int x, int y, int w, int h, int sizeFlags)
{
 	int xs, ys;
 	int x1, y1, w1, h1;
 	GetPosition(&x1, &y1);
 	GetSize(&w1, &h1);
 	scrollBar->GetPosition(&xs, &ys);

  	int _nRows;
#ifdef wx_motif
	int wl, hl;
	listBox->GetSize(&wl, &hl);
	if (w < 0)
		w = w1;
  	listBox->SetSize(x, y, wl + (w - w1), -1);

    SetLabel(listBox->GetLabel());

	if (h > 0 & h != h1)
  		_nRows = max((int) ((h - (h1 - itemHeight * nRows)) / (float) itemHeight + 0.5), 2);
	else
		_nRows = nRows;

   	Widget listBoxWidget = (Widget)listBox->GetHandle();
   	XtVaSetValues(listBoxWidget, XmNvisibleItemCount, _nRows, NULL) ;
	Position xx, yy;
    Dimension ww, hh;
    XmListPosToBounds(listBoxWidget, 1, &xx, &yy, &ww, &hh);
	ys = y + listBoxTopOffcet;
	xs = x  + ww + listBoxLeftOffcet;
#endif

#ifdef wx_xview

  	Panel_item x_listBox = (Panel_item) listBox->handle;
	if (x > 0 && x != x1)
  		xv_set(x_listBox, XV_X, x, NULL);
	if (y > 0 && y != y1)
  		xv_set(x_listBox, XV_Y, y, NULL);
	if (w > 0 & w != w1) {
  		int row_w = (int) xv_get(x_listBox, PANEL_LIST_WIDTH);
  		xv_set(x_listBox, PANEL_LIST_WIDTH, row_w + (w - w1), NULL);
        SetLabel(listBox->GetLabel());
	}

	if (h > 0 & h != h1)
  		_nRows = max((int) ((h - (h1 - itemHeight * nRows)) / (float) itemHeight + 0.5), 2);
	else
		_nRows = nRows;

	Scrollbar x_lbScrollBar = (Scrollbar) xv_get(x_listBox, PANEL_LIST_SCROLLBAR);
    xs = (int) (xv_get(x_lbScrollBar, XV_X) + xv_get(x_lbScrollBar, XV_WIDTH));
    ys = (int) xv_get(x_lbScrollBar, XV_Y);

#endif
  	scrollBar->SetSize(xs, ys, -1, -1);
    SetRowsNumber(_nRows);
    GetEventHandler()->OnSize(w, h);
}                               


void wxVirtListBox::SetLabel(char * label)
{
    if (label && label[0]) {
#ifdef wx_motif
        Dimension formWidth, labelWidth, labelWidth1;
		XtVaGetValues(listBox->formWidget, XmNwidth, &formWidth, NULL) ;
		XtVaGetValues(listBox->labelWidget, XmNwidth, &labelWidth, NULL) ;
#if TRUNCATE_LABEL
        if (labelWidth > formWidth/2) {
 			strcpy(wxBuffer, label);
 			wxBuffer[strlen(label) * formWidth /(2*labelWidth)] = 0;
		}
		listBox->SetLabel(wxBuffer);
#else
        listBox->SetLabel(label);
#endif
        XtVaGetValues(listBox->labelWidget, XmNwidth, &labelWidth1, NULL) ;
		if (listBoxLeftOffcet != wxERROR)
			listBoxLeftOffcet += labelWidth1 - labelWidth;
   		listBox->SetSize(-1, -1, formWidth, -1);
#endif
#ifdef wx_xview
   		Panel_item x_listBox =  (Panel_item)(listBox->GetHandle());
  		Xv_Font the_font = (Xv_Font) xv_get(x_listBox, PANEL_LABEL_FONT);
    	int ch_width = (int) xv_get (the_font, FONT_DEFAULT_CHAR_WIDTH);
		int listWidth = (int) xv_get(x_listBox, XV_WIDTH);//PANEL_LIST_WIDTH);
#if TRUNCATE_LABEL
        if (strlen(label) > listWidth / ch_width) {
 			strcpy(wxBuffer, label);
			wxBuffer[listWidth /ch_width] = 0;
		}
 		xv_set(x_listBox, PANEL_LABEL_STRING, wxBuffer, NULL);
#else
        xv_set(x_listBox, PANEL_LABEL_STRING, label, NULL);
#endif
#endif
    }
     CheckListBoxSize();
}

char * wxVirtListBox::GetLabel(void)
{
    return listBox->GetLabel();
}

void wxVirtListBox::ChangeColour (void)
{
 	listBox->ChangeColour();
 	scrollBar->ChangeColour();
}

#endif
