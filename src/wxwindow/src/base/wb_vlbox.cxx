/*
 * File:    wb_vlbox.cc
 * Purpose: Virtual list box items implementation
 * Author:  Sergey Krasnov (ksa@orgland.ru) 
 * Created: 1995
 * Updated:
 * Copyright:
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"

#include "wx_stdev.h"
#include "wx_utils.h"
#include "wx_lbox.h"

#endif

#if USE_VLBOX

#include "wx_scrol.h"
#include "wx_vlbox.h"

#include <ctype.h>
#include <stdlib.h>

wxbVirtListBox::wxbVirtListBox(void)
{
    __type = wxTYPE_VIRT_LIST_BOX;
    buttonFont = NULL;
    labelFont = NULL;
    backColour = NULL;
    labelColour = NULL;
    buttonColour = NULL;

    nRows = 0;
    nItems = 0;
    Selection = wxERROR;
    nLastViewStart = wxERROR;
    WasOnBottom = FALSE;
    IsVizible = FALSE;
    GetOptx = NULL;
    gArg = NULL;
    scrollBar = NULL;
    listBox = NULL;
}

wxbVirtListBox::wxbVirtListBox(wxPanel * panel, wxFunction func,
                CPFPCPI get_optx, char *garg,
               char *Title, int nrows, Bool Multiple,
               int x, int y, int width, int height,
               long style, char *name)
{
    if (!panel)
        return;
    __type = wxTYPE_VIRT_LIST_BOX;
    windowStyle = style;
    window_parent = panel;
    buttonFont = panel->buttonFont;
    labelFont = panel->labelFont;
    backColour = panel->backColour;
    labelColour = panel->labelColour;
    buttonColour = panel->buttonColour;
    labelPosition = panel->label_position;

    nRows = nrows;
    nItems = 0;
    Selection = wxERROR;
    nLastViewStart = wxERROR;
    WasOnBottom = FALSE;
    IsVizible = FALSE;
    GetOptx = get_optx;
    gArg = garg;
    scrollBar = NULL;
    listBox = NULL;
}

wxbVirtListBox::~wxbVirtListBox(void)
{
    if (scrollBar)
        delete scrollBar;
    scrollBar = NULL;
    if (listBox)
        delete listBox;
    listBox = NULL;
}

void wxbVirtListBox::Refresh(void)
{
    SetViewStart(nLastViewStart, TRUE);
}

void wxbVirtListBox::SetViewStart(int nViewStart, Bool Refresh_)
{

    if (WasOnBottom && nViewStart + nRows > nItems)
        // May be impossible situation
        nViewStart = max(nItems - nRows, 0);

    // Now checking Does user select "Got to bottom"?
    if (!WasOnBottom) {
        int viewStart, viewLength, objectLength, pageLength;
        scrollBar->GetValues(&viewStart, &viewLength, &objectLength, &pageLength);
        if (nViewStart + viewLength == objectLength)
        // Bottom cable anchor pressed (in XView)
            GoToBottom(nViewStart);
    }

    if (!Refresh_ && nViewStart == nLastViewStart) { 
    	// Nothing to do
        return;
	}        

    SetRedraw(FALSE);
    int row, n;
    if (Refresh_ || abs(nLastViewStart - nViewStart) >= nRows) {
        // Filling the whole list
        listBox->Clear();
        for (row = 0; row < nRows && InsertString(nViewStart, row); row++) {}

    } else if (nLastViewStart > nViewStart) {
        // Scrolling up
        n = nRows - (nLastViewStart - nViewStart);
        for (row = n; row < nRows; row++)
            listBox->Delete(n);
        n = nLastViewStart - nViewStart;
        for (row = 0; row < n && InsertString(nViewStart, row); row++) {}

    } else {
        // Scrolling down
        n = nViewStart - nLastViewStart;
        for (row = 0; row < n; row++)
            listBox->Delete(0);
        for (row = nRows - (nViewStart - nLastViewStart);
            row < nRows && InsertString(nViewStart, row); row++) {}
    }

    if (!WasOnBottom && nItems <  nViewStart + nRows) {
            nItems = nViewStart + nRows;
            scrollBar->SetObjectLength(nItems + nRows + 1);
    }

    nLastViewStart = nViewStart;
    if (scrollBar->GetValue() != nViewStart)
        scrollBar->SetValue(nViewStart);

    SetSelection(Selection);
    SetRedraw(TRUE);
}

Bool wxbVirtListBox::InsertString(int& nViewStart, int pos)
{
    if (WasOnBottom && pos + nViewStart >= nItems)
        return FALSE;

    char * item;
    if ((item = GetOptx(gArg, pos + nViewStart)) != NULL) {
        listBox->InsertItems(1, &item, pos);
        if (nViewStart + pos + 1 == nRows && !WasOnBottom &&
                (item = GetOptx(gArg, nRows + 1)) == NULL) {
            pos ++;
        }
    }
    if (!item) { // no items more
        WasOnBottom = TRUE;
        nItems = nViewStart + pos;
        nViewStart =  max(nItems - nRows, 0);
        scrollBar->SetObjectLength(nItems);
        if (scrollBar->GetValue() != nViewStart)
            scrollBar->SetValue(nViewStart);
        if (nItems <= nRows) {
            scrollBar->Show(FALSE);
            if (nItems < nRows) {
                nLastViewStart = nViewStart;
                SetRowsNumber(nItems);
            }
        } else {
            listBox->Clear();
            int row;
            for (row = 0; row < nRows; row++)
              InsertString(nViewStart, row);
        }
        return FALSE;
    }
    return TRUE;
}

void wxbVirtListBox::GoToBottom(int& nViewStart)
{
    while (GetOptx(gArg, nItems))
        nItems++;
    WasOnBottom = TRUE;
    nViewStart =  max(nItems - nRows, 0);
    scrollBar->SetObjectLength(nItems);
    if (scrollBar->GetValue() != nViewStart)
        scrollBar->SetValue(nViewStart);
}

void wxbVirtListBox::SetSelection(int n)
{
    char * item = NULL;
    if (n == wxERROR ||
            (!WasOnBottom && n >= nLastViewStart + nRows && (item = GetOptx(gArg, n)) == NULL) ||
            (!WasOnBottom && n >= nItems))
        HideSelection();
    else {
        Selection = n;
        if (nLastViewStart <= n && nLastViewStart + nRows > n)  {
            SetListSelection(n - nLastViewStart, TRUE);
        } else {
            int lbSelection = listBox->GetSelection();
            if (lbSelection != wxERROR) {
                SetListSelection(lbSelection, FALSE);
            }
        }
        wxCommandEvent event(wxEVENT_TYPE_VIRT_LISTBOX_COMMAND);
        event.commandInt = Selection;
        if (!item)
            item = GetOptx(gArg, n);
        event.commandString = copystring(item);
        event.eventObject = this;
        ProcessCommand(event);
        delete event.commandString ;
   }

}

void wxbVirtListBox::HideSelection(void)
{
    Selection = wxERROR;
    int lbSelection = listBox->GetSelection();
    if (lbSelection != wxERROR)
        SetListSelection(lbSelection, FALSE);
    wxCommandEvent event(wxEVENT_TYPE_VIRT_LISTBOX_COMMAND);
    event.commandInt = wxERROR;
    event.commandString = NULL;
    event.eventObject = this;
    ProcessCommand(event);
}

void wxbVirtListBox::Enable(Bool enable)
{
    listBox->Enable(enable);
    scrollBar->Enable(enable);
}

void wxbVirtListBox::Command (wxCommandEvent & event)
{
    ProcessCommand (event);
}

void wxbVirtListBox::ProcessCommand (wxCommandEvent & event)
{
    if (wxNotifyEvent (event, TRUE))
        return;

    wxFunction fun = callback;
    if (fun)
        (void) (*(fun)) (*this, event);

    wxNotifyEvent (event, FALSE);

}

int wxbVirtListBox::SearchItem(char * searchStr, Bool searchDown, Bool searchCase)
{
    if (GetSelection() == wxERROR)
        return wxERROR;

//     Here is version based on strstr function.
//  More efficien then StringMatch, but not enough
    char * item;
    if (searchCase) {
        int i;
        for (i = GetSelection() + (searchDown ? 1 : (-1));
                (item = GetOptx(gArg, i)) != NULL && i >=0;
                searchDown ? i++ : i--)
            if (strstr(item, searchStr))
                return i;
    } else {
        char *c, *cc;
        char serchStrUpcase[128];
        for (c = searchStr, cc = serchStrUpcase; *c; c++, cc++)
            *cc = toupper(*c);
        *cc = 0;
        int i;
        for (i = GetSelection() + (searchDown ? 1 : (-1));
                (item = GetOptx(gArg, i)) != NULL && i >=0;
                searchDown ? i++ : i--) {
            strcpy(wxBuffer, item);;
            for (c = wxBuffer; *c; c++)
                *c = toupper(*c);
            if (strstr(wxBuffer, serchStrUpcase))
                return i;
        }
    }
/*

// Here is the version based on wxWin StringMatch function. It is too slow.
    char * item;
    int i;
    for (i = GetSelection() + (searchDown ? 1 : (-1)); (item = GetOptx(gArg, i)) != NULL && i >=0;
               searchDown ? i++ : i--) {
        if (::StringMatch(searchStr, item, TRUE, searchCase))
            return i;
   }
*/
    return wxERROR;
}

#endif
