/*								-*- C++ -*-
 * $Id: ListBox.cc,v 1.16 1999/11/04 17:25:38 mflatt Exp $
 *
 * Purpose: list box panel item
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "ListBox.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxListBox
#define  Uses_wxStringList
#include "wx.h"
#define  Uses_TraversingEnforcerWidget
#define  Uses_MultiListWidget
#define  Uses_ScrollWinWidget
#include "widgets.h"

// don't allocate or free for every append or delete
#define LIST_CHUNK_SIZE	20
#define MULTILIST	((XfwfMultiListWidget)(X->handle))

#define wxLIST_BOX_WIDTH	70
#define wxLIST_BOX_HEIGHT	50

char *wxchoice_unprotect_amp(char *s);

//-----------------------------------------------------------------------------
// create and destroy wxListBox
//-----------------------------------------------------------------------------

wxListBox::wxListBox(void) : wxItem()
{
    __type = wxTYPE_LIST_BOX;

    AllowDoubleClick(TRUE);

    choices = client_data = NULL;
    num_choices = 0;
    num_free = 0;
}

wxListBox::wxListBox(wxPanel *panel, wxFunction func, char *title,
		     Bool multiple, int x, int y, int width, int height,
		     int n, char **_choices, long style, char *name) : wxItem()
{
    __type = wxTYPE_LIST_BOX;

    AllowDoubleClick(TRUE);

    choices = client_data = NULL;
    num_choices = 0;
    num_free = 0;

    Create(panel, func, title, multiple, x, y, width, height,
	   n, _choices, style, name);
}

Bool wxListBox::Create(wxPanel *panel, wxFunction func, char *title,
		       Bool multiple, int x, int y, int width, int height,
		       int n, char **choices, long style, char *name)
{
    ChainToPanel(panel, style|long(multiple), name);

    Bool vert = (panel->GetLabelPosition() == wxVERTICAL);

    title = wxGetCtlLabel(title);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfTraversingEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNlabel,       title,
	 XtNalignment,   vert ? XfwfTop : XfwfTopLeft,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 NULL);
    // create viewport
    X->scroll = XtVaCreateManagedWidget
	("viewport", xfwfScrolledWindowWidgetClass, X->frame,
	 XtNhideHScrollbar, TRUE,
	 XtNbackground, bg->GetPixel(cmap), /* MATTHEW */
	 XtNdoScroll, FALSE,
	 XtNhighlightThickness, 2,
	 NULL);
    // create multi list
    X->handle = XtVaCreateManagedWidget
	("list", xfwfMultiListWidgetClass, X->scroll,
	 XtNbackground,     bg->GetPixel(cmap),
	 XtNforeground,     fg->GetPixel(cmap),
	 XtNfont,           font->GetInternalFont(),
	 XtNborderWidth,    0,
	 XtNshadeSurplus,   FALSE,
	 XtNdefaultColumns, 1,
	 XtNforceColumns,   TRUE,
	 XtNcursor,         NULL,
	 XtNmaxSelectable,  (multiple & (wxMULTIPLE | wxEXTENDED)) ? 10000 : 1,
	 XtNclickExtends,   (Boolean)(multiple & wxEXTENDED),
	 NULL);

    Set(n, choices);
    // configure scrollbar
    Dimension row_height;
    XtVaGetValues(X->handle, XtNrowHeight, &row_height, NULL);
    XtVaSetValues(X->scroll,  XtNvScrollAmount, row_height, NULL);
    // callback
    callback = func;
    XtAddCallback(X->handle, XtNcallback,
		  wxListBox::EventCallback,  (XtPointer)this);

    long labelw = 0, labelh = 0;
    if (title) {
      float w, h;
      char *label_stripped;
      label_stripped = wxchoice_unprotect_amp(title);
      GetTextExtent(label_stripped, &w, &h, NULL, NULL, label_font);
      if (vert)
	labelh = (long)h;
      else
	labelw = (long)w;
    }

    panel->PositionItem(this, x, y,
			(width  > -1 ? width  : (wxLIST_BOX_WIDTH + labelw)),
			(height > -1 ? height : (wxLIST_BOX_HEIGHT + labelh)));
    AddEventHandlers();

    XtVaSetValues(X->handle, XtNwidth, 0, NULL);

    return TRUE;
}

wxListBox::~wxListBox(void)
{
    Clear();
}

//-----------------------------------------------------------------------------
// override parent methods
//-----------------------------------------------------------------------------

void wxListBox::ChangeColours(void)
{
    wxItem::ChangeColours();
    if (X->scroll) {
	if (parent->GetBackgroundColour())
	    XtVaSetValues(X->scroll, XtNbackground,
			  parent->GetBackgroundColour()->GetPixel(cmap), NULL);
	if (label_fg)
	    XtVaSetValues(X->scroll, XtNforeground,
			  label_fg->GetPixel(cmap), NULL);
    }
}

void wxListBox::SetSize(int x, int y, int width, int height, int flags)
{
    if (width > -1)
	XtVaSetValues(X->handle, XtNlongest, Dimension(width), NULL); 
    wxItem::SetSize(x, y, width, height, flags);
    XtVaSetValues(X->handle, XtNwidth, 0, NULL);
}

//-----------------------------------------------------------------------------
// change contents of wxListBox
//-----------------------------------------------------------------------------

void wxListBox::Append(char *item)
{
  int i, count, *selections;

  count = GetSelections(&selections);

  if (num_free == 0) {
    num_free = LIST_CHUNK_SIZE;
    char    **new_choices     = new char *[num_choices+LIST_CHUNK_SIZE];
    char    **new_client_data = new char *[num_choices+LIST_CHUNK_SIZE];
    // copy current choices
    for (i=0; i<num_choices; ++i) {
      new_choices[i] = choices[i];
      new_client_data[i] = client_data[i];
    }
    // delete old arrays
    delete choices;      choices = new_choices;
    delete client_data;  client_data = new_client_data;
  }
  // set new item
  choices[num_choices]     = copystring(item);
  client_data[num_choices] = NULL;
  // one choice more, one free space less
  ++num_choices; --num_free;
  SetInternalData();

  while (count--) {
    SetSelection(selections[count], TRUE);
  }
}

void wxListBox::Append(char *item, char *_client_data)
{
    Append(item);
    client_data[num_choices-1] = _client_data;
}

void wxListBox::Clear(void)
{
    if (choices) {
	// free strings
	for (int i=0; i<num_choices; ++i)
	    delete choices[i];
	// free array
	delete choices;
	choices = NULL;
    }
    if (client_data) {
	delete client_data;
	client_data = NULL;
    }
    num_choices = num_free = 0;
    SetInternalData();
}

void wxListBox::Delete(int n)
{
    if (0 <= n && n < num_choices) {
      int i, count, *selections;
      
      count = GetSelections(&selections);


      delete choices[n]; // free string;
      for (i=n+1; i<num_choices; ++i) { // shrink arrays
	choices[i-1] = choices[i];
	client_data[i-1] = client_data[i];
      }
      --num_choices; ++num_free;
      SetInternalData();

      while (count--) {
	if (selections[count] < n)
	  SetSelection(selections[count], TRUE);
	else if (selections[count] > n)
	  SetSelection(selections[count] - 1, TRUE);
      }
    }
}

void wxListBox::InsertItems(int n_items, char **items, int pos)
{
    pos = pos < num_choices ? pos : num_choices;
    int     i, j;
    char    **new_choices     = new char *[num_choices+n_items];
    char    **new_client_data = new char *[num_choices+n_items];

    for (i = 0; i < pos; ++i) {			// copy choices previous to pos
	new_choices[i] = choices[i];
	new_client_data[i] = client_data[i];
    }
    for (j = 0; j < n_items; ++i, ++j) {		    // copy new choices
	new_choices[i] = items[j];
	new_client_data[i] = NULL;
    }
    for (j = pos; j < num_choices; ++i, ++j) { 	       // copy left old choices
	new_choices[i] = choices[j];
	new_client_data[i] = client_data[j];
    }
    num_choices+=n_items;
    // delete old arrays
    delete choices;      choices = new_choices;
    delete client_data;  client_data = new_client_data;

    SetInternalData();
}

void wxListBox::Set(int n, char *_choices[])
{
    // clear ListBox
    Clear();
    // copy choices and initialize client_data
    num_choices = n;
    num_free = LIST_CHUNK_SIZE;
    choices = new char*[n+num_free];
    client_data = new char*[n+num_free];
    for (int i=0; i<n; ++i) {
	choices[i] = copystring(_choices[i]);
	client_data[i] = NULL;
    }
    SetInternalData();
}

void wxListBox::Set(wxStringList *slist)
{
    int	n = slist->Number();
    int i = 0;

    // clear ListBox
    Clear();
    // copy choices and initialize client_data
    num_choices = n;
    num_free = LIST_CHUNK_SIZE;
    choices = new char*[n+num_free];
    client_data = new char*[n+num_free];
    for (wxNode *node = slist->First(); node; node = node->Next()) {
	choices[i] = copystring((char*)node->Data());
	client_data[i] = NULL;
	++i;
    }
    SetInternalData();
}

void wxListBox::SetInternalData(void)
{
    int ww, hh;

    GetSize(&ww, &hh);
    XfwfMultiListSetNewData(
	MULTILIST, num_choices ? choices : (String*)NULL, num_choices,
	ww, TRUE, (Boolean*)NULL);
   
    /* MATTHEW: Make sure current scroll pos is legal. */
    Position pos;
    XtVaGetValues(X->handle, XtNy, &pos, NULL);
    Scroll(0, pos);
}

void wxListBox::SetFirstItem(int n)
{
    Dimension row_height;
    XtVaGetValues(X->handle, XtNrowHeight, &row_height, NULL);
    Scroll(0, n * row_height);
}

int wxListBox::GetFirstItem()
{
  Dimension row_height;
  Position y;
  XtVaGetValues(X->handle, XtNrowHeight, &row_height, XtNy, &y, NULL);

  y = -y;
  if (y % row_height)
    return (y / row_height) + 1;
  else
    return (y / row_height);
}

void wxListBox::SetFirstItem(char *s)
{
    int n;
    if ((n = FindString(s)) > -1) {
	SetFirstItem(n);
    }
}

int wxListBox::NumberOfVisibleItems()
{
  Dimension row_height;
  XtVaGetValues(X->handle, XtNrowHeight, &row_height, NULL);

  int cw, ch;
  GetClientSize(&cw, &ch);
  
  ch = ch / row_height;

  return max(1, ch);
}

//-----------------------------------------------------------------------------
// change state of wxListBox
//-----------------------------------------------------------------------------

void wxListBox::Deselect(int n)
{
    XfwfMultiListUnhighlightItem(MULTILIST, n);
}

int wxListBox::FindString(char *s)
{
    for (int i=0; i<num_choices; ++i)
	if (!strcmp(s, choices[i]))
	    return i;
    return -1;
}

char *wxListBox::GetClientData(int n)
{
    if (0 <= n && n < num_choices)
	return client_data[n];
    return NULL;
}

int wxListBox::GetSelection(void)
{
    XfwfMultiListReturnStruct *rs
	= XfwfMultiListGetHighlighted(MULTILIST);
    if (rs->num_selected >= 1)
	return rs->selected_items[0];
    return -1;
}

static int int_le(const void *a, const void *b)
{
  return (*(int *)a - *(int *)b);
}

int wxListBox::GetSelections(int **list_selections)
{
    XfwfMultiListReturnStruct *rs
	= XfwfMultiListGetHighlighted(MULTILIST);

    int *selections = new int[rs->num_selected], i;
    for (i = 0; i < rs->num_selected; i++)
      selections[i] = rs->selected_items[i];
    
    qsort(selections, rs->num_selected, sizeof(int), int_le);

    *list_selections = selections;

    return (rs->num_selected);
}

char *wxListBox::GetString(int n)
{
    if (0 <= n && n < num_choices)
	return choices[n];
    return NULL;
}

char *wxListBox::GetStringSelection(void)
{
    int n;
    if ((n = GetSelection()) > -1)
	return choices[n];
    return NULL;
}

int wxListBox::Number(void)
{
    return num_choices;
}

Bool wxListBox::Selected(int n)
{
    if (0 <= n && n < num_choices)
	return XfwfMultiListIsHighlighted(MULTILIST, n);
    return FALSE;
}

void wxListBox::SetClientData(int n, char *_client_data)
{
    if (0 <= n && n < num_choices)
	client_data[n] = _client_data;
}

void wxListBox::SetSelection(int n, Bool select)
{
  if (0 <= n && n < num_choices)
    if (select)
      XfwfMultiListHighlightItem(MULTILIST, n);
    else
      XfwfMultiListUnhighlightItem(MULTILIST, n);
}

void wxListBox::SetOneSelection(int n)
{
  if (0 <= n && n < num_choices) {
    if (style & (wxMULTIPLE | wxEXTENDED))
      XfwfMultiListUnhighlightAll(MULTILIST);
    XfwfMultiListHighlightItem(MULTILIST, n);
  }
}

Bool wxListBox::SetStringSelection(char *s)
{
    int n;
    if ((n = FindString(s)) > -1) {
	SetOneSelection(n);
	return TRUE;
    }
    return FALSE;
}

void wxListBox::SetString(int n, char *s)
{
  if (0 <= n && n < num_choices) {
    choices[n] = copystring(s);
    SetInternalData();    
  }
}

void wxListBox::Command(wxCommandEvent *event)
{
  ProcessCommand (event);
}

//-----------------------------------------------------------------------------
// callback for xfwfMultiListWidgetClass
//-----------------------------------------------------------------------------

void wxListBox::EventCallback(Widget WXUNUSED(w),
			     XtPointer dclient, XtPointer dcall)
{
    wxListBox                 *lbox   = (wxListBox*)dclient;
    XfwfMultiListReturnStruct *rs     = (XfwfMultiListReturnStruct*)dcall;
    wxCommandEvent            *event;

    event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);

    if (rs->action == XfwfMultiListActionDClick 
	&& lbox->allow_dclicks)
      event->eventType = wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND;

    lbox->ProcessCommand(event);
}

void wxListBox::OnChar(wxKeyEvent *e)
{
  int delta = 0;

  switch (e->keyCode) {
  case WXK_UP:
    delta = -1;
    break;
  case WXK_DOWN:
    delta = 1;
    break;
  }

  if (delta && num_choices) {
    int *sels;
    int n = GetSelections(&sels);
    if (n <= 1) {
      int s;
      if (n == 1)
	s = sels[0];
      else if (delta < 0)
	s = 2;
      else
	s = -1;

      SetSelection(s + delta);
      if (s != GetSelection()) {
	wxCommandEvent *event;
	event = new wxCommandEvent(wxEVENT_TYPE_CHOICE_COMMAND);
	ProcessCommand(event);
      }
    }
  }
}
