/*								-*- C++ -*-
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

// don't allocate or free for every append or del
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
    wxWindow_Xintern *ph;
    Widget wgt;
    Bool vert;
    long labelw = 0, labelh = 0;

    ChainToPanel(panel, style | ((long)multiple), name);
    
    vert = (panel->GetLabelPosition() == wxVERTICAL);

    title = wxGetCtlLabel(title);

    ph = parent->GetHandle();

    // create frame
    wgt = XtVaCreateManagedWidget
	(name, xfwfTraversingEnforcerWidgetClass, ph->handle,
	 XtNlabel,       title,
	 XtNalignment,   vert ? XfwfTop : XfwfTopLeft,
	 XtNbackground,  bg->GetPixel(cmap),
	 XtNforeground,  label_fg->GetPixel(cmap),
	 XtNfont,        label_font->GetInternalFont(),
	 NULL);
    X->frame = wgt;
    // create viewport
    wgt = XtVaCreateManagedWidget
	("viewport", xfwfScrolledWindowWidgetClass, X->frame,
	 XtNhideHScrollbar, TRUE,
	 XtNbackground, bg->GetPixel(cmap), /* MATTHEW */
	 XtNdoScroll, FALSE,
	 XtNhighlightThickness, 2,
	 NULL);
    X->scroll = wgt;
    // create multi list
    wgt = XtVaCreateManagedWidget
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
    X->handle = wgt;

    XtVaSetValues(X->scroll, XtNautoAdjustScrollbars, 0, NULL);
    misc_flags |= 8; /* Indicates no auto-scroll. */

    Set(n, choices);
    // callback
    callback = func;
    XtAddCallback(X->handle, XtNcallback,
		  wxListBox::EventCallback,  (XtPointer)saferef);

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
}

void wxListBox::SetSize(int x, int y, int width, int height, int flags)
{
    wxItem::SetSize(x, y, width, height, flags);
    OnListSize(width, height);
}

//-----------------------------------------------------------------------------
// change contents of wxListBox
//-----------------------------------------------------------------------------

void wxListBox::Append(char *item)
{
  int i, count, *selections;

  count = GetSelections(&selections);

  if (num_free == 0) {
    char **new_choices, **new_client_data;

    num_free = LIST_CHUNK_SIZE;
    new_choices     = new char *[num_choices+LIST_CHUNK_SIZE];
    new_client_data = new char *[num_choices+LIST_CHUNK_SIZE];
    // copy current choices
    for (i=0; i<num_choices; ++i) {
      new_choices[i] = choices[i];
      new_client_data[i] = client_data[i];
    }
    choices = new_choices;
    client_data = new_client_data;
  }
  // set new item
  {
    char *s;
    s = copystring(item);
    choices[num_choices]     = s;
  }
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
    if (choices)
      choices = NULL;
    if (client_data)
      client_data = NULL;
    num_choices = num_free = 0;
    SetInternalData();
}

void wxListBox::Delete(int n)
{
    if (0 <= n && n < num_choices) {
      int i, count, *selections;
      
      count = GetSelections(&selections);


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
    int     i, j;
    char **new_choices, **new_client_data;

    pos = pos < num_choices ? pos : num_choices;

    new_choices     = new char *[num_choices+n_items];
    new_client_data = new char *[num_choices+n_items];

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
    choices = new_choices;
    client_data = new_client_data;

    SetInternalData();
}

void wxListBox::Set(int n, char *_choices[])
{
  int i;
  char **sa;

  // clear ListBox
  Clear();

  // copy choices and initialize client_data
  num_choices = n;
  num_free = LIST_CHUNK_SIZE;
  sa = new char*[n+num_free];
  choices = sa;
  sa = new char*[n+num_free];
  client_data = sa;
  for (i = 0; i < n; i++) {
    char *s;
    s = copystring(_choices[i]);
    choices[i] = s;
    client_data[i] = NULL;
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

    OnListSize(0, 0);
}

void wxListBox::OnScroll(wxScrollEvent* event)
{
  int p;

  wxItem::OnScroll(event);

  p = GetScrollPos(wxVERTICAL);
  XtVaSetValues(X->handle, XtNoffset, p, NULL);
}

void wxListBox::OnSize(int width, int height)
{
  OnListSize(width, height);
  wxItem::OnSize(width, height);
}

void wxListBox::OnListSize(int, int)
{
  int v, s;
  v = NumberOfVisibleItems();
  s = num_choices - v;
  if (s < 0)
    s = 0;
  SetScrollRange(wxVERTICAL, s);
  if (!v)
    v = 1;
  SetScrollPage(wxVERTICAL, v);
}

void wxListBox::SetFirstItem(int n)
{
  SetScrollPos(wxVERTICAL, n);
    
  n = GetScrollPos(wxVERTICAL);
  XtVaSetValues(X->handle, XtNoffset, n, NULL);
}

int wxListBox::GetFirstItem()
{
  return GetScrollPos(wxVERTICAL);
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
  int cw, ch;

  XtVaGetValues(X->handle, XtNrowHeight, &row_height, NULL);

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
  for (int i=0; i<num_choices; ++i) {
    if (!strcmp(s, choices[i]))
      return i;
  }
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
  XfwfMultiListReturnStruct *rs;
  rs = XfwfMultiListGetHighlighted(MULTILIST);
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
    XfwfMultiListReturnStruct *rs;
    int *selections, i;

    rs = XfwfMultiListGetHighlighted(MULTILIST);

    selections = new int[rs->num_selected];
    for (i = 0; i < rs->num_selected; i++) {
      selections[i] = rs->selected_items[i];
    }
    
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
    s = copystring(s);
    choices[n] = s;
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
    wxListBox                 *lbox   = (wxListBox *)GET_SAFEREF(dclient);
    XfwfMultiListReturnStruct *rs     = (XfwfMultiListReturnStruct*)dcall;
    wxCommandEvent            *event;

    event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);

    if (rs->action == XfwfMultiListActionDClick 
	&& lbox->allow_dclicks)
      event->eventType = wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND;

    lbox->ProcessCommand(event);

#ifdef MZ_PRECISE_GC
    XFORM_RESET_VAR_STACK;
#endif
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
    int n;
    n = GetSelections(&sels);
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
	int first, count;

	// Is is visible?
	first = GetFirstItem();
	count = NumberOfVisibleItems() - 1;
	s = GetSelection();
	if (s < first)
	  SetFirstItem(s);
	else if (s > first + count) {
	  SetFirstItem(s - count);
	}

	event = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
	ProcessCommand(event);
      }
    }
  }
}
