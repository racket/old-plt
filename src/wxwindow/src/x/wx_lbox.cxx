/*
 * File:        wx_lbox.cc
 * Purpose:     List box implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_lbox.h"
#endif

#include <stdlib.h>
#include "common.h"

#include "wx_utils.h"
#include "wx_timer.h"
#include "wx_privt.h"
#include "wx_lbox.h"

#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/Form.h>

void 
wxListBoxCallback (Widget, XtPointer clientData, XmListCallbackStruct *cbs)
{
  wxListBox *item = (wxListBox *) clientData;

  wxCommandEvent *event  = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
  item->ProcessCommand (event);
}

/* Respond by getting the
 * designated "default button" in the action area and activate it
 * as if the user had selected it.
 */
void 
wxListBoxDefaultActionProc (Widget, XtPointer client_data, XmListCallbackStruct *)
{
  wxCommandEvent *event  = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND);
  item->ProcessCommand (event);
}

// Listbox item
IMPLEMENT_DYNAMIC_CLASS(wxListBox, wxItem)

wxListBox::wxListBox(void) : clientDataList (wxKEY_INTEGER)
{
  selected = -1;
  selections = 0;
  multiple = wxSINGLE;
  no_items = 0;
  labelWidget = NULL;
}

wxListBox::wxListBox (wxPanel * panel, wxFunction func,
	   char *Title, Bool Multiple,
	   int x, int y, int width, int height,
	   int N, char **Choices, long style, char *name):
	   wxbListBox (panel, func, Title, Multiple, x, y, width, height, N, Choices,
		       style, name) ,clientDataList (wxKEY_INTEGER)
{
  Create (panel, func, Title, Multiple, x, y, width, height, N, Choices,
	  style, name);
}

Bool wxListBox::
Create (wxPanel * panel, wxFunction func,
	char *Title, Bool Multiple,
	int x, int y, int width, int height,
	int N, char **Choices, long style, char *name)
{
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  selected = -1;
  selections = 0;
  multiple = Multiple & wxMULTIPLE_MASK;
  window_parent = panel;
  no_items = 0;
  labelPosition = panel->label_position;
  windowStyle = style;
  canAddEventHandler = TRUE;
  windowName = copystring (name);

  panel->GrowReady();

  Widget panelForm = panel->panelWidget;
  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
                                        XmNresizePolicy, XmRESIZE_NONE,
					NULL);

  if (Title)
    {
      char buf[400];
      wxStripMenuCodes(Title, buf);

      char *the_label = (style & wxFIXED_LENGTH) ? fillCopy (buf) : copystring (buf);
      XmString text = XmStringCreateSimple (the_label);
      labelWidget = XtVaCreateManagedWidget (buf,
#if USE_GADGETS
					     style & wxCOLOURED ?
				    xmLabelWidgetClass : xmLabelGadgetClass,
					     formWidget,
#else
					     xmLabelWidgetClass, formWidget,
#endif
					     XmNlabelString, text,
					     NULL);
      if (labelFont)
	XtVaSetValues (labelWidget,
		       XmNfontList, 
		       /* MATTHEW: [4] Provide display */
		       labelFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] Use form widget */
		       NULL);

      XmStringFree (text);
    }

  Widget listWidget;
  Arg args[3];
  int count;
  XtSetArg (args[0], XmNlistSizePolicy, XmCONSTANT);
  //  XtSetArg(args[0], XmNlistSizePolicy, XmRESIZE_IF_POSSIBLE);
  //  XtSetArg(args[0], XmNlistSizePolicy, XmVARIABLE);
  if (multiple == wxEXTENDED)
    XtSetArg (args[1], XmNselectionPolicy, XmMULTIPLE_SELECT);
  else if (multiple == wxMULTIPLE)
    XtSetArg (args[1], XmNselectionPolicy, XmEXTENDED_SELECT);
  else
    XtSetArg (args[1], XmNselectionPolicy, XmBROWSE_SELECT);
  //  XtSetArg(args[2], XmNscrollBarDisplayPolicy, XmSTATIC);
  if ((Multiple & wxALWAYS_SB) || (style & wxALWAYS_SB)) {
    XtSetArg (args[2], XmNscrollBarDisplayPolicy, XmSTATIC);
    count = 3;
  } else
    count = 2;

  listWidget = XmCreateScrolledList (formWidget, "listWidget", args, count);

  if (buttonFont)
    XtVaSetValues (listWidget,
		   XmNfontList, 
		   /* MATTHEW: [4] Provide display */
		   buttonFont->GetInternalFont(XtDisplay(formWidget)),  /* MATTHEW: [5] Use form widget */
		   NULL);

  handle = (char *) listWidget;

  if (panel->label_position == wxHORIZONTAL) {
    if (labelWidget)
      XtVaSetValues (labelWidget,
		     XmNtopAttachment, XmATTACH_FORM,
		     XmNleftAttachment, XmATTACH_FORM,
		     XmNalignment, XmALIGNMENT_BEGINNING,
		     NULL);
    XtVaSetValues (XtParent (listWidget),
		   XmNleftOffset, 4,
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNleftAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		   XmNleftWidget, labelWidget ? labelWidget : formWidget,
		   XmNrightAttachment, XmATTACH_FORM,
		   NULL);
  } else {
    if (labelWidget)
      XtVaSetValues (labelWidget,
		     XmNtopAttachment, XmATTACH_FORM,
		     XmNleftAttachment, XmATTACH_FORM,
		     XmNalignment, XmALIGNMENT_BEGINNING,
		     NULL);
    
    XtVaSetValues (XtParent (listWidget),
		   XmNtopAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		   XmNtopWidget, labelWidget ? labelWidget : formWidget,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   NULL);
  }
  
  XtManageChild (listWidget);

  if (width == -1)
    width = 150;
  if (height == -1)
    height = 80;

  XtAddCallback (listWidget, XmNbrowseSelectionCallback, (XtCallbackProc) wxListBoxCallback,
		 (XtPointer) this);
  XtAddCallback (listWidget, XmNextendedSelectionCallback, (XtCallbackProc) wxListBoxCallback,
		 (XtPointer) this);
  XtAddCallback (listWidget, XmNmultipleSelectionCallback, (XtCallbackProc) wxListBoxCallback,
		 (XtPointer) this);

  XtAddCallback (listWidget, XmNdefaultActionCallback, (XtCallbackProc) wxListBoxDefaultActionProc,
		 (XtPointer) this);

  panel->AttachWidget (this, formWidget, x, y, width, height);
  ChangeColour ();

  if (N > 0) {
    int i;
    for (i = 0; i < N; i++)
      Append (Choices[i]);
  }

  /* After creating widgets, no more resizes. */
  if (style & wxFIXED_LENGTH)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_SELF,
		       XmNleftAttachment, XmATTACH_SELF,
		       NULL);

      XtVaSetValues (XtParent (listWidget),
		     XmNtopAttachment, XmATTACH_SELF,
		     XmNbottomAttachment, XmATTACH_SELF,
		     XmNleftAttachment, XmATTACH_SELF,
		     XmNrightAttachment, XmATTACH_SELF,
		     NULL);
      if (labelWidget)
	{
	  XmString text = XmStringCreateSimple (Title);
	  XtVaSetValues (labelWidget,
			 XmNlabelString, text,
			 NULL);
	  XmStringFree (text);
	}
    }
    XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);

  Callback (func);

  wxWidgetHashTable->Put((long)listWidget, this);
  AddPreHandlers(listWidget);

  return TRUE;
}

wxListBox::~wxListBox (void)
{
  wxWidgetHashTable->Delete((long)handle);
}

void wxListBox::ChangeColour (void)
{
  int change;

  wxPanel *panel = (wxPanel *) window_parent;
  /* MATTHEW: [4] Provide display */
  change = wxComputeColors (XtDisplay(formWidget),
			    panel->backColour,
			    panel->buttonColour);
  if (change == wxBACK_COLORS)
    XtVaSetValues (formWidget,
		   XmNbackground, itemColors[wxBACK_INDEX].pixel,
		   XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		   XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		   XmNforeground, itemColors[wxFORE_INDEX].pixel,
		   NULL);
  else if (change == wxFORE_COLORS)
    XtVaSetValues (formWidget,
		   XmNforeground, itemColors[wxFORE_INDEX].pixel,
		   NULL);

  /* MATTHEW: [4] Provide display */
  change = wxComputeColors (XtDisplay(formWidget),
			    backColour, buttonColour);
  if (change == wxBACK_COLORS)
    {
      XtVaSetValues ((Widget) handle,
		     XmNbackground, itemColors[wxBACK_INDEX].pixel,
		     XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		     XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		     XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		     XmNforeground, itemColors[wxFORE_INDEX].pixel,
		     NULL);
      Widget parent = XtParent ((Widget) handle);
      XtVaSetValues (parent,
		     XmNbackground, itemColors[wxBACK_INDEX].pixel,
		     XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		     XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		     XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		     XmNforeground, itemColors[wxFORE_INDEX].pixel,
		     NULL);
      Widget hsb, vsb;
      XtVaGetValues (parent,
		     XmNhorizontalScrollBar, &hsb,
		     XmNverticalScrollBar, &vsb,
		     NULL);
      if (hsb)
	XtVaSetValues (hsb,
		       XmNbackground, itemColors[wxBACK_INDEX].pixel,
		       XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		       XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		       XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
      if (vsb)
	XtVaSetValues (vsb,
		       XmNbackground, itemColors[wxBACK_INDEX].pixel,
		       XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		       XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		       XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
    }
  else if (change == wxFORE_COLORS)
    {
      XtVaSetValues ((Widget) handle,
		     XmNforeground, itemColors[wxFORE_INDEX].pixel,
		     NULL);
      Widget parent = XtParent ((Widget) handle);
      XtVaSetValues (parent,
		     XmNforeground, itemColors[wxFORE_INDEX].pixel,
		     NULL);
      Widget hsb, vsb;
      XtVaGetValues (parent,
		     XmNhorizontalScrollBar, &hsb,
		     XmNverticalScrollBar, &vsb,
		     NULL);
      if (hsb)
	XtVaSetValues (hsb,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
      if (vsb)
	XtVaSetValues (vsb,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
    }

  if (labelWidget)
    {
      /* MATTHEW: [4] Provide display */
      change = wxComputeColors (XtDisplay(formWidget),
				panel->backColour, labelColour);

      if (change == wxBACK_COLORS)
	XtVaSetValues (labelWidget,
		       XmNbackground, itemColors[wxBACK_INDEX].pixel,
		       XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		       XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
      else if (change == wxFORE_COLORS)
	XtVaSetValues (labelWidget,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
    }
}


void wxListBox::SetFirstItem (int N)
{
  int count, length;

  if (N < 0)
    return;
  XtVaGetValues ((Widget) handle,
		 XmNvisibleItemCount, &count,
		 XmNitemCount, &length,
		 NULL);
  if ((N + count) >= length)
    N = length - count;
  XmListSetPos ((Widget) handle, N + 1);
}

void wxListBox::SetFirstItem (char *s)
{
  int N = FindString (s);

  if (N >= 0)
    SetFirstItem (N);
}

int wxListBox::GetFirstItem(void)
{
  int count = 1;

  XtVaGetValues((Widget)handle,
		XmNtopItemPosition, &count,
		NULL);
  
  if (count <= 0)
    return 0;
  
  return count - 1;
}


int wxListBox::NumberOfVisibleItems(void)
{
  int count = 1;

  XtVaGetValues((Widget)handle,
		XmNvisibleItemCount, &count,
		NULL);

  if (count <= 0)
    count = 1;

  return count;
}

void wxListBox::Delete (int N)
{
  int width1, height1;
  int width2, height2;
  Widget listBox = (Widget) handle;
  GetSize (&width1, &height1);

  XtUnmanageChild (formWidget);

  XmListDeletePos (listBox, N + 1);

  XtManageChild (formWidget);

  GetSize (&width2, &height2);
  // Correct for randomly resized listbox - bad boy, Motif!
  if (width1 != width2 || height1 != height2)
    SetSize (-1, -1, width1, height1);

  // (JDH) need to add code here to take care of clientDataList
  wxNode *node = clientDataList.Find((long)N);  // get item from list
  if (node) clientDataList.DeleteNode(node);    // if existed then delete from list
  node = clientDataList.First();                // we now have to adjust all keys that 
  while (node)                                  // are >=N+1
   { if (node->key.integer >= (long)(N+1))      // very ugly C++ wise but no other way 
       node->key.integer--;                     // to look at or change key value
     node = node->Next();
   }

  no_items--;
}

void wxListBox::Append (char *Item)
{
  int width1, height1;
  int width2, height2;

  Widget listBox = (Widget) handle;
  GetSize (&width1, &height1);

  XtUnmanageChild (formWidget);
  int n;
  XtVaGetValues (listBox, XmNitemCount, &n, NULL);
  XmString text = XmStringCreateSimple (Item);
//  XmListAddItem(listBox, text, n + 1);
  XmListAddItemUnselected (listBox, text, 0);
  XmStringFree (text);

  // It seems that if the list is cleared, we must re-ask for
  // selection policy!!
  Arg args[3];
  XtSetArg (args[0], XmNlistSizePolicy, XmCONSTANT);
  if (multiple == wxEXTENDED)
    XtSetArg (args[1], XmNselectionPolicy, XmMULTIPLE_SELECT);
  else if (multiple == wxMULTIPLE)
    XtSetArg (args[1], XmNselectionPolicy, XmEXTENDED_SELECT);
  else
    XtSetArg (args[1], XmNselectionPolicy, XmBROWSE_SELECT);
  XtSetValues (listBox, args, 2);

  XtManageChild (formWidget);

  GetSize (&width2, &height2);
  // Correct for randomly resized listbox - bad boy, Motif!
  if (width1 != width2 || height1 != height2)
    SetSize (-1, -1, width1, height1);

  no_items++;
}

void wxListBox::Append (char *Item, char *Client_data)
{
  int width1, height1;
  int width2, height2;

  Widget listBox = (Widget) handle;

  GetSize (&width1, &height1);
  XtUnmanageChild (formWidget);

  int n;
  XtVaGetValues (listBox, XmNitemCount, &n, NULL);
  XmString text = XmStringCreateSimple (Item);
//  XmListAddItem(listBox, text, n + 1);
  XmListAddItemUnselected (listBox, text, 0);
  XmStringFree (text);

  // It seems that if the list is cleared, we must re-ask for
  // selection policy!!
  Arg args[3];
  XtSetArg (args[0], XmNlistSizePolicy, XmCONSTANT);
  if (multiple == wxEXTENDED)
    XtSetArg (args[1], XmNselectionPolicy, XmMULTIPLE_SELECT);
  else if (multiple == wxMULTIPLE)
    XtSetArg (args[1], XmNselectionPolicy, XmEXTENDED_SELECT);
  else
    XtSetArg (args[1], XmNselectionPolicy, XmBROWSE_SELECT);
  XtSetValues (listBox, args, 2);

  clientDataList.Append ((long) n, (wxObject *) Client_data);

  XtManageChild (formWidget);
  GetSize (&width2, &height2);

  // Correct for randomly resized listbox - bad boy, Motif!
  if (width1 != width2 || height1 != height2)
    SetSize (-1, -1, width1, height1);

  no_items++;
}

void wxListBox::Set (int n, char *choices[])
{
  //for (int i = 0; i < n; i++)
  //  Append(choices[i]);
  //
  // To avoid flickering, we do only one Manage/Unmanage.

  int width1, height1;
  int width2, height2;

  Widget listBox = (Widget) handle;
  GetSize (&width1, &height1);

  XtUnmanageChild (formWidget);
/***
  for (int i=0; i<n; i++)
  {
    XmString text = XmStringCreateSimple(choices[i]);
    XmListAddItemUnselected(listBox, text, 0);
    XmStringFree(text);
  }
***/
//wxDebugMsg("Start add\n") ;
  XmString *text = new XmString[n];
  int i;
  for (i = 0; i < n; i++)
    text[i] = XmStringCreateSimple (choices[i]);
  XmListAddItems (listBox, text, n, 0);
  for (i = 0; i < n; i++)
    XmStringFree (text[i]);
//wxDebugMsg("End Add\n") ;

  // It seems that if the list is cleared, we must re-ask for
  // selection policy!!
  Arg args[3];
  XtSetArg (args[0], XmNlistSizePolicy, XmCONSTANT);
  if (multiple == wxEXTENDED)
    XtSetArg (args[1], XmNselectionPolicy, XmMULTIPLE_SELECT);
  else if (multiple == wxMULTIPLE)
    XtSetArg (args[1], XmNselectionPolicy, XmEXTENDED_SELECT);
  else
    XtSetArg (args[1], XmNselectionPolicy, XmBROWSE_SELECT);
  XtSetValues (listBox, args, 2);

  XtManageChild (formWidget);

  GetSize (&width2, &height2);
  // Correct for randomly resized listbox - bad boy, Motif!
  if (width1 != width2 || height1 != height2)
    SetSize (-1, -1, width1, height1);

  no_items = n;
}

int wxListBox::FindString (char *s)
{
  XmString str = XmStringCreateSimple (s);
  int *positions = NULL;
  int no_positions = 0;
  Bool success = XmListGetMatchPos ((Widget) handle, str, &positions, &no_positions);
  XmStringFree (str);
  if (success)
    {
      int pos = positions[0];
      if (positions)
	XtFree ((char *) positions);
      return pos - 1;
    }
  else
    return -1;
}

void wxListBox::Clear (void)
{
  if (no_items <= 0)
    return;

  int width1, height1;
  int width2, height2;

  Widget listBox = (Widget) handle;
  GetSize (&width1, &height1);

  XmListDeleteAllItems (listBox);
  clientDataList.Clear ();
  GetSize (&width2, &height2);

  // Correct for randomly resized listbox - bad boy, Motif!
  if (width1 != width2 || height1 != height2)
    SetSize (-1, -1, width1, height1);

  no_items = 0;
}

void wxListBox::SetSelection (int N, Bool select)
{
  if (select)
    {
/*
      if (multiple)
	{
	  int *selections = NULL;
	  int n = GetSelections (&selections);

	  // This hack is supposed to work, to make it possible to select more
	  // than one item, but it DOESN'T under Motif 1.1.

	  XtVaSetValues ((Widget) handle, XmNselectionPolicy, XmMULTIPLE_SELECT, NULL);

	  int i;
	  for (i = 0; i < n; i++)
	    XmListSelectPos ((Widget) handle, selections[i] + 1, FALSE);

	  XmListSelectPos ((Widget) handle, N + 1, FALSE);

	  XtVaSetValues ((Widget) handle, XmNselectionPolicy, XmEXTENDED_SELECT, NULL);
	}
      else
*/
	XmListSelectPos ((Widget) handle, N + 1, FALSE);

    }
  else
    XmListDeselectPos ((Widget) handle, N + 1);
}

Bool wxListBox::Selected (int N)
{
  // In Motif, no simple way to determine if the item is selected.
  int *theSelections;
  int count = GetSelections (&theSelections);
  if (count == 0)
    return FALSE;
  else
  {
    int j;
    for (j = 0; j < count; j++)
      if (theSelections[j] == N)
	return TRUE;
  }
  return FALSE;
}

void wxListBox::Deselect (int N)
{
  XmListDeselectPos ((Widget) handle, N + 1);
}

char *wxListBox::GetClientData (int N)
{
  wxNode *node = clientDataList.Find ((long) N);
  if (node)
    return (char *) node->Data ();
  else
    return NULL;
}

void wxListBox::SetClientData(int N, char *Client_data)
{
  wxNode *node = clientDataList.Find ((long) N);
  if (node)
    node->SetData ((wxObject *)Client_data);
  else
    clientDataList.Append((long) N, (wxObject *) Client_data);
}

// Return number of selections and an array of selected integers
// Use selections field to store data, which will be cleaned up
// by destructor if necessary.
int wxListBox::GetSelections (int **list_selections)
{
  Widget listBox = (Widget) handle;
  int *posList = NULL;
  int posCnt = 0;
  Bool flag = XmListGetSelectedPos (listBox, &posList, &posCnt);
  if (flag)
    {
      if (posCnt > 0)
	{
	  selections = new int[posCnt];
	  int i;
	  for (i = 0; i < posCnt; i++)
	    selections[i] = posList[i] - 1;

	  XtFree ((char *) posList);
	  *list_selections = selections;
	  return posCnt;
	}
      else
	return FALSE;
    }
  else
    return FALSE;
}

// Get single selection, for single choice list items
int wxListBox::GetSelection (void)
{
  Widget listBox = (Widget) handle;
  int *posList = NULL;
  int posCnt = 0;
  Bool flag = XmListGetSelectedPos (listBox, &posList, &posCnt);
  if (flag)
    {
      int id = -1;
      if (posCnt > 0)
	id = posList[0] - 1;
      XtFree ((char *) posList);
      return id;
    }
  else
    return -1;
}

// Find string for position
char *wxListBox::GetString (int N)
{
  Widget listBox = (Widget) handle;
  XmString *strlist;
  int n;
  XtVaGetValues (listBox, XmNitemCount, &n, XmNitems, &strlist, NULL);
  /* MATTHEW: [6] Check for negative */
  if (N >= 0 && N < n)
    {
      char *txt;
      if (XmStringGetLtoR (strlist[N], XmSTRING_DEFAULT_CHARSET, &txt))
	{
	  return copystring(txt);
	}
      else
	return NULL;
    }
  else
    return NULL;
}

void wxListBox::SetSize (int x, int y, int width, int height, int /* sizeFlags */)
{
  int pw, ph;
  
  GetParent()->GetSize(&pw, &ph);

  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  Bool isShow = XtIsManaged(formWidget);
  if (isShow)
    XtUnmanageChild(formWidget);

  if (x > -1)
    XtVaSetValues (formWidget, XmNleftAttachment, XmATTACH_SELF,
		   XmNx, x, NULL);
  if (y > -1)
    XtVaSetValues (formWidget, XmNtopAttachment, XmATTACH_SELF,
		   XmNy, y, NULL);

  // Must set the actual listbox to be desired size MINUS label size
  Dimension labelWidth = 0, labelHeight = 0, actualWidth = width, actualHeight = height;

  if (labelWidget)
    XtVaGetValues (labelWidget, XmNwidth, &labelWidth, XmNheight, &labelHeight, NULL);

  if (itemOrientation == wxHORIZONTAL)
    {
      actualWidth = width - labelWidth;
      actualHeight = height;
    }
  else
    {
      actualWidth = width;
      actualHeight = height - labelHeight;
    }

  if (width > -1)
    {
      int scrollWidthX = 0;
      // width - 10 is a fudge factor for taking the scroll bar into account.
      // A better way anybody?
      XtVaSetValues (formWidget, XmNwidth, width - scrollWidthX, XmNrightAttachment, XmATTACH_SELF, NULL);
    }
  if (height > -1)
    {
      XtVaSetValues (formWidget, XmNheight, height, XmNbottomAttachment, XmATTACH_SELF, NULL);
    }

  if (isShow)
    XtManageChild (formWidget);
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);

  // Check resulting size is correct
  int tempW, tempH;
  GetSize (&tempW, &tempH);

  GetParent()->SetSize(-1, -1, pw, ph, 0x100);

  sr_width = width;
  sr_height = height;
  GetEventHandler()->OnSize (width, height);
}

void
wxListBox::InsertItems(int nItems, char **Items, int /* pos */)
{
  int width1, height1;
  int width2, height2;

  Widget listBox = (Widget)handle;

  GetSize(&width1, &height1);

  XtUnmanageChild(formWidget);
  
  XmString *text = new XmString[nItems];
  int i;
  // Steve Hammes: Motif 1.1 compatibility
#if XmVersion > 1100
  for (i = 0; i < nItems; i++)
    text[i] = XmStringCreateSimple(Items[i]);
  XmListAddItemsUnselected(listBox, text, nItems, pos+1);
#else
  for (i = 0; i < nItems; i++)
  {
    text[i] = XmStringCreateSimple(Items[i]);
    XmListAddItemUnselected(listBox, text[i], i);
  }
#endif
  for (i = 0; i < nItems; i++)
	XmStringFree(text[i]);

 
 // It seems that if the list is cleared, we must re-ask for
  // selection policy!!
  Arg args[3];
  XtSetArg(args[0], XmNlistSizePolicy, XmCONSTANT);
  if (multiple==wxEXTENDED)
    XtSetArg(args[1], XmNselectionPolicy, XmMULTIPLE_SELECT);
  else if (multiple==wxMULTIPLE)
    XtSetArg(args[1], XmNselectionPolicy, XmEXTENDED_SELECT);
  else XtSetArg(args[1], XmNselectionPolicy, XmBROWSE_SELECT);
  XtSetValues(listBox,args,2) ;

  XtManageChild(formWidget);

  GetSize(&width2, &height2);
  // Correct for randomly resized listbox - bad boy, Motif!
  if (width1 != width2 /*|| height1 != height2*/)
    SetSize(-1, -1, width1, height1);

  no_items += nItems;
}

// Under construction
void wxListBox::SetString(int N, char *s)
{
  int width1, height1;
  int width2, height2;

  Widget listBox = (Widget) handle;
  GetSize (&width1, &height1);

//  XtUnmanageChild (formWidget);
  XmString text = XmStringCreateSimple (s);

  // WHAT'S THE MOTIF CALL TO SET THE TEXT OF AN EXISTING
  // ITEM???
  // There isn't one, so delete the item and add it again.
  XmListDeletePos (listBox, N+1);
  XmListAddItem (listBox, text, N+1);

  XmStringFree(text);

/*
  // It seems that if the list is cleared, we must re-ask for
  // selection policy!!
  Arg args[3];
  XtSetArg (args[0], XmNlistSizePolicy, XmCONSTANT);
  if (multiple == wxMULTIPLE)
    XtSetArg (args[1], XmNselectionPolicy, XmMULTIPLE_SELECT);
  else if (multiple == wxEXTENDED)
    XtSetArg (args[1], XmNselectionPolicy, XmEXTENDED_SELECT);
  else
    XtSetArg (args[1], XmNselectionPolicy, XmBROWSE_SELECT);
  XtSetValues (listBox, args, 2);
*/

//  XtManageChild (formWidget);

  GetSize (&width2, &height2);
  // Correct for randomly resized listbox - bad boy, Motif!
  if (width1 != width2 || height1 != height2)
    SetSize (-1, -1, width1, height1);
}
