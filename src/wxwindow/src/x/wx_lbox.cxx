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

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/List.h>
#include <Xm/Form.h>
#endif

#ifdef wx_xview
int wxListProc (Panel_item item, char *string, Xv_opaque client_data,
		Panel_list_op op, Event * event, int row);
#endif

#ifdef wx_motif
void 
wxListBoxCallback (Widget w, XtPointer clientData,
		   XmListCallbackStruct * cbs)
{
/*
   if (cbs->reason == XmCR_EXTENDED_SELECT)
   cout << "*** Extend select\n";
   else if (cbs->reason == XmCR_SINGLE_SELECT)
   cout << "*** Single select\n";
   else if (cbs->reason == XmCR_MULTIPLE_SELECT)
   cout << "*** Multiple select\n";
   else if (cbs->reason == XmCR_BROWSE_SELECT)
   cout << "*** Browse select\n";

   if (cbs->selection_type == XmMODIFICATION)
   cout << "*** Modification\n";
   else if (cbs->selection_type == XmINITIAL)
   cout << "*** Initial\n";
   else if (cbs->selection_type == XmADDITION)
   cout << "*** Addition\n";
 */

  wxListBox *item = (wxListBox *) clientData;

  wxCommandEvent *_event  = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
  wxCommandEvent &event = *_event;

  switch (cbs->reason)
    {
    case XmCR_MULTIPLE_SELECT:
    case XmCR_BROWSE_SELECT:
      {
	event.clientData = item->GetClientData (cbs->item_position - 1);
	//event.commandString = item->GetStringSelection();
	event.commandInt = cbs->item_position - 1;
	event.commandString = item->GetString (event.commandInt);
	event.extraLong = TRUE;
	event.eventObject = item;
	item->ProcessCommand (event);
	// NO! result of GetStringSelection is wxBuffer
	//delete[] event.commandString;
	break;
      }
    case XmCR_EXTENDED_SELECT:
      {
	switch (cbs->selection_type)
	  {
	  case XmINITIAL:
	  case XmADDITION:
	  case XmMODIFICATION:
	    {
	      event.clientData = item->GetClientData (cbs->item_position - 1);
	      //event.commandString = item->GetStringSelection();
	      event.commandInt = cbs->item_position - 1;
	      event.commandString = item->GetString (event.commandInt);
	      event.extraLong = TRUE;
	      event.eventObject = item;
	      item->ProcessCommand (event);
	      break;
	    }
	  }
      }
    }
}

/* Respond by getting the
 * designated "default button" in the action area and activate it
 * as if the user had selected it.
 */
void 
wxListBoxDefaultActionProc (Widget list_w, XtPointer client_data, XmListCallbackStruct * cbs)
{
  wxListBox *lbox = (wxListBox *) client_data;
  wxPanel *panel = (wxPanel *) lbox->GetParent ();
  if (panel)
    panel->GetEventHandler()->OnDefaultAction (lbox);
}

#endif

// Listbox item
IMPLEMENT_DYNAMIC_CLASS(wxListBox, wxItem)

wxListBox::wxListBox (void)
#ifdef wx_motif
: clientDataList (wxKEY_INTEGER)
#endif
{
  selected = -1;
  selections = 0;
  multiple = wxSINGLE;
  no_items = 0;
#ifdef wx_motif
  labelWidget = NULL;
#endif
#ifdef wx_xview
#endif
}

wxListBox::wxListBox (wxPanel * panel, wxFunction func,
	   char *Title, Bool Multiple,
	   int x, int y, int width, int height,
	   int N, char **Choices, long style, char *name):
wxbListBox (panel, func, Title, Multiple, x, y, width, height, N, Choices,
	    style, name)
#ifdef wx_motif
,clientDataList (wxKEY_INTEGER)
#endif
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
#ifdef wx_motif
  canAddEventHandler = TRUE;
  windowName = copystring (name);

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
      delete[]the_label;
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
  if ((Multiple & wxALWAYS_SB) || (style & wxALWAYS_SB))
    {
      XtSetArg (args[2], XmNscrollBarDisplayPolicy, XmSTATIC);
      count = 3;
    }
  else
    count = 2;


  listWidget = XmCreateScrolledList (formWidget, "listWidget", args, count);

  if (buttonFont)
    XtVaSetValues (listWidget,
		   XmNfontList, 
		   /* MATTHEW: [4] Provide display */
		   buttonFont->GetInternalFont(XtDisplay(formWidget)),  /* MATTHEW: [5] Use form widget */
		   NULL);

  handle = (char *) listWidget;

  if (panel->label_position == wxHORIZONTAL)
    {
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
    }
  else
    {
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

  if (N > 0)
  {
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
#endif
#ifdef wx_xview
  // char *title = NULL;

  int choose_one = ((Multiple & wxMULTIPLE_MASK) == wxSINGLE);
  Panel x_panel = (Panel) panel->GetHandle ();
  Panel_item x_list;

  int label_position;
  if (panel->label_position == wxVERTICAL)
    label_position = PANEL_VERTICAL;
  else
    label_position = PANEL_HORIZONTAL;

  if (panel->new_line)
    {
      x_list = (Panel_item) xv_create (x_panel, PANEL_LIST, PANEL_LAYOUT, label_position, PANEL_NEXT_ROW, -1, NULL);
      panel->new_line = FALSE;
    }
  else
    x_list = (Panel_item) xv_create (x_panel, PANEL_LIST, PANEL_LAYOUT, label_position, NULL);

  if (Title)
    {
      actualLabel = wxStripMenuCodes(Title);

      if (style & wxFIXED_LENGTH)
	{
	  char *the_label = fillCopy (actualLabel);
	  xv_set (x_list, PANEL_LABEL_STRING, the_label, NULL);

	  int label_x = (int) xv_get (x_list, PANEL_LABEL_X);
	  int item_x = (int) xv_get (x_list, PANEL_ITEM_X);
	  xv_set (x_list, PANEL_LABEL_STRING, actualLabel,
		  PANEL_LABEL_X, label_x,
		  PANEL_ITEM_X, item_x,
		  NULL);
          delete[] the_label;
	}
      else
	xv_set (x_list, PANEL_LABEL_STRING, actualLabel, NULL);
    }

  xv_set (x_list,
	  PANEL_CHOOSE_ONE, choose_one,
	  PANEL_NOTIFY_PROC, wxListProc,
	  PANEL_CLIENT_DATA, (char *) this,
	  PANEL_ITEM_MENU, NULL,
	  NULL);
  if (x > -1 && y > -1)
    (void) xv_set (x_list, XV_X, x, XV_Y, y, NULL);

  handle = (char *) x_list;

  SetSize (x, y, width, height);

/*
   if (buttonFont)
   xv_set(x_list, XV_FONT, buttonFont->GetInternalFont(), NULL) ;
 */


  if (N > 0)
    Set (N, Choices);

#endif

  Callback (func);
  return TRUE;
}

wxListBox::~wxListBox (void)
{
  if (selections)
    delete[]selections;
}

void wxListBox::ChangeColour (void)
{
#ifdef wx_motif
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
#endif
}


void wxListBox::SetFirstItem (int N)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
#endif
}

void wxListBox::SetFirstItem (char *s)
{
  int N = FindString (s);

  if (N >= 0)
    SetFirstItem (N);
}

void wxListBox::Delete (int N)
{
#ifdef wx_motif
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

#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item) handle;

  xv_set (list_item, PANEL_LIST_DELETE, N, NULL);
#endif
  no_items--;
}

void wxListBox::Append (char *Item)
{
#ifdef wx_motif
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

#endif
#ifdef wx_xview
  char *label = Item;
  Panel_item list_item = (Panel_item) handle;

  int n = (int) xv_get (list_item, PANEL_LIST_NROWS);

  xv_set (list_item, PANEL_LIST_INSERT, n,
	  PANEL_LIST_STRING, n, label,
	  PANEL_LIST_CLIENT_DATA, n, n,
	  NULL);

#endif
  no_items++;
}

void wxListBox::Append (char *Item, char *Client_data)
{
#ifdef wx_motif
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

#endif
#ifdef wx_xview
  char *label = Item;
  Panel_item list_item = (Panel_item) handle;

  int n = (int) xv_get (list_item, PANEL_LIST_NROWS);

  xv_set (list_item, PANEL_LIST_INSERT, n,
	  PANEL_LIST_STRING, n, label,
	  PANEL_LIST_CLIENT_DATA, n, Client_data,
	  NULL);

#endif
  no_items++;
}

void wxListBox::Set (int n, char *choices[])
{
#ifdef wx_motif
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
  delete[]text;
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

#endif
#ifdef wx_xview
  Panel_item list = (Panel_item) handle;
  if (selections)
    {
      delete[]selections;
      selections = NULL;
    }

  int max1 = (int) xv_get (list, PANEL_LIST_NROWS);
  xv_set (list, PANEL_LIST_DELETE_ROWS, 0, max1, NULL);

  int i;
  for (i = 0; i < n; i++)
    {
      char *label = choices[i];
      xv_set (list, PANEL_LIST_INSERT, i,
	      PANEL_LIST_STRING, i, label,
	      PANEL_LIST_CLIENT_DATA, i, i,
	      NULL);
    }
#endif
  no_items = n;
}

int wxListBox::FindString (char *s)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Panel_item list = (Panel_item) handle;

  int max1 = (int) xv_get (list, PANEL_LIST_NROWS);

  int i = 0;
  int found = -1;
  while (found == -1 && i < max1)
    {
      char *label = (char *) xv_get (list, PANEL_LIST_STRING, i);
      if (label && strcmp (label, s) == 0)
	found = i;
      else
	i++;
    }
  return found;
#endif
}

void wxListBox::Clear (void)
{
  if (no_items <= 0)
    return;
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item) handle;
  xv_set (list_item, PANEL_LIST_DELETE_ROWS, 0, no_items, NULL);
#endif
  no_items = 0;
}

void wxListBox::SetSelection (int N, Bool select)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item) handle;
  xv_set (list_item, PANEL_LIST_SELECT, N, select, NULL);
#endif
}

Bool wxListBox::Selected (int N)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item) handle;
  return (Bool) xv_get (list_item, PANEL_LIST_SELECTED, N);
#endif
}

void wxListBox::Deselect (int N)
{
#ifdef wx_motif
  XmListDeselectPos ((Widget) handle, N + 1);
#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item) handle;
  xv_set (list_item, PANEL_LIST_SELECT, N, FALSE, NULL);
#endif
}

char *wxListBox::GetClientData (int N)
{
#ifdef wx_motif
  wxNode *node = clientDataList.Find ((long) N);
  if (node)
    return (char *) node->Data ();
  else
    return NULL;
#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item) handle;
  char *data = (char *) xv_get (list_item, PANEL_LIST_CLIENT_DATA, N);
  return data;
#endif
}

void wxListBox::SetClientData(int N, char *Client_data)
{
#ifdef wx_motif
  wxNode *node = clientDataList.Find ((long) N);
  if (node)
    node->SetData ((wxObject *)Client_data);
  else
    clientDataList.Append((long) N, (wxObject *) Client_data);
#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item)handle;

  xv_set(list_item, PANEL_LIST_CLIENT_DATA, N, Client_data, 
                    NULL);

#endif
}

// Return number of selections and an array of selected integers
// Use selections field to store data, which will be cleaned up
// by destructor if necessary.
int wxListBox::GetSelections (int **list_selections)
{
#ifdef wx_motif
  Widget listBox = (Widget) handle;
  int *posList = NULL;
  int posCnt = 0;
  Bool flag = XmListGetSelectedPos (listBox, &posList, &posCnt);
  if (flag)
    {
      if (posCnt > 0)
	{
	  if (selections)
	    delete[]selections;
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
#endif
#ifdef wx_xview
  Panel_item x_list = (Panel_item) handle;

  int i = 0;
  int j = 0;

  if (selections)
    {
      delete[]selections;
      selections = NULL;
    }

  for (j = 0; j < no_items; j++)
    if (xv_get (x_list, PANEL_LIST_SELECTED, j))
      {
	i++;
      }
  if (i > 0)
    {
      selections = new int[i];
      int k = 0;
      for (j = 0; j < no_items; j++)
	if (xv_get (x_list, PANEL_LIST_SELECTED, j))
	  {
	    selections[k] = j;
	    k++;
	  }
    }

  *list_selections = selections;
  return i;
#endif
}

// Get single selection, for single choice list items
int wxListBox::GetSelection (void)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Panel_item x_list = (Panel_item) handle;

  int i = 0;
  if (selections)
    {
      delete[]selections;
      selections = NULL;
    }

  int found = -1;
  while (found == -1 && i < no_items)
    {
      if (xv_get (x_list, PANEL_LIST_SELECTED, i))
	found = i;
      else
	i++;
    }

  return found;
#endif
}

// Find string for position
char *wxListBox::GetString (int N)
{
#ifdef wx_motif
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

#endif
#ifdef wx_xview
  /* MATTHEW: [6] Safety */
  if (N >= 0 && N < no_items) {
    Panel_item x_list = (Panel_item) handle;
    return (char *) xv_get (x_list, PANEL_LIST_STRING, N);
  } else
    return NULL;
#endif
}

void wxListBox::SetSize (int x, int y, int width, int height, int sizeFlags)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  // Unfortunately, XView doesn't allow us to
  // set the height in pixels explicitly. Bummer!
//  wxItem::SetSize(x, y, width, height);
//  return;

  // Fudge factor for slider width
  int listBoxSliderWidth = 15;

  Xv_opaque x_win = (Xv_opaque) handle;
  int labelWidth = 0;
  int panelLayout = (int)xv_get(x_win, PANEL_LAYOUT);
  if (panelLayout == PANEL_HORIZONTAL)
    labelWidth = (int)xv_get(x_win, PANEL_LABEL_WIDTH);

  Panel_item list_item = (Panel_item) handle;

  int row_height = (int) xv_get (list_item, PANEL_LIST_ROW_HEIGHT);

  if (x > -1)
    xv_set (list_item, XV_X, x, NULL);

  if (y > -1)
    xv_set (list_item, XV_Y, y, NULL);

  // If we're prepared to use the existing size, then...
  if (width == -1 && height == -1 && ((sizeFlags & wxSIZE_AUTO) != wxSIZE_AUTO))
    return;

  int ww, hh;
  GetSize (&ww, &hh);

  int actualWidth = width;
  int actualHeight = height;

  if (width == -1)
    actualWidth = ww;
  if (height == -1)
    actualHeight = hh;

  // Not quite sure whether this is needed: is PANEL_LIST_WIDTH
  // the whole thing incl. label or just the list?
  if (panelLayout == PANEL_HORIZONTAL)
    actualWidth = width - labelWidth;

  if (width > -1)
    xv_set (list_item,
	    PANEL_LIST_WIDTH, max ((actualWidth - listBoxSliderWidth), 60), NULL);
  else
    xv_set (list_item,
	    PANEL_LIST_WIDTH, 0, NULL);

  Xv_Font theLabelFont = (Xv_Font) xv_get (list_item, PANEL_LABEL_FONT);
  int labelHeight =  (int) xv_get (theLabelFont, FONT_DEFAULT_CHAR_HEIGHT);

  // Subtract the label height if we're in vertical label mode.
  if (panelLayout == PANEL_VERTICAL)
    actualHeight -= labelHeight;

  if (height > -1)
  {
    int noRows = max ((int) (actualHeight / row_height), 4);
//    wxDebugMsg("actualHeight = %d, row_height = %d, labelHeight = %d, height = %d, noRows = %d\n",
//      actual_height, row_height, labelHeight, height, noRows);
    xv_set (list_item,
	PANEL_LIST_DISPLAY_ROWS, noRows,
	    NULL);
  }
  else
    xv_set (list_item,
	    PANEL_LIST_DISPLAY_ROWS, 4,
	    NULL);
  GetEventHandler()->OnSize (width, height);

  // Debugging
/*
  Rect *rect = (Rect *) xv_get (list_item, XV_RECT);

  int hh = rect->r_height;
  int ww = rect->r_width;
  wxDebugMsg("Size after setting = %d, %d\n", ww, hh);
*/
#endif
}

#ifdef wx_xview
#define DOUBLECLICK_DELAY 700

int 
wxListProc (Panel_item item, char *string, Xv_opaque client_data,
	    Panel_list_op op, Event * x_event, int row)
{
  static wxListBox *doubleClickLB = NULL;
  static int doubleClickFlag = 0;
  static long doubleClickTime = 0;
  static int doubleClickPos = -1;

  if (op == PANEL_LIST_OP_SELECT || op == PANEL_LIST_OP_DESELECT)
//  if (op == PANEL_LIST_OP_SELECT)
    {
      wxListBox *list = (wxListBox *) xv_get (item, PANEL_CLIENT_DATA);
      if ((op == PANEL_LIST_OP_SELECT) || (list->multiple != wxSINGLE))
	{
          doubleClickFlag ++;

          // Check if it's a double click.
          if (doubleClickFlag == 1)
          {
            doubleClickTime = wxGetElapsedTime(FALSE);
            doubleClickLB = list;
            doubleClickPos = row;
            // Now carry on as if nothing had happened: gets a single click.
          }
          else if (doubleClickFlag == 2 && doubleClickLB == list && doubleClickPos == row)
          {
            if ((wxGetElapsedTime(FALSE) - doubleClickTime) <= DOUBLECLICK_DELAY)
            {
              doubleClickFlag = 0;
              doubleClickLB = NULL;
              doubleClickTime = 0;
              doubleClickPos = -1;

              // Got a double click.
              wxPanel *parent = (wxPanel *)list->GetParent();
              parent->GetEventHandler()->OnDefaultAction(list);
              return XV_OK;
            }
            else
            {
              // Timed out. Normal single click.
              doubleClickFlag = 0;
              doubleClickLB = NULL;
              doubleClickTime = 0;
              doubleClickPos = -1;
            }
          }
          else
          {
            doubleClickFlag = 0;
            doubleClickLB = NULL;
            doubleClickTime = 0;
            doubleClickPos = -1;
          }

	  wxCommandEvent *_event  = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
	  wxCommandEvent &event = *_event;

	  event.commandString = string;
	  event.commandInt = row;
	  event.extraLong = (op == PANEL_LIST_OP_SELECT);
	  event.clientData = (char *) client_data;
	  event.eventHandle = (char *) x_event;
	  event.eventObject = list;
	  list->ProcessCommand (event);
	}
    }
  return XV_OK;
}
#endif

void
wxListBox::InsertItems(int nItems, char **Items, int pos)
{
#ifdef wx_motif
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

  delete[] text;

 
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

#endif
#ifdef wx_xview
  char *label;
  Panel_item list_item = (Panel_item)handle;

  int i;
  for (i = 0; i < nItems; i++) {
	label = Items[i];
  	xv_set(list_item, PANEL_LIST_INSERT, pos + i,
                    PANEL_LIST_STRING, pos + i, label,
                    PANEL_LIST_CLIENT_DATA, pos + i, pos + i,
                    NULL);
  }

#endif
  no_items += nItems;
}

// Under construction
void wxListBox::SetString(int N, char *s)
{
#ifdef wx_motif
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
#endif
#ifdef wx_xview
  Panel_item list_item = (Panel_item)handle;

  // Is this right?
  xv_set (list_item,
	      PANEL_LIST_STRING, N, s,
	      NULL);
#endif
}
