/*
 * File:        wx_check.cc
 * Purpose:     Checkbox implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_check.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_check.h"

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/Form.h>
#endif

/* Hack: used in doubleClickAllowed */
#define IS_CHECKED 0x1

#ifdef wx_xview
void wxCheckBoxProc (Panel_item item, int value, Event * x_event);
#endif

#ifdef wx_motif
void 
wxCheckBoxCallback (Widget w, XtPointer clientData,
		    XtPointer ptr)
{
  wxCheckBox *item = (wxCheckBox *) clientData;
  int i = item->GetValue();
  if (!!i == !!(item->doubleClickAllowed & IS_CHECKED))
    return;
  if (i)
    item->doubleClickAllowed |= IS_CHECKED;
  else
    item->doubleClickAllowed -= IS_CHECKED;

  wxCommandEvent *event  = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);
  item->ProcessCommand (*event);
}
#endif


// Single check box item

IMPLEMENT_DYNAMIC_CLASS(wxCheckBox, wxItem)

wxCheckBox::wxCheckBox (void)
{
  buttonBitmap = NULL;
}

wxCheckBox::wxCheckBox (wxPanel * panel, wxFunction func, char *Title,
	    int x, int y, int width, int height, long style, char *name):
wxbCheckBox (panel, func, Title, x, y, width, height, style, name)
{
  Create (panel, func, Title, x, y, width, height, style, name);
}

wxCheckBox::wxCheckBox (wxPanel * panel, wxFunction func, wxBitmap * bitmap,
	    int x, int y, int width, int height, long style, char *name):
wxbCheckBox (panel, func, bitmap, x, y, width, height, style, name)
{
  Create (panel, func, bitmap, x, y, width, height, style, name);
}

Bool wxCheckBox::
Create (wxPanel * panel, wxFunction func, char *Title,
	int x, int y, int width, int height, long style, char *name)
{
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  buttonBitmap = NULL;
  window_parent = panel;
  labelPosition = panel->label_position;
  windowStyle = style;

#if !USE_GADGETS
   canAddEventHandler = TRUE;
#else
   canAddEventHandler = FALSE;
#endif

  windowName = copystring (name);

  Widget panelForm = panel->panelWidget;
  if (!Title)
    Title = "";
  char buf[400];
  wxStripMenuCodes(Title, buf);
  XmString text = XmStringCreateSimple (buf);

  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  Widget buttonWidget = XtVaCreateManagedWidget ("toggle",
#if USE_GADGETS
						 style & wxCOLOURED ?
						 xmToggleButtonWidgetClass : xmToggleButtonGadgetClass,
						 formWidget,
#else
						 xmToggleButtonWidgetClass, formWidget,
#endif
						 XmNlabelString, text,
						 XmNtopAttachment, XmATTACH_FORM,
						 XmNleftAttachment, XmATTACH_FORM,
						 XmNbottomAttachment, XmATTACH_FORM,
						 XmNrightAttachment, XmATTACH_FORM,
						 NULL);

  Widget evWidget;
#if USE_GADGETS
  evWidget = formWidget;
#else
  evWidget = buttonWidget;
#endif

  handle = (char *) buttonWidget;

  if (buttonFont)
    XtVaSetValues (buttonWidget,
		   XmNfontList, 
		   /* MATHEW: [4] Provide display */
		   buttonFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] use formWidget */
		   NULL);

  XmStringFree (text);

  XtAddCallback (buttonWidget, XmNvalueChangedCallback, (XtCallbackProc) wxCheckBoxCallback,
		 (XtPointer) this);

  panel->AttachWidget (this, formWidget, x, y, width, height);
  ChangeColour ();

  XmToggleButtonSetState (buttonWidget, FALSE, TRUE);
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);

  Callback (func);

  wxWidgetHashTable->Put((long)buttonWidget, this);
  AddPreHandlers(evWidget, buttonWidget);

  return TRUE;
}

Bool wxCheckBox::
Create (wxPanel * panel, wxFunction func, wxBitmap * bitmap,
	int x, int y, int width, int height, long style, char *name)
{
  if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return Create(panel, func, "<bad-image>", x, y, width, height, style, name);

  bitmap->selectedIntoDC++;

  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  buttonBitmap = bitmap;
  window_parent = panel;
  labelPosition = panel->label_position;
  windowStyle = style;
#if !USE_GADGETS
  canAddEventHandler = TRUE;
#else
  canAddEventHandler = FALSE;
#endif
  windowName = copystring (name);

  Widget panelForm = panel->panelWidget;

  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  Widget buttonWidget = XtVaCreateManagedWidget ("toggle",
#if USE_GADGETS
						 style & wxCOLOURED ?
		      xmToggleButtonWidgetClass : xmToggleButtonGadgetClass,
						 formWidget,
#else
				      xmToggleButtonWidgetClass, formWidget,
#endif
					    XmNtopAttachment, XmATTACH_FORM,
					   XmNleftAttachment, XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					  XmNrightAttachment, XmATTACH_FORM,
						 NULL);

  Widget evWidget;
#if USE_GADGETS
  evWidget = formWidget;
#else
  evWidget = buttonWidget;
#endif

  handle = (char *) buttonWidget;

  if (buttonFont)
    XtVaSetValues (buttonWidget,
		   XmNfontList,
		   /* MATHEW: [4] Provide display */
		   buttonFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] use formWidget */
		   NULL);

  XtVaSetValues (buttonWidget,
		 XmNlabelPixmap, bitmap->GetLabelPixmap (buttonWidget),
		 XmNselectPixmap, bitmap->GetLabelPixmap (buttonWidget),
		 XmNlabelInsensitivePixmap, bitmap->GetInsensPixmap (buttonWidget),
		 XmNselectInsensitivePixmap, bitmap->GetInsensPixmap (buttonWidget),
		 XmNarmPixmap, bitmap->GetArmPixmap (buttonWidget),
		 XmNlabelType, XmPIXMAP,
		 NULL);

  XtAddCallback (buttonWidget, XmNvalueChangedCallback, wxCheckBoxCallback,
		 this);

  panel->AttachWidget (this, formWidget, x, y, width, height);
  ChangeColour ();

  XmToggleButtonSetState (buttonWidget, FALSE, TRUE);
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);

  Callback (func);

  wxWidgetHashTable->Put((long)buttonWidget, this);
  AddPreHandlers(evWidget, buttonWidget);

  return TRUE;
}

wxCheckBox::~wxCheckBox (void)
{
  if (buttonBitmap)
    --buttonBitmap->selectedIntoDC;
  wxWidgetHashTable->Delete((long)handle);
}

void wxCheckBox::ChangeColour (void)
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
    XtVaSetValues ((Widget) handle,
		   XmNbackground, itemColors[wxBACK_INDEX].pixel,
		   XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		   XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		   XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		   XmNforeground, itemColors[wxFORE_INDEX].pixel,
		   NULL);
  else if (change == wxFORE_COLORS)
    XtVaSetValues ((Widget) handle,
		   XmNforeground, itemColors[wxFORE_INDEX].pixel,
		   NULL);

  if (buttonBitmap)
    XtVaSetValues ((Widget) handle,
	     XmNlabelPixmap, buttonBitmap->GetLabelPixmap ((Widget) handle),
		   XmNlabelInsensitivePixmap, buttonBitmap->GetInsensPixmap ((Widget) handle),
		 XmNarmPixmap, buttonBitmap->GetArmPixmap ((Widget) handle),
		   NULL);
#endif
}

char *wxCheckBox::GetLabel ()
{
#ifdef wx_motif
  Widget widget = (Widget) handle;
  XmString text;
  char *s;
  XtVaGetValues (widget,
		 XmNlabelString, &text,
		 NULL);

  if (XmStringGetLtoR (text, XmSTRING_DEFAULT_CHARSET, &s))
    {
//    XmStringFree(text);
      char *val = copystring (s);
      XtFree (s);
      return val;
    }
  else
    {
//    XmStringFree(text);
      return NULL;
    }
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  return ((char *) xv_get (item, PANEL_CHOICE_STRING, 0));
#endif
}

void wxCheckBox::SetLabel (char *label)
{
  if (buttonBitmap)
    return;

#ifdef wx_motif
  Widget widget = (Widget) handle;
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  if (label)
    {
      XmString text = XmStringCreateSimple (label);
      XtVaSetValues (widget,
		     XmNlabelString, text,
		     XmNlabelType, XmSTRING,
		     NULL);
      XmStringFree (text);
    }
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
#endif
#ifdef wx_xview
  if (label)
    {
      Panel_item item = (Panel_item) handle;
      xv_set (item, PANEL_CHOICE_STRING, 0, label, NULL);
    }
#endif
}

void wxCheckBox::SetLabel (wxBitmap * bitmap)
{
  if (!buttonBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --buttonBitmap->selectedIntoDC;
  buttonBitmap = bitmap;
  buttonBitmap->selectedIntoDC++;

#ifdef wx_motif
  Widget widget = (Widget) handle;
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  if (bitmap)
    {
      XtVaSetValues (widget,
		     XmNlabelPixmap, bitmap->GetLabelPixmap (widget),
		     XmNselectPixmap, bitmap->GetLabelPixmap (widget),
		XmNlabelInsensitivePixmap, bitmap->GetInsensPixmap (widget),
	       XmNselectInsensitivePixmap, bitmap->GetInsensPixmap (widget),
		     XmNarmPixmap, bitmap->GetArmPixmap (widget),
		     XmNlabelType, XmPIXMAP,
		     NULL);
    }
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  if (!bitmap->x_image)
    bitmap->CreateServerImage(TRUE);

  xv_set (item, PANEL_CHOICE_IMAGE, 0, bitmap->x_image, NULL);
#endif
}

void wxCheckBox::SetValue (Bool val)
{
#ifdef wx_motif
  Widget buttonWidget = (Widget) handle;

  if (val)
    doubleClickAllowed |= IS_CHECKED;
  else
    doubleClickAllowed -= (doubleClickAllowed & IS_CHECKED);

  XmToggleButtonSetState (buttonWidget, val, TRUE);
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  int the_val = 0;
  if (val)
    the_val = 1;

  xv_set (item, PANEL_VALUE, the_val, NULL);
#endif
}

Bool wxCheckBox::GetValue (void)
{
#ifdef wx_motif
  Widget buttonWidget = (Widget) handle;
  return XmToggleButtonGetState (buttonWidget);
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  return (Bool) xv_get (item, PANEL_VALUE);
#endif
}

#ifdef wx_xview
void 
wxCheckBoxProc (Panel_item item, int value, Event * x_event)
{
  wxCheckBox *box = (wxCheckBox *) xv_get (item, PANEL_CLIENT_DATA);
  wxCommandEvent event  = new wxCommandEvent(wxEVENT_TYPE_CHECKBOX_COMMAND);

  event.commandString = (char *) xv_get (item, PANEL_LABEL_STRING);
  event.commandInt = value;
  event.eventObject = box;
  box->ProcessCommand (event);
}
#endif
