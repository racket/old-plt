/*
 * File:        wx_buttn.cc
 * Purpose:     Button implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_buttn.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_buttn.h"
#include "wx_privt.h"

#include <Xm/PushBG.h>
#include <Xm/PushB.h>
#include <Xm/Form.h>

void 
wxButtonCallback (Widget w, XtPointer clientData, XtPointer ptr)
{
  if (!wxWidgetHashTable->Get ((long) w))
    // Widget has been deleted!
    return;

  wxButton *item = (wxButton *) clientData;
  wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_BUTTON_COMMAND);
  event->eventObject = item;
  item->ProcessCommand (*event);
}

// Button item notifier

IMPLEMENT_DYNAMIC_CLASS(wxButton, wxItem)

wxButton::wxButton (void)
{
  buttonBitmap = NULL;
}

wxButton::wxButton (wxPanel * panel, wxFunction Function, char *label,
	  int x, int y, int width, int height, long style, char *name):
wxbButton (panel, Function, label, x, y, width, height, style, name)
{
  Create (panel, Function, label, x, y, width, height, style, name);
}

wxButton::wxButton (wxPanel * panel, wxFunction Function, wxBitmap * bitmap,
	  int x, int y, int width, int height, long style, char *name):
wxbButton (panel, Function, bitmap, x, y, width, height, style, name)
{
  Create (panel, Function, bitmap, x, y, width, height, style, name);
}

Bool wxButton::Create (wxPanel * panel, wxFunction Function, char *label,
	int x, int y, int width, int height, long style, char *name)
{
  SetName(name);
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  buttonBitmap = NULL;
  window_parent = panel;
  windowStyle = style;

  Callback (Function);
  labelPosition = wxHORIZONTAL;

  canAddEventHandler = TRUE;
  windowName = copystring (name);

  Widget panelForm = panel->panelWidget;
  if (!label)
    label = " ";
  char buf[400];
  wxStripMenuCodes(label, buf);
  XmString text = XmStringCreateSimple (buf);

  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  /*
   * Patch Note (important)
   * There is no major reason to put a defaultButtonThickness here.
   * Not requesting it give the ability to put wxButton with a spacing
   * as small as requested. However, if some button become a DefaultButton,
   * other buttons are no more aligned -- This is why we set
   * defaultButtonThickness of ALL buttons belonging to the same wxPanel,
   * in the ::SetDefaultButton method.
   */
  Widget buttonWidget = XtVaCreateManagedWidget ("button",
  // Gadget causes problems for default button operation.
  // Ah! But now you can create wxButton which have 'problems' with
  // the wxCOLOURED flag. And we are both happy!!
  // [It seems to me that it's better to use as MANY gadgets as possible...]
  // Now #ifdefed independently, JACS 25/4/93
#if (USE_GADGETS && USE_BUTTON_GADGET)
						 style & wxCOLOURED ?
						 xmPushButtonWidgetClass : xmPushButtonGadgetClass,
						 formWidget,
#else
						 xmPushButtonWidgetClass, formWidget,
#endif
						 XmNlabelString, text,
						 // XmNdefaultButtonShadowThickness, 1, // See comment for wxButton::SetDefault
						 XmNtopAttachment, XmATTACH_FORM,
						 XmNleftAttachment, XmATTACH_FORM,
						 XmNbottomAttachment, XmATTACH_FORM,
						 XmNrightAttachment, XmATTACH_FORM,
						 NULL);

  XmStringFree (text);

  // Record in case button callback gets called after the widget has
  // been deleted.
  wxWidgetHashTable->Put ((long) buttonWidget, this);

  if (buttonFont)
    XtVaSetValues (buttonWidget,
		   XmNfontList, 
		   /* MATHEW: [4] Provide display */
		   buttonFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] Use form widget */
		   NULL);

  handle = (char *) buttonWidget;

  XtAddCallback (buttonWidget, XmNactivateCallback, (XtCallbackProc) wxButtonCallback,
		 (XtPointer) this);

  panel->AttachWidget (this, formWidget, x, y, width, height);
//  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
  ChangeColour ();

  AddPreHandlers(buttonWidget);

  return TRUE;
}

Bool wxButton::Create (wxPanel * panel, wxFunction Function, wxBitmap * bitmap,
	int x, int y, int width, int height, long style, char *name)
{
  if (!bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return Create(panel, Function, "<bad-image>", x, y, width, height, style, name);

  bitmap->selectedIntoDC++;

  SetName(name);
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  buttonBitmap = bitmap;
  window_parent = panel;
  windowStyle = style;

  Callback (Function);
  labelPosition = wxHORIZONTAL;

  canAddEventHandler = TRUE;
  windowName = copystring (name);

  Widget panelForm = panel->panelWidget;

  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					NULL);

  /*
   * Patch Note (important)
   * There is no major reason to put a defaultButtonThickness here.
   * Not requesting it give the ability to put wxButton with a spacing
   * as small as requested. However, if some button become a DefaultButton,
   * other buttons are no more aligned -- This is why we set
   * defaultButtonThickness of ALL buttons belonging to the same wxPanel,
   * in the ::SetDefaultButton method.
   */
  Widget buttonWidget = XtVaCreateManagedWidget ("button",
  // Gadget causes problems for default button operation.
  // Ah! But now you can create wxButton which have 'problems' with
  // the wxCOLOURED flag. And we are both happy!!
  // [It seems to me that it's better to use as MANY gadgets as possible...]
#if (USE_GADGETS_FOR_IMAGES && USE_BUTTON_GADGET)
						 style & wxCOLOURED ?
			  xmPushButtonWidgetClass : xmPushButtonGadgetClass,
						 formWidget,
#else
					xmPushButtonWidgetClass, formWidget,
#endif
//                  XmNdefaultButtonShadowThickness, 1, // See comment for wxButton::SetDefault
					    XmNtopAttachment, XmATTACH_FORM,
					   XmNleftAttachment, XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					  XmNrightAttachment, XmATTACH_FORM,
						 NULL);

  // Record in case button callback gets called after the widget has
  // been deleted.
  wxWidgetHashTable->Put ((long) buttonWidget, this);

  handle = (char *) buttonWidget;

  // Yes, this is useful, even if we are setting a wxBitmap*, because
  // one can call ::SetLabel(char*) on this item!
  if (buttonFont)
    XtVaSetValues (buttonWidget,
		   XmNfontList, 
		   /* MATHEW: [4] Provide display */
		   buttonFont->GetInternalFont (XtDisplay(buttonWidget)),
		   NULL);

  XtVaSetValues (buttonWidget,
		 XmNlabelPixmap, bitmap->GetLabelPixmap (buttonWidget),
	  XmNlabelInsensitivePixmap, bitmap->GetInsensPixmap (buttonWidget),
		 XmNarmPixmap, bitmap->GetArmPixmap (buttonWidget),
		 XmNlabelType, XmPIXMAP,
		 NULL);

  XtAddCallback (buttonWidget, XmNactivateCallback, (XtCallbackProc) wxButtonCallback,
		 (XtPointer) this);

  panel->AttachWidget (this, formWidget, x, y, width, height);
//  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
  ChangeColour ();

  AddPreHandlers(buttonWidget);

  return TRUE;
}

wxButton::~wxButton (void)
{

  wxWidgetHashTable->Delete ((long) handle);

  if (buttonBitmap)
    --buttonBitmap->selectedIntoDC;
}

void wxButton::ChangeColour (void)
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
  change = wxComputeColors(XtDisplay(formWidget),
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

}

void wxButton::SetSize (int x, int y, int width, int height, int sizeFlags)
{
//  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  wxItem::SetSize (x, y, width, height, sizeFlags);
//  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
#if 0
  if (width > -1)
    XtVaSetValues ((Widget) handle, XmNwidth, width,
		   NULL);
  if (height > -1)
    XtVaSetValues ((Widget) handle, XmNheight, height,
		   NULL);
  GetEventHandler()->OnSize (width, height);
#endif
}

void wxButton::SetLabel (char *label)
{
  if (buttonBitmap)
    return;

  Widget widget = (Widget) handle;
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  if (label)
    {
      char buf[400];
      wxStripMenuCodes(label, buf);
      XmString text = XmStringCreateSimple (buf);
      XtVaSetValues (widget,
		     XmNlabelString, text,
		     XmNlabelType, XmSTRING,
		     NULL);
      XmStringFree (text);
    }
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
}

void wxButton::SetLabel (wxBitmap * bitmap)
{
  if (!buttonBitmap || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;

  --buttonBitmap->selectedIntoDC;
  buttonBitmap = bitmap;
  buttonBitmap->selectedIntoDC++;

  Widget widget = (Widget) handle;
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  if (bitmap)
    {
      XtVaSetValues (widget,
		     XmNlabelPixmap, bitmap->GetLabelPixmap (widget),
		XmNlabelInsensitivePixmap, bitmap->GetInsensPixmap (widget),
		     XmNarmPixmap, bitmap->GetArmPixmap (widget),
		     XmNlabelType, XmPIXMAP,
		     NULL);
    }
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
}

char *wxButton::GetLabel (void)
{
  Widget widget = (Widget) handle;
  XmString text;
  char *s;
  XtVaGetValues (widget,
		 XmNlabelString, &text,
		 NULL);

  if (XmStringGetLtoR (text, XmSTRING_DEFAULT_CHARSET, &s))
    {
      strcpy (wxBuffer, s);
      XtFree (s);
      return wxBuffer;
    }
  else
    {
      return NULL;
    }
}

void wxButton::SetDefault (void)
{
  wxPanel *panel = (wxPanel *) GetParent ();
  if (panel)
    panel->defaultItem = this;

#if 0
  // We initially do not set XmNdefaultShadowThickness, to have small buttons.
  // Unfortunattelly, buttons are now mis-aligned. We try to correct this
  // now -- setting this ressource to 1 for each button in the same row.
  // Because it's very hard to find wxButton in the same row,
  // correction is straighforward: we set resource for all wxButton
  // in this wxPanel (but not sub panels)
  for (wxChildNode * node = panel->GetChildren ()->First (); node; node = node->Next ())
    {
      wxButton *item = (wxButton *) node->Data ();
      if (wxSubType (item->__type, wxTYPE_BUTTON))
	{
          XtUnmanageChild (item->formWidget);

	  XtVaSetValues ((Widget) (item->handle),
			 XmNdefaultButtonShadowThickness, 1,
			 NULL);

          XtManageChild (item->formWidget);
	}
    }				// while
#endif

  // XtVaSetValues((Widget)handle, XmNshowAsDefault, 1, NULL);
  XtVaSetValues ((Widget) panel->handle, XmNdefaultButton, (Widget) handle, NULL);
}

void wxButton::Command (wxCommandEvent & event)
{
  // How do we fill in this event structure.
  XButtonEvent buttonEvent;
  XtCallActionProc ((Widget) handle, "ArmAndActivate", (XEvent *) & buttonEvent, NULL, 0);
}
