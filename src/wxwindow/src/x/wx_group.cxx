/*
 * File:        wx_group.cc
 * Purpose:     Group item implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_group.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_group.h"

#ifdef wx_motif
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#endif

/*
 * Group box
 */
 
IMPLEMENT_DYNAMIC_CLASS(wxGroupBox, wxItem)

wxGroupBox::wxGroupBox (void)
{
}

wxGroupBox::wxGroupBox (wxPanel * panel, char *label,
	  int x, int y, int width, int height, long style, char *name):
 wxbGroupBox(panel, label, x, y, width, height, style, name)
{
  Create (panel, label, x, y, width, height, style, name);
}

Bool wxGroupBox::Create (wxPanel * panel, char *label,
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
  window_parent = panel;
  windowStyle = style;

  labelPosition = panel->label_position;
  Bool vert = (labelPosition == wxVERTICAL);

#ifdef wx_motif
  canAddEventHandler = TRUE;
  windowName = copystring (name);

  Widget panelForm = panel->panelWidget;

  if (!label)
    label = " ";
  char buf[400];
  wxStripMenuCodes(label, buf);

  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  if (label)
  {
      XmString text = XmStringCreateSimple (buf);
      labelWidget = XtVaCreateManagedWidget (label,
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

  Widget frameWidget = XtVaCreateManagedWidget ("frame",
					xmFrameWidgetClass, formWidget,
                                        XmNshadowType, XmSHADOW_IN,
//					XmNmarginHeight, 0,
//					XmNmarginWidth, 0,
					NULL);

    if (labelWidget) {
      /* MATTHEW: [5] */
      if (vert)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_FORM,
		       XmNleftAttachment, XmATTACH_FORM,
		       XmNrightAttachment, XmATTACH_FORM,
		       XmNalignment, XmALIGNMENT_BEGINNING,
		       NULL);
      else
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_FORM,
		       XmNleftAttachment, XmATTACH_FORM,
		       XmNbottomAttachment, XmATTACH_FORM,
		       XmNalignment, XmALIGNMENT_BEGINNING,
		       NULL);
    }

  /* MATTHEW: [5] */
  if (vert)
    XtVaSetValues (frameWidget,
		   XmNtopAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		   XmNtopWidget, labelWidget ? labelWidget : formWidget,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   NULL);
  else
    XtVaSetValues (frameWidget,
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNleftAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		   XmNleftWidget, labelWidget ? labelWidget : formWidget,
		   XmNrightAttachment, XmATTACH_FORM,
		   NULL);

  handle = (char *) frameWidget;

  panel->AttachWidget (this, formWidget, x, y, width, height);
  ChangeColour ();
#endif
#ifdef wx_xview
  Panel x_panel = (Panel) (panel->GetHandle ());
  Panel_item x_message;

  if (panel->new_line)
    {
      x_message = (Panel_item) xv_create (x_panel, PANEL_MESSAGE, PANEL_LAYOUT, PANEL_HORIZONTAL, PANEL_NEXT_ROW, -1, NULL);
      panel->new_line = FALSE;
    }
  else
    x_message = (Panel_item) xv_create (x_panel, PANEL_MESSAGE, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);

  xv_set (x_message,
             PANEL_CLIENT_DATA, (char *) this,
             NULL);

  if (label)
  {
    actualLabel = wxStripMenuCodes(label);
    xv_set (x_message,
             PANEL_LABEL_STRING, actualLabel);
  }

/*
   if (labelFont)
   xv_set(x_message, XV_FONT, labelFont->GetInternalFont(), NULL) ;
 */

  if (x > -1 && y > -1)
    (void) xv_set (x_message, XV_X, x, XV_Y, y, NULL);

  handle = (char *) x_message;
#endif
  return TRUE;
}


wxGroupBox::~wxGroupBox (void)
{
#ifdef wx_motif
//  wxWidgetHashTable->Delete ((long) handle);
#endif
}

void wxGroupBox::SetSize (int x, int y, int width, int height, int sizeFlags)
{

#ifdef wx_motif
  wxItem::SetSize (x, y, width, height);

  Dimension xx, yy;
  XtVaGetValues (labelWidget, XmNwidth, &xx, XmNheight, &yy, NULL);

  /* MATTHEW: [5] */
  if (labelPosition == wxVERTICAL)
    xx = 0;
  else
    yy = 0;

  if (width > -1)
    XtVaSetValues ((Widget) handle, XmNwidth, max(1, width - xx),
		   NULL);
  if (height > -1)
    XtVaSetValues ((Widget) handle, XmNheight, max(1, height - yy),
		   NULL);

  sr_width = width;
  sr_height = height;

  GetEventHandler()->OnSize (width, height);
#endif
#ifdef wx_xview
#endif
}

void wxGroupBox::SetLabel (char *label)
{
#ifdef wx_motif
  Widget widget = (Widget) handle;
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
#endif
#ifdef wx_xview
  if (label)
    { 
     char buf[400];
     wxStripMenuCodes(label, buf);
     Panel_item item = (Panel_item) handle;
      xv_set (item, PANEL_LABEL_STRING, buf, NULL);
    }
#endif
}

char *wxGroupBox::GetLabel (void)
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
      strcpy (wxBuffer, s);
      XtFree (s);
      return wxBuffer;
    }
  else
    {
      return NULL;
    }
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  strcpy (wxBuffer, (char *) xv_get (item, PANEL_LABEL_STRING));
  return wxBuffer;
#endif
}
