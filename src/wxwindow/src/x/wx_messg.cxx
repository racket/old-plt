/*
 * File:        wx_messg.cc
 * Purpose:     Message item implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_messg.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_messg.h"

#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>

#define USE_MESSG_GADGETS USE_GADGETS

// Message
IMPLEMENT_DYNAMIC_CLASS(wxMessage, wxItem)

wxMessage::wxMessage (void)
{
}

wxMessage::wxMessage (wxPanel * panel, char *label, int x, int y, long style, char *name):
wxbMessage (panel, label, x, y, style, name)
{
  Create (panel, label, x, y, style, name);
}

#if USE_BITMAP_MESSAGE
wxMessage::wxMessage(wxPanel *panel, wxBitmap *image, int x, int y, long style, char *name):
  wxbMessage(panel, image, x, y, style, name)
{
  Create(panel, image, x, y, style, name);
}
#endif

Bool wxMessage::
Create (wxPanel * panel, char *label, int x, int y, long style, char *name)
{
  bm_label = NULL;
  SetName(name);
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  window_parent = panel;
  labelPosition = panel->label_position;
  windowStyle = style;
#if !USE_MESSG_GADGETS
  canAddEventHandler= TRUE;
#else
  canAddEventHandler= FALSE;
#endif
  windowName = copystring (name);

  if (!label)
    label = "";
  char *slabel = copystring(label);
  wxStripMenuCodes(label, slabel);

  Widget panelForm = panel->panelWidget;
  XmString text = XmStringCreateSimple(slabel);

  panel->GrowReady();

  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  labelWidget = XtVaCreateManagedWidget ("messageLabel",
#if USE_MESSG_GADGETS
					 style & wxCOLOURED ?
					 xmLabelWidgetClass : xmLabelGadgetClass,
					 formWidget,
#else
					 xmLabelWidgetClass, formWidget,
#endif
					 XmNlabelString, text,
					 XmNalignment, XmALIGNMENT_BEGINNING,
					 XmNtopAttachment, XmATTACH_FORM,
					 XmNleftAttachment, XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					 XmNrightAttachment, XmATTACH_FORM,
					 NULL);

  Widget evWidget;
#if USE_MESSG_GADGETS
  evWidget = formWidget;
#else
  evWidget = labelWidget;
#endif

  XmStringFree (text);

  handle = (char *) labelWidget;

  if (labelFont)
    XtVaSetValues (labelWidget,
		   XmNfontList, 
		   /* MATTHEW: [4] Provide display */
		   labelFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] Use form widget */
		   NULL);

  panel->AttachWidget (this, formWidget, x, y, -1, -1);
  ChangeColour ();

  wxWidgetHashTable->Put((long)labelWidget, this);
  AddPreHandlers(evWidget, labelWidget);

  return TRUE;
}

#if USE_BITMAP_MESSAGE
Bool wxMessage::
Create (wxPanel * panel, wxBitmap *image, int x, int y, long style, char *name)
{
  if (!image->Ok() || (image->selectedIntoDC < 0))
    return Create(panel, "<bad-image>", x, y, style, name);

  bm_label = image;
  image->selectedIntoDC++;

  SetName(name);
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  window_parent = panel;
  labelPosition = panel->label_position;
  windowStyle = style;

#if !USE_MESSG_GADGETS
  canAddEventHandler= TRUE;
#else
  canAddEventHandler= FALSE;
#endif
  windowName = copystring (name);

  Widget panelForm = panel->panelWidget;

  panel->GrowReady();

  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  labelWidget = XtVaCreateManagedWidget ("messageLabel",
#if USE_MESSG_GADGETS
					 style & wxCOLOURED ?
					 xmLabelWidgetClass : xmLabelGadgetClass,
					 formWidget,
#else
					 xmLabelWidgetClass, formWidget,
#endif
					 XmNalignment, XmALIGNMENT_BEGINNING,
					 XmNtopAttachment, XmATTACH_FORM,
					 XmNleftAttachment, XmATTACH_FORM,
					 XmNbottomAttachment, XmATTACH_FORM,
					 XmNrightAttachment, XmATTACH_FORM,
					 NULL);

  Widget evWidget;
#if USE_MESSG_GADGETS
  evWidget = formWidget;
#else
  evWidget = labelWidget;
#endif

  XtVaSetValues (labelWidget,
		 XmNlabelPixmap, image->GetLabelPixmap (labelWidget),
		 XmNlabelInsensitivePixmap, image->GetInsensPixmap (labelWidget),
                 XmNlabelType, XmPIXMAP,
                 NULL);
  handle = (char *) labelWidget;
/*
  if (labelFont)
    XtVaSetValues (labelWidget,
		   XmNfontList, labelFont->GetInternalFont (),
		   NULL);
*/
  panel->AttachWidget (this, formWidget, x, y, -1, -1);
  ChangeColour ();

  wxWidgetHashTable->Put((long)labelWidget, this);
  AddPreHandlers(evWidget, labelWidget);

  return TRUE;
}
#endif

wxMessage::~wxMessage (void)
{
  if (bm_label)
    --bm_label->selectedIntoDC;

  wxWidgetHashTable->Delete((long)labelWidget);
}

void wxMessage::ChangeColour (void)
{
  int change;

  wxPanel *panel = (wxPanel *) window_parent;
  /* MATTHEW: [4] Provide display */
  change = wxComputeColors (XtDisplay((Widget)formWidget),
			    panel->backColour,
			    panel->labelColour);
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
  change = wxComputeColors (XtDisplay((Widget)formWidget),
			    backColour, labelColour);
  if (change == wxBACK_COLORS)
    XtVaSetValues (labelWidget,
		   XmNbackground, itemColors[wxBACK_INDEX].pixel,
		   XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		   XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		   XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		   XmNforeground, itemColors[wxFORE_INDEX].pixel,
		   NULL);
  else if (change == wxFORE_COLORS)
    XtVaSetValues (labelWidget,
		   XmNforeground, itemColors[wxFORE_INDEX].pixel,
		   NULL);

}

#if USE_BITMAP_MESSAGE
void wxMessage::SetLabel(char *label)
{ 
  if (!bm_label) {
    /* Don't allow the widget to resize to match the label: */
    XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
    wxItem::SetLabel(label); 
    XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  }
}

void wxMessage::SetLabel(wxBitmap *image)
{
  if (!bm_label || !image->Ok() || (image->selectedIntoDC < 0))
    return;

  --bm_label->selectedIntoDC;
  bm_label = image;
  bm_label->selectedIntoDC++;

  Widget widget = (Widget) handle;
#if 0
  int x, y, w1, h1, w2, h2;

  GetPosition(&x, &y);

  w2 = image->GetWidth();
  h2 = image->GetHeight();
#endif

  if (image) {
    /* Don't allow the widget to resize to match the label: */
    XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);
    XtVaSetValues (widget,
		   XmNlabelPixmap, image->GetLabelPixmap(widget),
		   XmNlabelType, XmPIXMAP,
		   NULL);
    XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  }

#if 0
  GetSize(&w1, &h1);
  if (! (w1 == w2) && (h1 == h2))
    SetSize(x, y, w2, h2);
#endif
}
#endif
