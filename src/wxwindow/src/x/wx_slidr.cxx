/*
 * File:        wx_slidr.cc
 * Purpose:     Slider implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_slidr.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_main.h"
#include "wx_slidr.h"

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#endif

#define LABEL_OFFSET_PIXELS 4

// Slider

void 
wxSliderCallback (Widget widget, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
  wxSlider *slider = (wxSlider *) clientData;
  wxCommandEvent *event  = new wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);
  slider->ProcessCommand (*event);
}

IMPLEMENT_DYNAMIC_CLASS(wxSlider, wxItem)

wxSlider::wxSlider (void)
{
}

wxSlider::wxSlider (wxPanel * panel, wxFunction func, char *label, int value,
	  int min_value, int max_value, int width, int x, int y,
	  long style, char *name):
wxbSlider (panel, func, label, value, min_value, max_value, width, x, y, style, name)
{
  Create (panel, func, label, value, min_value, max_value, width, x, y, style, name);
}

Bool wxSlider::
Create (wxPanel * panel, wxFunction func, char *label, int value,
	int min_value, int max_value, int width, int x, int y,
	long style, char *name)
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
  labelPosition = panel->label_position;
  windowStyle = style;

  canAddEventHandler = TRUE;
  windowName = copystring (name);
  Widget panelForm = panel->panelWidget;

  panel->GrowReady();

  formWidget = XtVaCreateManagedWidget (windowName,
					xmRowColumnWidgetClass, panelForm,
					XmNorientation, panel->label_position == wxHORIZONTAL ? XmHORIZONTAL : XmVERTICAL,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  if (label)
    {
      char buf[400];
      char mnem = wxFindMnemonic(label);
      (void)wxStripMenuCodes(label, buf);
      char *the_label = (style & wxFIXED_LENGTH) ? fillCopy (buf) : copystring (buf);

      XmString text = XmStringCreateSimple (the_label);
      labelWidget = XtVaCreateManagedWidget ("choiceLabel",
#if USE_GADGETS
					     ((style & wxCOLOURED) 
					      ? xmLabelWidgetClass 
					      : xmLabelGadgetClass),
					     formWidget,
#else
					     xmLabelWidgetClass, formWidget,
#endif
					     XmNlabelString, text,
					     XmNmnemonic, mnem,
					     NULL);
      if (labelFont)
	XtVaSetValues (labelWidget,
		       XmNfontList, 
		       /* MATTHEW: [4] Provide display */
		       labelFont->GetInternalFont(XtDisplay(formWidget)),/* MATTHEW: [5] Use form widget */
		       NULL);

      XmStringFree (text);
      delete[]the_label;
    }

  Widget sliderWidget = XtVaCreateManagedWidget ("sliderWidget",
						 xmScaleWidgetClass, formWidget,
						 XmNorientation, (((windowStyle & wxVERTICAL) == wxVERTICAL) 
								  ? XmVERTICAL 
								  : XmHORIZONTAL),
						 XmNprocessingDirection, (((windowStyle & wxVERTICAL) == wxVERTICAL) 
									  ? XmMAX_ON_TOP 
									  : XmMAX_ON_RIGHT),
						 XmNmaximum, max_value,
						 XmNminimum, min_value,
						 XmNvalue, value,
						 XmNshowValue, (style & (wxHORIZONTAL << 2)) ? False : True,
						 NULL);

  if (buttonFont)
    XtVaSetValues(sliderWidget,
		  XmNfontList, 
		  /* MATTHEW: [4] Provide display */
		  buttonFont->GetInternalFont(XtDisplay(formWidget)),/* MATTHEW: [5] Use form widget */
		  NULL);

  handle = (char *) sliderWidget;
  
  XtAddCallback (sliderWidget, XmNvalueChangedCallback, (XtCallbackProc) wxSliderCallback, (XtPointer) this);

  panel->AttachWidget(this, formWidget, x, y,
		      (((windowStyle & wxVERTICAL) == wxVERTICAL) ? -1 : width),
		      (((windowStyle & wxVERTICAL) == wxVERTICAL) ? width : -1));
  
  ChangeColour ();

  Callback (func);

  wxWidgetHashTable->Put((long)sliderWidget, this);
  AddPreHandlers(sliderWidget);

  return TRUE;
}


wxSlider::~wxSlider (void)
{
  wxWidgetHashTable->Delete((long)handle);
}

void wxSlider::ChangeColour (void)
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

int wxSlider::GetValue (void)
{
  int val;
  XtVaGetValues ((Widget) handle, XmNvalue, &val, NULL);
  return val;
}

void wxSlider::SetValue (int value)
{
  int max_value, min_value;

  XtVaGetValues((Widget) handle, XmNmaximum, &max_value, XmNminimum, &min_value, NULL);

  if (value > max_value)
    value = max_value;
  if (value < min_value)
    value = min_value;

  XtVaSetValues((Widget) handle, XmNvalue, value, NULL);
}

void wxSlider::GetSize (int *width, int *height)
{
  wxItem::GetSize (width, height);
}

void wxSlider::SetSize (int x, int y, int width, int height, int /* sizeFlags */)
{
  Widget sliderWidget = (Widget) handle;

  Bool isShow = XtIsManaged(formWidget);

  int pw, ph;
  GetParent()->GetSize(&pw, &ph);

  if (isShow)
    XtUnmanageChild (formWidget);

  if (((windowStyle & wxHORIZONTAL) == wxHORIZONTAL) && width > -1)
  {
    Dimension labelWidth = 0;
    if (labelWidget && (labelPosition == wxHORIZONTAL))
      XtVaGetValues (labelWidget, XmNwidth, &labelWidth, NULL);

    /* -3: why? */
    XtVaSetValues (sliderWidget, XmNwidth /* XmNscaleWidth */, max ((width - labelWidth) - 3, 10), NULL);
  }

  if (((windowStyle & wxVERTICAL) == wxVERTICAL) && height > -1)
  {
    Dimension labelHeight = 0;
    if (labelWidget && (labelPosition == wxVERTICAL))
      XtVaGetValues (labelWidget, XmNheight, &labelHeight, NULL);

    XtVaSetValues (sliderWidget, XmNheight /* XmNscaleHeight */, max ((height - labelHeight), 10), NULL);
  }

  if (x > -1)
    XtVaSetValues (formWidget, XmNleftAttachment, XmATTACH_SELF, XmNx, x, NULL);
  if (y > -1)
    XtVaSetValues (formWidget, XmNtopAttachment, XmATTACH_SELF, XmNy, y, NULL);

  if (isShow)
    XtManageChild (formWidget);

  GetParent()->SetSize(-1, -1, pw, ph, 0x100);

  sr_width = width;
  sr_height = height;
  OnSize (width, height);
}
