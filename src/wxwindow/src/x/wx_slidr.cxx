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
#include "wx_slidr.h"

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#endif

#ifdef wx_xview
void wxSliderProc (Panel_item item, int value, Event * event);
#endif

// Slider

#ifdef wx_xview
void 
wxSliderProc (Panel_item item, int value, Event * x_event)
{
  wxSlider *slider = (wxSlider *) xv_get (item, PANEL_CLIENT_DATA);
  wxCommandEvent event  = new wxCommandEvent (wxEVENT_TYPE_SLIDER_COMMAND);
  event.commandInt = value;
  event.eventHandle = (char *) x_event;
  event.eventObject = slider;
  slider->ProcessCommand (event);
}
#endif

#ifdef wx_motif
void 
wxSliderCallback (Widget widget, XtPointer clientData, XmScaleCallbackStruct * cbs)
{
  wxSlider *slider = (wxSlider *) clientData;
  wxCommandEvent *event  = new wxCommandEvent(wxEVENT_TYPE_SLIDER_COMMAND);
  XtVaGetValues (widget, XmNvalue, &event->commandInt, NULL);
  event->eventHandle = (char *) cbs->event;
  event->eventObject = slider;
  slider->ProcessCommand (*event);
}
#endif

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
#ifdef wx_motif
  canAddEventHandler = TRUE;
  windowName = copystring (name);
  Widget panelForm = panel->panelWidget;

  formWidget = XtVaCreateManagedWidget (windowName,
					xmRowColumnWidgetClass, panelForm,
					XmNorientation, XmHORIZONTAL,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  if (label)
    {
      char buf[400];
      (void)wxStripMenuCodes(label, buf);
      char *the_label = (style & wxFIXED_LENGTH) ? fillCopy (buf) : copystring (buf);

      XmString text = XmStringCreateSimple (the_label);
      labelWidget = XtVaCreateManagedWidget ("choiceLabel",
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
		       labelFont->GetInternalFont(XtDisplay(formWidget)),/* MATTHEW: [5] Use form widget */
		       NULL);

      XmStringFree (text);
      delete[]the_label;
    }

  Widget sliderWidget = XtVaCreateManagedWidget ("sliderWidget",
					     xmScaleWidgetClass, formWidget,
      XmNorientation, (((windowStyle & wxVERTICAL) == wxVERTICAL) ? XmVERTICAL : XmHORIZONTAL),
				     XmNprocessingDirection, (((windowStyle & wxVERTICAL) == wxVERTICAL) ? XmMAX_ON_TOP : XmMAX_ON_RIGHT),
						 XmNmaximum, max_value,
						 XmNminimum, min_value,
						 XmNvalue, value,
						 XmNshowValue, True,
						 NULL);

  if (buttonFont)
    XtVaSetValues (sliderWidget,
		   XmNfontList, 
		   /* MATTHEW: [4] Provide display */
		   buttonFont->GetInternalFont(XtDisplay(formWidget)),/* MATTHEW: [5] Use form widget */
		   NULL);

  handle = (char *) sliderWidget;

  XtAddCallback (sliderWidget, XmNvalueChangedCallback, (XtCallbackProc) wxSliderCallback, (XtPointer) this);

  panel->AttachWidget (this, formWidget, x, y,
    (((windowStyle & wxVERTICAL) == wxVERTICAL) ? -1 : width),
    (((windowStyle & wxVERTICAL) == wxVERTICAL) ? width : -1));

  ChangeColour ();

  /* After creating widgets, no more resizes. */
  if (style & wxFIXED_LENGTH)
    {
      XtVaSetValues (formWidget,
		     XmNpacking, XmPACK_NONE,
		     NULL);

      if (labelWidget)
	{
	  XmString text = XmStringCreateSimple (label);
	  XtVaSetValues (labelWidget,
			 XmNlabelString, text,
			 NULL);
	  XmStringFree (text);
	}
    }
#endif
#ifdef wx_xview
  Panel x_panel = (Panel) (panel->GetHandle ());
  Panel_item x_slider;

  int label_position;
  if (panel->label_position == wxVERTICAL)
    label_position = PANEL_VERTICAL;
  else
    label_position = PANEL_HORIZONTAL;

  int sliderOrientation =
   (((windowStyle & wxVERTICAL) == wxVERTICAL) ? PANEL_VERTICAL : PANEL_HORIZONTAL);

  if (x > -1 && y > -1)
    {
      if (panel->new_line)
	{
	  x_slider = (Panel_item) xv_create (x_panel, PANEL_SLIDER,
                    PANEL_DIRECTION, sliderOrientation,
                    PANEL_LAYOUT, label_position, PANEL_NEXT_ROW, -1,
                    XV_X, x, XV_Y, y, NULL);
	  panel->new_line = FALSE;
	}
      else
	x_slider = (Panel_item) xv_create (x_panel, PANEL_SLIDER,
                    PANEL_DIRECTION, sliderOrientation,
                    PANEL_LAYOUT, label_position,
                    XV_X, x, XV_Y, y, NULL);

    }
  else
    {
      if (panel->new_line)
	{
	  x_slider = (Panel_item) xv_create (x_panel, PANEL_SLIDER,
                    PANEL_DIRECTION, sliderOrientation,
                    PANEL_LAYOUT, PANEL_HORIZONTAL, PANEL_NEXT_ROW, -1,
					     NULL);
	  panel->new_line = FALSE;
	}
      else
	x_slider = (Panel_item) xv_create (x_panel, PANEL_SLIDER,
                    PANEL_DIRECTION, sliderOrientation,
                    PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
    }

  if (label)
    {
      actualLabel = wxStripMenuCodes(label);
      
      if (style & wxFIXED_LENGTH)
	{
	  char *the_label = fillCopy (actualLabel);
	  xv_set (x_slider, PANEL_LABEL_STRING, the_label, NULL);

	  int label_x = (int) xv_get (x_slider, PANEL_LABEL_X);
	  int item_x = (int) xv_get (x_slider, PANEL_ITEM_X);
	  xv_set (x_slider, PANEL_LABEL_STRING, actualLabel,
		  PANEL_LABEL_X, label_x,
		  PANEL_ITEM_X, item_x,
		  NULL);
          delete[] the_label;
	}
      else
	xv_set (x_slider, PANEL_LABEL_STRING, actualLabel, NULL);
    }

  xv_set (x_slider,
	  PANEL_MIN_VALUE, min_value,
	  PANEL_MAX_VALUE, max_value,
	  PANEL_NOTIFY_PROC, wxSliderProc,
	  PANEL_CLIENT_DATA, (char *) this,
	  PANEL_VALUE, value,
	  NULL);

/*
   if (buttonFont)
   xv_set(x_slider, XV_FONT, buttonFont->GetInternalFont(), NULL) ;
 */

  if (width > 0)
    {
      xv_set (x_slider, PANEL_SLIDER_WIDTH, (int) width, NULL);
    };


  handle = (char *) x_slider;
#endif

  Callback (func);
  return TRUE;
}


wxSlider::~wxSlider (void)
{
}

void wxSlider::ChangeColour (void)
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

int wxSlider::GetValue (void)
{
#ifdef wx_motif
  int val;
  XtVaGetValues ((Widget) handle, XmNvalue, &val, NULL);
  return val;
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  return (int) xv_get (item, PANEL_VALUE);
#endif
}

void wxSlider::SetValue (int value)
{
#ifdef wx_motif
  XtVaSetValues ((Widget) handle, XmNvalue, value, NULL);
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  xv_set (item, PANEL_VALUE, value, NULL);
#endif
}

void wxSlider::GetSize (int *width, int *height)
{
#ifdef wx_motif
  wxItem::GetSize (width, height);
#endif
#ifdef wx_xview
  wxItem::GetSize (width, height);
#endif
}

void wxSlider::SetSize (int x, int y, int width, int height, int sizeFlags)
{
#ifdef wx_motif
  Widget sliderWidget = (Widget) handle;

  Bool isShow = XtIsManaged(formWidget);

  int pw, ph;
  GetParent()->GetSize(&pw, &ph);

  if (isShow)
    XtUnmanageChild (formWidget);

  if (((windowStyle & wxHORIZONTAL) == wxHORIZONTAL) && width > -1)
  {
    Dimension labelWidth = 0;
    if (labelWidget)
      XtVaGetValues (labelWidget, XmNwidth, &labelWidth, NULL);

    /* Why -3? I have no idea. */
    XtVaSetValues (sliderWidget, XmNwidth /* XmNscaleWidth */, max ((width - labelWidth) - 3, 10), NULL);
  }

  if (((windowStyle & wxVERTICAL) == wxVERTICAL) && height > -1)
  {
    Dimension labelHeight = 0;
//    if (labelWidget)
//      XtVaGetValues (labelWidget, XmNheight, &labelHeight, NULL);

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
#endif
#ifdef wx_xview
  wxItem::SetSize (x, y, width, height, sizeFlags);
#endif
}
