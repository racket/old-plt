/*
 * File:        wx_gauge.cc
 * Purpose:     Gauge implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_gauge.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"

#if USE_GAUGE
#include "wx_gauge.h"
#endif

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>

#if USE_GAUGE
#include "../../contrib/xmgauge/gauge.h"
#endif

#endif

/*
 * Gauge
 */

#if USE_GAUGE
IMPLEMENT_DYNAMIC_CLASS(wxGauge, wxItem)

#define LABEL_OFFSET_PIXELS 4

wxGauge::wxGauge(void)
{
}

wxGauge::wxGauge (wxPanel * panel,
	   char *Title, int range,
	   int x, int y, int width, int height,
	   long style, char *name):
wxbGauge (panel, Title, range, x, y, width, height, style, name)
{
  Create (panel, Title, range, x, y, width, height, style, name);
}

Bool wxGauge::
Create (wxPanel * panel, char *label, int range,
	int x, int y, int width, int height,
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
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  if (label)
    {
      char buf[400];
      wxStripMenuCodes(label, buf);
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

  Arg args[4];
  int count = 4;
  if (windowStyle & wxHORIZONTAL)
  {
    XtSetArg (args[0], XmNorientation, XmHORIZONTAL);
    XtSetArg (args[1], XmNprocessingDirection, XmMAX_ON_RIGHT);
  }
  else
  {
    XtSetArg (args[0], XmNorientation, XmVERTICAL);
    XtSetArg (args[1], XmNprocessingDirection, XmMAX_ON_TOP);
  }
  XtSetArg(args[2], XmNminimum, 0);
  XtSetArg(args[3], XmNmaximum, range);
  Widget gaugeWidget = XtCreateManagedWidget("gauge", xmGaugeWidgetClass, formWidget, args, count);


/*
  if (buttonFont)
    XtVaSetValues (listWidget,
		   XmNfontList, buttonFont->GetInternalFont (),
		   NULL);
*/
  handle = (char *) gaugeWidget;

  if (panel->label_position == wxHORIZONTAL)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_FORM,
		       XmNleftAttachment, XmATTACH_FORM,
		       XmNalignment, XmALIGNMENT_BEGINNING,
		       NULL);
      XtVaSetValues (gaugeWidget,
		     XmNleftOffset, labelWidget ? LABEL_OFFSET_PIXELS : 0,
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

      XtVaSetValues (gaugeWidget,
		     XmNtopAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		     XmNtopWidget, labelWidget ? labelWidget : formWidget,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNleftAttachment, XmATTACH_FORM,
		     XmNrightAttachment, XmATTACH_FORM,
		     NULL);
    }


  XtManageChild (gaugeWidget);

  if (width == -1)
    width = ((windowStyle & wxHORIZONTAL) ? 100 : 24);
  if (height == -1)
    height = ((windowStyle & wxHORIZONTAL) ? 24 : 100);

  panel->AttachWidget (this, formWidget, x, y, width, height);

  // Using a value of 0 causes a divide by zero crash in xmgauge
  // ratio = .../ (float)THIS.value;
  
  SetValue(1);

//  ChangeColour ();

  /* After creating widgets, no more resizes. */
  if (style & wxFIXED_LENGTH)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_SELF,
		       XmNleftAttachment, XmATTACH_SELF,
		       NULL);

      XtVaSetValues (gaugeWidget,
		     XmNtopAttachment, XmATTACH_SELF,
		     XmNbottomAttachment, XmATTACH_SELF,
		     XmNleftAttachment, XmATTACH_SELF,
		     XmNrightAttachment, XmATTACH_SELF,
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

  int orientation = (windowStyle & wxHORIZONTAL) ? PANEL_HORIZONTAL : PANEL_VERTICAL;

  if (x > -1 && y > -1)
    {
      if (panel->new_line)
	{
	  x_slider = (Panel_item) xv_create (x_panel, PANEL_GAUGE, PANEL_LAYOUT, label_position, PANEL_NEXT_ROW, -1,
                 PANEL_DIRECTION, orientation,
					     XV_X, x, XV_Y, y, NULL);
	  panel->new_line = FALSE;
	}
      else
	x_slider = (Panel_item) xv_create (x_panel, PANEL_GAUGE, PANEL_LAYOUT, label_position,
                 PANEL_DIRECTION, orientation,
                 XV_X, x, XV_Y, y, NULL);

    }
  else
    {
      if (panel->new_line)
	{
	  x_slider = (Panel_item) xv_create (x_panel, PANEL_GAUGE,
                 PANEL_DIRECTION, orientation,
                 PANEL_LAYOUT, PANEL_HORIZONTAL, PANEL_NEXT_ROW, -1,
                 NULL);
	  panel->new_line = FALSE;
	}
      else
	x_slider = (Panel_item) xv_create (x_panel, PANEL_GAUGE,
                 PANEL_DIRECTION, orientation,
                 PANEL_LAYOUT, PANEL_HORIZONTAL,
                 NULL);
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
	  PANEL_MIN_VALUE, 0,
	  PANEL_MAX_VALUE, range,
	  PANEL_CLIENT_DATA, (char *) this,
	  PANEL_VALUE, 0,
	  NULL);

/*
   if (buttonFont)
   xv_set(x_slider, XV_FONT, buttonFont->GetInternalFont(), NULL) ;
 */

  handle = (char *) x_slider;

  SetSize(x, y, width, -1);
#endif
  return TRUE;
}

wxGauge::~wxGauge (void)
{
}

void wxGauge::SetSize (int x, int y, int width, int height, int sizeFlags)
{
#ifdef wx_motif
  Bool isShow = XtIsManaged(formWidget);

  int pw, ph;
  GetParent()->GetSize(&pw, &ph);

  if (isShow)
    XtUnmanageChild (formWidget);

  if (x > -1)
    XtVaSetValues (formWidget, XmNleftAttachment, XmATTACH_SELF,
		   XmNx, x, NULL);
  if (y > -1)
    XtVaSetValues (formWidget, XmNtopAttachment, XmATTACH_SELF,
		   XmNy, y, NULL);

  // Must set the actual gauge to be desired size MINUS label size
  Dimension labelWidth = 0, labelHeight = 0, actualWidth = width, actualHeight = height;

  if (labelWidget)
    XtVaGetValues (labelWidget, XmNwidth, &labelWidth, XmNheight, &labelHeight, NULL);

  if (itemOrientation == wxHORIZONTAL)
    {
      actualWidth = width - labelWidth - (labelWidget ? LABEL_OFFSET_PIXELS : 0);
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

  GetParent()->SetSize(-1, -1, pw, ph, 0x100);

/*
  // Check resulting size is correct
  int tempW, tempH;
  GetSize (&tempW, &tempH);
*/
  sr_width = width;
  sr_height = height;
  GetEventHandler()->OnSize (width, height);
#endif
#ifdef wx_xview
  wxItem::SetSize(x, y, width, height, sizeFlags);

  if (width > -1)
  {
    Panel_item item = (Panel_item) handle;

    int oldGaugeWidth = xv_get (item, PANEL_GAUGE_WIDTH, NULL);
    Rect *rect = (Rect *) xv_get (item, XV_RECT);
    int oldWidth = rect->r_width;

    int diff = oldWidth - oldGaugeWidth;

    if (width > 0)
      width = width - diff;

    if (width > 0)
    {
      xv_set ((Panel_item)handle, PANEL_GAUGE_WIDTH, (int) width, NULL);
    };
  }
#endif
}

void wxGauge::SetShadowWidth(int w)
{
#ifdef wx_motif
  if (w == 0)
    w = 1;
  XtVaSetValues((Widget)handle, XmNshadowThickness, w, NULL);
#endif
}

void wxGauge::SetBezelFace(int w)
{
}

void wxGauge::SetRange(int r)
{
#ifdef wx_motif
  XtVaSetValues((Widget)handle, XmNmaximum, r, NULL);
#endif
#ifdef wx_xview
  xv_set((Panel_item)handle, PANEL_MIN_VALUE, 0, PANEL_MAX_VALUE, r, NULL);
#endif
}

void wxGauge::SetValue(int pos)
{
#ifdef wx_motif
  // XtVaSetValues((Widget)handle, XmNvalue, pos, NULL);
  XmGaugeSetValue((Widget)handle, pos);
#endif
#ifdef wx_xview
  xv_set((Panel_item)handle, PANEL_VALUE, pos, NULL);
#endif
}

void wxGauge::ChangeColour (void)
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
		     XmNborderColor, itemColors[wxFORE_INDEX].pixel,
		     NULL);
    }
  else if (change == wxFORE_COLORS)
    {
      XtVaSetValues ((Widget) handle,
		     XmNforeground, itemColors[wxFORE_INDEX].pixel,
		     XmNborderColor, itemColors[wxFORE_INDEX].pixel,
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
		       XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
      else if (change == wxFORE_COLORS)
	XtVaSetValues (labelWidget,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
    }

#endif
}

#endif
