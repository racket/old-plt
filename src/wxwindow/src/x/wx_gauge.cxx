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

  canAddEventHandler = TRUE;
  windowName = copystring (name);

  panel->GrowReady();

  Widget panelForm = panel->panelWidget;
  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  char *the_label = NULL;

  if (label)
    {
      char buf[400];
      wxStripMenuCodes(label, buf);
      the_label = (style & wxFIXED_LENGTH) ? fillCopy(buf) : copystring(buf);
      XmString text = XmStringCreateSimple (the_label);
      labelWidget = XtVaCreateManagedWidget (buf,
#if USE_GADGETS
					     (style & wxCOLOURED
					      ? xmLabelWidgetClass 
					      : xmLabelGadgetClass),
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

  float lw, lh;
  if (the_label && labelWidget) {
    /* Get the size of the label; there should be a better way than essentially
       inlining GetTextExtent... */
    XFontStruct *fontStruct;

    if (labelFont)
      (void)labelFont->GetInternalFont(GetXDisplay(), &fontStruct);
    else {
      XmFontList list;
      XmFontContext context;
      XmStringCharSet ignored;

      XtVaGetValues((Widget)labelWidget, XmNfontList, &list, NULL);
      if (XmFontListInitFontContext(&context, list)) {
	if (!XmFontListGetNextFont(context, &ignored, &fontStruct))
	  fontStruct = 0;
	XmFontListFreeFontContext(context);
      } else
	fontStruct = 0;
    }

    if (fontStruct) {
      int direction, ascent, descent2;
      XCharStruct overall;
      
      XTextExtents(fontStruct, the_label, strlen(the_label), &direction, &ascent,
		   &descent2, &overall);
      lw = overall.width;
      lh = ascent + descent2;
    } else
      lw = lh = 0;
  } else
    lw = lh = 0;

  if (width == -1) {
    width = ((windowStyle & wxHORIZONTAL) ? 100 : 24);
    if (panel->label_position == wxHORIZONTAL)
      width += lw;
  }
  if (height == -1) {
    height = ((windowStyle & wxHORIZONTAL) ? 24 : 100);
    if (panel->label_position != wxHORIZONTAL)
      height += lh;
  }

  panel->AttachWidget(this, formWidget, x, y, width, height);

  SetValue(0);

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

  wxWidgetHashTable->Put((long)gaugeWidget, this);

  AddPreHandlers(gaugeWidget);

  return TRUE;
}

wxGauge::~wxGauge (void)
{
  wxWidgetHashTable->Delete((long)handle);
}

void wxGauge::SetSize (int x, int y, int width, int height, int sizeFlags)
{
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
}

void wxGauge::SetShadowWidth(int w)
{
  if (w == 0)
    w = 1;
  XtVaSetValues((Widget)handle, XmNshadowThickness, w, NULL);
}

void wxGauge::SetBezelFace(int w)
{
}

void wxGauge::SetRange(int r)
{
  XmGaugeSetMax((Widget)handle, r);
}

void wxGauge::SetValue(int pos)
{
  // XtVaSetValues((Widget)handle, XmNvalue, pos, NULL);
  XmGaugeSetValue((Widget)handle, pos);
}

void wxGauge::ChangeColour (void)
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
}

#endif
