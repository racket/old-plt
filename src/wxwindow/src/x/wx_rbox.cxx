/*
 * File:        wx_rbox.cc
 * Purpose:     Radio box implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_rbox.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_rbox.h"

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#endif

void 
wxRadioBoxCallback (Widget w, XtPointer clientData,
		    XmToggleButtonCallbackStruct * cbs)
{
  if (!cbs->set)
    return;

  wxRadioBox *item = (wxRadioBox *) clientData;
  int sel = -1;
  int i;
  for (i = 0; i < item->no_items; i++)
    if (item->radioButtons && (item->radioButtons[i] == w))
      sel = i;
  item->selected = sel;

  wxCommandEvent *event  = new wxCommandEvent (wxEVENT_TYPE_RADIOBOX_COMMAND);
  item->ProcessCommand (*event);
}

// Radiobox item
IMPLEMENT_DYNAMIC_CLASS(wxRadioBox, wxItem)

wxRadioBox::wxRadioBox (void)
{
  selected = -1;
  no_items = 0;
  buttonBitmap = NULL;
  radioButtons = NULL;
  radioButtonLabels = NULL;
  labelWidget = NULL;
}

wxRadioBox::wxRadioBox (wxPanel * panel, wxFunction func,
	    char *Title,
	    int x, int y, int width, int height,
	    int N, char **Choices,
	    int majorDim, long Style, char *name):
wxbRadioBox (panel, func, Title, x, y, width, height, N, Choices,
	     majorDim, Style, name)
{
  Create (panel, func, Title, x, y, width, height, N, Choices, majorDim, Style, name);
}

wxRadioBox::wxRadioBox (wxPanel * panel, wxFunction func,
	    char *Title,
	    int x, int y, int width, int height,
	    int N, wxBitmap ** Choices,
	    int majorDim, long Style, char *name):
wxbRadioBox (panel, func, Title, x, y, width, height, N, Choices,
	     majorDim, Style, name)
{
  Create (panel, func, Title, x, y, width, height, N, Choices, majorDim, Style, name);
}

Bool wxRadioBox::
Create (wxPanel * panel, wxFunction func,
	char *Title,
	int x, int y, int width, int height,
	int N, char **Choices,
	int majorDim, long style, char *name)
{
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  buttonBitmap = new wxBitmap *[N];
  int i;
  for (i = 0; i < N; i++)
    buttonBitmap[i] = NULL;
  selected = -1;
  window_parent = panel;
  no_items = N;
  labelPosition = panel->label_position;
  if (majorDim == 0)
    majorDim = N;
  windowStyle = style;

  panel->GrowReady();

  canAddEventHandler = TRUE;
  windowName = copystring (name);
  Widget panelForm = panel->panelWidget;
  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  XmString text = 0;

  if (Title)
    {
      char buf[400];
      char mnem = wxFindMnemonic(Title);
      wxStripMenuCodes(Title, buf);

      char *the_label = (style & wxFIXED_LENGTH) ? fillCopy (buf) : copystring (buf);
      text = XmStringCreateSimple (the_label);
      labelWidget = XtVaCreateManagedWidget (Title,
#if USE_GADGETS
					     style & wxCOLOURED ?
				    xmLabelWidgetClass : xmLabelGadgetClass,
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
		       labelFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] Use form widget */
		       NULL);

      XmStringFree (text);
      delete[]the_label;
    }

  Arg args[3];
  //XtSetArg(args[0], XmNorientation, XmVERTICAL);
  //XtSetArg(args[1], XmNnumColumns, (panel->label_position == wxHORIZONTAL) ? N : 1);

  majorDim = (N + majorDim - 1) / majorDim;
/*
  XtSetArg (args[0], XmNorientation, panel->label_position == wxHORIZONTAL ?
	    XmHORIZONTAL : XmVERTICAL);
*/
  XtSetArg (args[0], XmNorientation, ((windowStyle & wxHORIZONTAL) == wxHORIZONTAL ?
  	                                XmHORIZONTAL : XmVERTICAL));
  XtSetArg (args[1], XmNnumColumns, majorDim);

  Widget radioBoxWidget = XmCreateRadioBox (formWidget, "radioBoxWidget", args, 2);

  handle = (char *) radioBoxWidget;

  if (panel->label_position == wxHORIZONTAL)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_FORM,
		       XmNleftAttachment, XmATTACH_FORM,
		       XmNbottomAttachment, XmATTACH_FORM,
		       XmNalignment, XmALIGNMENT_BEGINNING,
		       NULL);
      XtVaSetValues (radioBoxWidget,
		     XmNleftOffset, 4,
		     XmNtopAttachment, XmATTACH_FORM,
		     XmNbottomAttachment, XmATTACH_FORM,
	   XmNleftAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		     XmNleftWidget, labelWidget ? labelWidget : formWidget,
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

      XtVaSetValues (radioBoxWidget,
	    XmNtopAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		     XmNtopWidget, labelWidget ? labelWidget : formWidget,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNleftAttachment, XmATTACH_FORM,
		     NULL);
    }

  if (style & wxFLAT)
    XtVaSetValues (radioBoxWidget, XmNborderWidth, 1, NULL);

  radioButtons = new Widget[N];
  radioButtonLabels = new char *[N];
  radioEnabled = new Bool[N];
  for (i = 0; i < N; i++)
    {
      char mnem = wxFindMnemonic(Choices[i]);
      radioEnabled[i] = TRUE;
      radioButtonLabels[i] = copystring (Choices[i]);
      wxStripMenuCodes(Choices[i], radioButtonLabels[i]);
      radioButtons[i] = XtVaCreateManagedWidget(radioButtonLabels[i],
#if USE_GADGETS
						style & wxCOLOURED ?
						xmToggleButtonWidgetClass : xmToggleButtonGadgetClass,
						radioBoxWidget,
#else
						xmToggleButtonWidgetClass, radioBoxWidget,
#endif
						XmNmnemonic, mnem,
						NULL);
      XtAddCallback(radioButtons[i], XmNvalueChangedCallback, (XtCallbackProc) wxRadioBoxCallback,
		    (XtCallbackProc) this);
      
      if (buttonFont)
	XtVaSetValues(radioButtons[i],
		      XmNfontList,   
		      /* MATTHEW: [4] Provide display */
		      buttonFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] Use form widget */
		      NULL);
    }
  SetSelection (0);

  XtManageChild (radioBoxWidget);

  if (width == -1)
    width = 150;
  if (height == -1)
    height = 80;

  panel->AttachWidget (this, formWidget, x, y, width, height);
  ChangeColour ();

  /* After creating widgets, no more resizes. */
  if (style & wxFIXED_LENGTH)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_SELF,
		       XmNleftAttachment, XmATTACH_SELF,
		       NULL);

      XtVaSetValues (radioBoxWidget,
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

  Callback (func);

  wxWidgetHashTable->Put((long)radioBoxWidget, this);
  AddPreHandlers(radioBoxWidget);

  return TRUE;
}

static Pixmap wxOldPixmapValue;

Bool wxRadioBox::
Create (wxPanel * panel, wxFunction func,
	char *Title,
	int x, int y, int width, int height,
	int N, wxBitmap ** Choices,
	int majorDim, long style, char *name)
{
  if (panel)
    panel->AddChild (this);
  buttonFont = panel->buttonFont;
  labelFont = panel->labelFont;
  backColour = panel->backColour;
  labelColour = panel->labelColour;
  buttonColour = panel->buttonColour;
  selected = -1;
  window_parent = panel;
  no_items = N;
  labelPosition = panel->label_position;
  if (majorDim == 0) {
    if (N)
      majorDim = N;
    else
      majorDim = 1; /* MATTHEW: [13] */
  }
  buttonBitmap = new wxBitmap *[N];
  int i;
  for (i = 0; i < N; i++) {
    if (Choices[i]->Ok() && (Choices[i]->selectedIntoDC >= 0)) {
      buttonBitmap[i] = Choices[i];
      buttonBitmap[i]->selectedIntoDC++;
    } else
      buttonBitmap[i] = NULL;
  }
  windowStyle = style;

  panel->GrowReady();

  canAddEventHandler = TRUE;
  windowName = copystring (name);
  Widget panelForm = panel->panelWidget;
  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  if (Title)
    {
      char mnem = wxFindMnemonic(Title);
      char *actualLabel = wxStripMenuCodes(Title);
      char *the_label = (style & wxFIXED_LENGTH) ? fillCopy (actualLabel) : copystring (actualLabel);
      XmString text = XmStringCreateSimple (the_label);
      labelWidget = XtVaCreateManagedWidget (Title,
#if USE_GADGETS
					     style & wxCOLOURED ?
					     xmLabelWidgetClass : xmLabelGadgetClass,
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

  Arg args[3];
  //XtSetArg(args[0], XmNorientation, XmVERTICAL);
  //XtSetArg(args[1], XmNnumColumns, (panel->label_position == wxHORIZONTAL) ? N : 1);

  majorDim = (N + majorDim - 1) / majorDim;
  if (!majorDim)
    majorDim = 1; /* MATTHEW: [13] */

/*
  XtSetArg (args[0], XmNorientation, panel->label_position == wxHORIZONTAL ?
	    XmHORIZONTAL : XmVERTICAL);
*/
  XtSetArg (args[0], XmNorientation, ((windowStyle & wxHORIZONTAL) == wxHORIZONTAL ?
  	                                XmHORIZONTAL : XmVERTICAL));

  XtSetArg (args[1], XmNnumColumns, majorDim);

  Widget radioBoxWidget = XmCreateRadioBox (formWidget, "radioBoxWidget", args, 2);

  handle = (char *) radioBoxWidget;

  if (panel->label_position == wxHORIZONTAL)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_FORM,
		       XmNleftAttachment, XmATTACH_FORM,
		       XmNbottomAttachment, XmATTACH_FORM,
		       XmNalignment, XmALIGNMENT_BEGINNING,
		       NULL);
      XtVaSetValues (radioBoxWidget,
		     XmNleftOffset, 4,
		     XmNtopAttachment, XmATTACH_FORM,
		     XmNbottomAttachment, XmATTACH_FORM,
	   XmNleftAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		     XmNleftWidget, labelWidget ? labelWidget : formWidget,
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

      XtVaSetValues (radioBoxWidget,
	    XmNtopAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		     XmNtopWidget, labelWidget ? labelWidget : formWidget,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNleftAttachment, XmATTACH_FORM,
		     NULL);
    }

  if (style & wxFLAT)
    XtVaSetValues (radioBoxWidget, XmNborderWidth, 1, NULL);

  radioButtons = new Widget[N];
  radioButtonLabels = new char *[N];
  radioEnabled = new Bool[N];
  for (i = 0; i < N; i++)
    {
      char tmp[20];

      radioEnabled[i] = TRUE;

      sprintf (tmp, "<bad-image %d>", i);
      radioButtonLabels[i] = copystring (tmp);
      radioButtons[i] = XtVaCreateManagedWidget (tmp,
#if USE_GADGETS
						 style & wxCOLOURED ?
		      xmToggleButtonWidgetClass : xmToggleButtonGadgetClass,
						 radioBoxWidget,
#else
				  xmToggleButtonWidgetClass, radioBoxWidget,
#endif
						 NULL);
      Widget buttonWidget = radioButtons[i];
      if (buttonBitmap[i]) {
	XtVaGetValues(buttonWidget, XmNlabelPixmap, &wxOldPixmapValue, NULL);

	XtVaSetValues (buttonWidget,
		       XmNlabelPixmap, Choices[i]->GetLabelPixmap (buttonWidget),
		       XmNselectPixmap, Choices[i]->GetLabelPixmap (buttonWidget),
		       XmNlabelInsensitivePixmap, Choices[i]->GetInsensPixmap (buttonWidget),
		       XmNselectInsensitivePixmap, Choices[i]->GetInsensPixmap (buttonWidget),
		       XmNarmPixmap, Choices[i]->GetArmPixmap (buttonWidget),
		       XmNlabelType, XmPIXMAP,
		       NULL);
      }

      XtAddCallback (buttonWidget, XmNvalueChangedCallback, (XtCallbackProc) wxRadioBoxCallback, (XtPointer) this);
      if (buttonFont)
	XtVaSetValues (buttonWidget,
		       XmNfontList,   
		       /* MATTHEW: [4] Provide display */
		       buttonFont->GetInternalFont(XtDisplay(formWidget)),/* MATTHEW: [5] Use form widget */
		       NULL);
    }
  SetSelection (0);

  XtManageChild (radioBoxWidget);

  if (width == -1)
    width = 150;
  if (height == -1)
    height = 80;

  panel->AttachWidget (this, formWidget, x, y, width, height);
  ChangeColour ();


  /* After creating widgets, no more resizes. */
  if (style & wxFIXED_LENGTH)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_SELF,
		       XmNleftAttachment, XmATTACH_SELF,
		       NULL);

      XtVaSetValues (radioBoxWidget,
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

  Callback (func);

  wxWidgetHashTable->Put((long)radioBoxWidget, this);
  AddPreHandlers(radioBoxWidget);

  return TRUE;
}

wxRadioBox::~wxRadioBox (void)
{
  if (buttonBitmap) {
    int i;
    for (i = 0; i < no_items; i++)
      if (buttonBitmap[i]) {
	--buttonBitmap[i]->selectedIntoDC;
	XtVaSetValues (radioButtons[i],
		       XmNlabelPixmap, wxOldPixmapValue,
		       XmNselectPixmap, wxOldPixmapValue, 
		       XmNlabelInsensitivePixmap, wxOldPixmapValue,
		       XmNselectInsensitivePixmap, wxOldPixmapValue, 
		       XmNarmPixmap, wxOldPixmapValue,
		       XmNlabelType, XmSTRING,
		       NULL);
      }

    delete[]buttonBitmap;
  }
  if (radioButtonLabels)
    {
      int i;
      for (i = 0; i < no_items; i++)
	delete[]radioButtonLabels[i];
      delete[]radioButtonLabels;
    }
  if (radioButtons) {
#if 0
    int i;
    for (i = 0; i < no_items; i++)
      if (radioButtons[i])
	XtDestroyWidget(radioButtons[i]);
#endif
    delete[] radioButtons;
  }

  wxWidgetHashTable->Delete((long)handle);
}

void wxRadioBox::ChangeColour (void)
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
  change = wxComputeColors(XtDisplay(formWidget),
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
      int i;
      for (i = 0; i < no_items; i++)
	XtVaSetValues (radioButtons[i],
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
		     XmNborderColor, itemColors[wxFORE_INDEX].pixel,
		     NULL);
      int i;
      for (i = 0; i < no_items; i++)
	XtVaSetValues (radioButtons[i],
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
		       XmNarmColor, itemColors[wxSELE_INDEX].pixel,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
      else if (change == wxFORE_COLORS)
	XtVaSetValues (labelWidget,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       NULL);
    }
  int i;
  if (buttonBitmap)
    for (i = 0; i < no_items; i++)
      if (buttonBitmap[i])
	XtVaSetValues (radioButtons[i],
		       XmNlabelPixmap,
		       buttonBitmap[i]->GetLabelPixmap (radioButtons[i]),
		       XmNlabelInsensitivePixmap,
		       buttonBitmap[i]->GetInsensPixmap (radioButtons[i]),
		       XmNarmPixmap,
		       buttonBitmap[i]->GetArmPixmap (radioButtons[i]),
		       NULL);

#endif
}

char *wxRadioBox::GetLabel (int item)
{
  if (item < 0 || item >= no_items)
    return NULL;
#ifdef wx_motif
  Widget widget = (Widget) radioButtons[item];
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
  Panel_item x_choice = (Panel_item) handle;
  return ((char *) xv_get (x_choice, PANEL_CHOICE_STRING, item));
#endif
}

void wxRadioBox::SetLabel (int item, char *label)
{
  if (item < 0 || item >= no_items || (buttonBitmap && buttonBitmap[item]))
    return;

  Widget widget = (Widget) radioButtons[item];
  if (label) {
    char buf[400];
    char mnem = wxFindMnemonic(label);
    wxStripMenuCodes(label, buf);
    XmString text = XmStringCreateSimple (label);
    XtVaSetValues (widget,
		   XmNlabelString, text,
		   XmNmnemonic, mnem,
		   XmNlabelType, XmSTRING,
		   NULL);
    XmStringFree (text);
  }
}

void wxRadioBox::SetLabel (int item, wxBitmap * bitmap)
{
  if (item < 0 || item >= no_items || !buttonBitmap || !buttonBitmap[item]
      || !bitmap->Ok() || (bitmap->selectedIntoDC < 0))
    return;
  --buttonBitmap[item]->selectedIntoDC;
  buttonBitmap[item] = bitmap;
  ++buttonBitmap[item]->selectedIntoDC;
#ifdef wx_motif
  Widget widget = (Widget) radioButtons[item];
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
#endif
#ifdef wx_xview
  Panel_item x_choice = (Panel_item) handle;
  if (bitmap)
  {
    if (!bitmap->x_image)
      bitmap->CreateServerImage(TRUE);

    xv_set (x_choice, PANEL_CHOICE_IMAGE, item, bitmap->x_image, NULL);
  }
#endif
}

int wxRadioBox::FindString (char *s)
{
#ifdef wx_motif
  int i;
  for (i = 0; i < no_items; i++)
    if (strcmp (s, radioButtonLabels[i]) == 0)
      return i;
  return -1;
#endif
#ifdef wx_xview
  Panel_item choice = (Panel_item) handle;

  int max1 = no_items;

  int i = 0;
  int found = -1;
  while (found == -1 && i < max1)
    {
      char *label = (char *) xv_get (choice, PANEL_CHOICE_STRING, i);
      if (label && strcmp (label, s) == 0)
	found = i;
      else
	i++;
    }
  return found;
#endif
}

void wxRadioBox::SetSelection (int N)
{
  if ((N < 0) || (N >= no_items))
    return;

#ifdef wx_motif
  selected = N;
  XmToggleButtonSetState (radioButtons[N], TRUE, FALSE);
  int i;
  for (i = 0; i < no_items; i++)
    if (i != N)
      XmToggleButtonSetState (radioButtons[i], FALSE, FALSE);
#endif
#ifdef wx_xview
  Panel_item choice_item = (Panel_item) handle;
  xv_set (choice_item, PANEL_VALUE, N, NULL);
#endif
}

// Get selection
int wxRadioBox::GetSelection (void)
{
#ifdef wx_motif
  return selected;
#endif
#ifdef wx_xview
  return xv_get ((Panel_item) handle, PANEL_VALUE, NULL);
#endif
}

// Find string for position
char *wxRadioBox::GetString (int N)
{
  if ((N < 0) || (N >= no_items))
    return NULL;

#ifdef wx_motif

  return radioButtonLabels[N];
#endif
#ifdef wx_xview
  Panel_item item = (Panel_item) handle;
  return (char *) xv_get (item, PANEL_CHOICE_STRING, N);
#endif
}

void wxRadioBox::SetSize (int x, int y, int w, int h, int sizeFlags)
{
#ifdef wx_motif
#ifdef _SIZE_TRACE
  printf("assign R: %lx %d %d\n", this, w, h);
#endif

  Position cx, cy;
  Dimension cw, ch;
  int pchanged = 0, schanged = 0;

  XtVaGetValues(formWidget, 
		XmNx, &cx, 
		XmNy, &cy, 
		XmNwidth, &cw, 
		XmNheight, &ch, 
		NULL);
    
  if ((x > -1) && (x != cx)) {
    cx = x;
    pchanged = 1;
  }
  if ((y > -1) && (y != cy)) {
    cy = y;
    pchanged = 1;
  }
  if ((w > -1) && (w != cw)) {
    cw = w;
    schanged = 1;
  }
  if ((h > -1) && (h != ch)) {
    ch = h;
    schanged = 1;
  }

  if (!pchanged && !schanged)
    return;

  int width = w, height = h;
  Widget widget = (Widget) handle;
  int pw, ph;
  
  GetParent()->GetSize(&pw, &ph);

  Bool isShow = XtIsManaged(formWidget);
  if (isShow)
    XtUnmanageChild (formWidget);

  if (x > -1)
    XtVaSetValues (formWidget, XmNleftAttachment, XmATTACH_SELF,
		   XmNx, x, NULL);
  if (y > -1)
    XtVaSetValues (formWidget, XmNtopAttachment, XmATTACH_SELF,
		   XmNy, y, NULL);

  // Must set the actual RadioBox to be desired size MINUS label size
  Dimension labelWidth = 0, labelHeight = 0;
  int actualWidth = 0, actualHeight = 0;

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

  /* MATTHEW: [14] */
  if (actualWidth < 0)
    actualWidth = 1;
  if (actualHeight < 0)
    actualHeight = 1;

  if (width > 0)
    {
      XtVaSetValues (widget, XmNwidth, actualWidth, NULL);
    }
  if (height > 0)
    {
      XtVaSetValues (widget, XmNheight, actualHeight, NULL);
    }
  
  if (isShow)
    XtManageChild (formWidget);

  GetParent()->SetSize(-1, -1, pw, ph, 0x100);

  sr_width = width;
  sr_height = height;
  GetEventHandler()->OnSize (width, height);
#endif
#ifdef wx_xview
  wxItem::SetSize (x, y, width, height, sizeFlags);
#endif
}

#ifdef wx_motif
void wxRadioBox::Enable (Bool enable)
{
  int i;
  for (i = 0; i < no_items; i++)
    XtSetSensitive(radioButtons[i], 
		   enable ? radioEnabled[i] : FALSE);
}
#endif

void wxRadioBox::Enable (int item, Bool enable)
{
#ifdef wx_motif
#if 0
  if (item < 0)
    wxRadioBox::Enable (enable);
#endif
  if (item >= 0 && item < no_items) {
    radioEnabled[item] = enable;
    XtSetSensitive (radioButtons[item], enable);
  }
#endif
#ifdef wx_xview
  wxWindow::Enable (enable);
#endif
}

void wxRadioBox::Show (int item, Bool show)
{
  // This method isn't complete, and we try do do our best...
  // It's main purpose isn't for allowing Show/Unshow dynamically,
  // but rather to provide a way to design wxRadioBox such:
  //
  //        o Val1  o Val2   o Val3 
  //        o Val4           o Val6 
  //        o Val7  o Val8   o Val9 
  //
  // In my case, this is a 'direction' box, and the Show(5,False) is
  // coupled with an Enable(5,False)
  //
#ifdef wx_motif
  if (item < 0)
    wxItem::Show (show);
  else if (item < no_items)
    {
      XtVaSetValues (radioButtons[item],
		     XmNindicatorOn, (unsigned char) show,
		     NULL);
      // Please note that this is all we can do: removing the label
      // if switching to unshow state. However, when switching
      // to the on state, it's the prog. resp. to call SetLabel(item,...)
      // after this call!!
      if (!show)
        wxRadioBox::SetLabel (item, " ");
    }
#endif
#ifdef wx_xview
  //No way...
#endif
}


int wxRadioBox::ButtonFocus(int which)
{
  if (which >= no_items) return -1;

  if (which < 0) {
    Widget fw = XmGetFocusWidget((Widget)handle);
    int i;
    for (i = no_items; i--; )
      if (radioButtons[i] == fw)
	return i;
    return -1;
  } else {
    XmProcessTraversal(radioButtons[which], XmTRAVERSE_CURRENT);
    XmProcessTraversal(radioButtons[which], XmTRAVERSE_CURRENT);

    return -1;
  }
}
