/*
 * File:	wx_scrol.cc
 * Purpose:	Scrolbar items implementation (X version)
 * Author:  Sergey Krasnov (ksa@orgland.ru)
 * Created:	1994
 * Updated:
 * Copyright:	(c) 1994
 */

// static const char sccsid[] = "%W% %G%";

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"

#if USE_SCROLLBAR

#include "wx_stdev.h"
#include "wx_item.h"
#include "wx_scrol.h"
#include "wx_dialg.h"
#include "wx_utils.h"
#include "wx_privt.h"


#ifdef wx_motif
#include <X11/IntrinsicP.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrollBar.h>
#endif

// Scroll Bar


void wxScrollBarCallback(Widget widget, XtPointer clientData,
                        XmScaleCallbackStruct *cbs)
{
    wxScrollBar *scrollBar = (wxScrollBar *)clientData;
    wxCommandEvent *event = new wxCommandEvent(wxEVENT_TYPE_SCROLLBAR_COMMAND);
    XtVaGetValues(widget, XmNvalue, &event->commandInt, NULL);
    event->eventHandle = (char *)cbs->event;
    event->eventObject = scrollBar;
    scrollBar->ProcessCommand(*event);
}

IMPLEMENT_DYNAMIC_CLASS(wxScrollBar, wxItem)

wxScrollBar::wxScrollBar(void)
{
}

wxScrollBar::wxScrollBar(wxPanel *panel, wxFunction func, int direction,
                      int x, int y, int width, int height, long style, char *name):
	wxbScrollBar(panel, func, direction, x, y, width, height, style, name)
{
	Create(panel, func, direction, x, y, width, height, style, name);
}

Bool wxScrollBar::Create(wxPanel *panel, wxFunction func,
                    int direction, int x, int y, int width, int height,
                    long style, char *name)
{
    if (!panel)
        return FALSE;
    panel->AddChild(this);
    buttonFont = panel->buttonFont ;
    labelFont = panel->labelFont ;
    backColour = panel->backColour ;
    labelColour = panel->labelColour ;
    buttonColour = panel->buttonColour ;
    window_parent = panel;
    labelPosition = panel->label_position;

    windowName = copystring(name);
    Widget panelForm = panel->panelWidget;

    formWidget = XtVaCreateManagedWidget(windowName,
                  xmRowColumnWidgetClass, panelForm,
                  XmNorientation, XmHORIZONTAL,
                  XmNmarginHeight, 0,
                  XmNmarginWidth, 0,
                  NULL);

    int _direction = direction == wxHORIZONTAL ? XmHORIZONTAL: XmVERTICAL;

    Widget scrollBarWidget = XtVaCreateManagedWidget("scrollBarWidget",
                  xmScrollBarWidgetClass,  formWidget,
                  XmNorientation,      _direction,
                  NULL);
    if (buttonFont)
        XtVaSetValues(scrollBarWidget,
		      XmNfontList, 
		      /* MATTHEW: [4] Provide display */
		      buttonFont->GetInternalFont(XtDisplay(formWidget)), /* MATTHEW: [5] Use form widget */
		      NULL);

    handle = (char *)scrollBarWidget;

    XtAddCallback(scrollBarWidget, XmNvalueChangedCallback, (XtCallbackProc)wxScrollBarCallback, (XtPointer)this);
    XtAddCallback(scrollBarWidget, XmNdragCallback, (XtCallbackProc)wxScrollBarCallback, (XtPointer)this);

    panel->AttachWidget(this, formWidget, x, y, width, height);
    ChangeColour();

    /* After creating widgets, no more resizes. */
    if (style&wxFIXED_LENGTH)
        XtVaSetValues(formWidget,
                  XmNpacking,XmPACK_NONE,
                  NULL);

    Callback(func);

    wxWidgetHashTable->Put((long)scrollBarWidget, this);
    AddPreHandlers(scrollBarWidget);

    return TRUE;
}


wxScrollBar::~wxScrollBar(void)
{
  wxWidgetHashTable->Delete((long)scrollBarWidget);
}

void wxScrollBar::ChangeColour(void)
{
#ifdef wx_motif

    wxPanel *panel = (wxPanel*)window_parent ;
    /* MATTHEW: [4] Provide display */
    int change = wxComputeColors(XtDisplay(formWidget),
				 panel->backColour, panel->buttonColour);
    if (change==wxBACK_COLORS)
        XtVaSetValues(formWidget,
                  XmNbackground,        itemColors[wxBACK_INDEX].pixel,
                  XmNtopShadowColor,    itemColors[wxTOPS_INDEX].pixel,
                  XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
                  XmNforeground,        itemColors[wxFORE_INDEX].pixel,
                  NULL) ;
    else if (change==wxFORE_COLORS)
        XtVaSetValues(formWidget,
                  XmNforeground,        itemColors[wxFORE_INDEX].pixel,
                  NULL) ;

    /* MATTHEW: [4] Provide display */
    change = wxComputeColors(XtDisplay(formWidget),
			     backColour,buttonColour)  ;
    if (change==wxBACK_COLORS)
        XtVaSetValues((Widget)handle,
                  XmNbackground,        itemColors[wxBACK_INDEX].pixel,
                  XmNtopShadowColor,    itemColors[wxTOPS_INDEX].pixel,
                  XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
                  XmNarmColor,          itemColors[wxSELE_INDEX].pixel,
                  XmNforeground,        itemColors[wxFORE_INDEX].pixel,
                  NULL) ;
    else if (change==wxFORE_COLORS)
        XtVaSetValues((Widget)handle,
                  XmNforeground,        itemColors[wxFORE_INDEX].pixel,
                  NULL) ;
#endif
}


void wxScrollBar::GetSize(int *width, int *height)
{
#ifdef wx_motif
	wxItem::GetSize(width, height);
#endif
#ifdef wx_xview
	Scrollbar x_scrollBar = (Scrollbar) handle;
	*width = xv_get(x_scrollBar, XV_WIDTH);
	*height = xv_get(x_scrollBar, XV_HEIGHT);
#endif
}

void wxScrollBar::SetSize(int x, int y, int width, int height, int sizeFlags)
{
#ifdef wx_motif
    Widget scrollBarWidget = (Widget)handle;
 	if (x > -1)
		XtVaSetValues(scrollBarWidget, XmNx, x, NULL);
  	if (y> -1)
   		 XtVaSetValues(scrollBarWidget, XmNy, y, NULL);
  	if (width > -1)
    	XtVaSetValues(scrollBarWidget, XmNwidth, width, NULL);
  	if (height > -1)
    	XtVaSetValues(scrollBarWidget, XmNheight, height, NULL);
#endif
    wxItem::SetSize(x, y, width, height, sizeFlags);
}

void wxScrollBar::SetValue(int viewStart)
{
#ifdef wx_motif
    XtVaSetValues((Widget)handle, XmNvalue, viewStart, NULL);
#endif
#ifdef wx_xview
   xv_set((Scrollbar) handle, SCROLLBAR_VIEW_START, viewStart, NULL);
#endif
}

int wxScrollBar::GetValue(void)
{
#ifdef wx_motif
	int viewStart;
    XtVaGetValues((Widget)handle, XmNvalue, &viewStart, NULL);
	return viewStart;
#endif
#ifdef wx_xview
	return xv_get((Scrollbar) handle, SCROLLBAR_VIEW_START);
#endif
}

void wxScrollBar::SetPageLength(int pageLength)
{
#ifdef wx_motif
 	XtVaSetValues((Widget)handle, XmNpageIncrement, pageLength, NULL);
#endif
#ifdef wx_xview
   xv_set((Scrollbar) handle, SCROLLBAR_PAGE_LENGTH, pageLength, NULL);
#endif
}

void wxScrollBar::SetObjectLength(int objectLength)
{
#ifdef wx_motif
    XtVaSetValues((Widget)handle, XmNmaximum, objectLength, NULL);
#endif
#ifdef wx_xview
   	xv_set((Scrollbar) handle, SCROLLBAR_OBJECT_LENGTH, objectLength, NULL);
#endif
}

void wxScrollBar::SetViewLength(int viewLength)
{
#ifdef wx_motif
    XtVaSetValues((Widget)handle, XmNsliderSize, viewLength, NULL);
#endif
#ifdef wx_xview
   xv_set((Scrollbar) handle, SCROLLBAR_VIEW_LENGTH, viewLength, NULL);
#endif
}

void wxScrollBar::GetValues(int *viewStart, int *viewLength, int *objectLength,
//              	int *pixelsPerUnit,
				int *pageLength)
{
#ifdef wx_motif
    XtVaGetValues((Widget)handle,
			XmNvalue, viewStart,
			XmNsliderSize, viewLength,
 			XmNmaximum, objectLength,
//			XmNincrement, pixelsPerUnit,
			XmNpageIncrement, pageLength,
			NULL);
#endif
#ifdef wx_xview
	*viewStart = xv_get((Scrollbar) handle, SCROLLBAR_VIEW_START);
	*viewLength = xv_get((Scrollbar) handle, SCROLLBAR_VIEW_LENGTH);
	*objectLength = xv_get((Scrollbar) handle, SCROLLBAR_OBJECT_LENGTH);
// 	*pixelsPerUnit = xv_get((Scrollbar) handle, SCROLLBAR_PIXELS_PER_UNIT);
 	*pageLength = xv_get((Scrollbar) handle, SCROLLBAR_PAGE_LENGTH);
#endif
}

/*
void wxScrollBar::SetPixelsPerUnit(int pixelsPerUnit)
{
#ifdef wx_motif
    XtVaSetValues((Widget)handle, XmNincrement, pixelsPerUnit, NULL);
#endif
#ifdef wx_xview
	xv_set((Scrollbar) handle, SCROLLBAR_PIXELS_PER_UNIT, pixelsPerUnit, NULL);
#endif
}
*/

void wxScrollBar::Enable(Bool enable)
{
#ifdef wx_motif
    wxbScrollBar::Enable(enable);
#endif
#ifdef wx_xview
    Scrollbar window = (Scrollbar)handle;
    if (window)
        xv_set(window, SCROLLBAR_INACTIVE, !enable, NULL);
#endif
}

void wxScrollBar::Show (Bool show)
{
  SetShown(show);

  window_parent->GetChildren()->Show(this, show);

#ifdef wx_motif
    if (handle) {
        if (show)
            XtMapWidget(Widget(handle));
        else
            XtUnmapWidget(Widget(handle));
  }
#endif
#ifdef wx_xview
    xv_set ((Xv_opaque) handle, XV_SHOW, show, NULL);
#endif
}

char * wxScrollBar::GetLabel(void)
{
    return NULL;
}

void wxScrollBar::SetLabel(char *label)
{
}

#endif
