/*
 * File:      wx_panel.cc
 * Purpose:     wxPanel class implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_panel.cxx,v 1.3 1998/08/09 20:55:26 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_panel.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_item.h"
#include "wx_dcpan.h"
#include "wx_privt.h"

#if USE_EXTENDED_STATICS
#include "wx_stat.h"
#endif

#include <Xm/Form.h>
#include <Xm/BulletinB.h>
#include <Xm/PushB.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>


// Constructor
IMPLEMENT_DYNAMIC_CLASS(wxPanel, wxCanvas)

wxPanel::wxPanel (void)
{
  new_line = FALSE;
  label_position = wxHORIZONTAL;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;
  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;
  current_hspacing = hSpacing;
  current_vspacing = vSpacing;

  panelBackgroundBrush = NULL;

  labelFont = NULL;
  buttonFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;

  // For absolute-positioning auto layout
  cursor_x = PANEL_LEFT_MARGIN;
  cursor_y = PANEL_TOP_MARGIN;
  max_height = 0;
  max_line_height = 0;
  max_width = 0;
  last_created = 0;
  borderWidget = 0;
  new_line = FALSE;

  firstRowWidget = NULL;
  lastWidget = NULL;
  panelWidget = NULL;
  allRelative = TRUE;
  currentRow = 0;
  currentCol = 0;
  handle = NULL;

  // Focus processing
  previousFocus = NULL;
  manualChange = FALSE;
  has_child = FALSE;
  wx_dc = NULL;

}

wxPanel::wxPanel (wxWindow *parent, int x, int y, int width, int height,
	 long style, char *name):
wxbPanel (parent, x, y, width, height, style, name)
{
  Create (parent, x, y, width, height, style, name);
}

Bool wxPanel::
Create (wxWindow *parent, int x, int y, int width, int height,
	long style, char *name)
{
  wx_dc = NULL;

  SetName(name);
  new_line = FALSE;
  label_position = wxHORIZONTAL;

  if (!parent)
    return FALSE;

  window_parent = parent;

  panelBackgroundBrush = NULL;

  has_child = FALSE;
  windowStyle = style;

  hSpacing = PANEL_HSPACING;
  vSpacing = PANEL_VSPACING;

  initial_hspacing = hSpacing;
  initial_vspacing = vSpacing;

  current_hspacing = hSpacing;
  current_vspacing = vSpacing;

  labelFont = NULL;
  buttonFont = NULL;
  backColour = NULL;
  labelColour = NULL;
  buttonColour = NULL;
  if (wxSubType(parent->__type, wxTYPE_PANEL))
  {
    wxPanel *parentPanel = (wxPanel *)parent;
    parentPanel->GetValidPosition(&x,&y) ;
    labelFont = parentPanel->labelFont ;
    buttonFont = parentPanel->buttonFont ;
    backColour = parentPanel->backColour ;
    labelColour = parentPanel->labelColour ;
    buttonColour = parentPanel->buttonColour ;
  }

  // Focus processing
  previousFocus = NULL;
  manualChange = FALSE;

  // For absolute-positioning auto layout
  cursor_x = PANEL_LEFT_MARGIN;
  cursor_y = PANEL_TOP_MARGIN;
  max_height = 0;
  max_line_height = 0;
  max_width = 0;

  borderWidget = 0;

  firstRowWidget = NULL;
  lastWidget = NULL;
  allRelative = TRUE;
  panelWidget = NULL;

  currentRow = 0;
  currentCol = 0;

  last_created = 0;

  int p_width, p_height;
  parent->GetClientSize(&p_width, &p_height);
  // Must create panel big for Motif to resize properly later
  if (width == -1)
    width = p_width;
  if (height == -1)
    height = p_height;

  /* empirically determined: height can be 0 if status line is added */
  if (width < 1)
    width = 1;
  if (height < 1)
    height = 1;

  Widget parentWidget = 0;
  if (wxSubType(parent->__type, wxTYPE_FRAME))
    parentWidget = ((wxFrame *)parent)->clientArea;
  else if (wxSubType(parent->__type, wxTYPE_PANEL))
    parentWidget = (Widget)parent->handle;
  else
  {
    wxError("Panel subwindow must be a child of either a frame or panel!");
    return FALSE;
  }

  wxCanvas::Create(parent, x, y, width, height, style, name);
  panelWidget = (Widget)handle;

  // Construct a new brush that takes on the
  // real background colour of this panel.
  panelBackgroundBrush = new wxBrush;
  Pixel thePix;
  XtVaGetValues(panelWidget, XmNbackground, &thePix, NULL);
  panelBackgroundBrush->colour.pixel = thePix;
  GetPanelDC()->SetBackground(panelBackgroundBrush);
  return TRUE;
}

wxPanel::~wxPanel (void)
{
  if (panelBackgroundBrush)
  {
    wx_dc->SetBackground(NULL);
    delete panelBackgroundBrush;
  }
  DestroyChildren ();

/*
  if (borderWidget)
    {
      // This patch come from Torsten. (lier@lier1)
      // I do not know if it is needed in j1 version??
      wxWidgetHashTable->Delete ((long) borderWidget);
      XtDestroyWidget (borderWidget);
      borderWidget = 0;
    }
*/
}

void wxPanel:: SetSize (int x, int y, int w, int h, int sizeFlags)
{
#if 0
  /* MATTHEW: [15] Make widget bigger for adding controls */
  Dimension oldw, oldh;
  if (panelWidget) {
    XtVaGetValues(panelWidget, XmNwidth, &oldw, XmNheight, &oldh, NULL);
    XtUnmanageChild(panelWidget);
  } else
    oldw = oldh = 0;
#endif

  wxCanvas::SetSize(x, y, w, h, sizeFlags);

#if 0
  if (panelWidget) {
    if (oldw < w) oldw = w;
    if (oldh < h) oldh = h;
    XtVaSetValues (panelWidget, XmNwidth, oldw, XmNheight, oldh, NULL);
    XtManageChild(panelWidget);
  }
#endif
}

void wxPanel:: SetClientSize (int w, int h)
{
  SetSize (-1, -1, w, h);
}

void wxPanel:: GetPosition (int *x, int *y)
{
 wxCanvas::GetPosition(x, y);
}

/*****************************************************************
 * ITEM PLACEMENT FUNCTIONS
 *****************************************************************/


// Start a new line
void wxPanel:: RealNewLine (void)
{
  //cursor_x = PANEL_LEFT_MARGIN;
  cursor_x = initial_hspacing;
  if (max_line_height == 0)
    {
      cursor_y += current_vspacing;
    }
  else
    cursor_y += current_vspacing + max_line_height;
  max_line_height = 0;
  new_line = FALSE;
}

void wxPanel:: NewLine (void)
{
  if (new_line)
    current_vspacing += vSpacing;
  else
    current_vspacing = vSpacing;
  new_line = TRUE;
}

void wxPanel:: NewLine (int pixels)
{
  if (new_line)
    current_vspacing += pixels;
  else
    current_vspacing = pixels;
  new_line = TRUE;
}

void wxPanel:: Tab (void)
{
  // If we're doing a newline on a bulletin board, we work this
  // out ourselves.
  /* MATTHEW: use current_hspacing */
  current_hspacing += hSpacing;
}

void wxPanel:: Tab (int pixels)
{
  // If we're doing a newline on a bulletin board, we work this
  // out ourselves.
  /* MATTHEW: use current_hspacing */
  current_hspacing += pixels;
}

void wxPanel:: GetCursor (int *x, int *y)
{
  RealAdvanceCursor();
  *x = cursor_x;
  *y = cursor_y;
}

void wxPanel::SetItemCursor (int x, int y)
{
  last_created = NULL;
  new_line = FALSE;
  cursor_x = x;
  cursor_y = y;
}

// Set/get horizontal spacing
void wxPanel:: SetHorizontalSpacing (int sp)
{
  hSpacing = sp;
  current_hspacing = sp;
/*
   XtVaSetValues((Widget)handle,
   XmNmarginWidth, sp,
   NULL);
 */
}

int wxPanel:: GetHorizontalSpacing (void)
{
  return hSpacing;
}

// Set/get vertical spacing
void wxPanel:: SetVerticalSpacing (int sp)
{
  vSpacing = sp;
  current_vspacing = sp;
/*
   XtVaSetValues((Widget)handle,
   XmNmarginHeight, sp,
   NULL);
 */
}

int wxPanel:: GetVerticalSpacing (void)
{
  return vSpacing;
}

// Fits the panel around the items
void wxPanel:: Fit (void)
{
  RealAdvanceCursor ();
  SetClientSize (max_width + initial_hspacing, max_height + initial_vspacing);
}

// Update next cursor position
void wxPanel:: RealAdvanceCursor (void)
{
  wxWindow *item = last_created;
  if (item)
    {
      int width, height;
      int x, y;
      item->GetSize (&width, &height);
      item->GetPosition (&x, &y);

      if ((x + width) > max_width)
	max_width = x + width;
      if ((y + height) > max_height)
	max_height = y + height;
      if (height > max_line_height)
	max_line_height = height;
/*
   fprintf(stderr,"maxwidth %d maxheight %d maklineheight %d\n",
   max_width,max_height,max_line_height);
   fprintf(stderr,"Newline %d\n",new_line) ;
 */
      cursor_x = x + width + current_hspacing;
      cursor_y = y;
      last_created = NULL;
    }
  // Don't give a newline if we're starting the top row
  if (new_line)
    RealNewLine ();
}

// Update next cursor position
void wxPanel:: AdvanceCursor (wxWindow * item)
{
  // For calculating positions on a bulletin board.
  /*
     int width, height;
     int x, y;
     item->GetSize(&width, &height);
     item->GetPosition(&x, &y);

     if ((x + width) > max_width)
     max_width = x + width;
     if ((y + height) > max_height)
     max_height = y + height;
     if (height > max_line_height)
     max_line_height = height;

     cursor_x = x + width + hSpacing;
     cursor_y = y;
   */
  last_created = item;
}

// If x or y are not specified (i.e. < 0), supply
// values based on left to right, top to bottom layout.
// Internal use only.
void wxPanel:: GetValidPosition (int *x, int *y)
{
  // For calculating positions on a bulletin board.
  if (*x < 0)
    *x = cursor_x;

  if (*y < 0)
    *y = cursor_y;
}



void wxPanel::
AttachWidget (wxPanel * panel, Widget formWidget,
	      int x, int y, int width, int height)
{

//fprintf(stderr,"Subpanel %x Panel %x\n",this,panel) ;
  //fprintf(stderr,"Panel: ih %d iv %d ch %d cv %d hs %d vs %d\n",
  //      panel->initial_hspacing,panel->initial_vspacing,
  //      panel->current_hspacing,panel->current_vspacing,
  //      panel->hSpacing,panel->vSpacing) ;

  if ((x > -1) || (y > -1))
    panel->allRelative = FALSE;

  //itemOrientation = panel->label_position;

  wxWidgetHashTable->Put ((long) formWidget, this);
  XtTranslations ptr;
  XtOverrideTranslations (formWidget,
		   ptr = XtParseTranslationTable ("<Configure>: resize()"));
  XtFree ((char *) ptr);

  SetSize (x, y, width, height);
}

void wxPanel:: Centre (int direction)
{
  int x, y, width, height, panel_width, panel_height, new_x, new_y;

  wxWindow *father = GetParent ();
  if (!father)
    return;

  father->GetClientSize (&panel_width, &panel_height);
  GetSize (&width, &height);
  GetPosition (&x, &y);

  new_x = -1;
  new_y = -1;

  if (direction & wxHORIZONTAL)
    new_x = (int) ((panel_width - width) / 2);

  if (direction & wxVERTICAL)
    new_y = (int) ((panel_height - height) / 2);

  SetSize (new_x, new_y, -1, -1);

}

void wxPanel:: Enable (Bool Flag)
{
  if (panelWidget)
    XtSetSensitive (panelWidget, Flag);
}

void wxPanel:: AddChild (wxObject * child)
{
  if (!has_child)
    {
      initial_hspacing = hSpacing;
      initial_vspacing = vSpacing;
/*
   if (borderWidget)
   {
   XtVaSetValues(borderWidget,
   XmNmarginHeight,initial_vspacing,
   XmNmarginWidth,initial_hspacing,
   NULL) ;
   initial_hspacing = 0 ;
   initial_vspacing = 0 ;
   }
 */
    }
  has_child = TRUE;

  // cursor_x = hSpacing;
  // cursor_y = vSpacing;
  RealAdvanceCursor ();

  /* MATTHEW: Need to reset spacing just before an item is added. */

  current_hspacing = hSpacing;
  current_vspacing = vSpacing;

  children->Append (child);
}

void wxPanel:: ChangeColour (void)
{
  /* MATTHEW: [4] Provide display */
  int change = wxComputeColors (XtDisplay(panelWidget), 
				backColour, buttonColour);

  if (change == wxBACK_COLORS)
    {
      if (borderWidget)
	XtVaSetValues (borderWidget,
		       XmNbackground, itemColors[wxBACK_INDEX].pixel,
		       XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		       XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       XmNborderColor, itemColors[wxFORE_INDEX].pixel,
		       NULL);
      XtVaSetValues (panelWidget,
		     XmNbackground, itemColors[wxBACK_INDEX].pixel,
		     XmNtopShadowColor, itemColors[wxTOPS_INDEX].pixel,
		     XmNbottomShadowColor, itemColors[wxBOTS_INDEX].pixel,
		     XmNforeground, itemColors[wxFORE_INDEX].pixel,
		     XmNborderColor, itemColors[wxFORE_INDEX].pixel,
		     NULL);
    }
  else if (change == wxFORE_COLORS)
    {
      if (borderWidget)
	XtVaSetValues (borderWidget,
		       XmNforeground, itemColors[wxFORE_INDEX].pixel,
		       XmNborderColor, itemColors[wxFORE_INDEX].pixel,
		       NULL);
      XtVaSetValues (panelWidget,
		     XmNforeground, itemColors[wxFORE_INDEX].pixel,
		     XmNborderColor, itemColors[wxFORE_INDEX].pixel,
		     NULL);
    }

}


/*
 * Attach new widget to last widget, or below previous widget of
 * previous row if we've had a new line
 */

void wxPanel::AttachWidget (wxWindow *item, Widget formWidget,
	      int x, int y, int width, int height)
{

  if ((x > -1) || (y > -1))
    allRelative = FALSE;

  if (wxSubType(item->__type, wxTYPE_ITEM))
  {
    wxItem *panelItem = (wxItem *)item;
    panelItem->itemOrientation = label_position;
  }

  if (formWidget)
  {
    if (wxWidgetHashTable->Get ((long) formWidget))
    {
      wxError ("Widget table clash in wx_panel.cc");
      return;
    }
    wxWidgetHashTable->Put ((long) formWidget, item);
    XtTranslations ptr;
    XtOverrideTranslations (formWidget,
  		   ptr = XtParseTranslationTable ("<Configure>: resize()"));
    XtFree ((char *) ptr);
  }
  
  GetValidPosition (&x, &y);
  item->SetSize (x, y, width, height);
  AdvanceCursor (item);

  GrowDone();

  current_hspacing = hSpacing;
  current_vspacing = vSpacing;
}

void wxPanel::OptimizeLayout (void)
{
}

Window wxPanel::GetXWindow(void)
{
  return wxCanvas::GetXWindow();
}

void wxPanel::DoPaint(XRectangle *xrect, int n)
{
  updateRects.Clear();
  int i;
  for (i = 0; i < n; i++)
  {
    wxRectangle *rect = new wxRectangle;
    rect->x = xrect[i].x;
    rect->y = xrect[i].y;
    rect->width = xrect[i].width;
    rect->height = xrect[i].height;
    updateRects.Append(rect);
  }
  DoRefresh(TRUE);
  updateRects.Clear();
}

void wxPanel::OnPaint(void)
{
  PaintSelectionHandles();
}


// Override edit mode so we can remove translations etc.
void wxPanel::SetUserEditMode(Bool edit)
{
  editUIMode = edit;
  wxChildNode *node = GetChildren()->First();
  while (node)
  {
    wxWindow *win = (wxWindow *)node->Data();
    if (wxSubType(win->__type, wxTYPE_ITEM))
    {
      wxItem *item = (wxItem *)win;
      item->RemoveTranslations(edit);
    }
    node = node->Next();
  }
}


void wxPanel::GrowReady()
{
  wxWindow *p = GetParent();
  if (p) p->GrowReady();

  XtVaSetValues((Widget)handle,
		XmNresizePolicy, XmRESIZE_GROW,
		NULL);
}

void wxPanel::GrowDone()
{
  XtVaSetValues((Widget)handle,
		XmNresizePolicy, XmRESIZE_NONE,
		NULL);

  wxWindow *p = GetParent();
  if (p) p->GrowDone();
}
