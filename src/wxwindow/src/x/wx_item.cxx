/*
 * File:      wx_item.cc
 * Purpose:     Panel item implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * RCS_ID:      $Id: wx_item.cxx,v 1.3 1998/04/08 00:09:13 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_item.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_item.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_item.h"

#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/PushBG.h>

// See above notes
char *
fillCopy (const char *str)
{
  size_t len = strlen (str);
  char *ptr = new char[len + 1];
  // memset should be the fastest
  memset (ptr, MEANING_CHARACTER, len);
  ptr[len] = '\0';
  return ptr;
}

/* When implementing a new item, be sure to:

 * - add the item to the parent panel
 * - set window_parent to the parent
 * - NULL any extra child window pointers not created for this item
 *   (e.g. label control that wasn't needed)
 * - delete any extra child windows in the destructor (e.g. label control)
 * - implement GetSize and SetSize
 * - to find panel position if coordinates are (-1, -1), use GetPosition
 * - call AdvanceCursor after creation, for panel layout mechanism.
 *
 */

/*
   Motif notes

   A panel is a form.
   Each item is created on a RowColumn or Form of its own, to allow a label to
   be positioned. wxListBox and wxMultiText have forms, all the others have RowColumns.
   This is to allow labels to be positioned to the top left (can't do it with a
   RowColumn as far as I know).
   AttachWidget positions widgets relative to one another (left->right, top->bottom)
   unless the x, y coordinates are given (more than -1).
 */


// Item members
IMPLEMENT_ABSTRACT_CLASS(wxItem, wxWindow)

wxItem::wxItem (void)
{
#ifdef wx_motif
  formWidget = 0;
  labelWidget = 0;
  rowNumber = 0;
  colNumber = 0;
  itemOrientation = wxHORIZONTAL;
  oldTranslations = 0;
  canAddEventHandler = FALSE;
#endif
#ifdef wx_x
  actualLabel = NULL;
#endif
}

wxItem::~wxItem (void)
{
  // item may be a menu, so check.
  wxObject *obj = (wxObject *) GetParent ();
  if (!obj || !wxSubType (obj->__type, wxTYPE_PANEL))
    return;

  if (labelWidget && (labelWidget != (Widget) handle))
    XtDestroyWidget (labelWidget);

  if (formWidget)
    {
      wxWidgetHashTable->Delete ((long) formWidget);
      if (formWidget)
	XtDestroyWidget (formWidget);

      // Necessary, to avoid ~wxWindow to (re)delete the Widget!!
      // (because all formWidget subtree has been deleted)
      handle = (char *) NULL;
    }
}

#ifdef wx_motif
void wxItem::RemoveTranslations(Bool remove)
{
  Widget theWidget = (Widget)handle;
  if (theWidget)
  {
    if (remove && !oldTranslations)
    {
      XtVaGetValues(theWidget, XmNtranslations, &oldTranslations, NULL);
      XtUninstallTranslations(theWidget);
    }
    else if (!remove && oldTranslations)
    {
      XtVaSetValues(theWidget, XmNtranslations, oldTranslations, NULL);
      oldTranslations = 0;
    }
  }
}
#endif

void wxItem::GetSize (int *width, int *height)
{
  Dimension xx, yy;
  XtVaGetValues (formWidget, XmNwidth, &xx, XmNheight, &yy, NULL);
  *width = xx;
  *height = yy;
#ifdef _SIZE_TRACE
  printf("report I: %lx %d %d\n", this, (int)xx, (int)yy);
#endif
}

void wxItem::GetPosition (int *x, int *y)
{
  Dimension xx, yy;
  XtVaGetValues (formWidget, XmNx, &xx, XmNy, &yy, NULL);
  *x = xx;
  *y = yy;
}

void wxItem::SetSize (int x, int y, int w, int h, int sizeFlags)
{
//  if (formWidget)
//    XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);

  Position cx, cy;
  Dimension cw, ch;
  int pchanged = 0, schanged = 0;

#ifdef _SIZE_TRACE
  printf("assign I: %lx %d %d %s(%d)\n", this, w, h, wxGetTypeName(__type), __type);
#endif

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

  Bool isShow = XtIsManaged(formWidget);
  int pw, ph;
  
  GetParent()->GetSize(&pw, &ph);

  if (isShow)
    XtUnmanageChild (formWidget);

  XtVaSetValues(formWidget, 
		XmNleftAttachment, XmATTACH_SELF,
		XmNx, cx,
		XmNtopAttachment, XmATTACH_SELF,
		XmNy, cy,
		XmNwidth, cw,
		XmNheight, ch, 
		NULL);

  if (isShow)
    XtManageChild (formWidget);
//  if (formWidget)
//    XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);

  GetParent()->SetSize(-1, -1, pw, ph, 0x100);

  sr_width = w;
  sr_height = h;

  GetEventHandler()->OnSize(w, h);
}

void wxItem::SetLabel (char *label)
{
  char buf[400];
  char mnem = wxFindMnemonic (label);
  wxStripMenuCodes(label, buf);
  if (!labelWidget)
    return;
  XmString text = XmStringCreateSimple (buf);
  XtVaSetValues (labelWidget,
		 XmNlabelString, text,
		 XmNmnemonic, mnem,
		 NULL);
  XmStringFree (text);
}

char *wxItem::GetLabel (void)
{
  if (!labelWidget)
    return NULL;

  XmString text;
  char *s;
  XtVaGetValues (labelWidget,
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

void wxItem::SetFocus(void)
{
  wxWindow::SetFocus ();
}

Bool wxItem::Show(Bool show)
{
  if (show == IsShown())
    return TRUE;

  SetShown(show);

  window_parent->GetChildren()->Show(this, show);

  if (formWidget) {
    if (show)
      XtManageChild (formWidget);
    else
      XtUnmanageChild (formWidget);
  }

  return TRUE;
}

void wxItem::Enable (Bool enable)
{
  wxWindow::Enable (enable);
}

float wxItem::GetCharHeight (void)
{
  return 0.0;
}

float wxItem::GetCharWidth (void)
{
  return 0.0;
}

// Find the letter corresponding to the mnemonic, for Motif
char wxFindMnemonic (char *s)
{
  char mnem = 0;
  int len = strlen (s);
  int i;
  for (i = 0; i < len; i++)
    {
      if (s[i] == '&')
	{
	  // Carefully handle &&
	  if ((i + 1) <= len && s[i + 1] == '&')
	    i++;
	  else
	    {
	      mnem = s[i + 1];
	      break;
	    }
	}
    }
  return mnem;
}

char * wxFindAccelerator (char *s)
{
// The accelerator text is after the \t char.
  while (*s && *s != '\t')
    s++;
  if (*s == '\0')
    return (NULL);
  s++;
/*
   Now we need to format it as X standard:

   input            output

   F7           --> <Key>F7
   Ctrl+N       --> Ctrl<Key>N
   Alt+k        --> Meta<Key>k
   Ctrl+Shift+A --> Ctrl Shift<Key>A

 */

  wxBuffer[0] = '\0';
  char *tmp = copystring (s);
  s = tmp;
  char *p = s;

  while (1)
    {
      while (*p && *p != '+')
	p++;
      if (*p)
	{
	  *p = '\0';
	  if (wxBuffer[0])
	    strcat (wxBuffer, " ");
	  if (strcmp (s, "Alt"))
	    strcat (wxBuffer, s);
	  else
	    strcat (wxBuffer, "Meta");
	  s = p + 1;
	  p = s;
	}
      else
	{
	  strcat (wxBuffer, "<Key>");
	  strcat (wxBuffer, s);
	  break;
	}
    }
  delete[]tmp;
  return wxBuffer;
}

XmString wxFindAcceleratorText (char *s)
{
// The accelerator text is after the \t char.
  while (*s && *s != '\t')
    s++;
  if (*s == '\0')
    return (NULL);
  s++;
  XmString text = XmStringCreateSimple (s);
  return text;
}

void wxItem::ChangeColour(void)
{
}

