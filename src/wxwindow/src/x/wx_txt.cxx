/*
 * File:        wx_txt.cc
 * Purpose:     Panel items implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	April 1995
 * Copyright:   (c) 1995, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_txt.h"
#endif

#include <stdlib.h>
#include "common.h"
#include "wx_utils.h"
#include "wx_privt.h"
#include "wx_txt.h"

#include <X11/keysym.h>

#ifdef wx_motif
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/Text.h>
#include <Xm/Form.h>

void 
wxTextModifyProc (Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs);
#endif

#define LABEL_OFFSET_PIXELS 4

static Bool checkFunctionKey(wxPanel *panelPtr, wxItem *itemPtr,
		XEvent *event)
{
	if (event->type == KeyPress) {

		KeySym keySym;
		XComposeStatus compose;
		XLookupString((XKeyEvent *) event, wxBuffer, 20,
				&keySym, &compose);

		int id = CharCodeXToWX(keySym);

		wxKeyEvent *_event = new wxKeyEvent(wxEVENT_TYPE_CHAR);
		wxKeyEvent &wxevent = *_event;
      
		if (event->xkey.state & ShiftMask)
			wxevent.shiftDown = TRUE;
		if (event->xkey.state & ControlMask)
			wxevent.controlDown = TRUE;
		wxevent.eventObject = panelPtr;
		wxevent.keyCode = id;
        
		if (IsCursorKey(keySym) || IsPFKey(keySym) ||
				IsFunctionKey(keySym) || IsMiscFunctionKey(keySym) ||
				IsKeypadKey(keySym)) {
			if (panelPtr->GetEventHandler()->OnFunctionKey(wxevent))
				return TRUE;
		}
		if (!itemPtr->CallPreOnChar(itemPtr, &wxevent))
		  itemPtr->GetEventHandler()->OnChar(wxevent);
		return TRUE;
	}
	return FALSE;
}

void clientMsgHandler(Widget w, XtPointer cdata, XEvent *event)
{
	if (event->type != ClientMessage)
		return;

	// toDo: use memcpy to retrieve the pointers (using the '.b' part)
	wxItem *item1 = (wxItem *) event->xclient.data.l[0];
	wxItem *item2 = (wxItem *) event->xclient.data.l[1];
	wxPanel *panel = (wxPanel *) item1->GetParent();
	panel->GetEventHandler()->OnChangeFocus(item1, item2);
}

void keyPressHandler(Widget w, XtPointer cData, XEvent *event)
{
	wxText *item = (wxText *) cData;
	wxPanel *panel = (wxPanel *) item->GetParent();
	checkFunctionKey(panel, item, event);
}

void wxTextCallback(Widget w, XtPointer clientData,
                      XmAnyCallbackStruct *ptr)
{
  wxText *item = (wxText *)clientData;
  int type_event ;
  // sorry... but it's the only way we can get to the panel!
  wxPanel *panel = (wxPanel *) item->GetParent();
  if (panel->manualChange)
	return;

  switch (ptr->reason) {
  case XmCR_LOSING_FOCUS:
	panel->previousFocus = item;
	return;
  case XmCR_FOCUS:
/* THIS CODE CAUSES A CRASH in clientMsgHandler for more than 1
   text control. So for now, we're commenting it out. JACS 17/5/95
   
  	if (panel->previousFocus != NULL && panel->previousFocus != item) {
  		wxItem *item1 = panel->previousFocus;
  		wxItem *item2 = item;

	  	XClientMessageEvent ev;
	  	ev.display = XtDisplay(Widget(panel->handle));
	  	ev.window = XtWindow(Widget(panel->handle));
	  	ev.type = ClientMessage;
	  	ev.format = 32;
#if XmVersion > 1100
	  	ev.message_type = XA_INTEGER;
#endif
	  	ev.data.l[0] = (long) item1;
	  	ev.data.l[1] = (long) item2;
                // Surely this is static for all text items and will
                // only get added to the first created??? - JACS
	  	static Bool addedClientMsgHandler = FALSE;
	  	if (!addedClientMsgHandler) {
		  	XtAddEventHandler(Widget(panel->handle), NoEventMask, TRUE,
		  			(XtEventHandler) clientMsgHandler, NULL);
		  	addedClientMsgHandler = TRUE;
		}
	  	XSendEvent(XtDisplay((Widget) panel->handle),
	  			XtWindow((Widget) panel->handle), TRUE, 0, (XEvent *) &ev);
	}
*/
	return;
  case XmCR_ACTIVATE:
    if (item->GetWindowStyleFlag() & wxPROCESS_ENTER)
      type_event = wxEVENT_TYPE_TEXT_ENTER_COMMAND ;
    else {
      ((wxPanel *)item->GetParent())->OnDefaultAction(item);
      return;
    }
    break;
  default:
    type_event = wxEVENT_TYPE_TEXT_COMMAND ;
    break;
  }
  wxCommandEvent *event  = new wxCommandEvent(type_event);
  event->commandString = item->GetValue();
  event->eventObject = item;
  item->ProcessCommand(*event);
}


// Text item
IMPLEMENT_DYNAMIC_CLASS(wxText, wxItem)

wxText::wxText (void)
{
  internalTextValue = NULL;
}

wxText::wxText (wxPanel * panel, wxFunction Function, char *label, char *value,
	int x, int y, int width, int height, long style, char *name):
wxbText (panel, Function, label, value, x, y, width, height, style, name)
{
  Create (panel, Function, label, value, x, y, width, height, style, name);
}

Bool wxText::
Create (wxPanel * panel, wxFunction Function, char *label, char *value,
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
  labelPosition = panel->label_position;
  windowStyle = style;
  canAddEventHandler = TRUE;
  windowName = copystring (name);
  internalTextValue = NULL;
  Widget panelForm = panel->panelWidget;
  formWidget = XtVaCreateManagedWidget (windowName,
					xmFormWidgetClass, panelForm,
					XmNmarginHeight, 0,
					XmNmarginWidth, 0,
					NULL);

  Widget textWidget;

  if (buttonFont)
    textWidget = XtVaCreateManagedWidget ("text",
					  xmTextWidgetClass, formWidget,
					  XmNfontList,  
					  /* MATTHEW: [4] Provide display */
					  buttonFont->GetInternalFont(XtDisplay(formWidget)),
					  NULL);
  else
    textWidget = XtVaCreateManagedWidget ("text",
					  xmTextWidgetClass, formWidget,
					  NULL);

  int noCols = 10;
  if (value && (strlen (value) > noCols))
    noCols = strlen (value);

  /* What??? */
  if (noCols <= 0)
    noCols = 1;

  XtVaSetValues (textWidget,
		 XmNcolumns, noCols,
		 NULL);

  if (label)
    {
      char *the_label = (style & wxFIXED_LENGTH) ? fillCopy (label) : copystring (label);
      the_label = wxStripMenuCodes(the_label);
      XmString text = XmStringCreateSimple (the_label);
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
		       labelFont->GetInternalFont (XtDisplay(formWidget)), /* MATTHEW: [5] use formWidget */
		       NULL);

      XmStringFree (text);
      delete[]the_label;
    }

  if (panel->label_position == wxHORIZONTAL)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_FORM,
		       XmNleftAttachment, XmATTACH_FORM,
		       XmNbottomAttachment, XmATTACH_FORM,
		       XmNalignment, XmALIGNMENT_BEGINNING,
		       NULL);

      XtVaSetValues (textWidget,
		     XmNleftOffset, LABEL_OFFSET_PIXELS,
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

      XtVaSetValues (textWidget,
		     XmNtopAttachment, labelWidget ? XmATTACH_WIDGET : XmATTACH_FORM,
		     XmNtopWidget, labelWidget ? labelWidget : formWidget,
		     XmNbottomAttachment, XmATTACH_FORM,
		     XmNleftAttachment, XmATTACH_FORM,
		     XmNrightAttachment, XmATTACH_FORM,
		     XmNalignment, XmALIGNMENT_BEGINNING,
		     NULL);
    }

  handle = (char *) textWidget;

  if (windowStyle & wxREADONLY)
    XmTextSetEditable(textWidget, False);

  XtAddCallback(textWidget, XmNfocusCallback, (XtCallbackProc)wxTextCallback,
                  (XtPointer)this);
  XtAddCallback(textWidget, XmNlosingFocusCallback, (XtCallbackProc)wxTextCallback,
                  (XtPointer)this);
  XtAddCallback (textWidget, XmNactivateCallback, (XtCallbackProc) wxTextCallback,
		 (XtPointer) this);
  XtAddCallback (textWidget, XmNvalueChangedCallback, (XtCallbackProc) wxTextCallback,
		 (XtPointer) this);
  XtAddCallback(textWidget, XmNmodifyVerifyCallback, (XtCallbackProc)wxTextModifyProc, (XtPointer)this);


  panel->AttachWidget (this, formWidget, x, y, width, height);
  ChangeColour ();

  if (value)
    XmTextSetString (textWidget, value);

  /* After creating widgets, no more resizes. */
  if (style & wxFIXED_LENGTH)
    {
      if (labelWidget)
	XtVaSetValues (labelWidget,
		       XmNtopAttachment, XmATTACH_SELF,
		       XmNleftAttachment, XmATTACH_SELF,
		       NULL);

      XtVaSetValues (textWidget,
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
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);

  Callback (Function);

  return TRUE;
}

wxText::~wxText (void)
{
  if (internalTextValue)
    delete[] internalTextValue;
}

void wxText::ChangeColour (void)
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

char *wxText::GetValue (void)
{
  // If a password widget, return the REAL
  // string value, not a useless string of asterisks.
  if (internalTextValue)
    return internalTextValue;

  char *s = XmTextGetString ((Widget) handle);
  if (s)
    {
      strcpy (wxBuffer, s);
      XtFree (s);
      return wxBuffer;
    }
  else
    return NULL;
}

void wxText::SetValue (char *value)
{
  if (value)
    XmTextSetString ((Widget) handle, value);
}

void wxText::SetSize (int x, int y, int width, int height, int sizeFlags)
{
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_ANY, NULL);
  Bool isShow = XtIsManaged(formWidget);

  int pw, ph;
  GetParent()->GetSize(&pw, &ph);

  if (isShow)
    XtUnmanageChild (formWidget);

  if (x > -1)
    XtVaSetValues (formWidget, XmNleftAttachment, XmATTACH_SELF,
		   XmNx, x,
		   NULL);
  if (y > -1)
    XtVaSetValues (formWidget, XmNtopAttachment, XmATTACH_SELF,
		   XmNy, y,
		   NULL);

  Dimension labelWidth = 0, labelHeight = 0;
  int actualWidth = width, actualHeight = height;

  if (labelWidget)
    XtVaGetValues (labelWidget, XmNwidth, &labelWidth, XmNheight, &labelHeight, NULL);

  if (itemOrientation == wxHORIZONTAL)
    {
      actualWidth = width - labelWidth - LABEL_OFFSET_PIXELS;
      actualHeight = height;
    }
  else
    {
      actualWidth = width;
      actualHeight = height - labelHeight;
    }

  if (width > -1)
    XtVaSetValues ((Widget) handle, XmNwidth, max(actualWidth, 10),
		   NULL);
  if (height > -1)
    XtVaSetValues ((Widget) handle, XmNheight, max(actualHeight, 10),
		   NULL);

  if (isShow)
    XtManageChild (formWidget);
  XtVaSetValues(formWidget, XmNresizePolicy, XmRESIZE_NONE, NULL);

  GetParent()->SetSize(-1, -1, pw, ph, 0x100);

  int ww, hh;
  GetSize(&ww, &hh);
  sr_width = ww;
  sr_height = hh;
  GetEventHandler()->OnSize(ww, hh);
}

void wxText::SetFocus (void)
{
  wxPanel *panel = (wxPanel *) GetParent();
  panel->manualChange = TRUE;
  wxItem::SetFocus();
  panel->manualChange = FALSE;
}

// Clipboard operations
void wxText::Copy(void)
{
  XmTextCopy((Widget)handle, CurrentTime);
}

void wxText::Cut(void)
{
  XmTextCut((Widget)handle, CurrentTime);
}

void wxText::Paste(void)
{
  XmTextPaste((Widget)handle);
}

void wxText::SetEditable(Bool editable)
{
  Widget textWidget = (Widget)handle;
  XmTextSetEditable(textWidget, editable);
}

void 
wxTextModifyProc (Widget w, XtPointer clientData, XmTextVerifyCallbackStruct *cbs)
{
  wxText *tw = (wxText *) clientData;

  if ((tw->GetWindowStyleFlag() & wxPASSWORD) != wxPASSWORD)
    return;

#if 0  // _sm_ -- Broken original.
  if (cbs->text->ptr == NULL)
  {
    // Backspace
    if (tw->internalTextValue)
    {
      cbs->endPos = strlen(tw->internalTextValue);
      tw->internalTextValue[cbs->startPos] = 0;
      return;
    }
    else return;
  }

  char *newS = new char[cbs->endPos + 2]; // new char + NULL
  if (tw->internalTextValue)
  {
    strcpy(newS, tw->internalTextValue);
    delete[] tw->internalTextValue;
  }
  else newS[0] = 0;

  tw->internalTextValue = newS;
  strncat(tw->internalTextValue, cbs->text->ptr, cbs->text->length);
  tw->internalTextValue[cbs->endPos + cbs->text->length] = 0;

  int i;
  for (i = 0; i < cbs->text->length; i ++)
    cbs->text->ptr[i] = '*';
#else  // _sm_ My stab at it.
  /*
   * The preceding code has a number of serious bugs, inherited from the
   * original code in The Motif Programming Manual vol 6 (by Dan Heller).
   *
   * At least on my system (SunOS 4.1.3 + Motif 1.2), you need to think of
   * every event as a replace event.  cbs->text->ptr gives the replacement
   * text, cbs->startPos gives the index of the first char affected by the
   * replace, and cbs->endPos gives the index one more than the last char
   * affected by the replace (startPos == endPos implies an empty range).
   * Hence, a deletion is represented by replacing all input text with a
   * blank string ("", *not* NULL!).  A simple insertion that does not
   * overwrite any text has startPos == endPos.
   */

  char * & passwd = tw->internalTextValue;  // Set up a more convenient alias.

  if (!passwd)
    passwd = copystring(cbs->text->ptr);
  else
  {
    int len = passwd ? strlen(passwd) : 0; // Enough room for old text
    len += strlen(cbs->text->ptr) + 1;     // + new text (if any) + NUL
    len -= cbs->endPos - cbs->startPos;    // - text from affected region.

    char * newS = new char [len];
    char * p = passwd, * dest = newS, * insert = cbs->text->ptr;

    // Copy (old) text from passwd, up to the start posn of the change.
    int i;
    for (i = 0; i < cbs->startPos; ++i)
      *dest++ = *p++;

    // Copy the text to be inserted).
    while (*insert)
      *dest++ = *insert++;

    // Finally, copy into newS any remaining text from passwd[endPos] on.
    for (p = passwd + cbs->endPos; *p; )
      *dest++ = *p++;
    *dest = 0;

    delete [] passwd;  passwd = newS;
  }

  // Overwrite text with asterisks.  By the way, since we need "i" outside the
  // for-loop, ISO C++ now demands that it be declared outside the for-loop.
  // To check your code for these cases, you can
  //   #define for  if (0); else for
  // This makes your code run the same under both the old and new scoping rules.
  int i;
  for (i = 0; i < cbs->text->length; ++i)
    cbs->text->ptr[i] = '*';
  cbs->text->ptr[i] = 0;
#endif
}

