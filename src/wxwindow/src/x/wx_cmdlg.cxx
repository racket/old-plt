/*
 * File:	wx_cmdlg.cc
 * Purpose:	Common dialogs: X implementation
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 */

static const char sccsid[] = "%W% %G%";

#ifdef __GNUG__
#pragma implementation "wx_cmdlg.h"
#endif

#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "common.h"
#include "wx_gdi.h"
#include "wx_utils.h"
#include "wx_types.h"
#include "wx_frame.h"
#include "wx_main.h"
#include "wx_dialg.h"

#include "wx.h"

#include "wx_cmdlg.h"
#include <stdlib.h>

#include <Xm/MwmUtil.h>
#include <Xm/Label.h>
#include <Xm/BulletinB.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/DialogS.h>
#include <Xm/FileSB.h>
#include <Xm/RowColumn.h>
#include <Xm/LabelG.h>
char *wxMotifFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y);


/*
 * This common dialog code was formerly in wx_dialg.cc.
 */

/*
 * Common dialogs code
 *
 */

// Pop up a message box
int wxMessageBox(char *message, char *caption, long style,
                 wxWindow *parent, int x, int y)
{
  return wxbMessageBox(message, caption, style, parent, x, y);
}


char *wxFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  return wxMotifFileSelector(message, default_path, default_filename, 
                     default_extension, wildcard, flags,
                     parent, x, y);
}

typedef struct {
  char *wxFileSelectorAnswer;
  Bool wxFileSelectorReturned;
} FileSelData;

void wxFileSelCancel(Widget fs, XtPointer client_data, XmFileSelectionBoxCallbackStruct *cbs)
{
  FileSelData *d = (FileSelData *)client_data;

  d->wxFileSelectorAnswer = NULL;
  d->wxFileSelectorReturned = TRUE;
}

void wxFileSelOk(Widget fs, XtPointer client_data, XmFileSelectionBoxCallbackStruct *cbs)
{
  FileSelData *d = (FileSelData *)client_data;

  char *filename = NULL;
  if (!XmStringGetLtoR(cbs->value, XmSTRING_DEFAULT_CHARSET, &filename)) {
    d->wxFileSelectorAnswer = NULL;
    d->wxFileSelectorReturned = TRUE;
  } else {
    if (filename) {
      if (d->wxFileSelectorAnswer) delete[] d->wxFileSelectorAnswer;
      d->wxFileSelectorAnswer = copystring(filename);
      XtFree(filename);
    }
    d->wxFileSelectorReturned = TRUE;
  }
}

extern void wxDispatchEventsUntil(int (*f)(void *), void *data);

static int check_still_here(void *client_data)
{
  FileSelData *d = (FileSelData *)client_data;

  return d->wxFileSelectorReturned;
}

char *wxMotifFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  wxBeginBusyCursor();
  static char fileBuf[512];
  FileSelData *d;
  Widget parentWidget = 0;
  if (parent)
  {
    if (wxSubType(parent->__type, wxTYPE_FRAME))
      parentWidget = ((wxFrame *)parent)->frameShell;
    else if (wxSubType(parent->__type, wxTYPE_DIALOG_BOX))
      parentWidget = ((wxDialogBox *)parent)->dialogShell;
    else
      parentWidget = (Widget)parent->handle;
  }
  else if (wxTheApp->wx_frame)
    parentWidget = wxTheApp->wx_frame->frameShell;

  Widget fileSel = XmCreateFileSelectionDialog(parentWidget, "file_selector", NULL, 0);
  XtUnmanageChild(XmFileSelectionBoxGetChild(fileSel, XmDIALOG_HELP_BUTTON));

  Widget shell = XtParent(fileSel);

  if (message)
    XtVaSetValues(shell, XmNtitle, message, NULL);

  char *entirePath = NULL;

  if (default_path && default_filename)
  {
    sprintf(wxBuffer, "%s/%s", default_path, default_filename);
    entirePath = copystring(wxBuffer);
  }
  else if (default_path && !default_filename)
  {
    sprintf(wxBuffer, "%s/", default_path);
    entirePath = copystring(wxBuffer);
  }
  else if ((!default_path) && default_filename)
  {
    sprintf(wxBuffer, "%s", default_filename);
    entirePath = copystring(wxBuffer);
  }

  if (entirePath)
  {
    Widget selectionWidget = XmFileSelectionBoxGetChild(fileSel, XmDIALOG_TEXT);
    XmTextSetString(selectionWidget, entirePath);
    delete[] entirePath;
  }

  if (wildcard)
  {
    if (default_path)
      sprintf(wxBuffer, "%s/%s", default_path, wildcard);
    else
      sprintf(wxBuffer, "%s", wildcard);

    Widget filterWidget = XmFileSelectionBoxGetChild(fileSel, XmDIALOG_FILTER_TEXT);
    XmTextSetString(filterWidget, wxBuffer);
    XmFileSelectionDoSearch(fileSel, NULL);
  }

  d = new FileSelData;

  XtAddCallback(fileSel, XmNcancelCallback, (XtCallbackProc)wxFileSelCancel, (XtPointer)d);
  XtAddCallback(fileSel, XmNokCallback, (XtCallbackProc)wxFileSelOk, (XtPointer)d);

  XtManageChild(fileSel);

  d->wxFileSelectorAnswer = NULL;
  d->wxFileSelectorReturned = FALSE;

  wxEndBusyCursor();

  XtAddGrab(XtParent(fileSel), TRUE, FALSE);
  XEvent event;
  wxDispatchEventsUntil(check_still_here, (void *)d);
  XtRemoveGrab(XtParent(fileSel));

  XmUpdateDisplay(wxTheApp->topLevel); // Experimental

  //  XtDestroyWidget(fileSel);
  XtUnmapWidget(XtParent(fileSel));
  XtDestroyWidget(XtParent(fileSel));

  // Now process all events, because otherwise
  // this might remain on the screen
  XSync(XtDisplay(wxTheApp->topLevel), FALSE);
  while (wxTheApp->Pending())
  {
    XFlush(XtDisplay(wxTheApp->topLevel));
    wxTheApp->Dispatch();
  }

  if (d->wxFileSelectorAnswer) {
    strcpy(fileBuf, d->wxFileSelectorAnswer);
    return fileBuf;
  } else
    return NULL;
}
