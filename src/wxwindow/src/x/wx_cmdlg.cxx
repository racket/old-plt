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

#ifdef wx_motif
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
#endif

#ifdef wx_xview
#include <stdio.h>

#include "wx_lbox.h"
#include "wx_txt.h"
#include "wx_buttn.h"

#include <dirent.h>
#include <unistd.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/notice.h>

#ifndef _MAXPATHLEN
#define _MAXPATHLEN 1024
#endif
char *wxXFileSelector(wxWindow *parent, char *path, char *file, char *message, int flags, char *wild_card, int x, int y);
#endif

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300


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
#if defined(wx_xview) && USE_NOTICES
  Xv_opaque win = 0;
  if (parent)
    win = (Xv_opaque)parent->handle;
  else if (wxTheApp->wx_frame)
    win = (Xv_opaque)wxTheApp->wx_frame->handle;
  else
    return wxbMessageBox(message, caption, style, parent, x, y);

  int noticeStatus = 0;
  Xv_notice notice = 0;

  // OK only.
  if (((style & wxOK) == wxOK) &&
      ((style & wxYES_NO) == 0) &&
      ((style & wxCANCEL) == 0))
  {
    notice = xv_create(win, NOTICE,
             NOTICE_MESSAGE_STRING, message,// NULL,
             NOTICE_BUTTON_YES, "OK",
             NOTICE_BLOCK_THREAD, TRUE,
             NOTICE_NO_BEEPING, TRUE,
             NOTICE_STATUS, &noticeStatus,
             NULL);
//    if (x > -1 && y > -1)
//      xv_set(notice, NOTICE_FOCUS_XY, x, y, NULL);
    xv_set(notice, XV_SHOW, TRUE, NULL);

    xv_destroy(notice);
    return wxOK;
  }
  // OK, Cancel
  else if (((style & wxOK) == wxOK) &&
      ((style & wxCANCEL) == wxCANCEL))
  {
    notice = xv_create(win, NOTICE,
             NOTICE_MESSAGE_STRING, message,// NULL,
             NOTICE_BUTTON_YES, "OK",
             NOTICE_BUTTON_NO, "Cancel",
             NOTICE_BLOCK_THREAD, TRUE,
             NOTICE_NO_BEEPING, TRUE,
             NOTICE_STATUS, &noticeStatus,
             NULL);
    if (x > -1 && y > -1)
      xv_set(notice, NOTICE_FOCUS_XY, x, y, NULL);
    xv_set(notice, XV_SHOW, TRUE, NULL);

    xv_destroy(notice);
    if (noticeStatus == NOTICE_YES)
      return wxOK;
    else
      return wxCANCEL;
  }
  // Yes, No, Cancel
  else if (((style & wxYES_NO) == wxYES_NO) &&
      ((style & wxCANCEL) == wxCANCEL))
  {
    notice = xv_create(win, NOTICE,
             NOTICE_MESSAGE_STRING, message,// NULL,
             NOTICE_BUTTON, "Yes", 100,
             NOTICE_BUTTON, "No", 101,
             NOTICE_BUTTON, "Cancel", 102,
             NOTICE_BLOCK_THREAD, TRUE,
             NOTICE_NO_BEEPING, TRUE,
             NOTICE_STATUS, &noticeStatus,
             NULL);
    if (x > -1 && y > -1)
      xv_set(notice, NOTICE_FOCUS_XY, x, y, NULL);
    xv_set(notice, XV_SHOW, TRUE, NULL);

    xv_destroy(notice);
    if (noticeStatus == 100)
      return wxYES;
    else if (noticeStatus == 101)
      return wxNO;
    else // if (noticeStatus == 102)
      return wxCANCEL;
  }
  // Yes, no
  else if ((style & wxYES_NO) == wxYES_NO)
  {
    notice = xv_create(win, NOTICE,
             NOTICE_MESSAGE_STRING, message,// NULL,
             NOTICE_BUTTON_YES, "Yes",
             NOTICE_BUTTON_NO, "No",
             NOTICE_BLOCK_THREAD, TRUE,
             NOTICE_NO_BEEPING, TRUE,
             NOTICE_STATUS, &noticeStatus,
             NULL);
    if (x > -1 && y > -1)
      xv_set(notice, NOTICE_FOCUS_XY, x, y, NULL);
    xv_set(notice, XV_SHOW, TRUE, NULL);

    xv_destroy(notice);
    if (noticeStatus == NOTICE_YES)
      return wxYES;
    else
      return wxNO;
  }
  return 0; // Make the compiler happy
#else
  return wxbMessageBox(message, caption, style, parent, x, y);
#endif
}


char *wxFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;
#ifdef wx_motif
  return wxMotifFileSelector(message, default_path, default_filename, 
                     default_extension, wildcard, flags,
                     parent, x, y);
#elif defined(wx_xview)
  char buf[_MAXPATHLEN];
  if (default_path == NULL) {
    if ((default_path = getcwd(buf, sizeof(buf)/sizeof(char)-1)) == NULL)
      default_path = ".";
  }
  if (default_filename == NULL)
    default_filename = "";

  return wxXFileSelector(parent, default_path, default_filename, message, flags, wildcard, x, y);
#else
# error "Not Yet..."
#endif
}

#ifdef wx_xview
void wxReadDir(wxListBox *filebox, wxListBox *dirbox, char *path, char *filename, DIR *dirp)
{
  filebox->Show(FALSE);
  filebox->Clear();
  dirbox->Show(FALSE);
  dirbox->Clear();
  struct dirent *dir;

  wxBeginBusyCursor(); // @@@@

  // Parent first (if not root dir)
  if (*path != '/' || *(path + 1) != '\0')
    dirbox->Append("..");

  DIR *dirstream = NULL;
  if ((dir=readdir(dirp))==NULL) {
	printf("wxWin: There has been an error reading a directory\n");
	return;
  }

  char buf[_MAXPATHLEN];

  wxStringList files;
  wxStringList dirs;

  // Used to skip the first entry since we assumed
  // it to be "." but that assumtion is not POSIX.
  do {
    // We ignore all .xx files and dirs
    if (dir->d_name[0] == '.' && filename[0] != '.') continue;

    strcpy(buf, path);
    size_t len = strlen(buf);
    if (buf[len - 1] != '/')
      buf[len++] = '/';
    strcpy(&buf[len], dir->d_name);

    // The readdir is necessary on some systems since opendir
    // can open a file successfully, but readdir fails
    // on reading from a file.
    if ((dirstream = opendir(buf)) && (readdir(dirstream))) {
      closedir(dirstream);

      if (dir->d_name[0] != '.')
        dirs.Add(dir->d_name);
    } else if (!wxIsWild(filename) || wxMatchWild(filename, dir->d_name))
      files.Add(dir->d_name);

  } while ((dir = readdir(dirp)) != NULL);

  // Sort files and dirs
  files.Sort();
  dirs.Sort();

  for(wxNode *node = files.First(); node; node = files.First()) {
    char *s = (char *)node->Data();
    filebox->Append(s);
    delete[] s;
    delete node;
  }

  for(node = dirs.First(); node; node = dirs.First()) {
    char *s = (char *)node->Data();
    dirbox->Append(s);
    delete[] s;
    delete node;
  }

  wxEndBusyCursor(); // @@@@

  filebox->Show(TRUE);
  dirbox->Show(TRUE);
}

class wxXFileSelDialog: public wxDialogBox
{
 public:
  char *wild_card;
  wxText *name_item;
  wxText *path_item;
  wxListBox *list_item;
  wxListBox *file_item;
  wxListBox *dir_item;
  wxXFileSelDialog(wxWindow *frame, char *title, Bool isModal = FALSE,
              int x = -1, int y = -1, int
              width = -1, int height = -1);
};

wxXFileSelDialog::wxXFileSelDialog(wxWindow *frame, char *title, Bool isModal,
              int x, int y, int width, int height):
  wxDialogBox(frame, title, isModal, x, y, width, height)
{
}

void wxXFileSelPath(wxText& text, wxEvent& event)
{
}

void wxStripOneDir(char *dir)
{
#if 1
  if (dir && *dir) {
    char *tcp;

    if ((tcp = strrchr(dir, '/')) == NULL)
      tcp = dir; // No slash so zap it all
    else if (tcp == dir) // Don't strip away root
      tcp++; // Protect first '/'
    *tcp = '\0'; // Cut here
  }
#else
  int len = strlen(dir);
  if (len > 1) {
    int i = len - 1;
    char ch = 0;
    while (ch != '/' && i > 0) {
      ch = dir[i];
      i --;
    }
    if (++i == 0)
      i++;
    dir[i] = 0;
  }
#endif
}


void wxXFileSelDirList(wxListBox& listbox, wxEvent& event)
{
  char *name = copystring(listbox.GetStringSelection());
  wxXFileSelDialog *dialog = (wxXFileSelDialog *)listbox.GetParent();

  char *path = copystring(dialog->path_item->GetValue());
  char buf[_MAXPATHLEN];
  strcpy(buf, path);

  if (strcmp(name, "..") == 0) {
    wxStripOneDir(buf);
  } else {
    size_t len = strlen(buf);

    if (buf[len - 1] != '/') {
      buf[len++] = '/';
      buf[len] = '\0';
    }
    strcat(buf, name);
  }

  DIR *dirstream = NULL;

  if (dirstream = opendir(buf)) {
    dialog->path_item->SetValue(buf);
    wxReadDir(dialog->file_item, &listbox, buf, dialog->name_item->GetValue(), dirstream);
    closedir(dirstream);
  } else dialog->name_item->SetValue(name);
}

void wxXFileSelFileList(wxListBox& listbox, wxEvent& event)
{
  char *name = copystring(listbox.GetStringSelection());
  wxXFileSelDialog *dialog = (wxXFileSelDialog *)listbox.GetParent();

  dialog->name_item->SetValue(name);
}

int wxXFileSelResponse = 0;
char *wxXFileSelAnswer = NULL;

// Note:
// edz 111694:
// Instead of message boxes for warning messages we should probably
// use a status line--- better HCI.  [to be done]
void wxXFileSelOK(wxButton& ok, wxEvent& event)
{
  wxXFileSelDialog *dialog = (wxXFileSelDialog *)ok.GetParent();
  char *nameval = dialog->name_item->GetValue();
  char *pathval = dialog->path_item->GetValue();
  if (wxIsWild(nameval)) {
    DIR *dirstream = NULL;
    if (dirstream = opendir(pathval))
    {
      wxReadDir(dialog->file_item, dialog->dir_item, pathval, nameval, dirstream);
      closedir(dirstream);
    }
    else
     {
	  // Zap the file/dir lists
//  	  dialog->file_item->Show (FALSE);
  	  dialog->file_item->Clear ();
//  	  dialog->dir_item->Show (FALSE);
  	  dialog->dir_item->Clear ();
	  wxMessageBox("Specified Dir not readable", wxSTR_WARNING, wxOK);
      }
    return;
  }

  char *name = copystring(nameval);
  char *path = copystring(pathval);

  char buf[_MAXPATHLEN];
  strcpy(buf, path);
  int len = strlen(buf);
  if (buf[len - 1] != '/')
    strcat(buf, "/");
  strcat(buf, name);

  delete[] name;
  delete[] path;

  if (wxXFileSelAnswer)
    delete[] wxXFileSelAnswer;

  wxXFileSelAnswer = copystring(buf);  

  dialog->Show(FALSE);
  // delete dialog;

  wxXFileSelResponse = 1;
}

void wxXFileSelCancel(wxButton& cancel, wxEvent& event)
{
  wxXFileSelDialog *dialog = (wxXFileSelDialog *)cancel.GetParent();
  wxXFileSelResponse = 0;
  dialog->Show(FALSE);
  // delete dialog;
}

char *wxXFileSelector(wxWindow *parent, char *path, char *file, char *message, int flags, char *wild_card, int x, int y)
{
  wxXFileSelResponse = 0;
  DIR *dirstream;
  char buf[_MAXPATHLEN];

  if (path == NULL) {
    if ((path = getcwd(buf, sizeof(buf)/sizeof(char)-1)) == NULL)
      path = ".";
  }

  if (file == NULL)
    file = "";

  // Bounded at (0,0)
  // A coordinate of -1 means centre on Axis!
  int realX = x < 0 ? 0 : x;
  int realY = y < 0 ? 0 : y;

  if ((dirstream=opendir(path))!=NULL) {
    // @@@ Use Title
    wxXFileSelDialog *dialog = new wxXFileSelDialog(
	parent
	, message ? message : wxSTR_FILE_SELECTOR
	, TRUE, realX, realY, 800, 800);

    dialog->SetLabelPosition(wxHORIZONTAL);

    dialog->NewLine(); // Extra space is a question of taste

    wxText *name_text = new wxText(dialog, (wxFunction)NULL, wxSTR_LABEL_FILENAME, file, -1, -1, 300, -1);

    (void)new wxButton(dialog, (wxFunction)wxXFileSelOK, wxSTR_BUTTON_OK);
    wxButton *cancelBut = new wxButton(dialog, (wxFunction)wxXFileSelCancel, wxSTR_BUTTON_CANCEL);
    cancelBut->SetDefault(); // Cancel is Default!
    dialog->NewLine();
    wxText *path_text = new wxText(dialog, (wxFunction)wxXFileSelPath, wxSTR_LABEL_PATH, path, -1, -1, 400, -1);

    dialog->NewLine();

    dialog->SetLabelPosition(wxVERTICAL);

    wxListBox *filebox = new wxListBox(dialog, (wxFunction)wxXFileSelFileList, wxSTR_LABEL_FILES, wxSINGLE,
                                       -1, -1, 250, 300);
    int fX, fY;
    filebox->GetPosition(&fX, &fY);

    wxListBox *dirbox = new wxListBox(dialog, (wxFunction)wxXFileSelDirList, wxSTR_LABEL_DIRS, wxSINGLE,
                                      265, fY, 250, 300);

    dialog->path_item = path_text;
    dialog->name_item = name_text;
    dialog->dir_item = dirbox;
    dialog->file_item = filebox;
    dialog->wild_card = wild_card;

    dialog->Fit();

    // edz: @@@ 111594 Fixed handling of default
    if ((file == NULL || file[0] == '\0') && wild_card)
      dialog->name_item->SetValue (wild_card);

    wxReadDir(filebox, dirbox, path, wild_card, dirstream);

    // Centre if position not given @@@@
    if ( x < 0  && y < 0 )
      dialog->Centre(wxBOTH);
    else if ( x == -1 )
      dialog->Centre(wxVERTICAL);
    else if ( y == -1 )
      dialog->Centre(wxHORIZONTAL);

    dialog->Show(TRUE);

    if (wxXFileSelResponse == 0)
      return NULL;
    else {
      if ((flags & wxOVERWRITE_PROMPT) && FileExists(wxXFileSelAnswer)) {
        char buf[200];
        sprintf(buf, wxSTR_OVERWRITE_FILE, wxXFileSelAnswer);
        int ans = wxMessageBox(buf, wxSTR_WARNING, wxYES_NO);
        if (ans == wxYES) {
          strcpy(wxBuffer, wxXFileSelAnswer);
          return wxBuffer;
        } else return NULL;
      } else {
        strcpy(wxBuffer, wxXFileSelAnswer);
        return wxBuffer;
      }
    }			// wxXFileSelResponse != 0
  }				// If directory could be opened
  // ERROR
  return NULL;
}
#endif

#ifdef wx_motif
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

//#if XmVersion > 1000
// I'm not sure about what you mean with XmVersion.
// If this is for Motif1.1/Motif1.2, then check XmVersion>=1200
// (Motif1.1.4 ==> XmVersion 1100 )
// Nevertheless, I put here a #define, so anyone can choose in (I)makefile...
//
#if !DEFAULT_FILE_SELECTOR_SIZE
  int width = wxFSB_WIDTH;
  int height = wxFSB_HEIGHT;
  XtVaSetValues(fileSel,
                 XmNwidth, width,
                 XmNheight, height,
                 XmNresizePolicy, XmRESIZE_NONE,
                 NULL);
#endif

  XtManageChild(fileSel);

  d->wxFileSelectorAnswer = NULL;
  d->wxFileSelectorReturned = FALSE;

  wxEndBusyCursor();

  XtAddGrab(XtParent(fileSel), TRUE, FALSE);
  XEvent event;
#if 0
  while (!wxFileSelectorReturned)
  {
    XtAppNextEvent(wxTheApp->appContext, &event);
    XtDispatchEvent(&event);
  }
#else
   wxDispatchEventsUntil(check_still_here, (void *)d);
#endif
  XtRemoveGrab(XtParent(fileSel));

  XmUpdateDisplay(wxTheApp->topLevel); // Experimental

//  XtDestroyWidget(fileSel);
  XtUnmapWidget(XtParent(fileSel));
  XtDestroyWidget(XtParent(fileSel));

  // Now process all events, because otherwise
  // this might remain on the screen
  XSync(XtDisplay(wxTheApp->topLevel), FALSE);
  while (
#if 0
	 XtAppPending(wxTheApp->appContext)
#else
	 wxTheApp->Pending()
#endif
       )
  {
    XFlush(XtDisplay(wxTheApp->topLevel));
#if 0
    XtAppNextEvent(wxTheApp->appContext, &event);
    XtDispatchEvent(&event);
#else
    wxTheApp->Dispatch();
#endif
  }

  if (d->wxFileSelectorAnswer)
  {
    strcpy(fileBuf, d->wxFileSelectorAnswer);
    return fileBuf;
  }
  else return NULL;
}

#endif
