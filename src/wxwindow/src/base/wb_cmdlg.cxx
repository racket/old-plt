/*
 * File:	wb_cmdlg.cc
 * Purpose:	Common dialogs: generic code
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <stdlib.h>

static char *
wxDefaultFileSelector(Bool load, const char *what, char *extension, char *default_name)
{
  char prompt[50];
  sprintf(prompt, load ? "Select file" : "Save file", what);

  if (*extension == '.') extension++;
  char wild[60];
  sprintf(wild, "*.%s", extension);

  return wxFileSelector (prompt, NULL, default_name, (char *)extension, wild);
}


// Generic file load dialog
char *
wxLoadFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(TRUE, what, extension, default_name);
}


// Generic file save dialog
char *
wxSaveFileSelector(char *what, char *extension, char *default_name)
{
  return wxDefaultFileSelector(FALSE, what, extension, default_name);
}

/*
 * Message centring code
 *
 */

void wxSplitMessage(char *message, wxList *messageList, wxPanel *panel)
{
  char *copyMessage = copystring(message);
  size_t i = 0;
  size_t len = strlen(copyMessage);
  char *currentMessage = copyMessage;

  while (i < len) {
    while ((i < len) && (copyMessage[i] != '\n')) i++;
    if (i < len) copyMessage[i] = 0;
    wxMessage *mess = new wxMessage(panel, currentMessage);
    messageList->Append(mess);
    panel->NewLine();

    currentMessage = copyMessage + i + 1;
  }
  delete[] copyMessage;
}

void wxCentreMessage(wxList *messageList)
{
  // Do the message centering
  for(wxNode *node = messageList->First(); node; node = node->Next()) {
    wxMessage *mess = (wxMessage *)node->Data();
    mess->Centre();
  }
}


/*
 * A general purpose dialog box with an OnClose that returns TRUE.
 *
 */

class wxMessageBoxDialog: public wxDialogBox
{
 public:
  int buttonPressed;

  wxMessageBoxDialog(wxWindow *parent, char *caption, Bool isModal, int x, int y,
    int w, int h, long type):
   wxDialogBox(parent, caption, isModal, x, y, w, h, type)
 {
   buttonPressed = wxCANCEL;
 }
 Bool OnClose(void)
 {
   return TRUE;
 }
};

void wxDialogOkButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)but.GetParent();

  dialog->buttonPressed = wxOK;
  dialog->Show(FALSE);
}

void wxDialogCancelButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxDialogBox *dialog = (wxDialogBox *)but.GetParent();

  dialog->Show(FALSE);
}

void wxDialogYesButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)but.GetParent();
  dialog->buttonPressed = wxYES;
  dialog->Show(FALSE);
}

void wxDialogNoButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)but.GetParent();
  dialog->buttonPressed = wxNO;
  dialog->Show(FALSE);
}

// Pop up a message box: generic version used by X.
int wxbMessageBox(char *message, char *caption, long type,
                 wxWindow *parent, int x, int y)
{
  wxBeginBusyCursor();

  wxMessageBoxDialog *dialog = new wxMessageBoxDialog(parent, caption, TRUE, x, y, 1000, 1000, 0);

  Bool centre = ((type & wxCENTRE) == wxCENTRE);

  wxList *messageList = new wxList();
  wxSplitMessage(message, messageList, dialog);

  dialog->NewLine();

  // Create Buttons in a sub-panel, so they can be centered.
  wxPanel *but_panel = dialog ;

  wxButton *ok = NULL;
  wxButton *cancel = NULL;
  wxButton *yes = NULL;
  wxButton *no = NULL;

  if (type & wxYES_NO) {
    yes = new wxButton(but_panel, (wxFunction)&wxDialogYesButton, "Yes");
    no = new wxButton(but_panel, (wxFunction)&wxDialogNoButton, "No");
  }

  if (type & wxOK) {
    ok = new wxButton(but_panel, (wxFunction)&wxDialogOkButton, "Ok");
  }

  if (type & wxCANCEL) {
    cancel = new wxButton(but_panel, (wxFunction)&wxDialogCancelButton, "Cancel");
  }

  if (ok)
  {
    ok->SetDefault();
    ok->SetFocus();
  }
  else if (yes)
  {
    yes->SetDefault();
    yes->SetFocus();
  }

  dialog->Fit();

  // Do the message centering
  if (centre)
    wxCentreMessage(messageList);

  if ((x < 0) && (y < 0))
    dialog->Centre(wxBOTH);
  else if (x < 0)
    dialog->Centre(wxHORIZONTAL);
  else if (y < 0)
    dialog->Centre(wxVERTICAL);

  wxEndBusyCursor();
  dialog->Show(TRUE);

  return dialog->buttonPressed;
}
