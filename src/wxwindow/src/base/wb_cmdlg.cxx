/*
 * File:	wb_cmdlg.cc
 * Purpose:	Common dialogs: generic code
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
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
  wxListBox *listBoxItem;
  char *textAnswer;
  char *listSelection;
  char *listClientSelection;
  int listPosition;
  int buttonPressed;
  int  * listSelections;
  int     nlistSelections;

  wxMessageBoxDialog(wxWindow *parent, char *caption, Bool isModal, int x, int y,
    int w, int h, long type):
   wxDialogBox(parent, caption, isModal, x, y, w, h, type)
 {
   listBoxItem = NULL;
   buttonPressed = wxCANCEL;

   textAnswer = NULL;
   listSelection = NULL;
   listClientSelection = NULL;
   listPosition = 0;
   buttonPressed = wxCANCEL;
   listSelections = NULL;
   nlistSelections = -1;
 }
 Bool OnClose(void)
 {
   return TRUE;
 }
};

void wxDialogOkButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;

  if (dialog->listBoxItem)
    {
      if ( dialog->listBoxItem->multiple==wxSINGLE)
        {
          if (dialog->listSelection)
            delete[] dialog->listSelection;
          dialog->listSelection = 
            (dialog->listBoxItem->GetStringSelection() ?
             copystring(dialog->listBoxItem->GetStringSelection()) : NULL);
          dialog->listPosition = dialog->listBoxItem->GetSelection();
          dialog->listClientSelection = 
            dialog->listBoxItem->wxListBox::GetClientData(dialog->listPosition);
        }
      else 
        if (dialog->listBoxItem->multiple==wxMULTIPLE)
          {
            if (dialog->listSelections)
              delete[] dialog->listSelections;
            dialog->listSelections = 0;
            
            int * sels;
            dialog-> nlistSelections =
              dialog->listBoxItem->GetSelections (&sels);
            if ( dialog-> nlistSelections)
              {
                dialog->listSelections = new int [ dialog-> nlistSelections ];
                int i;
                for (i=0; i<  dialog-> nlistSelections; i++)
                  dialog->listSelections[i] = sels[i];
              }
          }
    }

  dialog->buttonPressed = wxOK;
  dialog->Show(FALSE);
  // delete dialog;
}

void wxDialogCancelButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxDialogBox *dialog = (wxDialogBox *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(dialog->__type,wxTYPE_DIALOG_BOX))
    dialog = (wxDialogBox*) ((wxPanel*)dialog)->GetParent() ;

  dialog->Show(FALSE);
  // delete dialog;
}

void wxDialogYesButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  dialog->buttonPressed = wxYES;
  dialog->Show(FALSE);
  // delete dialog;
}

void wxDialogNoButton(wxButton& but, wxEvent& WXUNUSED(event))
{
  wxPanel *panel = (wxPanel *)but.GetParent();
  // There is a possibility that buttons belong to a sub panel.
  // So, we must search the dialog.
  while (!wxSubType(panel->__type,wxTYPE_DIALOG_BOX))
    panel = (wxPanel*) panel->GetParent() ;

  wxMessageBoxDialog *dialog = (wxMessageBoxDialog *)panel;
  dialog->buttonPressed = wxNO;
  dialog->Show(FALSE);
  // delete dialog;
}

void wxDialogReturn(wxButton& but, wxEvent& event)
/* "but" isn't really a button, but it's parent is what counts */
{
  if (event.eventClass == wxEVENT_TYPE_TEXT_ENTER_COMMAND)
    wxDialogOkButton(but, event);
}


/*
 * BUGBUG Julian Smart 12/93
 * define USE_PANEL_IN_PANEL
 * if you dare use panel in panel for dialogs; I can't
 * get it to work without absolute positioning, because
 * wxItem::SetSize DOES NOT WORK (try the hello.cc About box.
 *
 */

// Return NULL if cancel pressed
char *wxGetTextFromUser(char *message, char *caption, char *default_value,
                        wxWindow *parent, int x, int y, Bool centre)
{
  return NULL;
}


char *wxGetSingleChoice(char *message, char *caption, int n, char *choices[],
                        wxWindow *parent, int x, int y, Bool centre, int width, int height)
{
  return NULL;
}

int wxGetSingleChoiceIndex(char *message, char *caption, int n, char *choices[],
                           wxWindow *parent, int x, int y, Bool centre,
                           int width, int height)
{
  return -1;
}

char *wxGetSingleChoiceData(char *message, char *caption, int n,
                            char *choices[], char *client_data[],
                            wxWindow *parent, int x, int y, Bool centre,
                            int width, int height)
{
  return NULL;
}


int wxGetMultipleChoice(char *message, char *caption,
			int n, char *choices[], 
			int nsel, int * selection,
			wxWindow *parent , int x , int y, Bool centre,
			int width, int height)
{
  return -1;
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
