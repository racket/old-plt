///////////////////////////////////////////////////////////////////////////////
// File:	wx_dialg.cc
// Purpose:	wxDialogBox (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

//-----------------------------------------------------------------------------
// Dialog box - like panel but doesn't need a frame, and is modal or non-modal
//-----------------------------------------------------------------------------

static const char sccsid[] = "%W% %G%";
#if 0
#ifdef GUSI
#include "TFileSpec.h"
#endif
#endif
#include "wx_dialg.h"
#include "wx_panel.h"
#include "wx_utils.h"
#include "wx_messg.h"
#include "wx_main.h"
#include "wx_buttn.h"
#include <StandardFile.h>
#include <TextUtils.h>
#include <Strings.h>
#ifdef PYLIB
extern "C" {
#include "nfullpath.h"
}
#endif

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

extern wxApp* wxTheApp;

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Other methods
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

static int IsUnshown(void *data)
{
	return !((wxDialogBox *)data)->IsShown();
}

//-----------------------------------------------------------------------------
void wxDialogBox::Show(Bool show)
{
	cFrame->Show(show);
	if (show) {
  	  if (cFrame->IsModal()) {
		wxWindow *oldm = wxGetModalWindow(ContextWindow());
		wxPutModalWindow(ContextWindow(), cFrame);
		
		wxDispatchEventsUntil(IsUnshown, (void *)this);

		wxPutModalWindow(ContextWindow(), oldm);
	  }
    }
}

Bool wxDialogBox::IsShown(void)
{
  return cFrame->IsShown();
}

void wxDialogBox::SetSize(int x, int y, int width, int height, int flags)
{
    if (!(flags & 0x70)) {
      int w, h, cw, ch;
      
      cFrame->GetSize(&w, &h);
      cFrame->GetClientSize(&cw, &ch);
      
	  cFrame->SetSize(x, y, width + (w - cw), height + (h - ch), flags);
	} else
	  wxWindow::SetSize(x, y, width, height, flags);
}


// Default resizing behaviour - if only ONE subwindow,
// resize to client rectangle size
void wxDialogBox::OnSize(int x, int y)
{
  // Search for a child which is a subwindow, not another frame.
  wxWindow *child = NULL;
  // Count the number of _subwindow_ children
  int noChildren = 0;
  for(wxChildNode *node = GetChildren()->First(); node; node = node->Next())
  {
    wxWindow *win = (wxWindow *)node->Data();
    WXTYPE winType = win->__type;

    if (wxSubType(winType, wxTYPE_PANEL) ||
        wxSubType(winType, wxTYPE_TEXT_WINDOW) ||
        wxSubType(winType, wxTYPE_CANVAS))
    {
      child = win;
      noChildren ++;
    }
  }
  if (!child || (noChildren > 1))
    return;

  int client_x, client_y;

  GetClientSize(&client_x, &client_y);
  child->SetSize(0, 0, client_x, client_y, 0x70);
}

//-----------------------------------------------------------------------------
Bool wxDialogBox::IsModal(void)
{
	return cFrame->IsModal();
}

//-----------------------------------------------------------------------------
void wxDialogBox::ShowModal(void)
{
	Show(TRUE);
	if (!cFrame->IsModal()) {
  	  while (IsShown())
		wxTheApp->MainLoop();
	}
}

void wxDialogBox::Fit(void)
{
	int x, y;

	wxPanel::Fit();
	wxPanel::GetSize(&x, &y);
	// cFrame->SetClientSize(x, y);
}

//-----------------------------------------------------------------------------
int wxDialogBox::GetButtonPressed(void)
{
	return cButtonPressed;
}

//-----------------------------------------------------------------------------
void wxDialogBox::SetButtonPressed(int buttonPressed)
{
	cButtonPressed = buttonPressed;
}

//-----------------------------------------------------------------------------
Bool wxDialogBox::OnClose(void)
{
	Bool result;
	if (IsModal())
	{
		Show(FALSE);
		result = FALSE; // don't want dialog frame deleted
	}
	else
	{
		result = TRUE;
	}

	if (result)
	  return cFrame->OnClose();

	return result;
}

//=============================================================================
//
// Message centring code
//
//=============================================================================

//-----------------------------------------------------------------------------
void wxSplitMessage(char* message, wxList* messageList, wxPanel* panel)
{
  char* copyMessage = copystring(message);
  int i = 0;
  int len = strlen(copyMessage);
  char* currentMessage = copyMessage;
  while (i < len)
  {
    while ((i < len) && (copyMessage[i] != '\n')) i ++;
    if (i < len) copyMessage[i] = 0;
    wxMessage* mess = new wxMessage(panel, currentMessage);
    messageList->Append(mess);
    panel->NewLine();

    currentMessage = copyMessage + i + 1;
  }
  delete[] copyMessage;
}

//-----------------------------------------------------------------------------
void wxCentreMessage(wxList* messageList)
{
  // Do the message centering
  wxNode* node = messageList->First();
  while (node)
  {
    wxMessage* mess = (wxMessage*)node->Data();
    mess->Centre();
    node = node->Next();
  }
}

//=============================================================================
// Public constructors
//=============================================================================

//-----------------------------------------------------------------------------
wxDialogBox::wxDialogBox // Constructor (for dialog window)
	(
		wxWindow*	parentFrame,		// this is ignored, used to be wxFrame*
		char*		windowTitle,
		Bool		modal,
		int 		x,
		int			y,
		int			width,
		int			height,
		long		style,
		char*		windowName,
		WXTYPE		objectType
	) :
		wxPanel (new wxFrame(NULL, windowTitle, 
				  x, y,
				  width, height, style, windowName, objectType),
	              0, 0, width, height),
		cButtonPressed (0)
{
  cFrame = (wxFrame *)GetParent();
  cFrame->cDialogPanel = this;
  cFrame->MakeModal(modal);
  
  /* Set dialog panel to frame's client size: */
  int w, h;
  cFrame->GetClientSize(&w, &h);
  SetSize(-1, -1, w, h, 0x70);
  
  __type = wxTYPE_DIALOG_BOX;
  
  wx_cursor = wxSTANDARD_CURSOR;
}

//=============================================================================
// Public destructor
//=============================================================================

//-----------------------------------------------------------------------------
wxDialogBox::~wxDialogBox()
{
	if (cFrame) {
		  wxTopLevelWindows(ContextWindow())->DeleteObject(cFrame);
		  cFrame = NULL;
	}
}

static wxDialogBox* wxGetDialogFromButton(wxButton& button);
static void wxOkButtonProc(wxButton& button, wxEvent& event);
static void wxCancelButtonProc(wxButton& button, wxEvent& event);
static void wxYesButtonProc(wxButton& button, wxEvent& event);
static void wxNoButtonProc(wxButton& button, wxEvent& event);

//-----------------------------------------------------------------------------
// Pop up a message box
//-----------------------------------------------------------------------------
int wxMessageBox(char* message, char* caption, long style,
                 wxWindow* parent, int x, int y)
{
	wxDialogBox* dialog = new wxDialogBox((wxFrame*)NULL, caption, TRUE, x, y);

//============================================
	wxPanel* dialogPanel = dialog;
	wxFont* theFont = new wxFont(12, wxSYSTEM, wxNORMAL, wxNORMAL);
	dialogPanel->SetLabelFont(theFont);
	dialogPanel->SetButtonFont(theFont);

//============================================
	wxPanel* messagePanel = new wxPanel(dialogPanel, -1, -1, -1, -1);
	Bool centre = ((style & wxCENTRE) == wxCENTRE);

	wxList messageList;
	wxSplitMessage(message, &messageList, messagePanel);
	messagePanel->Fit();
	if (centre)
	{
		wxCentreMessage(&messageList);
	}
	//dialogPanel->AdvanceCursor(messagePanel); // WCH: kludge

//============================================
	dialogPanel->NewLine();
	wxPanel* buttonPanel = new wxPanel(dialogPanel, -1, -1, -1, -1);

	wxButton* ok = NULL;
	wxButton* cancel = NULL;
	wxButton* yes = NULL;
	wxButton* no = NULL;

	if (style & wxYES_NO)
	{
		yes = new wxButton(buttonPanel, (wxFunction)&wxYesButtonProc, "Yes");
		no = new wxButton(buttonPanel, (wxFunction)&wxNoButtonProc, "No");
	}

	if (style & wxCANCEL)
	{
		cancel = new wxButton(buttonPanel, (wxFunction)&wxCancelButtonProc, "Cancel");
	}

	if (!(style & wxYES_NO) && (style & wxOK))
	{
		ok = new wxButton(buttonPanel, (wxFunction)&wxOkButtonProc, "OK");
	}

	if (ok)
		ok->SetDefault();
	else if (yes)
		yes->SetDefault();
	buttonPanel->Fit();
	buttonPanel->SetFocus();
	//dialogPanel->AdvanceCursor(messagePanel); // WCH: kludge

//============================================
	
	dialogPanel->Fit();
	dialog->Fit();

	messagePanel->Centre(wxHORIZONTAL);
	buttonPanel->Centre(wxHORIZONTAL);

	dialogPanel->Centre(wxBOTH);

//============================================
	
	dialog->Show(TRUE);
	int result = dialog->GetButtonPressed();
	delete dialog;

//============================================

	return result;
}

//=============================================================================
//
// Dialog button callback procedures
//
//=============================================================================

static wxDialogBox* wxGetDialogFromButton(wxButton& button)
{
	wxFrame *fr = button.GetRootFrame();
	wxDialogBox* dialog = fr->cDialogPanel;
	if (dialog == NULL) {
   		wxFatalError("No DialogBox found on RootFrame.");
   	}
   	return dialog;
}


//-----------------------------------------------------------------------------
static void wxOkButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxOK);
	dialog->Show(FALSE);
}

//-----------------------------------------------------------------------------
static void wxCancelButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxCANCEL);
	dialog->Show(FALSE);
}

//-----------------------------------------------------------------------------
static void wxYesButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxYES);
	dialog->Show(FALSE);
}

//-----------------------------------------------------------------------------
static void wxNoButtonProc(wxButton& button, wxEvent& event)
{
//	wxDialogBox* dialog = (wxDialogBox*) button.GetRootFrame();
	wxDialogBox* dialog = wxGetDialogFromButton(button);

	dialog->SetButtonPressed(wxNO);
	dialog->Show(FALSE);
}

extern "C" {
 extern char *scheme_build_mac_filename(FSSpec *f, int);
};

//= T.P. ==============================================================================
char *wxFileSelector(char *message, char *default_path,
                     char *default_filename, char *default_extension,
                     char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{	
	StandardFileReply	rep;
	SFTypeList typeList = { 'TEXT' };
	char * name;
	short numTypes = -1; // all types
	Str255	p_prompt,p_defname;
	OSErr err;

			
	if ((flags == 0) || (flags & wxOPEN))
	{	// get file
		::StandardGetFile( NULL, numTypes, typeList, &rep);	
	} else
	{	// put file
		if (message)
		{	
			strcpy((char *)p_prompt,message);
		} else 
		{
			strcpy((char *)p_prompt, "Save File Name");
		}
		C2PStr((char *)p_prompt);
		if (default_filename)
		{	strcpy((char *)p_defname, default_filename);
		} else 
		{
			strcpy((char *)p_defname, "");
		}
		C2PStr((char *)p_defname);
		::StandardPutFile( p_prompt, p_defname, &rep);
	}
	
	if (!rep.sfGood)
	  return NULL;
	
	return scheme_build_mac_filename(&rep.sfFile, 0);
}
