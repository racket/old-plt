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
      
      if ((width > -1) && (height > -1)) {
        cFrame->GetSize(&w, &h);
        cFrame->GetClientSize(&cw, &ch);
      } else
         w = h = cw = ch = 0;
      
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
				  width, height, style | wxMDI_CHILD | wxNO_RESIZE_BORDER, windowName, objectType),
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

//-----------------------------------------------------------------------------
// Pop up a message box
//-----------------------------------------------------------------------------
int wxMessageBox(char* message, char* caption, long style,
                 wxWindow* parent, int x, int y)
{

	return 0;
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
