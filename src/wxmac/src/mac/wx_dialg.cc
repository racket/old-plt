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

#include "wx_dialg.h"
#include "wx_panel.h"
#include "wx_utils.h"
#include "wx_messg.h"
#include "wx_main.h"
#include "wx_buttn.h"
#include "wx_macevents.h"
#ifndef WX_CARBON
# include <StandardFile.h>
# include <TextUtils.h>
# include <Strings.h>
# include <LowMem.h>
#endif

# define USE_NAVIGATION

#ifdef USE_NAVIGATION
# ifndef WX_CARBON
#  include <Navigation.h>
# endif
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
      wxPushModalWindow(ContextWindow(), cFrame);
      
      wxDispatchEventsUntil(IsUnshown, (void *)this);
      
      wxPopModalWindow(ContextWindow(), cFrame);
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
      wxWindow *win = (wxWindow *)(node->Data());
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

# define DIALOG_BORDER_STYLE wxMAXIMIZE

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
		      width, height, 
		      (style | wxMDI_CHILD 
		       | ((style & DIALOG_BORDER_STYLE) 
			  ? 0
			  : wxNO_RESIZE_BORDER)), 
		      windowName, objectType),
	  0, 0, width, height),
	  cButtonPressed (0)
{
  WXGC_IGNORE(this, cFrame);
  
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

extern int wxsMessageBox(char *message, char *caption, long style, wxWindow *parent);

//-----------------------------------------------------------------------------
// Pop up a message box
//-----------------------------------------------------------------------------
int wxMessageBox(char* message, char* caption, long style,
                 wxWindow* parent, int x, int y)
{

  return wxsMessageBox(message, caption, style, parent);
}

#ifndef OS_X
extern "C" {
#endif
  extern char *scheme_mac_spec_to_path(FSSpec *f);
  extern int scheme_mac_path_to_spec(const char *filename, FSSpec *spec);
#ifndef OS_X
}
#endif

//= T.P. ==============================================================================

static int navinited = 0;

int log_base_10(int i);

int log_base_10(int i)
{
  if (i < 10) { 
    return 1;
  } else {
    return 1 + log_base_10(i / 10);
  }
}

#ifdef OS_X
# define PATH_SEPARATOR "/"
#else
// Result from the dialog already has a separator:
# define PATH_SEPARATOR ""
#endif

static void ExtensionCallback(NavEventCallbackMessage callBackSelector, 
			      NavCBRecPtr callBackParms, 
			      void *callBackUD)
{
  /* action here */
}

static char *GetNthPath(NavReplyRecord *reply, int index)
{
  AEKeyword   theKeyword;
  DescType    actualType;
  Size        actualSize;
  FSRef	      fsref;
  OSErr       err;

  err = AEGetNthPtr(&(reply->selection), index, typeFSRef, &theKeyword, &actualType, 
		    &fsref, sizeof(fsref), &actualSize);
  if (err != noErr) {
    if (err == errAECoercionFail) {
      /* Try FSSpec: */
      FSSpec spec;

      err = AEGetNthPtr(&(reply->selection), index, typeFSS, &theKeyword, &actualType, 
			&spec, sizeof(FSSpec), &actualSize);
      if (err == noErr) {
	return scheme_mac_spec_to_path(&spec);
      }
    }
    return NULL;
  }

  return wxFSRefToPath(fsref);
}

static CFStringRef clientName = CFSTR("MrEd");
static NavEventUPP extProc = NewNavEventUPP((NavEventProcPtr)ExtensionCallback);

char *wxFileSelector(char *message, char *default_path,
                     char *default_filename, char *default_extension,
                     char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  if ((navinited >= 0) && (navinited || NavServicesAvailable())) {
    NavDialogRef outDialog;
    NavTypeListHandle openListH = NULL;
    OSErr derr;
    NavDialogCreationOptions dialogOptions;

    if (!navinited) {
      if (!NavLoad()) {
	navinited = 1;
      } else {
	navinited = -1;
	return wxFileSelector(message, default_path, default_filename,
			      default_extension, wildcard, flags,
			      parent, x, y);
      } 
    }

    NavGetDefaultDialogCreationOptions(&dialogOptions);
    if (default_filename) 
      dialogOptions.saveFileName = CFStringCreateWithCString(NULL,default_filename,CFStringGetSystemEncoding());
    if (message)
      dialogOptions.message = CFStringCreateWithCString(NULL,message,CFStringGetSystemEncoding());
    dialogOptions.modality = kWindowModalityAppModal;

    if (flags & wxBUNDLES_OK)
      dialogOptions.optionFlags |= (kNavSupportPackages | kNavAllowOpenPackages);
    if (!(flags & wxMULTIOPEN))
      dialogOptions.optionFlags -= (dialogOptions.optionFlags & kNavAllowMultipleFiles);

    // No way to set the starting directory? (There was in the old nav interface, but
    // apparently not anymore.)

    // create the dialog:
    if (flags & wxGETDIR) {
      derr = NavCreateChooseFolderDialog(&dialogOptions, extProc, NULL, NULL, &outDialog);
    } else if ((flags == 0) || (flags & wxOPEN) || (flags & wxMULTIOPEN)) {
      derr = NavCreateGetFileDialog(&dialogOptions, NULL, extProc, NULL, NULL, NULL, &outDialog);
    } else {
      derr = NavCreatePutFileDialog(&dialogOptions, 'TEXT', 'MrEd', 
				    extProc,
				    default_extension, &outDialog);
    }

    if (derr != noErr) {
      if (default_filename) 
	CFRelease(dialogOptions.saveFileName);
      if (message)
	CFRelease(dialogOptions.message);
      return NULL;
    }

    wxSetCursor(wxSTANDARD_CURSOR);
    
    // run the dialog (ApplicationModal doesn't return until user closes dialog):
    if (NavDialogRun(outDialog) != noErr) {
      if (default_filename)
        CFRelease(dialogOptions.saveFileName);
      if (message)
        CFRelease(dialogOptions.message);
      NavDialogDispose(outDialog);
      wxTheApp->AdjustCursor();
      return NULL;
    }

    wxTheApp->AdjustCursor();
    
    // dump those strings:
    if (default_filename)
      CFRelease(dialogOptions.saveFileName);
    if (message)
      CFRelease(dialogOptions.message);
    
    // did the user cancel?:
    NavUserAction action = NavDialogGetUserAction(outDialog);
    if ((action == kNavUserActionCancel) || (action == kNavUserActionNone)) {
      NavDialogDispose(outDialog);
      return NULL;
    }
    
    // get the user's reply:
    NavReplyRecord *reply = new NavReplyRecord;
    if (NavDialogGetReply(outDialog,reply) != noErr) {
      NavDialogDispose(outDialog);
      return NULL;
    }
    NavDialogDispose(outDialog);
    if (! reply->validRecord) {
      NavDisposeReply(reply);
      return NULL;
    }
    
    // parse the results
    char		*temp;

    if (flags & wxMULTIOPEN) {
      long count, index;
      char *newpath, *aggregate = "";
      OSErr err;
      
      err = AECountItems(&(reply->selection),&count);
      if (err != noErr) {
        NavDisposeReply(reply);
        return NULL;
      }
      
      for (index=1; index<=count; index++) {
	temp = GetNthPath(reply, index);
        if (temp != NULL) {
          newpath = new WXGC_ATOMIC char[strlen(aggregate) + 
                                         strlen(temp) +
                                         log_base_10(strlen(temp)) + 3];
          sprintf(newpath,"%s %ld %s",aggregate,strlen(temp),temp);
          aggregate = newpath;
        }
      }
      
      NavDisposeReply(reply);
      
      return aggregate;
    } else if ((flags & wxOPEN) || (flags & wxGETDIR)) {
      char *path;

      path = GetNthPath(reply, 1);
      NavDisposeReply(reply);
      return path;
    } else { // saving file
      int strLen = CFStringGetLength(reply->saveFileName) + 1;
      char *filename = new char[strLen];
      char *path, *wholepath;

      if (CFStringGetCString(reply->saveFileName,filename,strLen,CFStringGetSystemEncoding()) == FALSE) {
	// Unable to convert string
	NavDisposeReply(reply);
	return NULL;
      }

      path = GetNthPath(reply, 1);
      
      if (path == NULL) {
	NavDisposeReply(reply);
	return NULL;
      }
      
      wholepath = new WXGC_ATOMIC char[strlen(path) + strlen(filename) + 2];
      
      sprintf(wholepath,"%s" PATH_SEPARATOR "%s",path,filename);
      
      NavDisposeReply(reply);
      
      return wholepath;
    }
  } else
    return NULL;
}
