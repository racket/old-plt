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

#if 0
#ifdef powerc
# define USE_NAVIGATION
#endif
#endif

#ifdef USE_NAVIGATION
# include <Navigation.h>
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
  WXGC_IGNORE(cFrame);
  
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

extern "C" {
 extern char *scheme_build_mac_filename(FSSpec *f, int);
 extern int scheme_mac_path_to_spec(const char *filename, FSSpec *spec, long *type);
};

//= T.P. ==============================================================================

#ifdef USE_NAVIGATION

static int navinited = 0;

extern void QueueMrEdEvent(EventRecord *e);

static pascal void EventFilter(NavEventCallbackMessage callBackSelector,
                               NavCBRecPtr callBackParms, 
                               void *callBackUD)
{
  /* Essentially copied from Inside Macintosh: */
    switch (callBackSelector)
    {
        case kNavCBEvent:
            QueueMrEdEvent(callBackParms->eventData.event);
            break;
    }
}

#endif

char *wxFileSelector(char *message, char *default_path,
                     char *default_filename, char *default_extension,
                     char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{	
#ifdef USE_NAVIGATION
  if ((navinited >= 0) && (navinited || NavServicesAvailable())) {
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
    
    NavDialogOptions *dialogOptions = new NavDialogOptions;
    NavGetDefaultDialogOptions(dialogOptions);
    
    dialogOptions->dialogOptionFlags ^= kNavAllowPreviews;

    if (default_filename) {
      int len = strlen(default_filename);
      if (len > 64) len = 64;     
      dialogOptions->savedFileName[0] = len;
      memcpy(dialogOptions->savedFileName + 1, default_filename, len);
    }
    
    if (message) {
      int len = strlen(message);
      if (len > 255) len = 255;
      dialogOptions->message[0] = len;
      memcpy(dialogOptions->message + 1, message, len);
    }
      
    NavReplyRecord *reply = new NavReplyRecord;
    AEDesc *startp = NULL;
    OSErr err;
    FSSpec fsspec;
    long type;
    
    if (default_path && *default_path) {
      int len = strlen(default_path);
      char *s = new char[len + 3];
      memcpy(s, default_path, len);
      if (s[len - 1] != ':')
        s[len++] = ':';
      s[len] = 0;
      
      if (scheme_mac_path_to_spec(s, &fsspec, &type)) {
        /* Get name of dir: */
        CInfoPBRec pbrec;
        pbrec.hFileInfo.ioNamePtr = (unsigned char *)&(fsspec.name);
	pbrec.hFileInfo.ioVRefNum = fsspec.vRefNum;
	pbrec.hFileInfo.ioDirID = fsspec.parID;
	pbrec.hFileInfo.ioFDirIndex = -1;
	if (!PBGetCatInfo(&pbrec, 0)) {	
	  fsspec.parID = pbrec.dirInfo.ioDrParID;
          startp = new AEDesc;
          if (AECreateDesc(typeFSS, &fsspec, sizeof(fsspec),  startp))
            startp = NULL;
         }
      }
    }
    
    NavEventUPP   eventProc = NewNavEventProc(EventFilter);
    
    if ((flags == 0) || (flags & wxOPEN))
      err = NavChooseFile(startp, reply, dialogOptions,
                       eventProc,
                       NULL, NULL, NULL, NULL);
    else {
      err = NavPutFile(startp, reply, dialogOptions,
                       eventProc,
                       'TEXT',
                       'MrEd',
                       NULL);
      if (!err && reply->validRecord)
        NavCompleteSave(reply, kNavTranslateInPlace);
    }
    
    DisposeRoutineDescriptor(eventProc);
    
    if (!reply->validRecord) {
      err = 1;
      NavDisposeReply(reply);
    }
      
    if (startp)
      AEDisposeDesc(startp);
      
    if (!err) {
        AEKeyword   theKeyword;
        DescType    actualType;
        Size        actualSize;
                        
        AEGetNthPtr(&(reply->selection), 1,
                            typeFSS, &theKeyword,
                             &actualType, &fsspec,
                             sizeof(fsspec),
                             &actualSize);
       
        NavDisposeReply(reply);
        
        return scheme_build_mac_filename(&fsspec, 0);
     } else 
       return NULL;
  } else {
#endif
	StandardFileReply	rep;
	SFTypeList typeList = { 'TEXT' };
	char * name;
	short numTypes = -1; // all types
	Str255	p_prompt,p_defname;
	OSErr err;


	if (default_path) {
	   FSSpec sp;
	   if (scheme_mac_path_to_spec(default_path, &sp, NULL)) {
	     LMSetCurDirStore(sp.parID);
	     LMSetSFSaveDisk(-sp.vRefNum);
	   }
	}
	
			
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
#ifdef USE_NAVIGATION
  }
#endif
}
