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
#include "wx_macevents.h"
#ifndef OS_X
  #include <StandardFile.h>
  #include <TextUtils.h>
  #include <Strings.h>
  #include <LowMem.h>
#endif
#ifdef PYLIB
extern "C" {
#include "nfullpath.h"
}
#endif

# define USE_NAVIGATION

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

/* we no longer ignore the border style on dialogs */
#if 1
# define DIALOG_BORDER_STYLE wxMAXIMIZE
#else
# define DIALOG_BORDER_STYLE 0
#endif

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

extern "C" {
 extern char *scheme_mac_spec_to_path(FSSpec *f);
 extern int scheme_mac_path_to_spec(const char *filename, FSSpec *spec);
}

//= T.P. ==============================================================================

#ifdef USE_NAVIGATION

static int navinited = 0;

#ifndef OS_X
extern void QueueMrEdEvent(EventRecord *e);
#endif

static pascal void EventFilter(NavEventCallbackMessage callBackSelector,
                               NavCBRecPtr callBackParms, 
                               void *callBackUD)
{
#ifndef OS_X
  /* Essentially copied from Inside Macintosh: */
    switch (callBackSelector)
    {
        // never happens in nav services 3.0?:
        case kNavCBEvent:
        	switch (((callBackParms->eventData).eventDataParms.event)->what) {
        		case updateEvt:
		            QueueMrEdEvent(callBackParms->eventData.eventDataParms.event);
		            break;
		    }
            break;
    }
#endif            
}

#endif

int log_base_10(int i);

int log_base_10(int i)
{
  if (i < 10) { 
    return 1;
  } else {
    return 1 + log_base_10(i / 10);
  }
}

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

    NavEventUPP   eventProc = NewNavEventUPP(EventFilter);
    
#ifdef OS_X // Use Navigation Services 3.0
    static CFStringRef clientName = CFSTR("MrEd");

    NavDialogCreationOptions dialogOptions;
    NavGetDefaultDialogCreationOptions(&dialogOptions);
    
    if (default_filename) 
        dialogOptions.saveFileName = CFStringCreateWithCString(NULL,default_filename,CFStringGetSystemEncoding());
    if (message)
        dialogOptions.message = CFStringCreateWithCString(NULL,message,CFStringGetSystemEncoding());
    dialogOptions.modality = kWindowModalityAppModal;
    
    NavDialogRef outDialog;
    
    // looks like there's no way to specify a default directory to start in...
 
    // create the dialog:
    if ((flags == 0) || (flags & wxOPEN) || (flags & wxMULTIOPEN)) {
      if (NavCreateGetFileDialog(&dialogOptions,NULL,eventProc,NULL,NULL,NULL,&outDialog) != noErr) {
        if (default_filename) 
          CFRelease(dialogOptions.saveFileName);
        if (message)
          CFRelease(dialogOptions.message);
        return NULL;
      }
    } else {
      if (NavCreatePutFileDialog(&dialogOptions,'TEXT','MrEd',eventProc,NULL,&outDialog) != noErr) {
        if (default_filename)
          CFRelease(dialogOptions.saveFileName);
        if (message)
          CFRelease(dialogOptions.message);
      }
    }
    
    // run the dialog (ApplicationModal doesn't return until user closes dialog):
    if (NavDialogRun(outDialog) != noErr) {
      if (default_filename)
        CFRelease(dialogOptions.saveFileName);
      if (message)
        CFRelease(dialogOptions.message);
      NavDialogDispose(outDialog);
      return NULL;
    }
    
    // dump those strings:
    if (default_filename)
      CFRelease(dialogOptions.saveFileName);
    if (message)
      CFRelease(dialogOptions.message);
    DisposeNavEventUPP(eventProc);
    
    // did the user cancel?:
    NavUserAction action = NavDialogGetUserAction(outDialog);
    if ((action == kNavUserActionCancel) || (action == kNavUserActionNone)) {
      NavDialogDispose(outDialog);
      return NULL;
    }
    
    // get the user's reply:
    NavReplyRecord *reply = new NavReplyRecord;
    if (NavDialogGetReply(outDialog,reply) != noErr) {
      //TEMPORARY
      fprintf(stderr,"NavDialogGetReply failed.\n");
      NavDialogDispose(outDialog);
      return NULL;
    }
    NavDialogDispose(outDialog);
    if (! reply->validRecord) {
      //TEMPORARY
      fprintf(stderr,"reply->validRecord is FALSE.\n");
      NavDisposeReply(reply);
      return NULL;
    }
    
    // parse the results
    AEKeyword   theKeyword;
    DescType    actualType;
    Size        actualSize;
    char	*temp;

    if (flags & wxMULTIOPEN) {
      long count, index;
      FSSpec fsspec;
      char *newpath, *aggregate;
      
      AECountItems(&(reply->selection),&count);
    	    
      for (index=1; index<=count; index++) {
        AEGetNthPtr(&(reply->selection),index,typeFSS, &theKeyword, &actualType, &fsspec,sizeof(fsspec),&actualSize);
        temp = scheme_mac_spec_to_path(&fsspec);
        newpath = new WXGC_ATOMIC char[strlen(aggregate) + 
                                       strlen(temp) +
                                       log_base_10(strlen(temp)) + 3];
        sprintf(newpath,"%s %ld %s",aggregate,strlen(temp),temp);
        aggregate = newpath;
      }
	
      NavDisposeReply(reply);
	    
      return aggregate;
      
    } else if (flags & wxOPEN) {
      FSSpec fsspec;
      
      AEGetNthPtr(&(reply->selection), 1, typeFSS, &theKeyword, &actualType, &fsspec, sizeof(fsspec), &actualSize);
      
      NavDisposeReply(reply);
      
      return scheme_mac_spec_to_path(&fsspec);
      
    } else { // saving file
    	int strLen = CFStringGetLength(reply->saveFileName) + 1;
        char *str = new char[strLen];

		if (CFStringGetCString(reply->saveFileName,str,strLen,CFStringGetSystemEncoding()) == FALSE) {
			// Unable to convert string
			NavDisposeReply(reply);
			delete str;
			return NULL;
		}
        
        NavDisposeReply(reply);
        
        return str;
    }
    
#else    
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
    
    if (default_path && *default_path) {
      int len = strlen(default_path);
      char *s = new char[len + 3];
      memcpy(s, default_path, len);
      if (s[len - 1] != ':')
        s[len++] = ':';
      s[len] = 0;
      if (scheme_mac_path_to_spec(s, &fsspec)) {
        startp = new AEDesc;
        if (AECreateDesc(typeFSS, &fsspec, sizeof(fsspec),  startp)) {
          startp = NULL;
        }
      }
    }
    
    if ((flags == 0) || (flags & wxOPEN) || (flags & wxMULTIOPEN))
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
    
    DisposeNavEventUPP(eventProc);
    
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

    	if (flags & wxMULTIOPEN) {
    	    long count, index;
    	    char *aggregate = "";
    	    char *newpath, *temp;
    	    
    	    AECountItems(&(reply->selection),&count);
    	    
    	    for (index=1; index<=count; index++) {
    	    	AEGetNthPtr(&(reply->selection),index,
    	    		    typeFSS, &theKeyword,
    	    		    &actualType, &fsspec,
    	    		    sizeof(fsspec),
    	    		    &actualSize);
		temp = scheme_mac_spec_to_path(&fsspec);
		newpath = new WXGC_ATOMIC char[strlen(aggregate) + 
						       strlen(temp) +
						       log_base_10(strlen(temp)) + 3];
		sprintf(newpath,"%s %ld %s",aggregate,strlen(temp),temp);
		aggregate = newpath;
	    }
	    
	    NavDisposeReply(reply);
	    
	    return aggregate;
	} else {
	    AEGetNthPtr(&(reply->selection), 1,
                        typeFSS, &theKeyword,
                        &actualType, &fsspec,
                        sizeof(fsspec),
                        &actualSize);
       
	    NavDisposeReply(reply);
	    return scheme_mac_spec_to_path(&fsspec);
	}
     } else 
       return NULL;
#endif       
  } else {
#endif
#ifdef OS_X
    wxFatalError("Navigation Services Unavailable.","");
    return NULL;
#else
	StandardFileReply	rep;
	SFTypeList typeList = { 'TEXT' };
	char * name;
	short numTypes = -1; // all types
	Str255	p_prompt,p_defname;


	if (default_path) {
	   FSSpec sp;
	   CInfoPBRec pb;
	   if (scheme_mac_path_to_spec(default_path, &sp)) {
  	     pb.dirInfo.ioNamePtr = sp.name;
	     pb.dirInfo.ioVRefNum = sp.parID; // sounds crazy, I know
	     pb.dirInfo.ioFDirIndex = 0;
	     if (PBGetCatInfo(&pb, 0) == noErr) {
	       LMSetCurDirStore(pb.dirInfo.ioDrDirID);
	     }
	     LMSetSFSaveDisk(-sp.vRefNum);
	   }
	}
	
			
	if ((flags == 0) || (flags & wxOPEN) || (flags & wxMULTIOPEN))
	{	// get file
		::StandardGetFile( NULL, numTypes, typeList, &rep);	
	} else
	{	// put file
		if (message)
		{	
			CopyCStringToPascal(message,p_prompt);
		} else 
		{
			CopyCStringToPascal("Save File Name",p_prompt);
		}
		if (default_filename)
		{	
                        CopyCStringToPascal(default_filename,p_defname);
		} else 
		{
			CopyCStringToPascal("",p_defname);
		}
		::StandardPutFile( p_prompt, p_defname, &rep);
	}
	
	if (!rep.sfGood)
	  return NULL;
	
	name = scheme_mac_spec_to_path(&rep.sfFile);
	
	if (flags & wxMULTIOPEN) {
	    char *aggregate = new char[strlen(name) + log_base_10(strlen(name)) + 2];
	    sprintf(aggregate,"%d %s",strlen(name),name);
	  
	    return aggregate;
	} else {
	    return name;
	}
#endif
#ifdef USE_NAVIGATION
  }
#endif
}

