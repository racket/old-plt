/*
 * File:	wx_cmdlg.cc
 * Purpose:	Common dialogs: MS Windows implementation
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 */

// static const char sccsid[] = "%W% %G%";

#if defined(_MSC_VER)
# include "wx.h"
#else


// For some reason, this must be defined for common dialogs to work.
#ifdef __WATCOMC__
#define INCLUDE_COMMDLG_H	1
#endif

#include "common.h"
#include "wx_gdi.h"
#include "wx_utils.h"
#include "wx_types.h"
#include "wx_frame.h"
#include "wx_dialg.h"
#include "wx_wmgr.h"

#include "wx_cmdlg.h"
#endif

#include "wx_pdf.h"

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300


/*
 * The following code was formerly in wx_dialg.cc.
 */


/*
 * Common dialogs
 *
 */
 
// Pop up a message box
int wxMessageBox(char *message, char *caption, long type,
                 wxWindow *parent, int x, int y)
{
#if 0
  if ((type & wxCENTRE) == wxCENTRE)
#endif
    return wxbMessageBox(message, caption, type, parent, x, y);

#if 0
  if (!parent) parent = wxDefaultParent;

  HWND hWnd = 0;
  if (parent) hWnd = parent->GetHWND();
  unsigned int msStyle = MB_OK;
  if (type & wxYES_NO)
  {
    if (type & wxCANCEL)
      msStyle = MB_YESNOCANCEL;
    else
      msStyle = MB_YESNO;
  }
  if (type & wxOK)
  {
    if (type & wxCANCEL)
      msStyle = MB_OKCANCEL;
    else
      msStyle = MB_OK;
  }
  if (type & wxICON_EXCLAMATION)
	 msStyle |= MB_ICONEXCLAMATION;
  else if (type & wxICON_HAND)
    msStyle |= MB_ICONHAND;
  else if (type & wxICON_INFORMATION)
    msStyle |= MB_ICONINFORMATION;
  else if (type & wxICON_QUESTION)
    msStyle |= MB_ICONQUESTION;

  if (hWnd)
    msStyle |= MB_APPLMODAL;
  else
    msStyle |= MB_TASKMODAL;
    
  int msAns;

#ifndef USE_SEP_WIN_MANAGER
  msAns = MessageBox(hWnd, (LPCSTR)message, (LPCSTR)caption, msStyle);
#else
  wxwmMessageBox r;

  r.owner = hWnd;
  r.text = (LPCSTR)message;
  r.title = (LPCSTR)caption;
  r.style = msStyle;
  wxwmMessage(WXM_MESSAGE_BOX, (LPARAM)&r);
  msAns = r.result;
#endif

  int ans = wxOK;
  switch (msAns)
  {
    case IDCANCEL:
      ans = wxCANCEL;
      break;
    case IDOK:
      ans = wxOK;
      break;
    case IDYES:
      ans = wxYES;
      break;
    case IDNO:
      ans = wxNO;
      break;
  }
  return ans;
#endif
}

#if USE_COMMON_DIALOGS

// Crashes: removed for present - JACS Oct 94
// DISCOVERED WHY: according to the MS Knowledge Base,
// you must put it in the EXPORTS section of the .DEF file.
// Shouldn't _EXPORT do this? Anyway, it sounds rather
// like the reported error: not being able to access
// local variable because of wrong segment setting.
// Worth a try.
// OR -- more likely -- need to do MakeProcInstance in wxFileSelector
// to make it work properly.
#if 0    		 
unsigned int APIENTRY _EXPORT
  wxFileHook(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
#if FAFA_LIB
  switch(message)
  {
        case WM_ERASEBKGND:
        {
          RECT rect;
	  HDC pDC = (HDC)wParam ;
          GetClientRect(hWnd, &rect);
          int mode = SetMapMode(pDC, MM_TEXT);
          FillRect(pDC, &rect, brushFace);
          SetMapMode(pDC, mode);
          break;
        }
#ifdef WIN32
        case WM_CTLCOLORDLG:
	{
          return (unsigned int)brushFace ;
          break;
	}
        case WM_CTLCOLORBTN:
        case WM_CTLCOLORSTATIC:
	{
          HDC pDC = (HDC)wParam;
          SetTextColor(pDC,colorLabel) ;
          SetBkMode(pDC,TRANSPARENT) ;
          return (unsigned int)brushFace ;
          break;
	}
        case WM_CTLCOLOREDIT:
	{
          HDC pDC = (HDC)wParam;
          SetTextColor(pDC,colorLabel) ;
          return NULL ;
          break;
	}
#else
        case WM_CTLCOLOR:
        {
          int nCtlColor = (int)HIWORD(lParam);
          HDC pDC = (HDC)wParam;
	  switch(nCtlColor)
          {
          case CTLCOLOR_DLG:
            return (unsigned int)brushFace ;
          break ;
	  case CTLCOLOR_BTN:
	  case CTLCOLOR_STATIC:
	    // BUGBUG For some reason it crashes at this point,
	    // so I'm commenting out this whole hook.
            SetTextColor(pDC,colorLabel) ;
            SetBkMode(pDC,TRANSPARENT) ;
            return (unsigned int)brushFace ;
          break ;
	  case CTLCOLOR_EDIT:
            SetTextColor(pDC,colorLabel) ;
            return NULL ;
          break ;
          }
          break;
        }
#endif
  }
#endif
  return(FALSE) ; // Pass non processed messages to standard dialog box fct.
}
#endif
#endif
  // end USE_COMMON_DIALOGS

static BOOL DoGetSaveFileName(OPENFILENAME *of, HWND parent)
{
  if (!of->hwndOwner)
    of->hwndOwner = parent;
  return GetSaveFileName(of);
}

static BOOL DoGetOpenFileName(OPENFILENAME *of, HWND parent)
{
  if (!of->hwndOwner)
    of->hwndOwner = parent;
  return GetOpenFileName(of);
}

static int set_init_dir;
extern void MrEdSyncCurrentDir(void);

char *wxFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
#if !USE_COMMON_DIALOGS
  // Great file selector folks !!!
  return wxGetTextFromUser(message, "Enter filename", NULL, parent, x, y);
#else
  // Use extended version (below) NO!! This function is very buggy. (WAS very buggy.)
//  return wxFileSelectorEx(message, default_path, default_filename,
//        NULL, wildcard, flags, parent, x, y);

  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;

  if (!parent) parent = wxDefaultParent;

  wxWnd *wnd = NULL;
  HWND hwnd = NULL;
  if (parent)
  {
    wnd = (wxWnd *)parent->handle;
    hwnd = wnd->handle;
  }
  char *file_buffer;

  file_buffer = new WXGC_ATOMIC char[400];

  if (default_filename)
    strcpy(file_buffer, default_filename);
  else file_buffer[0] = 0;

  char *title_buffer;
  title_buffer = new WXGC_ATOMIC char[50];
  title_buffer[0] = 0;

  char *filter_buffer;
  filter_buffer = new WXGC_ATOMIC char[200];

  if (!wildcard)
    wildcard = "*.*";

/* Alejandro Sierra's wildcard modification

In wxFileSelector you can put, instead of a single wild_card, pairs of
strings separated by '|'. The first string is a description, and the
second is the wild card. You can put any number of pairs.

eg.  "description1 (*.ex1)|*.ex1|description2 (*.ex2)|*.ex2"

If you put a single wild card, it works as before my modification.
*/

// Here begin my changes (Alex)              ******************************
  if (wildcard)                               
  {
         if (!strchr(wildcard, '|'))         // No '|'s, I leave it as it was
                sprintf(filter_buffer, "Files (%s)|%s",wildcard, wildcard);
         else
                strcpy(filter_buffer, wildcard);

         int len = strlen(filter_buffer);

         int i;
         for (i = 0; i < len; i++)
                if (filter_buffer[i]=='|')
                  filter_buffer[i] = '\0';

         filter_buffer[len+1] = '\0';

  }
// Here end my changes (Alex)              ******************************
/* Now replaced by Alex's code above

  if (wildcard)
  {
    sprintf(filter_buffer, "Files (%s)", wildcard);
    int len1 = strlen(filter_buffer);
    int len2 = strlen(wildcard);

    filter_buffer[len1] = 0;
    int i;
    for (i = 0; i < len2; i++)
      filter_buffer[len1 + 1 + i] = wildcard[i];
    filter_buffer[len1 + 1 + len2] = 0;
    filter_buffer[len1 + 2 + len2] = 0;
  }
*/

  if (!set_init_dir) {
    set_init_dir = 1;
    MrEdSyncCurrentDir();
  }

  OPENFILENAME *of;
  of = new OPENFILENAME;

  memset(of, 0, sizeof(OPENFILENAME));

  of->lStructSize = sizeof(OPENFILENAME);
  of->hwndOwner = hwnd;

  if (wildcard)
  {
    of->lpstrFilter = (LPSTR)filter_buffer;
    of->nFilterIndex = 1L;
  }
  else
  {
    of->lpstrFilter = NULL;
    of->nFilterIndex = 0L;
  }
  of->lpstrCustomFilter = NULL;
  of->nMaxCustFilter = 0L;
  of->lpstrFile = file_buffer;
  of->nMaxFile = 400;
  of->lpstrFileTitle = title_buffer;
  of->nMaxFileTitle = 50;
  of->lpstrInitialDir = default_path;
  of->lpstrTitle = message;
  of->nFileOffset = 0;
  of->nFileExtension = 0;
  of->lpstrDefExt = default_extension;

  long msw_flags = 0;

  if (flags & wxSAVE)
    msw_flags |= OFN_OVERWRITEPROMPT;
  if (flags & wxHIDE_READONLY)
    msw_flags |= OFN_HIDEREADONLY;
  if (default_path)
    msw_flags |= OFN_NOCHANGEDIR;
  of->Flags = msw_flags;
//  of.Flags = msw_flags|OFN_ENABLEHOOK;
//  of.lpfnHook = wxFileHook ;

  Bool success;

  if (flags & wxSAVE)
    success = wxPrimitiveDialog((wxPDF)DoGetSaveFileName, of, 1);
  else
    success = wxPrimitiveDialog((wxPDF)DoGetOpenFileName, of, 1);

//  DWORD error = CommDlgExtendedError();
  if (success)
    return file_buffer;
  else
    return NULL;
#endif
  // End USE_COMMON_DIALOGS
}

#if USE_COMMON_DIALOGS
/*
    wxFileSelector  modification (only for MS-Windows (yet) ) :


    char* wxFileSelectorEx( char* 	  title,               // = "Select a file"
						    char* 	  defaultDir,          // = NULL
						    char* 	  defaultFileName,     // = NULL
						    int*      defaultFilterIndex,  // = NULL
						    char*     filter,              // = "*.*"
                            int       flags,               // = 0
                            wxWindow* parent,              // = NULL
                            int       x,                   // = -1
                            int       y )                  // = -1

    wxFileSelectorEx is as wxFileSelector with little changes:

    The defaultFilterIndex is changed to int*,
    so You can get the selected filter.

    To the filename is automaticely added the selected
    extension if You type only the filename in the dialogbox.

    eg.  if filter is "Data (*.dat)|*.dat|Text (*.txt)|*.txt|All (*.*)|*.*"

    and you have selected: *.txt
    and you type 'hello' into the Filedialog,
    the filename is expanded to hello.txt.
    If you want explicitely save or load 'hello' as
    'hello' and not as 'hello.txt', You have to select *.*,
    or type 'hello.'  .

    So I use it in my Application:

void Open( void )
{
# ifdef wx_msw
    char tempList[] = "Daten (*.dat)|*.dat|Texte (*.txt)|*.txt|Alle (*.*)|*.*";
# else
    char tempList[] = "*.dat";  // <<== This also works under wx_msw  
# endif                                 

    static int fileDialogIndex = 1;

    char* tempName = wxFileSelector( "Open file",
                                     NULL,
                                     NULL,
                                     &fileDialogIndex,
                                     tempList,
                                     wxOPEN | wxHIDE_READONLY,
                                     NULL,
                                     20,
                                     20 );
}

Regards,
         Alfred

*/

# if __BORLANDC__
#   include <dir.h>  // for MAXPATH etc. ( Borland 3.1 ) 
# endif

# ifndef MAXPATH
# define MAXPATH   400   
# endif

# ifndef MAXDRIVE
# define MAXDRIVE  3
# endif

# ifndef MAXFILE 
# define MAXFILE   9
# endif

# ifndef MAXEXT
# define MAXEXT    5
# endif


char* wxFileSelectorEx( char* 	  title,               // = "Select a file"
			char* 	  defaultDir,          // = NULL
			char* 	  defaultFileName,     // = NULL
			int*      defaultFilterIndex,  // = NULL
			char*     filter,              // = "*.*"
                        int       flags,               // = 0
                        wxWindow* parent,              // = NULL
                        int       x,                   // = -1
                        int       y )                  // = -1

{
    if ( x < 0 ) { x = wxDIALOG_DEFAULT_X; }
    if ( y < 0 ) { y = wxDIALOG_DEFAULT_Y; }

	if (!parent) parent = wxDefaultParent;

    HWND  HWindow = ( parent ) ? ((wxWnd *)parent->handle)->handle : NULL;

	static char  fileNameBuffer [ MAXPATH ];		   // the file-name
	char         titleBuffer    [ MAXFILE+1+MAXEXT ];  // the file-name, without path

	*fileNameBuffer = '\0';
    *titleBuffer    = '\0';

	char* filterBuffer = NULL;
	char* extension    = NULL;

    long msw_flags = 0;
    if ( flags & wxHIDE_READONLY ) { msw_flags |= OFN_HIDEREADONLY; }


	OPENFILENAME of;
	memset(&of, 0, sizeof(OPENFILENAME));

    of.lpstrCustomFilter = NULL;   // system should not save custom filter
	of.nMaxCustFilter    = 0L;

	of.nFileOffset       = 0;      // 0-based pointer to filname in lpstFile
	of.nFileExtension    = 0;      // 0-based pointer to extension in lpstrFile
	of.lpstrDefExt       = NULL;   // no default extension

	of.lStructSize 		 = sizeof(OPENFILENAME);
	of.hwndOwner 		 = HWindow;
	of.lpstrTitle        = title;


	of.lpstrFileTitle    = titleBuffer;
	of.nMaxFileTitle     = MAXFILE + 1 + MAXEXT;	// Windows 3.0 and 3.1

	of.lpstrInitialDir   = defaultDir;

	of.Flags 			 = msw_flags;



    //=== Like Alejandro Sierra's wildcard modification >>===================
    /*
        In wxFileSelector you can put, instead of a single wild_card,
        pairs of strings separated by '|'.
        The first string is a description, and the
        second is the wild card. You can put any number of pairs.

        eg.  "description1 (*.ex1)|*.ex1|description2 (*.ex2)|*.ex2"

        If you put a single wild card, it works as before the modification.
    */
    //=======================================================================

	if ( !filter ) filter = "*.*";

	int filterBufferLen = 0;

	if ( !strchr( filter, '|' ) ) { 			// only one filter ==> default text:
		char buffText[] = "Files (%s)|%s";
		filterBufferLen = strlen( filter )*2 + strlen( buffText ) -4;
		filterBuffer    = new char[ filterBufferLen +2 ];

		if ( filterBuffer ) {
			sprintf( filterBuffer, buffText, filter, filter );
		}
	}
	else {                  					// more then one filter
		filterBufferLen = strlen( filter );
		filterBuffer    = new char[ filterBufferLen +2 ];

		if ( filterBuffer ) {
			strcpy( filterBuffer, filter );
	    }
	}

    if ( filterBuffer ) {  					    // Substituting '|' with '\0'
		for ( int i = 0; i < filterBufferLen; i++ ) {
			if ( filterBuffer[i] == '|' ) { filterBuffer[i] = '\0'; }
		}
	}

	filterBuffer[filterBufferLen+1] = '\0';

	of.lpstrFilter  = (LPSTR)filterBuffer;
	of.nFilterIndex = (defaultFilterIndex) ? *defaultFilterIndex : 1;



    //=== Setting defaultFileName >>=========================================

	if ( defaultFileName ) {
        strncpy( fileNameBuffer, defaultFileName, MAXPATH-1 );
	    fileNameBuffer[ MAXPATH-1 ] = '\0';
    }

	of.lpstrFile = fileNameBuffer;  // holds returned filename
	of.nMaxFile  = MAXPATH;




    //== Execute FileDialog >>=================================================

    Bool success = (flags & wxSAVE) ? GetSaveFileName(&of) : GetOpenFileName(&of);

    char* returnFileName = NULL;

    if ( success )
    {
        //=== Adding the correct extension >>=================================

        if ( defaultFilterIndex ) {
            *defaultFilterIndex = (int)of.nFilterIndex;
        }

	    if ( of.nFileExtension && fileNameBuffer[ of.nFileExtension-1] != '.' )
        {                                    // user has typed an filename
                                             // without an extension:

	        int   maxFilter = (int)(of.nFilterIndex*2L-1L);
	        extension       = filterBuffer;

	        for( int i = 0; i < maxFilter; i++ ) {		    // get extension
		        extension = extension + strlen( extension ) +1;
	        }

	        if (  (extension = strrchr( extension, '.' ))   // != "blabla" 
			      && !strrchr( extension, '*' )             // != "blabla.*"
			      && !strrchr( extension, '?' )             // != "blabla.?"
			      && extension[1]                           // != "blabla."
			      && extension[1] != ' ' )                  // != "blabla. "
	        {
                              // now concat extension to the fileName:

                int len = strlen( fileNameBuffer );
	            strncpy( fileNameBuffer + len, extension, MAXPATH - len );
                fileNameBuffer[ MAXPATH -1 ] = '\0';
            }
	    }

        returnFileName = fileNameBuffer;


	    //=== Simulating the wxOVERWRITE_PROMPT >>============================

	    if ( (flags & wxOVERWRITE_PROMPT) && ::FileExists( fileNameBuffer ) )
        {
		    char  questionText[] = "Replace file\n%s\n?";
		    char* messageText    = new char[strlen(questionText)+strlen(fileNameBuffer)-1];
		    sprintf( messageText, questionText, fileNameBuffer );

		    if ( messageText && ( wxMessageBox( messageText, title, wxYES_NO ) != wxYES ) )
            {
                returnFileName = NULL;
		    }

		    delete[] messageText;
	    }

    } // END: if ( success )


    delete[] filterBuffer;

    return returnFileName;
}
#endif

