/*
 * File:	wx_utils.h
 * Purpose:	Miscellaneous utilities
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wxb_utilsh
#define wxb_utilsh

#ifdef __GNUG__
#pragma interface
#endif

#include "wx_setup.h"
#include "wx_obj.h"
#include "wx_list.h"
#include "wx_win.h"
#include "iostream.h"

#ifdef wx_x
#ifndef VMS
/*steve: these two are not known under VMS */
#include <dirent.h>
#include <unistd.h>
#endif
#endif

// sprintf is often needed, but we don't always want to include the whole
// of stdio.h!
#ifdef wx_msw
extern "C" int sprintf(char *, const char *, ...);
#elif defined(wx_x)
#include <stdio.h>
#endif

// Forward declaration
class wxFrame;

// Stupid ASCII macros
#define   wxToUpper(C)      (((C) >= 'a' && (C) <= 'z')? (C) - 'a' + 'A': (C))
#define   wxToLower(C)      (((C) >= 'A' && (C) <= 'Z')? (C) - 'A' + 'a': (C))

// Return a string with the current date/time
char *wxNow(void);

// Make a copy of this string using 'new'
char *copystring(const char *s);

// Generate a unique ID
long wxNewId(void);
#define NewId wxNewId

// Ensure subsequent IDs don't clash with this one
void wxRegisterId(long id);
#define RegisterId wxRegisterId

// Return the current ID
long wxGetCurrentId(void);

// Useful buffer
extern char *wxBuffer;

// Various conversions
void StringToFloat(char *s, float *number);
char *FloatToString(float number);
void StringToDouble(char *s, double *number);
char *DoubleToString(double number);
void StringToInt(char *s, int *number);
void StringToLong(char *s, long *number);
char *IntToString(int number);
char *LongToString(long number);

// Matches string one within string two regardless of case
#ifndef IN_CPROTO
Bool StringMatch(char *one, char *two, Bool subString = TRUE, Bool exact = FALSE);
#endif

// A shorter way of using strcmp
#define wxStringEq(s1, s2) (s1 && s2 && (strcmp(s1, s2) == 0))

// Some file utilities

#ifdef IN_CPROTO
typedef       void    *wxPathList ;
typedef       void    *wxLogClass;
#else
// Path searching
class wxPathList: public wxList
{
  DECLARE_DYNAMIC_CLASS(wxPathList)

  public:

  void AddEnvList(char *envVariable);    // Adds all paths in environment variable
  void Add(char *path);
  char *FindValidPath(char *filename);   // Find the first full path
                                         // for which the file exists
  void EnsureFileAccessible(char *path); // Given full path and filename,
                                         // add path to list
  Bool Member(char *path);
};

Bool wxFileExists(const char *filename);
#define FileExists wxFileExists

Bool wxDirExists(const char *dir);
#define DirExists wxDirExists

Bool wxIsAbsolutePath(const char *filename);
#define IsAbsolutePath wxIsAbsolutePath

// Get filename
char *wxFileNameFromPath(char *path);
#define FileNameFromPath wxFileNameFromPath

// Get directory
char *wxPathOnly(char *path);
#define PathOnly wxPathOnly

void wxDos2UnixFilename(char *s);
#define Dos2UnixFilename wxDos2UnixFilename

void wxUnix2DosFilename(char *s);
#define Unix2DosFilename wxUnix2DosFilename

// Strip the extension, in situ
void wxStripExtension(char *buffer);

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName(const char *prefix, char *buf = NULL);

// Expand file name (~/ and ${OPENWINHOME}/ stuff)
char *wxExpandPath(char *dest, const char *path);

// Contract w.r.t environment (</usr/openwin/lib, OPENWHOME> -> ${OPENWINHOME}/lib)
// and make (if under the home tree) relative to home
// [caller must copy-- volatile]
char *wxContractPath (const char *filename,
   const char *envname = NULL, const char *user = NULL);

// Destructive removal of /./ and /../ stuff
char *wxRealPath(char *path);

// Allocate a copy of the full absolute path
char *wxCopyAbsolutePath(const char *path);

// Get first file name matching given wild card.
// Flags are reserved for future use.
#define wxFILE  1
#define wxDIR   2
char *wxFindFirstFile(const char *spec, int flags = wxFILE);
char *wxFindNextFile(void);

// Does the pattern contain wildcards?
Bool wxIsWild(const char *pattern);

// Does the pattern match the text (usually a filename)?
// If dot_special is TRUE, doesn't match * against . (eliminating
// `hidden' dot files)
Bool wxMatchWild(const char *pattern,  const char *text, Bool dot_special = TRUE);

// Execute another program. Returns FALSE if there was an error.
Bool wxExecute(char **argv, Bool Async = FALSE);
Bool wxExecute(const char *command, Bool Async = FALSE);

// Execute a command in an interactive shell window
// If no command then just the shell
Bool wxShell(const char *command = NULL);

// Concatenate two files to form third
Bool wxConcatFiles(const char *file1, const char *file2, const char *file3);

// Copy file1 to file2
Bool wxCopyFile(const char *file1, const char *file2);

// Remove file
Bool wxRemoveFile(const char *file);

// Rename file
Bool wxRenameFile(const char *file1, const char *file2);

// Get current working directory.
// If buf is NULL, allocates space using new, else
// copies into buf.
char *wxGetWorkingDirectory(char *buf = NULL, int sz = 1000);

// Set working directory
Bool wxSetWorkingDirectory(char *d);

// Sleep for nSecs seconds under UNIX, do nothing under Windows
void wxSleep(int nSecs);

// Get free memory in bytes, or -1 if cannot determine amount (e.g. on UNIX)
long wxGetFreeMemory(void);

// Consume all events until no more left
void wxFlushEvents(void);

// Make directory
Bool wxMkdir(const char *dir);

/*
 * Network and username functions.
 *
 */

// Get eMail address
Bool wxGetEmailAddress(char *buf, int maxSize);

// Get hostname.
Bool wxGetHostName(char *buf, int maxSize);

// Get user ID e.g. jacs
Bool wxGetUserId(char *buf, int maxSize);

// Get user name e.g. Julian Smart
Bool wxGetUserName(char *buf, int maxSize);

/*
 * Strip out any menu codes
 */
char *wxStripMenuCodes(char *in, char *out = NULL);

// Find the window/widget with the given title or label.
// Pass a parent to begin the search from, or NULL to look through
// all windows.
wxWindow *wxFindWindowByLabel(char *title, wxWindow *parent = NULL);

// Find window by name, and if that fails, by label.
wxWindow *wxFindWindowByName(char *name, wxWindow *parent = NULL);

// Returns menu item id or -1 if none.
int wxFindMenuItemId(wxFrame *frame, char *menuString, char *itemString);

// A debugging stream buffer.
// Under Windows, this writes to the Windows debug output.
// Under other platforms, it writes to cerr.

// ALl this horrible gubbins required for Borland, because the calling
// convention needs to be the same as for streambuf.
// Thanks to Gerhard.Vogt@embl-heidelberg.de for this solution.

#if defined(__BORLANDC__) && defined(__BCOPT__) && !defined(_RTL_ALLOW_po) && !defined(__FLAT__)
#pragma option -po-
#endif

class wxDebugStreamBuf: public streambuf
{
  public:
    wxDebugStreamBuf(void);
    ~wxDebugStreamBuf(void) {}

    int overflow(int i);
    inline int underflow(void) { return EOF; }
    int sync(void);
};
#if defined(__BORLANDC__) && defined(__BCOPT__) && !defined(_RTL_ALLOW_po) && !defined(__FLAT__)
#pragma option -po.
#endif

#if (!defined(__MINMAX_DEFINED) && !defined(max))
#define max(a,b)            (((a) > (b)) ? (a) : (b))
#define min(a,b)            (((a) < (b)) ? (a) : (b))
#define __MINMAX_DEFINED 1
#endif

// Yield to other apps/messages
Bool wxYield(void);

// Format a message on the standard error (UNIX) or the debugging
// stream (Windows)
void wxDebugMsg(const char *fmt ...) ;
 
// Sound the bell
void wxBell(void) ;
  
// Get OS version
int wxGetOsVersion(int *majorVsn=NULL,int *minorVsn=NULL) ;
 
// Set the cursor to the busy cursor for all windows
class wxCursor;
extern wxCursor *wxHOURGLASS_CURSOR;
void wxBeginBusyCursor(wxCursor *cursor = wxHOURGLASS_CURSOR);
 
// Restore cursor to normal
void wxEndBusyCursor(void);
 
// TRUE if we're between the above two calls
Bool wxIsBusy(void);
  
/* Error message functions used by wxWindows */

// Non-fatal error (continues) 
void wxError(const char *msg, const char *title = "wxWindows Internal Error");

// Fatal error (exits)
void wxFatalError(const char *msg, const char *title = "wxWindows Fatal Error");

// Reading and writing resources (eg WIN.INI, .Xdefaults)
#if USE_RESOURCES
Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, float value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, long value, const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, int value, const char *file = NULL);

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, float *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, long *value, const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, int *value, const char *file = NULL);
#endif // USE_RESOURCES

#ifdef wx_x
// 'X' Only, will soon vanish....
// Get current Home dir and copy to dest
char *wxGetHomeDir(char *dest);
#endif
// Get the user's home dir (caller must copy--- volatile)
// returns NULL is no HOME dir is known
char *wxGetUserHome(const char *user = NULL);

// X only
#ifdef wx_x
// Get X display: often needed in the wxWindows implementation.
Display *wxGetDisplay(void);
/* MATTHEW: [4] Added wxSetDisplay and wxGetDisplayName */
Bool wxSetDisplay(char *display_name);
char *wxGetDisplayName(void);
#endif

#ifdef wx_x

#include <X11/Xlib.h>

#define wxMAX_RGB           0xff
#define wxMAX_SV            1000
#define wxSIGN(x)           ((x < 0) ? -x : x)
#define wxH_WEIGHT          4
#define wxS_WEIGHT          1
#define wxV_WEIGHT          2

typedef struct wx_hsv {
                        int h,s,v;
                      } wxHSV;
 
#define wxMax3(x,y,z) ((x > y) ? ((x > z) ? x : z) : ((y > z) ? y : z))
#define wxMin3(x,y,z) ((x < y) ? ((x < z) ? x : z) : ((y < z) ? y : z))

#define wxMax2(x,y)   ((x > y) ? x : y)
#define wxMin2(x,y)   ((x < y) ? x : y)

void wxHSVToXColor(wxHSV *hsv,XColor *xcolor);
void wxXColorToHSV(wxHSV *hsv,XColor *xcolor);
void wxAllocNearestColor(Display *display,Colormap colormap,XColor *xcolor);
int wxAllocColor(Display *display,Colormap colormap,XColor *xcolor);

#endif //wx_x

#endif // IN_CPROTO
#endif // wxb_utilsh
