/*
 * File:	wx_utils.cc
 * Purpose:	Various utilities
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_utils.cc,v 1.2 1994/08/14 23:00:24 edz Exp edz $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_setup.h"
#include "wx_utils.h"
#include "wx_main.h"
#include "wx_timer.h"
#include "wx_gdi.h"
#include "wx_wmgr.h"

#endif

#include <ctype.h>
#include <direct.h>

#include <dos.h>
#ifdef __BORLANDC__ // Please someone tell me which version of Borland needs
                    // this (3.1 I believe) and how to test for it.
                    // If this works for Borland 4.0 as well, then no worries.
#include <dir.h>
#endif

#if 0
#ifdef WIN32
#include <io.h>
#endif
#endif

/* MATTHEW: [5] Normalize wxSleep(): */
#define WX_USE_GLOBAL_SLEEP 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if 0
#ifndef __WATCOMC__
#include <errno.h>
#endif
#endif
#include <stdarg.h>

// In the WIN.INI file
static const char WX_SECTION[] = "wxWindows";
static const char eHOSTNAME[]  = "HostName";
static const char eUSERID[]    = "UserId";
static const char eUSERNAME[]  = "UserName";


// For the following functions we SHOULD fill in support
// for Windows-NT (which I don't know) as I assume it begin
// a POSIX Unix (so claims MS) that it has some special
// functions beyond those provided by WinSock

// Get full hostname (eg. DoDo.BSn-Germany.crg.de)
Bool wxGetHostName(char *buf, int maxSize)
{
#if 0
// This code is Moving to the configuration utility....
# define MAXHOSTNAMELEN 128
  char hostname[MAXHOSTNAMELEN + 1];
  int nRC;

  if ((nRc = gethostname (hostname, MAXHOSTNAMELEN)) == 0)
    {
      struct hostent *hostptr;
      char fullname[MAXHOSTNAMELEN + 1];

      if ((hostptr = gethostbyname (hostname)) != NULL)
	{
	  char *tp1, *tp2;
	  if ((tp1 = strchr (hostname, '.')) == NULL)
	    {
	      if ((tp2 = strchr (hostptr->h_name, '.')) != NULL)
		{
		  sprintf (fullname, "%s.%s", hostname, tp2 + 1);
		  strcpy (hostname, fullname);
		}
	      else
		hostname[0] = '\0';	/* better none than not complete */
	    }
	  /* else we have complete address */
	}
	strncpy(buf, hostname, maxSize - 1);
	buf[maxSize] = '\0';
	return TRUE;
    }
  return FALSE;
#else
  char *sysname;
  const char *default_host = "noname";

  if ((sysname = getenv("SYSTEM_NAME")) == NULL) {
     GetProfileString(WX_SECTION, eHOSTNAME, default_host, buf, maxSize - 1);
  } else
    strncpy(buf, sysname, maxSize - 1);
  buf[maxSize] = '\0';
#endif
  return *buf ? TRUE : FALSE;
}

// Get user ID e.g. jacs
Bool wxGetUserId(char *buf, int maxSize)
{
  char *user;
  const char *default_id = "anonymous";

  // Can't assume we have NIS (PC-NFS) or some other ID daemon
  // So we ...
  if (	(user = getenv("USER")) == NULL &&
	(user = getenv("LOGNAME")) == NULL ) {
     // Use wxWindows configuration data (comming soon)
     GetProfileString(WX_SECTION, eUSERID, default_id, buf, maxSize - 1);
  } else
    strncpy(buf, user, maxSize - 1);
  return *buf ? TRUE : FALSE;
}

// Get user name e.g. Julian Smart
Bool wxGetUserName(char *buf, int maxSize)
{
  const char *default_name = "Unknown User";

  extern HANDLE hPenWin; // PenWindows Running?
  if (hPenWin) {
    // PenWindows Does have a user concept!
    // Get the current owner of the recognizer
    GetPrivateProfileString("Current", "User", default_name, wxBuffer, maxSize - 1, "PENWIN.INI");
    strncpy(buf, wxBuffer, maxSize - 1);
  } else {
    // Could use NIS, MS-Mail or other site specific programs
    // Use wxWindows configuration data 
    GetProfileString(WX_SECTION, eUSERNAME, default_name, buf, maxSize - 1);
  }
  return *buf ? TRUE : FALSE;
}

// Execute a command (e.g. another program) in a
// system-independent manner.

Bool wxExecute(char **argv, Bool Async)
{
  if (*argv == NULL)
    return FALSE;

  char command[1024];
  command[0] = '\0';

  int argc;
  for (argc = 0; argv[argc]; argc++)
   {
    if (argc)
      strcat(command, " ");
    strcat(command, argv[argc]);
   }

  return wxExecute((char *)command, Async);
}

Bool wxExecute(const char *command, Bool Async)
{
  if (command == NULL || *command == '\0')
    return FALSE;

  long Instance_ID = WinExec((LPCSTR)command, SW_SHOW);
  if (Instance_ID < 32) return(FALSE);
// WIN32 doesn't have GetModuleUsage!!
#ifndef WIN32
  if (Async) {
    int running;
    do {
      wxYield();
      running = GetModuleUsage((HANDLE)Instance_ID);
    } while (running);
  }
#endif
  return(TRUE);
}

//
// Execute a program in an Interactive Shell
//
Bool
wxShell(const char *command)
{
  char *shell;
  if ((shell = getenv("COMSPEC")) == NULL)
    shell = "\\COMMAND.COM";

  char tmp[255];
  if (command && *command)
    sprintf(tmp, "%s /c %s", shell, command);
  else
    strcpy(tmp, shell);

  return wxExecute((char *)tmp, FALSE); /* MATTHEW: BC */
}



Bool wxRemoveFile(const char *file)
{
// Zortech -- what's the proper define?
#ifdef ZTC
  int flag = unlink(file);
#else
  int flag = remove(file);
#endif
  if (flag == 0) return TRUE;
  return FALSE;
}

Bool wxMkdir(const char *dir)
{
  return (mkdir(dir) == 0);
}

Bool wxRmdir(const char *dir)
{
  return (rmdir(dir) == 0);
}

Bool wxDirExists(const char *dir)
{
  /* MATTHEW: [6] Always use same code for Win32, call FindClose */
#if defined(WIN32)
  WIN32_FIND_DATA fileInfo;
#else
#ifdef __BORLANDC__
  struct ffblk fileInfo;
#else
  struct find_t fileInfo;
#endif
#endif

#if defined(WIN32)
	HANDLE h = FindFirstFile((LPTSTR)dir,(LPWIN32_FIND_DATA)&fileInfo);

	if (h==INVALID_HANDLE_VALUE)
	 return FALSE;
	else {
	 FindClose(h);
	 return (fileInfo.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
	}
#else
  // In Borland findfirst has a different argument
  // ordering from _dos_findfirst. But _dos_findfirst
  // _should_ be ok in both MS and Borland... why not?
#ifdef __BORLANDC__
  return (findfirst(dir, &fileInfo, _A_SUBDIR) == 0  && (fileInfo.ff_attrib & _A_SUBDIR));
#else
  return ((_dos_findfirst(dir, _A_SUBDIR, &fileInfo) == 0) && (fileInfo.attrib & _A_SUBDIR));
#endif
#endif
}

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName(const char *prefix, char *buf)
{
#ifndef	WIN32
  char tmp[144];
  ::GetTempFileName(0, prefix, 0, tmp);
#else
  char tmp[MAX_PATH];
  char tmpPath[MAX_PATH];
  ::GetTempPath(MAX_PATH, tmpPath);
  ::GetTempFileName(tmpPath, prefix, 0, tmp);
#endif
  if (buf) strcpy(buf, tmp);
  else buf = copystring(tmp);
  return buf;
/**** old
  char tmp[64];
  ::GetTempFileName(0, prefix, 0, tmp);
  if (buf) strcpy(buf, tmp);
  else buf = copystring(tmp);
  return buf;
*/
}

// Get first file name matching given wild card.
// Flags are reserved for future use.

#ifdef WIN32
HANDLE wxFileStrucHandle = INVALID_HANDLE_VALUE;
WIN32_FIND_DATA wxFileStruc;
#else
#ifdef __BORLANDC__
static struct ffblk wxFileStruc;
#else
static struct _find_t wxFileStruc;
#endif
#endif
static char *wxFileSpec = NULL;
static int wxFindFileFlags;

char *wxFindFirstFile(const char *spec, int flags)
{
  if (wxFileSpec)
	 delete[] wxFileSpec;
  wxFileSpec = copystring(spec);
  wxFindFileFlags = flags; /* MATTHEW: [5] Remember flags */

  // Find path only so we can concatenate
  // found file onto path
  char *p = wxPathOnly(wxFileSpec);
  if (p && (strlen(p) > 0))
	 strcpy(wxBuffer, p);
  else
	 wxBuffer[0] = 0;

#ifdef WIN32
  if (wxFileStrucHandle != INVALID_HANDLE_VALUE)
	 FindClose(wxFileStrucHandle);

  wxFileStrucHandle = FindFirstFile(spec, &wxFileStruc);

  if (wxFileStrucHandle == INVALID_HANDLE_VALUE)
	 return NULL;

  Bool isdir = !!(wxFileStruc.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);

  if (isdir && !(flags & wxDIR))
	 return wxFindNextFile();
  else if (!isdir && flags && !(flags & wxFILE))
	 return wxFindNextFile();

  if (wxBuffer[0] != 0)
	 strcat(wxBuffer, "\\");
  strcat(wxBuffer, wxFileStruc.cFileName);
  return wxBuffer;
#else

  int flag = _A_NORMAL;
  if (flags & wxDIR) /* MATTHEW: [5] Use & */
    flag = _A_SUBDIR;

#ifdef __BORLANDC__
  if (findfirst(spec, &wxFileStruc, flag) == 0)
#else
  if (_dos_findfirst(spec, flag, &wxFileStruc) == 0)
#endif
  {
    /* MATTHEW: [5] Check directory flag */
    char attrib;

#ifdef __BORLANDC__
    attrib = wxFileStruc.ff_attrib;
#else
    attrib = wxFileStruc.attrib;
#endif

    if (attrib & _A_SUBDIR) {
      if (!(wxFindFileFlags & wxDIR))
	return wxFindNextFile();
    } else if (wxFindFileFlags && !(wxFindFileFlags & wxFILE))
		return wxFindNextFile();

	 if (wxBuffer[0] != 0)
		strcat(wxBuffer, "\\");

#ifdef __BORLANDC__
	 strcat(wxBuffer, wxFileStruc.ff_name);
#else
	 strcat(wxBuffer, wxFileStruc.name);
#endif
	 return wxBuffer;
  }
  else
    return NULL;
#endif // WIN32
}

char *wxFindNextFile(void)
{
  // Find path only so we can concatenate
  // found file onto path
  char *p = wxPathOnly(wxFileSpec);
  if (p && (strlen(p) > 0))
	 strcpy(wxBuffer, p);
  else
	 wxBuffer[0] = 0;
  
 try_again:

#ifdef WIN32
  if (wxFileStrucHandle == INVALID_HANDLE_VALUE)
	 return NULL;

  Bool success = FindNextFile(wxFileStrucHandle, &wxFileStruc);
  if (!success) {
		FindClose(wxFileStrucHandle);
      wxFileStrucHandle = INVALID_HANDLE_VALUE;
		return NULL;
  }

  Bool isdir = !!(wxFileStruc.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
  
  if (isdir && !(wxFindFileFlags & wxDIR))
    goto try_again;
  else if (!isdir && wxFindFileFlags && !(wxFindFileFlags & wxFILE))
	 goto try_again;

  if (wxBuffer[0] != 0)
    strcat(wxBuffer, "\\");
  strcat(wxBuffer, wxFileStruc.cFileName);
  return wxBuffer;  
#else

#ifdef __BORLANDC__
  if (findnext(&wxFileStruc) == 0)
#else
  if (_dos_findnext(&wxFileStruc) == 0)
#endif
  {
    /* MATTHEW: [5] Check directory flag */
    char attrib;

#ifdef __BORLANDC__
    attrib = wxFileStruc.ff_attrib;
#else
    attrib = wxFileStruc.attrib;
#endif

    if (attrib & _A_SUBDIR) {
      if (!(wxFindFileFlags & wxDIR))
	goto try_again;
    } else if (wxFindFileFlags && !(wxFindFileFlags & wxFILE))
      goto try_again;


	 if (wxBuffer[0] != 0)
      strcat(wxBuffer, "\\");
#ifdef __BORLANDC__
	 strcat(wxBuffer, wxFileStruc.ff_name);
#else
	 strcat(wxBuffer, wxFileStruc.name);
#endif
	 return wxBuffer;
  }
  else
    return NULL;
#endif
}

// Get current working directory.
// If buf is NULL, allocates space using new, else
// copies into buf.
char *wxGetWorkingDirectory(char *buf, int sz)
{
  if (!buf)
    buf = new char[1000];
#ifdef __BORLANDC__
  (void)getcwd(buf, sz);
#elif __WATCOMC__
  (void)getcwd(buf, sz);
#else
  (void)_getcwd(buf, sz);
#endif
  return buf;
}

Bool wxSetWorkingDirectory(char *d)
{
  Bool success = (chdir(d) == 0);

  // Must change drive, too.
  // How is this implemented in WIN32/NT???

#if !defined(WIN32)
  Bool isDriveSpec = ((strlen(d) > 1) && (d[1] == ':'));
  if (isDriveSpec)
  {
    char firstChar = d[0];

    // To upper case
    if (firstChar > 90)
      firstChar = firstChar - 32;

    // To a drive number
    unsigned int driveNo = firstChar - 64;
    if (driveNo > 0)
    {
       unsigned int noDrives;
       _dos_setdrive(driveNo, &noDrives);
    }
  }
#endif
  return success;
}

// Get free memory in bytes, or -1 if cannot determine amount (e.g. on UNIX)
long wxGetFreeMemory(void)
{
  return (long)GetFreeSpace(0);
}

/* MATTHEW: [5] switch */
#if !WX_USE_GLOBAL_SLEEP
// Sleep for nSecs seconds. Attempt a Windows implementation using timers.
static Bool inTimer = FALSE;
class wxSleepTimer: public wxTimer
{
 public:
  inline void Notify(void)
  {
    inTimer = FALSE;
    Stop();
  }
};
static wxTimer *wxTheSleepTimer = NULL;
#endif

void wxSleep(int nSecs)
{
/* MATTHEW: [5] switch */
#if !WX_USE_GLOBAL_SLEEP
  if (inTimer)
    return;

  wxTheSleepTimer = new wxSleepTimer;
  inTimer = TRUE;
  wxTheSleepTimer->Start(nSecs*1000);
  while (inTimer)
  {
    if (wxTheApp->Pending())
      wxTheApp->Dispatch();
}
  delete wxTheSleepTimer;
  wxTheSleepTimer = NULL;
#else
  _sleep(nSecs);
#endif
}

// Consume all events until no more left
void wxFlushEvents(void)
{
}

// Output a debug mess., in a system dependent fashion.
void wxDebugMsg(const char *fmt ...)
{
  va_list ap;
  static char buffer[512];

  if (!wxTheApp->wantDebugOutput)
    return ;

  va_start(ap, fmt);

  wvsprintf(buffer,fmt,ap) ;
  OutputDebugString((LPCSTR)buffer) ;

  va_end(ap);
}

// Non-fatal error: pop up message box and (possibly) continue
void wxError(const char *msg, const char *title)
{
  sprintf(wxBuffer, "%s\nContinue?", msg);

  int msAns;

#ifndef USE_SEP_WIN_MANAGER
  msAns = MessageBox(NULL, (LPCSTR)wxBuffer, (LPCSTR)title,
							MB_ICONSTOP | MB_YESNO);
#else
  wxwmMessageBox r;

  r.owner = NULL;
  r.text = (LPCSTR)wxBuffer;
  r.title = (LPCSTR)title;
  r.style = MB_ICONSTOP | MB_YESNO;
  wxwmMessage(WXM_MESSAGE_BOX, (LPARAM)&r);
  msAns = r.result;
#endif

  if (msAns == IDNO)
	 wxExit();
}

// Fatal error: pop up message box and abort
void wxFatalError(const char *msg, const char *title)
{
  sprintf(wxBuffer, "%s: %s", title, msg);
  FatalAppExit(0, (LPCSTR)wxBuffer);
}

// Emit a beeeeeep
void wxBell(void)
{
// #ifdef WIN32
//  Beep(1000,1000) ;	// 1kHz during 1 sec.
// #else
  MessageBeep(MB_OK) ;
// #endif
}

int wxGetOsVersion(int *majorVsn, int *minorVsn)
{
  DWORD Version = GetVersion();
  WORD  lowWord = LOWORD(Version);  
  
  
  BOOL  Win32s = ( Version & 0x80000000 );
  BOOL  Win95   = (( Version & 0xFF ) >= 4);
  BOOL  WinNT   = Version < 0x80000000;
  
  // Get the version number
  if (majorVsn)
          *majorVsn = LOBYTE(lowWord);
  if (minorVsn)
          *minorVsn = HIBYTE(lowWord);

  if (Win95)
    return wxWINDOWS_NT /* wxWIN95 */;
  else if (Win32s)
    return wxWIN32S;
  else if (WinNT)
    return wxWINDOWS_NT;
  else
    return wxWINDOWS;
}

// Reading and writing resources (eg WIN.INI, .Xdefaults)
#if USE_RESOURCES
Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file)
{
  if (file)
    return WritePrivateProfileString((LPCSTR)section, (LPCSTR)entry, (LPCSTR)value, (LPCSTR)file);
  else
    return WriteProfileString((LPCSTR)section, (LPCSTR)entry, (LPCSTR)value);
}

Bool wxWriteResource(const char *section, const char *entry, float value, const char *file)
{
  char buf[50];
  sprintf(buf, "%.4f", value);
  return wxWriteResource(section, entry, (char *)buf, file); /* MATTHEW: BC */
}

Bool wxWriteResource(const char *section, const char *entry, long value, const char *file)
{
  char buf[50];
  sprintf(buf, "%ld", value);
  return wxWriteResource(section, entry, (char *)buf, file); /* MATTHEW: BC */
}

Bool wxWriteResource(const char *section, const char *entry, int value, const char *file)
{
  char buf[50];
  sprintf(buf, "%d", value);
  return wxWriteResource(section, entry, (char *)buf, file); /* MATTHEW: BC */
}

static char *wxUserResourceFile;

void wxInitUserResource(char *s)
{
   wxUserResourceFile = s;
}

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file)
{
  static const char defunkt[] = "$$default";
  int no_file = !file;

  if (!file)
    file = wxUserResourceFile;

  wxBuffer[0] = 0;

  if (file)
  {
    int n = GetPrivateProfileString((LPCSTR)section, (LPCSTR)entry, (LPCSTR)defunkt,
                                    (LPSTR)wxBuffer, 1000, (LPCSTR)file);
    if (n == 0 || strcmp(wxBuffer, defunkt) == 0)
     return FALSE;
  }
  
  if (!no_file) {
    int n = GetProfileString((LPCSTR)section, (LPCSTR)entry, (LPCSTR)defunkt,
                                    (LPSTR)wxBuffer, 1000);
    if (n == 0 || strcmp(wxBuffer, defunkt) == 0)
      return FALSE;
  }
  if (*value) delete[] (*value);
      *value = copystring(wxBuffer);
      return TRUE;
}

Bool wxGetResource(const char *section, const char *entry, float *value, const char *file)
{
  char *s = NULL;
  Bool succ = wxGetResource(section, entry, (char **)&s, file); /* MATTHEW: BC */
  if (succ)
  {
    *value = (float)strtod(s, NULL);
    delete[] s;
    return TRUE;
  }
  else return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value, const char *file)
{
  char *s = NULL;
  Bool succ = wxGetResource(section, entry, (char **)&s, file); /* MATTHEW: BC */
  if (succ)
  {
    *value = strtol(s, NULL, 10);
    delete[] s;
    return TRUE;
  }
  else return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value, const char *file)
{
  char *s = NULL;
  Bool succ = wxGetResource(section, entry, (char **)&s, file); /* MATTHEW: BC */
  if (succ)
  {
    *value = (int)strtol(s, NULL, 10);
    delete[] s; 
    return TRUE;
  }
  else return FALSE;
}
#endif // USE_RESOURCES

// Old cursor
static HCURSOR wxBusyCursorOld = 0;
static int wxBusyCursorCount = 0;

extern int wxGetBusyState();
extern void wxSetBusyState(int);

// Set the cursor to the busy cursor for all windows
void wxBeginBusyCursor(wxCursor *cursor)
{
  wxBusyCursorCount = wxGetBusyState();
  wxBusyCursorCount++;
  wxSetBusyState(wxBusyCursorCount);

  if (wxBusyCursorCount == 1)
    wxBusyCursorOld = ::SetCursor(cursor->ms_cursor);
  else
    (void)::SetCursor(cursor->ms_cursor);
}

// Restore cursor to normal
void wxEndBusyCursor(void)
{
  wxBusyCursorCount = wxGetBusyState();
  if (wxBusyCursorCount == 0)
    return;
  --wxBusyCursorCount;
  wxSetBusyState(wxBusyCursorCount);
    
  if (wxBusyCursorCount == 0)
  {
    ::SetCursor(wxBusyCursorOld);
    wxBusyCursorOld = 0;
  }
}

// TRUE if we're between the above two calls
Bool wxIsBusy(void)
{
  return (wxGetBusyState() > 0);
}    

// Hack for MS-DOS
char *wxGetUserHome (const char *user)
{
  char *home;
  if (user && *user) {
    char tmp[64];
    if (wxGetUserId(tmp, sizeof(tmp)/sizeof(char))) {
      // Guests belong in the temp dir
      if (stricmp(tmp, "annonymous") == 0) {
	if ((home = getenv("TMP")) != NULL ||
	    (home = getenv("TMPDIR")) != NULL ||
	    (home = getenv("TEMP")) != NULL)
	  return *home ? home : "\\";
      }
      if (stricmp(tmp, user) == 0)
	user = NULL;
    }
  }
  if (user == NULL || *user == '\0')
    if ((home = getenv("HOME")) != NULL)
    {
      strcpy(wxBuffer, home);
      Unix2DosFilename(wxBuffer);
      return wxBuffer;
    }
  return NULL; // No home known!
}

