/*
 * File:	wx_utils.cc
 * Purpose:	Various utilities
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#include "wx.h"

#include <ctype.h>
#include <direct.h>

#include <dos.h>

#define WX_USE_GLOBAL_SLEEP 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
  char *sysname;
  const char *default_host = "noname";

  if ((sysname = getenv("SYSTEM_NAME")) == NULL) {
     GetProfileString(WX_SECTION, eHOSTNAME, default_host, buf, maxSize - 1);
  } else
    strncpy(buf, sysname, maxSize - 1);
  buf[maxSize] = '\0';
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

  // Could use NIS, MS-Mail or other site specific programs
  // Use wxWindows configuration data 
  GetProfileString(WX_SECTION, eUSERNAME, default_name, buf, maxSize - 1);

  return *buf ? TRUE : FALSE;
}

Bool wxRemoveFile(const char *file)
{
  int flag = remove(file);
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
  WIN32_FIND_DATA fileInfo;

  HANDLE h = FindFirstFile((LPTSTR)dir,(LPWIN32_FIND_DATA)&fileInfo);

  if (h==INVALID_HANDLE_VALUE)
    return FALSE;
  else {
    FindClose(h);
    return (fileInfo.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY);
  }
}

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName(const char *prefix, char *buf)
{
  char tmp[MAX_PATH];
  char tmpPath[MAX_PATH];
  ::GetTempPath(MAX_PATH, tmpPath);
  ::GetTempFileName(tmpPath, prefix, 0, tmp);

  if (buf) strcpy(buf, tmp);
  else buf = copystring(tmp);
  return buf;
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

  msAns = MessageBox(NULL, (LPCSTR)wxBuffer, (LPCSTR)title,
							MB_ICONSTOP | MB_YESNO);

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
  MessageBeep(MB_OK) ;
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

  if (file) {
    int n = GetPrivateProfileString((LPCSTR)section, (LPCSTR)entry, (LPCSTR)defunkt,
                                    (LPSTR)wxBuffer, 1000, (LPCSTR)file);
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

extern int wxGetBusyState();
extern void wxSetBusyState(int);

extern void wxResetCurrentCursor(void);

extern HCURSOR wxMSWSetCursor(HCURSOR c);

// Set the cursor to the busy cursor for all windows
void wxBeginBusyCursor(wxCursor *cursor)
{
  int wxBusyCursorCount = wxGetBusyState();
  wxBusyCursorCount++;
  wxSetBusyState(wxBusyCursorCount);

  if (wxBusyCursorCount == 1)
    wxMSWSetCursor(cursor->ms_cursor);
  else
    (void)wxMSWSetCursor(cursor->ms_cursor);
}

// Restore cursor to normal
void wxEndBusyCursor(void)
{
  int wxBusyCursorCount = wxGetBusyState();
  if (wxBusyCursorCount == 0)
    return;
  --wxBusyCursorCount;
  wxSetBusyState(wxBusyCursorCount);
    
  if (wxBusyCursorCount == 0) {
    wxResetCurrentCursor();
  }
}

// TRUE if we're between the above two calls
Bool wxIsBusy(void)
{
  return (wxGetBusyState() > 0);
}    

