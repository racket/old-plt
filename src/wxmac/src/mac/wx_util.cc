#include "wx.h"
#include "wx_utils.h"
#include "wx_list.h"
#include "wx_main.h"
#include <stdarg.h>
#include <ctype.h>
#ifdef WX_CARBON
#include <sys/types.h>
#endif
#include <unistd.h>
#ifndef WX_CARBON
# include <Strings.h>
# include <Gestalt.h>
# include <Files.h>
# include <Sound.h>
# include <Folders.h>
# include <PPCToolbox.h>
extern "C" long atol(char *);
extern "C" int atoi(char *);
#endif

extern "C" {
#ifdef WX_CARBON
# include <sys/stat.h>
#else
# include <stat.h>
#endif        
}

static int wxFindFileFlags = 0;

#ifndef OS_X
extern "C" {
#endif
  extern char *scheme_mac_spec_to_path(FSSpec *spec);
#ifndef OS_X
}
#endif

// Get a temporary filename, opening and closing the file.
char *wxGetTempFileName (const char *prefix, char *dest)
{
  static char *temp_folder;
  static int temp_len;
  static short last_temp = 0;	// cache last to speed things a bit
  // At most 1000 temp files to a process! We use a ring count.
  char *buf;

  if (!temp_folder) {
    FSSpec spec;
    SInt16 vRefNum;
    SInt32 dirID;
    const Str255 fileName = "\p";
    
    if (FindFolder(kOnSystemDisk, 'temp', kCreateFolder, &vRefNum, &dirID) == noErr) {
      wxREGGLOB(temp_folder);
      FSMakeFSSpec(vRefNum,dirID,fileName,&spec);
      temp_folder = scheme_mac_spec_to_path(&spec);
    }
    else
      temp_folder = "";
    temp_len = strlen(temp_folder);
  }
  
  if (!prefix)
    prefix = "";
  else {
    int i;
    for (i = 0; prefix[i]; i++)
      if (prefix[i] == ':') {
        prefix = "";
        break;
      }
    if (i > 15)
      prefix = "";
  }
  
  buf = new char[temp_len + strlen(prefix) + 20];

  for (short suffix = last_temp + 1; suffix != last_temp; ++suffix %= 1000)
    {
      struct stat stbuf;
      sprintf (buf, "%s�%s%d", temp_folder, prefix, (int) suffix); // CJC FIXME - really should get a temp folder
      if (stat ((char *)buf, &stbuf) != 0)
	{
	  // Touch the file to create it (reserve name)
	  FILE *fd = fopen (buf, "w");
	  if (fd)
	    fclose (fd);
	  last_temp = suffix;
          if (dest)
	    strcpy(dest, buf);
	  else
	    dest = copystring(buf);
	  return dest;
	}
    }
  if (dest) dest[0] = 0;
  return NULL;
}

void wxRemoveFile(char *filename)
{
  unlink(filename);
}

// Get free memory in bytes, or -1 if cannot determine amount (e.g. on UNIX)
long 
wxGetFreeMemory (void)	// CJC FIXME this could be Macintized.
{
  return -1;
}

// Sleep for nSecs seconds.
// XView implementation according to the Heller manual
void 
wxSleep (int nSecs)
{
  // sleep (nSecs);
}



// Old cursor
static int wxBusyCursorCount = FALSE;

extern int wxGetBusyState();
extern void wxSetBusyState(int);

// Set the cursor to the busy cursor for all windows
void 
wxBeginBusyCursor (wxCursor * cursor)
{
  int s = wxGetBusyState();
  wxSetBusyState(s + 1);

  if (!s) {
    for (wxChildNode *node = wxTopLevelWindows(NULL)->First(); node; node = node->Next()) {
      wxFrame *f = (wxFrame *)node->Data();
      f->cBusyCursor = 1;
    }

    wxTheApp->AdjustCursor();
  }
}

// Restore cursor to normal
void 
wxEndBusyCursor (void)
{
  int s = wxGetBusyState();
  wxSetBusyState(s - 1);

  if (s == 1) {
    for (wxChildNode *node = wxTopLevelWindows(NULL)->First(); node; node = node->Next()) {
      wxFrame *f = (wxFrame *)node->Data();
      f->cBusyCursor = 0;
    }
    
    wxTheApp->AdjustCursor();
  }
}

// TRUE if we're between the above two calls
Bool 
wxIsBusy (void)
{
  return wxGetBusyState() > 0;
}

Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file)
{
  return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, float value, const char *file)
{
  return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, long value, const char *file)
{
  return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, int value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, float *value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value, const char *file)
{
  return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value, const char *file)
{
  return FALSE;
}

void wxBell()
{
  SysBeep(0);
}

int wxGetOsVersion(int *a, int *b)
{
  long systemVersion;
  
  ::Gestalt(gestaltSystemVersion,&systemVersion);

  *a = 10 * ((systemVersion >> 12) & 0xF) +
    ((systemVersion >>  8) & 0xF);
  *b =      ((systemVersion >>  4) & 0xF);
  // sub-version num (bottom four bits) is ignored
  
  return wxMACINTOSH;
}

// Output a debug mess., in a system dependent fashion.
void 
wxDebugMsg (const char *fmt...)
{
  va_list ap;
  char buffer[BUFSIZ];

  if (!wxTheApp->wantDebugOutput)
    return ;

  va_start (ap, fmt);

  vsprintf (buffer, fmt, ap);
  // cerr << buffer;

  va_end (ap);
}


// Get hostname.
Bool wxGetHostName(char *buf, int maxSize)
{	Bool good = FALSE;

 if (maxSize>9)
   {	strcpy(buf,"Macintosh");
   good = TRUE;
   }
 return good;
}

// Get user ID e.g. jacs
Bool wxGetUserId(char *buf, int maxSize)
#ifdef WX_CARBON
{
  CFStringRef username;
  
  username = CSCopyUserName(true);
  return CFStringGetCString(username, buf,maxSize,kCFStringEncodingISOLatin1);
}
#else
{	return wxGetUserName(buf,maxSize); }
#endif

// Get user name e.g. Julian Smart
Bool wxGetUserName(char *buf, int maxSize)
#ifdef WX_CARBON
{
  CFStringRef username;
  
  username = CSCopyUserName(false);
  return CFStringGetCString(username, buf,maxSize,kCFStringEncodingISOLatin1);
}
#else    
{	Bool good = FALSE;
 unsigned long userRef;
 Str32 name;
	
 if (maxSize>32)
   {	good = GetDefaultUser( &userRef, name) == noErr;
   if (good) {
     CopyPascalStringToC(name,buf);
   }
   }
 return good;
}


#endif

#ifdef WX_CARBON

/* wxFSRefToPath converts an FSRef to a path.  This code is copied from 
 * scheme_mac_spec_to_path in MzScheme, but I don't want to widen the 
 * MzScheme interface any more than I already have.
 * 
 * May return NULL.
 */
char *wxFSRefToPath(FSRef fsref)
{
  int longEnough = FALSE;
  OSErr err;
  int strLen = 256;
  char *str;
  
  str = new WXGC_ATOMIC char[strLen];
  
  while (! longEnough) {
    err = FSRefMakePath(&fsref,(unsigned char *)str,strLen);
    if (err == pathTooLongErr) {
      strLen *= 2;
      str = new WXGC_ATOMIC char[strLen];
    } else if (err == noErr) {
      longEnough = TRUE;
    } else {
      return NULL;
    }
  }
  
  return str;
}

#endif
