/*
   FIXME - This needs substantial work
   10/7/95 - This is just enough to link wxPython v2 and extend - untested and/or stubbed
   
   One day, I'll rename it to wx_utils.cc 
   
   Some (most of these functions could be based on GUSI or Mac-Python. There ought
   to be a choice. (Easier than rebuilding python to use GUSI).
*/
#include "wx.h"
#include "wx_utils.h"
#include "wxstring.h"
#include "wx_list.h"
#include <Strings.h>
#include "wx_main.h"
#include <stdarg.h>
#include <ctype.h>
#include <unistd.h>
#if 1
#include <Files.h>
#include <PPCToolbox.h>
extern "C" long atol(char *);
extern "C" int atoi(char *);
#else
#if defined(PYLIB)
extern "C" {
//	#include "MacDefs.h"
//	#include "macstat.h"
	#include <stat.h>
	#include <Files.h>
//	#define S_ISDIR(x) (x & S_IFDIR)
	#include "dirent.h"
	#include "nfullpath.h"
//	extern "C" mkdir(const char *);
	extern "C" rmdir(const char *);
	extern "C" chdir(const char *);
	extern "C" sleep(int);
	extern "C" char *getcwd(char *, int);
	extern "C" long atol(char *);
	extern "C" int atoi(char *);
}
#elif defined(GUSI)
#include "GUSI.h"
#else
#error "GUSI or PYLIB required for this Module"
#endif
#endif

extern "C" {
	#include <stat.h>
}

#if 0
static DIR *wxDirStream = NULL;
#endif
static char *wxFileSpec = NULL;
static int wxFindFileFlags = 0;

extern "C" {
  extern char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
};

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
    if (!FindFolder(kOnSystemDisk, 'temp', kCreateFolder, &spec.vRefNum, &spec.parID))
	  temp_folder = scheme_build_mac_filename(&spec, 1);
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
      sprintf (buf, "%s¥%s%d", temp_folder, prefix, (int) suffix); // CJC FIXME - really should get a temp folder
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

// Some additions from Louis Birk
int AddOrReplaceEntry(const char *section, const char *entry, char *Value, const char *file);
int GetEntry(const char *section, const char *entry, char *Value, const char *file);

wxNode *EntryMember (wxList *, const char *, int );
wxNode *EntryMember (wxList *list, const char *s, int length)
{
  for (wxNode * node = list->First (); node; node = node->Next ())
    {
      const char *s1 = (const char *) ((wxString *)(node->Data()))->GetData();
      if (s == s1 || strncmp (s, s1, length) == 0)
	return node;
    }
  return 0;
}

wxNode *SectionMember (wxList *, const char *, int );
wxNode *SectionMember (wxList *list, const char *s, int length)
{
  for (wxNode * node = list->First (); node; node = node->Next ())
    {
      const char *s1 = (const char *) ((wxString *)(node->Data()))->GetData();
      if (s == s1 || strncmp (s, s1, length) == 0)
	return node;
    }
  return 0;
}

wxList *GetStringList(FILE *fd);
int WriteStringList(FILE *fd, wxList *sList);

static char *wxResourceFile;
static wxList *rmain;

void wxInitResources(char *s);

void wxInitResources(char *s)
{
   wxResourceFile = s;
   
   FILE *fd;
   
   if ( (fd = fopen(s, "r+")) == 0 ) // file note there
     rmain = NULL;
   else {
    // find the section or create it
    rmain = GetStringList(fd);

    fclose(fd);
   }
}

int AddOrReplaceEntry(const char *section, const char *entry, char *Value, const char *file)
{
  FILE *fd;
  wxList *sList;
  wxNode *sNode;
  wxNode *eNode;
  char Section[256];

  if (!file)
    return FALSE;

  if ( (fd = fopen(file, "r+")) == 0 ) {
    // file not there
    if ( (fd = fopen(file, "w")) == 0 )
      return FALSE;
    sList = new wxList();
  } else {
   // find the section or create it
    sList = GetStringList(fd);
  }
  
  // make a section
  sprintf(Section, "[%s]", section);

  if ( sNode = SectionMember(sList, Section, strlen(Section)))
  {
    if ( (eNode = EntryMember(sList, entry, strlen(entry))) )
    {
      // replace with new entry
      sList->Insert(eNode, (new wxString(Value) ));

      // delete the old entry
      sList->DeleteNode(eNode); 
    }
    else // section there, entry missing, add it
    {
      // add the entry
      wxNode *afterSection = sNode->Next();

      // replace with new entry
      sList->Insert(afterSection, (new wxString(Value) ));

    }
  }
  else // section missing, add it
  {
    // add the section
    sList->Append(new wxString(Section) );
    // add the entry
    sList->Append(new wxString(Value) );
  }

  int retval;

  if (WriteStringList(fd, sList))
    retval = TRUE;
  else
    retval = FALSE;

  fclose(fd);

  return retval;
}

int GetEntry(const char *section, const char *entry, char *Value, const char *file)
{
  FILE *fd;
  wxList *sList;
  wxNode *sNode;
  wxNode *eNode;
  char Section[256];

  if (!file) {
    if (!rmain)
  	  return FALSE;
  	sList = rmain;
  } else {
    if ( (fd = fopen(file, "r+")) == 0 ) // file note there
      return FALSE;

    // find the section or create it
    sList = GetStringList(fd);

    fclose(fd);
  }

  // make a section
  sprintf(Section, "[%s]", section);

  if ( sNode = SectionMember(sList, Section, strlen(Section)))
  {
    if ( (eNode = EntryMember(sList, entry, strlen(entry))) )
    {
      const char *s1 = (const char *) ((wxString *)(eNode->Data()))->GetData();
        /* This fixes the blanks/tabs etc. in front of the Value
       	 Thomas Fettig fettig@dfki.uni-sb.de  06-dec-95 */
      int i = 1;
      char *s2 = strstr(s1, "=");
      while (isspace(s2[i]) && (s2[i] !='\0'))
       	i++;
      strcpy(Value,s2+i);
      return TRUE;
    }
   }

  return FALSE;

}

wxList *GetStringList(FILE *fd)
{
  wxList *theList = new wxList();
  char *line;
  char line2[256];

  while ( (line = fgets(line2, sizeof(line2), fd)) != 0)
  {
    int len = strlen(line);
    while (len && (line[len - 1] == '\n')) {
      line[--len] = 0;
    }
    theList->Append(new wxString(line));
  }
  if (theList->Number() > 0)
    return theList;
  else
    return 0;
}

int WriteStringList(FILE *fd, wxList *sList)
{
  int err;

  fseek(fd, 0, SEEK_SET);
  for (wxNode * node = sList->First (); node; node = node->Next ())
    {
      const char *s1 = (const char *) ((wxString *)(node->Data()))->GetData();
      if ( (err = fputs(s1, fd)) != 0)
        return FALSE;
      fputs("\n", fd);
    }
  return TRUE;

}

// Resource additions from Louis Birk - All entry names must be unique!
//

Bool wxWriteResource(const char *section, const char *entry, char *value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%s", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, float value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%f", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, long value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%d", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxWriteResource(const char *section, const char *entry, int value, const char *file)
{
  char Value[256];

  sprintf(Value, "%s=%d", entry, value);

  if ( AddOrReplaceEntry(section, entry, Value, file) )
    return TRUE;
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, char **value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    char *s = copystring(Value);
    value[0] = s;
    return TRUE;
  }
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, float *value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    sscanf(Value, "%f", value);
    return TRUE;
  }
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    value[0] = atol(Value);
    return TRUE;
  }
  else
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value, const char *file)
{
  char Value[256];

  if (GetEntry(section, entry, Value, file))
  {
    value[0] = atoi(Value);
    return TRUE;
  }
  else
    return FALSE;
}

void wxBell()
{
	SysBeep(0);
}

int wxGetOsVersion(int *a, int *b)
{
  SysEnvRec sysEnvRec;
  ::SysEnvirons(2, &sysEnvRec);

  *a = sysEnvRec.systemVersion >> 8;
  *b = (sysEnvRec.systemVersion >> 4) & 0xF;

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


//------ the following should be in wx_utils.cc ----------------

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
{	return wxGetUserName(buf,maxSize); }

// Get user name e.g. Julian Smart
Bool wxGetUserName(char *buf, int maxSize)
{	Bool good = FALSE;
	unsigned long userRef;
	Str32 name;
	
	if (maxSize>32)
	{	good = GetDefaultUser( &userRef, name) == noErr;
	  /* MATTHEW: [5] */
	  if (good) {
#if defined(PPCC) || defined(MrCpp) || defined(GUSI) || defined(PYLIB)
		p2cstr(name);
#else
		PtoCstr(name);
#endif
		strcpy(buf,(char *)name);
	  }
	}
	return good;
}
