
#ifdef WX_CARBON
# ifdef OS_X
#  include <Carbon/Carbon.h>
# else
#  include <Carbon.h>
# endif
#else
# include <Files.h>
# include <EPPC.h>
# include <AppleEvents.h>
# include <Events.h>
#endif

#ifndef FOR_STARTER
# include "scheme.h"
# include <ctype.h>
# include <string.h>
#else
# define scheme_malloc_atomic NewPtr
# define scheme_malloc NewPtr
# define memcpy(d, s, l) BlockMove(s, d, l)

extern int strlen(char *);
extern int isspace(int);

static void strcpy(char *s, char *d)
{
  while (*d) *(s++) = *(d++);
  *s = 0;
}
#endif

#include "simpledrop.h"

int scheme_mac_ready, scheme_mac_argc = 0;
char **scheme_mac_argv;

#ifdef OS_X
void GetStarterInfo();
#endif

static char *ThisAppName(void)
{	
#ifndef FOR_STARTER
# ifdef OS_X
  return "not used";
# else
  char *result, *dir;
  OSErr err;
  ProcessInfoRec info;
  ProcessSerialNumber curPSN;
  Str255 buffer;
  int dlen;
  
  err = GetCurrentProcess(&curPSN);
  
  info.processInfoLength = sizeof(ProcessInfoRec);
  info.processName = buffer;
  info.processAppSpec = NULL;
  
  err = GetProcessInformation(&curPSN, &info);
  
  dir = scheme_os_getcwd(NULL, 0, NULL, 1);
  dlen = strlen(dir);
  
  result = (char *)scheme_malloc_atomic(buffer[0] + dlen + 2);
  memcpy(result, dir, dlen);
  result[dlen] = ':';
  memcpy(result + dlen + 1, buffer + 1, buffer[0]);
  result[buffer[0] + dlen + 1] = 0;
  
  return result;
# endif
#else
  return "starter";
#endif
}

static void parse_commandline(char *s, char *src, int addon)
{
  char *token, *pos, ender;
  int count = 0, i;
  char *command[32];

  token = s;
  while (*token && (count < 32)) {
    while (isspace(*token)) token++;
    if (!*token) break;

    pos = token;
    command[count] = pos;
    while (*token && !isspace(*token)) {
      if (*token == '"') {
	ender = '"';
	token++;
      } else if (*token == '\'') {
	ender = '\'';
	token++;
      } else
	ender = 0;
      
      if (ender) {
	while (*token && (*token != ender))
	  *(pos++) = *(token++);
	if (*token)
	  token++;
      } else
	*(pos++) = *(token++);
    }
    if (*token)
      token++;
    *pos = 0;

#ifndef FOR_STARTER
    if (src && !strcmp(command[count], "%%")) {
      /* Replace %% with file name */
      command[count] = src;
    } else if (src && (command[count][0] == '%') && (command[count][1]) == ':') {
      /* Replace % with file directory */
      char *s, *r;
      int i;
      i = strlen(command[count]) + strlen(src);
      r = (char *)scheme_malloc_atomic(i + 1);
      s = scheme_strdup(src);
      i = strlen(s) - 1;
      while (i && s[i] != ':')
	i--;
      s[i + 1] = 0;
      strcpy(r, s);
      strcat(r, command[count] + 2);
      command[count] = r;
    }
#endif

    count++;
  }	  	
  
  scheme_mac_argc = 1 + count + (addon ? 1 : 0);
  scheme_mac_argv = (char **)scheme_malloc(scheme_mac_argc * sizeof(char *));
  scheme_mac_argv[0] = ThisAppName();
  for (i = 0; i < count; i++) {
    scheme_mac_argv[i + 1] = (char *)scheme_malloc_atomic(strlen(command[i]) + 1);
    strcpy(scheme_mac_argv[i + 1], command[i]);
  }
  if (addon)
    scheme_mac_argv[count + 1] = src;
}

extern void ParseLine(char *s, int *argc, char ***argv)
{
  parse_commandline(s, NULL, 0);
  *argc = scheme_mac_argc;
  *argv = scheme_mac_argv;
}

static void Startup(char **argv, int argc)
{
  int i;
  
  scheme_mac_ready = 1;
  
  if (!argc) {
    scheme_mac_argc = 1;
    scheme_mac_argv = (char **)scheme_malloc(sizeof(char *));
    scheme_mac_argv[0] = ThisAppName();
    return;
  }

  scheme_mac_argv = NULL;

#if !defined(FOR_STARTER) && !defined(OS_X)
  if (argc == 1) {
    /* See if this file has startup flags */
    char buf[2048];
    FILE *f = fopen(argv[0], "r");
    buf[0] = 0;
    if (f) {
      fgets(buf, 2048, f);
      fclose(f);
    }
    if (buf[0] == '#' && buf[1]  == '!') {
      char *s;
      int l;
      
      s = buf + 2;
      while (*s && !isspace(*s))
	s++;
      while (*s && isspace(*s))
	s++;
      
      l = strlen(s);
      while (l && isspace(s[l - 1])) {
	--l;
	s[l] = 0;
      }
      
      if (*s) {
	/* Yes, it does. */
	parse_commandline(s, argv[0], buf[2] == '!');
      }
    }  
  } 
  
  if (!scheme_mac_argv) {
    scheme_mac_argc = (argc ? argc + 2 : 1);
    scheme_mac_argv = (char **)scheme_malloc(scheme_mac_argc * sizeof(char *));
    for (i = 0; i < argc; i++)
      scheme_mac_argv[i + 2] = argv[i];
    if (argc)
      scheme_mac_argv[1] = "-F";
    
    scheme_mac_argv[0] = ThisAppName();
  }
#else
  scheme_mac_argc = argc + 1;
  scheme_mac_argv = (char **)scheme_malloc(scheme_mac_argc * sizeof(char *));
  for (i = 0; i < argc; i++)
    scheme_mac_argv[i + 1] = argv[i];
  scheme_mac_argv[0] = ThisAppName();
#endif
}

static int gone = 0;

#ifdef OS_X
extern char *scheme_mac_spec_to_path(FSSpec *spec);
#endif

static pascal short DoNothing(const AppleEvent *a, AppleEvent *b, long c)
{
  return 0;
}

static pascal OSErr OpenApplicationStuff(const AppleEvent *a, AppleEvent *b, long c)
{
  if (!gone) {
    gone = 1;
    Startup(NULL, 0);
  }
  
  return 0;
}

static pascal OSErr OpenFinderDoc(const AppleEvent *evt, AppleEvent *b, long c)
{
  AEDescList	docList;
  long		count, size;
  short		i, loadedAlready=0, j;
  DescType	retType;
  AEKeyword	keywd;
  FSSpec		fss;
  char        **files;
  
  AEGetParamDesc(evt, keyDirectObject, typeAEList, &docList);
  AECountItems(&docList, &count);
  files = (char **)scheme_malloc(sizeof(char *) * count);
  j = 0;
  for (i = 0; i < count; i++){
    AEGetNthPtr(&docList, i + 1, typeFSS, &keywd, &retType, (Ptr)&fss, sizeof(FSSpec), &size);
    files[i + j] = scheme_mac_spec_to_path(&fss);
    if (!files[i + j])
      --j;
  }
  AEDisposeDesc(&docList);
  
  if (!gone) {
    gone = 1;
    Startup(files, count + j);
  } else {
    Drop_Runtime(files, count + j);
  }
  
  return 0;
}

static pascal OSErr CmdLineMessage(const AppleEvent *evt, AppleEvent *b, long c)
{
  AEDescList	cmdList;
  DescType	retType;
  AEKeyword	keywd;
  char          *cmdLine;
  long          size;

  AEGetParamDesc(evt, keyDirectObject, typeAEList, &cmdList);
  size = 1023;
  cmdLine = (char *)NewPtr(size + 1);
  AEGetNthPtr(&cmdList, 1, typeChar, &keywd, &retType, (Ptr)cmdLine, size, &size);

#if 0
  {
    char buf[256];
    sprintf(buf + 1, "cmdline %d '%4.4s' %20.20s", size, &retType, cmdLine);
    buf[0] = strlen(buf);
    DebugStr((unsigned char *)buf);
  }
#endif

  cmdLine[size] = 0;
  scheme_mac_ready = 1;
  parse_commandline(cmdLine, NULL, 0);
  
  return 0;
}

static pascal OSErr SetUpQuitMessage(const AppleEvent *a, AppleEvent *b, long c)
{
}

static void Install(void)
{
  short err=0;
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenApplication, NewAEEventHandlerUPP(OpenApplicationStuff), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments, NewAEEventHandlerUPP(OpenFinderDoc), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments, NewAEEventHandlerUPP(DoNothing), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, NewAEEventHandlerUPP(SetUpQuitMessage), 0, 0);
  err = AEInstallEventHandler('PLT ', 'cmdl', NewAEEventHandlerUPP(CmdLineMessage), 0, 0);
}

#ifdef WX_CARBON
# define SLEEP_TIME 0x7FFFFFFF
#else
# define SLEEP_TIME 60
#endif

void Drop_GetArgs(int *argc, char ***argv, int *in_terminal)
{
  *in_terminal = 1;

  MZ_REGISTER_STATIC(scheme_mac_argv);

  Install();
  while (!scheme_mac_ready) {
    EventRecord event;
    
    WaitNextEvent(highLevelEventMask, &event, SLEEP_TIME, 0L);
    if (event.what == kHighLevelEvent) {
#ifdef WX_CARBON
      AEProcessAppleEvent(&event);
#else
      // high level events do not occur under OS X (per se, now they're just apple events)
      if ((event.message == 'PLT ') && ((*(long *)&event.where) == 'cmdl')) {
        /* Replaces OpenApp or OpenDocs: */
        TargetID src;
        unsigned long ref, len;
        char *data;
        data = (char *)scheme_malloc(5000);
        len = 4999;
        AcceptHighLevelEvent(&src, &ref, data, &len);
        data[len] = 0;
        scheme_mac_ready = 1;
        parse_commandline(data, NULL, 0);
      } else
        AEProcessAppleEvent(&event);
#endif
    }
  }

#ifdef OS_X
  {
    int from_finder;

    from_finder = (((*argc) > 1) && (strncmp((*argv)[1],"-psn_",5) == 0));
    if (from_finder) {
      /* Finder started app, or someone wants us to think so; set
	 *in_terminal to 0 and combine AE-based command-line with given
	 command line */
      int i, new_argc;
      char **new_argv;
      *in_terminal = 0;
      new_argc = (scheme_mac_argc - 1) + (*argc - 2) + 1;
      new_argv = (char **)scheme_malloc(scheme_mac_argc * sizeof(char *));
      new_argv[0] = (*argv)[0];
      for (i = 2; i < (*argc); i++) {
	new_argv[i - 1] = (*argv)[i];
      }
      for (; i < new_argc; i++) {
	new_argv[i] = scheme_mac_argv[i - (*argc) + 1];
      }
      scheme_mac_argc = new_argc;
      scheme_mac_argv = new_argv;
    } else {
      /* command-line start; no AE arguments */
      scheme_mac_argc = *argc;
      scheme_mac_argv = *argv;
    }

    GetStarterInfo();

    /* Open the PLT_MrEd framework resources: */
    {
      CFBundleRef fwBundle;

      fwBundle = CFBundleGetBundleWithIdentifier(CFSTR("org.plt-scheme.PLT_MrEd"));
      if (fwBundle) {
	SInt16 refNum;
	SInt16 lRefNum;
	CFBundleOpenBundleResourceFiles(fwBundle, &refNum, &lRefNum);
      }
    }

  }
#endif  

  *argc = scheme_mac_argc;
  *argv = scheme_mac_argv;
}

/**********************************************************************/

#ifdef OS_X

#define BUFSIZE 1000
#define RSRCNAME "starter-info"
#define EXECNAME "MrEd"

static CFPropertyListRef getPropertyList()
{
  // locate the starter's bundle:
  CFBundleRef appBundle = CFBundleGetMainBundle();
  
  // get a URL for the named resource
  CFURLRef myRef = CFBundleCopyResourceURL(appBundle, CFSTR(RSRCNAME), 
					   NULL, NULL);
  if (myRef == NULL) {
    return NULL;
  }
  
  // Load the XML data using its URL.
  CFDataRef       xmlData;
  CFURLCreateDataAndPropertiesFromResource(kCFAllocatorDefault, myRef, 
					   &xmlData, NULL, NULL, NULL);

  // convert to a Property List
  CFStringRef error;
  CFPropertyListRef propertyList;
  propertyList = CFPropertyListCreateFromXMLData(kCFAllocatorDefault, xmlData, 
						 kCFPropertyListImmutable, 
						 &error);
  if (error != NULL) {
    return NULL;
  }

  return propertyList;
}

char *ConvertCFStringRef(CFStringRef str)
{
  static char buf[BUFSIZE];

  Boolean success;
  success = CFStringGetCString(str,buf,BUFSIZE,kCFStringEncodingISOLatin1);
  if (!success) {
    return "???";
  }
  char *result = new char[strlen(buf) + 1];
  strcpy(result,buf);
  return result;
}  

void GetStarterInfo()
{
  int i;

  CFPropertyListRef propertyList = getPropertyList();

  if (propertyList) {
    
    CFStringRef execName;
    CFArrayRef storedArgsArray;
    CFIndex count;
    char **storedArgs;
    
    if (CFDictionaryContainsKey((CFDictionaryRef)propertyList,
				(const void *)(CFSTR("executable name")))) {
      execName = (CFStringRef)CFDictionaryGetValue((CFDictionaryRef)propertyList,
						   (CFSTR("executable name")));
      scheme_mac_argv[0] = ConvertCFStringRef(execName);
    }

    if (CFDictionaryContainsKey((CFDictionaryRef)propertyList,
				(const void *)CFSTR("stored arguments"))) {
      storedArgsArray = (CFArrayRef)CFDictionaryGetValue((CFDictionaryRef)propertyList,
							 (CFSTR("stored arguments")));
    } else {
      return;
    }
    
    count = CFArrayGetCount(storedArgsArray);
    
    storedArgs = new char *[scheme_mac_argc + count];
    
    storedArgs[0] = scheme_mac_argv[0];
    for (i = 0; i < count; i++) {
      CFStringRef arg = (CFStringRef)CFArrayGetValueAtIndex(storedArgsArray,i);
      storedArgs[i + 1] = ConvertCFStringRef(arg);
    }
    for (i = 1; i < scheme_mac_argc; i++) {
      storedArgs[count + i] = scheme_mac_argv[i];
    }

    scheme_mac_argv = storedArgs;
    scheme_mac_argc += count;
  }
}

#endif
