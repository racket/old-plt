
#include <Files.h>
#ifndef OS_X
  #include <EPPC.h>
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

extern char *wxFSSpecToPath(const FSSpec *);

int scheme_mac_ready, scheme_mac_argc = 0;
char **scheme_mac_argv;

static char *ThisAppName(void)
{	
#ifndef FOR_STARTER
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
  
  result = (char *)scheme_malloc_atomic(buffer[0] + dlen + 1);
  memcpy(result, dir, dlen);
  memcpy(result + dlen, buffer + 1, buffer[0]);
  result[buffer[0] + dlen] = 0;
  
  return result;
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
	  *(pos++) = *(token)++;
	if (*token)
	  token++;
      } else
	*(pos++) = *(token)++;
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

#ifndef FOR_STARTER
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

#ifndef OS_X
#ifdef __cplusplus
extern "C" {
#endif
  extern char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
#ifdef __cplusplus
};
#endif
#endif // OS_X

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
#ifdef OS_X
    files[i + j] = wxFSSpecToPath(&fss);
#else        
    files[i + j] = scheme_build_mac_filename(&fss, 0);
#endif    
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

static pascal OSErr SetUpQuitMessage(const AppleEvent *a, AppleEvent *b, long c)
{
  Drop_Quit();
  
  return 0;
}

static void Install(void)
{
  short err=0;
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenApplication, NewAEEventHandlerUPP(OpenApplicationStuff), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEOpenDocuments, NewAEEventHandlerUPP(OpenFinderDoc), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEPrintDocuments, NewAEEventHandlerUPP(DoNothing), 0, 0);
  err = AEInstallEventHandler(kCoreEventClass, kAEQuitApplication, NewAEEventHandlerUPP(SetUpQuitMessage), 0, 0);
}

void Drop_GetArgs(int *argc, char ***argv)
{
  Install();
  while (!scheme_mac_ready) {
    EventRecord event;
    
    WaitNextEvent(highLevelEventMask, &event, -1, 0L);
    if (event.what == kHighLevelEvent) {
#ifdef OS_X
        AEProcessAppleEvent(&event);
#else
// high level events do not occur under OS X
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
#ifndef OS_X
  *argc = scheme_mac_argc;
  *argv = scheme_mac_argv;
#endif  
}
