/*
 *  X-starter.cpp
 *  MrEd
 *
 */

#include <unistd.h>
#include <Carbon/Carbon.h>

#define BUFSIZE 1000
#define RSRCNAME "starter-info"
#define EXECNAME "MrEd"

void ExitRudely(bool numeric, OSErr err, const char *msg)
{
    if (numeric) {
      fprintf(stderr,"starter failed. Error number: %d\n", err);
    } else {
      fprintf(stderr,"starter failed. %s\n",msg);
    }
    exit(1);
}

CFPropertyListRef getPropertyList()
{
  // locate the starter's bundle:
  CFBundleRef appBundle = CFBundleGetMainBundle();
  
  // get a URL for the named resource
  CFURLRef myRef = CFBundleCopyResourceURL(appBundle, CFSTR(RSRCNAME), NULL, NULL);
  if (myRef == NULL) {
    ExitRudely(FALSE,0,"Couldn't locate named resource in starter bundle.");
  }
  
  // Load the XML data using its URL.
  CFDataRef       xmlData;
  CFURLCreateDataAndPropertiesFromResource(kCFAllocatorDefault, myRef, &xmlData, NULL, NULL, NULL);

  // convert to a Property List
  CFStringRef error;
  CFPropertyListRef propertyList = CFPropertyListCreateFromXMLData (kCFAllocatorDefault, xmlData, 
      kCFPropertyListImmutable, &error);
  if (error != NULL) {
    ExitRudely(FALSE,0,CFStringGetCStringPtr(error,kCFStringEncodingISOLatin1));
  }

  return propertyList;
}

char *ConvertCFStringRef(CFStringRef str)
{
  static char buf[BUFSIZE];

  Boolean success = CFStringGetCString(str,buf,BUFSIZE,kCFStringEncodingISOLatin1);
  if (!success) {
    ExitRudely(FALSE,0,"can't convert CFStringRef to C string of size BUFSIZE.");
  }
  char *result = new char[strlen(buf) + 1];
  strcpy(result,buf);
  return result;
}  

int main(int argc, char *argv[], char *envp[])
{
  InitCursor();
  
  CFPropertyListRef propertyList = getPropertyList();
  
  CFStringRef execName;
  CFArrayRef storedArgsArray;
  CFIndex count;
  char *executablePath;
  char **storedArgs;
  
  if (CFDictionaryContainsKey((CFDictionaryRef)propertyList,(const void *)(CFSTR("executable name")))) {
    execName = (CFStringRef)CFDictionaryGetValue((CFDictionaryRef)propertyList,(CFSTR("executable name")));
  } else {
    ExitRudely(FALSE,0,"Unable to locate key \"executable name\" in property list.");
  }
  
  executablePath = ConvertCFStringRef(execName);
  fprintf(stderr,"executable path: %s\n",executablePath);

  if (CFDictionaryContainsKey((CFDictionaryRef)propertyList,(const void *)CFSTR("stored arguments"))) {
    storedArgsArray = (CFArrayRef)CFDictionaryGetValue((CFDictionaryRef)propertyList,(CFSTR("stored arguments")));
  } else {
    ExitRudely(FALSE,0,"Unable to locate key \"stored arguments\" in property list.");
  }
  
  count = CFArrayGetCount(storedArgsArray);
  
  storedArgs = new char *[count+ 2];
  
  storedArgs[0] = executablePath;
  
  for(int i = 0;i < count; i++) {
    CFStringRef arg = (CFStringRef)CFArrayGetValueAtIndex(storedArgsArray,i);
    storedArgs[i+1] = ConvertCFStringRef(arg);
  }

  // determine whether this was command-line started, and if so, append those args:
  // FIX THIS TO THE RIGHT SOLUTION ON MONDAY!
  if ((argc > 1) && (strncmp(argv[1],"-psn_",5) == 0)) { // did the finder start this app?
    storedArgs[count + 1] = 0;
  } else {
    // append the passed-in args:
    for (int i = 0; i < argc; i++) {
      storedArgs[i + count + 1] = argv[i];
    }
    storedArgs[argc+count+1] = 0;
  }

  pid_t pid = fork();
  
  if (pid == -1) {
    fprintf(stderr,"error on fork: %d\n",errno);
    exit(1);
  }
  
  if (pid == 0) {
    // do the jump!	
    execve(executablePath,storedArgs,envp);
  } 
  
  // else exit.
}
