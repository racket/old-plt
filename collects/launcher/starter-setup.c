#include "escheme.h"

//int scheme_mac_path_to_spec(const char *filename, FSSpec *spec, long *type);

OSErr setup_alias(char *execPath, char *starterPath)
{
  FSSpec execSpec, starterSpec;
  AliasHandle aliasHndl;
  Str255 newName = "\pExecutable Location";
  int resFile;
  AliasHandle oldHndl;

  scheme_mac_path_to_spec(execPath, &execSpec, NULL);
  scheme_mac_path_to_spec(starterPath, &starterSpec, NULL);

  if (NewAlias(NULL, &execSpec, &aliasHndl) != noErr) {
    return 1;
  }
  
  FSpOpenResFile(&starterSpec,fsRdWrPerm);
  resFile = CurResFile();
  
  // is the resource already there?
  oldHndl = (AliasHandle)GetResource('alis',128);
  if (oldHndl != NULL) {
    RemoveResource((Handle)oldHndl);
    DisposeHandle((Handle)oldHndl);
  }
  
  AddResource((Handle)aliasHndl,'alis',128,newName);
  WriteResource((Handle)aliasHndl);
  CloseResFile(resFile);
  DisposeHandle((Handle)aliasHndl);
  
  return noErr;
} 
  
  
int setup_aliases(char *mzPath, char *mrPath, char *mzStarterPath, char *mrStarterPath)
{
  int mzresult, mrresult;
  
  mzresult = setup_alias(mzPath, mzStarterPath);
  mrresult = setup_alias(mrPath, mrStarterPath);
  
  if ((mzresult == noErr) && (mrresult == noErr)) {
    return noErr;
  } else {
    return 1;
  }
}

Scheme_Object *setup_wrapper(int argc, Scheme_Object **argv)
{
  OSErr err;
  
  if (!(SCHEME_STRINGP(argv[0])) ||
      !(SCHEME_STRINGP(argv[1])) ||
      !(SCHEME_STRINGP(argv[2])) ||
      !(SCHEME_STRINGP(argv[3]))) {
    return scheme_false;
  }
  
  err = setup_aliases((SCHEME_STR_VAL(argv[0])), // I admit I'm assuming pathnames don't contain NULL
                      (SCHEME_STR_VAL(argv[1])),
                      (SCHEME_STR_VAL(argv[2])),
                      (SCHEME_STR_VAL(argv[3])));
                      
  if (err != noErr) {
    return scheme_false;
  } else {
    return scheme_true;
  }
}

Scheme_Object *result_proc;

Scheme_Object *scheme_initialize(Scheme_Env *env) {

  result_proc = scheme_make_prim_w_arity(setup_wrapper,"setup-mac-starter-aliases",4,4);
  
  return result_proc;
}

		
Scheme_Object *scheme_reload(Scheme_Env *env) {
  return result_proc; 
}

// basic functions:

unsigned long strlen(const char *s)
{
  unsigned long i;
  for (i = 0; s[i]; i++);
  return i;
}

char *strcat(char *a, const char *b) // I have no idea what it's supposed to return ...
{
  unsigned long i,j;
  i = strlen(a);
  for (j = 0; a[i] = b[j]; i++, j++);
  return a;
}

void *memcpy(void *dst, const void *src, unsigned long len) // this one either.
{
  unsigned long i;
  for (i = 0; i < len; i++) {
    ((char *)dst)[i] = ((char *)src)[i];
  }
  return dst;
}
