#include "escheme.h"

// COPIED from file.c in mzscheme:

#ifdef MAC_FILE_SYSTEM
long scheme_creator_id = 'MzSc';
#endif

#ifdef UNIX_FILE_SYSTEM
# define FN_SEP '/'
# define IS_A_SEP(x) ((x) == '/')
#endif
#ifdef DOS_FILE_SYSTEM
# define FN_SEP '\\'
# define IS_A_SEP(x) (((x) == '/') || ((x) == '\\'))
#endif
#ifdef MAC_FILE_SYSTEM
# define FN_SEP ':'
# define IS_A_SEP(x) ((x) == ':')
#endif
#ifdef PALMOS_STUFF
# define FN_SEP 0
# define IS_A_SEP(x) (!(x))
#endif

// next line copied from schpriv.h
#define REGISTER_SO(x) MZ_REGISTER_STATIC(x)

// next lines copied from scheme.h
/* GC registration: */
//void scheme_register_static(void *ptr, long size);
#if defined(MUST_REGISTER_GLOBALS) || defined(GC_MIGHT_USE_REGISTERED_STATICS)
# define MZ_REGISTER_STATIC(x)  scheme_register_static((void *)&x, sizeof(x))
#else
# define MZ_REGISTER_STATIC(x) /* empty */
#endif

static Scheme_Object *path_err_symbol;


#define CURRENT_WD() scheme_get_param(scheme_config, MZCONFIG_CURRENT_DIRECTORY)


int scheme_is_complete_path(const char *s, long len);
static char *do_path_to_complete_path(char *filename, long ilen, const char *wrt, long wlen);

static int find_mac_file(const char *filename, int use_real_cwd,
			 FSSpec *spec, int finddir, int findfile,
			 int *dealiased, int *wasdir, int *exists,
			 long *filedate, int *flags, 
			 long *type, unsigned long *size,
			 FInfo *finfo) 
/* finddir:   0 => don't care if dir is found (but set *wasdir)
              1 => must find a dir
   findfile:  0 => don't care if file is found (but unset *wasdir)
              1 => must find a file
             -1 => must find a link
             -2 => must find a file or link
             -3 => don't care if file or link is found (but unset *wasdir)
   wasdir and exists are always filled in, unless they're null; *exists
    is set to 2 if a link is found
   filedate, flags, type, size, and finfo are only filled in
   when findfile >= 0 (and when they're non-null)
 */
{
  WDPBRec  wdrec;
  CInfoPBRec pbrec;
  char buf[256];
  short find_vref;
  long find_dir_id;
  int need_filedate = 0;

  if (dealiased)
    *dealiased = 0;
  if (wasdir)
    *wasdir = 1;
  if (exists)
    *exists = 0;
  if (flags)
    *flags = 0;
  if (type)
    *type = 0;
  if (size)
    *size = 0;
  if (filedate)
    *filedate = 0;

  if (use_real_cwd) {
    wdrec.ioNamePtr = (StringPtr)buf;
    if (PBHGetVol(&wdrec, 0))
      return 0;
    
    find_vref = wdrec.ioWDVRefNum;
    find_dir_id = wdrec.ioWDDirID;
  } else {
    int len = filename ? strlen(filename) : 0;
    find_vref = -1;
    find_dir_id = 0;
    if (!filename || !scheme_is_complete_path(filename, len))
      filename = do_path_to_complete_path((char *)filename, len, NULL, 0);
  }
  
  /* filename is empty => Local directory */
  if (!*filename) {
    if (findfile && (findfile != -3))
      return 0;
    if (exists)
      *exists = 1;
    need_filedate = 1;
  } else {
    const char *p;
    int has_colon;
    
    for (p = filename; *p && (*p != ':'); p++) {}
    has_colon = (*p == ':');
    
    p = filename;    
    if (*p == ':') {
      p++;
    } else if (has_colon) {
      char vbuf[256];
      int len = 0;
      HParamBlockRec hrec;
      /* It's an absolute path: get the Volume Name and vRefNum */
      while (*p && *p != ':') {
	vbuf[len++] = *(p++);
	if (len > 32)
	  return 0;
      }
      vbuf[len] = 0;
      if (*p)
	p++;
      
      strcat(vbuf, ":");
      hrec.volumeParam.ioNamePtr = c2pstr(vbuf);
      hrec.volumeParam.ioVRefNum = 0;
      hrec.volumeParam.ioVolIndex = -1;
      if (PBHGetVInfo(&hrec, 0))
	return 0;
      find_vref = hrec.volumeParam.ioVRefNum;
      find_dir_id = 0;
    }
    
    if (!*p) {
      if (findfile && (findfile != -3))
        return 0;
      if (exists)
        *exists = 1;
      need_filedate = 1;
    }

    while (p && *p) {
      const char *next = p;
      int len = 0;
      Boolean isFolder, wasAlias;
      
      while (*next && *next != ':') {
	buf[len++] = *(next++);
	if (len > 32)
	  return 0;
      }
      buf[len] = 0;
      
      if (*next)
	next++;
      if (!*next)
	next = NULL;
      
      if (len > 32)
	return 0;
      
      if (!len) {
	/* Parent directory */
	pbrec.hFileInfo.ioNamePtr = spec->name;
	pbrec.hFileInfo.ioVRefNum = find_vref;
	pbrec.hFileInfo.ioDirID = find_dir_id;
	pbrec.hFileInfo.ioFDirIndex = -1;
	if (PBGetCatInfo(&pbrec, 0))
	  return 0;	
	find_dir_id = pbrec.dirInfo.ioDrParID;
	if (!next) {
	  if (findfile && (findfile != -3))
	    return 0;
	  if (wasdir)
	    *wasdir = 1;
	  if (exists)
	    *exists = 1;
	  need_filedate = 1;
	}
      } else {
	spec->vRefNum = find_vref;
	spec->parID = find_dir_id;
	memcpy((void *)spec->name, buf, len);
	spec->name[len] = 0;
	c2pstr((char *)spec->name);
	
	pbrec.hFileInfo.ioNamePtr = spec->name;
	pbrec.hFileInfo.ioVRefNum = spec->vRefNum;
	pbrec.hFileInfo.ioDirID = spec->parID;
	pbrec.hFileInfo.ioFDirIndex = 0;
	if (PBGetCatInfo(&pbrec, 0)) {
	  if (finddir || (findfile && (findfile != -3)) || next)
	    return 0;
	  if (wasdir)
	    *wasdir = 0;
	  break;
	} else {
	  /* If it's a file, it could be an alias: */
	  if (!(pbrec.hFileInfo.ioFlAttrib & 0x10)) {
	    Str32 origname;
	    memcpy(origname, spec->name, 32);
	    ResolveAliasFile(spec, 1, &isFolder, &wasAlias);
	    if (!next && (findfile < 0)) {
	      if (wasAlias) {
	        spec->vRefNum = find_vref;
	        spec->parID = find_dir_id;
	        memcpy((void *)spec->name, origname, 32);
	      }
		  if (findfile <= -2) {
	        if (wasdir)
	         *wasdir = 0;
			if (exists)
			 *exists = wasAlias ? 2 : 1;
	        return 1;
	      }
	      if (wasdir)
	        *wasdir = isFolder;
	      if (wasAlias && exists)
	        *exists = 2;
	      return wasAlias ? 1 : 0;
	    }
	    if (wasAlias && dealiased)
	      *dealiased = 1;
	    
	    if (wasAlias) {
	      /* Look it up again: */
	      find_vref = spec->vRefNum;
	      find_dir_id = spec->parID;
	      pbrec.hFileInfo.ioNamePtr = spec->name;
	      pbrec.hFileInfo.ioVRefNum = spec->vRefNum;
	      pbrec.hFileInfo.ioDirID = spec->parID;
	      pbrec.hFileInfo.ioFDirIndex = 0;
	      if (PBGetCatInfo(&pbrec, 0)) {
	        if (finddir || (findfile && (findfile != -3)) || next)
	          return 0;
	        if (wasdir)
	          *wasdir = 0;
	        if (exists)
	          *exists = 0;
	        break;
	      }
	    }
	  }
	 
	  if (!(pbrec.hFileInfo.ioFlAttrib & 0x10)) {
	    if (finddir || next)
	      return 0; /* Not a directory */
	    if (wasdir)
	      *wasdir = 0;
	    if (exists)
	      *exists = 1;
	    if (filedate)
	      *filedate = pbrec.hFileInfo.ioFlMdDat;
	    if (type)
	      *type = pbrec.hFileInfo.ioFlFndrInfo.fdType;
	    if (flags)
	      *flags = pbrec.hFileInfo.ioFlAttrib;
	    if (size)
	      *size = (pbrec.hFileInfo.ioFlLgLen
		       + pbrec.hFileInfo.ioFlRLgLen);
	    if (finfo)
	      memcpy(finfo, &pbrec.hFileInfo.ioFlFndrInfo, sizeof(FInfo));
	  } else {
	    if (findfile && (findfile != -3) && !next)
	      return 0;
	    find_vref = spec->vRefNum;
	    find_dir_id = pbrec.hFileInfo.ioDirID;
	    if (!next) {
	      if (flags)
	        *flags = pbrec.hFileInfo.ioFlAttrib;
	      if (filedate)
	        *filedate = pbrec.dirInfo.ioDrMdDat;
	      if (exists)
	        *exists = 1;
	     }
	  }
        }  
      }
      
      p = next;
    }
  }
  
  if (need_filedate && filedate) {
    Str255 buffer;
    pbrec.hFileInfo.ioNamePtr = buffer;
    pbrec.hFileInfo.ioVRefNum = find_vref;
    pbrec.hFileInfo.ioDirID = find_dir_id;
    pbrec.hFileInfo.ioFDirIndex = -1;
    if (!PBGetCatInfo(&pbrec, 0))
      *filedate = pbrec.dirInfo.ioDrMdDat;
  }
  
  spec->vRefNum = find_vref;
  spec->parID = find_dir_id;
  
  return 1;
}

char *scheme_build_mac_filename(FSSpec *spec, int given_dir)
{
#define QUICK_BUF_SIZE 256

  CInfoPBRec pbrec;
  char buf[256], qbuf[QUICK_BUF_SIZE], *s;
  int i, j, size = 0, alloced = QUICK_BUF_SIZE - 1;
  int vref = spec->vRefNum;
  long dirID = spec->parID;

  s = qbuf;
  if (!given_dir) {
    for (i = spec->name[0]; i; i--) {
      s[size++] = spec->name[i];
    }
  }

  while (1)  {
    pbrec.hFileInfo.ioNamePtr = (unsigned char *)buf;
    pbrec.hFileInfo.ioVRefNum = vref;
    pbrec.hFileInfo.ioDirID = dirID;
    pbrec.hFileInfo.ioFDirIndex = -1;
	if (PBGetCatInfo(&pbrec, 0))
	  return NULL;
	
	if (size + buf[0] + 1 > alloced) {
	   char *old;
	   
	   alloced *= 2;
	   old = (char *)scheme_malloc_atomic(alloced + 1);
	   memcpy(old, s, size);
	}

    s[size++] = ':';
    for (i = buf[0]; i; i--) {
      s[size++] = buf[i];
    }
	  
    dirID = pbrec.dirInfo.ioDrParID;
    if (dirID == 1)
      break;
  }
  
  if (alloced < QUICK_BUF_SIZE) {
    s = (char *)scheme_malloc_atomic(size + 1);
    for (j = 0, i = size; i--; j++) {
      s[j] = qbuf[i];
    }
  } else {
    int save;
    
    for (i = 0, j = size - 1; i < j; i++, j--) {
      save = s[i];
      s[i] = s[j];
      s[j] = save;
    }
  }
  
  s[size] = 0;
  return s;
}

int scheme_mac_path_to_spec(const char *filename, FSSpec *spec, long *type)
{
  return find_mac_file(filename, 1, spec, 0, 0, NULL, NULL, NULL, NULL, NULL, type, NULL, NULL);
}

static int has_null(const char *s, long l)
{
  while (l--) {
    if (!s[l])
      return 1;
  }

  return 0;
}

static void raise_null_error(const char *name, Scheme_Object *path, const char *mod)
{
  scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		   path,
		   path_err_symbol,
		   "%s: pathname%s contains a null character: %Q", 
		   name, mod, 
		   path);
}

int scheme_is_relative_path(const char *s, long len)
{
#ifdef MAC_FILE_SYSTEM
  int i;
#endif

  if (!len)
    return 0;

#ifdef UNIX_FILE_SYSTEM
  return !((s[0] == '/') || (s[0] == '~'));
#endif
#ifdef DOS_FILE_SYSTEM
  if (IS_A_SEP(s[0])
      || ((len >= 2) 
	  && isalpha(((unsigned char)s[0]))
	  && (s[1] == ':')))
    return 0;
  else
    return 1;
#endif
#ifdef MAC_FILE_SYSTEM
  if (s[0] != ':')
    for (i = 1; i < len; i++) {
      if (s[i] == ':')
	return 0;
    }

  return 1;
#endif
}

int scheme_is_complete_path(const char *s, long len)
{
  if (!len)
    return 0;

  if (!scheme_is_relative_path(s, len)) {
#ifdef DOS_FILE_SYSTEM
    if (IS_A_SEP(s[0]) && IS_A_SEP(s[1])) {
      if (check_dos_slashslash_drive(s, len, NULL, 0))
	return 1;
      else
	return 0;
    } else if ((len >= 2) 
	       && isalpha(((unsigned char)s[0]))
	       && (s[1] == ':')) {
      return 1;
    } else
      return 0;
#else
    return 1;
#endif
  } else 
    return 0;
}

static char *do_path_to_complete_path(char *filename, long ilen, const char *wrt, long wlen)
{
  if (!scheme_is_complete_path(filename, ilen)) {
    char *naya;
    int skip_sep = 0;

    if (!wrt) {
      Scheme_Object *wd = CURRENT_WD();
      wrt = SCHEME_STR_VAL(wd);
      wlen = SCHEME_STRTAG_VAL(wd);
    }

#ifdef DOS_FILE_SYSTEM
    if (!scheme_is_relative_path(filename, ilen)) {
      /* Absolute, not complete. Fill in the disk */
      wrt = get_drive_part(wrt, wlen);
      wlen = strlen(wrt);
      /* drop trailing separator */
      if (IS_A_SEP(wrt[wlen - 1])) {
	wlen--;
	skip_sep = 1;
      }
    }
#endif

    naya = (char *)scheme_malloc_atomic(ilen + wlen + 2);
    memcpy(naya, wrt, wlen);
    if (!skip_sep)
      if (!IS_A_SEP(naya[wlen - 1]))
	naya[wlen++] = FN_SEP;
#ifdef MAC_FILE_SYSTEM
    if (IS_A_SEP(filename[0])) {
      filename++;
      ilen--;
    }
#endif
    memcpy(naya + wlen, filename, ilen);
    naya[wlen + ilen] = 0;
    
    return naya;
  }

  return filename;
}

static Scheme_Object *path_to_complete_path(int argc, Scheme_Object **argv)
{
  Scheme_Object *p, *wrt;
  char *s;
  int len;

  p = argv[0];
  if (!SCHEME_STRINGP(p))
    scheme_wrong_type("path->complete-path", "string", 0, argc, argv);
  if (argc > 1) {
    wrt = argv[1];
    if (!SCHEME_STRINGP(wrt))
      scheme_wrong_type("path->complete-path", "string", 1, argc, argv);
  } else
    wrt = NULL;

  s = SCHEME_STR_VAL(p);
  len = SCHEME_STRTAG_VAL(p);

  if (has_null(s, len))
    raise_null_error("path->complete-path", p, "");
  if (wrt) {
    char *ws;
    int wlen;

    ws = SCHEME_STR_VAL(wrt);
    wlen = SCHEME_STRTAG_VAL(wrt);
    
    if (has_null(ws, wlen))
      raise_null_error("path->complete-path", p, "");

    if (!scheme_is_complete_path(ws, wlen))
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		       wrt,
		       path_err_symbol,
		       "path->complete-path: second argument is not a complete path: \"%q\"",
		       ws);

    if (!scheme_is_complete_path(s, len)) {
      s = do_path_to_complete_path(s, len, ws, wlen);
      return scheme_make_sized_string(s, strlen(s), 0);
    }
  } else if (!scheme_is_complete_path(s, len)) {
    s = do_path_to_complete_path(s, len, NULL, 0);

    return scheme_make_sized_string(s, strlen(s), 0);
  }
   
  return p;
}



// end of COPIED region

OSErr setup_alias(char *execPath, char *starterPath)
{
  FSSpec execSpec, starterSpec;
  AliasHandle aliasHndl;
  Str255 newName = "\pExecutable Location";

  scheme_mac_path_to_spec(execPath, &execSpec, NULL);
  scheme_mac_path_to_spec(starterPath, &starterSpec, NULL);

  if (NewAlias(NULL, &execSpec, &aliasHndl) != noErr) {
    return 1;
  }
  
  FSpOpenResFile(&starterSpec,fsRdWrPerm);
  
  AddResource((Handle)aliasHndl,'alis',128,newName);
  WriteResource((Handle)aliasHndl);
  
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
  REGISTER_SO(path_err_symbol);
  path_err_symbol = scheme_intern_symbol("ill-formed-path");

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
  return NULL;
}

void *memcpy(void *dst, const void *src, unsigned long len) // this one either.
{
  unsigned long i;
  for (i = 0; i < len; i++) {
    ((char *)dst)[i] = ((char *)src)[i];
  }
  return NULL;
}
