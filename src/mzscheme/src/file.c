/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

#include "schpriv.h"
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifndef NO_STAT_PROC
# ifdef NO_SYS_INCLUDE_SUBDIR
#  include <stat.h>
# else
#  include <sys/types.h>
#  include <sys/stat.h>
# endif
#endif
#ifdef EXPAND_FILENAME_TILDE
# include <pwd.h>
#endif
#ifndef NO_FILE_SYSTEM_UTILS
# include <ctype.h>
# ifndef NO_READDIR
#  include <dirent.h>
# endif
#endif
#ifdef DIR_INCLUDE
# include <dir.h>
#endif
#ifdef DIRECT_INCLUDE
# include <direct.h>
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#ifdef USE_MAC_FILE_TOOLBOX
# include <Files.h>
#endif
#ifdef UNIX_FILE_SYSTEM
# include <fcntl.h>
# include <grp.h>
# include <pwd.h>
#endif
#ifdef DOS_FILE_SYSTEM
# include <windows.h>
#endif
#ifdef NO_ERRNO_GLOBAL
# define errno -1
#else
# include <errno.h>
#endif

#if defined(S_IFDIR) && !defined(S_ISDIR)
# define S_ISDIR(m) ((m) & S_IFDIR)
#endif
#if defined(S_IFREG) && !defined(S_ISREG)
# define S_ISREG(m) ((m) & S_IFREG)
#endif
#if defined(S_IFLNK) && !defined(S_ISLNK)
# define S_ISLNK(m) ((m) & S_IFLNK)
#endif
#if defined(_S_IFDIR) && !defined(S_ISDIR)
# define S_ISDIR(m) ((m) & _S_IFDIR)
#endif
#if defined(_S_IFREG) && !defined(S_ISREG)
# define S_ISREG(m) ((m) & _S_IFREG)
#endif

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

/* local */
static Scheme_Object *file_exists(int argc, Scheme_Object **argv);
static Scheme_Object *directory_exists(int argc, Scheme_Object **argv);
static Scheme_Object *link_exists(int argc, Scheme_Object **argv);

#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *delete_file(int argc, Scheme_Object **argv);
static Scheme_Object *rename_file(int argc, Scheme_Object **argv);
static Scheme_Object *copy_file(int argc, Scheme_Object **argv);
static Scheme_Object *directory_list(int argc, Scheme_Object *argv[]);
static Scheme_Object *filesystem_root_list(int argc, Scheme_Object *argv[]);
static Scheme_Object *make_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *delete_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *relative_pathname_p(int argc, Scheme_Object **argv);
static Scheme_Object *absolute_pathname_p(int argc, Scheme_Object **argv);
static Scheme_Object *complete_pathname_p(int argc, Scheme_Object **argv);
#endif
static Scheme_Object *path_to_complete_path(int argc, Scheme_Object **argv);
#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *resolve_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *expand_path(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_drive(int argc, Scheme_Object *argv[]);
static Scheme_Object *normal_path_case(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_modify_seconds(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_or_dir_permissions(int argc, Scheme_Object *argv[]);
static Scheme_Object *file_size(int argc, Scheme_Object *argv[]);
static Scheme_Object *current_library_collection_paths(int argc, Scheme_Object *argv[]);
#endif

#ifdef DIR_FUNCTION
static Scheme_Object *current_directory(int argc, Scheme_Object *argv[]);
#endif

#ifdef DOS_FILE_SYSTEM
static char get_cur_drive_id(void);
#endif

static Scheme_Object *up_symbol, *relative_symbol;
static Scheme_Object *same_symbol;
#ifndef NO_FILE_SYSTEM_UTILS
static Scheme_Object *read_symbol, *write_symbol, *execute_symbol;

# ifdef MACINTOSH_EVENTS
static Scheme_Object *record_symbol, *file_symbol;
# endif
#endif

#ifdef MACINTOSH_EVENTS
void (*scheme_handle_aewait_event)(EventRecord *e);
#endif

void scheme_init_file(Scheme_Env *env)
{
  if (scheme_starting_up) {
    REGISTER_SO(up_symbol);
    REGISTER_SO(relative_symbol);
    REGISTER_SO(same_symbol);
#ifndef NO_FILE_SYSTEM_UTILS
    REGISTER_SO(read_symbol);
    REGISTER_SO(write_symbol);
    REGISTER_SO(execute_symbol);
#endif
    
    up_symbol = scheme_intern_symbol("up");
    relative_symbol = scheme_intern_symbol("relative");
    same_symbol = scheme_intern_symbol("same");

#ifndef NO_FILE_SYSTEM_UTILS

    read_symbol = scheme_intern_symbol("read");
    write_symbol = scheme_intern_symbol("write");
    execute_symbol = scheme_intern_symbol("execute");

# ifdef MACINTOSH_EVENTS
	record_symbol = scheme_intern_symbol("record");
	file_symbol = scheme_intern_symbol("file");
# endif
#endif
    
    scheme_set_param(scheme_config, MZCONFIG_COLLECTION_PATHS,  scheme_null);
  }

  scheme_add_global_constant("file-exists?", 
			     scheme_make_prim_w_arity(file_exists, 
						      "file-exists?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("directory-exists?", 
			     scheme_make_prim_w_arity(directory_exists, 
						      "directory-exists?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("link-exists?", 
			     scheme_make_prim_w_arity(link_exists, 
						      "link-exists?", 
						      1, 1), 
			     env);
#ifndef NO_FILE_SYSTEM_UTILS
  scheme_add_global_constant("delete-file", 
			     scheme_make_prim_w_arity(delete_file, 
						      "delete-file", 
						      1, 1), 
			     env);
  scheme_add_global_constant("rename-file", 
			     scheme_make_prim_w_arity(rename_file, 
						      "rename-file", 
						      2, 2), 
			     env);
  scheme_add_global_constant("copy-file", 
			     scheme_make_prim_w_arity(copy_file, 
						      "copy-file", 
						      2, 2), 
			     env);
  scheme_add_global_constant("build-path", 
			     scheme_make_prim_w_arity(scheme_build_pathname,
						      "build-path", 
						      1, -1), 
			     env);
  scheme_add_global_constant("split-path", 
			     scheme_make_prim_w_arity2(scheme_split_pathname,
						       "split-path",
						       1, 1,
						       3, 3), 
			     env);
  scheme_add_global_constant("relative-path?", 
			     scheme_make_prim_w_arity(relative_pathname_p,
						      "relative-path?",
						      1, 1), 
			     env);
  scheme_add_global_constant("absolute-path?", 
			     scheme_make_prim_w_arity(absolute_pathname_p,
						      "absolute-path?",
						      1, 1), 
			     env);
  scheme_add_global_constant("complete-path?", 
			     scheme_make_prim_w_arity(complete_pathname_p,
						      "complete-path?",
						      1, 1), 
			     env);
  scheme_add_global_constant("path->complete-path",
			     scheme_make_prim_w_arity(path_to_complete_path,
						      "path->complete-path",
						      1, 2), 
			     env);
  scheme_add_global_constant("resolve-path",
			     scheme_make_prim_w_arity(resolve_path,
						      "resolve-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("expand-path",
			     scheme_make_prim_w_arity(expand_path,
						      "expand-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("normal-case-path",
			     scheme_make_prim_w_arity(normal_path_case,
						      "normal-case-path",
						      1, 1), 
			     env);
  scheme_add_global_constant("directory-list",
			     scheme_make_prim_w_arity(directory_list,
						      "directory-list",
						      0, 1), 
			     env);
  scheme_add_global_constant("filesystem-root-list",
			     scheme_make_prim_w_arity(filesystem_root_list,
						      "filesystem-root-list",
						      0, 0), 
			     env);
  scheme_add_global_constant("make-directory",
			     scheme_make_prim_w_arity(make_directory,
						      "make-directory",
						      1, 1), 
			     env);
  scheme_add_global_constant("delete-directory",
			     scheme_make_prim_w_arity(delete_directory,
						      "delete-directory",
						      1, 1), 
			     env);
  scheme_add_global_constant("file-modify-seconds",
			     scheme_make_prim_w_arity(file_modify_seconds,
						      "file-modify-seconds",
						      1, 1), 
			     env);
  scheme_add_global_constant("file-or-directory-permissions",
			     scheme_make_prim_w_arity(file_or_dir_permissions,
						      "file-or-directory-permissions",
						      1, 1), 
			     env);
  scheme_add_global_constant("file-size",
			     scheme_make_prim_w_arity(file_size,
						      "file-size",
						      1, 1), 
			     env);

  scheme_add_global_constant("current-drive", 
			     scheme_make_prim_w_arity(current_drive, 
						      "current-drive", 
						      0, 0), 
			     env);
#endif

#ifdef DIR_FUNCTION
  scheme_add_global_constant("current-directory", 
			     scheme_make_prim_w_arity(current_directory, 
						      "current-directory", 
						      0, 1), 
			     env);
#endif

#ifndef NO_FILE_SYSTEM_UTILS
  scheme_add_global_constant("current-library-collection-paths",
			     scheme_register_parameter(current_library_collection_paths,
						       "current-library-collection-paths",
						       MZCONFIG_COLLECTION_PATHS),
			     env);
#endif
}

#ifdef USE_MAC_FILE_TOOLBOX
char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
#endif

#ifdef USE_GET_CURRENT_DIRECTORY
static char *getcwd(char *s, int l)
{
 int need_l;
  
 need_l = GetCurrentDirectory(l, s);

 if (need_l > l) {
	if (l)
	  return NULL;
	s = scheme_malloc_atomic(need_l);
	need_l = GetCurrentDirectory(need_l, s);
 }

 if (!need_l)
  return NULL;

 return s;
}
#endif

char *scheme_getcwd(char *buf, int buflen, int *actlen, int noexn)
{
#ifdef USE_MAC_FILE_TOOLBOX
    char *dir;
    char nbuf[64];
    WDPBRec rec;
    FSSpec spec;
    
    rec.ioNamePtr = (StringPtr)nbuf;
    PBHGetVol(&rec, 0);
    spec.vRefNum = rec.ioVRefNum;
    spec.parID = rec.ioWDDirID;
    
    dir = scheme_build_mac_filename(&spec, 1);

    if (actlen)
      *actlen = strlen(dir) + 1;

    return dir;
#else
# define GETCWD_BUFSIZE 1024
    char buffer[GETCWD_BUFSIZE], *r, *gbuf;
    int obuflen = buflen;

    if (buflen < GETCWD_BUFSIZE) {
      gbuf = buffer;
      buflen = GETCWD_BUFSIZE;
    } else
      gbuf = buf;

    r = MSC_IZE(getcwd)(gbuf, buflen - 1);
    if (!r) {
      char *r2;

      r = MSC_IZE(getcwd)(NULL, 0);
      if (!r) {
	/* Something bad happened! */
	if (noexn) {
	  if (actlen)
	    *actlen = 0;

	  if (buf) {
	    buf[0] = 0;
	    return buf;
	  } else {
	    return "";
	  }
	}

	scheme_raise_exn(MZEXN_I_O, "current-directory: unknown failure (%d)", errno);
      }

      buflen = strlen(r) + 1;
      r2 = (char *)scheme_malloc_atomic(buflen);
      memcpy(r2, r, buflen);
      r2[buflen] = 0;
      free(r);
      r = r2;

      if (actlen)
	*actlen = buflen;
    } else {
      int slen = strlen(r) + 1;

      if (actlen)
	*actlen = slen;

      if (obuflen < slen)
      	r = scheme_strdup(r);
      else if (r != buf) {
	memcpy(buf, r, slen);
	r = buf;
      }
    }
     
    return r;
#endif
}

int scheme_setcwd(char *expanded, int noexn)
{
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
#endif
  int err;

#ifdef USE_MAC_FILE_TOOLBOX
  if (find_mac_file(expanded, &spec, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)) {
    WDPBRec rec;
    
    rec.ioNamePtr = NULL;
    rec.ioVRefNum = spec.vRefNum;
    rec.ioWDDirID = spec.parID;
    
    err = PBHSetVol(&rec, 0);
  } else
    err = 1;
#else
  err = MSC_IZE(chdir)(expanded);
#endif

  if (err && !noexn)
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM_DIRECTORY,
		       scheme_make_string(expanded),
		       "current-directory: unable to switch to directory: \"%.255s\"",
		       expanded);

  return !err;
}

Scheme_Object *scheme_get_file_directory(const char *filename)
{
  Scheme_Object *v = scheme_make_sized_string((char *)filename, strlen(filename), 0);
  
  v = path_to_complete_path(1, &v);
  scheme_split_pathname(1, &v);
  return scheme_current_process->ku.multiple.array[0];
}

#ifdef USE_MAC_FILE_TOOLBOX
static int find_mac_file(const char *filename, FSSpec *spec, int finddir, int findfile,
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

  wdrec.ioNamePtr = (StringPtr)buf;
  if (PBHGetVol(&wdrec, 0))
    return 0;
  
  find_vref = wdrec.ioWDVRefNum;
  find_dir_id = wdrec.ioWDDirID;
  
  /* filename is NULL => Local directory */
  if (!*filename) {
    if (findfile && (findfile != -3))
      return 0;
    if (exists)
      *exists = 1;
    need_filedate = 1;
  } else {
    const char *p;
    int has_colon;
    
    for (p = filename; *p && (*p != ':'); p++);
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
    for (i = spec->name[0]; i; i--)
      s[size++] = spec->name[i];
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
    for (i = buf[0]; i; i--)
      s[size++] = buf[i];
	  
	dirID = pbrec.dirInfo.ioDrParID;
	if (dirID == 1)
	  break;
  }
  
  if (alloced < QUICK_BUF_SIZE) {
    s = (char *)scheme_malloc_atomic(size + 1);
    for (j = 0, i = size; i--; j++)
      s[j] = qbuf[i];
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

void scheme_file_create_hook(char *filename)
{
  FSSpec spec;
    
  if (find_mac_file(filename, &spec, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)) {
    FInfo info;

    FSpGetFInfo(&spec, &info);
    info.fdCreator = scheme_creator_id;
    info.fdType = 'TEXT';
    FSpSetFInfo(&spec, &info);
  }
}

int scheme_mac_path_to_spec(const char *filename, FSSpec *spec, long *type)
{
  return find_mac_file(filename, spec, 0, 0, NULL, NULL, NULL, NULL, NULL, type, NULL, NULL);
}
#endif

static int has_null(const char *s, long l)
{
  while (l--)
    if (!s[l])
      return 1;

  return 0;
}

static void raise_null_error(const char *name, Scheme_Object *path, const char *mod)
{
  scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH,
		   path,
		   "%s: pathname%s contains a null character: %.255s...", 
		   name, mod, SCHEME_STR_VAL(path));
}

#ifdef DOS_FILE_SYSTEM
static int check_dos_slashslash_drive(const char *next, int len, 
				      int *drive_end, int exact)
{
  int j;
  int is_drive = 0;

  if (drive_end)
    *drive_end = len;

  if (IS_A_SEP(next[1])) {
    /* Check for a drive form: //x/y */
    for (j = 2; j < len; j++)
      if (!IS_A_SEP(next[j])) {
	/* Found non-sep */
	for (; j < len; j++)
	  if (IS_A_SEP(next[j])) {
	    /* Found sep again: */
	    for (; j < len; j++)
	      if (!IS_A_SEP(next[j])) {
		/* Found non-sep again */
		for (; j < len; j++)
		  if (IS_A_SEP(next[j])) {
		    /* Found sep again... */
		    if (drive_end)
		      *drive_end = j;
		    if (exact) {
		      for (; j < len; j++)
			if (!IS_A_SEP(next[j])) {
			  /* Found non-sep again 
			     - not a drive (it's absolute path) */
			  break;
			}
		    } else
		      is_drive = 1;
		    break;
		  }
		if (j >= len)
		  is_drive = 1;
		break;
	      }
	    break;
	  }
	break;
      }
  }

  return is_drive;
}
#endif

static char *do_expand_filename(char* filename, int ilen, char *errorin, 
				int *expanded,
				int report_bad_user)
{
  if (expanded)
    *expanded = 0;

  if (ilen < 0)
    ilen = strlen(filename);
  else  {
    if (has_null(filename, ilen)) {
      if (errorin)
	raise_null_error(errorin, scheme_make_string(filename), "");
      else 
	return NULL;
    }
  }

#ifdef EXPAND_FILENAME_TILDE
  /* User home lookup strategy taken from wxWindows: */

  if (filename[0] == '~') {
    char user[256], *home = NULL, *naya;
    struct passwd *who = NULL;
    int u, f, len, flen;
    
    for (u = 0, f = 1; 
	 u < 255 && filename[f] && filename[f] != '/'; 
	 u++, f++)
      user[u] = filename[f];

    if (filename[f] && filename[f] != '/') {
      if (errorin && report_bad_user)
	scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH_USERNAME,
			    scheme_make_string(filename),
			    "%s: bad username in %.255s", errorin, filename);
      return NULL;
    }
    user[u] = 0;

    if (!user[0]) {
      if (!(home = getenv("HOME"))) {
	char *ptr;

	ptr = getenv("USER");
	if (!ptr)
	  ptr = getenv("LOGNAME");

	who = ptr ? getpwnam(ptr) : NULL;

	if (!who)
	  who = getpwuid(getuid());
      }
    } else
      who = getpwnam(user);

    if (!home && who)
      home = who->pw_dir;

    if (!home) {
      if (errorin && report_bad_user)
	scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH_USERNAME,
			    scheme_make_string(filename),
			    "%s: bad username in %.255s", errorin, filename);
      return NULL;
    }

    len = strlen(home);
    if (f < ilen) 
      flen = ilen - f - 1;
    else
      flen = 0;
    naya = (char *)scheme_malloc_atomic(len + flen + 2);
    memcpy(naya, home, len);
    naya[len] = '/';
    memcpy(naya + len + 1, filename + f + 1, flen);
    naya[len + flen + 1] = 0;

    if (expanded)
      *expanded = 1;
  
    return naya;
  }
#endif
#ifdef DOS_FILE_SYSTEM
  {
    int fixit = 0, i;

    /* Clean up the name, removing mulitple // and
       adding "/" after "c:" if necessary */
    if (isalpha(filename[0])
	&& (filename[1] == ':') && !IS_A_SEP(filename[2]))
      fixit = -1;
    else {
      int found_slash = 0;

      for (i = ilen; i-- > 1; ) {
	if (IS_A_SEP(filename[i])) {
	  if (IS_A_SEP(filename[i - 1])) {
	    if ((i > 1) || !found_slash)
	      fixit = 1;
	    break;
	  }
	  found_slash = 1;
	}
      }
    }

    if (fixit) {
      int allow_leading, pos;
      char *naya;
      
      if (expanded)
	*expanded = 1;

      /* Allow // at start? */
      allow_leading = 0;
      if (IS_A_SEP(filename[0]) && IS_A_SEP(filename[1])) {
	for (i = 2; i < ilen; i++)
	  if (!IS_A_SEP(filename[i])) {
	    /* Found non-sep */
	    for (; i < ilen; i++)
	      if (IS_A_SEP(filename[i])) {
		/* Found sep */
		for (; i < ilen; i++)
		  if (!IS_A_SEP(filename[i])) {
		    /* Found non-sep; allow leading */
		    allow_leading = 1;
		    break;
		  }
		break;
	      }
	    break;
	  }
      }

      pos = i = 0;
      naya = (char *)scheme_malloc_atomic(ilen + 2);

      if (allow_leading) {
	naya[0] = filename[0];
	naya[1] = filename[1];
	pos = i = 2;
      } else if (fixit == -1) {
	naya[0] = filename[0];
	naya[1] = ':';
	naya[2] = '\\';
	pos = 3;
	i = 2;
      }

      while (i < ilen) {
	if (IS_A_SEP(filename[i])
	    && IS_A_SEP(filename[i + 1])) {
	  i++;
	} else
	  naya[pos++] = filename[i++];
      }

      naya[pos] = 0;
      filename = naya;
      ilen = pos;
    }
  }
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  {
    FSSpec spec;
    int dealiased, wasdir;
    
    if (find_mac_file(filename, &spec, 0, 0, &dealiased, &wasdir, NULL, NULL, NULL, NULL, NULL, NULL))
      if (dealiased) {
        char *s;
        s = scheme_build_mac_filename(&spec, wasdir);
	if (s) {
	  if (expanded)
	    *expanded = 1;
	  return s;
        }
    }
  }
#endif

  return filename;
}

char *scheme_expand_filename(char* filename, int ilen, char *errorin, int *expanded)
{
  return do_expand_filename(filename, ilen, errorin, expanded, 1);
}

int scheme_file_exists(char *filename)
{
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  
  if (!find_mac_file(filename, &spec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL))
    return 0;
  return 1;

#else
#ifdef NO_STAT_PROC
  FILE *fp;

  fp = fopen(filename, "r");
  if (fp) {
    fclose(fp);
    return 1;
  } else
    return 0;
#else
  struct MSC_IZE(stat) buf;

  return !MSC_IZE(stat)(filename, &buf) && !S_ISDIR(buf.st_mode);
#endif
#endif
}

#ifndef USE_WINDOWS_FIND_FIRST
# define FIND_FIRST _findfirst
# define FIND_NEXT _findnext
# define FIND_CLOSE _findclose
# define FF_TYPE struct _finddata_t
# define FF_HANDLE_TYPE long
# define FIND_FAILED(h) (h < 0)
# define GET_FF_ATTRIBS(fd) (fd.attrib)
#else
# define FIND_FIRST FindFirstFile
# define FIND_NEXT FindNextFile
# define FIND_CLOSE FindClose
# define FF_TYPE WIN32_FIND_DATA
# define FF_HANDLE_TYPE HANDLE
# define FIND_FAILED(h) (h == INVALID_HANDLE_VALUE)
# define _A_RDONLY FILE_ATTRIBUTE_READONLY
# define GET_FF_ATTRIBS(fd) (fd.dwFileAttributes)
#endif

#ifdef DOS_FILE_SYSTEM
static int UNC_dir_exists(char *dirname, int len, int *flags)
{
  char *copy;
  FF_TYPE fd;
  FF_HANDLE_TYPE fh;
  
  copy = scheme_malloc_atomic(len + 10);
  memcpy(copy, dirname, len);
  if (!IS_A_SEP(copy[len - 1])) {
    copy[len] = '\\';
    len++;
  }
  memcpy(copy + len, "*.*", 4);
  fh = FIND_FIRST(copy, &fd);
  if (FIND_FAILED(fh))
    return 0;
  else {
    if (flags)
      *flags = GET_FF_ATTRIBS(fd);
    FIND_CLOSE(fh);
    return 1;
  }
}
#endif

int scheme_directory_exists(char *dirname)
{
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  
  if (!find_mac_file(dirname, &spec, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL))
    return 0;

  return 1;
#else
#ifdef NO_STAT_PROC
  return 0;
#else
  struct MSC_IZE(stat) buf;
  int v;

#ifdef DOS_FILE_SYSTEM
  int strip_end, strip_char, len;

  len = strlen(dirname);

  if ((len > 1) && IS_A_SEP(dirname[0]) && check_dos_slashslash_drive(dirname, len, NULL, 1)) {
    /* stat doesn't work with UNC "drive" names */
    return UNC_dir_exists(dirname, len, NULL);
  } else if ((len > 1) && (dirname[len - 1] == '\\' || dirname[len - 1] == '/')
	     && (dirname[len - 2] != ':')) {
    strip_end = len - 1;
    strip_char = dirname[strip_end];
    dirname[strip_end] = 0;
  } else
    strip_end = strip_char = 0;
#endif

  v = !MSC_IZE(stat)(dirname, &buf) && S_ISDIR(buf.st_mode);
  
#ifdef DOS_FILE_SYSTEM
  if (strip_end)
    dirname[strip_end] = strip_char;
#endif
  
  return v;
#endif
#endif
}

int scheme_is_regular_file(char *filename)
{
#ifdef USE_MAC_FILE_TOOLBOX
  return 1;
#else
#ifdef NO_STAT_PROC
  return 0;
#else
  struct MSC_IZE(stat) buf;

  return !MSC_IZE(stat)(filename, &buf) && S_ISREG(buf.st_mode);
#endif  
#endif
}

static Scheme_Object *file_exists(int argc, Scheme_Object **argv)
{
  char *f;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("file-exists?", "string", 0, argc, argv);

  f = do_expand_filename(SCHEME_STR_VAL(argv[0]),
			 SCHEME_STRTAG_VAL(argv[0]),
			 "file-exists?",
			 NULL,
			 0);

  return (f && scheme_file_exists(f)) ? scheme_true : scheme_false;
}

static Scheme_Object *directory_exists(int argc, Scheme_Object **argv)
{
  char *f;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("directory-exists?", "string", 0, argc, argv);

  f = do_expand_filename(SCHEME_STR_VAL(argv[0]),
			 SCHEME_STRTAG_VAL(argv[0]),
			 "directory-exists?",
			 NULL,
			 0);

  return (f && scheme_directory_exists(f)) ? scheme_true : scheme_false;
}

static Scheme_Object *link_exists(int argc, Scheme_Object **argv)
{
  char *filename;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("link-exists?", "string", 0, argc, argv);

  filename = SCHEME_STR_VAL(argv[0]);

#ifndef UNIX_FILE_SYSTEM
  if (has_null(filename, SCHEME_STRTAG_VAL(argv[0]))) {
    raise_null_error("link-exists?", scheme_make_string(filename), "");
    return NULL;
  }
#endif

#ifdef DOS_FILE_SYSTEM
  return scheme_false;
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  {
    FSSpec spec;
  
    if (!find_mac_file(filename, &spec, 0, -1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL))
      return scheme_false;

    return scheme_true;
  }
#endif
#ifdef UNIX_FILE_SYSTEM
  {
    struct MSC_IZE(stat) buf;

    filename = do_expand_filename(filename,
				  SCHEME_STRTAG_VAL(argv[0]),
				  "link-exists?",
				  NULL,
				  0);

    if (!MSC_IZE(lstat)(filename, &buf) && S_ISLNK(buf.st_mode))
      return scheme_true;
    else
      return scheme_false;
  }
#endif
}

#ifdef DOS_FILE_SYSTEM
static char get_cur_drive_id(void)
{
#ifdef USE_GETDRIVE
  char cwd[1];
  int drive = _getdrive();
  cwd[0] = 'a' + (drive -1);
#else
  char buffer[256], *cwd;
  
  if (!(cwd = scheme_getcwd(buffer, 256, NULL, 0)))
    return 'c';
#endif
  return cwd[0];
}
#endif

char *scheme_getdrive()
{
#ifdef DOS_FILE_SYSTEM
  char *drive = MALLOC_N(char, 4);

  drive[0] = get_cur_drive_id();
  drive[1] = ':';
  drive[2] = '\\';
  drive[3] = 0;

  return drive;
#else
  return "";
#endif
}

char *scheme_normal_path_case(char *si, int len)
{
#ifndef UNIX_FILE_SYSTEM
  int i;
  char *s;

  s = MALLOC_N_ATOMIC(char, len + 1);
  memcpy(s, si, len + 1);

  for (i = 0; i < len; i++) {
    s[i] = tolower(s[i]);
#ifdef DOS_FILE_SYSTEM
    if (s[i] == '/')
      s[i] = '\\';
#endif
  }

  return s;
#else
  return si;
#endif
}

Scheme_Object *scheme_build_pathname(int argc, Scheme_Object **argv)
{
#define PN_BUF_LEN 256
  int pos, i, len, no_sep;
  int alloc = PN_BUF_LEN;
  char buffer[PN_BUF_LEN], *str, *next;
  int rel;
#ifdef MAC_FILE_SYSTEM
  int prev_no_sep;
#endif
#ifdef DOS_FILE_SYSTEM
  int first_was_drive = 0;
#endif

  str = buffer;
  pos = 0;

  no_sep = 0; /* This is acutally initialized after we know whether
		 it's relative or not. */
  
  for (i = 0 ; i < argc; i++) {
    if (SCHEME_STRINGP(argv[i])
	|| (SCHEME_SYMBOLP(argv[i]) 
	    && (SAME_OBJ(argv[i], up_symbol)
		|| SAME_OBJ(argv[i], same_symbol)))) {
      if (SAME_OBJ(argv[i], up_symbol)) {
#ifdef UNIX_FILE_SYSTEM
	next = "..";
	len = 2;
#endif
#ifdef DOS_FILE_SYSTEM
	next = "..";
	len = 2;
#endif
#ifdef MAC_FILE_SYSTEM
	next = "";
	len = 0;
#endif
      } else if (SAME_OBJ(argv[i], same_symbol)) {
#ifdef UNIX_FILE_SYSTEM
	next = ".";
	len = 1;
#endif
#ifdef DOS_FILE_SYSTEM
	next = ".";
	len = 1;
#endif
#ifdef MAC_FILE_SYSTEM
	next = "";
	len = 0;
#endif
      } else {
	next = SCHEME_STR_VAL(argv[i]);
	len = SCHEME_STRTAG_VAL(argv[i]);
#ifdef MAC_FILE_SYSTEM
	/* ":" is not a legal particle */
	if ((len == 1) && (next[0] == ':'))
	  len = 0;
#endif	
	if (!len) {
	  scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH,
			   scheme_make_string(next),
			   "build-path: %d%s pathname element is an empty string%s", 
			   i + 1,
			   scheme_number_suffix(i + 1),
			   scheme_make_args_string("other ", i, argc, argv)); 
	  return scheme_false;
	}

	if (has_null(next, len)) {
	  raise_null_error("build-path", argv[i], " element");
	  return NULL;
	}
      }

      /* +3: null term, leading sep, and trailing sep (if up & Mac) */
      if (pos + len + 3 >= alloc) {
	char *naya;
	int newalloc;

	newalloc = 2 * alloc + len + 1;
	naya = (char *)scheme_malloc_atomic(newalloc);
	memcpy(naya, str, pos);
	
	str = naya;
      }

#ifdef UNIX_FILE_SYSTEM
      if (next[0] == '/') {
	rel = 0;
	if (i) {
	  scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH,
			      scheme_make_string(next),
			      "build-path: absolute path \"%.255s\" cannot be"
			      " added to a pathname",
			   next);
	  return scheme_false;
	}
      } else
	rel = 1;
#endif
#ifdef DOS_FILE_SYSTEM
      {
	int is_drive;

	if (IS_A_SEP(next[0])) {
	  rel = 0;
	  is_drive = check_dos_slashslash_drive(next, len, NULL, 1);
	} else if ((len >= 2) 
		   && isalpha(next[0])
		   && (next[1] == ':')) {
	  int j;
	  rel = 0;
	  for (j = 2; j < len; j++)
	    if (!IS_A_SEP(next[j]))
	      break;
	  is_drive = (j >= len);
	} else {
	  rel = 1;
	  is_drive = 0;
	}

	if (!rel) {
	  if (i && (!first_was_drive || (i > 1) || is_drive)) {
	    if (pos > 30) {
	      str[27] = '.';
	      str[28] = '.';
	      str[29] = '.';
	      str[30] = 0;
	    } else
	      str[pos] = 0;
	    scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH,
			     scheme_make_string(next),
			     "build-path: %s \"%s\" cannot be"
			     " added to the pathname \"%.255s\"",
			     is_drive ? "drive" : "absolute path",
			     next, str);
	    return scheme_false;
	  }

	  if (i == 1) {
	    /* Absolute path onto a drive: skip separator(s) */
	    while (len && IS_A_SEP(next[0])) {
	      next++;
	      len--;
	    }
	  }
	}

	if (!i)
	  first_was_drive = is_drive;
      }
#endif
#ifdef MAC_FILE_SYSTEM
      /* If we're appending a relative path, strip leading sep; otherwise,
         check for embedded colons */
      if (next[0] == FN_SEP) {
	rel = 1;
	if (i) {
	  next++;
	  --len;
	}
      } else {
	/* Except for first, ignore a colon at the end. */
	int last = i ? len - 1 : len, j;
	rel = 1;
	for (j = 0; j < last; j++)
	  if (next[j] == ':') {
	    if (i) {
	      scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH,
			       scheme_make_string(next),
			       "build-path: absolute path \"%.255s\" cannot be"
			       " added to a pathname",
			       next);
	      return scheme_false;
	    } else {
	      rel = 0;
	      break;
	    }
	  }
      }
#endif

      if (!i) {
#ifdef MAC_FILE_SYSTEM
	no_sep = !rel || (next[0] == ':');
#else
	no_sep = 1;
#endif
      }
      
#ifdef MAC_FILE_SYSTEM
      if (SAME_OBJ(argv[i], same_symbol)) {
	prev_no_sep = no_sep;
	no_sep = 1;
      }
#endif

      if (!no_sep)
	str[pos++] = FN_SEP;

      memcpy(str + pos, next, len);
      pos += len;

      /* If last path elem ends in a separator, don't add one: */
      if (len) {
	no_sep = IS_A_SEP(next[len - 1]);
#ifdef MAC_FILE_SYSTEM
	if (!len)
	  no_sep = 0;
#endif
      } else {
#ifdef MAC_FILE_SYSTEM
	if (SAME_OBJ(argv[i], same_symbol))
	  no_sep = prev_no_sep;
	else
	  no_sep = 0;
#else
	no_sep = 0;
#endif
      }
    } else {
      scheme_wrong_type("build-path", "string or 'up or 'same", i, argc, argv);
      return scheme_false;
    }
  }

#ifdef MAC_FILE_SYSTEM
  if (argc && SAME_OBJ(argv[argc - 1], up_symbol))
    str[pos++] = ':';
#endif

  str[pos] = 0;

  return scheme_make_sized_string(str, pos, alloc == PN_BUF_LEN);
}

Scheme_Object *scheme_split_pathname(int argc, Scheme_Object **argv)
{
  char *s;
  int p, len, last_was_sep = 0, is_dir;
  Scheme_Object *file, *three[3], *inpath;
#ifdef DOS_FILE_SYSTEM
  int allow_double_before, drive_end;
#endif

  inpath = argv[0];

  if (!SCHEME_STRINGP(inpath))
    scheme_wrong_type("split-path", "string", 0, argc, argv);

  s = SCHEME_STR_VAL(inpath);
  len = SCHEME_STRTAG_VAL(inpath);

  if (!len) {
    scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH,
		     inpath, "split-path: pathname is an empty string");
  }

  if (has_null(s, len))
    raise_null_error("split-path", inpath, "");

#ifdef DOS_FILE_SYSTEM
  allow_double_before = 0;
  if ((len > 2) && IS_A_SEP(s[0]) && IS_A_SEP(s[1])) {
    if (check_dos_slashslash_drive(s, len, &drive_end, 0))
      allow_double_before = 1;
    else
      drive_end = 0;
  } else if ((len > 1) && isalpha(s[0]) && (s[1] == ':'))
    drive_end = 2;
  else
    drive_end = 0;

  if (drive_end && IS_A_SEP(s[drive_end]))
    drive_end++;
#endif

#ifndef MAC_FILE_SYSTEM
#ifdef DOS_FILE_SYSTEM
# define ALLOW_DOUBLE_BEFORE allow_double_before
#else
# define ALLOW_DOUBLE_BEFORE 0
#endif
  /* Look for confusing repeated separators (e.g. "x//y") */
  for (p = len; p--; ) {
    if (p > ALLOW_DOUBLE_BEFORE) {
      if (IS_A_SEP(s[p]) && IS_A_SEP(s[p - 1])) {
	/* Found it; copy without repeats */
	int q;
	char *old = s;

	s = (char *)scheme_malloc_atomic(len);
	--len;

	for (p = 0, q = 0; p < ALLOW_DOUBLE_BEFORE; p++)
	  s[q++] = old[p];

	for (; p < len; p++) {
	  if (!IS_A_SEP(old[p]) || !IS_A_SEP(old[p + 1]))
	    s[q++] = old[p];
	}
	s[q++] = old[len];
	len = q;
	break;
      }
    }
  }
#endif

#ifdef DOS_FILE_SYSTEM
  if (len <= drive_end)
    p = -1;
  else
#endif
    {
      for (p = len; p--; ) {
	if (IS_A_SEP(s[p])) {
	  if (p != len - 1)
	    break;
	  else
	    last_was_sep = 1;
	}
#ifdef DOS_FILE_SYSTEM
	if (p < drive_end)
	  break;
#endif
      }
    }

#define MAKE_SPLIT(x, y, z) \
  (three[0] = x, three[1] = y, three[2] = z ? scheme_true : scheme_false, \
   scheme_values(3, three))

  if (p < 0) {
    Scheme_Object *dir;

    /* No splitting available. 
       For Unx & DOS, it was relative or exactly root.
       For Mac, it is relative or root with trailing sep. */
#ifdef UNIX_FILE_SYSTEM
    if (s[0] == '/')
      return MAKE_SPLIT(scheme_false, scheme_make_sized_string(s, len, 1), 1);
#endif
#ifdef DOS_FILE_SYSTEM
    if (IS_A_SEP(s[0]) || drive_end)
      return MAKE_SPLIT(scheme_false, scheme_make_sized_string(s, len, 1), 1);
#endif

#ifdef MAC_FILE_SYSTEM
    dir = last_was_sep ? scheme_false : relative_symbol;
#else
    dir = relative_symbol;
#endif

  /* Check for 'up: */
#ifndef MAC_FILE_SYSTEM
  if ((s[0] == '.') && (s[1] == '.')
      && (2 >= len || IS_A_SEP(s[2])))
    {
      file = up_symbol;
      is_dir = 1;
    } 
  else if ((s[0] == '.') && (1 >= len || IS_A_SEP(s[1])))
    {
      file = same_symbol;
      is_dir = 1;
    } 
  else
#endif
    {
      file = scheme_make_sized_string(s, len - last_was_sep, 1);
      is_dir = last_was_sep;
    }

    return MAKE_SPLIT(dir, file, is_dir);
  }
		 
  /* Check for 'up and 'same: */
#if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM)
  if ((s[p + 1] == '.') && (s[p + 2] == '.')
      && (p + 3 >= len || IS_A_SEP(s[p + 3])))
#endif
#ifdef MAC_FILE_SYSTEM
  if ((p == len - 2) && s[p + 1] == ':')
#endif  
    {
      file = up_symbol;
      is_dir = 1;
    } 
#ifndef MAC_FILE_SYSTEM
  else if ((s[p + 1] == '.') && (p + 2 >= len || IS_A_SEP(s[p + 2])))
    {
      file = same_symbol;
      is_dir = 1;
    }
#endif
  else 
    {
      file = scheme_make_sized_string(s + p + 1, 
				      len - p -last_was_sep-1, 
				      1);
      is_dir = last_was_sep;
    }

  /* Check directory */
  if (p > 0)
    return MAKE_SPLIT(scheme_make_sized_string(s, p + 1, 1), 
		      file, 
		      is_dir);
	
  /* p = 0; for Unix & Dos, this means root dir. For Mac, this means
     it was relative. */
#ifdef MAC_FILE_SYSTEM
  return MAKE_SPLIT(relative_symbol, file, is_dir);
#else
  return MAKE_SPLIT(scheme_make_sized_string(s, 1, 1), file, is_dir);
#endif  
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
	  && isalpha(s[0])
	  && (s[1] == ':')))
    return 0;
  else
    return 1;
#endif
#ifdef MAC_FILE_SYSTEM
  if (s[0] != ':')
    for (i = 1; i < len; i++)
      if (s[i] == ':')
	return 0;

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
	       && isalpha(s[0])
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
    char *s;
    int len;

    s = SCHEME_STR_VAL(wrt);
    len = SCHEME_STRTAG_VAL(wrt);
    
    if (has_null(s, len))
      raise_null_error("path->complete-path", p, "");

    if (!scheme_is_complete_path(s, len))
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM_PATH,
		       wrt,
		       "path->complete-path: second argument is not a complete path: \"%.255s\"",
		       s);
  }

  if (!scheme_is_complete_path(s, len)) {
    Scheme_Object *args[2];

#ifdef DOS_FILE_SYSTEM
    if (!scheme_is_relative_path(s, len)) {
      /* Get just the drive: */
      if (wrt) {
        char *s;
        int len;
      
        s = SCHEME_STR_VAL(wrt);
        len = SCHEME_STRTAG_VAL(wrt);
        if (s[1] == ':') {
	  wrt = scheme_make_sized_string(s, 2, 0);
        } else {
	  int i;

	  /* Skip first slashes: */
	  for (i = 0; IS_A_SEP(s[i]); i++);
	  /* Find next slash: */
	  for (; !IS_A_SEP(s[i]); i++);
	  /* Skip these slashes: */
	  for (; IS_A_SEP(s[i]); i++);
	  /* Find next slash or end-of-string: */
	  for (; s[i] && !IS_A_SEP(s[i]); i++);
	
	  wrt = scheme_make_sized_string(s, i, 0);
	}
      } else {
	char d[4];

        d[0] = get_cur_drive_id();
        d[1] = ':';
        d[2] = 0;

        wrt = scheme_make_sized_string(d, 2, 1);
      }
    }
#endif

    if (!wrt) {
      char *d = scheme_getcwd(NULL, 0, NULL, 0);
      wrt = scheme_make_string(d);
    }

    args[0] = wrt;
    args[1] = p;
    return scheme_build_pathname(2, args);
  } else
    return p;
}

#ifndef NO_FILE_SYSTEM_UTILS

static Scheme_Object *delete_file(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("delete-file", "string", 0, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX
  {
    FSSpec spec;
    char *file;
    
    file = SCHEME_STR_VAL(argv[0]);
    if (has_null(file, SCHEME_STRTAG_VAL(argv[0])))
      raise_null_error("delete-file", argv[0], "");
    
    if (!find_mac_file(file, &spec, 0, -2, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL))
      return scheme_false;

    if (FSpDelete(&spec))
      return scheme_false;
    else
      return scheme_true;
  }
#endif

  if (!MSC_IZE(unlink)(scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
					      SCHEME_STRTAG_VAL(argv[0]),
					      "delete-file",
					      NULL)))
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *rename_file(int argc, Scheme_Object **argv)
{
  char *src, *dest;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec srcspec, destspec;
  int swas_dir, sexists, dexists;
#endif

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("rename-file", "string", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("rename-file", "string", 1, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX
  src = SCHEME_STR_VAL(argv[0]);
  if (has_null(src, SCHEME_STRTAG_VAL(argv[0])))
	raise_null_error("rename-file", argv[0], "");
  dest = SCHEME_STR_VAL(argv[1]);
  if (has_null(dest, SCHEME_STRTAG_VAL(argv[1])))
	raise_null_error("rename-file", argv[1], "");
#else
  src = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
			       SCHEME_STRTAG_VAL(argv[0]),
			       "rename-file",
			       NULL);
  dest = scheme_expand_filename(SCHEME_STR_VAL(argv[1]),
				SCHEME_STRTAG_VAL(argv[1]),
				"rename-file",
				NULL);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  if (find_mac_file(src, &srcspec, 0, -3, NULL, &swas_dir, &sexists, NULL, NULL, NULL, NULL, NULL)
      && sexists) {
    if (find_mac_file(dest, &destspec, 0, 0, NULL, NULL, &dexists, NULL, NULL, NULL, NULL, NULL)) {
      /* Directory already exists or different volumes => failure */
      if (!dexists && (srcspec.vRefNum == destspec.vRefNum)) {
        int rename;
        
        if (swas_dir) {
          /* Get parent of directory to be moved: */
          CInfoPBRec pb;
          
          pb.hFileInfo.ioNamePtr = srcspec.name;
          pb.hFileInfo.ioVRefNum = srcspec.vRefNum;
          pb.hFileInfo.ioFDirIndex = -1;
          pb.hFileInfo.ioDirID = srcspec.parID;
          if (PBGetCatInfo(&pb, 0))
            return scheme_false;
            
          srcspec.parID = pb.dirInfo.ioDrParID;
        }
        
        rename = ((destspec.name[0] != srcspec.name[0])
                  || memcmp(destspec.name, srcspec.name, destspec.name[0] + 1));
        
        if (srcspec.parID != destspec.parID) {
          CMovePBRec mv;
          mv.ioNamePtr = srcspec.name;
          mv.ioVRefNum = srcspec.vRefNum;
          mv.ioDirID = srcspec.parID;
          mv.ioNewName = NULL;
          mv.ioNewDirID = destspec.parID;
          if (PBCatMove(&mv, 0))
            return scheme_false;
        }
        
        if (rename) {
          srcspec.parID = destspec.parID;
          if (FSpRename(&srcspec, destspec.name))
            return scheme_false;
      	}
      	
        return scheme_true;
      }
    }
  }
  
  return scheme_false;
#else
  if (scheme_file_exists(dest) || scheme_directory_exists(dest))
    return scheme_false;
  
  if (!rename(src, dest))
    return scheme_true;
  else
    return scheme_false;
#endif
}

#ifdef MAC_FILE_SYSTEM
static int xCopyFile(short dest, short src) {
  long i, j;
  char buffer[256]; 
  
  do {
    i = 256;
    FSRead(src, &i, buffer);
    j = i;
    FSWrite(dest, &i, buffer);
    if (j != i) return 0;
  } while (i);
  
  return 1;
}
#endif

static Scheme_Object *copy_file(int argc, Scheme_Object **argv)
{
  char *src, *dest;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("copy-file", "string", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("copy-file", "string", 1, argc, argv);

#ifdef MAC_FILE_SYSTEM
  src = SCHEME_STR_VAL(argv[0]);
  if (has_null(src, SCHEME_STRTAG_VAL(argv[0])))
	raise_null_error("copy-file", argv[0], "");
  dest = SCHEME_STR_VAL(argv[1]);
  if (has_null(dest, SCHEME_STRTAG_VAL(argv[1])))
	raise_null_error("copy-file", argv[1], "");
#else
  src = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
			       SCHEME_STRTAG_VAL(argv[0]),
			       "copy-file",
			       NULL);
  dest = scheme_expand_filename(SCHEME_STR_VAL(argv[1]),
				SCHEME_STRTAG_VAL(argv[1]),
				"copy-file",
				NULL);
#endif

#ifdef UNIX_FILE_SYSTEM
  {
# define COPY_BUFFER_SIZE 2048
    FILE *s, *d;
    char b[COPY_BUFFER_SIZE];
    long len;
    int ok;
    struct stat buf;

    if (stat(src, &buf) || S_ISDIR(buf.st_mode))
      return scheme_false;

    if (!stat(dest, &buf))
      return scheme_false;

    s = fopen(src, "rb");
    if (!s)
      return scheme_false;

    d = fopen(dest, "wb");
    if (!d) {
      fclose(s);
      return scheme_false;
    }
    
    ok = 1;
    while ((len = fread(b, 1, COPY_BUFFER_SIZE, s))) {
      if (fwrite(b, 1, len, d) != len) {
	ok = 0;
	break;
      }
    }
    if (!feof(s))
      ok = 0;

    fclose(s);
    fclose(d);

    if (ok) {
      if (chmod(dest, buf.st_mode))
	return scheme_false;
      return scheme_true;
    } else
      return scheme_false;
  }
#endif
#ifdef DOS_FILE_SYSTEM
  return CopyFile(src, dest, TRUE) ? scheme_true : scheme_false;
#endif
#ifdef MAC_FILE_SYSTEM
  { 
  	FInfo finfo;
    FSSpec srcspec, destspec;
    int exists;
    static OSErr en;
   
    if (find_mac_file(src, &srcspec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &finfo)) {
      if (find_mac_file(dest, &destspec, 0, 0, NULL, NULL, &exists, NULL, NULL, NULL, NULL, NULL)
      	  && !exists) {
        CopyParam rec;
        
        /* Try CopyFile first: */
        rec.ioVRefNum = srcspec.vRefNum;
        rec.ioDstVRefNum = destspec.vRefNum;
        rec.ioDirID = srcspec.parID;
        rec.ioNewDirID = destspec.parID;
        rec.ioNamePtr = srcspec.name;
        rec.ioNewName = destspec.name;
        rec.ioCopyName = NULL;
        if (!(en = PBHCopyFileSync((HParmBlkPtr)&rec)))
          return scheme_true;
        else {
          /* Try the old-fashioned way: */
          short sdf, srf, ddf, drf;
          if (!FSpOpenDF(&srcspec, fsRdPerm, &sdf)) {
            if (!FSpOpenRF(&srcspec, fsRdPerm, &srf)) {
              if (!FSpCreate(&destspec, finfo.fdCreator, finfo.fdType, smSystemScript)) {
                if (!FSpOpenDF(&destspec, fsWrPerm, &ddf)) {
                  if (!FSpOpenRF(&destspec, fsWrPerm, &drf)) {
                    if (xCopyFile(ddf, sdf)) {
                      if (xCopyFile(drf, srf)) {
	                    FSClose(drf);
	                    FSClose(ddf);
	                    FSClose(srf);
	                    FSClose(sdf);
	                    return scheme_true;
	                  }
	                }
	                FSClose(drf);
                  }
                  FSClose(ddf);
                }
                FSpDelete(&destspec);
              }
              FSClose(srf);
            }
            FSClose(sdf);
          }
        }
      }
    }
  }
#endif

  return scheme_false;
}

static Scheme_Object *relative_pathname_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("relative-path?", "string", 0, argc, argv);

  s = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  if (has_null(s, len))
    return scheme_false;

  return (scheme_is_relative_path(s, len)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *complete_pathname_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("complete-path?", "string", 0, argc, argv);

  s = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  if (has_null(s, len))
    return scheme_false;

  return (scheme_is_complete_path(s, len)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *absolute_pathname_p(int argc, Scheme_Object **argv)
{
  char *s;
  int len;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("absolute-path?", "string", 0, argc, argv);

  s = SCHEME_STR_VAL(argv[0]);
  len = SCHEME_STRTAG_VAL(argv[0]);

  if (!len || has_null(s, len))
    return scheme_false;

  return (!scheme_is_relative_path(s, len)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *resolve_path(int argc, Scheme_Object *argv[])
{
#ifndef NO_READLINK
#define SL_NAME_MAX 2048
  char buffer[SL_NAME_MAX];
#endif
#ifndef NO_READLINK
  long len;
  int copied = 0;
#endif
  char *filename;
  int expanded;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("resolve-path", "string", 0, argc, argv);

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "resolve-path",
				    &expanded);

#ifndef NO_READLINK
  /* Make sure pathname doesn't have trailing separator: */
  len = strlen(filename);
  while (len && IS_A_SEP(filename[len - 1])) {
    if (!expanded && !copied) {
      filename = scheme_strdup(filename);
      copied = 1;
    }
    filename[--len] = 0;
  }

  if ((len = readlink(filename, buffer, SL_NAME_MAX)) > 0)
    return scheme_make_sized_string(buffer, len, 1);
#endif

  if (!expanded)
    return argv[0];
  else
    return scheme_make_sized_string(filename, strlen(filename), 1);
}

static Scheme_Object *current_drive(int argc, Scheme_Object *argv[])
{
#ifdef DOS_FILE_SYSTEM
  char *drive = scheme_getdrive();

  return scheme_make_sized_string(drive, strlen(drive), 0);
#else
  return scheme_false;
#endif
}

static Scheme_Object *expand_path(int argc, Scheme_Object *argv[])
{
  char *filename;
  int expanded;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("expand-path", "string", 0, argc, argv);

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "expand-path",
				    &expanded);

#if 0
  /* Don't prefix a drive anymore. */
#ifdef DOS_FILE_SYSTEM
#ifdef DIR_FUNCTION
  if (IS_A_SEP(filename[0]) && !IS_A_SEP(filename[1])) {
    /* Put a drive on the filename: */
    int len;
    char *old = filename;
    
    len = strlen(filename);
    filename = (char *)scheme_malloc_atomic(len + 4);
    memcpy(filename + 2, old, len + 1);
    filename[0] = get_cur_drive_id();
    filename[1] = ':';
    filename[2] = '\\';
    
    expanded = 1;
  }
#endif
#endif
#endif

  if (!expanded)
    return argv[0];
  else
    return scheme_make_sized_string(filename, strlen(filename), 1);
}

static Scheme_Object *normal_path_case(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("normal-case-path", "string", 0, argc, argv);

#ifdef UNIX_FILE_SYSTEM
  return argv[0];
#else
  {
    int len = SCHEME_STRTAG_VAL(argv[0]);
    return scheme_make_sized_string(scheme_normal_path_case(SCHEME_STR_VAL(argv[0]), 
							    len), len, 0);
  }
#endif
}

static Scheme_Object *directory_list(int argc, Scheme_Object *argv[])
{
#if !defined(NO_READDIR) || defined(USE_MAC_FILE_TOOLBOX) || defined(USE_FINDFIRST)
  char *filename;
  Scheme_Object *first = scheme_null, *last = NULL, *n, *elem;
#endif
#ifndef NO_READDIR
  DIR *dir;
  int nlen;
  struct dirent *e;
#endif
#ifdef USE_FINDFIRST
  char *pattern;
  int len;
  long hfile;
  struct _finddata_t info;
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  CInfoPBRec pbrec;
  char buf[256];
  FSSpec dir;
  short find_position = 0;
#else
  int counter = 0;
#endif

  if (argc && !SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("directory-list", "string", 0, argc, argv);

#if defined(NO_READDIR) && !defined(USE_MAC_FILE_TOOLBOX) && !defined(USE_FINDFIRST)
  return scheme_null;
#else

#ifndef USE_MAC_FILE_TOOLBOX
  if (argc)
    filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				      SCHEME_STRTAG_VAL(argv[0]),
				      "directory-list",
				      NULL);
  else
    filename = NULL;

  if (filename && !scheme_directory_exists(filename))
    scheme_raise_exn(MZEXN_I_O_FILESYSTEM_DIRECTORY,
		     scheme_make_string(filename),
		     "directory-list: could not open \"%.255s\"",
		     filename);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  if (argc) {
    filename = SCHEME_STR_VAL(argv[0]);
    if (has_null(filename, SCHEME_STRTAG_VAL(argv[0])))
	  raise_null_error("directory-list", argv[0], "");
  } else
    filename = "";
  if (!find_mac_file(filename, &dir, 1, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)) {
    if (argc) {
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM_DIRECTORY,
		       argv[0],
		     "directory-list: could not open \"%.255s\"",
		     filename);
    }
    return scheme_null;
  }
  
  while (1) {
    pbrec.hFileInfo.ioVRefNum = dir.vRefNum;
    pbrec.hFileInfo.ioDirID = dir.parID;
    pbrec.hFileInfo.ioFDirIndex = find_position + 1;
    pbrec.hFileInfo.ioNamePtr = (unsigned char *)buf;
    if (PBGetCatInfo(&pbrec, 0) || !*buf)
      break;
    
    find_position++;

    if (!(find_position & 0x15))
      scheme_process_block(0);
    
    n = scheme_make_sized_string(buf + 1, buf[0], 1);
    elem = scheme_make_pair(n, scheme_null);
    if (last)
      SCHEME_CDR(last) = elem;
    else
      first = elem;
    last = elem;
  }
  
  return first;
#else
#ifdef USE_FINDFIRST

  if (!filename)
    pattern = "*.*";
  else {
    len = strlen(filename);
    pattern = (char *)scheme_malloc_atomic(len + 5);
    memcpy(pattern, filename, len);
    if (len && !IS_A_SEP(pattern[len - 1]))
      pattern[len++] = '\\';      
    memcpy(pattern + len, "*.*", 4);
  }

  hfile = FIND_FIRST(pattern, &info);
  if (FIND_FAILED(hfile))
    return scheme_null;

  do {
    if ((info.name[0] == '.')
	&& (!info.name[1] || ((info.name[1] == '.')
			      && !info.name[2]))) {
      /* skip . and .. */
    } else {
      n = scheme_make_string(info.name);
      elem = scheme_make_pair(n, scheme_null);
      if (last)
	SCHEME_CDR(last) = elem;
      else
	first = elem;
      last = elem;
    }
    counter++;
    if (!(counter & 0x15))
      scheme_process_block(0);
  } while (!FIND_NEXT(hfile, &info));
  FIND_CLOSE(hfile);

  return first;
#else
  
  dir = opendir(filename ? filename : ".");
  if (!dir)
    return scheme_null;
  
  while ((e = readdir(dir))) {
#ifdef DIRENT_NO_NAMLEN
    nlen = strlen(e->d_name);
#else
    nlen = e->d_namlen;
#endif
#if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM)
    if (nlen == 1 && e->d_name[0] == '.')
      continue;
    if (nlen == 2 && e->d_name[0] == '.' && e->d_name[1] == '.')
      continue;
#endif
    n = scheme_make_sized_string(e->d_name, nlen, 1);
    elem = scheme_make_pair(n, scheme_null);
    if (last)
      SCHEME_CDR(last) = elem;
    else
      first = elem;
    last = elem;

    counter++;
    if (!(counter & 0x15))
      scheme_process_block(0);
  }
  
  closedir(dir);

  return first;
#endif
#endif
#endif
}

static Scheme_Object *filesystem_root_list(int argc, Scheme_Object *argv[])
{
  Scheme_Object *first = scheme_null;
#if defined(DOS_FILE_SYSTEM) || defined(USE_MAC_FILE_TOOLBOX)
  Scheme_Object *last = NULL, *v;
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  HParamBlockRec rec;
  int i;
#endif

#ifdef UNIX_FILE_SYSTEM 
  first = scheme_make_pair(scheme_make_string("/"), scheme_null);
#endif
#ifdef DOS_FILE_SYSTEM
  {
    unsigned count, i;
    char buffer[4];
#ifdef USE_GETDRIVE
    int orig_drive = _getdrive();
    count = 26;
#else
#ifdef USE_GETDISK
    count = setdisk(getdisk());
#else
    /* Just guess: */
    count = 3;
#endif
#endif

    buffer[1] = ':';
    buffer[2] = '\\';
    buffer[3] = 0;

    for (i = 0; i < count; i++) {
#ifdef USE_GETDRIVE
      if (!_chdrive(i + 1))
#endif
	{
	  buffer[0] = i + 'a';
	  v = scheme_make_pair(scheme_make_string(buffer), scheme_null);
	  if (last)
	    SCHEME_CDR(last) = v;
	  else
	    first = v;
	  last = v;
	}
    }
#ifdef USE_GETDRIVE
    _chdrive(orig_drive);
#endif
  }
#endif
#ifdef USE_MAC_FILE_TOOLBOX
  i = 1;
  while (1) {
    Str255 name;
    Scheme_Object *v;
    
    rec.volumeParam.ioVolIndex = i;
    rec.volumeParam.ioNamePtr = name;
    
    if (PBHGetVInfo(&rec, 0))
      break;
    
    name[name[0] + 1] = ':';
    v = scheme_make_pair(scheme_make_sized_string((char *)name + 1, 
						  name[0] + 1, 1), 
    	                 scheme_null);
    if (last)
      SCHEME_CDR(last) = v;
    else
      first = v;
    last = v;
    i++;
   }
#endif

  return first;
}

static Scheme_Object *make_directory(int argc, Scheme_Object *argv[])
{
#ifdef NO_MKDIR
  return scheme_false;
#else
  char *filename;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("make-directory", "string", 0, argc, argv);

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "make-directory",
				    NULL);
  
  if (MSC_IZE(mkdir)(filename
#ifndef MKDIR_NO_MODE_FLAG
		     , 0xFFFF
#endif
		     ))
    return scheme_false;
  else
    return scheme_true;
#endif
}

static Scheme_Object *delete_directory(int argc, Scheme_Object *argv[])
{
#ifdef NO_RMDIR
  return scheme_false;
#else
  char *filename;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("delete-directory", "string", 0, argc, argv);

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "delete-directory",
				    NULL);
  
  if (MSC_IZE(rmdir)(filename))
    return scheme_false;
  else
    return scheme_true;
#endif
}

static Scheme_Object *file_modify_seconds(int argc, Scheme_Object **argv)
{
  char *file;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  long mtime;
  int exists;
#else
  struct MSC_IZE(stat) buf;
#endif

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("file-modify-seconds", "string", 0, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX	  
  file = SCHEME_STR_VAL(argv[0]);
  if (has_null(file, SCHEME_STRTAG_VAL(argv[0])))
	  raise_null_error("file-modify-seconds", argv[0], "");
#else
  file = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				SCHEME_STRTAG_VAL(argv[0]),
				"file-modify-seconds",
				NULL);
#endif

#ifdef USE_MAC_FILE_TOOLBOX	  
  if (!find_mac_file(file, &spec, 0, 0, NULL, NULL, &exists, &mtime, NULL, NULL, NULL, NULL)
      || !exists)
    return scheme_false;

  return scheme_make_integer_value_from_time(mtime);
#else
  if (MSC_IZE(stat)(file, &buf))
    return scheme_false;

  if (S_ISDIR(buf.st_mode))
    return scheme_false;

  return scheme_make_integer_value_from_time(buf.st_mtime);
#endif
}

#ifdef UNIX_FILE_SYSTEM
# define GROUP_CACHE_SIZE 10
struct {
  gid_t gid;
  char set, in;
} group_mem_cache[GROUP_CACHE_SIZE];
static int user_in_group(gid_t gid)
{
  struct group *g;
  struct passwd *pw;
  int i, in;

  for (i = 0; i < GROUP_CACHE_SIZE; i++)
    if (group_mem_cache[i].set && (group_mem_cache[i].gid == gid))
      return group_mem_cache[i].in;

  pw = getpwuid(getuid());
  if (!pw)
    return 0;

  g = getgrgid(gid);
  if (!g)
    return 0;

  for (i = 0; g->gr_mem[i]; i++)
    if (!strcmp(g->gr_mem[i], pw->pw_name))
      break;

  in = !!(g->gr_mem[i]);

  for (i = 0; i < GROUP_CACHE_SIZE; i++)
    if (!group_mem_cache[i].set) {
      group_mem_cache[i].set = 1;
      group_mem_cache[i].gid = gid;
      group_mem_cache[i].in = in;
    }

  return in;
}
#endif

static Scheme_Object *file_or_dir_permissions(int argc, Scheme_Object *argv[])
{
  Scheme_Object *l = scheme_null;
  char *filename;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
  int flags, isdir, exists;
  long type;
#endif

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("file-or-directory-permissions", "string", 0, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX	  
  filename = SCHEME_STR_VAL(argv[0]);
  if (has_null(filename, SCHEME_STRTAG_VAL(argv[0])))
	  raise_null_error("file-or-directory-permissions", argv[0], "");
#else
  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "file-or-directory-permissions",
				    NULL);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  if (!find_mac_file(filename, &spec, 0, 0, NULL, &isdir, &exists, NULL, &flags, &type, NULL, NULL)
      || !exists)
    return scheme_null;

  l = scheme_make_pair(read_symbol, l);

  if (type == 'APPL')
    l = scheme_make_pair(execute_symbol, l);

  if (!(flags & 0x1))
    l = scheme_make_pair(write_symbol, l);

  return l;
#else
#ifdef NO_STAT_PROC
  return scheme_null;
#else
#ifdef UNIX_FILE_SYSTEM
  {
    struct stat buf;
    int read, write, execute;

    if (stat(filename, &buf))
      return scheme_null;

    if (buf.st_uid == getuid()) {
      read = !!(buf.st_mode & S_IRUSR);
      write = !!(buf.st_mode & S_IWUSR);
      execute = !!(buf.st_mode & S_IXUSR);
    } else if (user_in_group(buf.st_gid)) {
      read = !!(buf.st_mode & S_IRGRP);
      write = !!(buf.st_mode & S_IWGRP);
      execute = !!(buf.st_mode & S_IXGRP);
    } else {
      read = !!(buf.st_mode & S_IROTH);
      write = !!(buf.st_mode & S_IWOTH);
      execute = !!(buf.st_mode & S_IXOTH);
    }
    
    if (read)
      l = scheme_make_pair(read_symbol, l);
    if (write)
      l = scheme_make_pair(write_symbol, l);
    if (execute)
      l = scheme_make_pair(execute_symbol, l);
  }
#endif  
#ifdef DOS_FILE_SYSTEM
  {
    int len = strlen(filename);

    if ((len > 1) && IS_A_SEP(filename[0]) && check_dos_slashslash_drive(filename, len, NULL, 1)) {
      /* stat doesn't work with UNC "drive" names */
      int flags;
      if (UNC_dir_exists(filename, len, &flags)) {
	l = scheme_make_pair(execute_symbol, scheme_make_pair(read_symbol, l));
	if (!(flags & _A_RDONLY))
	  l = scheme_make_pair(write_symbol, l);
      } else
	return scheme_null;
    } else {
      struct MSC_IZE(stat) buf;
      
      if (MSC_IZE(stat)(filename, &buf))
	return scheme_null;
      
      if (buf.st_mode & MSC_IZE(S_IREAD))
	l = scheme_make_pair(execute_symbol,
			     scheme_make_pair(read_symbol, l));
      if (buf.st_mode & MSC_IZE(S_IWRITE))
	l = scheme_make_pair(write_symbol, l);
    }
  }
#endif
#endif
#endif

  return l;
}

static Scheme_Object *file_size(int argc, Scheme_Object *argv[])
{
  char *filename;
#ifdef USE_MAC_FILE_TOOLBOX
  FSSpec spec;
#endif
  unsigned long len = 0;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("file-size", "string", 0, argc, argv);

#ifdef USE_MAC_FILE_TOOLBOX	  
  filename = SCHEME_STR_VAL(argv[0]);
  if (has_null(filename, SCHEME_STRTAG_VAL(argv[0])))
	  raise_null_error("file-size", argv[0], "");
#else
  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "file-size",
				    NULL);
#endif

#ifdef USE_MAC_FILE_TOOLBOX
  if (!find_mac_file(filename, &spec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, &len, NULL))
    return scheme_false;
#endif
#if defined(UNIX_FILE_SYSTEM) || defined(DOS_FILE_SYSTEM)
  {
    struct MSC_IZE(stat) buf;

    if (MSC_IZE(stat)(filename, &buf) || S_ISDIR(buf.st_mode))
      return scheme_false;

    len = buf.st_size;
  }
#endif

  return scheme_make_integer_value_from_unsigned(len);
}

#endif

#ifdef DIR_FUNCTION
static Scheme_Object *current_directory(int argc, Scheme_Object **args)
{
  if (argc) {
    char *expanded;

    if (!SCHEME_STRINGP(args[0]))
      scheme_wrong_type("current-directory", "string", 0, argc, args);

    expanded = scheme_expand_filename(SCHEME_STR_VAL(args[0]),
				      SCHEME_STRTAG_VAL(args[0]),
				      "current-directory",
				      NULL);

    scheme_setcwd(expanded, 0);

    return scheme_void;
  } else
    return scheme_make_string_without_copying(scheme_getcwd(NULL, 0, NULL, 0));
}

#ifdef USE_FCHDIR
static const char *wd_via_fchdir = "fchdir";
static int using_fchdir = 0;
/* File descriptors are a limited resource, so set a max number to be
   used for tracking thread directories: */
#define MAX_PROCESSES_USE_FCHDIR 8
#endif

void scheme_set_process_directory(Scheme_Process *newp, Scheme_Process *oldp)
{
#ifdef USE_MAC_FILE_TOOLBOX
  WDPBRec  wdrec;
  char buf[256];

  if (oldp->running > 0) {
    wdrec.ioNamePtr = (StringPtr)buf;
    if (PBHGetVol(&wdrec, 0))
      oldp->wd_inited = 0;
    else {
      oldp->vrefnum = wdrec.ioWDVRefNum;
      oldp->dirid = wdrec.ioWDDirID;
      oldp->wd_inited = 1;
    }
  }

  if (newp->wd_inited) {
    wdrec.ioNamePtr = NULL;
    wdrec.ioVRefNum = newp->vrefnum;
    wdrec.ioWDDirID = newp->dirid;

    PBHSetVol(&wdrec, 0);
  }
#else
  int len, old_set = 0;

  if (oldp->running > 0) {
#ifdef USE_FCHDIR
    int used_fchdir;
    
    if (using_fchdir < MAX_PROCESSES_USE_FCHDIR) {
      oldp->wd_len = open(".", O_RDONLY, 0);
      if (oldp->wd_len != -1) {
	/* printf("open dir: #%d %d\n", using_fchdir, oldp->wd_len); */
	oldp->working_directory = (char *)wd_via_fchdir;
	using_fchdir++;
	used_fchdir = 1;
      } else
	used_fchdir = 0;
    } else
      used_fchdir = 0;
    
    if (!used_fchdir) {
#endif
      old_set = 1;
      
      if (!oldp->working_directory) {
	oldp->wd_len = GETCWD_BUFSIZE;
	oldp->working_directory = (char *)scheme_malloc_atomic(oldp->wd_len);
      }
      
      oldp->working_directory = scheme_getcwd(oldp->working_directory, oldp->wd_len, &len, 1);
      if (len > oldp->wd_len)
	oldp->wd_len = len;
#ifdef USE_FCHDIR
    }
#endif
  }

#ifdef USE_FCHDIR
  if (newp->working_directory == wd_via_fchdir) {
    fchdir(newp->wd_len);
    close(newp->wd_len);
    --using_fchdir;
    /* printf("close dir: #%d %d\n", using_fchdir, newp->wd_len); */
    newp->working_directory = NULL;
  } else {
#endif
    if (newp->working_directory
	&& (!old_set
	    || strcmp(oldp->working_directory, newp->working_directory)))
      MSC_IZE(chdir)(newp->working_directory);
#ifdef USE_FCHDIR
  }
#endif
#endif
}

char *scheme_get_thread_current_directory(Scheme_Process *p, int *len, int noexn)
{
  if (p == scheme_current_process)
    return scheme_getcwd(NULL, 0, len, noexn);
  
#ifdef USE_MAC_FILE_TOOLBOX
  {
    FSSpec spec;
    char *s;

    spec.vRefNum = p->vrefnum;
    spec.parID = p->dirid;

    s = scheme_build_mac_filename(&spec, 1);
    *len = strlen(s);

    return s;
  }
#else
# ifdef USE_FCHDIR
  if (p->working_directory == wd_via_fchdir) {
    char *savedir = scheme_getcwd(NULL, 0, NULL, 1);
    char *result;

    fchdir(p->wd_len);
    result = scheme_getcwd(NULL, 0, len, noexn);
    scheme_setcwd(savedir, noexn);

    return result;
  }
# endif
  *len = strlen(p->working_directory);
  return scheme_strdup(p->working_directory);
#endif  
}

void scheme_done_process_directory(Scheme_Process *p)
{
#ifdef USE_FCHDIR
  if (p->working_directory == wd_via_fchdir) {
    close(p->wd_len);
    --using_fchdir;
    /* printf("close dir: #%d %d\n", using_fchdir, p->wd_len); */
    p->working_directory = NULL;
  }
#endif
}
#endif

#ifndef NO_FILE_SYSTEM_UTILS

static Scheme_Object *collpaths_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *v = argv[0];

  if (scheme_proper_list_length(v) < 0)
    return scheme_false;

  while (SCHEME_PAIRP(v)) {
    if (!SCHEME_STRINGP(SCHEME_CAR(v)))
      return scheme_false;
    v = SCHEME_CDR(v);
  }

  return SCHEME_NULLP(v) ? scheme_true : scheme_false;
}

static Scheme_Object *current_library_collection_paths(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-library-collection-paths", MZCONFIG_COLLECTION_PATHS,
			     argc, argv,
			     -1, collpaths_p, "list of strings", 0);
}

#endif

#ifdef MACINTOSH_EVENTS

static long check_four(char *name, int which, int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[which];

  if (!SCHEME_STRINGP(o))
    scheme_wrong_type(name, "MacOS type/creator 4-character string", which, argc, argv);

  if (SCHEME_STRTAG_VAL(o) != 4)
    scheme_raise_exn(MZEXN_APPLICATION_MACOS_TYPESTRING,
		     o,
		     "%s: string is not a 4-character type or creator signature: %s%s",
		     name,
		     scheme_make_provided_string(o, 1, NULL),
		     scheme_make_args_string("other ", which, argc, argv));
  
  return *(long *)SCHEME_STR_VAL(o);
}

static int appl_name_to_spec(char *name, int find_path, Scheme_Object *o, FSSpec *spec)
{
  if (find_path) {
    DTPBRec rec;
    Str255 nm;
    short vrefnum;
    long creator = check_four(name, 0, 1, &o);

    if (GetVol(nm, &vrefnum))
      return 0;
    rec.ioNamePtr = NULL;
    rec.ioVRefNum = vrefnum;

    if (PBDTGetPath(&rec))
      return 0;

    rec.ioIndex = 0;
    rec.ioNamePtr = nm;
    rec.ioFileCreator = creator;

    if (PBDTGetAPPL(&rec, 0))
      return 0;
      
    memcpy(spec->name, nm, 32);
    spec->vRefNum = vrefnum;
    spec->parID = rec.ioAPPLParID;
  } else {
    char *s;
    
    if (!SCHEME_STRINGP(o))
      scheme_wrong_type(name, "string", 0, 1, &o);
  
    s = scheme_expand_filename(SCHEME_STR_VAL(o),
			       SCHEME_STRTAG_VAL(o),
			       name,
			       NULL);

    if (!find_mac_file(s, spec, 0, 1, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL))
      return 0;
  }
  
  return 1;
}
  
int scheme_mac_start_app(char *name, int find_path, Scheme_Object *o)
{
  FSSpec spec;
  LaunchParamBlockRec rec;

  if (!appl_name_to_spec(name, find_path, o, &spec))
    return 0;

  rec.launchBlockID = extendedBlock;
  rec.launchEPBLength = extendedBlockLen;
  rec.launchFileFlags = 0;
  rec.launchControlFlags = launchContinue | launchNoFileFlags;
  rec.launchAppSpec = &spec;
  rec.launchAppParameters = NULL;

  return !LaunchApplication(&rec);
}

#ifndef FALSE
# define FALSE 0
# define TRUE 1
#endif

static int ae_marshall(AEDescList *ae, AEDescList *list_in, AEKeyword kw, Scheme_Object *v, 
		       char *name, OSErr *err, char **stage)
{
  DescType type;
  Ptr data;
  Size size;
  Boolean x_b;
  long x_i;
  double x_d;
  FSSpec x_fss;
  Handle alias = NULL;
  int retval = 1;
    
  switch (SCHEME_TYPE(v)) {
  case scheme_true_type:
  case scheme_false_type:
    x_b = SCHEME_TRUEP(v) ? TRUE : FALSE;
    type = typeBoolean;
    data = (char *)&x_b;
    size = sizeof(Boolean);
    break;
  case scheme_integer_type:
    x_i = SCHEME_INT_VAL(v);
    type = typeLongInteger;
    data = (char *)&x_i;
    size = sizeof(long);
    break;
  case scheme_string_type:
    type = typeChar;
    data = SCHEME_STR_VAL(v);
    size = SCHEME_STRTAG_VAL(v);
    break;
  case scheme_float_type:
  case scheme_double_type:
    x_d = SCHEME_FLOAT_VAL(v);
    type = typeFloat;
    data = (char *)&x_d;
    size = sizeof(double);
    break;
  case scheme_vector_type: /* vector => record */
    if ((SCHEME_VEC_SIZE(v) >= 1)
	&& ((SCHEME_VEC_ELS(v)[0] == record_symbol)
	    || (SCHEME_VEC_ELS(v)[0] == file_symbol))) {
      if (SCHEME_VEC_ELS(v)[0] == file_symbol) {
	if ((SCHEME_VEC_SIZE(v) == 2)
	    && SCHEME_STRINGP(SCHEME_VEC_ELS(v)[1]))  {
	  char *s = SCHEME_STR_VAL(SCHEME_VEC_ELS(v)[1]);
	  long l = SCHEME_STRTAG_VAL(SCHEME_VEC_ELS(v)[1]);
	  if (!has_null(s, l)) {
	    if (scheme_mac_path_to_spec(s, &x_fss, NULL)) {
	      *err = NewAliasMinimal(&x_fss, (AliasHandle *)&alias);
	      if (*err == -43) {
	        /* Can't make alias; make FSSpec, instead */
	        type = typeFSS;
	        data = (char *)&x_fss;
	        size = sizeof(FSSpec);
	        break;
	      } else if (*err) {
		*stage = "converting file to alias: ";
		return NULL;
	      }
	      type = typeAlias;
	      HLock(alias);
	      data = (char *)*alias;
	      size = GetHandleSize(alias);
	      break;
	    }
	  }
	}
	scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
			 "%s: cannot interpret vector as a file specification: %s",
			 name,
			 scheme_make_provided_string(v, 1, NULL));
      }
      /* record case falls through to list */
    } else {
      scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		       "%s: cannot convert ill-tagged or untagged vector: %s",
		       name,
		       scheme_make_provided_string(v, 1, NULL));
    }
  case scheme_pair_type: /* /\ falls through */
  case scheme_null_type:
    {
      int l;
      int isrec = SCHEME_VECTORP(v);
        
      if (isrec)
	v = SCHEME_CDR(scheme_vector_to_list(v));
        
      l = scheme_proper_list_length(v);
      if (l >= 0) {
	AEDescList *list;
	list = MALLOC_ONE_ATOMIC(AEDescList);
          
	*err = AECreateList(NULL, 0, isrec, list);
	if (*err) {
	  *stage = "cannot create list/record: ";
	  return 0;
	}
		  
	while (!SCHEME_NULLP(v)) {
	  Scheme_Object *a = SCHEME_CAR(v);
	  AEKeyword rkw;
	  if (isrec) {
	    Scheme_Object *k;
	    if (!SCHEME_PAIRP(a)
		|| !SCHEME_PAIRP(SCHEME_CDR(a))
		|| !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(a)))
		|| !SCHEME_STRINGP(SCHEME_CAR(a))) {
	      /* Bad record form. */
	      scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
			       "%s: cannot interpret vector part as a record field: %s",
			       name,
			       scheme_make_provided_string(a, 1, NULL));
	    }
	    k = SCHEME_CAR(a);
	    a = SCHEME_CADR(a);
	    rkw = check_four(name, 0, 1, &k);
	  } else
	    rkw = 0;
	  if (!ae_marshall(NULL, list, rkw, a, name, err, stage)) {
	    AEDisposeDesc(list);
	    return 0;
	  }
	  v = SCHEME_CDR(v);
	}
		  
	if (list_in) {
	  if (kw)
	    *err = AEPutKeyDesc(list_in, kw, list);
	  else
	    *err = AEPutDesc(list_in, 0, list);
	  if (*err) {
	    *stage = "cannot add list item: ";
	    AEDisposeDesc(list);
	    return 0;
	  }
	} else {
	  if (kw)
	    *err = AEPutParamDesc(ae, keyDirectObject, list);
	  else
	    *err = AEPutParamDesc(ae, kw, list);
	  if (*err) {
	    *stage = "cannot install argument: ";
	    AEDisposeDesc(list);
	    return 0;
	  }
	}
		
	AEDisposeDesc(list);
		  
	return 1;
      }
    }
  default:
    /* Don't know how to marshall */
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "%s: cannot convert value for sending: %s",
		     name,
		     scheme_make_provided_string(v, 1, NULL));
    return 0;
  }
    
  if (list_in) {
    if (kw)
      *err = AEPutKeyPtr(list_in, kw, type, data, size);
    else
      *err = AEPutPtr(list_in, 0, type, data, size);
    if (alias)
      DisposeHandle(alias);
    if (*err) {
      *stage = "cannot add list item: ";
      retval = 0;
    }
  } else {
    if (kw)
      *err = AEPutParamPtr(ae, kw, type, data, size);
    else
      *err = AEPutParamPtr(ae, keyDirectObject, type, data, size);
    if (*err) {
      *stage = "cannot install argument: ";
      retval = 0;
    }
  }

  if (alias)
    DisposeHandle(alias);
	
  return retval;
}

static Scheme_Object *ae_unmarshall(AppleEvent *reply, AEDescList *list_in, int pos,
                                    OSErr *err, char **stage, Scheme_Object **record)
{

  DescType rtype;
  long ret, sz;
  AEKeyword kw;
  Scheme_Object *result;

  if (list_in) {
    if (AEGetNthPtr(list_in, pos, typeWildCard, &kw, &rtype, NULL, 0, &sz))
      return scheme_void;
  } else {
    if (AEGetParamPtr(reply, keyDirectObject, typeWildCard, &rtype, NULL, 0, &sz))
      return scheme_void;
  }
  
  {
    Boolean x_b;
    long x_i;
    double x_d;
    char *x_s;
    FSSpec x_f;
    Ptr data;
    
    switch (rtype) {
    case typeBoolean:
      data = (char *)&x_b;
      break;
    case typeLongInteger:
    case typeShortInteger:
      rtype = typeLongInteger;
      data = (char *)&x_i;
      sz = sizeof(long);
      break;
    case typeLongFloat:
    case typeShortFloat:
    case typeExtended:
      rtype = typeFloat;
      data = (char *)&x_d;
      sz = sizeof(double);
      break;
    case typeChar:
      data = x_s = scheme_malloc_atomic(sz + 1);
      x_s[0] = 0;
      break;
    case typeAlias:
    case typeFSS:
      rtype = typeFSS;
      data = (char *)&x_f;
      sz = sizeof(FSSpec);
      break;
    case typeAEList:
    case typeAERecord:
      {
	AEDescList *list;
	Scheme_Object *first = scheme_null, *last = NULL, *v, *rec, **recp;
	int i;
         
	list = MALLOC_ONE_ATOMIC(AEDescList);
          
	if (list_in) {
	  if (AEGetNthDesc(list_in, pos, rtype, &kw, list))
	    return NULL;
	  if (record)
	    *record = scheme_make_sized_string((char *)&kw, sizeof(long), 1);
	} else {
	  if (AEGetParamDesc(reply, keyDirectObject, rtype, list))
	    return NULL;
	}
         
	if (rtype == typeAERecord)
	  recp = &rec;
	else
	  recp = NULL;
         
	for (i = 1; v = ae_unmarshall(NULL, list, i, err, stage, recp); i++) {
	  if (v == scheme_void)
	    break;
	  else if (!v) {
	    AEDisposeDesc(list);
	    return NULL;
	  } else {
	    Scheme_Object *pr = scheme_make_pair(v, scheme_null);
	    if (recp) {
	      pr = scheme_make_pair(rec, pr);
	      pr = scheme_make_pair(pr, scheme_null);
	    }
	           
	    if (last)
	      SCHEME_CDR(last) = pr;
	    else
	      first = pr;
	    last = pr;
	  }
	}
         
	if (recp)
	  first = scheme_list_to_vector(scheme_make_pair(record_symbol, first));
         
	AEDisposeDesc(list);
	return first;
      }
    default:
      /* Don't know how to un-marshall */
      *err = -1;
      *stage = "error translating the reply to a Scheme value: ";
      return NULL;
    }
    
    if (list_in) {
      *err = AEGetNthPtr(list_in, pos, rtype, &kw, &rtype, data, sz, &sz);
      if (record)
        *record = scheme_make_sized_string((char *)&kw, sizeof(long), 1);
      if (*err) {
        *stage = "lost a list value: ";
        return NULL;
      }
    } else {
      *err = AEGetParamPtr(reply, keyDirectObject, rtype, &rtype, data, sz, &sz);
      if (*err) {
        *stage = "lost the return value: ";
        return NULL;
      }
    }
    
    switch (rtype) {
    case typeBoolean:
      result = (x_b ? scheme_true : scheme_false);
      break;
    case typeLongInteger:
      result = scheme_make_integer(x_i);
      break;
    case typeFloat:
      result = scheme_make_double(x_d);
      break;
    case typeChar:
      result = scheme_make_sized_string(x_s, sz, 0);
      break;
    case typeFSS:
      result = scheme_make_sized_string(scheme_build_mac_filename(&x_f, 0), -1, 0);
      break;      
    }
  }
  
  return result;
}

/* Single-threaded ok: */
static mz_jmp_buf escape_while_waiting;
static int escaped = 0;

static int handlerInstalled = 0;
typedef struct ReplyItem {
  long id;
  AppleEvent ae;
  struct ReplyItem *next;
} ReplyItem;
static ReplyItem *reply_queue;

static pascal Boolean while_waiting(EventRecord *e, long *sleeptime, RgnHandle *rgn)
{
   mz_jmp_buf save;

	if (escaped) return TRUE;

   if (scheme_handle_aewait_event)
     scheme_handle_aewait_event(e);
   
   memcpy(&save, &scheme_error_buf, sizeof(mz_jmp_buf));
   if (scheme_setjmp(scheme_error_buf)) {
     memcpy(&escape_while_waiting, &save, sizeof(mz_jmp_buf));
     escaped = 1;
     return TRUE; /* Immediately return to AESend */
   } else {
     scheme_process_block(0);
     memcpy(&scheme_error_buf, &save, sizeof(mz_jmp_buf));
   }
   
   return FALSE;
}

static pascal OSErr HandleAnswer(AppleEvent *evt, AppleEvent *rae, long k)
{
  ReplyItem *r = MALLOC_ONE(ReplyItem);
  DescType rtype;
  long id, sz;
  
  AEGetAttributePtr(evt, keyReturnIDAttr, typeLongInteger, &rtype, &r->id, sizeof(long), &sz);
  
  AEDuplicateDesc(evt, &r->ae);

  r->next = reply_queue;
  reply_queue = r;
  
  return 0;
}

static void wait_for_reply(AppleEvent *ae, AppleEvent *reply)
{
  EventRecord e;
  DescType rtype;
  long id, sz;
  ReplyItem *r, *prev;
  
  if (!handlerInstalled) {
    handlerInstalled = TRUE;
    AEInstallEventHandler(kCoreEventClass, kAEAnswer, NewAEEventHandlerProc(HandleAnswer), 0, 0);
    REGISTER_SO(reply_queue);
  }
  
  AEGetAttributePtr(ae, keyReturnIDAttr, typeLongInteger, &rtype, &id, sizeof(long), &sz);
  
  while (1) {
    WaitNextEvent(highLevelEventMask, &e, 60, 0L);
    if (e.what == kHighLevelEvent)
      AEProcessAppleEvent(&e);
    else {
	  if (while_waiting(&e, NULL, NULL))
	    break;
	}
	
	prev = NULL;
	for (r = reply_queue; r; r = r->next) {
	  if (r->id == id) {
	    /* Got the reply */
	    memcpy(reply, &r->ae, sizeof(AppleEvent));
	    if (prev)
	      prev->next = r->next;
	    else
	      reply_queue = r->next;
	    return;
	  }
	  prev = r;
	}
  }
}

int scheme_mac_send_event(char *name, int argc, Scheme_Object **argv, 
			  Scheme_Object **result, OSErr *err, char **stage)
{
  AEEventClass classid;
  AEEventID eventid;
  AppleEvent *ae = NULL, *reply = NULL;
  AEAddressDesc *target = NULL;
  DescType rtype;
  int i, retval;
  long ret, sz, dst;


  dst = check_four(name, 0, argc, argv);
  classid = check_four(name, 1, argc, argv);
  eventid = check_four(name, 2, argc, argv);

  target = MALLOC_ONE_ATOMIC(AEAddressDesc);
  *err = AECreateDesc(typeApplSignature, &dst, sizeof(long), target);
  if (*err) {
    *stage = "application not found: ";
    goto fail;
  }
    
  ae = MALLOC_ONE_ATOMIC(AppleEvent);
  *err = AECreateAppleEvent(classid, eventid, target, kAutoGenerateReturnID, 
                            kAnyTransactionID, ae);
  if (*err) {
    *stage = "cannot create event: ";
    ae = NULL;    
    goto fail;
  }
  
  if ((argc > 3) && !SCHEME_VOIDP(argv[3])) {
    if (!ae_marshall(ae, NULL, 0, argv[3], name, err, stage))
      goto fail;
  }
  
  if (argc > 4) {
    Scheme_Object *l = argv[4];
    char *expected = "list of pairs containing a type-string and a value";
    while (SCHEME_PAIRP(l)) {
      Scheme_Object *a = SCHEME_CAR(l), *k, *v;
      AEKeyword kw;
      /* Must be a list of 2-item lists: keyword and value */
      if (!SCHEME_PAIRP(a) 
          || !SCHEME_PAIRP(SCHEME_CDR(a))
          || !SCHEME_NULLP(SCHEME_CDR(SCHEME_CDR(a)))
          || !SCHEME_STRINGP(SCHEME_CAR(a)))
        break; /* => type error */
      k = SCHEME_CAR(a);
      v = SCHEME_CADR(a);
      kw = check_four(name, 0, 1, &k);
      if (!ae_marshall(ae, NULL, kw, v, name, err, stage))
        goto fail;
      l = SCHEME_CDR(l);
    }
    if (!SCHEME_NULLP(l))
      scheme_wrong_type(name, expected, 4, argc, argv);
  }
  
  reply = MALLOC_ONE_ATOMIC(AppleEvent);
  *err = AESend(ae, reply, kAEQueueReply | kAECanInteract, kAENormalPriority, kNoTimeOut, NULL, NULL);
  if (*err) {
    *stage = "send failed: ";
    reply = NULL;
    goto fail;
  }
  wait_for_reply(ae, reply);
  if (escaped) {
     reply = NULL;
     escaped = 0;
     memcpy(&scheme_error_buf, &escape_while_waiting, sizeof(mz_jmp_buf));
     memset(&escape_while_waiting, 0, sizeof(mz_jmp_buf));
     goto escape;
  }
  
  if (!AEGetParamPtr(reply, keyErrorString, typeChar, &rtype, NULL, 0, &sz) && sz) {
    *err = -1;
    if (sz > 256) sz = 256;
    *stage = scheme_malloc_atomic(sz + 1);
    *stage[sz] = 0;
    AEGetParamPtr(reply, keyErrorString, typeChar, &rtype, *stage, sz, &sz);
    goto fail;
  }
  if (!AEGetParamPtr(reply, keyErrorNumber, typeLongInteger, &rtype, &ret, sizeof(long), &sz)
      && ret) {
    *err = ret;
    
    *stage = "application replied with error: ";
    goto fail;
  }
  
  *result = ae_unmarshall(reply, NULL, 0, err, stage, NULL);
  if (!*result)
    goto fail;
  
succeed:
  retval = 1;
  goto done;
escape:
  retval = -1;
  goto done;
fail:
  retval = 0;
   
done:
  if (ae) AEDisposeDesc(ae);
  if (reply) AEDisposeDesc(reply);
  if (target) AEDisposeDesc(target);
  
  if (retval < 0) {
    scheme_longjmp(scheme_error_buf, 1);
  }
  
  return retval;
}
#endif
