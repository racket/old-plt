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

  Thanks to Patrick Barta for the WINDOWS_DYNAMIC_LOAD code.
  Thanks to William Ng for the CODEFRAGMENT_DYNAMIC_LOAD code.
*/

/* Some copilers don't like re-def of GC_malloc in schemef.h: */
#define SCHEME_NO_GC_PROTO

#include "schpriv.h"
#include "schvers.h"
#include "schgc.h"

#ifdef UNIX_DYNAMIC_LOAD
# include <dlfcn.h>
#endif
#ifdef AIX_DYNAMIC_LOAD
# include  "../aixdlfcn/dlfcn.h"
# define UNIX_DYNAMIC_LOAD
#endif
#if defined(WINDOWS_DYNAMIC_LOAD)
# include <windows.h>
#endif
#if defined(CODEFRAGMENT_DYNAMIC_LOAD)
# include <CodeFragments.h>
static Boolean get_ext_file_spec(FSSpec *spec, const char *filename );
static Boolean load_ext_file_spec(FSSpec *spec, CFragConnectionID *connID);
#endif

#ifdef SHL_DYNAMIC_LOAD
#include <dl.h>
#include <errno.h>
#define dlopen(file, flag) ((void *)shl_load(file, BIND_IMMEDIATE, 0L))
void *dlsym(void *_handle, const char *name)
{
  void *result;
  shl_t handle = (shl_t)_handle;

  if (!shl_findsym(&handle, name, TYPE_PROCEDURE, (void *)&result))
    return result;
  else
    return NULL;
}
static char *dlerror(void) {
  static char errbuf[20];
  sprintf(errbuf, "%d", errno);
  return errbuf;
}
#define UNIX_DYNAMIC_LOAD
#endif

#ifdef LINK_EXTENSIONS_BY_TABLE
#undef SCHEME_NO_GC_PROTO
#include "schemex.h"
#endif

static Scheme_Object *load_extension(int argc, Scheme_Object **argv);
static Scheme_Object *current_load_extension(int argc, Scheme_Object *argv[]);
static Scheme_Object *default_load_extension(int argc, Scheme_Object *argv[]);

#ifdef LINK_EXTENSIONS_BY_TABLE
Scheme_Extension_Table *scheme_extension_table;

#define SSI_ARG_TYPES Scheme_Extension_Table *
#define SSI_ARGS scheme_extension_table
#else
#define SSI_ARG_TYPES
#define SSI_ARGS
#endif

#ifndef UNIX_DYNAMIC_LOAD
#ifndef WINDOWS_DYNAMIC_LOAD
#ifndef CODEFRAGMENT_DYNAMIC_LOAD
#define NO_DYNAMIC_LOAD
#endif
#endif
#endif

#ifndef NO_DYNAMIC_LOAD
static Scheme_Hash_Table *loaded_extensions; /* hash on scheme_initialize pointer */
static Scheme_Hash_Table *fullpath_loaded_extensions; /* hash on full path name */
#endif

void scheme_init_dynamic_extension(Scheme_Env *env)
{
  if (scheme_starting_up) {
#ifndef NO_DYNAMIC_LOAD
    REGISTER_SO(loaded_extensions);
    REGISTER_SO(fullpath_loaded_extensions);
    loaded_extensions = scheme_hash_table(0, SCHEME_hash_ptr, 0, 0);
    fullpath_loaded_extensions = scheme_hash_table(0, SCHEME_hash_string, 0, 0);
#endif

#ifdef LINK_EXTENSIONS_BY_TABLE
    REGISTER_SO(scheme_extension_table);
    
    scheme_extension_table = 
      (Scheme_Extension_Table *)scheme_malloc_atomic(sizeof(Scheme_Extension_Table));
#include "schemex.inc"
#endif

    scheme_set_param(scheme_config, MZCONFIG_LOAD_EXTENSION_HANDLER,
		     scheme_make_prim_w_arity2(default_load_extension,
					       "default-load-extension-handler",
					       1, 1,
					       0, -1));
  }

  scheme_add_global_constant("load-extension", 
			     scheme_make_prim_w_arity2(load_extension, 
						       "load-extension",
						       1, 1,
						       0, -1), 
			     env);

  scheme_add_global_constant("current-load-extension", 
			     scheme_register_parameter(current_load_extension, 
						       "current-load-extension",
						       MZCONFIG_LOAD_EXTENSION_HANDLER), 
			     env);
}

static Scheme_Object *
current_load_extension(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load-extension", MZCONFIG_LOAD_EXTENSION_HANDLER,
			     argc, argv,
			     1, NULL, NULL, 0);
}

#ifndef NO_DYNAMIC_LOAD

typedef struct {
  void *handle;
  Scheme_Object *(*init_f)(Scheme_Env *);
  Scheme_Object *(*reload_f)(Scheme_Env *);
} ExtensionData;

#endif

static Scheme_Object *do_load_extension(const char *filename, Scheme_Env *env)
{
#ifndef NO_DYNAMIC_LOAD
  Scheme_Object *(*init_f)(Scheme_Env *); /* set by platform-specific code */
  Scheme_Object *(*reload_f)(Scheme_Env *); /* set by platform-specific code */
  ExtensionData *ed;
  void *handle;
  int comppath;

  comppath = scheme_is_complete_path(filename, strlen(filename));

  reload_f = NULL;
  handle = NULL;

  if (comppath)
    init_f = (Scheme_Object *(*)(Scheme_Env *))scheme_lookup_in_table(fullpath_loaded_extensions, filename);
  else
    init_f = NULL;

  if (!init_f) {
#endif

#ifdef UNIX_DYNAMIC_LOAD
    void *dl;
    char *(*f)(SSI_ARG_TYPES), *vers;
    
    /* Make sure that filename is not a pathless filename.
       Some Unix systems don't search as a relative path
       otherwise. */
    if (filename[0] != '/') {
      int l = strlen(filename);
      char *s = (char *)scheme_malloc_atomic(l + 3);
      s[0] = '.';
      s[1] = '/';
      memcpy(s + 2, filename, l + 1);
      filename = s;
    }
    
    dl = dlopen((char *)filename, 1);
    if (!dl)
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_OPEN,
		       scheme_make_string(filename),
		       "load-extension: couldn't open \"%s\" (%s)",
		       filename, dlerror());
    
    handle = dl;
    
#ifdef UNDERSCORE_DYNLOAD_SYMBOL_PREFIX
# define SO_SYMBOL_PREFIX "_"
#else
# define SO_SYMBOL_PREFIX
#endif

    f = (char*(*)(SSI_ARG_TYPES))dlsym(dl, SO_SYMBOL_PREFIX "scheme_initialize_internal");
    
    if (!f)
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_OPEN,
		       scheme_make_string(filename),
		       "load-extension: \"%s\" is not an extension (%s)", 
		       filename, dlerror());
    
    vers = f(SSI_ARGS);
    if (!vers || strcmp(vers, VERSION))
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_VERSION,
		       scheme_make_string(filename),
		       "load-extension: bad version %s from \"%s\"",
		       vers, filename);
    
    init_f = (Scheme_Object*(*)(Scheme_Env*))dlsym(dl, SO_SYMBOL_PREFIX "scheme_initialize");
    reload_f = (Scheme_Object*(*)(Scheme_Env*))dlsym(dl, SO_SYMBOL_PREFIX "scheme_reload");
    
    if (!init_f || !reload_f)
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_INITIALIZE,
		       scheme_make_string(filename),
		       "load-extension: no %s in \"%s\" (%s)",
		       init_f ? "scheme_reload" : "scheme_initialize",
		       filename, dlerror());
#endif
#if defined(WINDOWS_DYNAMIC_LOAD)
    HINSTANCE dl;
    char *(*f)(SSI_ARG_TYPES), *vers;
  
    dl = LoadLibrary(filename);
    if (!dl)
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_OPEN,
		       scheme_make_string(filename),
		       "load-extension: could not load \"%s\" (%ld)",
		       filename, GetLastError());
    
    handle = (void *)dl;
    
    f = (char*(*)(SSI_ARG_TYPES))GetProcAddress(dl, "scheme_initialize_internal");
    
    if (!f)
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_OPEN,
		       scheme_make_string(filename),
		       "load-extension: \"%s\" is not an extension (%ld)",
		       filename, GetLastError());
    
    vers = (f)(SSI_ARGS);
    if (!vers || strcmp(vers, VERSION))
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_VERSION,
		       scheme_make_string(filename),
		       "load-extension: bad version %s from \"%s\"",
		       vers, filename);
    
    init_f = (Scheme_Object*(*)(Scheme_Env*))GetProcAddress(dl,"scheme_initialize");
    reload_f = (Scheme_Object*(*)(Scheme_Env*))GetProcAddress(dl,"scheme_reload");
    
    if (!init_f || !reload_f)
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_INITIALIZE,
		       scheme_make_string(filename),
		       "load-extension: no %s in \"%s\"", 
		       init_f ? "scheme_reload" : "scheme_initialize",
		       filename);
#endif
#if defined(CODEFRAGMENT_DYNAMIC_LOAD)
    FSSpec spec;
    char *(*f)(), *vers;
    CFragConnectionID connID;
    
    if (get_ext_file_spec( &spec, filename ) && load_ext_file_spec( &spec, &connID ) )
      {
	OSErr err;
	handle = (void *)connID;
	
	err = FindSymbol( connID, "\pscheme_initialize_internal", ( Ptr * )&f, 0 );
	if ( err != noErr )
	  scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_OPEN,
			   scheme_make_string(filename),
			   "load-extension: \"%s\" is not an extension",
			   filename);
	
	vers = (f)(SSI_ARGS);
	
	if (!vers || strcmp(vers, VERSION))
	  scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_VERSION,
			   scheme_make_string(filename),
			   "load-extension: bad version %s from \"%s\"",
			   vers, filename);
	
	err = FindSymbol( connID, "\pscheme_initialize", ( Ptr * )&init_f, 0 );
	if ( err != noErr )
	  init_f = NULL;
	else {
	  err = FindSymbol( connID, "\pscheme_reload", ( Ptr * )&reload_f, 0 );
	  if ( err != noErr )
	    reload_f = NULL;
	}

	if ( err != noErr )
	  scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_INITIALIZE,
			   scheme_make_string(filename),
			   "load-extension: no %s in \"%s\"", 
			   init_f ? "scheme_reload" : "scheme_initialize",
			   filename);
	

      }
    else
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_OPEN,
		       scheme_make_string(filename),
		       "load-extension: could not load extension: \"%s\"",
		       filename);
#endif
#ifdef NO_DYNAMIC_LOAD
    scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		     "load-extension: not supported on this platform");
    return NULL;
#else

    if (comppath)
      scheme_add_to_table(fullpath_loaded_extensions, filename, (void *)init_f, 0);
  }
#endif

#ifndef NO_DYNAMIC_LOAD
  scheme_no_dumps("a dynamic extension has been loaded");

  ed = (ExtensionData *)scheme_lookup_in_table(loaded_extensions, (const char *)init_f);

  if (ed) {
    if (!ed->reload_f) {
      scheme_raise_exn(MZEXN_MISC_DYNAMIC_EXTENSION_OPEN,
		       scheme_make_string(filename),
		       "load-extension: the extension is already loaded: \"%s\"",
		       filename);
    } else
      init_f = ed->reload_f;
  } else {
    ed = MALLOC_ONE(ExtensionData);
    ed->handle = handle;
    ed->init_f = init_f;
    ed->reload_f = reload_f;
    scheme_add_to_table(loaded_extensions, (const char *)init_f, ed, 0);
  }

  return init_f(env);
#endif
}

void scheme_register_extension_global(void *ptr, long size)
{
  GC_add_roots((char *)ptr, (char *)(((char *)ptr) + size + 1));
}

static Scheme_Object *load_extension(int argc, Scheme_Object **argv)
{
  return scheme_load_with_clrd(argc, argv, "load-extension", MZCONFIG_LOAD_EXTENSION_HANDLER);
}

static Scheme_Object *default_load_extension(int argc, Scheme_Object **argv)
{
  char *filename;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("default-load-extension", "string", 0, argc, argv);
  
  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "default-load-extension",
				    NULL);

  return scheme_force_value(do_load_extension(filename, scheme_get_env(scheme_config)));
}

Scheme_Object *scheme_load_extension(const char *filename, Scheme_Env *env)
{
  Scheme_Object *a[1];

  a[0] = scheme_make_string(filename);
  return load_extension(1, a);
}

#if defined(CODEFRAGMENT_DYNAMIC_LOAD)

static Boolean get_ext_file_spec(FSSpec *spec, const char *filename)
{
#ifndef EXTENSIONS_WITHOUT_PATH
	return scheme_mac_path_to_spec(filename, spec, NULL);
#else
	/* William Ng's code for always finding an extension in a particular place. */
	/* This is a very Mac-like idea, but not MzScheme-like. */
    ProcessSerialNumber currentPSN;
    ProcessInfoRec info;
	Boolean ret = false;
    currentPSN.highLongOfPSN = 0;
    currentPSN.lowLongOfPSN = kCurrentProcess;
    info.processInfoLength = sizeof(ProcessInfoRec);
    info.processName = NULL;
    info.processAppSpec = spec;
    
	if ( GetProcessInformation(&currentPSN, &info)==noErr )
	{
#ifdef EXTENSION_IN_SEPARATE_FOLDER
		/* call PBGetCatInfoSync to get the folder par id */
		#define EXTENSION_FOLDER_NAME "\pextensions"
		HFileInfo file_info = {0};
		CInfoPBPtr	myCPBPtr;           /* for the PBGetCatInfo call */
		myCPBPtr = (CInfoPBRec*)&file_info;
	
		myCPBPtr->hFileInfo.ioNamePtr 	= EXTENSION_FOLDER_NAME;
		myCPBPtr->hFileInfo.ioVRefNum 	= spec->vRefNum;
		myCPBPtr->hFileInfo.ioFDirIndex	= 0;
		myCPBPtr->hFileInfo.ioDirID		= spec->parID;
		
		if (PBGetCatInfoSync(myCPBPtr) == noErr) 
		{
			if ((myCPBPtr->hFileInfo.ioFlAttrib & ioDirMask) != 0) 
			{   /* we have a directory */
				spec->parID   = myCPBPtr->hFileInfo.ioDirID;
				strcpy( ( char * )spec->name, filename );
				c2pstr( ( char * )spec->name );
				ret = true;
			}
		}
#else
		/* copy the extension filename to the FSSpec */
		strcpy( ( char * )spec->name, filename );
		c2pstr( ( char * )spec->name );
		ret = true;

#endif
	}
			
	return ret;
#endif
}

static Boolean load_ext_file_spec(FSSpec *spec, CFragConnectionID *connID)
{
	OSErr err = GetDiskFragment(spec, 0, 0, 0, kLoadNewCopy, connID, 0, 0);
	return err==noErr;
}

#endif

