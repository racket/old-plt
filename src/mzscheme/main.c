/*
  MzScheme
  Copyright (c) 1995-99 Matthew Flatt

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

#include "scheme.h"

/* #define STANDALONE_WITH_EMBEDDED_EXTENSION */
/*    STANDALONE_WITH_EMBEDDED_EXTENSION builds an executable with
      built-in extensions. The extension is initialized by calling
      scheme_initialize(env), where `env' is the initial environment.
      By default, command-line parsing, the REP, and initilization
      file loading are turned off. */

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
# define DONT_PARSE_COMMAND_LINE
# define DONT_RUN_REP
# define DONT_LOAD_INIT_FILE
#endif

#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifndef NO_USER_BREAK_HANDLER
# include <signal.h>
#endif
#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_LOCALE
# include <locale.h>
#endif
#ifdef MACINTOSH_EVENTS
# include <Events.h>
#endif
#ifdef MACINTOSH_SIOUX
# include <console.h>
# pragma far_data on
# include <SIOUX.h>
# pragma far_data reset
#endif
#ifdef MACINTOSH_SET_STACK
# include <Memory.h>
#endif
#ifdef MACINTOSH_EVENTS
# include "simpledrop.h"
#endif

#ifdef WIN32_THREADS
/* Only set up for Boehm GC that thinks it's a DLL: */
#include <windows.h>
# define GC_THINKS_ITS_A_DLL_BUT_ISNT
#endif
#ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
extern BOOL WINAPI DllMain(HINSTANCE inst, ULONG reason, LPVOID reserved);
#endif

/* #define NO_GCING */

#if defined(_IBMR2)
static void dangerdanger(int ignored)
{
  char *s = "mzscheme: Danger - paging space low\n";
  write(2, s, strlen(s));
  scheme_collect_garbage();
}
#endif

#ifndef NO_USER_BREAK_HANDLER

#ifdef MACINTOSH_EVENTS
static Scheme_Object *orig_evaluator;
#endif

#ifndef MACINTOSH_EVENTS
static void user_break_hit(int ignore)
{
  scheme_break_thread(NULL);

# ifdef SIGSET_NEEDS_REINSTALL
  MZ_SIGSET(SIGINT, user_break_hit);
# endif
}
#endif

#ifdef MACINTOSH_EVENTS

static int WeAreFront()
{
  static int inited;
  static ProcessSerialNumber us;
  ProcessSerialNumber front;
  Boolean r;
  
  if (!inited) {
    GetCurrentProcess(&us);
    inited = 1;
  }
  GetFrontProcess(&front);
  SameProcess(&us, &front, &r);
  
  return r;
}

static int is_break_event(EventRecord *e)
{
   if ((e->what == keyDown)
       && ((((e->message & charCodeMask) == '.') 
            && (e->modifiers & cmdKey))
           || (((e->message & charCodeMask) == 3)
	           && (e->modifiers & controlKey))))
	 return 1;
  else
    return 0;
}

static int check_break_flag()
{
#ifdef MACINTOSH_GIVE_TIME
  static long last_time;
  static int front = 1;

  if (TickCount() > last_time + (front ? 30 : 0)) {
      EventRecord e;
      front = WeAreFront();
      while (WaitNextEvent(everyEvent, &e, front ? 0 : 30, NULL)) {
        if (is_break_event(&e)) {
	       return 1;
	     }
# ifdef MACINTOSH_SIOUX
        SIOUXHandleOneEvent(&e);
# endif
       }
      last_time = TickCount();
    }
#endif
    return 0;
}

static void handle_one(EventRecord *e)
{
  if (is_break_event(e))
    scheme_break_thread(NULL);
  
# ifdef MACINTOSH_SIOUX
  SIOUXHandleOneEvent(e);
# endif
}

static void MacSleep(float secs, void *fds)
{
   EventRecord e;
   if (WaitNextEvent(everyEvent, &e, secs * 60, NULL)) {
     if (is_break_event(&e))
       scheme_break_thread(NULL);
     
     handle_one(&e);
   }
}

#endif

#endif

#ifdef MZ_STACK_START_HACK
void *mzscheme_stack_start;
#endif

#ifdef MACINTOSH_EVENTS
void Drop_Runtime(char **argv, int argc)
{
  int i;
	
  for (i = 0; i < argc; i++) {
    printf("(load \"%s\") ", argv[i]);
  }
  if (argc) printf("\n");
}

void Drop_Quit()
{
  ExitToShell();
}
#endif

#ifndef DONT_PARSE_COMMAND_LINE

#ifdef MACINTOSH_SIOUX
# pragma far_data on
static void SetSIOUX(void)
{
  SIOUXSettings.initializeTB = 0;
}
# pragma far_data off
#endif

#endif /* DONT_PARSE_COMMAND_LINE */

#ifndef DONT_LOAD_INIT_FILE
static char *get_init_filename(Scheme_Env *env)
{
  Scheme_Object *f = scheme_lookup_global(scheme_intern_symbol("find-system-path"), 
					  env);
  Scheme_Object *type = scheme_intern_symbol("init-file");
  Scheme_Object *path;
  
  if (f) {
    path = _scheme_apply(f, 1, &type);

    return SCHEME_STR_VAL(path);
  } else {
    return "no such file";
  }
}
#endif

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
extern Scheme_Object *scheme_initialize(Scheme_Env *env);
#endif

#ifdef EXPAND_FILENAME_TILDE
# define INIT_FILENAME "~/.mzschemerc"
#else
# ifdef DOS_FILE_SYSTEM
#  define INIT_FILENAME "%%HOMEDIRVE%%\\%%HOMEPATH%%\\mzschemerc.ss"
# else
#  define INIT_FILENAME "PREFERENCES:mzschemerc.ss"
# endif
#endif
#define GET_INIT_FILENAME get_init_filename
#define PRINTF printf
#define PROGRAM "MzScheme"
#define PROGRAM_LC "mzscheme"
#define BANNER scheme_banner()
#define MZSCHEME_CMD_LINE

#include "cmdline.inc"

#ifdef NO_GCING
extern int GC_free_space_divisor;
#endif

static void do_scheme_rep(void)
{
# ifndef NO_USER_BREAK_HANDLER
#  ifdef MACINTOSH_EVENTS
  scheme_set_param(scheme_config, MZCONFIG_ENABLE_BREAK, scheme_true);
  
  orig_evaluator = scheme_get_param(scheme_config, MZCONFIG_EVAL_HANDLER);
#  endif
# endif

  /* enter read-eval-print loop */
  scheme_rep();
  printf("\n");
}

static int cont_run(FinishArgs *f)
{
  return finish_cmd_line_run(f, do_scheme_rep);
}

int actual_main(int argc, char *argv[])
{
#ifdef NO_GCING
  GC_free_space_divisor = 1;
#endif

#ifdef MACINTOSH_SET_STACK
  long calcLimit;
  THz zone;
	
  zone = GetZone();
  calcLimit = ((long)LMGetCurStackBase()-(*(long *)zone)-sizeof(Zone))*3/4;
  if (calcLimit % 2)
    calcLimit++;
  SetApplLimit((Ptr)((*(long *)zone)+sizeof(Zone)+calcLimit));
#endif

#ifdef MACINTOSH_EVENTS
  MaxApplZone();
	
  InitGraf(&qd.thePort);		/* initialize Mac stuff */
  InitFonts();
  InitWindows();
  InitMenus();
  TEInit();
  InitDialogs(NULL);
  
  MoreMasters();
  MoreMasters();
	
#ifdef MACINTOSH_SIOUX
# pragma far_data on
  SIOUXSettings.initializeTB = 0;
# pragma far_data off
#endif

  scheme_handle_aewait_event = handle_one;

  scheme_sleep = MacSleep;

  Drop_GetArgs(&argc, &argv);
#endif

#ifdef MACINTOSH_SIOUX
  { 
    KeyMap keys;
    GetKeys(keys);
    if (keys[1] & 32768L) { /* Cmd key down */
	    int argc2;
	    char **argv2;
	    argc2 = ccommand(&argv2);
	    if (argc2 > 1) {
	      int i, j;
	      char **both = (char **)malloc(sizeof(char *) * (argc + argc2 - 1));
	      for (i = 0; i < argc; i++)
	        both[i] = argv[i];
	      for (j = 1; j < argc2; j++, i++)
	        both[i] = argv2[j];
	        
	      argv = both;
	      argc += argc2 - 1;
	    }
	}
  }
  
# pragma far_data on
  SIOUXSettings.autocloseonquit = 0;
  SIOUXSettings.asktosaveonclose = 0;
# pragma far_data off
#endif

#ifdef USE_LOCALE
  /* See note in string.c */
  setlocale( LC_ALL, "" );
#endif

#ifndef MACINTOSH_EVENTS
  MZ_SIGSET(SIGINT, user_break_hit);
#else
  scheme_check_for_break = check_break_flag;
#endif

  return run_from_cmd_line(argc, argv, scheme_basic_env, cont_run);
}

/**************** OSKIT stuff START **********************/
#if defined(OSKIT) && !defined(OSKIT_TEST)

# include <oskit/fs/bmodfs.h> 
# include <oskit/dev/clock.h> 
# include <oskit/c/sys/time.h> 
# include <oskit/x86/pc/dev.h>
# ifdef USE_OSKIT_CONSOLE
#  include <oskit/x86/pc/direct_cons.h> 
# else
#  include <oskit/dev/freebsd.h> 
# endif
void start_clock()
{
# define LOCAL_TO_GMT(t) /* (t)->tv_sec += secondswest */
  oskit_timespec_t time;
  /* use fdev's default clock device */
  oskit_clock_t *clock = oskit_clock_init();
  
  oskit_rtc_get(&time);   /* read rtc */
  LOCAL_TO_GMT(&time);            /* adjust for local time */
  oskit_clock_settime(clock, &time); /* set time */
  
  set_system_clock(clock);
}

# ifdef OSK_LINUX_FILESYSTEMS
#  include <oskit/dev/dev.h>
#  include <oskit/fs/filesystem.h> 
#  include <oskit/fs/dir.h> 
#  include <oskit/diskpart/diskpart.h> 
#  include <oskit/fs/linux.h> 
#  include <oskit/dev/linux.h> 
#  include <oskit/principal.h>
static oskit_principal_t *cur_principal;
int start_linux_fs(char *diskname, char *partname)
{
  int err;
  oskit_identity_t id;
  oskit_blkio_t *disk;
  oskit_blkio_t *part;
  oskit_filesystem_t *fs;
  oskit_dir_t *root;
# define MAX_PARTS 30
  diskpart_t part_array[MAX_PARTS];

# define CHECK(what, f) \
  if ((err = f)) { printf("filesystem init error at " what ": %d\n", err); return 0; }

  printf(">> Initializing devices\n");
  oskit_dev_init();
  oskit_linux_init_ide();
  oskit_linux_init_scsi();
  oskit_dev_probe();
  CHECK("fsinit", fs_linux_init());

  id.uid = 0;
  id.gid = 0;
  id.ngroups = 0;
  id.groups = 0;
  CHECK("makeprinciple", oskit_principal_create(&id, &cur_principal));

  printf(">> Opening disk\n");
  CHECK("diskopen", oskit_linux_block_open(diskname, OSKIT_DEV_OPEN_ALL, &disk));

  printf(">> Reading partitions\n");
  (void)diskpart_blkio_get_partition(disk, part_array, MAX_PARTS);
  if (diskpart_blkio_lookup_bsd_string(part_array, partname, disk, &part) == 0) {
    printf("can't find partition %s\n", partname);
    return 0;
  }

  printf(">> Mounting filesystem\n");
  CHECK("mount", fs_linux_mount(part, 0, &fs));
  CHECK("getroot", oskit_filesystem_getroot(fs, &root));

  fs_init(root);

  return 1;
}

oskit_error_t oskit_get_call_context(const struct oskit_guid *iid, void **out_if)
{
  if (memcmp(iid, &oskit_iunknown_iid, sizeof(*iid)) == 0 ||
      memcmp(iid, &oskit_principal_iid, sizeof(*iid)) == 0) {
    *out_if = cur_principal;
    oskit_principal_addref(cur_principal);
    return 0;
  }
  
  *out_if = 0;
  return OSKIT_E_NOINTERFACE;
}
# endif

#endif /* OSKIT */
/**************** OSKIT stuff END **********************/

int main(int argc, char **argv)
{
#ifdef USE_SENORA_GC
  void *mzscheme_stack_start;
#endif
#if defined(MZ_STACK_START_HACK) || defined(USE_SENORA_GC)
  long start2;

  mzscheme_stack_start = (void *)&start2;
#endif

#ifdef USE_SENORA_GC
  GC_set_stack_base(mzscheme_stack_start);
#endif

#ifdef USE_MSVC_MD_LIBRARY
  GC_pre_init();
#endif

#ifdef GC_THINKS_ITS_A_DLL_BUT_ISNT
  DllMain(NULL, DLL_PROCESS_ATTACH, NULL);
#endif

  /******* OSKIT init START *******/
#if defined(OSKIT) && !defined(OSKIT_TEST)
  oskit_init_libc();

# ifdef OSK_LINUX_FILESYSTEMS
  if (argc > 2) {
    start_linux_fs(argv[1], argv[2]);
    argv[2] = argv[0];
    argv += 2;
    argc -= 2;
  } else {
    printf("No disk or partition specified; using in-memory filesystem.\n");
    fs_init(oskit_bmod_init());
  }
# else
  fs_init(oskit_bmod_init());
# endif

# ifdef USE_OSKIT_CONSOLE
  /* We talk to console directly */
  direct_cons_set_flags(DC_NONBLOCK);
# else
  /* C library handles console */
  /* THIS DOESN'T WORK. I don't know why. */
#  ifndef OSK_LINUX_FILESYSTEMS
  oskit_dev_init();
#  endif
  oskit_freebsd_init_sc();
  oskit_console_init();
# endif

  start_clock();
#endif
  /******* OSKIT init END *******/

  scheme_actual_main = actual_main;

  return scheme_image_main(argc, argv);
}

#if 0
/* For testing STANDALONE_WITH_EMBEDDED_EXTENSION */
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_eval_string("(lambda (v) (and (eq? v #t) (lambda () (printf \"These were the args: ~a~n\" argv))))", env);
}
#endif
