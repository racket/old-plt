/*
 * File:        mred.cc
 * Purpose:     MrEd main file, including a hodge-podge of global stuff
 * Author:      Matthew Flatt
 * Created:     1995
 * Copyright:   (c) 1995-2000, Matthew Flatt
 */

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

/* wx_xt: */
#define Uses_XtIntrinsic
#define Uses_XtIntrinsicP
#define Uses_XLib
#define Uses_wxApp

#define IS_MRMAIN

#include "wx.h"

class wxStandardSnipClassList;
class wxBufferDataClassList;

#ifdef INCLUDE_WITHOUT_PATHS
# include "wxscheme.h"
# include "wxsmred.h"
#else
# include "wxs/wxscheme.h"
# include "wxs/wxsmred.h"
#endif

#include "mred.h"

#ifdef wx_mac
extern short wxMacDisableMods;
extern long wxMediaCreatorId;
#endif
#ifdef MACINTOSH_EVENTS
# include "simpledrop.h"
#endif


#if defined(_IBMR2)
static void dangerdanger(int)
{
  char *s = "mred: Danger - paging space low\n";
  write(2, s, strlen(s));
  scheme_collect_garbage();
}
#endif

static void yield_indefinitely()
{
  mred_wait_eventspace();
}

#ifndef DONT_LOAD_INIT_FILE
static char *get_init_filename(Scheme_Env *env)
{
  Scheme_Object *type;
  Scheme_Object *path;

  type = scheme_intern_symbol("init-file");
  
  path = wxSchemeFindDirectory(1, &type);

  return SCHEME_STR_VAL(path);
}
#endif

#ifdef STANDALONE_WITH_EMBEDDED_EXTENSION
extern "C" Scheme_Object *scheme_initialize(Scheme_Env *env);
#endif

#ifdef wx_x
# define INIT_FILENAME "~/.mredrc"
#else
# ifdef wx_msw
#  define INIT_FILENAME "%%HOMEDIRVE%%\\%%HOMEPATH%%\\mredrc.ss"
# else
#  define INIT_FILENAME "PREFERENCES:mredrc.ss"
# endif
#endif
#define GET_INIT_FILENAME get_init_filename
#if REDIRECT_STDIO || WINDOW_STDIO || WCONSOLE_STDIO
# define PRINTF scheme_console_printf
#else
# define PRINTF printf
#endif
#define PROGRAM "MrEd"
#define PROGRAM_LC "mred"

#ifdef wx_mac
#define GET_PLTCOLLECTS_VIA_RESOURCES
#endif

#ifdef GET_PLTCOLLECTS_VIA_RESOURCES
extern char *scheme_getenv_hack;
extern char *scheme_getenv_hack_value;
static char *pltcollects_from_resource;
#define SETUP_GETENV_HACK (scheme_getenv_hack = "PLTCOLLECTS", scheme_getenv_hack_value = pltcollects_from_resource);
#define TAKEDOWN_GETENV_HACK (scheme_getenv_hack = NULL, scheme_getenv_hack_value = NULL);
#else
#define SETUP_GETENV_HACK /* empty */
#define TAKEDOWN_GETENV_HACK /* empty */
#endif

#define CMDLINE_STDIO_FLAG
#define VERSION_YIELD_FLAG

#ifdef INCLUDE_WITHOUT_PATHS
# include "cmdline.inc"
#else
# include "../mzscheme/cmdline.inc"
#endif

#ifdef wx_mac
void Drop_Runtime(char **argv, int argc)
{
  int i;
  mz_jmp_buf savebuf;
  
  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));

  if (scheme_setjmp(scheme_error_buf)) {
    /* give up on rest */
    scheme_clear_escape();
  } else {
    for (i = 0; i < argc; i++) {
      Scheme_Object *p[1];
      p[0] = scheme_make_string(argv[i]);
      scheme_apply(wxs_app_file_proc, 1, p);
    }
  }

  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
}

void Drop_Quit()
{
  mz_jmp_buf savebuf;
  
  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));

  if (scheme_setjmp(scheme_error_buf)) {
    scheme_clear_escape();
  } else {
    scheme_apply(wxs_app_quit_proc, 0, NULL);
  }

  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));
}
#endif

#ifdef wx_x
#if INTERRUPT_CHECK_ON
static int interrupt_signal_received;

static void interrupt(int)
{
  interrupt_signal_received = 1;

  signal(SIGINT, interrupt);
}
#endif
#endif

#if defined(_IBMR2)
static void dangerdanger_gui(int)
{
  if (danger_signal_received) {
    fprintf(stderr, "mred: Danger - paging space STILL low - exiting\n");
    exit(-1);
  } else {
    fprintf(stderr, "mred: Danger - paging space low\n");
    scheme_collect_garbage();
    danger_signal_received = 1;
  }
  
  signal(SIGDANGER, dangerdanger_gui);
}
#endif

static void do_graph_repl(Scheme_Env *env)
{
  if (!scheme_setjmp(scheme_error_buf)) {
    scheme_eval_string("(graphical-read-eval-print-loop)", env);
  }
}

static FinishArgs *xfa;

static int finish_cmd_line_run(void)
{
  return finish_cmd_line_run(xfa, do_graph_repl);
}

static int do_main_loop(FinishArgs *fa)
{
  wxREGGLOB(xfa);
  xfa = fa;

  wxDoMainLoop();

  return 0;
}

static void run_from_cmd_line(int argc, char **argv, Scheme_Env *(*mk_basic_env)(void))
{
  run_from_cmd_line(argc, argv, mk_basic_env, do_main_loop);
}

int actual_main(int argc, char **argv)
{
  int r;

  wxCreateApp();

  r = wxEntry(argc, argv);

#ifdef wx_msw
  mred_clean_up_gdi_objects();
#endif	

  return r;
}

int main(int argc, char *argv[])
{
  void *stack_start;

  stack_start = (void *)&stack_start;

#if defined(MZ_PRECISE_GC)
  stack_start = (void *)&__gc_var_stack__;
  GC_init_type_tags(_scheme_last_type_, scheme_weak_box_type);
#endif

  /* Set stack base and turn off auto-finding of static variables ---
     unless this is MacOS, where wxWindows doesn't currently register
     its static variables, --- or Windows, where scheme_set_stack_base
     is called by wxWindows. */
#ifndef wx_msw
# ifndef MACOS_FIND_STACK_BOUNDS
#  ifndef OS_X
  scheme_set_stack_base(stack_start, 1);
#  endif
# endif
#endif

#if defined(_IBMR2)
  signal(SIGDANGER, dangerdanger_gui);
#endif
#ifdef wx_x
# if INTERRUPT_CHECK_ON
  signal(SIGINT, interrupt);
# endif
#endif

#ifdef SGC_STD_DEBUGGING
  fprintf(stderr, "Starting MrEd with sgc for debugging\n");
#endif

#ifdef wx_mac
  wxMacDisableMods = 4096;

#ifndef OS_X
  scheme_creator_id = 'MrEd';
  wxMediaCreatorId = 'MrEd';
#endif

# if !defined(__powerc) && !defined(__ppc__)
  long calcLimit, size;
  THz zone;
	
  zone = GetZone();
  size = ((long)LMGetCurStackBase()-(*(long *)zone)-sizeof(Zone));
  calcLimit = size - 1048576; /* 1 MB stack */
  if (calcLimit % 2)
    calcLimit++;
  SetApplLimit((Ptr)((*(long *)zone)+sizeof(Zone)+calcLimit));
# endif
#endif

#ifdef wx_mac
  /* initialize Mac stuff */
  ::InitCursor();
#ifdef OS_X
  ::MoreMasterPointers(512);
#else
  ::MaxApplZone();
  ::InitWindows();
  ::InitGraf(&qd.thePort);		
  ::InitFonts();
  ::InitDialogs(NULL);
  ::TEInit();
  ::InitMenus();
  for (int i=0; i<4; i++) {
	::MoreMasters();
          }
#endif
  
  Drop_GetArgs(&argc, &argv);
  
  { 
    KeyMap keys;
    GetKeys(keys);
    if (keys[1] & 32768L) { /* Cmd key down */
      DialogPtr dial;
      short hit, type;
      Rect box;
      Handle hand;
      Str255 str;
      char temp[256];
      int argc2;
      char **argv2;
  
      dial = GetNewDialog(128, NULL, (WindowRef)-1);
      do {
        ModalDialog(NULL, &hit);
      } while (hit > 2);
      if (hit == 1) {
        GetDialogItem(dial, 3, &type, &hand, &box);
        GetDialogItemText(hand, str);
        CopyPascalStringToC(str,temp);
        ParseLine(temp, &argc2, &argv2);
      } else {
        argc2 = 0;
        argv2 = NULL;
      }
      DisposeDialog(dial);
      
      if (argc2) {
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
#endif

  scheme_actual_main = actual_main;
  mred_run_from_cmd_line = run_from_cmd_line;
  mred_finish_cmd_line_run = finish_cmd_line_run;

  return scheme_image_main(argc, argv);
}


#ifdef wx_msw 

int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR m_lpCmdLine, int nCmdShow)
{
  return wxWinMain(hInstance, hPrevInstance, m_lpCmdLine, nCmdShow, main);
}

#endif
