
#include "escheme.h"
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#ifdef NEEDS_SELECT_H
# include <sys/select.h>
#endif
#include <readline/readline.h>

extern Function *rl_event_hook;

Scheme_Object *do_readline(int argc, Scheme_Object **argv)
{
  char *s;
  Scheme_Object *o;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("readline", "string", 0, argc, argv);

  s = readline(SCHEME_STR_VAL(argv[0]));
  if (!s)
    return scheme_eof;

  o = scheme_make_string(s);
  
  free(s);

  return o;
}

Scheme_Object *do_add_history(int argc, Scheme_Object **argv)
{
  char *s;
  Scheme_Object *o;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("add-history", "string", 0, argc, argv);
  
  add_history(SCHEME_STR_VAL(argv[0]));

  return scheme_void;
}

static int check(Scheme_Object *x)
{
  fd_set fd;
  struct timeval time = {0, 0};

  FD_ZERO(&fd);
  FD_SET(0, &fd);
  return select(1, &fd, NULL, NULL, &time);
}

static void set_fd_wait(Scheme_Object *x, void *fd)
{
  MZ_FD_SET(0, (fd_set *)fd);
}

static int block(void)
{  
  scheme_block_until(check, set_fd_wait, scheme_void, 0.0);
  return 0;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object *v;
  MZ_DECL_VAR_REG(1);
  MZ_VAR_REG(1, env);

  v = MZ_CWVR(scheme_intern_symbol("mzrl"));
  
  env = MZ_CWVR(scheme_primitive_module(v, env));

  v = MZ_CWVR(scheme_make_prim_w_arity(do_readline, "readline", 1, 1));
  MZ_CWVR(scheme_add_global("readline", v, env));

  v = MZ_CWVR(scheme_make_prim_w_arity(do_add_history, "add-history", 1, 1));
  MZ_CWVR(scheme_add_global("add-history", v, env));
  
  MZ_CWVR(scheme_finish_primitive_module(env));

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  rl_readline_name = "mzscheme";

  rl_event_hook = block;

  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("mzrl");
}
