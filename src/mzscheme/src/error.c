/*
  MzScheme
  Copyright (c) 1995-2001 Matthew Flatt

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
#include <ctype.h>
#ifdef DOS_FILE_SYSTEM
# include <windows.h>
#endif

/* globals */
void (*scheme_console_printf)(char *str, ...);
void (*scheme_console_output)(char *str, long len);
void (*scheme_exit)(int v);

#ifdef MEMORY_COUNTING_ON
long scheme_misc_count;
#endif

/* locals */
static Scheme_Object *error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_syntax_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_type_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *raise_mismatch_error(int argc, Scheme_Object *argv[]);
static Scheme_Object *error_escape_handler(int, Scheme_Object *[]);
static Scheme_Object *error_display_handler(int, Scheme_Object *[]);
static Scheme_Object *error_value_string_handler(int, Scheme_Object *[]);
static Scheme_Object *exit_handler(int, Scheme_Object *[]);
static Scheme_Object *error_print_width(int, Scheme_Object *[]);
static Scheme_Object *error_print_srcloc(int, Scheme_Object *[]);
static Scheme_Object *def_error_escape_proc(int, Scheme_Object *[]);
static Scheme_Object *def_error_display_proc(int, Scheme_Object *[]);
static Scheme_Object *def_error_value_string_proc(int, Scheme_Object *[]);
static Scheme_Object *def_exit_handler_proc(int, Scheme_Object *[]);

static Scheme_Object *do_raise(Scheme_Object *arg, int return_ok, int need_debug);

static Scheme_Object *newline_char;

static Scheme_Object *def_err_val_proc;
static Scheme_Object *def_error_esc_proc;
Scheme_Object *scheme_def_exit_proc;

static char *init_buf(long *len, long *blen);
static char *prepared_buf;
static long prepared_buf_len;

static Scheme_Object *kernel_symbol;

static Scheme_Object *syntax_sl; /* back-door argument to scheme_wrong_syntax */

typedef struct {
  int args;
  Scheme_Object *type;
  Scheme_Object **names;
  int count;
  Scheme_Object *exptime;
  int super_pos;
} exn_rec;

#define _MZEXN_TABLE
#include "schexn.h"
#undef _MZEXN_TABLE

static void default_printf(char *msg, ...)
{
  va_list args;
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  fflush(stderr);
}

static void default_output(char *s, long len)
{
  fwrite(s, len, 1, stderr);
  fflush(stderr);
}

void scheme_init_error_escape_proc(Scheme_Thread *p)
{
  if (!def_error_esc_proc) {
    REGISTER_SO(def_error_esc_proc);
    def_error_esc_proc =
      scheme_make_prim_w_arity(def_error_escape_proc,
			       "default-error-escape-handler",
			       0, 0);
  }

  scheme_set_param(p->config, MZCONFIG_ERROR_ESCAPE_HANDLER, def_error_esc_proc);
}

/*
  Recognized by scheme_[v]sprintf:

  %c = char
  %d = int
  %ld = long int
  %o = int, octal
  %f = double
  %% = percent

  %s = string
  %S = Scheme symbol
  %t = string with size
  %T = Scheme string
  %q = truncated-to-256 string
  %Q = truncated-to-256 Scheme string
  %V = scheme_value

  %L = line number, -1 means no line
  %e = error number for strerror()
  %E = error number for platform-specific error string
*/

static long sch_vsprintf(char *s, long maxlen, const char *msg, va_list args)
{
  long i, j;
  char buf[100];

  /* Since we might malloc, move all pointers into a local array for
     the sake of precise GC. We have to do numbers, too, for
     consistency. */

  int pp = 0, ip = 0, dp = 0;
  void *ptrs[25];
  long ints[25];
  double dbls[25];

  for (j = 0; msg[j]; j++) {
    if (msg[j] == '%') {
      int type;

      j++;
      type = msg[j];

      switch (type) {
      case 'c':	  
	ints[ip++] = va_arg(args, int);
	break;
      case 'd':	  
      case 'o':
	ints[ip++] = va_arg(args, int);
	break;
      case 'l':
	ints[ip++] = va_arg(args, long);
	break;
      case 'f':
	dbls[dp++] = va_arg(args, double);
	break;
      case 'L':	  
	ints[ip++] = va_arg(args, long);
	break;
      case 'e':	  
      case 'E':
	ints[ip++] = va_arg(args, int);
	break;
      case 'S':
      case 'V':
      case 'T':
      case 'Q':
	ptrs[pp++] = va_arg(args, Scheme_Object*);
	break;
      default:
	ptrs[pp++] = va_arg(args, char*);
	if (type == 't') {
	  ints[ip++] = va_arg(args, long);
	}
      }
    }
  }
  pp = 0;
  ip = 0;
  dp = 0;

  --maxlen;

  i = j = 0;
  while ((i < maxlen) && msg[j]) {
    if (msg[j] == '%') {
      int type;

      j++;
      type = msg[j++];

      if (type == '%')
	s[i++] = '%';
      else {
	const char *t;
	int tlen;
	int dots = 0;
	
	switch (type) {
	case 'c':	  
	  {
	    int c;
	    c = ints[ip++];
	    buf[0] = c;
	    t = buf;
	    tlen = 1;
	  }
	  break;
	case 'd':
	  {
	    int d;
	    d = ints[ip++];
	    sprintf(buf, "%d", d);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case 'o':
	  {
	    int d;
	    d = ints[ip++];
	    sprintf(buf, "%o", d);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case 'l':
	  {
	    long d;
	    j++;
	    d = ints[ip++];
	    sprintf(buf, "%ld", d);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case 'f':
	  {
	    double f;
	    j++;
	    f = dbls[dp++];
	    sprintf(buf, "%f", f);
	    t = buf;
	    tlen = strlen(t);
	  }
	  break;
	case 'L':	  
	  {
	    long d;
	    d = ints[ip++];
	    if (d >= 0) {
	      sprintf(buf, "%ld:", d);
	      t = buf;
	      tlen = strlen(t);
	    } else {
	      t = ":";
	      tlen = 1;
	    }
	  }
	  break;
	case 'e':
	case 'E':
	  {
	    int en;
	    en = ints[ip++];
	    if (en) {
	      char *es;
#ifdef NO_STRERROR_AVAILABLE
	      es = "Unknown error";
#else
# ifdef DOS_FILE_SYSTEM
	      char mbuf[256];
	      if (type == 'E') {
		if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, 
				  en, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
				  mbuf, 255, NULL)) {
		  int i;
		  es = mbuf;
		  /* Remove newlines: */
		  for (i = strlen(es) - 1; i > 0; i--) {
		    if (isspace(es[i]))
		      es[i] = 0;
		    else
		      break;
		  }
		} else
		  es = NULL;
	      } else
		es = NULL;
	      if (!es)
# endif
		es = strerror(en);
#endif
	      tlen = strlen(es) + 24;
	      t = (const char *)scheme_malloc_atomic(tlen);
	      sprintf((char *)t, "%s; errno=%d", es, en);
	      tlen = strlen(t);
	    } else {
	      t = "errno=?";
	      tlen = 7;
	    }
	    
	  }
	  break;
	case 'S':
	  {
	    Scheme_Object *sym;
	    sym = (Scheme_Object *)ptrs[pp++];
	    t = scheme_symbol_name_and_size(sym, &tlen, 0);
	  }
	  break;
	case 'V':
	  {
	    Scheme_Object *o;
	    o = (Scheme_Object *)ptrs[pp++];
	    t = scheme_make_provided_string(o, 1, &tlen);
	  }
	  break;
	case 'T':
	case 'Q':
	  {
	    Scheme_Object *str;
	    str = (Scheme_Object *)ptrs[pp++];
	    t = SCHEME_STR_VAL(str);
	    tlen = SCHEME_STRLEN_VAL(str);
	  }
	  break;
	default:
	  {
	    t = (char *)ptrs[pp++];
	    if (type == 't') {
	      tlen = ints[ip++];
	      if (tlen < 0)
		tlen = strlen(t);
	    } else
	      tlen = strlen(t);
	  }
	  break;
	}

	if ((type == 'q') || (type == 'Q')) {
	  if (tlen > 256) {
	    tlen = 250;
	    dots = 1;
	  }
	}
	
	while (tlen && i < maxlen) {
	  s[i++] = *(t++);
	  tlen--;
	}
	
	if (dots) {
	  if (i < maxlen - 3) {
	    s[i++] = '.';
	    s[i++] = '.';
	    s[i++] = '.';
	  }
	}
      }
    } else {
      s[i++] = msg[j++];
    }
  }

  s[i] = 0;

  return i;
}

static long scheme_sprintf(char *s, long maxlen, const char *msg, ...)
{
  long len;

  va_list args;
  va_start(args, msg);
  len = sch_vsprintf(s, maxlen, msg, args);
  va_end(args);

  return len;
}

void scheme_init_error(Scheme_Env *env)
{
  if (!scheme_console_printf)
    scheme_console_printf = default_printf;
  if (!scheme_console_output)
    scheme_console_output = default_output;

  scheme_add_global_constant("error", 
			     scheme_make_prim_w_arity(error, 
						      "error",
						      1, -1), 
			     env);
  scheme_add_global_constant("raise-syntax-error", 
			     scheme_make_prim_w_arity(raise_syntax_error, 
						      "raise-syntax-error",
						      2, 4), 
			     env);
  scheme_add_global_constant("raise-type-error", 
			     scheme_make_prim_w_arity(raise_type_error, 
						      "raise-type-error",
						      3, -1), 
			     env);
  scheme_add_global_constant("raise-mismatch-error", 
			     scheme_make_prim_w_arity(raise_mismatch_error, 
						      "raise-mismatch-error",
						      3, 3), 
			     env);
  scheme_add_global_constant("error-display-handler", 
			     scheme_register_parameter(error_display_handler, 
						       "error-display-handler",
						       MZCONFIG_ERROR_DISPLAY_HANDLER), 
			     env);
  scheme_add_global_constant("error-value->string-handler", 
			     scheme_register_parameter(error_value_string_handler, 
						       "error-value->string-handler",
						       MZCONFIG_ERROR_PRINT_VALUE_HANDLER), 
			     env);
  scheme_add_global_constant("error-escape-handler", 
			     scheme_register_parameter(error_escape_handler,
						      "error-escape-handler",
						       MZCONFIG_ERROR_ESCAPE_HANDLER),
			     env);
  scheme_add_global_constant("exit-handler", 
			     scheme_register_parameter(exit_handler, 
						       "exit-handler",
						       MZCONFIG_EXIT_HANDLER), 
			     env);
  scheme_add_global_constant("error-print-width", 
			     scheme_register_parameter(error_print_width, 
						       "error-print-width",
						       MZCONFIG_ERROR_PRINT_WIDTH), 
			     env);
  scheme_add_global_constant("error-print-source-location", 
			     scheme_register_parameter(error_print_srcloc, 
						       "error-print-source-location",
						       MZCONFIG_ERROR_PRINT_SRCLOC), 
			     env);
  scheme_add_global_constant("exit", 
			     scheme_make_prim_w_arity(scheme_do_exit, 
						      "exit", 
						      0, 1), 
			     env);

  newline_char = scheme_make_char('\n');

  REGISTER_SO(scheme_def_exit_proc);
  scheme_def_exit_proc = scheme_make_prim_w_arity(def_exit_handler_proc, 
						  "default-exit-handler",
						  1, 1);

  REGISTER_SO(def_err_val_proc);
  def_err_val_proc = scheme_make_prim_w_arity(def_error_value_string_proc,
					      "default-error-value->string-handler",
					      2, 2);
  
  REGISTER_SO(prepared_buf);
  prepared_buf = init_buf(NULL, &prepared_buf_len);
  
  REGISTER_SO(kernel_symbol);
  kernel_symbol = scheme_intern_symbol("#%kernel");

  REGISTER_SO(syntax_sl);

  scheme_init_error_config();
}

void scheme_init_error_config(void)
{
  Scheme_Config *config = scheme_config;

  scheme_set_param(config, MZCONFIG_EXIT_HANDLER, scheme_def_exit_proc);

  {
    Scheme_Object *edh;
    edh = scheme_make_prim_w_arity(def_error_display_proc,
				   "default-error-display-handler",
				   2, 2);
    scheme_set_param(config, MZCONFIG_ERROR_DISPLAY_HANDLER, edh);
  }

  scheme_set_param(config, MZCONFIG_ERROR_PRINT_VALUE_HANDLER,
		   def_err_val_proc);
}

static void
scheme_inescapeable_error(const char *a, const char *b)
{
  int al, bl;
  char *t;

  al = strlen(a);
  bl = strlen(b);
  t = scheme_malloc_atomic(al + bl + 2);
  memcpy(t, a, al);
  memcpy(t + al, b, bl);
  t[al + bl] = '\n';
  t[al + bl + 1] = 0;
  
  scheme_console_output(t, al + bl + 1);
}

#define RAISE_RETURNED "exception handler did not escape"

static void
call_error(char *buffer, int len, Scheme_Object *exn)
{
  Scheme_Object *p[2];
  mz_jmp_buf savebuf;

  if (scheme_current_thread->error_invoked == 5) {
    scheme_longjmp (scheme_error_buf, 1);
  } else if (scheme_current_thread->error_invoked == 1) {
    scheme_inescapeable_error("error trying to display error: ", buffer);
    scheme_longjmp (scheme_error_buf, 1);
  } else if (scheme_current_thread->error_invoked == 2) {
    scheme_inescapeable_error("error trying to escape from error: ", buffer);
    scheme_longjmp(scheme_error_buf, 1);
  } else {
    scheme_current_thread->error_invoked = 1;
    p[0] = scheme_make_immutable_sized_string(buffer, len, 1);
    p[1] = exn;
    memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));
    if (scheme_setjmp(scheme_error_buf)) {
      scheme_current_thread->error_invoked = 0;
      scheme_longjmp(savebuf, 1);
    } else {
      if (buffer)
	scheme_apply_multi(scheme_get_param(scheme_config, MZCONFIG_ERROR_DISPLAY_HANDLER), 2, p);
      scheme_current_thread->error_invoked = 2;
      /* Typically jumps out of here */
      scheme_apply_multi(scheme_get_param(scheme_config, MZCONFIG_ERROR_ESCAPE_HANDLER), 0, NULL);
      /* Uh-oh; record the error fall back to the default escaper */
      scheme_inescapeable_error("error escape handler did not escape; calling the default error escape handler", "");
      scheme_current_thread->error_invoked = 0;
      scheme_longjmp(savebuf, 1); /* force an exit */
    }
  }
}

static long get_print_width(void)
{
  long print_width;
  Scheme_Object *w;

  w = scheme_get_param(scheme_config, MZCONFIG_ERROR_PRINT_WIDTH);
  if (SCHEME_INTP(w))
    print_width = SCHEME_INT_VAL(w);
  else
    print_width = 10000;
  
  return print_width;
}

static char *init_buf(long *len, long *_size)
{
  long size, print_width;

  print_width = get_print_width();
  
  if (len)
    *len = print_width;

  size = (3 * scheme_max_found_symbol_name + 500
	  + 2 * print_width);
  if (_size)
    *_size = size;

  return (char *)scheme_malloc_atomic(size);
}

void scheme_reset_prepared_error_buffer(void)
{
  prepared_buf = init_buf(NULL, &prepared_buf_len);
}

void
scheme_signal_error (char *msg, ...)
{
  va_list args;
  char *buffer;
  long len;

  /* Precise GC: Don't allocate before getting hidden args off stack */
  buffer = prepared_buf;

  va_start(args, msg);
  len = sch_vsprintf(buffer, prepared_buf_len, msg, args);
  va_end(args);

  prepared_buf = init_buf(NULL, &prepared_buf_len);

  if (scheme_current_thread->current_local_env) {
    char *s2 = " [during expansion]";
    strcpy(buffer + len, s2);
    len = strlen(s2);
  }

  buffer[len++] = '\n';
  buffer[len] = 0;

  if (scheme_starting_up) {
    scheme_console_output(buffer, len);
    exit(0);
  }

#ifndef SCHEME_NO_EXN
  scheme_raise_exn(MZEXN_MISC, "%t", buffer, len);
#else
  call_error(buffer, len, scheme_false);
#endif
}

void scheme_warning(char *msg, ...)
{
  va_list args;
  char *buffer;
  long len;

  /* Precise GC: Don't allocate before getting hidden args off stack */
  buffer = prepared_buf;

  va_start(args, msg);
  len = sch_vsprintf(buffer, prepared_buf_len, msg, args);
  va_end(args);

  prepared_buf = init_buf(NULL, &prepared_buf_len);

  buffer[len++] = '\n';
  buffer[len] = 0;

  scheme_write_string(buffer, len,
		      scheme_get_param(scheme_config, MZCONFIG_ERROR_PORT));
}

static void pre_conv(void *v)
{
  scheme_current_thread->err_val_str_invoked = 1;
}
	
static Scheme_Object *now_do_conv(void *v)
{
  Scheme_Object **argv = (Scheme_Object **)v;
  return _scheme_apply(argv[2], 2, argv);
}

static void post_conv(void *v)
{
  scheme_current_thread->err_val_str_invoked = 0;
}
	
static char *error_write_to_string_w_max(Scheme_Object *v, int len, int *lenout)
{
  Scheme_Object *o, *args[3];

  o = scheme_get_param(scheme_config, MZCONFIG_ERROR_PRINT_VALUE_HANDLER);
  
  if ((SAME_OBJ(o, def_err_val_proc)
       && SAME_OBJ(scheme_get_param(scheme_config, MZCONFIG_PORT_PRINT_HANDLER),
		   scheme_default_global_print_handler))
      || (scheme_current_thread->err_val_str_invoked)) {
    long l;
    char *s;
    s = scheme_write_to_string_w_max(v, &l, len);
    if (lenout)
      *lenout = l;
    return s;
  } else {
    args[0] = v;
    args[1] = scheme_make_integer(len);
    args[2] = o;

    o = scheme_dynamic_wind(pre_conv, now_do_conv,
			    post_conv, NULL,
			    (void *)args);

    if (SCHEME_STRINGP(o)) {
      char *s = SCHEME_STR_VAL(o);
      if (SCHEME_STRTAG_VAL(o) > len) {
	s[len] = 0;
	if (lenout)
	  *lenout = len;
      } else if (lenout)
	*lenout = SCHEME_STRTAG_VAL(o);
      return s;
    } else {
      if (lenout)
	*lenout = 3;
      return "...";
    }
  }
}

static char *make_arity_expect_string(const char *name, int namelen,
				      int minc, int maxc, 
				      int argc, Scheme_Object **argv,
				      long *_len, int is_method)
{
  long len, pos, slen;
  int xargc, xminc, xmaxc;
  char *s;

  s = init_buf(&len, &slen);

  if (!name)
    name = "#<procedure>";
  
  xargc = argc - (is_method ? 1 : 0);
  xminc = minc - (is_method ? 1 : 0);
  xmaxc = maxc - (is_method ? 1 : 0);

  if (minc < 0) {
    const char *n;
    int nlen;

    if (minc == -2) {
      n = name;
      nlen = (namelen < 0 ? strlen(n) : namelen);
    } else
      n = scheme_get_proc_name((Scheme_Object *)name, &nlen, 1);

    if (!n) {
      n = "#<case-lambda-procedure>";
      nlen = strlen(n);
    }

    pos = scheme_sprintf(s, slen, "%t: no clause matching %d argument%s",
			 n, nlen,
			 xargc, xargc == 1 ? "" : "s");
  } else if (!maxc)
    pos = scheme_sprintf(s, slen, "%t: expects no arguments, given %d",
			 name, namelen, xargc);
  else if (maxc < 0)
    pos = scheme_sprintf(s, slen, "%t: expects at least %d argument%s, given %d",
			 name, namelen, xminc, (xminc == 1) ? "" : "s", xargc);
  else if (minc == maxc)
    pos = scheme_sprintf(s, slen, "%t: expects %d argument%s, given %d",
			 name, namelen, xminc, (xminc == 1) ? "" : "s", xargc);
  else
    pos = scheme_sprintf(s, slen, "%t: expects %d to %d arguments, given %d",
			 name, namelen, xminc, xmaxc, xargc);

  if (xargc && argv) {
    len /= xargc;
    if ((xargc < 50) && (len >= 3)) {
      int i;
      
      strcpy(s + pos, ":");
      pos++;
      
      for (i = (is_method ? 1 : 0); i < argc; i++) {
	int l;
	char *o;
	o = error_write_to_string_w_max(argv[i], len, &l);
	memcpy(s + pos, " ", 1);
	memcpy(s + pos + 1, o, l);
	pos += l + 1;
      }

      s[pos] = 0;
    }
  }

  *_len = pos;

  return s;
}

void scheme_wrong_count_m(const char *name, int minc, int maxc, 
			  int argc, Scheme_Object **argv, int is_method)
{
  Scheme_Object *arity; 
  Scheme_Object *v;
  char *s;
  long len;
  Scheme_Thread *p = scheme_current_thread;

  if (argv == p->tail_buffer) {
    /* See calls in scheme_do_eval: */
    GC_CAN_IGNORE Scheme_Object **tb;
    p->tail_buffer = NULL; /* so args aren't zeroed */
    tb = MALLOC_N(Scheme_Object *, p->tail_buffer_size);
    p->tail_buffer = tb;
  }

  /* minc = 1 -> name is really a case-lambda proc */

  if (minc == -1) {
    /* Check for is_method in case-lambda */
    Scheme_Case_Lambda *cl = (Scheme_Case_Lambda *)name;
    if (cl->count) {
      Scheme_Closure_Compilation_Data *data;
      data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(cl->array[0]);
      if (data->flags & CLOS_IS_METHOD)
	is_method = 1;
    } else if (cl->name && SCHEME_BOXP(cl->name)) {
      /* See note in schpriv.h about the IS_METHOD hack */
      is_method = 1;
    }
  }

  /* Watch out for impossible is_method claims: */
  if (!argc || !minc)
    is_method = 0;

  v = scheme_make_integer(argc - (is_method ? 1 : 0));

  s = make_arity_expect_string(name, -1, minc, maxc, argc, argv, &len, is_method);

  if (minc >= 0)
    arity = scheme_make_arity((short)(minc - (is_method ? 1 : 0)), (short)(maxc - (is_method ? 1 : 0)));
  else if (minc == -1) {
    arity = scheme_arity((Scheme_Object *)name);
    if (is_method) {
      /* Post-process the arity. It must be a list. We can mutate it. */
      Scheme_Object *l = arity, *a, *b;
      while (!SCHEME_NULLP(l)) {
	a = SCHEME_CAR(l);
	if (SCHEME_INTP(a))
	  SCHEME_CAR(l) = scheme_make_integer(SCHEME_INT_VAL(a) - 1);
	else {
	  /* arity-at-least: */
	  b = ((Scheme_Structure *)a)->slots[0];
	  b = scheme_make_integer(SCHEME_INT_VAL(b) - 1);
	  ((Scheme_Structure *)a)->slots[0] = b;
	}
	l = SCHEME_CDR(l);
      }
    }
  } else
    arity = scheme_false; /* BUG! if this happens */

  scheme_raise_exn(MZEXN_APPLICATION_ARITY, v, arity, "%t", s, len);
}

void scheme_wrong_count(const char *name, int minc, int maxc, int argc,
			Scheme_Object **argv)
{
  /* don't allocate here, in case rands == p->tail_buffer */
  scheme_wrong_count_m(name, minc, maxc, argc, argv, 0);
}

void scheme_case_lambda_wrong_count(const char *name, 
				    int argc, Scheme_Object **argv,
				    int is_method,
				    int count, ...)
{
  Scheme_Object *arity, *a;
  char *s;
  long len;
  va_list args;
  int i;

  /* Watch out for impossible is_method claims: */
  if (!argc)
    is_method = 0;

  arity = scheme_alloc_list(count);

  va_start(args, count);
  for (i = 0, a = arity; i < count; i++, a = SCHEME_CDR(a)) {
    short mina, maxa;
    Scheme_Object *av;

    mina = va_arg(args, int);
    maxa = va_arg(args, int);
    
    if (is_method) {
      mina -= 1;
      maxa -= 1;
    }

    av = scheme_make_arity(mina, maxa);
    SCHEME_CAR(a) = av;
  }
  va_end(args);

  s = make_arity_expect_string(name, -1, -2, 0, argc, argv, &len, is_method);

  scheme_raise_exn(MZEXN_APPLICATION_ARITY, scheme_make_integer(argc), 
		   arity, "%t", s, len);
}

char *scheme_make_arity_expect_string(Scheme_Object *proc,
				      int argc, Scheme_Object **argv,
				      long *_slen)
{
  const char *name;
  int namelen = -1;
  int mina, maxa;

  if (SCHEME_PRIMP(proc)) {
    name = ((Scheme_Primitive_Proc *)proc)->name;
    mina = ((Scheme_Primitive_Proc *)proc)->mina;
    maxa = ((Scheme_Primitive_Proc *)proc)->maxa;
  } else if (SCHEME_CLSD_PRIMP(proc)) {
    name = ((Scheme_Closed_Primitive_Proc *)proc)->name;
    mina = ((Scheme_Closed_Primitive_Proc *)proc)->mina;
    maxa = ((Scheme_Closed_Primitive_Proc *)proc)->maxa;
  } else if (SAME_TYPE(SCHEME_TYPE(proc), scheme_case_closure_type)) {
    name = scheme_get_proc_name(proc, &namelen, 1);
    mina = -2;
    maxa = 0;
  } else {
    Scheme_Closure_Compilation_Data *data;
    
    data = (Scheme_Closure_Compilation_Data *)SCHEME_COMPILED_CLOS_CODE(proc);
    mina = maxa = data->num_params;
    if (data->flags & CLOS_HAS_REST) {
      --mina;
      maxa = -1;
    }
    name = scheme_get_proc_name(proc, &namelen, 1);
  }

  return make_arity_expect_string(name, namelen, mina, maxa, argc, argv, _slen, 0);
}

char *scheme_make_args_string(char *s, int which, int argc, Scheme_Object **argv, long *_olen)
{
  char *other;
  long len;
  GC_CAN_IGNORE char *isres = "arguments";

  other = init_buf(&len, NULL);

  if (argc < 0) {
    isres = "results";
    argc = -argc;
  }
  
  len /= (argc - (((which >= 0) && (argc > 1)) ? 1 : 0));
  if ((argc < 50) && (len >= 3)) {
    int i, pos;
    
    sprintf(other, "; %s%s were:", s, isres);
    pos = strlen(other);
    for (i = 0; i < argc; i++) {
      if (i != which) {
	int l;
	char *o;
	o = error_write_to_string_w_max(argv[i], len, &l);
	memcpy(other + pos, " ", 1);
	memcpy(other + pos + 1, o, l);
	pos += l + 1;
      }
    }
    other[pos] = 0;
    if (_olen)
      *_olen = pos;
  } else {
    sprintf(other, "; given %d arguments total", argc);
    if (_olen)
      *_olen = strlen(other);
  }

  return other;
}

const char *scheme_number_suffix(int which)
{
  static char *ending[] = {"st", "nd", "rd"};

  if (!which)
    return "th";
  --which;

  which = which % 100;

  return ((which < 10 || which >= 20)
	  && ((which % 10) < 3)) ? ending[which % 10] : "th";
}

void scheme_wrong_type(const char *name, const char *expected, 
		       int which, int argc,
		       Scheme_Object **argv)
{
  Scheme_Object *o;
  char *s;
  int slen;
  int isres = 0;
  GC_CAN_IGNORE char *isress = "argument";
  Scheme_Object *typesym;

  o = argv[which < 0 ? 0 : which];
  if (argc < 0) {
    argc = -argc;
    isress = "result";
    isres = 1;
  }

  s = scheme_make_provided_string(o, 1, &slen);

  typesym = scheme_intern_symbol(expected);

  if ((which < 0) || (argc == 1))
    scheme_raise_exn(MZEXN_APPLICATION_TYPE, o, typesym,
		     "%s: expects %s of type <%s>; "
		     "given %t",
		     name, isress, expected, s, slen);
  else {
    char *other;
    long olen;

    if ((which >= 0) && (argc > 1))
      other = scheme_make_args_string("other ", which, 
				      (isres ? -argc : argc), 
				      argv, &olen);
    else {
      other = "";
      olen = 0;
    }

    scheme_raise_exn(MZEXN_APPLICATION_TYPE, o, typesym,
		     "%s: expects type <%s> as %d%s %s, "
		     "given: %t%t",
		     name, expected, which + 1,
		     scheme_number_suffix(which + 1),
		     isress,
		     s, slen, other, olen);
  }
}

void scheme_arg_mismatch(const char *name, const char *msg, Scheme_Object *o)
{
  char *s;
  int slen;

  s = scheme_make_provided_string(o, 1, &slen);

  scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, o,
		   "%s: %s%t",
		   name, msg, s, slen);
}

#define MZERR_MAX_SRC_LEN 100

static char *make_srcloc_string(Scheme_Stx_Srcloc *srcloc, long *len)
{
  long line, col;

  line = srcloc->line;
  col = srcloc->col;
  if (col < 0)
    col = srcloc->pos;

  if (col >= 0) {
    Scheme_Object *src;
    char *srcstr, *result;
    long srclen, rlen;
    
    src = srcloc->src;
    if (src && SCHEME_STRINGP(src)) {
      /* Strip off prefix matching the current directory: */
      src = scheme_remove_current_directory_prefix(src);

      /* Truncate from the front, to get the interesting part of paths: */
      srclen = SCHEME_STRLEN_VAL(src);
      if (srclen > MZERR_MAX_SRC_LEN) {
	srcstr = scheme_malloc_atomic(MZERR_MAX_SRC_LEN);
	memcpy(srcstr, SCHEME_STR_VAL(src) + (srclen - MZERR_MAX_SRC_LEN),
	       MZERR_MAX_SRC_LEN);
	srcstr[0] = '.';
	srcstr[1] = '.';
	srcstr[2] = '.';
	srclen = MZERR_MAX_SRC_LEN;
      } else
	srcstr = SCHEME_STR_VAL(src);
    } else
      srcstr = scheme_display_to_string_w_max(src, &srclen, MZERR_MAX_SRC_LEN);
    
    result = (char *)scheme_malloc_atomic(srclen + 15);

    rlen = scheme_sprintf(result, srclen + 15, "%t:%L%ld: ", 
			  srcstr, srclen, line, col);
    
    if (len) *len = rlen;
    return result;
  } else {
    if (len) *len = 0;
    return NULL;
  }
}

void scheme_read_err(Scheme_Object *port, 
		     Scheme_Object *stxsrc,
		     long line, long col, long pos, long span, 
		     int gotc,
		     const char *detail, ...)
{
  va_list args;
  char *s, *ls, lbuf[30], *fn;
  long slen, fnlen;
  int show_loc;

  /* Precise GC: Don't allocate before getting hidden args off stack */
  s = prepared_buf;

  va_start(args, detail);
  slen = sch_vsprintf(s, prepared_buf_len, detail, args);
  va_end(args);
  
  prepared_buf = init_buf(NULL, &prepared_buf_len);

  ls = "";
  fnlen = 0;

  show_loc = SCHEME_TRUEP(scheme_get_param(scheme_config, MZCONFIG_ERROR_PRINT_SRCLOC));
  
  if (stxsrc) {
    Scheme_Object *xsrc;

    xsrc = scheme_make_stx_w_offset(scheme_false, line, col, pos, span, stxsrc, STX_SRCTAG);

    stxsrc = ((Scheme_Stx *)xsrc)->srcloc->src;
    line = ((Scheme_Stx *)xsrc)->srcloc->line;
    col = ((Scheme_Stx *)xsrc)->srcloc->col;
    pos = ((Scheme_Stx *)xsrc)->srcloc->pos;

    if (show_loc)
      fn = make_srcloc_string(((Scheme_Stx *)xsrc)->srcloc, &fnlen);
    else
      fn = NULL;
  } else
    fn = NULL;

  if (!fn && show_loc) {
    long column;

    if (col < 0)
      column = pos;
    else
      column = col;

    if (port) {
      Scheme_Object *str;
      fn = SCHEME_IPORT_NAME(port);
      str = scheme_make_string_without_copying(fn);
      str = scheme_remove_current_directory_prefix(str);
      fn = SCHEME_STR_VAL(str);
    } else
      fn = "UNKNOWN";

    fnlen = strlen(fn);

    if (column >= 0) {
      scheme_sprintf(lbuf, 30, ":%L%ld: ", line, column);
      ls = lbuf;
    } else
      ls = ": ";
  } else if (!show_loc) {
    fn = "";
    fnlen = 0;
  }
    

  scheme_raise_exn((gotc == EOF) ? MZEXN_READ_EOF : ((gotc == SCHEME_SPECIAL) ? MZEXN_READ_NON_CHAR : MZEXN_READ), 
		   stxsrc ? stxsrc : scheme_false,
		   (line < 0) ? scheme_false : scheme_make_integer(line),
		   (col < 0) ? scheme_false : scheme_make_integer(col),
		   (pos < 0) ? scheme_false : scheme_make_integer(pos),
		   (span < 0) ? scheme_false : scheme_make_integer(span),
		   "%t%s%t", 
		   fn, fnlen, ls,
		   s, slen);
}

const char *scheme_compile_stx_string = "compile";
const char *scheme_expand_stx_string = "expand";
const char *scheme_application_stx_string = "application";
const char *scheme_set_stx_string = "set!";
const char *scheme_begin_stx_string = "begin";

void scheme_wrong_syntax(const char *where, 
			 Scheme_Object *detail_form, 
			 Scheme_Object *form, 
			 const char *detail, ...)
{
  long len, slen, vlen, dvlen, blen, plen;
  char *s, *buffer;
  char *v, *dv, *p;
  Scheme_Object *mod, *nomwho, *who;
  int show_src;

  who = NULL;
  nomwho = NULL;
  mod = scheme_false;

  /* Check for special strings that indicate `form' doesn't have a
     good name: */
  if ((where == scheme_compile_stx_string)
      || (where == scheme_expand_stx_string)) {
    who = nomwho = scheme_false;
  } else if (where == scheme_application_stx_string) {
    who = scheme_intern_symbol("#%app");
    nomwho = who;
    mod = scheme_intern_symbol("mzscheme");
  } else if ((where == scheme_set_stx_string)
	     || (where == scheme_begin_stx_string)) {
    who = scheme_intern_symbol(where);
    nomwho = who;
    mod = scheme_intern_symbol("mzscheme");
    if (where == scheme_begin_stx_string)
      where = "begin (possibly implicit)";
  } else if (syntax_sl) {
    who = SCHEME_CAR(syntax_sl);
    nomwho = SCHEME_CADR(syntax_sl);
    mod = SCHEME_CADR(SCHEME_CDR(syntax_sl));
    syntax_sl = NULL;
  }

  if (!detail) {
    s = "bad syntax";
    slen = strlen(s);
  } else {
    va_list args;

    /* Precise GC: Don't allocate before getting hidden args off stack */
    s = prepared_buf;

    va_start(args, detail);
    slen = sch_vsprintf(s, prepared_buf_len, detail, args);
    va_end(args);

    prepared_buf = init_buf(NULL, &prepared_buf_len);
  }
  
  buffer = init_buf(&len, &blen);

  p = NULL;
  plen = 0;

  show_src = SCHEME_TRUEP(scheme_get_param(scheme_config, MZCONFIG_ERROR_PRINT_SRCLOC));
  
  if (form) {
    Scheme_Object *pform;
    if (SCHEME_STXP(form)) {
      p = make_srcloc_string(((Scheme_Stx *)form)->srcloc, &plen);
      pform = scheme_syntax_to_datum(form, 0, NULL);

      /* Try to extract syntax name from syntax */
      if (!nomwho && (SCHEME_SYMBOLP(SCHEME_STX_VAL(form)) || SCHEME_STX_PAIRP(form))) {
	Scheme_Object *first;
	if (SCHEME_STX_PAIRP(form))
	  first = SCHEME_STX_CAR(form);
	else
	  first = form;
	if (SCHEME_SYMBOLP(SCHEME_STX_VAL(first))) {
	  /* Get module and name at source: */
	  int phase;
	  who = SCHEME_STX_VAL(first); /* printed name is local name */
	  /* name in exception is nominal source: */
 	  if (scheme_current_thread->current_local_env)
	    phase = scheme_current_thread->current_local_env->genv->phase;
	  else phase = 0;
	  scheme_stx_module_name(&first, phase, &mod, &nomwho);
	}
      }
    } else {
      pform = form;
      if (!detail_form)
	form = scheme_datum_to_syntax(form, scheme_false, scheme_false, 1, 0);
    }
    /* don't use error_write_to_string_w_max since this is code */
    if (show_src)
      v = scheme_write_to_string_w_max(pform, &vlen, len);
    else {
      v = NULL;
      vlen = 0;
    }
  } else {
    form = scheme_false;
    v = NULL;
    vlen = 0;
  }

  if (detail_form) {
    Scheme_Object *pform;
    if (SCHEME_STXP(detail_form)) {
      if (((Scheme_Stx *)detail_form)->srcloc->line >= 0)
	p = make_srcloc_string(((Scheme_Stx *)detail_form)->srcloc, &plen);
      pform = scheme_syntax_to_datum(detail_form, 0, NULL);
      /* To go in exn record: */
      form = detail_form;
    } else {
      pform = detail_form;
      /* To go in exn record: */
      form = scheme_datum_to_syntax(detail_form, 
				    /* Use source location of `form': */
				    SCHEME_STXP(form) ? form : scheme_false, 
				    scheme_false, 1, 0);
    }

    /* don't use error_write_to_string_w_max since this is code */
    if (show_src)
      dv = scheme_write_to_string_w_max(pform, &dvlen, len);
    else {
      dv = NULL;
      dvlen = 0;
    }
  } else {
    dv = NULL;
    dvlen = 0;
  }

  if (!who) {
    if (where)
      who = scheme_intern_symbol(where);
    else
      who = scheme_false;
  }
  if (!nomwho)
    nomwho = who;

  if (!where) {
    if (SCHEME_FALSEP(who))
      where = "?";
    else
      where = scheme_symbol_val(who);
  }

  if (v) {
    if (dv)
      blen = scheme_sprintf(buffer, blen, "%t%s: %t at: %t in: %t", 
			    p, plen, 
			    where, s, slen, 
			    dv, dvlen,
			    v, vlen);
    else
      blen = scheme_sprintf(buffer, blen, "%t%s: %t in: %t", 
			    p, plen,
			    where, s, slen, 
			    v, vlen);
  } else
    blen = scheme_sprintf(buffer, blen, "%s: %t", where, s, slen);
  
  scheme_raise_exn(MZEXN_SYNTAX, form, nomwho, mod, 
		   "%t", buffer, blen);
}

void scheme_wrong_rator(Scheme_Object *rator, int argc, Scheme_Object **argv)
{
  long len, slen;
  int rlen;
  char *s, *r;

  s = init_buf(&len, NULL);

  r = scheme_make_provided_string(rator, 1, &rlen);

  if (argc)
    len /= argc;

  slen = 0;
  if (argc && (argc < 50) && (len >= 3)) {
    int i;

    strcpy(s, "; arguments were:");
    slen = 17;
    for (i = 0; i < argc; i++) {
      char *o;
      int olen;

      o = error_write_to_string_w_max(argv[i], len, &olen);
      memcpy(s + slen, " ", 1);
      memcpy(s + slen + 1, o, olen);
      slen += 1 + olen;
    }
    s[slen] = 0;
  } else {
    slen = -1;
    if (argc)
      sprintf(s, " (%d args)", argc);
    else
      s = " (no arguments)";
  }

  scheme_raise_exn(MZEXN_APPLICATION_TYPE, rator, scheme_intern_symbol("procedure"),
		   "procedure application: expected procedure, given: %t%t",
		   r, rlen, s, slen);
}

void scheme_wrong_return_arity(const char *where, 
			       int expected, int got,
			       Scheme_Object **argv,
			       const char *detail, ...)
{
  long slen, vlen, blen;
  char *s, *buffer;
  char *v;

  if ((got != 1) && SAME_OBJ(scheme_current_thread->ku.multiple.array,
			     scheme_current_thread->values_buffer))
    scheme_current_thread->values_buffer = NULL;
    
  if (!detail) {
    s = NULL;
    slen = 0;
  } else {
    va_list args;

    /* Precise GC: Don't allocate before getting hidden args off stack */
    s = prepared_buf;

    va_start(args, detail);
    slen = sch_vsprintf(s, prepared_buf_len, detail, args);
    va_end(args);

    prepared_buf = init_buf(NULL, &prepared_buf_len);
  }

  buffer = init_buf(NULL, &blen);

  if (!got || !argv) {
    v = "";
    vlen = 0;
  } else {
    int i;
    long len, origlen, maxpos;
    Scheme_Object **array;

    v = init_buf(&len, NULL);
    v[0] = ':';
    v[1] = 0;

    array = ((got == 1) ? (Scheme_Object **)&argv : argv);

    origlen = len;
    len /= got;

    maxpos = got;
    if (len < 3) {
      maxpos = origlen / 4;
      len = 3;
    }

    vlen = 1;
    for (i = 0; i < maxpos; i++) {
      char *o;
      int olen;

      o = error_write_to_string_w_max(array[i], len, &olen);
      memcpy(v + vlen, " ", 1);
      memcpy(v + vlen + 1, o, olen);
      vlen += 1 + olen;
    }

    if (maxpos != got) {
      strcpy(v + vlen, " ...");
      vlen += 4;
    }
    v[vlen] = 0;
  }

  blen = scheme_sprintf(buffer, 
			blen,
			"%s%scontext%s%t%s expected %d value%s,"
			" received %d value%s%t",
			where ? where : "",
			where ? ": " : "",
			s ? " (" : "",
			s ? s : "",
			slen,
			s ? ")" : "",
			expected,
			(expected == 1) ? "" : "s",
			got,
			(got == 1) ? "" : "s",
			v, vlen);

  scheme_raise_exn(MZEXN_APPLICATION_ARITY, 
		   scheme_make_integer(got),
		   scheme_make_integer(expected),
		   "%t",
		   buffer, blen);
}

void scheme_raise_out_of_memory(const char *where, const char *msg, ...)
{
  char *s;
  long slen;

  if (!msg) {
    s = "";
    slen = 0;
  } else {
    va_list args;

    /* Precise GC: Don't allocate before getting hidden args off stack */
    s = prepared_buf;

    va_start(args, msg);
    slen = sch_vsprintf(s, prepared_buf_len, msg, args);
    va_end(args);

    prepared_buf = init_buf(NULL, &prepared_buf_len);
  }

  scheme_raise_exn(MZEXN_MISC_OUT_OF_MEMORY,
		   "%s%sout of memory %t",
		   where ? where : "",
		   where ? ": " : "",
		   s, slen);
}

void scheme_unbound_global(Scheme_Bucket *b)
{
  Scheme_Object *name = (Scheme_Object *)b->key;

  if (((Scheme_Bucket_With_Home *)b)->home->module) {
    scheme_raise_exn(MZEXN_VARIABLE, 
		     name,
		     "reference to uninitialized module identifier: %S in module: %S",
		     name,
		     ((Scheme_Bucket_With_Home *)b)->home->module->modname);
  } else {
    scheme_raise_exn(MZEXN_VARIABLE, 
		     name,
		     "reference to undefined identifier: %S",
		     name);
  }
}

char *scheme_make_provided_string(Scheme_Object *o, int count, int *lenout)
{
  long len;

  len = get_print_width();

  if (count)
    len /= count;

  return error_write_to_string_w_max(o, len, lenout);
}

static Scheme_Object *error(int argc, Scheme_Object *argv[])
{
  Scheme_Object *newargs[2];

  if (SCHEME_SYMBOLP(argv[0])) {
    if (argc < 2) {
      const char *s;
      int l;

      s = scheme_symbol_val(argv[0]);
      l = SCHEME_SYM_LEN(argv[0]);

      /* Just a symbol */
      newargs[0] = 
	scheme_append_string(scheme_make_string("error: "),
			     scheme_make_sized_string((char *)s, l, 1));
      SCHEME_SET_STRING_IMMUTABLE(newargs[0]);
    } else {
      char *s, *r;
      long l, l2;
      Scheme_Object *port;
      port = scheme_make_string_output_port();

      /* Chez-style: symbol, format string, format items... */
      if (!SCHEME_STRINGP(argv[1]))
	scheme_wrong_type("error", "string", 1, argc, argv);
      
      scheme_do_format("error", port, NULL, -1, 1, 2, argc, argv);

      s = scheme_get_sized_string_output(port, &l);

      l2 = SCHEME_SYM_LEN(argv[0]);
      r = MALLOC_N_ATOMIC(char, l + l2 + 3);
      memcpy(r, SCHEME_SYM_VAL(argv[0]), l2);
      memcpy(r + l2, ": ", 2);
      memcpy(r + l2 + 2, s, l + 1);

      newargs[0] = scheme_make_immutable_sized_string(r, l + l2 + 2, 0);
    }
  } else {
    Scheme_Config *config = scheme_config;
    Scheme_Object *strout;
    char *str;
    long len, i;

    /* String followed by other values: */
    if (!SCHEME_STRINGP(argv[0]))
      scheme_wrong_type("error", "string or symbol", 0, argc, argv);

    strout = scheme_make_string_output_port();

    scheme_internal_display(argv[0], strout, config);
    for (i = 1; i < argc ; i++) {
      scheme_write_string(" ", 1, strout);
      scheme_internal_write(argv[i], strout, config);
    }

    str = scheme_get_sized_string_output(strout, &len);
    newargs[0] = scheme_make_immutable_sized_string(str, len, 0);
  }

#ifndef NO_SCHEME_EXNS
  newargs[1] = scheme_void;
  do_raise(scheme_make_struct_instance(exn_table[MZEXN_USER].type, 
				       2, newargs), 
	   0, 1);
  
  return scheme_void;
#else
  _scheme_apply_multi(scheme_get_param(scheme_config, MZCONFIG_ERROR_DISPLAY_HANDLER), 1, newargs);

  return _scheme_tail_apply(scheme_get_param(scheme_config, MZCONFIG_ERROR_ESCAPE_HANDLER),
			    0, NULL);
#endif
}

static Scheme_Object *raise_syntax_error(int argc, Scheme_Object *argv[])
{
  const char *who;
  Scheme_Object *sl = NULL;

  if (scheme_proper_list_length(argv[0]) == 3) {
    if (SCHEME_SYMBOLP(SCHEME_CAR(argv[0]))) {
      sl = SCHEME_CDR(argv[0]);
      if (SCHEME_SYMBOLP(SCHEME_CAR(argv[0]))
	  || SCHEME_FALSEP(SCHEME_CAR(argv[0]))) {
	sl = SCHEME_CADR(sl);
	if (!SCHEME_SYMBOLP(sl)
	    && !SCHEME_FALSEP(sl)
	    && !SAME_TYPE(SCHEME_TYPE(sl), scheme_module_index_type))
	  sl = NULL;
	else
	  sl = argv[0];
      } else 
	sl = NULL;
    }
  }

  if (!sl && !SCHEME_FALSEP(argv[0]) && !SCHEME_SYMBOLP(argv[0]))      
    scheme_wrong_type("raise-syntax-error", "symbol, module source list, or #f", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("raise-syntax-error", "string", 1, argc, argv);

  if (SCHEME_SYMBOLP(argv[0]))
    who = scheme_symbol_val(argv[0]);
  else
    who = NULL;

  syntax_sl = sl; /* back-door argument to scheme_wrong_syntax */

  scheme_wrong_syntax(who,
		      (argc > 3) ? argv[3] : NULL,
		      (argc > 2) ? argv[2] : NULL,
		      "%T", argv[1]);

  return NULL;
}

static Scheme_Object *raise_type_error(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("raise-type-error", "symbol", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("raise-type-error", "string", 1, argc, argv);

  if (argc == 3) {
    Scheme_Object *v;
    v = argv[2];
    scheme_wrong_type(scheme_symbol_val(argv[0]),
		      SCHEME_STR_VAL(argv[1]),
		      -1, 0, &v);
  } else {
    Scheme_Object **args;
    int i;

    if (!(SCHEME_INTP(argv[2]) && (SCHEME_INT_VAL(argv[2]) >= 0))
	&& !(SCHEME_BIGNUMP(argv[2]) && SCHEME_BIGPOS(argv[2])))
      scheme_wrong_type("raise-type-error", "exact non-negative integer", 2, argc, argv);

    if ((SCHEME_INTP(argv[2]) && (SCHEME_INT_VAL(argv[2]) >= argc - 3))
	|| SCHEME_BIGNUMP(argv[2]))
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH, argv[2],
		       "raise-type-error: position index is %V, "
		       "but only %d arguments provided",
		       argv[2],
		       argc - 3);

    args = MALLOC_N(Scheme_Object *, argc - 3);
    for (i = 3; i < argc; i++) {
      args[i - 3] = argv[i];
    }

    scheme_wrong_type(scheme_symbol_val(argv[0]),
		      SCHEME_STR_VAL(argv[1]),
		      SCHEME_INT_VAL(argv[2]),
		      argc - 3, args);
  }

  return NULL;
}

static Scheme_Object *raise_mismatch_error(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_SYMBOLP(argv[0]))
    scheme_wrong_type("raise-mismatch-error", "symbol", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("raise-mismatch-error", "string", 1, argc, argv);

  scheme_arg_mismatch(scheme_symbol_val(argv[0]),
		      SCHEME_STR_VAL(argv[1]),
		      argv[2]);

  return NULL;
}

static Scheme_Object *good_print_width(int c, Scheme_Object **argv)
{
  return ((SCHEME_INTP(argv[0]) || (SCHEME_BIGNUMP(argv[0])))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *error_print_width(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-print-width", 
			     scheme_make_integer(MZCONFIG_ERROR_PRINT_WIDTH),
			     argc, argv,
			     -1, good_print_width, "integer greater than three", 0);
}

static Scheme_Object *error_print_srcloc(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-print-source-location", 
			     scheme_make_integer(MZCONFIG_ERROR_PRINT_SRCLOC),
			     argc, argv,
			     -1, NULL, NULL, 1);
}

static Scheme_Object *
def_error_display_proc(int argc, Scheme_Object *argv[])
{
  Scheme_Config *config = scheme_config;
  Scheme_Object *port = scheme_get_param(config, MZCONFIG_ERROR_PORT);

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("default-error-display-handler", "string", 0, argc, argv);
  /* don't care about argv[1] */

  scheme_write_string(SCHEME_STR_VAL(argv[0]), 
		      SCHEME_STRTAG_VAL(argv[0]),
		      port);
  scheme_write_string("\n", 1, port);

  return scheme_void;
}

static Scheme_Object *
def_error_value_string_proc(int argc, Scheme_Object *argv[])
{
  long origl, len, l;
  char *s;
  Scheme_Object *pph;

  if (!SCHEME_INTP(argv[1]))
    scheme_wrong_type("default-error-value->string-handler", "number", 1, argc, argv);

  origl = len = SCHEME_INT_VAL(argv[1]);

  pph = scheme_get_param(scheme_config, MZCONFIG_PORT_PRINT_HANDLER);
  if (SAME_OBJ(pph, scheme_default_global_print_handler)) {
    if (len < 3)
      len = 3;
    
    s = scheme_write_to_string_w_max(argv[0], &l, len);
    
    if ((origl < 3) && (l > origl))
      l = origl;
  } else {
    Scheme_Object *a[2];

    a[0] = argv[0];
    a[1] = scheme_make_string_output_port();
    _scheme_apply(pph, 2, a);
    
    s = scheme_get_sized_string_output(a[1], &l);

    if (l > origl) {
      l = origl;
      if (origl >= 1) {
	s[origl - 1] = '.';
	if (origl >= 2) {
	  s[origl - 2] = '.';
	  if (origl >= 3)
	    s[origl - 3] = '.';
	}
      }
    }
  }

  return scheme_make_sized_string(s, l, 0);
}

static Scheme_Object *
def_error_escape_proc(int argc, Scheme_Object *argv[])
{
  scheme_longjmp(scheme_error_buf, 1);

  return scheme_void; /* Never get here */
}

static Scheme_Object *
error_display_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-display-handler", 
			     scheme_make_integer(MZCONFIG_ERROR_DISPLAY_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *
error_value_string_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-value->string-handler", 
			     scheme_make_integer(MZCONFIG_ERROR_PRINT_VALUE_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *
error_escape_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("error-escape-handler",
			     scheme_make_integer(MZCONFIG_ERROR_ESCAPE_HANDLER),
			     argc, argv,
			     0, NULL, NULL, 0);
}

static Scheme_Object *
exit_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("exit-handler", 
			     scheme_make_integer(MZCONFIG_EXIT_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

int scheme_exiting_result; /* used by hack in port.c */

static Scheme_Object *
def_exit_handler_proc(int argc, Scheme_Object *argv[])
{
  long status;

  if (SCHEME_INTP(argv[0])) {
    status = SCHEME_INT_VAL(argv[0]);
    if (status < 1 || status > 255)
      status = 0;
  } else
    status = 0;

  scheme_exiting_result = status;

  if (scheme_exit)
    scheme_exit(status);
  else
    exit(status);

  return scheme_void;
}

Scheme_Object *
scheme_do_exit(int argc, Scheme_Object *argv[])
{
  long status;
  Scheme_Object *handler;

  if (argc == 1) {
    if (SCHEME_INTP(argv[0]))
      status = SCHEME_INT_VAL(argv[0]);
    else
      status = 0;
  } else
    status = 0;

  handler = scheme_get_param(scheme_config, MZCONFIG_EXIT_HANDLER);

  if (handler) {
    Scheme_Object *p[1];

    p[0] = argc ? argv[0] : scheme_make_integer(status);
    scheme_apply_multi(handler, 1, p);
  } else if (scheme_exit)
    scheme_exit(status);
  else
    exit(status);

  return scheme_void;
}

/***********************************************************************/

void
scheme_raise_exn(int id, ...)
{
  va_list args;
  long alen;
  char *msg;
  int i, c;
  Scheme_Object *eargs[MZEXN_MAXARGS];
  char *buffer;

  /* Precise GC: Don't allocate before getting hidden args off stack */
  buffer = prepared_buf;

  va_start(args, id);

  if (id == MZEXN_OTHER)
    c = 3;
  else
    c = exn_table[id].args;

  for (i = 2; i < c; i++) {
    eargs[i] = va_arg(args, Scheme_Object*);
  }

  msg = va_arg(args, char*);

  alen = sch_vsprintf(buffer, prepared_buf_len, msg, args);
  va_end(args);

  prepared_buf = init_buf(NULL, &prepared_buf_len);

#ifndef NO_SCHEME_EXNS
  eargs[0] = scheme_make_immutable_sized_string(buffer, alen, 1);
  eargs[1] = scheme_void;

  do_raise(scheme_make_struct_instance(exn_table[id].type, 
				       c, eargs),
	   0, 1);
#else
  call_error(buffer, alen, scheme_false);
#endif
}

#ifndef NO_SCHEME_EXNS

static Scheme_Object *
def_exn_handler(int argc, Scheme_Object *argv[])
{
  char *s;
  int len = -1;

  if (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_structure_type)
      && scheme_is_struct_instance(exn_table[MZEXN].type, argv[0])) {
    Scheme_Object *str = ((Scheme_Structure *)argv[0])->slots[0];
    if (SCHEME_STRINGP(str)) {
      s = SCHEME_STR_VAL(str);
      len = SCHEME_STRTAG_VAL(str);
    } else
      s = "exception raised [message field is not a string]";
  } else {
    char *v;

    v = scheme_make_provided_string(argv[0], 1, &len);
    s = scheme_malloc_atomic(len + 21);
    memcpy(s, "uncaught exception: ", 20);
    memcpy(s + 20, v, len + 1);
    len += 20;
  }

  call_error(s, len, argv[0]);

  return scheme_void;
}

static Scheme_Object *
exn_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-exception-handler", 
			     scheme_make_integer(MZCONFIG_EXN_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *
init_exn_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("initial-exception-handler", 
			     scheme_make_integer(MZCONFIG_INIT_EXN_HANDLER),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static void pre_raise(void *v)
{
  scheme_current_thread->exn_raised = 1;
}
	
static Scheme_Object *now_do_raise(void *v)
{
  Scheme_Object *p[1];

  p[0] = (Scheme_Object *)v;

  return scheme_apply(scheme_get_param(scheme_config, MZCONFIG_EXN_HANDLER), 
		      1, (Scheme_Object **)p);
}

static void post_raise_or_debug(void *v)
{
  scheme_current_thread->exn_raised = 0;
}
	
static Scheme_Object *
do_raise(Scheme_Object *arg, int return_ok, int need_debug)
{
 Scheme_Object *v;

 if (scheme_current_thread->error_invoked) {
   char *s;
   long slen = -1;
   if (SAME_TYPE(SCHEME_TYPE(arg), scheme_structure_type)
       && scheme_is_struct_instance(exn_table[MZEXN].type, arg)) {
     Scheme_Object *str = ((Scheme_Structure *)arg)->slots[0];
     if (SCHEME_STRINGP(str)) {
       char *msg, *prefix = "exception raised: ";
       long len, clen;
       clen = strlen(prefix);
       msg = SCHEME_STR_VAL(str);
       len = SCHEME_STRLEN_VAL(str);
       s = (char *)scheme_malloc_atomic(len + clen);
       memcpy(s, prefix, clen);
       memcpy(s + clen, msg, len);
       slen = clen + len;
     } else
       s = "exception raised [message field is not a string]";
   } else
     s = "raise called (with non-exception value)";
   call_error(s, slen, arg);
 }

 if (scheme_current_thread->exn_raised) {
   long len, mlen = -1, blen;
   char *buffer, *msg, *raisetype;

   buffer = init_buf(&len, &blen);

   if (SAME_TYPE(SCHEME_TYPE(arg), scheme_structure_type)
       && scheme_is_struct_instance(exn_table[MZEXN].type, arg)) {
     Scheme_Object *str = ((Scheme_Structure *)arg)->slots[0];
     raisetype = "exception raised";
     if (SCHEME_STRINGP(str)) {
       msg = SCHEME_STR_VAL(str);
       mlen = SCHEME_STRLEN_VAL(str);
     } else
       msg = "[exception message field is not a string]";
   } else {
     msg = error_write_to_string_w_max(arg, len, NULL);
     raisetype = "raise called (with non-exception value)";
   }

   blen = scheme_sprintf(buffer, blen, "%s by %s: %t",
			 raisetype,
			 (scheme_current_thread->exn_raised < 2) 
			 ? "exception handler"
			 : "debug info handler",
			 msg, mlen);

   call_error(buffer, blen, scheme_false);

   return scheme_void;
 }

 if (need_debug) {
   Scheme_Object *marks;
   marks = scheme_current_continuation_marks();
   ((Scheme_Structure *)arg)->slots[1] = marks;
 }

 v = scheme_dynamic_wind(pre_raise, now_do_raise,
			 post_raise_or_debug, NULL,
			 (void *)arg);

 if (return_ok)
   return v;

 call_error(RAISE_RETURNED, -1, scheme_false);

 return scheme_void;
}

static Scheme_Object *
sch_raise(int argc, Scheme_Object *argv[])
{
  return do_raise(argv[0], 0, 0);
}

void scheme_raise(Scheme_Object *exn)
{
  do_raise(exn, 0, 0);
}

void scheme_init_exn(Scheme_Env *env)
{
  int i, j;
  Scheme_Object *tmpo, **tmpop;

#define _MZEXN_DECL_FIELDS
# include "schexn.h"
#undef _MZEXN_DECL_FIELDS

  REGISTER_SO(exn_table);

#ifdef MEMORY_COUNTING_ON
# ifndef GLOBAL_EXN_TABLE
  scheme_misc_count += (sizeof(exn_rec) * MZEXN_OTHER);
# endif
#endif

#define _MZEXN_PRESETUP
# include "schexn.h"
#undef _MZEXN_PRESETUP

#define EXN_PARENT(id) exn_table[id].type

#define EXN_FLAGS SCHEME_STRUCT_EXPTIME

#define SETUP_STRUCT(id, parent, name, argc, args) \
    { tmpo = scheme_make_struct_type_from_string(name, parent, argc); \
      exn_table[id].type = tmpo; \
      tmpop = scheme_make_struct_names_from_array(name, argc, args, EXN_FLAGS, &exn_table[id].count); \
      exn_table[id].names = tmpop; }
  
#define EXNCONS scheme_make_pair
#define _MZEXN_SETUP
#include "schexn.h"

  for (i = 0; i < MZEXN_OTHER; i++) {
    if (exn_table[i].count) {
      Scheme_Object **values, *et;
      int sp;

      values = scheme_make_struct_values(exn_table[i].type,
					 exn_table[i].names,
					 exn_table[i].count,
					 EXN_FLAGS);
      for (j = exn_table[i].count - 1; j--; ) {
	scheme_add_global_constant_symbol(exn_table[i].names[j],
					  values[j],
					  env);
      }

      sp = exn_table[i].super_pos;
      et = scheme_make_struct_exptime(exn_table[i].names, exn_table[i].count,
				      (sp >= 0) ? exn_table[sp].exptime : NULL,
				      EXN_FLAGS);
      exn_table[i].exptime = et;
      scheme_add_global_keyword_symbol(exn_table[i].names[exn_table[i].count - 1], et, env);
    }
  }

  scheme_add_global_constant("current-exception-handler", 
			     scheme_register_parameter(exn_handler, 
						       "current-exception-handler",
						       MZCONFIG_EXN_HANDLER), 
			     env);

  scheme_add_global_constant("initial-exception-handler", 
			     scheme_register_parameter(init_exn_handler, 
						       "initial-exception-handler",
						       MZCONFIG_INIT_EXN_HANDLER), 
			     env);

  scheme_add_global_constant("raise", 
			     scheme_make_prim_w_arity(sch_raise, 
						      "raise", 
						      1, 1), 
			     env);

  scheme_init_exn_config();
}

void scheme_init_exn_config(void)
{
  Scheme_Config *config = scheme_config;
  Scheme_Object *h;

  h = scheme_make_prim_w_arity(def_exn_handler,
			       "default-exception-handler",
			       1, 1);
  
  scheme_set_param(config, MZCONFIG_EXN_HANDLER, h);
  scheme_set_param(config, MZCONFIG_INIT_EXN_HANDLER, h);
}

#endif
