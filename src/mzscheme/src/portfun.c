/*
  MzScheme
  Copyright (c) 2004 PLT Scheme, Inc.
  Copyright (c) 2000-2001 Matthew Flatt

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

/* This file implements the least platform-specific aspects of MzScheme
   port types. */

#include "schpriv.h"

static Scheme_Object *input_port_p (int, Scheme_Object *[]);
static Scheme_Object *output_port_p (int, Scheme_Object *[]);
static Scheme_Object *current_input_port (int, Scheme_Object *[]);
static Scheme_Object *current_output_port (int, Scheme_Object *[]);
static Scheme_Object *current_error_port (int, Scheme_Object *[]);
static Scheme_Object *make_input_port (int, Scheme_Object *[]);
static Scheme_Object *make_output_port (int, Scheme_Object *[]);
static Scheme_Object *open_input_file (int, Scheme_Object *[]);
static Scheme_Object *open_output_file (int, Scheme_Object *[]);
static Scheme_Object *open_input_output_file (int, Scheme_Object *[]);
static Scheme_Object *close_input_port (int, Scheme_Object *[]);
static Scheme_Object *close_output_port (int, Scheme_Object *[]);
static Scheme_Object *call_with_output_file (int, Scheme_Object *[]);
static Scheme_Object *call_with_input_file (int, Scheme_Object *[]);
static Scheme_Object *with_input_from_file (int, Scheme_Object *[]);
static Scheme_Object *with_output_to_file (int, Scheme_Object *[]);
static Scheme_Object *read_f (int, Scheme_Object *[]);
static Scheme_Object *read_syntax_f (int, Scheme_Object *[]);
static Scheme_Object *read_char (int, Scheme_Object *[]);
static Scheme_Object *read_char_spec (int, Scheme_Object *[]);
static Scheme_Object *read_line (int, Scheme_Object *[]);
static Scheme_Object *sch_read_string (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_string (int, Scheme_Object *[]);
static Scheme_Object *read_string_bang (int, Scheme_Object *[]);
static Scheme_Object *read_string_bang_nonblock (int, Scheme_Object *[]);
static Scheme_Object *read_string_bang_break (int, Scheme_Object *[]);
static Scheme_Object *peek_string_bang (int, Scheme_Object *[]);
static Scheme_Object *peek_string_bang_nonblock (int, Scheme_Object *[]);
static Scheme_Object *peek_string_bang_break (int, Scheme_Object *[]);
static Scheme_Object *write_string_avail(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_string_avail_nonblock(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_string_avail_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *peek_char (int, Scheme_Object *[]);
static Scheme_Object *peek_char_spec (int, Scheme_Object *[]);
static Scheme_Object *eof_object_p (int, Scheme_Object *[]);
static Scheme_Object *char_ready_p (int, Scheme_Object *[]);
static Scheme_Object *sch_write (int, Scheme_Object *[]);
static Scheme_Object *display (int, Scheme_Object *[]);
static Scheme_Object *sch_print (int, Scheme_Object *[]);
static Scheme_Object *newline (int, Scheme_Object *[]);
static Scheme_Object *write_char (int, Scheme_Object *[]);
static Scheme_Object *load (int, Scheme_Object *[]);
static Scheme_Object *current_load (int, Scheme_Object *[]);
static Scheme_Object *current_load_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *default_load (int, Scheme_Object *[]);
static Scheme_Object *use_compiled_kind(int, Scheme_Object *[]);
static Scheme_Object *transcript_on(int, Scheme_Object *[]);
static Scheme_Object *transcript_off(int, Scheme_Object *[]);
static Scheme_Object *flush_output (int, Scheme_Object *[]);
static Scheme_Object *open_input_string (int, Scheme_Object *[]);
static Scheme_Object *open_output_string (int, Scheme_Object *[]);
static Scheme_Object *get_output_string (int, Scheme_Object *[]);
static Scheme_Object *sch_pipe(int, Scheme_Object **args);
static Scheme_Object *port_read_handler(int, Scheme_Object **args);
static Scheme_Object *port_display_handler(int, Scheme_Object **args);
static Scheme_Object *port_write_handler(int, Scheme_Object **args);
static Scheme_Object *port_print_handler(int, Scheme_Object **args);
static Scheme_Object *global_port_print_handler(int, Scheme_Object **args);
static Scheme_Object *global_port_count_lines(int, Scheme_Object **args);
static Scheme_Object *port_count_lines(int, Scheme_Object **args);
static Scheme_Object *port_next_location(int, Scheme_Object **args);

static Scheme_Object *sch_default_read_handler(void *ignore, int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[]);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

static Scheme_Object *any_symbol, *any_one_symbol;
static Scheme_Object *cr_symbol, *lf_symbol, *crlf_symbol;

static Scheme_Object *all_symbol, *none_symbol;

static Scheme_Object *module_symbol;

static Scheme_Object *default_read_handler;
static Scheme_Object *default_display_handler;
static Scheme_Object *default_write_handler;
static Scheme_Object *default_print_handler;

Scheme_Object *scheme_default_global_print_handler;

Scheme_Object *scheme_write_proc, *scheme_display_proc, *scheme_print_proc;

static mzshort drh_cases[4] = { 1, 1, 3, 3};

#define fail_err_symbol scheme_false

/*========================================================================*/
/*                             initialization                             */
/*========================================================================*/

void 
scheme_init_port_fun(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  REGISTER_SO(default_read_handler);
  REGISTER_SO(default_display_handler);
  REGISTER_SO(default_write_handler);
  REGISTER_SO(default_print_handler);
  
  REGISTER_SO(scheme_write_proc);
  REGISTER_SO(scheme_display_proc);
  REGISTER_SO(scheme_print_proc);
  
  REGISTER_SO(any_symbol);
  REGISTER_SO(any_one_symbol);
  REGISTER_SO(cr_symbol);
  REGISTER_SO(lf_symbol);
  REGISTER_SO(crlf_symbol);
  
  any_symbol = scheme_intern_symbol("any");
  any_one_symbol = scheme_intern_symbol("any-one");
  cr_symbol = scheme_intern_symbol("return");
  lf_symbol = scheme_intern_symbol("linefeed");
  crlf_symbol = scheme_intern_symbol("return-linefeed");

  REGISTER_SO(all_symbol);
  REGISTER_SO(none_symbol);
  
  all_symbol = scheme_intern_symbol("all");
  none_symbol = scheme_intern_symbol("none");
  
  REGISTER_SO(module_symbol);
  
  module_symbol = scheme_intern_symbol("module");
  
  scheme_write_proc = scheme_make_prim_w_arity(sch_write, 
					       "write", 
					       1, 2);
  scheme_display_proc = scheme_make_prim_w_arity(display, 
						 "display", 
						 1, 2);
  scheme_print_proc = scheme_make_prim_w_arity(sch_print, 
					       "print", 
					       1, 2);
  
  /* Made as a closed prim so we can get the arity right: */
  default_read_handler = scheme_make_closed_prim_w_arity(sch_default_read_handler,
							 NULL,
							 "default-port-read-handler", 
							 1, 3);
  /* Fixup arity: */
  {
    Scheme_Closed_Case_Primitive_Proc *c;
    c = MALLOC_ONE_TAGGED(Scheme_Closed_Case_Primitive_Proc);
    memcpy(c, default_read_handler, sizeof(Scheme_Closed_Primitive_Proc));
    c->p.mina = -2;
    c->p.maxa = -2;
    c->cases = drh_cases;
    default_read_handler = (Scheme_Object *)c;
  }
    

  default_display_handler = scheme_make_prim_w_arity(sch_default_display_handler,
						     "default-port-display-handler", 
						     2, 2);
  default_write_handler = scheme_make_prim_w_arity(sch_default_write_handler,
						   "default-port-write-handler", 
						   2, 2);
  default_print_handler = scheme_make_prim_w_arity(sch_default_print_handler,
						   "default-port-print-handler", 
						   2, 2);
  
  scheme_init_port_fun_config();

  scheme_add_global_constant("eof", scheme_eof, env);

  scheme_add_global_constant("input-port?", 
			     scheme_make_folding_prim(input_port_p, 
						      "input-port?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("output-port?", 
			     scheme_make_folding_prim(output_port_p, 
						      "output-port?", 
						      1, 1, 1), 
			     env);
  
  scheme_add_global_constant("file-stream-port?", 
			     scheme_make_folding_prim(scheme_file_stream_port_p, 
						      "file-stream-port?", 
						      1, 1, 1), 
			     env);
  
  scheme_add_global_constant("current-input-port", 
			     scheme_register_parameter(current_input_port,
						       "current-input-port",
						       MZCONFIG_INPUT_PORT),
			     env);
  scheme_add_global_constant("current-output-port", 
			     scheme_register_parameter(current_output_port,
						       "current-output-port",
						       MZCONFIG_OUTPUT_PORT),
			     env);
  scheme_add_global_constant("current-error-port", 
			     scheme_register_parameter(current_error_port, 
						       "current-error-port",
						       MZCONFIG_ERROR_PORT),
			     env);
  
  scheme_add_global_constant("open-input-file", 
			     scheme_make_prim_w_arity(open_input_file, 
						      "open-input-file", 
						      1, 2), 
			     env);
  scheme_add_global_constant("open-input-string", 
			     scheme_make_prim_w_arity(open_input_string, 
						      "open-input-string", 
						      1, 1), 
			     env);
  scheme_add_global_constant("open-output-file", 
			     scheme_make_prim_w_arity(open_output_file,
						      "open-output-file",
						      1, 3), 
			     env);
  scheme_add_global_constant("open-output-string", 
			     scheme_make_prim_w_arity(open_output_string,
						      "open-output-string", 
						      0, 0),
			     env);
  scheme_add_global_constant("get-output-string", 
			     scheme_make_prim_w_arity(get_output_string,
						      "get-output-string",
						      1, 1),
			     env);
  scheme_add_global_constant("open-input-output-file", 
			     scheme_make_prim_w_arity(open_input_output_file,
						      "open-input-output-file",
						      1, 3),
			     env);
  scheme_add_global_constant("close-input-port", 
			     scheme_make_prim_w_arity(close_input_port,
						      "close-input-port", 
						      1, 1), 
			     env);
  scheme_add_global_constant("close-output-port", 
			     scheme_make_prim_w_arity(close_output_port, 
						      "close-output-port", 
						      1, 1), 
			     env);
  scheme_add_global_constant("call-with-output-file",
			     scheme_make_prim_w_arity2(call_with_output_file,
						       "call-with-output-file",
						       2, 4,
						       0, -1),
			     env);
  scheme_add_global_constant("call-with-input-file",
			     scheme_make_prim_w_arity2(call_with_input_file,
						       "call-with-input-file",
						       2, 3,
						       0, -1),
			     env);
  scheme_add_global_constant("with-output-to-file",
			     scheme_make_prim_w_arity2(with_output_to_file,
						       "with-output-to-file",
						       2, 4,
						       0, -1),
			     env);
  scheme_add_global_constant("with-input-from-file",
			     scheme_make_prim_w_arity2(with_input_from_file,
						       "with-input-from-file",
						       2, 3,
						       0, -1),
			     env);
  scheme_add_global_constant("make-custom-input-port", 
			     scheme_make_prim_w_arity(make_input_port, 
						      "make-custom-input-port", 
						      3, 3), 
			     env);
  scheme_add_global_constant("make-custom-output-port", 
			     scheme_make_prim_w_arity(make_output_port, 
						      "make-custom-output-port", 
						      4, 4), 
			     env);
  
  scheme_add_global_constant("read", 
			     scheme_make_prim_w_arity(read_f,
						      "read", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-syntax", 
			     scheme_make_prim_w_arity(read_syntax_f,
						      "read-syntax", 
						      1, 3), 
			     env);
  scheme_add_global_constant("read-char", 
			     scheme_make_prim_w_arity(read_char, 
						      "read-char", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-char-or-special", 
			     scheme_make_prim_w_arity(read_char_spec, 
						      "read-char-or-special", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-line", 
			     scheme_make_prim_w_arity(read_line, 
						      "read-line", 
						      0, 2), 
			     env);
  scheme_add_global_constant("read-string", 
			     scheme_make_prim_w_arity(sch_read_string, 
						      "read-string", 
						      1, 2), 
			     env);
  scheme_add_global_constant("peek-string", 
			     scheme_make_prim_w_arity(sch_peek_string, 
						      "peek-string", 
						      2, 3), 
			     env);
  scheme_add_global_constant("read-string-avail!", 
			     scheme_make_prim_w_arity(read_string_bang, 
						      "read-string-avail!", 
						      1, 4), 
			     env);
  scheme_add_global_constant("read-string-avail!*", 
			     scheme_make_prim_w_arity(read_string_bang_nonblock, 
						      "read-string-avail!*", 
						      1, 4), 
			     env);
  scheme_add_global_constant("read-string-avail!/enable-break", 
			     scheme_make_prim_w_arity(read_string_bang_break, 
						      "read-string-avail!/enable-break", 
						      1, 4), 
			     env);
  scheme_add_global_constant("peek-string-avail!", 
			     scheme_make_prim_w_arity(peek_string_bang, 
						      "peek-string-avail!", 
						      2, 5), 
			     env);
  scheme_add_global_constant("peek-string-avail!*", 
			     scheme_make_prim_w_arity(peek_string_bang_nonblock, 
						      "peek-string-avail!*", 
						      2, 5), 
			     env);
  scheme_add_global_constant("peek-string-avail!/enable-break", 
			     scheme_make_prim_w_arity(peek_string_bang_break, 
						      "peek-string-avail!/enable-break", 
						      2, 5), 
			     env);
  scheme_add_global_constant("write-string-avail", 
			     scheme_make_prim_w_arity(write_string_avail, 
						      "write-string-avail", 
						      1, 4),
			     env);
  scheme_add_global_constant("write-string-avail*", 
			     scheme_make_prim_w_arity(write_string_avail_nonblock, 
						      "write-string-avail*", 
						      1, 4),
			     env);
  scheme_add_global_constant("write-string-avail/enable-break",
			     scheme_make_prim_w_arity(write_string_avail_break, 
						      "write-string-avail/enable-break", 
						      1, 4),
			     env);
  scheme_add_global_constant("peek-char", 
			     scheme_make_prim_w_arity(peek_char, 
						      "peek-char", 
						      0, 2), 
			     env);
  scheme_add_global_constant("peek-char-or-special", 
			     scheme_make_prim_w_arity(peek_char_spec, 
						      "peek-char-or-special", 
						      0, 2), 
			     env);
  scheme_add_global_constant("eof-object?", 
			     scheme_make_folding_prim(eof_object_p, 
						      "eof-object?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("char-ready?", 
			     scheme_make_prim_w_arity(char_ready_p, 
						      "char-ready?", 
						      0, 1), 
			     env);
  scheme_add_global_constant("write", scheme_write_proc, env);
  scheme_add_global_constant("display", scheme_display_proc, env);
  scheme_add_global_constant("print", scheme_print_proc, env);
  scheme_add_global_constant("newline", 
			     scheme_make_prim_w_arity(newline, 
						      "newline", 
						      0, 1), 
			     env);
  scheme_add_global_constant("write-char", 
			     scheme_make_prim_w_arity(write_char, 
						      "write-char", 
						      1, 2), 
			     env);
  
  scheme_add_global_constant("port-read-handler", 
			     scheme_make_prim_w_arity(port_read_handler, 
						      "port-read-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("port-display-handler", 
			     scheme_make_prim_w_arity(port_display_handler, 
						      "port-display-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("port-write-handler", 
			     scheme_make_prim_w_arity(port_write_handler, 
						      "port-write-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("port-print-handler", 
			     scheme_make_prim_w_arity(port_print_handler, 
						      "port-print-handler", 
						      1, 2), 
			     env);
  scheme_add_global_constant("global-port-print-handler",
			     scheme_register_parameter(global_port_print_handler,
						       "global-port-print-handler",
						       MZCONFIG_PORT_PRINT_HANDLER),
			     env);
  
  scheme_add_global_constant("load", 
			     scheme_make_prim_w_arity2(load, 
						       "load", 
						       1, 1,
						       0, -1),
			      env);
  scheme_add_global_constant("current-load", 
			     scheme_register_parameter(current_load, 
						       "current-load",
						       MZCONFIG_LOAD_HANDLER), 
			     env);
  scheme_add_global_constant("current-load-relative-directory", 
			     scheme_register_parameter(current_load_directory, 
						       "current-load-relative-directory",
						       MZCONFIG_LOAD_DIRECTORY), 
			     env);

  scheme_add_global_constant("use-compiled-file-kinds",
			     scheme_register_parameter(use_compiled_kind,
						       "use-compiled-file-kinds",
						       MZCONFIG_USE_COMPILED_KIND),
			     env);

  scheme_add_global_constant ("transcript-on", 
			      scheme_make_prim_w_arity(transcript_on,
						       "transcript-on", 
						       1, 1),
			      env);
  scheme_add_global_constant ("transcript-off", 
			      scheme_make_prim_w_arity(transcript_off,
						       "transcript-off", 
						       0, 0),
			      env);
  
  scheme_add_global_constant("flush-output", 
			     scheme_make_prim_w_arity(flush_output, 
						      "flush-output", 
						      0, 1), 
			     env);
  scheme_add_global_constant("file-position", 
			     scheme_make_prim_w_arity(scheme_file_position, 
						      "file-position", 
						      1, 2), 
			     env);
  scheme_add_global_constant("file-stream-buffer-mode", 
			     scheme_make_prim_w_arity(scheme_file_buffer, 
						      "file-stream-buffer-mode", 
						      1, 2), 
			     env);
  scheme_add_global_constant("port-file-identity", 
			     scheme_make_prim_w_arity(scheme_file_identity, 
						      "port-file-identity", 
						      1, 1), 
			     env);
  
  scheme_add_global_constant("make-pipe", 
			     scheme_make_prim_w_arity2(sch_pipe, 
						       "make-pipe", 
						       0, 1,
						       2, 2), 
			     env);

  scheme_add_global_constant("port-count-lines!", 
			     scheme_make_prim_w_arity(port_count_lines, 
						      "port-count-lines!", 
						      1, 1),
			     env);
  scheme_add_global_constant("port-next-location", 
			     scheme_make_prim_w_arity2(port_next_location, 
						       "port-next-location", 
						       1, 1, 
						       3, 3),
			     env);
  scheme_add_global_constant("port-count-lines-enabled",
			     scheme_register_parameter(global_port_count_lines,
						       "port-count-lines-enabled",
						       MZCONFIG_PORT_COUNT_LINES),
			     env);
}


void scheme_init_port_fun_config(void)
{
  Scheme_Config *config = scheme_config;
  scheme_set_param(config, MZCONFIG_LOAD_DIRECTORY, scheme_false);
  scheme_set_param(config, MZCONFIG_USE_COMPILED_KIND, all_symbol);

  {
    Scheme_Object *dlh;
    dlh = scheme_make_prim_w_arity2(default_load,
				    "default-load-handler",
				    2, 2,
				    0, -1);
    scheme_set_param(config, MZCONFIG_LOAD_HANDLER, dlh);
  }

  REGISTER_SO(scheme_default_global_print_handler);
  scheme_default_global_print_handler 
    = scheme_make_prim_w_arity(sch_default_global_port_print_handler,
			       "default-global-port-print-handler",
			       2, 2);
  scheme_set_param(config, 
		   MZCONFIG_PORT_PRINT_HANDLER, 
		   scheme_default_global_print_handler);
}

/*========================================================================*/
/*                          string input ports                            */
/*========================================================================*/

static long 
string_get_or_peek_string(Scheme_Input_Port *port, 
			  char *buffer, long offset, long size,
			  int peek, long skip)
{
  Scheme_Indexed_String *is;

  is = (Scheme_Indexed_String *) port->port_data;
  if (is->index + skip >= is->size)
    return EOF;
  else {
    long l, delta;

    delta = is->index + skip;

    if (delta + size <= is->size)
      l = size;
    else
      l = (is->size - delta);
      
    memcpy(buffer + offset, is->string + delta, l);
    if (!peek)
      is->index += l;
    
    return l;
  }
}

static long 
string_get_string(Scheme_Input_Port *port, 
		  char *buffer, long offset, long size,
		  int nonblock)
{
  return string_get_or_peek_string(port, buffer, offset, size, 0, 0);
}

static long 
string_peek_string(Scheme_Input_Port *port, 
		   char *buffer, long offset, long size,
		   Scheme_Object *sskip,
		   int nonblock)
{
  long skip;

  if (SCHEME_INTP(sskip))
    skip = SCHEME_INT_VAL(sskip);
  else
    skip = ((Scheme_Indexed_String *)port->port_data)->size;

  return string_get_or_peek_string(port, buffer, offset, size, 1, skip);
}

static int
string_char_ready (Scheme_Input_Port *port)
{
  return 1;
}

static void
string_close_in (Scheme_Input_Port *port)
{
}

static Scheme_Indexed_String *
make_indexed_string (const char *str, long len)
{
  Scheme_Indexed_String *is;

  is = MALLOC_ONE_RT(Scheme_Indexed_String);
#ifdef MZTAG_REQUIRED
  is->type = scheme_rt_indexed_string;
#endif

  if (str) {
    if (len < 0) {
      is->string = (char *)str;
      is->size = -len;
    } else {
      char *ca;
      ca = (char *)scheme_malloc_atomic(len);
      is->string = ca;
      memcpy(is->string, str, len);
      is->size = len;
    }
  } else {
    char *ca;
    is->size = 100;
    ca = (char *)scheme_malloc_atomic(is->size + 1);
    is->string = ca;
  }
  is->index = 0;
  return (is);
}

Scheme_Object *
scheme_make_sized_string_input_port(const char *str, long len)
{
  Scheme_Input_Port *ip;

  ip = _scheme_make_input_port(scheme_string_input_port_type,
			       make_indexed_string(str, len),
			       string_get_string,
			       string_peek_string,
			       string_char_ready,
			       string_close_in,
			       NULL,
			       0);

  ip->name = "STRING";

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_string_input_port(const char *str)
{
  return scheme_make_sized_string_input_port(str, strlen(str));
}

/*========================================================================*/
/*                          string output ports                           */
/*========================================================================*/

static long
string_write_string(Scheme_Output_Port *port, 
		    const char *str, long d, long len, 
		    int rarely_block)
{
  Scheme_Indexed_String *is;

  is = (Scheme_Indexed_String *) port->port_data;

  if (is->index + len >= is->size) {
    char *old;

    old = is->string;

    if (len > is->size)
      is->size += 2 * len;
    else
      is->size *= 2;

    {
      char *ca;
      ca = (char *)scheme_malloc_atomic(is->size + 1);
      is->string = ca;
    }
    memcpy(is->string, old, is->index);
  }
  
  memcpy(is->string + is->index, str + d, len);
  is->index += len;

  return len;
}

static void
string_close_out (Scheme_Output_Port *port)
{
  return;
}

Scheme_Object *
scheme_make_string_output_port (void)
{
  Scheme_Output_Port *op;

  op = scheme_make_output_port (scheme_string_output_port_type,
				make_indexed_string(NULL, 0),
				string_write_string,
				NULL,
				string_close_out,
				NULL,
				0);

  return (Scheme_Object *)op;
}

char *
scheme_get_sized_string_output(Scheme_Object *port, long *size)
{
  Scheme_Output_Port *op;
  Scheme_Indexed_String *is;
  char *v;
  long len;

  if (!SCHEME_OUTPORTP(port))
    return NULL;

  op = (Scheme_Output_Port *)port;
  if (op->sub_type != scheme_string_output_port_type)
    return NULL;

  is = (Scheme_Indexed_String *)op->port_data;

  len = is->index;
  if (is->u.hot > len)
    len = is->u.hot;

  v = (char *)scheme_malloc_atomic(len + 1);
  memcpy(v, is->string, len);
  v[len] = 0;
  
  if (size)
    *size = len;

  return v;
}

char *
scheme_get_string_output(Scheme_Object *port)
{
  return scheme_get_sized_string_output(port, NULL);
}

/*========================================================================*/
/*                 "user" input ports (created from Scheme)               */
/*========================================================================*/

typedef struct User_Input_Port {
  MZTAG_IF_REQUIRED
  Scheme_Object *waitable;
  Scheme_Object *read_proc;
  Scheme_Object *peek_proc;
  Scheme_Object *close_proc;
  Scheme_Object *peeked;
  Scheme_Object *closed_sema;
  Scheme_Object *reuse_str;
} User_Input_Port;

#define MAX_USER_INPUT_REUSE_SIZE 1024

static long 
user_get_or_peek_string(Scheme_Input_Port *port, 
			char *buffer, long offset, long size,
			int nonblock,
			int peek, Scheme_Object *peek_skip,
			Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *fun, *val, *a[3];
  User_Input_Port *uip = (User_Input_Port *)port->port_data;

 try_again:

  val = uip->peeked;
  if (val) {
    /* Leftover from a read-based peek used to implement `char-ready?'
       This can't happen is peek is 1, because in that case we have a
       peek_proc, so there's no need for read-based peeks. */
    uip->peeked = NULL;
    if (SCHEME_CHARP(val)) {
      buffer[offset] = SCHEME_CHAR_VAL(val);
      return 1;
    } else if (SCHEME_VOIDP(val)) {
      return SCHEME_SPECIAL;
    } else
      return EOF;
  }

  if (peek)
    fun = uip->peek_proc;
  else
    fun = uip->read_proc;

  if (uip->reuse_str && (size == SCHEME_STRLEN_VAL(uip->reuse_str))) {
    a[0] = uip->reuse_str;
    uip->reuse_str = NULL;
  } else {
    char *vb;
    vb = scheme_malloc_atomic(size + 1);
    a[0] = scheme_make_sized_string(vb, size, 0);
  }
  a[1] = peek_skip;
  val = scheme_apply(fun, peek ? 2 : 1, a);

  if (size <= MAX_USER_INPUT_REUSE_SIZE) {
    uip->reuse_str = a[0];
  }

  if (SCHEME_EOFP(val))
    return EOF;
  else {
    int n;

    if (!SCHEME_INTP(val) || (SCHEME_INT_VAL(val) < 0)) {
      if (SCHEME_BIGNUMP(val) && SCHEME_BIGPOS(val)) {
	n = -1;
      } else if (scheme_check_proc_arity(NULL, 4, 0, 1, &val)) {
	port->special = val;
	return SCHEME_SPECIAL;
      } else if (scheme_is_waitable(val)) {
	/* A peek/read failed, and we were given a waitable that unblocks
	   when the peek (at some offset) might succeed. */
	if (nonblock) {
	  if (sinfo) {
	    a[0] = val;
	    a[1] = uip->closed_sema;
	    val = scheme_make_waitable_set(2, a);
	    scheme_set_wait_target(sinfo, val, (Scheme_Object *)port, NULL, 0, 1);
	  }
	  return 0;
	} else {
	  /* Wait on the given waitable. */
	  a[0] = scheme_false;
	  a[1] = val;
	  a[2] = uip->closed_sema;
	  val = scheme_object_wait_multiple(3, a);
	  
	  if (!SAME_OBJ(val, uip->closed_sema)) {
	    /* Port may have been closed while we were waiting: */
	    if (port->closed) {
	      /* Another thread closed the input port while we were waiting. */
	      /* Call scheme_getc to signal the error */
	      scheme_getc((Scheme_Object *)port);
	    }
	    goto try_again;
	  } else {
	    /* Another thread closed the input port while we were waiting. */
	    /* Call scheme_peekc to signal the error */
	    scheme_peekc((Scheme_Object *)port);
	    return 0; /* doesn't get here */
	  }
	}
      } else {
	scheme_wrong_type(peek ? "user port peek-string" : "user port read-string", 
			  (peek 
			   ? "non-negative exact integer, eof, waitable, or procedure of arity 4"
			   : "non-negative exact integer, eof, or procedure of arity 4"), 
			  -1, -1, &val);
	return 0;
      }
    } else
      n = SCHEME_INT_VAL(val);

    if ((n < 0) || (n > SCHEME_STRLEN_VAL(a[0]))) {
      scheme_arg_mismatch(peek ? "user port peek-string" : "user port read-string",
			  "result integer is larger than the supplied string: ",
			  val);
    }

    if (n) {
      memcpy(buffer + offset, SCHEME_STR_VAL(a[0]), n);
      return n;
    } else {
      scheme_thread_block(0.0); /* penalty for inaccuracy? */
      /* but don't loop forever due to inaccurracy */
      if (nonblock) {
	if (sinfo)
	  sinfo->spin = 1;
	return 0;
      }
      goto try_again;
    }
  }
}

static long 
user_get_string(Scheme_Input_Port *port, 
		char *buffer, long offset, long size,
		int nonblock)
{
  return user_get_or_peek_string(port, buffer, offset, size, nonblock, 
				 0, NULL, NULL);
}

static long 
user_peek_string(Scheme_Input_Port *port, 
		 char *buffer, long offset, long size,
		 Scheme_Object *skip,
		 int nonblock)
{
  return user_get_or_peek_string(port, buffer, offset, size, nonblock, 1, skip, NULL);
}

static int
user_char_ready_sinfo(Scheme_Input_Port *port, Scheme_Schedule_Info *sinfo)
{
  int c, can_peek;
  char s[1];
  User_Input_Port *uip = (User_Input_Port *)port->port_data;

  /* We implement char-ready? by a non-blocking peek for a single
     character. If the port provides a precise waitable, it
     effectively determines the result, because the peek function
     checks the waitable. */

  can_peek = (uip->peek_proc ? 1 : 0);

  c = user_get_or_peek_string(port, s, 0, 1, 1, 
			      can_peek, scheme_make_integer(0),
			      sinfo);

  if (c == EOF) {
    if (!can_peek)
      uip->peeked = scheme_true;
    return 1;
  } else if (c) {
    if (!can_peek) {
      if (c == SCHEME_SPECIAL)
	uip->peeked = scheme_void;
      else
	uip->peeked = scheme_make_character(s[0]);
    }
    return 1;
  } else
    return 0;
}

static int
user_char_ready(Scheme_Input_Port *port)
{
  return user_char_ready_sinfo(port, NULL);
}

int scheme_user_port_char_probably_ready(Scheme_Input_Port *ip, Scheme_Schedule_Info *sinfo)
{
  User_Input_Port *uip = (User_Input_Port *)ip->port_data;

  if (uip->peeked)
    return 1;

  if (sinfo->false_positive_ok) {
    /* Causes the thread to swap in: */
    sinfo->potentially_false_positive = 1;
    return 1;
  } else {
    return user_char_ready_sinfo(ip, sinfo);
  }
}

static void
user_needs_wakeup_input (Scheme_Input_Port *port, void *fds)
{
  /* Nothing... */
}

static void
user_close_input(Scheme_Input_Port *port)
{
  User_Input_Port *uip = (User_Input_Port *)port->port_data;

  scheme_apply_multi(uip->close_proc, 0, NULL);

  /* Wake up anyone who's blocked on the port.  We rely on the fact
     that port_sema doesn't swap anything in (because port->closed
     isn't set, yet). */
  scheme_post_sema_all(uip->closed_sema);
}

/*========================================================================*/
/*                 "user" output ports (created from Scheme)              */
/*========================================================================*/

typedef struct User_Output_Port {
  MZTAG_IF_REQUIRED
  Scheme_Object *proc_for_waitable;
  Scheme_Object *write_proc;
  Scheme_Object *flush_proc;
  Scheme_Object *close_proc;
  Scheme_Object *closed_sema;
} User_Output_Port;

int scheme_user_port_write_probably_ready(Scheme_Output_Port *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *proc_for_waitable, *val;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  if (port->closed)
    return 1;

  proc_for_waitable = uop->proc_for_waitable;
  if (SCHEME_TRUEP(proc_for_waitable)) {
    if (sinfo->false_positive_ok) {
      sinfo->potentially_false_positive = 1;
      return 1;
    }
    
    val = scheme_apply(proc_for_waitable, 0, NULL);

    if (scheme_is_waitable(val)) {
      scheme_set_wait_target(sinfo, val, (Scheme_Object *)port, NULL, 0, 1);
      return 0;
    } else
      return 1; /* non-waitable => ready */
  }

  return 1;
}

static int
user_write_ready(Scheme_Output_Port *port)
{
  /* This function should never be called. If we are ready-checking as 
     a waitable, then scheme_user_port_write_probably_ready is called,
     instead. */
  return 1;
}

static long
user_write_string(Scheme_Output_Port *port, const char *str, long offset, long len, 
		  int rarely_block)
{
  /* As always, rarely_block => flush, !len => flush,
     rarely_block == 1 => len > 0 */
  Scheme_Object *p[4], *to_write = NULL, *val;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

 try_again:

  if (!len && !rarely_block) {
    /* Just a flush request */
    scheme_apply_multi(uop->flush_proc, 0, NULL);
    return 0;
  } else {
    if (!to_write)
      to_write = scheme_make_sized_offset_string((char *)str, offset, len, 1);
    p[0] = to_write;
    SCHEME_SET_IMMUTABLE(p[0]);
    p[1] = scheme_make_integer(0);
    p[2] = scheme_make_integer(len);
    p[3] = (rarely_block ? scheme_true : scheme_false);
    
    val = scheme_apply(uop->write_proc, 4, p);
  }

  if (SCHEME_FALSEP(val)) {
    if (!rarely_block)
      scheme_arg_mismatch("user port write-string",
			  "bad result for blocking mode: ",
			  val);
    if (rarely_block == 2)
      return -1;
  } if (SCHEME_INTP(val) 
      && (SCHEME_INT_VAL(val) >= 0)
      && (SCHEME_INT_VAL(val) <= len)) {
    int n;

    n = SCHEME_INT_VAL(val);
    
    if (!rarely_block && (n != len)) {
      scheme_arg_mismatch("user port write-string",
			  "result integer for blocking mode is not the "
			  "length of the supplied string: ",
			  val);
    }

    if (n || (rarely_block != 1))
      return n;

    /* In this case, no progress when we need to make some progress... */
  } else {
    if ((SCHEME_INTP(val) && (SCHEME_INT_VAL(val) > 0))
	|| (SCHEME_BIGNUMP(val) && SCHEME_BIGPOS(val))) {
      scheme_arg_mismatch("user port write-string",
			  "result integer is larger than the supplied string: ",
			  val);
    } else {
      scheme_wrong_type("user port write-string",
			"non-negative exact integer or #f", 
			-1, -1, &val);
    }
    return 0;
  }

  /* Assert: rarely_block == 1, and we haven't written anything. */
  {
    Scheme_Object *proc_for_waitable;

    scheme_thread_block(0.0);

    proc_for_waitable = uop->proc_for_waitable;
    if (SCHEME_TRUEP(proc_for_waitable)) {
      Scheme_Object *waitable;

      waitable = scheme_apply(proc_for_waitable, 0, NULL);

      /* close while getting a waitable? */
      if (port->closed)
	return 0;

      if (scheme_is_waitable(waitable)) {
	/* We're going to block on the waitable, so be prepared
	   for a close while we wait: */
	p[0] = scheme_false;
	p[1] = waitable;
	p[2] = uop->closed_sema;
	val = scheme_object_wait_multiple(3, p);
	
	if (port->closed)
	  return 0;
      }
    }

    goto try_again;
  }

}

static void
user_needs_wakeup_output (Scheme_Output_Port *port, void *fds)
{
  /* Nothing needed. */
}

static void
user_close_output (Scheme_Output_Port *port)
{
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  scheme_apply_multi(uop->close_proc, 0, NULL);

  /* Wake up anyone who's blocked on the port.  We rely on the fact
     that port_sema doesn't swap anything in (because port->closed
     isn't set, yet. */
  scheme_post_sema_all(uop->closed_sema);
}

/*========================================================================*/
/*                               pipe ports                               */
/*========================================================================*/

static void pipe_did_read(Scheme_Pipe *pipe)
{
  while (SCHEME_PAIRP(pipe->wakeup_on_read)) {
    Scheme_Object *sema;
    sema = SCHEME_CAR(pipe->wakeup_on_read);
    pipe->wakeup_on_read = SCHEME_CDR(pipe->wakeup_on_read);
    scheme_post_sema(sema);
  }
}

static void pipe_did_write(Scheme_Pipe *pipe)
{
  while (SCHEME_PAIRP(pipe->wakeup_on_write)) {
    Scheme_Object *sema;
    sema = SCHEME_CAR(pipe->wakeup_on_write);
    pipe->wakeup_on_write = SCHEME_CDR(pipe->wakeup_on_write);
    scheme_post_sema(sema);
  }
}

static long pipe_get_or_peek_string(Scheme_Input_Port *p, 
				    char *buffer, long offset, long size,
				    int nonblock,
				    int peek, long peek_skip)
{
  Scheme_Pipe *pipe;
  long c;

  pipe = (Scheme_Pipe *)(p->port_data);

  if ((pipe->bufstart == pipe->bufend) && !pipe->eof) {
    if (nonblock)
      return 0;

    scheme_block_until((Scheme_Ready_Fun)scheme_char_ready_or_user_port_ready,
		       NULL, (Scheme_Object *)p, 0.0);

    if (p->closed) {
      /* Another thread closed the input port while we were waiting. */
      /* Call scheme_getc to signal the error */
      scheme_getc((Scheme_Object *)p);
      return 0; /* doesn't get here */
    }
  }
  
  if (pipe->bufstart == pipe->bufend)
    c = EOF;
  else {
    long bs = pipe->bufstart;
    c = 0;
    if (bs > pipe->bufend) {
      int n;

      /* Determine how much to copy: */
      n = pipe->buflen - bs;
      if (n < peek_skip) {
	peek_skip -= n;
	bs += n;
	n = 0;
      } else {
	bs += peek_skip;
	n -= peek_skip;
	peek_skip = 0;
      }
      if (n > size)
	n = size;

      /* Copy it */
      memcpy(buffer + offset, pipe->buf + bs, n);

      /* Fix up indices */
      bs += n;
      if (bs == pipe->buflen)
	bs = 0;
      if (!peek)
	pipe->bufstart = bs;
      size -= n;
      c += n;
    }
    if (bs < pipe->bufend) {
      int n;

      /* Determine how much to copy: */
      n = pipe->bufend - bs;
      if (n < peek_skip) {
	peek_skip -= n;
	bs += n;
	n = 0;
      } else {
	bs += peek_skip;
	n -= peek_skip;
	peek_skip = 0;
      }
      if (n > size)
	n = size;

      /* Copy it */
      memcpy(buffer + offset + c, pipe->buf + bs, n);

      /* Fix up indices */
      bs += n;
      if (!peek)
	pipe->bufstart = bs;
      size -= n;
      c += n;
    }
  }

  if (!peek)
    pipe_did_read(pipe);
  else {
    if (!c) {
      if (size && pipe->eof)
	return EOF;
      if (!nonblock) {
	/* must have skipped too far; 
	   need to sleep until chars are ready */
	Scheme_Object *my_sema, *wp;
	my_sema = scheme_make_sema(0);
	wp = scheme_make_pair(my_sema, pipe->wakeup_on_write);
	pipe->wakeup_on_write = wp;
	scheme_wait_sema(my_sema, 0);
      }
    }
  }

  return c;
}

static long pipe_get_string(Scheme_Input_Port *p, 
			    char *buffer, long offset, long size,
			    int nonblock)
{
  return pipe_get_or_peek_string(p, buffer, offset, size, nonblock, 0, 0);
}

static long pipe_peek_string(Scheme_Input_Port *p, 
			     char *buffer, long offset, long size,
			     Scheme_Object *skip,
			     int nonblock)
{
  long peek_skip;

  if (SCHEME_INTP(skip))
    peek_skip = SCHEME_INT_VAL(skip);
  else {
#ifdef SIXTY_FOUR_BIT_INTEGERS
    peek_skip = 0x7FFFFFFFFFFFFFFF;
#else
    peek_skip = 0x7FFFFFFF;
#endif
  }

  return pipe_get_or_peek_string(p, buffer, offset, size, nonblock, 1, peek_skip);
}

static long pipe_write_string(Scheme_Output_Port *p, 
			      const char *str, long d, long len, 
			      int rarely_block)
{
  Scheme_Pipe *pipe;
  long avail, firstpos, firstn, secondn, endpos;
  long wrote = 0;

  pipe = (Scheme_Pipe *)(p->port_data);

 try_again:
  
  if (pipe->eof || !len)
    return len + wrote;

  if (pipe->bufstart <= pipe->bufend) {
    firstn = pipe->buflen - pipe->bufend;
    avail = firstn + pipe->bufstart - 1;
    if (!pipe->bufstart)
      --firstn;
  } else {
    firstn = avail = pipe->bufstart - pipe->bufend - 1;
  }
  firstpos = pipe->bufend;

  if (pipe->bufmax && (avail < len)) {
    /* Must we block to write it all? */
    long xavail = avail + (pipe->bufmax - pipe->buflen);
    if (xavail < len) {
      /* We must block to write it all. */
      Scheme_Object *my_sema;

      /* First, write as much as seems immediately possible. */
      xavail = pipe_write_string(p, str, d, xavail, rarely_block);
      wrote += xavail;
      d += xavail;
      len -= xavail;

      /* For non-blocking mode, that might be good enough.
	 rarely_block == 2 means that even nothing is good enough. */
      if ((rarely_block && wrote) || (rarely_block == 2))
	return wrote;

      /* Now, wait until we can write more, then start over. */
      while (1) {
	if (pipe->bufstart <= pipe->bufend) {
	  avail = (pipe->buflen - pipe->bufend) + pipe->bufstart - 1;
	} else {
	  avail = pipe->bufstart - pipe->bufend - 1;
	}
	if (avail || pipe->eof || p->closed)
	  goto try_again;

	my_sema = scheme_make_sema(0);
	{
	  Scheme_Object *wp;
	  wp = scheme_make_pair(my_sema, pipe->wakeup_on_read);
	  pipe->wakeup_on_read = wp;
	}
	
	scheme_wait_sema(my_sema, 0);
      }
      /* Doesn't get here */
    }
  }

  if (avail < len) {
    unsigned char *old;
    int newlen;

    old = pipe->buf;
    newlen = 2 * (pipe->buflen + len);
    if (pipe->bufmax && (newlen > pipe->bufmax))
      newlen = pipe->bufmax;

    {
      unsigned char *uca;
      uca = (unsigned char *)scheme_malloc_atomic(newlen);
      pipe->buf = uca;
    }

    if (pipe->bufstart <= pipe->bufend) {
      memcpy(pipe->buf, old + pipe->bufstart, pipe->bufend - pipe->bufstart);
      pipe->bufend -= pipe->bufstart;
      pipe->bufstart = 0;
    } else {
      int slen;
      slen = pipe->buflen - pipe->bufstart;
      memcpy(pipe->buf, old + pipe->bufstart, slen);
      memcpy(pipe->buf + slen, old, pipe->bufend);
      pipe->bufstart = 0;
      pipe->bufend += slen;
    }

    pipe->buflen = newlen;

    firstpos = pipe->bufend;
    firstn = len;
    endpos = firstpos + firstn;

    secondn = 0;
  } else {
    if (firstn >= len) {
      firstn = len;
      endpos = (firstpos + len) % pipe->buflen;
      secondn = 0;
    } else {
      secondn = len - firstn;
      endpos = secondn;
    }
  }

  if (firstn)
    memcpy(pipe->buf + firstpos, str + d, firstn);
  if (secondn)
    memcpy(pipe->buf, str + d + firstn, secondn);

  pipe->bufend = endpos;

  pipe_did_write(pipe);

  return len + wrote;
}

static int pipe_char_ready(Scheme_Input_Port *p)
{
  Scheme_Pipe *pipe;
  int v;

  pipe = (Scheme_Pipe *)(p->port_data);
  
  v = (pipe->bufstart != pipe->bufend || pipe->eof);

  return v;
}

static void pipe_in_close(Scheme_Input_Port *p)
{
  Scheme_Pipe *pipe;

  pipe = (Scheme_Pipe *)(p->port_data);
  
  pipe->eof = 1;

  /* to wake up any other threads blocked on pipe I/O: */
  pipe_did_read(pipe);
  pipe_did_write(pipe);
}

static void pipe_out_close(Scheme_Output_Port *p)
{
  Scheme_Pipe *pipe;

  pipe = (Scheme_Pipe *)(p->port_data);
  
  pipe->eof = 1;

  /* to wake up any other threads blocked on pipe I/O: */
  pipe_did_read(pipe);
  pipe_did_write(pipe);
}

static int pipe_out_ready(Scheme_Output_Port *p)
{
  Scheme_Pipe *pipe;
  long avail;

  pipe = (Scheme_Pipe *)(p->port_data);
  
  if (pipe->eof || !pipe->bufmax)
    return 1;

  if (pipe->bufstart <= pipe->bufend) {
    avail = (pipe->buflen - pipe->bufend) + pipe->bufstart - 1;
  } else {
    avail = pipe->bufstart - pipe->bufend - 1;
  }

  return !!avail;
}

void scheme_pipe_with_limit(Scheme_Object **read, Scheme_Object **write, int queuelimit)
{
  Scheme_Pipe *pipe;
  Scheme_Input_Port *readp;
  Scheme_Output_Port *writep;

  if (queuelimit) queuelimit++; /* need separator in circular buffer */

  pipe = MALLOC_ONE_RT(Scheme_Pipe);
#ifdef MZTAG_REQUIRED
  pipe->type = scheme_rt_pipe;
#endif
  pipe->buflen = ((queuelimit && (queuelimit < 100)) ? queuelimit : 100);
  {
    unsigned char *uca;
    uca = (unsigned char *)scheme_malloc_atomic(pipe->buflen);
    pipe->buf = uca;
  }
  pipe->bufstart = pipe->bufend = 0;
  pipe->eof = 0;
  pipe->bufmax = queuelimit;
  pipe->wakeup_on_read = scheme_null;
  pipe->wakeup_on_write = scheme_null;

  readp = _scheme_make_input_port(scheme_pipe_read_port_type,
				  (void *)pipe,
				  pipe_get_string,
				  pipe_peek_string,
				  pipe_char_ready,
				  pipe_in_close,
				  NULL,
				  0);

  readp->name = "PIPE";

  writep = scheme_make_output_port(scheme_pipe_write_port_type,
				   (void *)pipe,
				   pipe_write_string,
				   pipe_out_ready,
				   pipe_out_close,
				   NULL,
				   0);

  *read = (Scheme_Object *)readp;
  *write = (Scheme_Object *)writep;
}

void scheme_pipe(Scheme_Object **read, Scheme_Object **write)
{
  scheme_pipe_with_limit(read, write, 0);
}

static Scheme_Object *sch_pipe(int argc, Scheme_Object **args)
{
  Scheme_Object *v[2];
  int bufmax;

  if (argc == 1) {
    Scheme_Object *o = args[0];
    if ((SCHEME_INTP(o) || SCHEME_BIGNUMP(o))
	&& SCHEME_TRUEP(scheme_positive_p(1, args))) {
      if (SCHEME_INTP(o))
	bufmax = SCHEME_INT_VAL(o);
      else
	bufmax = 0;
    } else {
      scheme_wrong_type("make-pipe", "positive exact integer", 0, argc, args);
      return NULL;
    }
  } else
    bufmax = 0;

  scheme_pipe_with_limit(&v[0], &v[1], bufmax);

  return scheme_values(2, v);
}

/*========================================================================*/
/*                    Scheme functions and helpers                        */
/*========================================================================*/

static Scheme_Object *
input_port_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_INPORTP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
output_port_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_OUTPORTP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *current_input_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-input-port", scheme_make_integer(MZCONFIG_INPUT_PORT),
			     argc, argv,
			     -1, input_port_p, "input-port", 0);
}

static Scheme_Object *current_output_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-output-port", scheme_make_integer(MZCONFIG_OUTPUT_PORT),
			     argc, argv,
			     -1, output_port_p, "output-port", 0);
}

static Scheme_Object *current_error_port(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-error-port", scheme_make_integer(MZCONFIG_ERROR_PORT),
			     argc, argv,
			     -1, output_port_p, "error port", 0);
}

static Scheme_Object *
make_input_port(int argc, Scheme_Object *argv[])
{
  Scheme_Input_Port *ip;
  User_Input_Port *uip;
  Scheme_Object *s;

  scheme_check_proc_arity("make-custom-input-port", 1, 0, argc, argv);
  if (SCHEME_TRUEP(argv[1])) {
    if (!scheme_check_proc_arity(NULL, 2, 1, argc, argv))
      scheme_wrong_type("make-custom-input-port", "procedure (arity 2) or #f", 1, argc, argv);
  }
  scheme_check_proc_arity("make-custom-input-port", 0, 2, argc, argv);
  
  uip = MALLOC_ONE_RT(User_Input_Port);
#ifdef MZTAG_REQUIRED
  uip->type = scheme_rt_user_input;
#endif

  uip->read_proc = argv[0];
  uip->peek_proc = argv[1];
  uip->close_proc = argv[2];
  s = scheme_make_sema(0);
  uip->closed_sema = s;

  if (!SCHEME_TRUEP(uip->peek_proc))
    uip->peek_proc = NULL;

  ip = _scheme_make_input_port(scheme_user_input_port_type,
			       uip,
			       user_get_string,
			       (uip->peek_proc
				? user_peek_string
				: NULL),
			       user_char_ready,
			       user_close_input,
			       user_needs_wakeup_input,
			       0);

  ip->name = "CUSTOMPORT";

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_output_port (int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  User_Output_Port *uop;
  Scheme_Object *s;

  if (SCHEME_TRUEP(argv[0])) {
    if (!scheme_check_proc_arity(NULL, 0, 0, argc, argv))
      scheme_wrong_type("make-custom-output-port", "procedure (arity 0) or #f", 0, argc, argv);
  }
  scheme_check_proc_arity("make-custom-output-port", 4, 1, argc, argv);
  scheme_check_proc_arity("make-custom-output-port", 0, 2, argc, argv);
  scheme_check_proc_arity("make-custom-output-port", 0, 3, argc, argv);

  uop = MALLOC_ONE_RT(User_Output_Port);
#ifdef MZTAG_REQUIRED
  uop->type = scheme_rt_user_output;
#endif

  uop->proc_for_waitable = argv[0];
  uop->write_proc = argv[1];
  uop->flush_proc = argv[2];
  uop->close_proc = argv[3];
  s = scheme_make_sema(0);
  uop->closed_sema = s;

  op = scheme_make_output_port(scheme_user_output_port_type,
			       uop,
			       user_write_string,
			       user_write_ready,
			       user_close_output,
			       user_needs_wakeup_output,
			       0);

  return (Scheme_Object *)op;
}

static Scheme_Object *
open_input_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_input_file("open-input-file", 0, argc, argv);
}

static Scheme_Object *
open_input_string (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("open-input-string", "string", 0, argc, argv);

  return scheme_make_sized_string_input_port(SCHEME_STR_VAL(argv[0]), 
					     SCHEME_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
open_output_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_output_file("open-output-file", 0, argc, argv, 0);
}

static Scheme_Object *
open_input_output_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_output_file("open-input-output-file", 0, argc, argv, 1);
}

static Scheme_Object *
open_output_string (int argc, Scheme_Object *argv[])
{
  return scheme_make_string_output_port();
}

static Scheme_Object *
get_output_string (int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  char *s;
  long size;

  op = (Scheme_Output_Port *)argv[0];
  if (!SCHEME_OUTPORTP(argv[0]) 
      || (op->sub_type != scheme_string_output_port_type))
    scheme_wrong_type("get-output-string", "string output port", 0, argc, argv);

  s = scheme_get_sized_string_output(argv[0], &size);

  return scheme_make_sized_string(s, size, 1);
}

static Scheme_Object *
close_input_port (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("close-input-port", "input-port", 0, argc, argv);

  scheme_close_input_port (argv[0]);
  return (scheme_void);
}

static Scheme_Object *
close_output_port (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("close-output-port", "output-port", 0, argc, argv);

  scheme_close_output_port (argv[0]);
  return (scheme_void);
}

static Scheme_Object *
call_with_output_file (int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *port, *v, **m;

  scheme_check_proc_arity("call-with-output-file", 1, 1, argc, argv);

  port = scheme_do_open_output_file("call-with-output-file", 1, argc, argv, 0);
  
  v = _scheme_apply_multi(argv[1], 1, &port);

  m = p->ku.multiple.array;
  if (v == SCHEME_MULTIPLE_VALUES) {
    if (SAME_OBJ(m, p->values_buffer))
      p->values_buffer = NULL;
  }

  scheme_close_output_port(port);

  p->ku.multiple.array = m;

  return v;
}

static Scheme_Object *
call_with_input_file(int argc, Scheme_Object *argv[])
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *port, *v, **m;

  scheme_check_proc_arity("call-with-input-file", 1, 1, argc, argv);

  port = scheme_do_open_input_file("call-with-input-file", 1, argc, argv);
  
  v = _scheme_apply_multi(argv[1], 1, &port);
  
  m = p->ku.multiple.array;
  if (v == SCHEME_MULTIPLE_VALUES) {
    if (SAME_OBJ(m, p->values_buffer))
      p->values_buffer = NULL;
  }

  scheme_close_input_port(port);

  p->ku.multiple.array = m;

  return v;
}

static Scheme_Object *with_call_thunk(void *d)
{
  return _scheme_apply_multi(SCHEME_CAR((Scheme_Object *)d), 0, NULL);
}

static void with_set_output_port(void *d)
{
  Scheme_Config *config = scheme_config;

  SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)) = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);
  scheme_set_param(config, MZCONFIG_OUTPUT_PORT, SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static void with_unset_output_port(void *d)
{
  Scheme_Config *config = scheme_config;

  scheme_set_param(config, MZCONFIG_OUTPUT_PORT, SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)));
  scheme_close_output_port(SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static Scheme_Object *
with_output_to_file (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;

  scheme_check_proc_arity("with-output-to-file", 0, 1, argc, argv);

  port = scheme_do_open_output_file("with-output-to-file", 1, argc, argv, 0);
  
  v = scheme_dynamic_wind(with_set_output_port,
			  with_call_thunk,
			  with_unset_output_port,
			  NULL,
			  scheme_make_pair(argv[1], 
					   scheme_make_pair(port,
							    scheme_void)));

  scheme_close_output_port(port);

  return v;
}

static void with_set_input_port(void *d)
{
  Scheme_Config *config = scheme_config;

  SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)) = scheme_get_param(config, MZCONFIG_INPUT_PORT);
  scheme_set_param(config, MZCONFIG_INPUT_PORT, SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static void with_unset_input_port(void *d)
{
  Scheme_Config *config = scheme_config;

  scheme_set_param(config, MZCONFIG_INPUT_PORT, SCHEME_CDR(SCHEME_CDR((Scheme_Object *)d)));
  scheme_close_input_port(SCHEME_CAR(SCHEME_CDR((Scheme_Object *)d)));
}

static Scheme_Object *
with_input_from_file(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;

  scheme_check_proc_arity("with-input-from-file", 0, 1, argc, argv);

  port = scheme_do_open_input_file("with-input-from-file", 1, argc, argv);
  
  v = scheme_dynamic_wind(with_set_input_port,
			  with_call_thunk,
			  with_unset_input_port,
			  NULL,
			  scheme_make_pair(argv[1], 
					   scheme_make_pair(port,
							    scheme_void)));

  scheme_close_input_port(port);

  return v;
}

static int check_offset_list(Scheme_Object *l)
{
  Scheme_Object *a;

  if (SCHEME_PAIRP(l)) {
    a = SCHEME_CAR(l);
    if (scheme_nonneg_exact_p(a)) {
      l = SCHEME_CDR(l);
      if (SCHEME_PAIRP(l)) {
	a = SCHEME_CAR(l);
	if (scheme_nonneg_exact_p(a)) {
	  l = SCHEME_CDR(l);
	  if (SCHEME_PAIRP(l)) {
	    a = SCHEME_CAR(l);
	    if (scheme_nonneg_exact_p(a)) {
	      l = SCHEME_CDR(l);
	      if (SCHEME_NULLP(l))
		return 1;
	    }
	  }
	}
      }
    }
  }

  return 0;
}

static Scheme_Object *make_offset(Scheme_Object *delta, Scheme_Object *src)
{
  Scheme_Stx_Offset *o;
  Scheme_Object *line, *col, *pos;
      
  line = SCHEME_CAR(delta);
  delta = SCHEME_CDR(delta);
  col = SCHEME_CAR(delta);
  pos = SCHEME_CADR(delta);

  /* Offsets are too large => lost track */
  if (SCHEME_BIGNUMP(line))
    line = scheme_make_integer(-1);
  if (SCHEME_BIGNUMP(col))
    col = scheme_make_integer(-1);
  if (SCHEME_BIGNUMP(pos))
    pos = scheme_make_integer(-1);

  o = MALLOC_ONE_TAGGED(Scheme_Stx_Offset);
  o->type = scheme_stx_offset_type;
  o->src = src;
  o->line = SCHEME_INT_VAL(line);
  o->col = SCHEME_INT_VAL(col);
  o->pos = SCHEME_INT_VAL(pos);

  if (!o->line && !o->col && !o->pos)
    return src;

  return (Scheme_Object *)o;
}

static Scheme_Object *sch_default_read_handler(void *ignore, int argc, Scheme_Object *argv[])
{
  Scheme_Object *src;

  if (!(argc == 1) && !(argc == 3))
    scheme_case_lambda_wrong_count("default-port-read-handler", argc, argv, 0, 2, 1, 1, 3, 3);

  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("default-port-read-handler", "input-port", 0, argc, argv);

  if (argc == 3) {
    if (!check_offset_list(argv[2]))
      scheme_wrong_type("default-port-read-handler", "#f or list of three non-negative exact integers", 2, argc, argv);
  }

  if ((Scheme_Object *)argv[0] == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  if (argc > 1)
    src = make_offset(argv[2], argv[1]);
  else
    src = NULL;

  return scheme_internal_read(argv[0], src, -1, 0);
}

static Scheme_Object *read_f(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("read", "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_thread->config);
  
  if (((Scheme_Input_Port *)port)->read_handler) {
    Scheme_Object *o[1];
    o[0] = port;
    return _scheme_apply(((Scheme_Input_Port *)port)->read_handler, 1, o);
  } else {
    if (port == scheme_orig_stdin_port)
      scheme_flush_orig_outputs();

    return scheme_internal_read(port, NULL, -1, 0);
  }
}

static Scheme_Object *read_syntax_f(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  Scheme_Object *delta = scheme_false;

  if ((argc > 1) && !SCHEME_INPORTP(argv[1]))
    scheme_wrong_type("read-syntax", "input-port", 1, argc, argv);

  if (argc > 1)
    port = argv[1];
  else
    port = CURRENT_INPUT_PORT(scheme_current_thread->config);

  if (argc > 2) {
    /* Argument should be a list: (list line col pos) */
    if (!check_offset_list(argv[2]))
      scheme_wrong_type("read-syntax", "#f or list of three non-negative exact integers", 2, argc, argv);
    
    delta = argv[2];
  }
  
  if (((Scheme_Input_Port *)port)->read_handler) {
    Scheme_Object *o[3], *result;
    o[0] = port;
    o[1] = argv[0];
    if (SCHEME_FALSEP(delta))
      delta = scheme_make_pair(scheme_make_integer(0),
			       scheme_make_pair(scheme_make_integer(0),
						scheme_make_pair(scheme_make_integer(0),
								 scheme_null)));
    o[2] = delta;

    result = _scheme_apply(((Scheme_Input_Port *)port)->read_handler, 3, o);
    if (SCHEME_STXP(result))
      return result;
    else {
      o[0] = result;
      /* -1 for argument count indicates "result" */
      scheme_wrong_type("read handler for read-syntax", "syntax object", 0, -1, o);
      return NULL;
    }
  } else {
    Scheme_Object *src = argv[0];

    if (SCHEME_TRUEP(delta))
      src = make_offset(delta, src);

    if (port == scheme_orig_stdin_port)
      scheme_flush_orig_outputs();

    return scheme_internal_read(port, src, -1, 0);
  }
}

static Scheme_Object *
do_read_char(char *name, int argc, Scheme_Object *argv[], int peek, int spec)
{
  Scheme_Object *port;
  int ch;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type(name, "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if (peek) {
    Scheme_Object *skip;

    if (argc > 1) {
      skip = argv[1];
      if (!(SCHEME_INTP(skip) && (SCHEME_INT_VAL(skip) >= 0))
	  && !(SCHEME_BIGNUMP(skip) && SCHEME_BIGPOS(skip))) {
	scheme_wrong_type(name, "non-negative exact integer", 1, argc, argv);
	return NULL;
      }
    } else
      skip = NULL;
    
    if (spec)
      ch = scheme_peekc_special_ok_skip(port, skip);
    else
      ch = scheme_peekc_skip(port, skip);
  } else {
    if (spec)
      ch = scheme_getc_special_ok(port);
    else
      ch = scheme_getc(port);
  }

  if (ch == SCHEME_SPECIAL) {
    if (peek)
      return scheme_intern_symbol("special");
    else {
      Scheme_Object *r, *exn;
      r = scheme_get_special(port, scheme_false, -1, -1, scheme_tell(port), &exn);
      if (!r) {
	scheme_raise(exn);
      }
      return r;
    }
  } else if (ch == EOF)
    return scheme_eof;
  else
    return _scheme_make_char(ch);
}

static Scheme_Object *
read_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-char", argc, argv, 0, 0);
}

static Scheme_Object *
read_char_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-char-or-special", argc, argv, 0, 1);
}

static Scheme_Object *
peek_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-char", argc, argv, 1, 0);
}

static Scheme_Object *
peek_char_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-char-or-special", argc, argv, 1, 1);
}

static Scheme_Object *
read_line (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  int ch;
  int crlf = 0, cr = 0, lf = 1;
  char *buf, *oldbuf, onstack[32];
  long size = 31, oldsize, i = 0;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("read-line", "input-port", 0, argc, argv);
  if (argc > 1) {
    Scheme_Object *v = argv[1];
    if (SAME_OBJ(v, any_symbol)) {
      crlf = cr = lf = 1;
    } else if (SAME_OBJ(v, any_one_symbol)) {
      crlf = 0;
      cr = lf = 1;
    } else if (SAME_OBJ(v, cr_symbol)) {
      crlf = lf = 0;
      cr = 1;
    } else if (SAME_OBJ(v, lf_symbol)) {
      crlf = cr = 0;
      lf = 1;
    } else if (SAME_OBJ(v, crlf_symbol)) {
      lf = cr = 0;
      crlf = 1;
    } else
      scheme_wrong_type("read-line", "newline specification symbol", 1, argc, argv);
  }

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  buf = onstack;

  while (1) {
    ch = scheme_getc(port);
    if (ch == EOF) {
      if (!i)
	return scheme_eof;
      break;
    }
    if (ch == '\r') {
      if (crlf) {
	int ch2;
	ch2 = scheme_getc(port);
	if (ch2 == '\n')
	  break;
	else {
	  scheme_ungetc(ch2, port);
	  if (cr)
	    break;
	}
      } else if (cr)
	break;
    } else if (ch == '\n') {
      if (lf) break;
    }
    
    if (i >= size) {
      oldsize = size;
      oldbuf = buf;

      size *= 2;
      buf = (char *)scheme_malloc_atomic(size + 1);
      memcpy(buf, oldbuf, oldsize);
    }
    buf[i++] = ch;
  }
  buf[i] = '\0';

  return scheme_make_sized_string(buf, i, buf == onstack);
}

static Scheme_Object *
do_general_read_string(const char *who, int argc, Scheme_Object *argv[],
		       int alloc_mode, int only_avail, int peek)
{
  Scheme_Object *port, *str, *peek_skip;
  long size, start, finish, got;
  int delta, size_too_big = 0;

  if (alloc_mode) {
    if (!SCHEME_INTP(argv[0])) {
      if (SCHEME_BIGNUMP(argv[0])) {
	size = 1;
	size_too_big = 1;
      } else
	size = -1; /* cause the error message to be printed */
    } else
      size = SCHEME_INT_VAL(argv[0]);
    
    if (size < 0) {
      scheme_wrong_type(who, "non-negative exact integer", 0, argc, argv);
      return NULL;
    }
    str = NULL; /* allocated later */
  } else {
    if (!SCHEME_MUTABLE_STRINGP(argv[0])) {
      scheme_wrong_type(who, "mutable-string", 0, argc, argv);
      return NULL;
    }
    str = argv[0];
    size = 0;
  }

  if (peek) {
    Scheme_Object *v;
    v = argv[1];
    if (SCHEME_INTP(v) && (SCHEME_INT_VAL(v) >= 0))
      peek_skip = v;
    else if (SCHEME_BIGNUMP(v) && SCHEME_BIGPOS(v))
      peek_skip = v;
    else {
      scheme_wrong_type(who, "non-negative exact integer", 1, argc, argv);
      return NULL;
    }
    delta = 1;
  } else {
    peek_skip = scheme_make_integer(0);
    delta = 0;
  }

  if ((argc > (1+delta)) && !SCHEME_INPORTP(argv[1+delta]))
    scheme_wrong_type(who, "input-port", 1+delta, argc, argv);
  
  if (alloc_mode) {
    start = 0;
    finish = size;
  } else {
    scheme_get_substring_indices(who, str, 
				 argc, argv,
				 2+delta, 3+delta, &start, &finish);
    
    size = finish - start;
  }

  if (argc > (delta+1))
    port = argv[delta+1];
  else
    port = CURRENT_INPUT_PORT(scheme_config);

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  if (!size) {
    if (alloc_mode)
      return scheme_make_sized_string("", 0, 0);
    else
      return scheme_make_integer(0);
  }

  if (alloc_mode) {
    if (size_too_big) {
      scheme_raise_out_of_memory(who, "making string of length %s",
				 scheme_make_provided_string(argv[0], 0, NULL));
      return NULL;
    }
    str = scheme_alloc_string(size, 0);
  }

  got = scheme_get_string(who, port, 
			  SCHEME_STR_VAL(str), start, size, 
			  only_avail,
			  peek, peek_skip);

  if (got == EOF)
    return scheme_eof;

  if (alloc_mode) {
    if (got < size) {
      /* Ended up with a shorter string: */
      str = scheme_make_sized_string(SCHEME_STR_VAL(str), got, 1);
    }
    return str;
  } else
    return scheme_make_integer(got);
}

static Scheme_Object *
sch_read_string(int argc, Scheme_Object *argv[])
{
  return do_general_read_string("read-string", argc, argv, 1, 0, 0);
}

static Scheme_Object *
sch_peek_string(int argc, Scheme_Object *argv[])
{
  return do_general_read_string("peek-string", argc, argv, 1, 0, 1);
}

static Scheme_Object *
read_string_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_string("read-string-avail!", argc, argv, 0, 1, 0);
}

static Scheme_Object *
read_string_bang_nonblock(int argc, Scheme_Object *argv[])
{
  return do_general_read_string("read-string-avail!*", argc, argv, 0, 2, 0);
}

static Scheme_Object *
peek_string_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_string("peek-string-avail!", argc, argv, 0, 1, 1);
}

static Scheme_Object *
peek_string_bang_nonblock(int argc, Scheme_Object *argv[])
{
  return do_general_read_string("peek-string-avail!*", argc, argv, 0, 2, 1);
}


static Scheme_Object *
do_write_string_avail(const char *who, int argc, Scheme_Object *argv[], int rarely_block)
{
  Scheme_Object *port, *str;
  long size, start, finish, putten;

  if (!SCHEME_STRINGP(argv[0])) {
    scheme_wrong_type(who, "string", 0, argc, argv);
    return NULL;
  } else
    str = argv[0];
  if ((argc > 1) && !SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type(who, "output-port", 1, argc, argv);
  
  scheme_get_substring_indices(who, str, 
			       argc, argv,
			       2, 3, &start, &finish);

  size = finish - start;

  if (argc > 1)
    port = argv[1];
  else
    port = CURRENT_OUTPUT_PORT(scheme_config);

  putten = scheme_put_string(who, port, 
			     SCHEME_STR_VAL(str), start, size,
			     rarely_block);

  if (putten < 0)
    return scheme_false;
  else
    return scheme_make_integer(putten);
}

static Scheme_Object *
write_string_avail(int argc, Scheme_Object *argv[])
{
  return do_write_string_avail("write-string-avail", argc, argv, 1);
}

static Scheme_Object *
write_string_avail_nonblock(int argc, Scheme_Object *argv[])
{
  return do_write_string_avail("write-string-avail*", argc, argv, 2);
}

typedef struct {
  MZTAG_IF_REQUIRED
  int argc;
  Scheme_Object **argv;
  Scheme_Config *config;
  Scheme_Object *orig_param_val;
  Scheme_Prim *k;
} Breakable;

static void pre_breakable(void *data)
{
  Breakable *b = (Breakable *)data;

  b->orig_param_val = scheme_get_param(b->config, MZCONFIG_ENABLE_BREAK);
  scheme_set_param(b->config, MZCONFIG_ENABLE_BREAK, scheme_true);
}

static Scheme_Object *do_breakable(void *data)
{
  Breakable *b = (Breakable *)data;
  Scheme_Prim *k;

  /* Need to check for a break, in case one was queued and we just enabled it: */
  scheme_check_break_now();

  k = b->k;
  return k(b->argc, b->argv);
}

static void post_breakable(void *data)
{
  Breakable *b = (Breakable *)data;
  scheme_set_param(b->config, MZCONFIG_ENABLE_BREAK, b->orig_param_val);
}

Scheme_Object *
scheme_call_enable_break(Scheme_Prim *prim, int argc, Scheme_Object *argv[])
{
  Breakable *b;

  b = MALLOC_ONE_RT(Breakable);
#ifdef MZTAG_REQUIRED
  b->type = scheme_rt_breakable;
#endif
  b->argc = argc;
  b->argv = argv;
  b->k = prim;
  b->config = scheme_current_thread->config;

  return scheme_dynamic_wind(pre_breakable, 
			     do_breakable, 
			     post_breakable, 
			     NULL, b);
}

static Scheme_Object *
read_string_bang_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(read_string_bang, argc, argv);
}

static Scheme_Object *
peek_string_bang_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(peek_string_bang, argc, argv);
}

static Scheme_Object *
write_string_avail_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(write_string_avail, argc, argv);
}

static Scheme_Object *
eof_object_p (int argc, Scheme_Object *argv[])
{
  return (SCHEME_EOFP(argv[0]) ? scheme_true : scheme_false);
}

static Scheme_Object *
char_ready_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("char-ready?", "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_config);
  
  return (scheme_char_ready(port) ? scheme_true : scheme_false);
}

static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-display-handler", "output-port", 1, argc, argv);

  scheme_internal_display(argv[0], argv[1], scheme_config);

  return scheme_void;
}

static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-write-handler", "output-port", 1, argc, argv);

  scheme_internal_write(argv[0], argv[1], scheme_config);

  return scheme_void;
}

static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-print-handler", "output-port", 1, argc, argv);

  return _scheme_apply(scheme_get_param(scheme_config,
					MZCONFIG_PORT_PRINT_HANDLER),
		       argc, argv);
}

static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-global-port-print-handler", "output-port", 1, argc, argv);

  scheme_internal_write(argv[0], argv[1], scheme_config);

  return scheme_void;
}

static Scheme_Object *
display_write(char *name, 
	      int argc, Scheme_Object *argv[], int escape)
{
  Scheme_Object *port;
  Scheme_Config *config = scheme_config;

  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type(name, "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(config);
  
  if (escape > 0) {
    if (!((Scheme_Output_Port *)port)->display_handler) {
      Scheme_Object *v = argv[0];
      if (SCHEME_STRINGP(v)) {
	scheme_put_string(name, port,
			  SCHEME_STR_VAL(v), 0, SCHEME_STRLEN_VAL(v),
			  0);
      } else
	scheme_internal_display(v, port, config);
    } else {
      Scheme_Object *a[2];
      a[0] = argv[0];
      a[1] = port;
      _scheme_apply_multi(((Scheme_Output_Port *)port)->display_handler, 2, a);
    }
  } else if (!escape) {
    Scheme_Object *h;

    h = ((Scheme_Output_Port *)port)->write_handler;
    
    if (!h)
      scheme_internal_write(argv[0], port, config);
    else {
      Scheme_Object *a[2];
      a[0] = argv[0];
      a[1] = port;
      _scheme_apply_multi(h, 2, a);      
    }
  } else {
    Scheme_Object *h;
    Scheme_Object *a[2];
    
    a[0] = argv[0];
    a[1] = port;
    
    h = ((Scheme_Output_Port *)port)->print_handler;
						
    if (!h)
      sch_default_print_handler(2, a);
    else
      _scheme_apply_multi(h, 2, a);      
  }

  return scheme_void;
}

static Scheme_Object *
sch_write (int argc, Scheme_Object *argv[])
{
  return display_write("write", argc, argv, 0);
}

static Scheme_Object *
display (int argc, Scheme_Object *argv[])
{
  return display_write("display", argc, argv, 1);
}

static Scheme_Object *
sch_print (int argc, Scheme_Object *argv[])
{
  return display_write("print", argc, argv, -1);
}

static Scheme_Object *
newline (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("newline", "output-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_OUTPUT_PORT(scheme_config);
  
  (void)scheme_put_string("newline", port, "\n", 0, 1, 0);

  return scheme_void;
}

static Scheme_Object *
write_char (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  unsigned char buffer[1];

  if (argc && !SCHEME_CHARP(argv[0]))
    scheme_wrong_type("write-char", "character", 0, argc, argv);
  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type("write-char", "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_config);

  buffer[0] = SCHEME_CHAR_VAL(argv[0]);

  scheme_put_string("write-char", port,
		    (char *)buffer, 0, 1,
		    0);

  return scheme_void;
}

static Scheme_Object *port_read_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Input_Port *ip;

  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("port-read-handler", "input-port", 0, argc, argv);

  ip = (Scheme_Input_Port *)argv[0];
  if (argc == 1) {
    if (ip->read_handler)
      return ip->read_handler;
    else
      return default_read_handler;
  } else {
    if (argv[1] == default_read_handler)
      ip->read_handler = NULL;
    else {
      if (!scheme_check_proc_arity(NULL, 1, 1, argc, argv)
	  || !scheme_check_proc_arity(NULL, 3, 1, argc, argv)) {
	scheme_wrong_type("port-read-handler", "procedure (arity 1 and 3)", 1, argc, argv);
	return NULL;
      }
      
      ip->read_handler = argv[1];
    }

    return scheme_void;
  }
}

static Scheme_Object *port_display_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-display-handler", "output-port", 0, argc, argv);

  op = (Scheme_Output_Port *)argv[0];
  if (argc == 1) {
    if (op->display_handler)
      return op->display_handler;
    else
      return default_display_handler;
  } else {
    scheme_check_proc_arity("port-display-handler", 2, 1, argc, argv);
    if (argv[1] == default_display_handler)
      op->display_handler = NULL;
    else
      op->display_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *port_write_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-write-handler", "output-port", 0, argc, argv);

  op = (Scheme_Output_Port *)argv[0];
  if (argc == 1) {
    if (op->write_handler)
      return op->write_handler;
    else
      return default_write_handler;
  } else {
    scheme_check_proc_arity("port-write-handler", 2, 1, argc, argv);
    if (argv[1] == default_write_handler)
      op->write_handler = NULL;
    else
      op->write_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *port_print_handler(int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;

  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-print-handler", "output-port", 0, argc, argv);

  op = (Scheme_Output_Port *)argv[0];
  if (argc == 1) {
    if (op->print_handler)
      return op->print_handler;
    else
      return default_print_handler;
  } else {
    scheme_check_proc_arity("port-print-handler", 2, 1, argc, argv);
    if (argv[1] == default_print_handler)
      op->print_handler = NULL;
    else
      op->print_handler = argv[1];

    return scheme_void;
  }
}

static Scheme_Object *global_port_print_handler(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("global-port-print-handler",
			     scheme_make_integer(MZCONFIG_PORT_PRINT_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *port_count_lines(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("port-count-lines!", "input-port", 0, argc, argv);

  scheme_count_lines(argv[0]);

  return scheme_void;
}

static Scheme_Object *global_port_count_lines(int argc, Scheme_Object **argv)
{
  return scheme_param_config("port-count-lines-enabled", 
			     scheme_make_integer(MZCONFIG_PORT_COUNT_LINES), 
			     argc, argv, -1, NULL, NULL, 1);
}

static Scheme_Object *port_next_location(int argc, Scheme_Object *argv[])
{
  Scheme_Object *a[3];
  long line, col, pos;

  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("port-next-location", "input-port", 0, argc, argv);

  line = scheme_tell_line(argv[0]);
  col = scheme_tell_column(argv[0]);
  pos = scheme_tell(argv[0]);

  a[0] = ((line < 0) ? scheme_false : scheme_make_integer_value(line));
  a[1] = ((col < 0) ? scheme_false : scheme_make_integer_value(col));
  a[2] = ((pos < 0) ? scheme_false : scheme_make_integer_value(pos+1));

  return scheme_values(3, a);
}

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Config *config;
  Scheme_Object *port;
  Scheme_Thread *p;
  Scheme_Object *stxsrc;
  Scheme_Object *expected_module;
  Scheme_Object *reader_params[10]; /* only #ts and #fs; see mzmarksrc.c */
} LoadHandlerData;

static void swap_reader_params(void *data)
{
  LoadHandlerData *lhd = (LoadHandlerData *)data;

  if (SCHEME_SYMBOLP(lhd->expected_module)) {
    Scheme_Object *reader_params[10];

    reader_params[0] = scheme_get_param(lhd->config, MZCONFIG_CASE_SENS);
    reader_params[1] = scheme_get_param(lhd->config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS);
    reader_params[2] = scheme_get_param(lhd->config, MZCONFIG_CURLY_BRACES_ARE_PARENS);
    reader_params[3] = scheme_get_param(lhd->config, MZCONFIG_CAN_READ_GRAPH);
    reader_params[4] = scheme_get_param(lhd->config, MZCONFIG_CAN_READ_COMPILED);
    reader_params[5] = scheme_get_param(lhd->config, MZCONFIG_CAN_READ_BOX);
    reader_params[6] = scheme_get_param(lhd->config, MZCONFIG_CAN_READ_PIPE_QUOTE);
    reader_params[7] = scheme_get_param(lhd->config, MZCONFIG_CAN_READ_DOT);
    reader_params[8] = scheme_get_param(lhd->config, MZCONFIG_CAN_READ_QUASI);
    reader_params[9] = scheme_get_param(lhd->config, MZCONFIG_READ_DECIMAL_INEXACT);

    scheme_set_param(lhd->config, MZCONFIG_CASE_SENS, lhd->reader_params[0]);
    scheme_set_param(lhd->config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS, lhd->reader_params[1]);
    scheme_set_param(lhd->config, MZCONFIG_CURLY_BRACES_ARE_PARENS, lhd->reader_params[2]);
    scheme_set_param(lhd->config, MZCONFIG_CAN_READ_GRAPH, lhd->reader_params[3]);
    scheme_set_param(lhd->config, MZCONFIG_CAN_READ_COMPILED, lhd->reader_params[4]);
    scheme_set_param(lhd->config, MZCONFIG_CAN_READ_BOX, lhd->reader_params[5]);
    scheme_set_param(lhd->config, MZCONFIG_CAN_READ_PIPE_QUOTE, lhd->reader_params[6]);
    scheme_set_param(lhd->config, MZCONFIG_CAN_READ_DOT, lhd->reader_params[7]);
    scheme_set_param(lhd->config, MZCONFIG_CAN_READ_QUASI, lhd->reader_params[8]);
    scheme_set_param(lhd->config, MZCONFIG_READ_DECIMAL_INEXACT, lhd->reader_params[9]);

    lhd->reader_params[0] = reader_params[0];
    lhd->reader_params[1] = reader_params[1];
    lhd->reader_params[2] = reader_params[2];
    lhd->reader_params[3] = reader_params[3];
    lhd->reader_params[4] = reader_params[4];
    lhd->reader_params[5] = reader_params[5];
    lhd->reader_params[6] = reader_params[6];
    lhd->reader_params[7] = reader_params[7];
    lhd->reader_params[8] = reader_params[8];
    lhd->reader_params[9] = reader_params[9];
  }
}

static void post_load_handler(void *data)
{
  LoadHandlerData *lhd = (LoadHandlerData *)data;

  swap_reader_params(data);

  scheme_close_input_port((Scheme_Object *)lhd->port);
}

static Scheme_Object *do_load_handler(void *data)
{  
  LoadHandlerData *lhd = (LoadHandlerData *)data;
  Scheme_Object *port = lhd->port;
  Scheme_Thread *p = lhd->p;
  Scheme_Config *config = lhd->config;
  Scheme_Object *last_val = scheme_void, *obj, **save_array = NULL;
  Scheme_Env *genv;
  int save_count = 0, got_one = 0;

  while ((obj = scheme_internal_read(port, lhd->stxsrc, 1, 0))
	 && !SCHEME_EOFP(obj)) {
    save_array = NULL;
    got_one = 1;

    /* ... begin special support for module loading ... */

    if (SCHEME_SYMBOLP(lhd->expected_module)) {
      /* Must be of the form `(module <expectedname> ...)',possibly compiled. */
      /* Also, file should have no more expressions. */
      Scheme_Object *a, *d, *other = NULL;
      Scheme_Module *m;

      d = obj;

      m = scheme_extract_compiled_module(SCHEME_STX_VAL(d));
      if (m) {
	if (!SAME_OBJ(m->modname, lhd->expected_module)) {
	  other = m->modname;
	  d = NULL;
	}
      } else {
	if (!SCHEME_STX_PAIRP(d))
	  d = NULL;
	else {
	  a = SCHEME_STX_CAR(d);
	  if (!SAME_OBJ(SCHEME_STX_VAL(a), module_symbol))
	    d = NULL;
	  else {
	    d = SCHEME_STX_CDR(d);
	    if (!SCHEME_STX_PAIRP(d))
	      d = NULL;
	    else {
	      a = SCHEME_STX_CAR(d);
	      other = SCHEME_STX_VAL(a);
	      if (!SAME_OBJ(other, lhd->expected_module))
		d = NULL;
	    }
	  }
	}
      }

      /* If d is NULL, shape was wrong */
      if (!d) {
	if (!other || !SCHEME_SYMBOLP(other))
	  other = scheme_make_string("something else");
	else {
	  char *s, *t;
	  long len, slen;

	  t = "declaration for `";
	  len = strlen(t);
	  slen = SCHEME_SYM_LEN(other);

	  s = (char *)scheme_malloc_atomic(len + slen + 2);
	  memcpy(s, t, len);
	  memcpy(s + len, SCHEME_SYM_VAL(other), slen);
	  s[len + slen] = '\'';
	  s[len + slen + 1]= 0;

	  other = scheme_make_sized_string(s, len + slen + 1, 0);
	}

	scheme_raise_exn(MZEXN_MODULE,
			 "default-load-handler: expected a `module' declaration for `%S', found: %T in: %q",
			 lhd->expected_module,
			 other,
			 ((Scheme_Input_Port *)port)->name);

	return NULL;
      }

      /* Check no more expressions: */
      d = scheme_internal_read(port, lhd->stxsrc, 1, 0);
      if (!SCHEME_EOFP(d)) {
	scheme_raise_exn(MZEXN_MODULE,
			 "default-load-handler: expected only a `module' declaration for `%S', but found an extra expression in: %q",
			 lhd->expected_module,
			 ((Scheme_Input_Port *)port)->name);
	
	return NULL;
      }

      if (!m) {
	/* Replace `module' in read expression with one bound to #%kernel's `module': */
	a = SCHEME_STX_CAR(obj);
	d = SCHEME_STX_CDR(obj);
	a = scheme_datum_to_syntax(module_symbol, a, scheme_sys_wraps(NULL), 0, 1);
	d = scheme_make_immutable_pair(a, d);
	obj = scheme_datum_to_syntax(d, obj, scheme_false, 0, 1);
      }
    }

    /* ... end special support for module loading ... */

    genv = (Scheme_Env *)scheme_get_param(config, MZCONFIG_ENV);
    if (genv->rename)
      obj = scheme_add_rename(obj, genv->rename);
    if (genv->exp_env && genv->exp_env->rename)
      obj = scheme_add_rename(obj, genv->exp_env->rename);
    
    last_val = _scheme_apply_multi(scheme_get_param(config, MZCONFIG_EVAL_HANDLER),
				   1, &obj);

    /* If multi, we must save then: */
    if (last_val == SCHEME_MULTIPLE_VALUES) {
      save_array = p->ku.multiple.array;
      save_count = p->ku.multiple.count;

      if (SAME_OBJ(save_array, p->values_buffer))
	p->values_buffer = NULL;
    }

    if (SCHEME_SYMBOLP(lhd->expected_module))
      break;
  }

  if (SCHEME_SYMBOLP(lhd->expected_module) && !got_one) {
    scheme_raise_exn(MZEXN_MODULE,
		     "default-load-handler: expected a `module' declaration for `%S', but found end-of-file in: %q",
		     lhd->expected_module,
		     ((Scheme_Input_Port *)port)->name);
    
    return NULL;
  }

  if (save_array) {
    p->ku.multiple.array = save_array;
    p->ku.multiple.count = save_count;
  }

  return last_val;
}

static Scheme_Object *default_load(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *name, *expected_module;
  int ch;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Config *config = p->config;
  LoadHandlerData *lhd;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("default-load-handler", "string", 0, argc, argv);
  expected_module = argv[1];
  if (!SCHEME_FALSEP(expected_module) && !SCHEME_SYMBOLP(expected_module))
    scheme_wrong_type("default-load-handler", "symbol or #f", 1, argc, argv);

  port = scheme_do_open_input_file("default-load-handler", 0, 1, argv);

  /* Turn on line/column counting, unless it's a .zo file: */
  {
    long len;

    len = SCHEME_STRLEN_VAL(argv[0]);
    if ((len < 3)
	|| (SCHEME_STR_VAL(argv[0])[len - 3] != '.')
	|| (SCHEME_STR_VAL(argv[0])[len - 2] != 'z')
	|| (SCHEME_STR_VAL(argv[0])[len - 1] != 'o'))
      scheme_count_lines(port);
  }

  /* Skip over #! at beginning of file */
  if ((ch = scheme_getc(port)) == '#') {
    if ((ch = scheme_getc(port)) == '!') {
      int oldch;
    eol_loop:
      oldch = 0;
      while (1) {
	ch = scheme_getc(port);
	if (ch == EOF || ch == '\n' || ch == '\r')
	  break;
	oldch = ch;
      }
      if (oldch == '\\')
	goto eol_loop;
    } else {
      scheme_ungetc(ch, port);
      scheme_ungetc('#', port);
    }
  } else
    scheme_ungetc(ch, port);

  lhd = MALLOC_ONE_RT(LoadHandlerData);
#ifdef MZTAG_REQUIRED
  lhd->type = scheme_rt_load_handler_data;
#endif
  lhd->p = p;
  lhd->config = config;
  lhd->port = port;
  name = scheme_make_string(((Scheme_Input_Port *)port)->name);
  lhd->stxsrc = name;
  lhd->expected_module = expected_module;

  if (SCHEME_SYMBOLP(expected_module)) {
    /* Init reader params: */
    lhd->reader_params[0] = scheme_false;  /* MZCONFIG_CASE_SENS */
    lhd->reader_params[1] = scheme_true;   /* MZCONFIG_SQUARE_BRACKETS_ARE_PARENS */
    lhd->reader_params[2] = scheme_true;   /* MZCONFIG_CURLY_BRACES_ARE_PARENS */
    lhd->reader_params[3] = scheme_true;   /* MZCONFIG_CAN_READ_GRAPH */
    lhd->reader_params[4] = scheme_true;   /* MZCONFIG_CAN_READ_COMPILED */
    lhd->reader_params[5] = scheme_true;   /* MZCONFIG_CAN_READ_BOX */
    lhd->reader_params[6] = scheme_true;   /* MZCONFIG_CAN_READ_PIPE_QUOTE */
    lhd->reader_params[7] = scheme_true;   /* MZCONFIG_CAN_READ_DOT */
    lhd->reader_params[8] = scheme_true;   /* MZCONFIG_CAN_READ_QUASI */
    lhd->reader_params[9] = scheme_true;   /* MZCONFIG_READ_DECIMAL_INEXACT */
  }

  return scheme_dynamic_wind(swap_reader_params, do_load_handler, post_load_handler,
			     NULL, (void *)lhd);
}

typedef struct {
  MZTAG_IF_REQUIRED
  int param;
  Scheme_Object *filename;
  Scheme_Config *config;
  Scheme_Object *load_dir, *old_load_dir;
} LoadData;

static void pre_load(void *data)
{
  LoadData *ld = (LoadData *)data;

  scheme_set_param(ld->config, MZCONFIG_LOAD_DIRECTORY, ld->load_dir);  
}

static void post_load(void *data)
{
  LoadData *ld = (LoadData *)data;

  scheme_set_param(ld->config, MZCONFIG_LOAD_DIRECTORY, ld->old_load_dir);
}

static Scheme_Object *do_load(void *data)
{  
  LoadData *ld = (LoadData *)data;
  Scheme_Object *argv[2];

  argv[0] = ld->filename;
  argv[1] = scheme_false;
  return _scheme_apply_multi(scheme_get_param(ld->config, ld->param), 2, argv);
}

Scheme_Object *scheme_load_with_clrd(int argc, Scheme_Object *argv[],
				     char *who, int handler_param)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Config *config = p->config;
  LoadData *ld;
  const char *filename;
  Scheme_Object *load_dir;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(who, "string", 0, argc, argv);

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    who,
				    NULL,
				    SCHEME_GUARD_FILE_READ);

  /* Calculate load directory */
  load_dir = scheme_get_file_directory(filename);

  ld = MALLOC_ONE_RT(LoadData);
#ifdef MZTAG_REQUIRED
  ld->type = scheme_rt_load_data;
#endif
  ld->param = handler_param;
  {
    Scheme_Object *ss;
    ss = scheme_make_sized_string((char *)filename, -1, 0);
    ld->filename = ss;
  }
  ld->config = config;
  ld->load_dir = load_dir;
  ld->old_load_dir = scheme_get_param(config, MZCONFIG_LOAD_DIRECTORY);

  return scheme_dynamic_wind(pre_load, do_load, post_load,
			     NULL, (void *)ld);
}

static Scheme_Object *load(int argc, Scheme_Object *argv[])
{
  return scheme_load_with_clrd(argc, argv, "load", MZCONFIG_LOAD_HANDLER);
}

static Scheme_Object *
current_load(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load",
			     scheme_make_integer(MZCONFIG_LOAD_HANDLER),
			     argc, argv,
			     2, NULL, NULL, 0);
}

static Scheme_Object *abs_directory_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *d = argv[0];

  if (!SCHEME_FALSEP(d)) {
    char *expanded;
    Scheme_Object *ed;
    char *s;
    int len;

    if (!SCHEME_STRINGP(d))
      return NULL;

    s = SCHEME_STR_VAL(d);
    len = SCHEME_STRTAG_VAL(d);

    if (!scheme_is_complete_path(s, len))
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		       d,
		       scheme_intern_symbol("ill-formed-path"),
		       "current-load-relative-directory: not a complete path: \"%q\"",
		       s);

    expanded = scheme_expand_filename(s, len, "current-load-relative-directory", NULL, 
				      SCHEME_GUARD_FILE_EXISTS);
    ed = scheme_make_immutable_sized_string(expanded, strlen(expanded), 1);
    if (!scheme_directory_exists(expanded)) {
      scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		       ed,
		       fail_err_symbol,
		       "current-load-relative-directory: directory not found or not a directory: \"%q\"",
		       expanded);
    }

    return ed;
  }

  return scheme_false;
}

static Scheme_Object *
current_load_directory(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("current-load-relative-directory", 
			     scheme_make_integer(MZCONFIG_LOAD_DIRECTORY),
			     argc, argv,
			     -1, abs_directory_p, "string or #f", 1);
}

Scheme_Object *scheme_load(const char *file)
{
  Scheme_Object *p[1];
  mz_jmp_buf savebuf;
  Scheme_Object * volatile val;

  p[0] = scheme_make_string(file);
  memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf));
  if (scheme_setjmp(scheme_error_buf)) {
    val = NULL;
  } else {
    val = scheme_apply_multi(scheme_make_prim((Scheme_Prim *)load),
			     1, p);
  }
  memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf));

  return val;
}

static Scheme_Object *compiled_kind_p(int argc, Scheme_Object **argv)
{
  Scheme_Object *o = argv[0];
  
  if (SAME_OBJ(o, all_symbol))
    return o;
  if (SAME_OBJ(o, none_symbol))
    return o;

  return NULL;
}

static Scheme_Object *use_compiled_kind(int argc, Scheme_Object *argv[])
{
  return scheme_param_config("use-compiled-file-kinds",
			     scheme_make_integer(MZCONFIG_USE_COMPILED_KIND),
			     argc, argv,
			     -1, compiled_kind_p, "compiled file kind symbol", 1);
}

static Scheme_Object *
transcript_on(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("transcript-on", "string", 0, argc, argv);

  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "transcript-on: not supported");

  return scheme_void;
}

static Scheme_Object *
transcript_off(int argc, Scheme_Object *argv[])
{
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "transcript-off: not supported");

  return scheme_void;
}

static Scheme_Object *
flush_output(int argc, Scheme_Object *argv[])
{
  Scheme_Object *op;

  if (argc && !SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("flush-output", "output-port", 0, argc, argv);

  if (argc) 
    op = argv[0];
  else
    op = CURRENT_OUTPUT_PORT(scheme_config);

  scheme_flush_output(op);

  return (scheme_void);
}

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_PORTFUN_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_breakable, mark_breakable);  
  GC_REG_TRAV(scheme_rt_indexed_string, mark_indexed_string);
  GC_REG_TRAV(scheme_rt_load_handler_data, mark_load_handler_data);
  GC_REG_TRAV(scheme_rt_load_data, mark_load_data);
  GC_REG_TRAV(scheme_rt_user_input, mark_user_input);
  GC_REG_TRAV(scheme_rt_user_output, mark_user_output);
}

END_XFORM_SKIP;

#endif
