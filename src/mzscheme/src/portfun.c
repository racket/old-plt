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
static Scheme_Object *read_byte (int, Scheme_Object *[]);
static Scheme_Object *read_byte_spec (int, Scheme_Object *[]);
static Scheme_Object *read_line (int, Scheme_Object *[]);
static Scheme_Object *read_byte_line (int, Scheme_Object *[]);
static Scheme_Object *sch_read_string (int, Scheme_Object *[]);
static Scheme_Object *sch_read_string_bang (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_string (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_string_bang (int, Scheme_Object *[]);
static Scheme_Object *sch_read_bytes (int, Scheme_Object *[]);
static Scheme_Object *sch_read_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_bytes (int, Scheme_Object *[]);
static Scheme_Object *sch_peek_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *read_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *read_bytes_bang_nonblock (int, Scheme_Object *[]);
static Scheme_Object *read_bytes_bang_break (int, Scheme_Object *[]);
static Scheme_Object *peek_bytes_bang (int, Scheme_Object *[]);
static Scheme_Object *peek_bytes_bang_nonblock (int, Scheme_Object *[]);
static Scheme_Object *peek_bytes_bang_break (int, Scheme_Object *[]);
static Scheme_Object *write_bytes(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_string(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail_nonblock(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *can_write_atomic(int argc, Scheme_Object *argv[]);
static Scheme_Object *can_read_atomic(int argc, Scheme_Object *argv[]);
static Scheme_Object *can_write_special(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_special(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_special_nonblock(int argc, Scheme_Object *argv[]);
static Scheme_Object *peek_char (int, Scheme_Object *[]);
static Scheme_Object *peek_char_spec (int, Scheme_Object *[]);
static Scheme_Object *peek_byte (int, Scheme_Object *[]);
static Scheme_Object *peek_byte_spec (int, Scheme_Object *[]);
static Scheme_Object *eof_object_p (int, Scheme_Object *[]);
static Scheme_Object *char_ready_p (int, Scheme_Object *[]);
static Scheme_Object *byte_ready_p (int, Scheme_Object *[]);
static Scheme_Object *read_bytes_avail_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *peek_bytes_avail_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_bytes_avail_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *write_special_evt(int argc, Scheme_Object *argv[]);
static Scheme_Object *read_byte_evt (int argc, Scheme_Object *argv[]);
static Scheme_Object *peek_byte_evt (int argc, Scheme_Object *argv[]);
static Scheme_Object *read_special_evt (int argc, Scheme_Object *argv[]);
static Scheme_Object *peek_special_evt (int argc, Scheme_Object *argv[]);
static Scheme_Object *sch_write (int, Scheme_Object *[]);
static Scheme_Object *display (int, Scheme_Object *[]);
static Scheme_Object *sch_print (int, Scheme_Object *[]);
static Scheme_Object *newline (int, Scheme_Object *[]);
static Scheme_Object *write_char (int, Scheme_Object *[]);
static Scheme_Object *write_byte (int, Scheme_Object *[]);
static Scheme_Object *load (int, Scheme_Object *[]);
static Scheme_Object *current_load (int, Scheme_Object *[]);
static Scheme_Object *current_load_directory(int argc, Scheme_Object *argv[]);
static Scheme_Object *default_load (int, Scheme_Object *[]);
static Scheme_Object *transcript_on(int, Scheme_Object *[]);
static Scheme_Object *transcript_off(int, Scheme_Object *[]);
static Scheme_Object *flush_output (int, Scheme_Object *[]);
static Scheme_Object *open_input_char_string (int, Scheme_Object *[]);
static Scheme_Object *open_input_byte_string (int, Scheme_Object *[]);
static Scheme_Object *open_output_string (int, Scheme_Object *[]);
static Scheme_Object *get_output_char_string (int, Scheme_Object *[]);
static Scheme_Object *get_output_byte_string (int, Scheme_Object *[]);
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
  scheme_add_global_constant("terminal-port?", 
			     scheme_make_folding_prim(scheme_terminal_port_p, 
						      "terminal-port?", 
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
  scheme_add_global_constant("open-input-bytes", 
			     scheme_make_prim_w_arity(open_input_byte_string, 
						      "open-input-bytes", 
						      1, 1), 
			     env);
  scheme_add_global_constant("open-input-string", 
			     scheme_make_prim_w_arity(open_input_char_string, 
						      "open-input-string", 
						      1, 1), 
			     env);
  scheme_add_global_constant("open-output-file", 
			     scheme_make_prim_w_arity(open_output_file,
						      "open-output-file",
						      1, 3), 
			     env);
  scheme_add_global_constant("open-output-bytes", 
			     scheme_make_prim_w_arity(open_output_string,
						      "open-output-bytes", 
						      0, 0),
			     env);
  scheme_add_global_constant("open-output-string", 
			     scheme_make_prim_w_arity(open_output_string,
						      "open-output-string", 
						      0, 0),
			     env);
  scheme_add_global_constant("get-output-bytes", 
			     scheme_make_prim_w_arity(get_output_byte_string,
						      "get-output-bytes",
						      1, 1),
			     env);
  scheme_add_global_constant("get-output-string", 
			     scheme_make_prim_w_arity(get_output_char_string,
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
  scheme_add_global_constant("make-input-port", 
			     scheme_make_prim_w_arity(make_input_port, 
						      "make-input-port", 
						      4, 6), 
			     env);
  scheme_add_global_constant("make-output-port",
			     scheme_make_prim_w_arity(make_output_port, 
						      "make-output-port", 
						      4, 7), 
			     env);
  
  scheme_add_global_constant("read", 
			     scheme_make_prim_w_arity(read_f,
						      "read", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-syntax", 
			     scheme_make_prim_w_arity(read_syntax_f,
						      "read-syntax", 
						      0, 3), 
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
  scheme_add_global_constant("read-byte", 
			     scheme_make_prim_w_arity(read_byte, 
						      "read-byte", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-byte-or-special", 
			     scheme_make_prim_w_arity(read_byte_spec, 
						      "read-byte-or-special", 
						      0, 1), 
			     env);
  scheme_add_global_constant("read-bytes-line", 
			     scheme_make_prim_w_arity(read_byte_line, 
						      "read-bytes-line", 
						      0, 2), 
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
  scheme_add_global_constant("read-string!", 
			     scheme_make_prim_w_arity(sch_read_string_bang, 
						      "read-string!", 
						      1, 4), 
			     env);
  scheme_add_global_constant("peek-string", 
			     scheme_make_prim_w_arity(sch_peek_string, 
						      "peek-string", 
						      2, 3), 
			     env);
  scheme_add_global_constant("peek-string!", 
			     scheme_make_prim_w_arity(sch_peek_string_bang, 
						      "peek-string!", 
						      2, 5), 
			     env);
  scheme_add_global_constant("read-bytes", 
			     scheme_make_prim_w_arity(sch_read_bytes, 
						      "read-bytes", 
						      1, 2), 
			     env);
  scheme_add_global_constant("read-bytes!", 
			     scheme_make_prim_w_arity(sch_read_bytes_bang, 
						      "read-bytes!", 
						      1, 4), 
			     env);
  scheme_add_global_constant("peek-bytes", 
			     scheme_make_prim_w_arity(sch_peek_bytes, 
						      "peek-bytes", 
						      2, 3), 
			     env);
  scheme_add_global_constant("peek-bytes!", 
			     scheme_make_prim_w_arity(sch_peek_bytes_bang, 
						      "peek-bytes!", 
						      2, 5), 
			     env);
  scheme_add_global_constant("read-bytes-avail!", 
			     scheme_make_prim_w_arity(read_bytes_bang, 
						      "read-bytes-avail!", 
						      1, 4), 
			     env);
  scheme_add_global_constant("read-bytes-avail!*", 
			     scheme_make_prim_w_arity(read_bytes_bang_nonblock, 
						      "read-bytes-avail!*", 
						      1, 4), 
			     env);
  scheme_add_global_constant("read-bytes-avail!/enable-break", 
			     scheme_make_prim_w_arity(read_bytes_bang_break, 
						      "read-bytes-avail!/enable-break", 
						      1, 4), 
			     env);
  scheme_add_global_constant("peek-bytes-avail!", 
			     scheme_make_prim_w_arity(peek_bytes_bang, 
						      "peek-bytes-avail!", 
						      2, 5), 
			     env);
  scheme_add_global_constant("peek-bytes-avail!*", 
			     scheme_make_prim_w_arity(peek_bytes_bang_nonblock, 
						      "peek-bytes-avail!*", 
						      2, 5), 
			     env);
  scheme_add_global_constant("peek-bytes-avail!/enable-break", 
			     scheme_make_prim_w_arity(peek_bytes_bang_break, 
						      "peek-bytes-avail!/enable-break", 
						      2, 5), 
			     env);
  scheme_add_global_constant("port-reads-atomic?", 
			     scheme_make_prim_w_arity(can_read_atomic, 
						      "port-reads-atomic?", 
						      1, 1),
			     env);
  scheme_add_global_constant("write-bytes", 
			     scheme_make_prim_w_arity(write_bytes, 
						      "write-bytes", 
						      1, 4),
			     env);
  scheme_add_global_constant("write-string", 
			     scheme_make_prim_w_arity(write_string, 
						      "write-string", 
						      1, 4),
			     env);
  scheme_add_global_constant("write-bytes-avail", 
			     scheme_make_prim_w_arity(write_bytes_avail, 
						      "write-bytes-avail", 
						      1, 4),
			     env);
  scheme_add_global_constant("write-bytes-avail*", 
			     scheme_make_prim_w_arity(write_bytes_avail_nonblock, 
						      "write-bytes-avail*", 
						      1, 4),
			     env);
  scheme_add_global_constant("write-bytes-avail/enable-break",
			     scheme_make_prim_w_arity(write_bytes_avail_break, 
						      "write-bytes-avail/enable-break", 
						      1, 4),
			     env);
  scheme_add_global_constant("port-writes-atomic?", 
			     scheme_make_prim_w_arity(can_write_atomic, 
						      "port-writes-atomic?", 
						      1, 1),
			     env);
  scheme_add_global_constant("port-writes-special?", 
			     scheme_make_prim_w_arity(can_write_special, 
						      "port-writes-special?", 
						      1, 1),
			     env);
  scheme_add_global_constant("write-special", 
			     scheme_make_prim_w_arity(write_special, 
						      "write-special", 
						      1, 2),
			     env);
  scheme_add_global_constant("write-special-avail*", 
			     scheme_make_prim_w_arity(write_special_nonblock, 
						      "write-special-avail*", 
						      1, 2),
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
  scheme_add_global_constant("peek-byte", 
			     scheme_make_prim_w_arity(peek_byte, 
						      "peek-byte", 
						      0, 2), 
			     env);
  scheme_add_global_constant("peek-byte-or-special", 
			     scheme_make_prim_w_arity(peek_byte_spec, 
						      "peek-byte-or-special", 
						      0, 2), 
			     env);
  scheme_add_global_constant("eof-object?", 
			     scheme_make_folding_prim(eof_object_p, 
						      "eof-object?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("byte-ready?", 
			     scheme_make_prim_w_arity(byte_ready_p, 
						      "byte-ready?", 
						      0, 1), 
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
  scheme_add_global_constant("write-byte", 
			     scheme_make_prim_w_arity(write_byte, 
						      "write-byte", 
						      1, 2), 
			     env);

  scheme_add_global_constant("read-bytes-avail!-evt",
			     scheme_make_prim_w_arity(read_bytes_avail_evt,
						      "read-bytes-avail!-evt",
						      1, 4),
			     env);
  scheme_add_global_constant("peek-bytes-avail!-evt",
			     scheme_make_prim_w_arity(peek_bytes_avail_evt,
						      "peek-bytes-avail!-evt",
						      2, 5),
			     env);
  scheme_add_global_constant("read-byte-evt",
			     scheme_make_prim_w_arity(read_byte_evt,
						      "read-byte-evt",
						      0, 1),
			     env);
  scheme_add_global_constant("read-byte-or-special-evt",
			     scheme_make_prim_w_arity(read_special_evt,
						      "read-byte-or-special-evt",
						      0, 1),
			     env);
  scheme_add_global_constant("peek-byte-evt",
			     scheme_make_prim_w_arity(peek_byte_evt,
						      "peek-byte-evt",
						      1, 2),
			     env);
  scheme_add_global_constant("peek-byte-or-special-evt",
			     scheme_make_prim_w_arity(peek_special_evt,
						      "peek-byte-or-special-evt",
						      1, 2),
			     env);
  scheme_add_global_constant("write-bytes-avail-evt",
			     scheme_make_prim_w_arity(write_bytes_avail_evt,
						      "write-bytes-avail-evt",
						      1, 4),
			     env);
  scheme_add_global_constant("write-special-evt",
			     scheme_make_prim_w_arity(write_special_evt,
						      "write-special-evt",
						      2, 2),
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
  scheme_set_root_param(MZCONFIG_LOAD_DIRECTORY, scheme_false);
  scheme_set_root_param(MZCONFIG_USE_COMPILED_KIND, 
			scheme_make_immutable_pair(scheme_make_path("compiled"),
						   scheme_null));

  {
    Scheme_Object *dlh;
    dlh = scheme_make_prim_w_arity2(default_load,
				    "default-load-handler",
				    2, 2,
				    0, -1);
    scheme_set_root_param(MZCONFIG_LOAD_HANDLER, dlh);
  }

  REGISTER_SO(scheme_default_global_print_handler);
  scheme_default_global_print_handler 
    = scheme_make_prim_w_arity(sch_default_global_port_print_handler,
			       "default-global-port-print-handler",
			       2, 2);
  scheme_set_root_param(MZCONFIG_PORT_PRINT_HANDLER, 
			scheme_default_global_print_handler);
}

/*========================================================================*/
/*                          string input ports                            */
/*========================================================================*/

static long 
string_get_or_peek_bytes(Scheme_Input_Port *port, 
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
string_get_bytes(Scheme_Input_Port *port, 
		  char *buffer, long offset, long size,
		  int nonblock)
{
  return string_get_or_peek_bytes(port, buffer, offset, size, 0, 0);
}

static long 
string_peek_bytes(Scheme_Input_Port *port, 
		   char *buffer, long offset, long size,
		   Scheme_Object *sskip,
		   int nonblock)
{
  long skip;

  if (SCHEME_INTP(sskip))
    skip = SCHEME_INT_VAL(sskip);
  else
    skip = ((Scheme_Indexed_String *)port->port_data)->size;

  return string_get_or_peek_bytes(port, buffer, offset, size, 1, skip);
}

static int
string_byte_ready (Scheme_Input_Port *port)
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
scheme_make_sized_byte_string_input_port(const char *str, long len)
{
  Scheme_Input_Port *ip;

  ip = scheme_make_input_port(scheme_string_input_port_type,
			      make_indexed_string(str, len),
			      scheme_intern_symbol("string"),
			      scheme_get_evt_via_get,
			      string_get_bytes,
			      scheme_peek_evt_via_peek,
			      string_peek_bytes,
			      string_byte_ready,
			      string_close_in,
			      NULL,
			      0);

  return (Scheme_Object *)ip;
}

Scheme_Object *
scheme_make_byte_string_input_port(const char *str)
{
  return scheme_make_sized_byte_string_input_port(str, strlen(str));
}

/*========================================================================*/
/*                          string output ports                           */
/*========================================================================*/

static long
string_write_bytes(Scheme_Output_Port *port, 
		    const char *str, long d, long len, 
		    int rarely_block, int enable_break)
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
scheme_make_byte_string_output_port (void)
{
  Scheme_Output_Port *op;

  op = scheme_make_output_port (scheme_string_output_port_type,
				make_indexed_string(NULL, 0),
				scheme_intern_symbol("string"),
				scheme_write_evt_via_write,
				string_write_bytes,
				NULL,
				string_close_out,
				NULL,
				NULL,
				NULL,
				0);

  return (Scheme_Object *)op;
}

char *
scheme_get_sized_byte_string_output(Scheme_Object *port, long *size)
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
  return scheme_get_sized_byte_string_output(port, NULL);
}

/*========================================================================*/
/*                 "user" input ports (created from Scheme)               */
/*========================================================================*/

typedef struct User_Input_Port {
  MZTAG_IF_REQUIRED
  Scheme_Object *evt;
  Scheme_Object *read_evt_proc; /* NULL => no atomic reads */
  Scheme_Object *read_proc;
  Scheme_Object *peek_evt_proc; /* NULL => implement via peek_proc */
  Scheme_Object *peek_proc;
  Scheme_Object *close_proc;
  Scheme_Object *reuse_str;
  Scheme_Object *peeked;
} User_Input_Port;

#define MAX_USER_INPUT_REUSE_SIZE 1024

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Result checking                                             */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* This function is mainly responsible for checking the result of a
   read-proc or peek-proc, or checking the result of an event.

   In the mode for read-/peek-proc, if it gets an event, then it is
   supposed to sync and loop.

   In the mode of event-result checking, events are treated as errors. */

static long user_read_result(const char *who, Scheme_Input_Port *port, 
			     Scheme_Object *val, Scheme_Object *bstr, 
			     int peek, int nonblock, int evt_ok, int special_ok,
			     Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *a[1];

  val_again:
  
  if (SCHEME_EOFP(val))
    return EOF;
  else {
    int n;

    if (!SCHEME_INTP(val) || (SCHEME_INT_VAL(val) < 0)) {
      a[0] = val;
      if (SCHEME_BIGNUMP(val) && SCHEME_BIGPOS(val)) {
	n = -1;
      } else if (SCHEME_PAIRP(val)) {
	Scheme_Object *orig = val;
	a[0] = SCHEME_CDR(val);
	val = SCHEME_CAR(val);
	if (scheme_check_proc_arity(NULL, 4, 0, 1, a)
	    && ((SCHEME_INTP(val) && (SCHEME_INT_VAL(val) >= 0))
		|| (SCHEME_BIGNUMP(val) && SCHEME_BIGPOS(val)))) {
	  if (!special_ok) {
	    scheme_arg_mismatch(who, 
				"the port has no specific peek procedure, so"
				" a special read result is not allowed: ",
				orig);
	    return 0;
	  }
	  port->special_width = val;
	  port->special = a[0];
	  return SCHEME_SPECIAL;
	} else
	  val = NULL;
	n = 0;
      } else if (evt_ok && scheme_is_evt(val)) {
	/* A peek/read failed, and we were given a evt that unblocks
	   when the read/peek (at some offset) succeeds. */
	if (nonblock) {
	  if (sinfo) {
	    scheme_set_sync_target(sinfo, val, (Scheme_Object *)port, NULL, 0, 1);
	  }
	  return 0;
	} else {
	  /* Sync on the given evt. */
	  a[0] = val;
	  if (nonblock < 0)
	    val = scheme_sync(1, a);
	  else
	    val = scheme_sync_enable_break(1, a);
	  
	  /* Port may have been closed while we were syncing: */
	  if (port->closed) {
	    /* Another thread closed the input port while we were syncing. */
	    /* Call scheme_getc to signal the error */
	    if (peek)
	      scheme_peek_byte((Scheme_Object *)port);
	    else
	      scheme_get_byte((Scheme_Object *)port);
	    return 0; /* doesn't get here */
	  }
	  goto val_again;
	}
      } else {
	val = NULL;
	n = 0;
      }

      if (!val) {
	scheme_wrong_type(who, 
			  (evt_ok
			   ? (special_ok
			      ? "non-negative exact integer, eof, evt, or pair for special"
			      : "non-negative exact integer, eof, or evt")
			   : "non-negative exact integer, eof, or pair for special"),
			  -1, -1, a);
	return 0;
      }
    } else
      n = SCHEME_INT_VAL(val);

    if ((n < 0) || (n > SCHEME_BYTE_STRLEN_VAL(bstr))) {
      scheme_arg_mismatch(who,
			  "result integer is larger than the supplied string: ",
			  val);
    }

    return n;
  }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Main reader                                                 */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Call read-proc or peek-proc. This function may be called to
   implement peek-evt-proc in terms of peek-proc, in which case the
   result is an event (which packages immediate results, if that's
   what the peek function returned). */

static long 
user_get_or_peek_bytes(Scheme_Input_Port *port, 
		       char *buffer, long offset, long size,
		       int nonblock,
		       int peek, Scheme_Object *peek_skip,
		       Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *fun, *val, *a[2], *bstr;
  User_Input_Port *uip = (User_Input_Port *)port->port_data;
  long r;
  Scheme_Cont_Frame_Data cframe;

  val = uip->peeked;
  if (val) {
    /* Leftover from a read-based peek used to implement `char-ready?'
       This can't happen is peek is 1, because in that case we have a
       peek_proc, so there's no need for read-based peeks. */
    uip->peeked = NULL;
    if (SCHEME_INTP(val)) {
      buffer[offset] = SCHEME_INT_VAL(val);
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
  
  while (1) {
    
    if (uip->reuse_str && (size == SCHEME_BYTE_STRLEN_VAL(uip->reuse_str))) {
      bstr = uip->reuse_str;
      uip->reuse_str = NULL;
    } else {
      char *vb;
      vb = scheme_malloc_atomic(size + 1);
      bstr = scheme_make_sized_byte_string(vb, size, 0);
    }
    a[0] = bstr;
    a[1] = peek_skip;

    /* Disable breaks while calling the port's function: */
    scheme_push_break_enable(&cframe, 0, 0);

    /* Call the read/peek function: */
    val = scheme_apply(fun, peek ? 2 : 1, a);
    
    scheme_pop_break_enable(&cframe, 0);

    if ((size <= MAX_USER_INPUT_REUSE_SIZE)
	&& (SCHEME_INTP(val) || SCHEME_EOFP(val) || SCHEME_PAIRP(val))) {
      uip->reuse_str = bstr;
    }

    r = user_read_result(peek ? "user port peek" : "user port read",
			 port, val, bstr, peek, nonblock, 
			 1, !!uip->peek_proc, sinfo);

    if (r > 0) {
      memcpy(buffer + offset, SCHEME_BYTE_STR_VAL(bstr), r);
      return r;
    } else if (r) {
      return r;
    }
    
    scheme_thread_block_enable_break(0.0, nonblock < 0); /* penalty for inaccuracy? */
    scheme_current_thread->ran_some = 1;
    /* but don't loop forever due to inaccurracy */
    if (nonblock > 0) {
      if (sinfo)
	sinfo->spin = 1;
      return 0;
    }
  }
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Event wrapper                                               */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* If any proc produces an event, we wrap it with extra contract
   checks, and to move the content of the string that the port filled
   to the string that the reader supplied. */

static Scheme_Object *user_read_evt_wrapper(void *d, int argc, struct Scheme_Object *argv[])
{
  int peek;
  Scheme_Object *bstr, *val, *port;
  long r;
  char *buffer;
  const char *who;

  val = argv[0];
  bstr = (Scheme_Object *)((void **)d)[0];
  peek = SCHEME_TRUEP((Scheme_Object *)((void **)d)[3]);
  port = (Scheme_Object *)((void **)d)[4];
  buffer = (char *)((void **)d)[1];

  who = (buffer
	 ? (peek
	    ? "user port peek-bytes-avail!-evt" 
	    : "user port read-bytes-avail!-evt")
	 : (peek 
	    ? "user port peek-byte-evt" 
	    : "user port read-byte-evt"));

  r = user_read_result(who, (Scheme_Input_Port *)port, val, bstr, peek, 0, 0, 1, NULL);  

  if (r < 0) {
    if (r == SCHEME_SPECIAL) {
      val = scheme_get_ready_special(port, NULL, peek);
      if (SCHEME_TRUEP((Scheme_Object *)((void **)d)[5]))
	return val;
      else {
	scheme_arg_mismatch(who,
			    "unexpected non-byte value: ",
			    val);
	return NULL;
      }
    } else
      return scheme_eof;
  } else {
    long offset;
    if (buffer) {
      val = (Scheme_Object *)((void **)d)[2];
      offset = SCHEME_INT_VAL(val);
      memcpy(buffer + offset, SCHEME_BYTE_STR_VAL(bstr), r);
      return scheme_make_integer(r);
    } else {
      return scheme_make_integer(((unsigned char *)SCHEME_BYTE_STR_VAL(bstr))[0]);
    }
  }
}

static Scheme_Object *
wrap_user_read_evt(Scheme_Object *evt,
		   Scheme_Input_Port *port, Scheme_Object *bstr, char *buffer, long offset,
		   int peek, int byte_or_special)
{
  void **args;
  Scheme_Object *a[2], *wrapper;

  /* Wrap the evt for result checking and to wrap special
     results: */
  args = MALLOC_N(void*, 6);
  args[0] = bstr;
  args[1] = buffer;
  args[2] = scheme_make_integer(offset);
  args[3] = (peek ? scheme_true : scheme_false);
  args[4] = port;
  args[5] = (byte_or_special ? scheme_false : scheme_true);
  wrapper = scheme_make_closed_prim(user_read_evt_wrapper, args);

  a[0] = evt;
  a[1] = wrapper;
  return scheme_convert_evt(2, a);
}

static Scheme_Object * 
user_get_or_peek_bytes_evt(Scheme_Input_Port *port, 
			   char *buffer, long offset, long size,
			   int peek, Scheme_Object *skip,
			   int byte_or_special)
{
  Scheme_Object *bstr;
  Scheme_Object *fun, *a[2], *val;
  char *vb;
  User_Input_Port *uip = (User_Input_Port *)port->port_data;

  vb = scheme_malloc_atomic(size + 1);
  bstr = scheme_make_sized_byte_string(vb, size, 0);

  if (peek)
    fun = uip->peek_evt_proc;
  else
    fun = uip->read_evt_proc;

  a[0] = bstr;
  a[1] = skip;
  val = scheme_apply(fun, peek ? 2 : 1, a);  

  if (!scheme_is_evt(val)) {
    a[0] = val;
    scheme_wrong_type((peek 
		       ? "user port peek-bytes-avail!-evt" 
		       : "user port read-bytes-avail!-evt"), 
		      "evt", -1, -1, a);
    return NULL;
  }

  return wrap_user_read_evt(val, port, bstr, buffer, offset, peek, byte_or_special);
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*   Main entry points                                           */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

static Scheme_Object * 
user_get_bytes_evt(Scheme_Input_Port *port, 
		   char *buffer, long offset, long size,
		   int byte_or_special)
{
  return user_get_or_peek_bytes_evt(port, buffer, offset, size, 0, NULL,
				    byte_or_special);
}

static Scheme_Object * 
user_peek_bytes_evt(Scheme_Input_Port *port, 
		    char *buffer, long offset, long size,
		    Scheme_Object *skip,
		    int byte_or_special)
{
  return user_get_or_peek_bytes_evt(port, buffer, offset, size, 1, skip,
				    byte_or_special);
}

static long 
user_get_bytes(Scheme_Input_Port *port, 
	       char *buffer, long offset, long size,
	       int nonblock)
{
  return user_get_or_peek_bytes(port, buffer, offset, size, 
				nonblock, 0, NULL,
				NULL);
}

static long 
user_peek_bytes(Scheme_Input_Port *port, 
		char *buffer, long offset, long size,
		Scheme_Object *skip,
		int nonblock)
{
  return user_get_or_peek_bytes(port, buffer, offset, size, 
				nonblock, 1, skip, 
				NULL);
}

static int
user_byte_ready_sinfo(Scheme_Input_Port *port, Scheme_Schedule_Info *sinfo)
{
  int c, can_peek;
  char s[1];
  User_Input_Port *uip = (User_Input_Port *)port->port_data;

  /* We implement char-ready? by a non-blocking peek for a single
     character. If the port provides a precise waitable, it
     effectively determines the result, because the peek function
     checks the waitable. */

  can_peek = (uip->peek_proc ? 1 : 0);

  c = user_get_or_peek_bytes(port, s, 0, 1, 
			     1, can_peek, scheme_make_integer(0),
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
	uip->peeked = scheme_make_integer(s[0]);
    }
    return 1;
  } else
    return 0;
}

static int
user_byte_ready(Scheme_Input_Port *port)
{
  return user_byte_ready_sinfo(port, NULL);
}

int scheme_user_port_byte_probably_ready(Scheme_Input_Port *ip, Scheme_Schedule_Info *sinfo)
{
  User_Input_Port *uip = (User_Input_Port *)ip->port_data;

  if (uip->peeked)
    return 1;

   if (sinfo->false_positive_ok) {
    /* Causes the thread to swap in: */
    sinfo->potentially_false_positive = 1;
    return 1;
  } else {
    return user_byte_ready_sinfo(ip, sinfo);
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
}

/*========================================================================*/
/*                 "user" output ports (created from Scheme)              */
/*========================================================================*/

typedef struct User_Output_Port {
  MZTAG_IF_REQUIRED
  Scheme_Object *evt;
  Scheme_Object *write_evt_proc;
  Scheme_Object *write_proc;
  Scheme_Object *flush_proc;
  Scheme_Object *close_proc;
  Scheme_Object *write_special_evt_proc;
  Scheme_Object *write_special_proc;
} User_Output_Port;

int scheme_user_port_write_probably_ready(Scheme_Output_Port *port, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *val;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  if (port->closed)
    return 1;

  val = uop->evt;

  scheme_set_sync_target(sinfo, val, (Scheme_Object *)port, NULL, 0, 1);
  return 0;
  
}

static int
user_write_ready(Scheme_Output_Port *port)
{
  /* This function should never be called. If we are ready-checking as 
     a evt, then scheme_user_port_write_probably_ready is called,
     instead. */
  return 1;
}

static long
user_write_result(const char *who, Scheme_Output_Port *port, int evt_ok,
		  Scheme_Object *val, int rarely_block, int enable_break, long len)
{
  Scheme_Object *p[2];

  while (1) {
    if (SCHEME_FALSEP(val)) {
      if (!rarely_block)
	scheme_arg_mismatch(who,
			    "bad result for blocking mode: ",
			    val);
      else if (rarely_block == 2)
	return -1;
      else
	return 0;
    } else if (SCHEME_INTP(val) 
	       && (SCHEME_INT_VAL(val) >= 0)
	       && (SCHEME_INT_VAL(val) <= len)) {
      int n;

      n = SCHEME_INT_VAL(val);
    
      if (!rarely_block && (n != len)) {
	scheme_arg_mismatch(who,
			    "result integer for blocking mode is not the "
			    "length of the supplied string: ",
			    val);
      }

      return n;
    } else if (evt_ok && scheme_is_evt(val)) {
      /* A write failed, and we were given a evt that unblocks when
	 the write succeeds. */
      if (rarely_block == 2) {
	return 0;
      } else {
	/* Sync on the given evt. */
	p[0] = val;
	if (enable_break)
	  val = scheme_sync_enable_break(1, p);
	else
	  val = scheme_sync(1, p);
	  
	/* Port may have been closed while we were syncing: */
	if (port->closed)
	  return 0;
      }
    } else {
      if ((SCHEME_INTP(val) && (SCHEME_INT_VAL(val) > 0))
	  || (SCHEME_BIGNUMP(val) && SCHEME_BIGPOS(val))) {
	scheme_arg_mismatch(who,
			    "result integer is larger than the supplied string: ",
			    val);
      } else {
	p[0] = val;
	scheme_wrong_type(who,
			  "non-negative exact integer, #f, or evt", 
			  -1, -1, p);
      }
      return 0;
    }
  }
}

static long
user_write_bytes(Scheme_Output_Port *port, const char *str, long offset, long len, 
		  int rarely_block, int enable_break)
{
  /* As always, rarely_block => flush, !len => flush,
     rarely_block == 1 => len > 0 */
  Scheme_Object *p[5], *to_write, *val;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;
  int n, re_enable_break;
  Scheme_Cont_Frame_Data cframe;

  if (enable_break)
    re_enable_break = 1;
  else
    re_enable_break = scheme_can_break(scheme_current_thread);

  to_write = scheme_make_sized_offset_byte_string((char *)str, offset, len, 1);
  p[0] = to_write;
  SCHEME_SET_IMMUTABLE(p[0]);
  p[1] = scheme_make_integer(0);
  p[2] = scheme_make_integer(len);
  p[3] = (rarely_block ? scheme_true : scheme_false);
  p[4] = (re_enable_break ? scheme_true : scheme_false);

  while (1) {

    /* Disable breaks while calling the port's function: */
    scheme_push_break_enable(&cframe, 0, 0);

    val = scheme_apply(uop->write_proc, 5, p);

    scheme_pop_break_enable(&cframe, 0);
    
    n = user_write_result("user port write", port,
			  1, val, rarely_block, enable_break, len);
    
    if (n || (rarely_block != 1))
      return n;
    
    /* rarely_block == 1, and we haven't written anything. */
    scheme_thread_block(0.0);
    scheme_current_thread->ran_some = 1;
  }
}

static Scheme_Object *user_write_evt_wrapper(void *d, int argc, struct Scheme_Object *argv[])
{
  Scheme_Object *val, *port;
  long r, len;

  port = (Scheme_Object *)((void **)d)[0];
  val = (Scheme_Object *)((void **)d)[1];
  len = SCHEME_INT_VAL(val);
  val = argv[0];

  r = user_write_result("user port write-evt", (Scheme_Output_Port *)port, 
			0, val, 1, 0, len);

  if (!r && len) {
    /* Port must have been closed */
    scheme_arg_mismatch("user port write-evt",
			"port is closed: ",
			port);    
  }

  return scheme_make_integer(r);
}

static Scheme_Object * 
user_write_bytes_evt(Scheme_Output_Port *port, 
			  const char *buffer, long offset, long size)
{
  Scheme_Object *to_write, *wrapper;
  Scheme_Object *a[3], *val;
  void **args;
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  to_write = scheme_make_sized_offset_byte_string((char *)buffer, offset, size, 1);
  SCHEME_SET_IMMUTABLE(to_write);
  a[0] = to_write;
  a[1] = scheme_make_integer(0);
  a[2] = scheme_make_integer(size);
  val = scheme_apply(uop->write_evt_proc, 3, a);  

  if (!scheme_is_evt(val)) {
    a[0] = val;
    scheme_wrong_type("user port write-evt", "evt", -1, -1, a);
    return NULL;
  }

  /* Wrap the evt for result checking: */
  args = MALLOC_N(void*, 2);
  args[0] = port;
  args[1] = scheme_make_integer(size);
  wrapper = scheme_make_closed_prim(user_write_evt_wrapper, args);

  a[0] = val;
  a[1] = wrapper;
  return scheme_convert_evt(2, a);
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
}

static int
user_write_special (Scheme_Output_Port *port, Scheme_Object *v, int nonblock)
{
  Scheme_Object *a[2];
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  a[0] = v;
  a[1] = (nonblock ? scheme_true : scheme_false);
  v = scheme_apply(uop->write_special_proc, 2, a);

  while (1) {
    if (scheme_is_evt(v)) {
      if (!nonblock) {
	a[0] = v;
	v = scheme_sync(1, a);
      } else
	return 0;
    } else
      break;
  }

  return SCHEME_TRUEP(v);
}

static Scheme_Object*
user_write_special_evt (Scheme_Output_Port *port, Scheme_Object *v)
{
  Scheme_Object *a[1];
  User_Output_Port *uop = (User_Output_Port *)port->port_data;

  a[0] = v;
  v = scheme_apply(uop->write_special_evt_proc, 2, a);

  if (!scheme_is_evt(v)) {
    a[0] = v;
    scheme_wrong_type("user port write-special-evt", "evt", -1, -1, a);
  }

  return v;
}

int scheme_is_user_port(Scheme_Object *port)
{
  if (SCHEME_INPORTP(port)) {
    return SAME_OBJ(scheme_user_input_port_type,
		    ((Scheme_Input_Port *)port)->sub_type);
  } else {
    return SAME_OBJ(scheme_user_output_port_type,
		    ((Scheme_Output_Port *)port)->sub_type);
  }
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

static long pipe_get_or_peek_bytes(Scheme_Input_Port *p, 
				    char *buffer, long offset, long size,
				    int nonblock,
				    int peek, long peek_skip)
{
  Scheme_Pipe *pipe;
  long c;

  pipe = (Scheme_Pipe *)(p->port_data);

  if ((pipe->bufstart == pipe->bufend) && !pipe->eof) {
    if (nonblock > 0)
      return 0;

    scheme_block_until_enable_break((Scheme_Ready_Fun)scheme_byte_ready_or_user_port_ready,
				    NULL, (Scheme_Object *)p, 0.0,
				    nonblock);

    if (p->closed) {
      /* Another thread closed the input port while we were syncing. */
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
	scheme_wait_sema(my_sema, (nonblock < 0) ? -1 : 0);
      }
    }
  }

  return c;
}

static long pipe_get_bytes(Scheme_Input_Port *p, 
			    char *buffer, long offset, long size,
			    int nonblock)
{
  return pipe_get_or_peek_bytes(p, buffer, offset, size, nonblock, 0, 0);
}

static long pipe_peek_bytes(Scheme_Input_Port *p, 
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

  return pipe_get_or_peek_bytes(p, buffer, offset, size, nonblock, 1, peek_skip);
}

static long pipe_write_bytes(Scheme_Output_Port *p, 
			      const char *str, long d, long len, 
			      int rarely_block, int enable_break)
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
      xavail = pipe_write_bytes(p, str, d, xavail, rarely_block, enable_break);
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
	
	scheme_wait_sema(my_sema, enable_break ? -1 : 0);
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

static int pipe_byte_ready(Scheme_Input_Port *p)
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
  Scheme_Object *name;

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

  name = scheme_intern_symbol("pipe");

  readp = scheme_make_input_port(scheme_pipe_read_port_type,
				 (void *)pipe,
				 name,
				 scheme_get_evt_via_get,
				 pipe_get_bytes,
				 scheme_peek_evt_via_peek,
				 pipe_peek_bytes,
				 pipe_byte_ready,
				 pipe_in_close,
				 NULL,
				 0);

  writep = scheme_make_output_port(scheme_pipe_write_port_type,
				   (void *)pipe,
				   name,
				   scheme_write_evt_via_write,
				   pipe_write_bytes,
				   pipe_out_ready,
				   pipe_out_close,
				   NULL,
				   NULL,
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
  Scheme_Object *name;

  scheme_check_proc_arity("make-input-port", 1, 1, argc, argv); /* read */
  scheme_check_proc_arity2("make-input-port", 2, 2, argc, argv, 1); /* peek */
  scheme_check_proc_arity("make-input-port", 0, 3, argc, argv); /* close */
  if (argc > 4)
    scheme_check_proc_arity2("make-input-port", 1, 4, argc, argv, 1); /* read-evt */
  if (argc > 5)
    scheme_check_proc_arity2("make-input-port", 2, 5, argc, argv, 1); /* peek-evt */
  name = argv[0];

  /* It makes no sense to supply peek-evt without peek: */
  if ((argc > 5) && SCHEME_FALSEP(argv[2]) && !SCHEME_FALSEP(argv[5]))
    scheme_arg_mismatch("make-output-port",
			"peek argument is #f, but peek-evt argument is not: ",
			argv[6]);

  /* It makes no sense to supply read-evt without peek-evt: */
  if ((argc > 5) && SCHEME_FALSEP(argv[4]) && !SCHEME_FALSEP(argv[5]))
    scheme_arg_mismatch("make-output-port",
			"read-evt argument is #f, but peek-evt argument is not: ",
			argv[6]);
  /* Vice-versa: */
  if ((argc > 4) && !SCHEME_FALSEP(argv[4]) && ((argc < 6) || SCHEME_FALSEP(argv[5])))
    scheme_arg_mismatch("make-output-port",
			"peek-evt argument is #f, but read-evt argument is not: ",
			argv[6]);
  
  uip = MALLOC_ONE_RT(User_Input_Port);
#ifdef MZTAG_REQUIRED
  uip->type = scheme_rt_user_input;
#endif
  
  uip->read_proc = argv[1];
  uip->peek_proc = argv[2];
  if (SCHEME_FALSEP(uip->peek_proc))
    uip->peek_proc = NULL;
  uip->close_proc = argv[3];
  uip->read_evt_proc = ((argc > 4) ? argv[4] : scheme_false);
  if (SCHEME_FALSEP(uip->read_evt_proc))
    uip->read_evt_proc = NULL;
  uip->peek_evt_proc = ((argc > 5) ? argv[5] : scheme_false);
  if (SCHEME_FALSEP(uip->peek_evt_proc))
    uip->peek_evt_proc = NULL;

  ip = scheme_make_input_port(scheme_user_input_port_type,
			      uip,
			      name,
			      uip->read_evt_proc ? user_get_bytes_evt : NULL,
			      user_get_bytes,
			      uip->peek_evt_proc ? user_peek_bytes_evt : NULL,
			      uip->peek_proc ? user_peek_bytes : NULL,
			      user_byte_ready,
			      user_close_input,
			      user_needs_wakeup_input,
			      0);

  if (!uip->peek_proc)
    ip->pending_eof = 1; /* means that pending EOFs should be tracked */

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_output_port (int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  User_Output_Port *uop;
  Scheme_Object *name;

  if (!scheme_is_evt(argv[1])) {
    scheme_wrong_type("make-output-port", "evt", 1, argc, argv);
  }
  scheme_check_proc_arity("make-output-port", 5, 2, argc, argv); /* write */
  scheme_check_proc_arity("make-output-port", 0, 3, argc, argv); /* close */
  if (argc > 4)
    scheme_check_proc_arity2("make-output-port", 2, 4, argc, argv, 1); /* write-special */
  if (argc > 5)
  scheme_check_proc_arity2("make-output-port", 3, 5, argc, argv, 1); /* write-evt */
  if (argc > 6)
    scheme_check_proc_arity2("make-output-port", 1, 6, argc, argv, 1); /* write-special-evt */

  /* It makes no sense to supply write-special-evt without write-special: */
  if ((argc > 6) && SCHEME_FALSEP(argv[4]) && !SCHEME_FALSEP(argv[6]))
    scheme_arg_mismatch("make-output-port",
			"write-special argument is #f, but write-special-evt argument is not: ",
			argv[6]);

  /* It makes no sense to supply write-special-evt without write-evt: */
  if ((argc > 6) && SCHEME_FALSEP(argv[5]) && !SCHEME_FALSEP(argv[6]))
    scheme_arg_mismatch("make-output-port",
			"write-evt argument is #f, but write-special-evt argument is not: ",
			argv[6]);

  /* It makes no sense to supply write-evt without write-special-evt when write-special
     is provided */
  if ((argc > 5) && !SCHEME_FALSEP(argv[5]) 
      && ((argc < 7) || SCHEME_FALSEP(argv[6])) 
      && !SCHEME_FALSEP(argv[4]))
    scheme_arg_mismatch("make-output-port",
			"write-special-evt argument is #f, but write-evt argument is not, and write-special argument is not: ",
			argv[4]);
  name = argv[0];

  uop = MALLOC_ONE_RT(User_Output_Port);
#ifdef MZTAG_REQUIRED
  uop->type = scheme_rt_user_output;
#endif

  uop->evt = argv[1];
  uop->write_proc = argv[2];
  uop->close_proc = argv[3];
  uop->write_evt_proc = ((argc > 5) ? argv[5] : scheme_false);
  if (SCHEME_FALSEP(uop->write_evt_proc))
      uop->write_evt_proc = NULL;
  if ((argc < 5) || SCHEME_FALSEP(argv[4])) {
    uop->write_special_proc = NULL;
    uop->write_special_evt_proc = NULL;
  } else {
    uop->write_special_proc = argv[4];
    uop->write_special_evt_proc = ((argc > 6) ? argv[6] : scheme_false);
    if (SCHEME_FALSEP(uop->write_special_evt_proc))
      uop->write_special_evt_proc = NULL;
  }

  op = scheme_make_output_port(scheme_user_output_port_type,
			       uop,
			       name,
			       uop->write_evt_proc ? user_write_bytes_evt : NULL,
			       user_write_bytes,
			       user_write_ready,
			       user_close_output,
			       user_needs_wakeup_output,
			       uop->write_special_evt_proc ? user_write_special_evt : NULL,
			       uop->write_special_proc ? user_write_special : NULL,
			       0);

  return (Scheme_Object *)op;
}

static Scheme_Object *
open_input_file (int argc, Scheme_Object *argv[])
{
  return scheme_do_open_input_file("open-input-file", 0, argc, argv);
}

static Scheme_Object *
open_input_byte_string (int argc, Scheme_Object *argv[])
{
  if (!SCHEME_BYTE_STRINGP(argv[0]))
    scheme_wrong_type("open-input-bytes", "byte string", 0, argc, argv);

  return scheme_make_sized_byte_string_input_port(SCHEME_BYTE_STR_VAL(argv[0]), 
						  SCHEME_BYTE_STRTAG_VAL(argv[0]));
}

static Scheme_Object *
open_input_char_string (int argc, Scheme_Object *argv[])
{
  Scheme_Object *o;

  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("open-input-string", "string", 0, argc, argv);

  o = scheme_char_string_to_byte_string(argv[0]);

  return scheme_make_sized_byte_string_input_port(SCHEME_BYTE_STR_VAL(o), 
						  SCHEME_BYTE_STRTAG_VAL(o));
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
  return scheme_make_byte_string_output_port();
}

Scheme_Object *do_get_output_string(const char *who, int is_byte,
				    int argc, Scheme_Object *argv[])
{
  Scheme_Output_Port *op;
  char *s;
  long size;

  op = (Scheme_Output_Port *)argv[0];
  if (!SCHEME_OUTPORTP(argv[0]) 
      || (op->sub_type != scheme_string_output_port_type))
    scheme_wrong_type(who, "string output port", 0, argc, argv);

  s = scheme_get_sized_byte_string_output(argv[0], &size);

  if (is_byte)
    return scheme_make_sized_byte_string(s, size, 1);
  else
    return scheme_make_sized_utf8_string(s, size);
}

static Scheme_Object *
get_output_byte_string (int argc, Scheme_Object *argv[])
{
  return do_get_output_string("get-output-bytes", 1, argc, argv);
}

static Scheme_Object *
get_output_char_string (int argc, Scheme_Object *argv[])
{
  return do_get_output_string("get-output-string", 0, argc, argv);
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

static void with_close_output(void *d)
{
  scheme_close_output_port(SCHEME_CDR((Scheme_Object *)d));
}


static Scheme_Object *
with_output_to_file (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  scheme_check_proc_arity("with-output-to-file", 0, 1, argc, argv);

  port = scheme_do_open_output_file("with-output-to-file", 1, argc, argv, 0);
  
  config = scheme_extend_config(scheme_current_config(),
				MZCONFIG_OUTPUT_PORT, 
				port);

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  v = scheme_dynamic_wind(NULL,
			  with_call_thunk,
			  with_close_output,
			  NULL,
			  scheme_make_pair(argv[1], port));

  scheme_pop_continuation_frame(&cframe);

  return v;
}

static void with_close_input(void *d)
{
  scheme_close_input_port(SCHEME_CDR((Scheme_Object *)d));
}

static Scheme_Object *
with_input_from_file(int argc, Scheme_Object *argv[])
{
  Scheme_Object *port, *v;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  scheme_check_proc_arity("with-input-from-file", 0, 1, argc, argv);

  port = scheme_do_open_input_file("with-input-from-file", 1, argc, argv);

  config = scheme_extend_config(scheme_current_config(),
				MZCONFIG_INPUT_PORT, 
				port);

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  v = scheme_dynamic_wind(NULL,
			  with_call_thunk,
			  with_close_input,
			  NULL,
			  scheme_make_pair(argv[1], port));

  scheme_pop_continuation_frame(&cframe);

  return v;
}

static int check_offset_list(Scheme_Object *l)
{
  Scheme_Object *a;

  if (SCHEME_PAIRP(l)) {
    a = SCHEME_CAR(l);
    if (SCHEME_FALSEP(a) || scheme_nonneg_exact_p(a)) {
      l = SCHEME_CDR(l);
      if (SCHEME_PAIRP(l)) {
	a = SCHEME_CAR(l);
	if (SCHEME_FALSEP(a) || scheme_nonneg_exact_p(a)) {
	  l = SCHEME_CDR(l);
	  if (SCHEME_PAIRP(l)) {
	    a = SCHEME_CAR(l);
	    if (SCHEME_FALSEP(a) || scheme_nonneg_exact_p(a)) {
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
  if (SCHEME_FALSEP(line) || SCHEME_BIGNUMP(line))
    line = scheme_make_integer(-1);
  if (SCHEME_FALSEP(col) || SCHEME_BIGNUMP(col))
    col = scheme_make_integer(-1);
  if (SCHEME_FALSEP(pos) || SCHEME_BIGNUMP(pos))
    pos = scheme_make_integer(-1);

  o = MALLOC_ONE_TAGGED(Scheme_Stx_Offset);
  o->so.type = scheme_stx_offset_type;
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
      scheme_wrong_type("default-port-read-handler", "#f or list of three non-negative exact integers or #fs", 2, argc, argv);
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
    port = CURRENT_INPUT_PORT(scheme_current_config());
  
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
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if (argc > 2) {
    /* Argument should be a list: (list line col pos) */
    if (!check_offset_list(argv[2]))
      scheme_wrong_type("read-syntax", "#f or list of three non-negative exact integers or #fs", 2, argc, argv);
    
    delta = argv[2];
  }
  
  if (((Scheme_Input_Port *)port)->read_handler) {
    Scheme_Object *o[3], *result;
    o[0] = port;
    o[1] = (argc ? argv[0] : ((Scheme_Input_Port *)port)->name);
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
    Scheme_Object *src;

    src = (argc ? argv[0] : ((Scheme_Input_Port *)port)->name);

    if (SCHEME_TRUEP(delta))
      src = make_offset(delta, src);

    if (port == scheme_orig_stdin_port)
      scheme_flush_orig_outputs();

    return scheme_internal_read(port, src, -1, 0);
  }
}

static Scheme_Object *
do_read_char(char *name, int argc, Scheme_Object *argv[], int peek, int spec, int is_byte)
{
  Scheme_Object *port;
  int ch;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type(name, "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

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
    
    if (spec) {
      if (is_byte)
	ch = scheme_peek_byte_special_ok_skip(port, skip);
      else
	ch = scheme_peekc_special_ok_skip(port, skip);
    } else {
      if (is_byte)
	ch = scheme_peek_byte_skip(port, skip);
      else
	ch = scheme_peekc_skip(port, skip);
    }
  } else {
    if (spec) {
      if (is_byte)
	ch = scheme_get_byte_special_ok(port);
      else
	ch = scheme_getc_special_ok(port);
    } else {
      if (is_byte)
	ch = scheme_get_byte(port);
      else
	ch = scheme_getc(port);
    }
  }

  if (ch == SCHEME_SPECIAL) {
    return scheme_get_ready_special(port, NULL, peek);
  } else if (ch == EOF)
    return scheme_eof;
  else if (is_byte)
    return scheme_make_integer(ch);
  else
    return _scheme_make_char(ch);
}

static Scheme_Object *
read_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-char", argc, argv, 0, 0, 0);
}

static Scheme_Object *
read_char_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-char-or-special", argc, argv, 0, 1, 0);
}

static Scheme_Object *
peek_char (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-char", argc, argv, 1, 0, 0);
}

static Scheme_Object *
peek_char_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-char-or-special", argc, argv, 1, 1, 0);
}

static Scheme_Object *
read_byte (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-byte", argc, argv, 0, 0, 1);
}

static Scheme_Object *
read_byte_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("read-byte-or-special", argc, argv, 0, 1, 1);
}

static Scheme_Object *
peek_byte (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-byte", argc, argv, 1, 0, 1);
}

static Scheme_Object *
peek_byte_spec (int argc, Scheme_Object *argv[])
{
  return do_read_char("peek-byte-or-special", argc, argv, 1, 1, 1);
}

static Scheme_Object *
do_read_special_evt (const char *name, int peek, int argc, Scheme_Object *argv[], int special_ok)
{
  Scheme_Object *port, *skip;
  
  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type(name, "input-port", 0, argc, argv); 

  if (peek) {
    if (argc) {
      skip = argv[0];
      if (!(SCHEME_INTP(skip) && (SCHEME_INT_VAL(skip) >= 0))
	  && !(SCHEME_BIGNUMP(skip) && SCHEME_BIGPOS(skip))) {
	scheme_wrong_type(name, "non-negative exact integer", 0, argc, argv);
	return NULL;
      }
    } else
      skip = scheme_make_integer(0);
  } else
    skip = NULL;

  if (argc > (peek ? 1 : 0))
    port = argv[peek ? 1 : 0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());
  
  return scheme_make_read_evt(name, port,
			      NULL, 0, 1,
			      peek, skip,
			      special_ok);
}

static Scheme_Object *
read_byte_evt (int argc, Scheme_Object *argv[])
{
  return do_read_special_evt("read-byte-evt", 0, argc, argv, 0);
}

static Scheme_Object *
peek_byte_evt (int argc, Scheme_Object *argv[])
{
  return do_read_special_evt("peek-byte-evt", 0, argc, argv, 0);
}

static Scheme_Object *
read_special_evt (int argc, Scheme_Object *argv[])
{
  return do_read_special_evt("read-byte-or-special-evt", 0, argc, argv, 1);
}

static Scheme_Object *
peek_special_evt (int argc, Scheme_Object *argv[])
{
  return do_read_special_evt("peek-byte-or-special-evt", 0, argc, argv, 1);
}


static Scheme_Object *
do_read_line (int as_bytes, const char *who, int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  int ch;
  int crlf = 0, cr = 0, lf = 1;
  mzchar *buf, *oldbuf, onstack[32];
  char *bbuf, *oldbbuf;
  long size = 31, oldsize, i = 0;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type(who, "input-port", 0, argc, argv);

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
      scheme_wrong_type(who, "newline specification symbol", 1, argc, argv);
  }

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  if (as_bytes) {
    buf = NULL;
    bbuf = (char *)onstack;
    size *= sizeof(mzchar);
  } else {
    bbuf = NULL;
    buf = onstack;
  }

  while (1) {
    if (as_bytes)
      ch = scheme_get_byte(port);
    else
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

    if (as_bytes) {
      if (i >= size) {
	oldsize = size;
	oldbbuf = bbuf;
	
	size *= 2;
	bbuf = (char *)scheme_malloc_atomic(size + 1);
	memcpy(bbuf, oldbbuf, oldsize);
      }
      bbuf[i++] = ch;
    } else {
      if (i >= size) {
	oldsize = size;
	oldbuf = buf;
	
	size *= 2;
	buf = (mzchar *)scheme_malloc_atomic((size + 1) * sizeof(mzchar));
	memcpy(buf, oldbuf, oldsize * sizeof(mzchar));
      }
      buf[i++] = ch;
    }
  }

  if (as_bytes) {
    bbuf[i] = '\0';
    return scheme_make_sized_byte_string(bbuf, i, bbuf == (char *)onstack);
  } else {
    buf[i] = '\0';
    return scheme_make_sized_char_string(buf, i, buf == onstack);
  }
}

static Scheme_Object *
read_line (int argc, Scheme_Object *argv[])
{
  return do_read_line(0, "read-line", argc, argv);
}

static Scheme_Object *
read_byte_line (int argc, Scheme_Object *argv[])
{
  return do_read_line(1, "read-byte-line", argc, argv);
}


static Scheme_Object *
do_general_read_bytes(int as_bytes, 
		      const char *who, int argc, Scheme_Object *argv[],
		      int alloc_mode, int only_avail, int peek, int get_evt)
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
    if (as_bytes) {
      if (!SCHEME_MUTABLE_BYTE_STRINGP(argv[0])) {
	scheme_wrong_type(who, "mutable byte string", 0, argc, argv);
	return NULL;
      }
    } else {
      if (!SCHEME_MUTABLE_CHAR_STRINGP(argv[0])) {
	scheme_wrong_type(who, "mutable string", 0, argc, argv);
	return NULL;
      }
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
    scheme_wrong_type(who, "input port", 1+delta, argc, argv);
  
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
    port = CURRENT_INPUT_PORT(scheme_current_config());

  if ((Scheme_Object *)port == scheme_orig_stdin_port)
    scheme_flush_orig_outputs();

  if (!size) {
    if (alloc_mode) {
      if (as_bytes)
	return scheme_make_sized_byte_string("", 0, 0);
      else
	return scheme_make_sized_char_string((mzchar *)"\0\0\0", 0, 0);
    } else
      return scheme_make_integer(0);
  }

  if (alloc_mode) {
    if (size_too_big) {
      scheme_raise_out_of_memory(who, "making string of length %s",
				 scheme_make_provided_string(argv[0], 0, NULL));
      return NULL;
    }
    if (as_bytes)
      str = scheme_alloc_byte_string(size, 0);
    else
      str = scheme_alloc_char_string(size, 0);
  }

  if (get_evt) {
    return scheme_make_read_evt(who, port, 
				SCHEME_BYTE_STR_VAL(str), start, size, 
				peek, peek_skip, 0);
  } else if (as_bytes) {
    got = scheme_get_byte_string(who, port, 
				 SCHEME_BYTE_STR_VAL(str), start, size, 
				 only_avail,
				 peek, peek_skip);
  } else {
    got = scheme_get_char_string(who, port, 
				 SCHEME_CHAR_STR_VAL(str), start, size, 
				 peek, peek_skip);
  }

  if (got == EOF)
    return scheme_eof;

  if (alloc_mode) {
    if (got < size) {
      /* Ended up with a shorter string: */
      if (as_bytes)
	str = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(str), got, 1);
      else
	str = scheme_make_sized_char_string(SCHEME_CHAR_STR_VAL(str), got, 1);
    }
    return str;
  } else
    return scheme_make_integer(got);
}

static Scheme_Object *
sch_read_bytes(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes", argc, argv, 1, 0, 0, 0);
}

static Scheme_Object *
sch_read_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes!", argc, argv, 0, 0, 0, 0);
}

static Scheme_Object *
sch_peek_bytes(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes", argc, argv, 1, 0, 1, 0);
}

static Scheme_Object *
sch_peek_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes!", argc, argv, 0, 0, 1, 0);
}

static Scheme_Object *
read_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes-avail!", argc, argv, 0, 1, 0, 0);
}

static Scheme_Object *
read_bytes_bang_nonblock(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes-avail!*", argc, argv, 0, 2, 0, 0);
}

static Scheme_Object *
peek_bytes_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes-avail!", argc, argv, 0, 1, 1, 0);
}

static Scheme_Object *
peek_bytes_bang_nonblock(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes-avail!*", argc, argv, 0, 2, 1, 0);
}

static Scheme_Object *
sch_read_string(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "read-string", argc, argv, 1, 0, 0, 0);
}

static Scheme_Object *
sch_read_string_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "read-string!", argc, argv, 0, 0, 0, 0);
}

static Scheme_Object *
sch_peek_string(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "peek-string", argc, argv, 1, 0, 1, 0);
}

static Scheme_Object *
sch_peek_string_bang(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(0, "peek-string!", argc, argv, 0, 0, 1, 0);
}

static Scheme_Object *
read_bytes_bang_break(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes-avail!/enable-break", argc, argv, 0, -1, 0, 0);
}

static Scheme_Object *
peek_bytes_bang_break(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes-avail!/enable-break", argc, argv, 0, -1, 1, 0);
}

static Scheme_Object *
read_bytes_avail_evt(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "read-bytes-avail!-evt", argc, argv, 0, 1, 0, 1);
}

static Scheme_Object *
peek_bytes_avail_evt(int argc, Scheme_Object *argv[])
{
  return do_general_read_bytes(1, "peek-bytes-avail!-evt", argc, argv, 0, 1, 1, 1);
}

static Scheme_Object *
do_write_bytes_avail(int as_bytes, const char *who, 
		     int argc, Scheme_Object *argv[], 
		     int rarely_block, int get_evt)
{
  Scheme_Object *port, *str;
  long size, start, finish, putten;

  if (as_bytes && !SCHEME_BYTE_STRINGP(argv[0])) {
    scheme_wrong_type(who, "byte string", 0, argc, argv);
    return NULL;
  } else if (!as_bytes && !SCHEME_CHAR_STRINGP(argv[0])) {
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
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  if (get_evt)
    return scheme_make_write_evt(who, port,
				 NULL, SCHEME_BYTE_STR_VAL(str), start, size);
  else if (as_bytes)
    putten = scheme_put_byte_string(who, port, 
				    SCHEME_BYTE_STR_VAL(str), start, size,
				    rarely_block);
  else
    putten = scheme_put_char_string(who, port, 
				    SCHEME_CHAR_STR_VAL(str), start, size);

  if (putten < 0)
    return scheme_false;
  else
    return scheme_make_integer(putten);
}

static Scheme_Object *
write_bytes(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes", argc, argv, 0, 0);
}

static Scheme_Object *
write_bytes_avail(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail", argc, argv, 1, 0);
}

static Scheme_Object *
write_bytes_avail_break(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail", argc, argv, -1, 0);
}

static Scheme_Object *
write_bytes_avail_nonblock(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail*", argc, argv, 2, 0);
}

static Scheme_Object *
write_string(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(0, "write-string", argc, argv, 0, 0);
}

static Scheme_Object *
write_bytes_avail_evt(int argc, Scheme_Object *argv[])
{
  return do_write_bytes_avail(1, "write-bytes-avail-evt", argc, argv, 1, 1);
}

static Scheme_Object *
do_write_special(const char *name, int argc, Scheme_Object *argv[], int nonblock, int get_evt)
{
  Scheme_Object *port;
  int ok;

  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type(name, "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  if (((Scheme_Output_Port *)port)->write_special_fun) {
    if (get_evt) {
      return scheme_make_write_evt(name, port, argv[0], NULL, 0, 0);
    } else {
      Scheme_Write_Special_Fun ws = ((Scheme_Output_Port *)port)->write_special_fun;
      ok = ws((Scheme_Output_Port *)port, argv[0], nonblock);
    }
  } else {
    scheme_arg_mismatch(name,
			"port does not support special values: ",
			port);
    return NULL;
  }

  return ok ? scheme_true : scheme_false;
}

static Scheme_Object *can_write_atomic(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-writes-atomic?", "output port", 0, argc, argv);

  if (((Scheme_Output_Port *)argv[0])->write_string_evt_fun)
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *can_read_atomic(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("port-reads-atomic?", "input port", 0, argc, argv);

  if (((Scheme_Input_Port *)argv[0])->get_string_evt_fun)
    return scheme_true;
  else
    return scheme_false;  
}

static Scheme_Object *
can_write_special(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[0]))
    scheme_wrong_type("port-writes-special?", "output port", 0, argc, argv);

  if (((Scheme_Output_Port *)argv[0])->write_special_fun)
    return scheme_true;
  else
    return scheme_false;
}

static Scheme_Object *
write_special(int argc, Scheme_Object *argv[])
{
  return do_write_special("write-special", argc, argv, 0, 0);
}

static Scheme_Object *
write_special_nonblock(int argc, Scheme_Object *argv[])
{
  return do_write_special("write-special-avail*", argc, argv, 1, 0);
}

static Scheme_Object *
write_special_evt(int argc, Scheme_Object *argv[])
{
  return do_write_special("write-special-evt", argc, argv, 1, 1);
}


Scheme_Object *
scheme_call_enable_break(Scheme_Prim *prim, int argc, Scheme_Object *argv[])
{
  Scheme_Cont_Frame_Data cframe;
  Scheme_Object *v; 

  scheme_push_break_enable(&cframe, 1, 1);

  v = prim(argc, argv);

  scheme_pop_break_enable(&cframe, 0);

  return v;
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
    port = CURRENT_INPUT_PORT(scheme_current_config());
  
  return (scheme_char_ready(port) ? scheme_true : scheme_false);
}

static Scheme_Object *
byte_ready_p (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;

  if (argc && !SCHEME_INPORTP(argv[0]))
    scheme_wrong_type("byte-ready?", "input-port", 0, argc, argv);

  if (argc)
    port = argv[0];
  else
    port = CURRENT_INPUT_PORT(scheme_current_config());
  
  return (scheme_byte_ready(port) ? scheme_true : scheme_false);
}

static Scheme_Object *sch_default_display_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-display-handler", "output-port", 1, argc, argv);

  scheme_internal_display(argv[0], argv[1], scheme_current_config());

  return scheme_void;
}

static Scheme_Object *sch_default_write_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-write-handler", "output-port", 1, argc, argv);

  scheme_internal_write(argv[0], argv[1], scheme_current_config());

  return scheme_void;
}

static Scheme_Object *sch_default_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-port-print-handler", "output-port", 1, argc, argv);

  return _scheme_apply(scheme_get_param(scheme_current_config(),
					MZCONFIG_PORT_PRINT_HANDLER),
		       argc, argv);
}

static Scheme_Object *sch_default_global_port_print_handler(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_OUTPORTP(argv[1]))
    scheme_wrong_type("default-global-port-print-handler", "output-port", 1, argc, argv);

  scheme_internal_write(argv[0], argv[1], scheme_current_config());

  return scheme_void;
}

static Scheme_Object *
display_write(char *name, 
	      int argc, Scheme_Object *argv[], int escape)
{
  Scheme_Object *port;
  Scheme_Config *config;

  config = scheme_current_config();

  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type(name, "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(config);
  
  if (escape > 0) {
    if (!((Scheme_Output_Port *)port)->display_handler) {
      Scheme_Object *v = argv[0];
      if (SCHEME_BYTE_STRINGP(v)) {
	scheme_put_byte_string(name, port,
			       SCHEME_BYTE_STR_VAL(v), 0, SCHEME_BYTE_STRLEN_VAL(v),
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
    port = CURRENT_OUTPUT_PORT(scheme_current_config());
  
  (void)scheme_put_byte_string("newline", port, "\n", 0, 1, 0);

  return scheme_void;
}

static Scheme_Object *
write_byte (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  int v;
  unsigned char buffer[1];

  if (argc && !SCHEME_INTP(argv[0]))
    scheme_wrong_type("write-char", "exact integer in [0,255]", 0, argc, argv);
  v = SCHEME_INT_VAL(argv[0]);
  if ((v < 0) || (v > 255))
    scheme_wrong_type("write-char", "exact integer in [0,255]", 0, argc, argv);

  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type("write-char", "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  buffer[0] = v;

  scheme_put_byte_string("write-char", port,
			 (char *)buffer, 0, 1,
			 0);

  return scheme_void;
}

static Scheme_Object *
write_char (int argc, Scheme_Object *argv[])
{
  Scheme_Object *port;
  unsigned char buffer[MAX_UTF8_CHAR_BYTES];
  unsigned int ubuffer[1];
  int len;

  if (argc && !SCHEME_CHARP(argv[0]))
    scheme_wrong_type("write-char", "character", 0, argc, argv);
  if (argc > 1) {
    if (!SCHEME_OUTPORTP(argv[1]))
      scheme_wrong_type("write-char", "output-port", 1, argc, argv);
    port = argv[1];
  } else
    port = CURRENT_OUTPUT_PORT(scheme_current_config());

  ubuffer[0] = SCHEME_CHAR_VAL(argv[0]);
  len = scheme_utf8_encode_all(ubuffer, 1, buffer);

  scheme_put_byte_string("write-char", port,
			 (char *)buffer, 0, len,
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
} LoadHandlerData;

static void post_load_handler(void *data)
{
  LoadHandlerData *lhd = (LoadHandlerData *)data;

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
	  other = scheme_make_byte_string("something else");
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

	  other = scheme_make_sized_byte_string(s, len + slen + 1, 0);
	}

	scheme_raise_exn(MZEXN_FAIL,
			 "default-load-handler: expected a `module' declaration for `%S', found: %T in: %V",
			 lhd->expected_module,
			 other,
			 ((Scheme_Input_Port *)port)->name);

	return NULL;
      }

      /* Check no more expressions: */
      d = scheme_internal_read(port, lhd->stxsrc, 1, 0);
      if (!SCHEME_EOFP(d)) {
	scheme_raise_exn(MZEXN_FAIL,
			 "default-load-handler: expected only a `module' declaration for `%S', but found an extra expression in: %V",
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

    genv = scheme_get_env(config);
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
    scheme_raise_exn(MZEXN_FAIL,
		     "default-load-handler: expected a `module' declaration for `%S', but found end-of-file in: %V",
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
  Scheme_Object *port, *name, *expected_module, *v;
  int ch;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Config *config;
  LoadHandlerData *lhd;
  Scheme_Cont_Frame_Data cframe;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("default-load-handler", SCHEME_PATH_STRING_STR, 0, argc, argv);
  expected_module = argv[1];
  if (!SCHEME_FALSEP(expected_module) && !SCHEME_SYMBOLP(expected_module))
    scheme_wrong_type("default-load-handler", "symbol or #f", 1, argc, argv);

  port = scheme_do_open_input_file("default-load-handler", 0, 1, argv);

  /* Turn on line/column counting, unless it's a .zo file: */
  if (SCHEME_PATHP(argv[0])) {
    long len;

    len = SCHEME_BYTE_STRLEN_VAL(argv[0]);
    if ((len < 3)
	|| (SCHEME_BYTE_STR_VAL(argv[0])[len - 3] != '.')
	|| (SCHEME_BYTE_STR_VAL(argv[0])[len - 2] != 'z')
	|| (SCHEME_BYTE_STR_VAL(argv[0])[len - 1] != 'o'))
      scheme_count_lines(port);
  } else {
    long len;

    len = SCHEME_CHAR_STRLEN_VAL(argv[0]);
    if ((len < 3)
	|| (SCHEME_CHAR_STR_VAL(argv[0])[len - 3] != '.')
	|| (SCHEME_CHAR_STR_VAL(argv[0])[len - 2] != 'z')
	|| (SCHEME_CHAR_STR_VAL(argv[0])[len - 1] != 'o'))
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

  config = scheme_current_config();
  config = scheme_extend_config(config, MZCONFIG_CASE_SENS, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_SQUARE_BRACKETS_ARE_PARENS, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CURLY_BRACES_ARE_PARENS, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_GRAPH, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_COMPILED, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_BOX, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_PIPE_QUOTE, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_DOT, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_CAN_READ_QUASI, scheme_true);
  config = scheme_extend_config(config, MZCONFIG_READ_DECIMAL_INEXACT, scheme_true);

  lhd = MALLOC_ONE_RT(LoadHandlerData);
#ifdef MZTAG_REQUIRED
  lhd->type = scheme_rt_load_handler_data;
#endif
  lhd->p = p;
  lhd->config = config;
  lhd->port = port;
  name = ((Scheme_Input_Port *)port)->name;
  lhd->stxsrc = name;
  lhd->expected_module = expected_module;
  
  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  v = scheme_dynamic_wind(NULL, do_load_handler, post_load_handler,
			  NULL, (void *)lhd);

  scheme_pop_continuation_frame(&cframe);

  return v;
}

Scheme_Object *scheme_load_with_clrd(int argc, Scheme_Object *argv[],
				     char *who, int handler_param)
{
  const char *filename;
  Scheme_Object *load_dir, *a[2], *filename_path, *v;
  Scheme_Cont_Frame_Data cframe;
  Scheme_Config *config;

  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type(who, SCHEME_PATH_STRING_STR, 0, argc, argv);

  filename = scheme_expand_string_filename(argv[0],
					   who,
					   NULL,
					   SCHEME_GUARD_FILE_READ);

  /* Calculate load directory */
  load_dir = scheme_get_file_directory(filename);

  filename_path = scheme_make_sized_path((char *)filename, -1, 0);

  config = scheme_extend_config(scheme_current_config(),
				MZCONFIG_LOAD_DIRECTORY, 
				load_dir);

  scheme_push_continuation_frame(&cframe);
  scheme_set_cont_mark(scheme_parameterization_key, (Scheme_Object *)config);

  a[0] = filename_path;
  a[1] = scheme_false;
  v = _scheme_apply_multi(scheme_get_param(config, handler_param), 2, a);

  scheme_pop_continuation_frame(&cframe);

  return v;
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

    if (!SCHEME_PATH_STRINGP(d))
      return NULL;

    ed = (SCHEME_PATHP(d) ? d : scheme_char_string_to_path(d));
    s = SCHEME_BYTE_STR_VAL(ed);
    len = SCHEME_BYTE_STRTAG_VAL(ed);

    if (!scheme_is_complete_path(s, len))
      scheme_raise_exn(MZEXN_FAIL_CONTRACT,
		       "current-load-relative-directory: not a complete path: \"%q\"",
		       s);

    expanded = scheme_expand_string_filename(d, "current-load-relative-directory", NULL, 
					     SCHEME_GUARD_FILE_EXISTS);
    ed = scheme_make_sized_path(expanded, strlen(expanded), 1);

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
			     -1, abs_directory_p, "path, string, or #f", 1);
}

Scheme_Object *scheme_load(const char *file)
{
  Scheme_Object *p[1];
  mz_jmp_buf newbuf, *savebuf;
  Scheme_Object * volatile val;

  p[0] = scheme_make_path(file);
  savebuf = scheme_current_thread->error_buf;
  scheme_current_thread->error_buf = &newbuf;
  if (scheme_setjmp(newbuf)) {
    val = NULL;
  } else {
    val = scheme_apply_multi(scheme_make_prim((Scheme_Prim *)load),
			     1, p);
  }
  scheme_current_thread->error_buf = savebuf;

  return val;
}

static Scheme_Object *
transcript_on(int argc, Scheme_Object *argv[])
{
  if (!SCHEME_PATH_STRINGP(argv[0]))
    scheme_wrong_type("transcript-on", SCHEME_PATH_STRING_STR, 0, argc, argv);

  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
		   "transcript-on: not supported");

  return scheme_void;
}

static Scheme_Object *
transcript_off(int argc, Scheme_Object *argv[])
{
  scheme_raise_exn(MZEXN_FAIL_UNSUPPORTED,
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
    op = CURRENT_OUTPUT_PORT(scheme_current_config());

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
  GC_REG_TRAV(scheme_rt_indexed_string, mark_indexed_string);
  GC_REG_TRAV(scheme_rt_load_handler_data, mark_load_handler_data);
  GC_REG_TRAV(scheme_rt_user_input, mark_user_input);
  GC_REG_TRAV(scheme_rt_user_output, mark_user_output);
}

END_XFORM_SKIP;

#endif
