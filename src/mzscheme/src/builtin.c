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

#include "schpriv.h"
#include "schminc.h"

/* On the Mac, 68K, store the built-in Scheme code as pc-relative */
#if defined(__MWERKS__)
#if !defined(__POWERPC__)
#pragma pcrelstrings on
#endif
#endif

Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env)
{
  Scheme_Object *port, *expr, *saved;
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Config *config = p->config;

  port = scheme_make_sized_string_input_port(str, -len); /* negative means it's constant */

  saved = scheme_get_param(config, MZCONFIG_ENV);
  if (!env) env = (Scheme_Env *)saved;
  scheme_set_param(config, MZCONFIG_ENV, (Scheme_Object *)env);
  expr = scheme_internal_read(port, NULL, 1, 1);
  scheme_set_param(config, MZCONFIG_ENV, saved);

  return _scheme_eval_compiled(expr, env);
}

void scheme_add_embedded_builtins(Scheme_Env *env)
{
#define EVAL_ONE_STR(str) scheme_eval_string(str, env)
#define EVAL_ONE_SIZED_STR(str, len) scheme_eval_compiled_sized_string(str, len, env)

#if USE_COMPILED_STARTUP
# include "cstartup.inc"
#else
# include "startup.inc"
#endif
}

#if defined(__MWERKS__)
#if !defined(__POWERPC__)
#pragma pcrelstrings reset
#endif
#endif
