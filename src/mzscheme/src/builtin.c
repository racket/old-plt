/*
  MzScheme
  Copyright (c) 2000 Matthew Flatt

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

/* On the Mac, 68K, store the built-in Scheme code as pc-relative */
#if defined(__MWERKS__)
#if !defined(__POWERPC__)
#pragma pcrelstrings on
#endif
#endif

void scheme_add_embedded_builtins(Scheme_Env *env)
{
#define EVAL_ONE_STR(str) scheme_eval_string(str, env)
#define EVAL_ONE_SIZED_STR(str, len) scheme_eval_compiled_sized_string(str, len, env)

#if USE_COMPILED_MACROS
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
