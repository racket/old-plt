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
*/

#include "schpriv.h"
#include "schmach.h"

#define STX_GRAPH_FLAG 0x1

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);
static Scheme_Object *graph_syntax_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv);
static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_e(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_line(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_col(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src(int argc, Scheme_Object **argv);

void scheme_init_stx(Scheme_Env *env)
{
  scheme_add_global_constant("syntax?", 
			     scheme_make_folding_prim(syntax_p,
						      "syntax?",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-graph?", 
			     scheme_make_folding_prim(graph_syntax_p,
						      "syntax-graph?",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("syntax->datum", 
			     scheme_make_folding_prim(syntax_to_datum,
						      "syntax->datum",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("datum->syntax", 
			     scheme_make_folding_prim(datum_to_syntax,
						      "datum->syntax",
						      2, 2, 1),
			     env);

  scheme_add_global_constant("syntax-e", 
			     scheme_make_folding_prim(syntax_e,
						      "syntax-e",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-line", 
			     scheme_make_folding_prim(syntax_line,
						      "syntax-line",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-column", 
			     scheme_make_folding_prim(syntax_col,
						      "syntax-column",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-source", 
			     scheme_make_folding_prim(syntax_src,
						      "syntax-source",
						      1, 1, 1),
			     env);
}

Scheme_Object *scheme_make_stx(Scheme_Object *val, 
			       long line, long col, 
			       Scheme_Object *src)
{
  Scheme_Stx *stx;

  stx = MALLOC_ONE_TAGGED(Scheme_Stx);
  stx->type = scheme_stx_type;
  stx->val = val;
  stx->line = line;
  stx->col = col;
  stx->src = src;
  stx->marks = scheme_null;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_graph_stx(Scheme_Object *stx, long line, long col)
{
  ((Scheme_Stx *)stx)->hash_code |= STX_GRAPH_FLAG;

  return stx;
}

static unsigned short mark_id;

Scheme_Object *scheme_new_mark()
{
  mark_id = mark_id + 1;
  return scheme_make_pair(scheme_make_integer(mark_id), scheme_null);
}

Scheme_Object *scheme_add_mark(Scheme_Object *o, Scheme_Object *m)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *marks, *prev;

  prev = NULL;
  for (marks = stx->marks; !SCHEME_NULLP(marks); marks = SCHEME_CDR(marks)) {
    if (SAME_OBJ(SCHEME_CAR(marks), m)) {
      if (prev) {
	SCHEME_CDR(prev) = SCHEME_CDR(marks);
      } else {
	stx->marks = SCHEME_CDR(marks);
	break;
      }
    }
  }

  stx->marks = scheme_make_pair(m, stx->marks);

  return scheme_void;
}

static void add_marks(Scheme_Object *o, Scheme_Object *ml)
{
  while (!SCHEME_NULLP(ml)) {
    scheme_add_mark(o, SCHEME_CAR(ml));
    ml = SCHEME_CDR(ml);
  }
}

Scheme_Object *scheme_stx_content(Scheme_Object *o)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *marks = stx->marks;

  if (SCHEME_NULLP(marks)) {
    Scheme_Object *v = stx->val;
    Scheme_Object *ml = scheme_null;

    /* Reverse the list of marks, to preserve order: */
    while (!SCHEME_NULLP(marks)) {
      Scheme_Object *p = marks;
      marks = SCHEME_CDR(marks);
      SCHEME_CDR(p) = ml;
      ml = p;
    }

    if (SCHEME_PAIRP(v)) {
      while (SCHEME_PAIRP(v)) {
	add_marks(SCHEME_CAR(v), ml);
	v = SCHEME_CDR(v);
      }
      if (!SCHEME_NULLP(v))
	add_marks(v, ml);
    } else if (SCHEME_BOXP(v)) {
      add_marks(SCHEME_BOX_VAL(v), ml);
    } else if (SCHEME_VECTORP(v)) {
      int size = SCHEME_VEC_SIZE(v), i;

      for (i = 0; i < size; i++) {
	add_marks(SCHEME_VEC_ELS(v)[i], ml);
      }
    }

    stx->marks = scheme_null;
  }

  return stx->val;
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b)
{
  if (SCHEME_STXP(a))
    a = SCHEME_STX_VAL(a);
  if (SCHEME_STXP(b))
    b = SCHEME_STX_VAL(b);

  return SAME_OBJ(a, b);
}

int scheme_stx_free_eq(Scheme_Object *a, Scheme_Object *b)
{
  if (SCHEME_STXP(a))
    a = SCHEME_STX_VAL(a);
  if (SCHEME_STXP(b))
    b = SCHEME_STX_VAL(b);

  return SAME_OBJ(a, b);
}

/*========================================================================*/
/*                           syntax->datum                                */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht);

static Scheme_Object *syntax_to_datum_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table **ht = (Scheme_Hash_Table **)p->ku.k.p2;

  return syntax_to_datum_inner(o, ht);
}
#endif

static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *ph, *v, *result;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
# ifndef MZ_REAL_THREADS
      Scheme_Process *p = scheme_current_process;
# endif
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)ht;
      return scheme_handle_stack_overflow(syntax_to_datum_k);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (stx->hash_code & STX_GRAPH_FLAG) {
    if (!*ht)
      *ht = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);
    
    ph = (Scheme_Object *)scheme_lookup_in_table(*ht, (char *)stx);

    if (ph)
      return ph;
    else {
      ph = scheme_alloc_small_object();
      ph->type = scheme_placeholder_type;
      
      scheme_add_to_table(*ht, (char *)stx, (void *)ph, 0);
    }
  } else 
    ph = NULL;
    
  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    
    while (SCHEME_PAIRP(v)) {
      Scheme_Object *a;
      
      a = syntax_to_datum_inner(SCHEME_CAR(v), ht);
      
      p = scheme_make_pair(a, scheme_null);
      
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
      v = SCHEME_CDR(v);
    }
    if (!SCHEME_NULLP(v)) {
      v = syntax_to_datum_inner(v, ht);
      SCHEME_CDR(last) = v;
    }
    
    result = first;
  } else if (SCHEME_BOXP(v)) {
    v = syntax_to_datum_inner(SCHEME_BOX_VAL(v), ht);
    result = scheme_box(v);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    Scheme_Object *r, *a;
    
    r = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i], ht);
      SCHEME_VEC_ELS(r)[i] = a;
    }
    
    result = r;
  } else
    result = v;

  if (ph)
    SCHEME_PTR_VAL(ph) = result;

  return result;
}

Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx)
{
  Scheme_Hash_Table *ht = NULL;
  Scheme_Object *v;

  v = syntax_to_datum_inner(stx, &ht);

  if (ht)
    v = scheme_resolve_placeholders(v, 0);

  return v;
}

/*========================================================================*/
/*                           datum->syntax                                */
/*========================================================================*/


#ifdef DO_STACK_CHECK
static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
					    Scheme_Stx *stx,
					    Scheme_Hash_Table *ht);

static Scheme_Object *datum_to_syntax_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Stx *stx = (Scheme_Stx *)p->ku.k.p2;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p3;

  return datum_to_syntax_inner(o, stx, ht);
}
#endif

static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
					    Scheme_Stx *stx,
					    Scheme_Hash_Table *ht)
{
  Scheme_Object *result, *ph = NULL;

  if (SCHEME_SYNTAXP(o))
    return o;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
# ifndef MZ_REAL_THREADS
      Scheme_Process *p = scheme_current_process;
# endif
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)stx;
      p->ku.k.p3 = (void *)ht;
      return scheme_handle_stack_overflow(datum_to_syntax_k);
    }
  }
#endif

  SCHEME_USE_FUEL(1);

  if (ht) {
    if (SCHEME_PAIRP(o)
	|| SCHEME_BOXP(o)
	|| SCHEME_VECTORP(o)) {
      Scheme_Bucket *b;
      b = scheme_bucket_from_table(ht, (const char *)o);
      
      if ((long)b->val != 1) {
	if ((long)b->val & 0x1) {
	  ph = scheme_alloc_small_object();
	  ph->type = scheme_placeholder_type;
	  b->val = (void *)ph;
	} else {
	  return (Scheme_Object *)b->val;
	}
      }
    }
  }

  if (SCHEME_PAIRP(o)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    
    while (SCHEME_PAIRP(o)) {
      Scheme_Object *a;
      
      if (ht && last) {
	if ((long)scheme_lookup_in_table(ht, (const char *)o) != 1) {
	  /* cdr is shared. Stop here. */
	  break;
	}
      }

      a = datum_to_syntax_inner(SCHEME_CAR(o), stx, ht);
      
      p = scheme_make_pair(a, scheme_null);
      
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
      o = SCHEME_CDR(o);
    }
    if (!SCHEME_NULLP(o)) {
      o = datum_to_syntax_inner(o, stx, ht);
      SCHEME_CDR(last) = o;
    }

    result = first;
  } else if (SCHEME_BOXP(o)) {
    o = datum_to_syntax_inner(SCHEME_PTR_VAL(o), stx, ht);
    result = scheme_box(o);
  } else if (SCHEME_VECTORP(o)) {
    int size = SCHEME_VEC_SIZE(o), i;
      Scheme_Object *a;

      result = scheme_make_vector(size, NULL);

      for (i = 0; i < size; i++) {
	a = datum_to_syntax_inner(SCHEME_VEC_ELS(o)[i], stx, ht);
	SCHEME_VEC_ELS(result)[i] = a;
      }
  } else {
    result = o;
  }

  if (SCHEME_FALSEP(stx))
    result = scheme_make_stx(result, -1, -1, scheme_false);
  else
    result = scheme_make_stx(result, stx->line, stx->col, stx->src);

  ((Scheme_Stx *)result)->marks = stx->marks;
  if (ph) {
    ((Scheme_Stx *)result)->hash_code |= STX_GRAPH_FLAG;
    SCHEME_PTR_VAL(ph) = result;
  }

  return result;
}

Scheme_Object *scheme_datum_to_syntax(Scheme_Object *o, Scheme_Object *stx)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v;

  if (!SCHEME_FALSEP(stx) && !SCHEME_STXP(stx))
    return o;

  ht = scheme_setup_datum_graph(o, 0);

  v = datum_to_syntax_inner(o, (Scheme_Stx *)stx, ht);

  if (ht)
    v = scheme_resolve_placeholders(v, 1);

  return v;
}

/*========================================================================*/
/*                    Scheme functions and helpers                        */
/*========================================================================*/

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv)
{
  return SCHEME_STXP(argv[0]) ? scheme_true : scheme_false;
}

static Scheme_Object *graph_syntax_p(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-graph?", "syntax", 0, argc, argv);

  return ((((Scheme_Stx *)argv[0])->hash_code & STX_GRAPH_FLAG)
	  ? scheme_true
	  : scheme_false);
}


static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax->datum", "syntax", 0, argc, argv);
    
  return scheme_syntax_to_datum(argv[0]);
}

static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv)
{
  if (!SCHEME_FALSEP(argv[1]) && !SCHEME_STXP(argv[1]))
    scheme_wrong_type("datum->syntax", "syntax or #f", 1, argc, argv);
    
  return scheme_datum_to_syntax(argv[0], argv[1]);
}


static Scheme_Object *syntax_e(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-e", "syntax", 0, argc, argv);
    
  return scheme_stx_content(argv[0]);
}

static Scheme_Object *syntax_line(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-line", "syntax", 0, argc, argv);
    
  return scheme_make_integer(stx->line);
}

static Scheme_Object *syntax_col(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-column", "syntax", 0, argc, argv);
    
  return scheme_make_integer(stx->col);
}

static Scheme_Object *syntax_src(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-src", "syntax", 0, argc, argv);

  return stx->src;
}
