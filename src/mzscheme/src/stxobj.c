/*
  MzScheme
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
*/

#include "schpriv.h"
#include "schmach.h"

/* FIXME: syntax->list and resolve_env need stack checks. */

#define STX_GRAPH_FLAG 0x1

#define STX_DEBUG 0

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);
static Scheme_Object *graph_syntax_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv);
static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_e(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_line(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_col(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_to_list(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_original_p(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_property(int argc, Scheme_Object **argv);

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv);
static Scheme_Object *free_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_binding(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv);

static Scheme_Object *barrier_symbol;

static Scheme_Object *source_symbol; /* uninterned! */
static Scheme_Object *origin_symbol;
static Scheme_Object *lexical_symbol;

static Scheme_Object *mark_id = scheme_make_integer(0);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj))

typedef struct Module_Renames {
  Scheme_Type type; /* = scheme_rename_table_type */
  MZ_HASH_KEY_EX
  char plus_kernel, nonmodule;
  Scheme_Hash_Table *ht; /* localname ->  modidx  OR  (cons modidx exportname) */
  long phase;
} Module_Renames;

static Module_Renames *krn;

#define SCHEME_RENAMESP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_rename_table_type))

/* Wraps:

   A wrap is a list of wrap-elems.

   - A wrap-elem <num> is a mark
   - A wrap-elem (vector <sym> <ht> <stx> ... <sym-or-void> ...) is a lexical rename
                         env  (sym   var      var-resolved
                              ->pos)           void => not yet computed
                              or #f  sym => mark
                                      check done, var-resolved is answer to replace #f
   - A wrap-elem <rename-table> is a module rename set
         the hash table maps renamed syms to modname-srcname pairs
   - A wrap-elem (box (vector <num> <midx> <midx>)) is a phase shift
         by <num>, remapping the first <midx> to the second <midx>
   - A wrap-elem '* is a mark barrier, which is applied to the
         result of an expansion so that top-level marks do not
         break re-expansions

  The lazy_prefix field of a syntax object keeps track of how many of the
  first wraps need to be propagated to sub-syntax.
*/

/*========================================================================*/
/*                           initialization                               */
/*========================================================================*/

void scheme_init_stx(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

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

  scheme_add_global_constant("syntax-object->datum", 
			     scheme_make_folding_prim(syntax_to_datum,
						      "syntax-object->datum",
						      1, 1 + STX_DEBUG, 1),
			     env);
  scheme_add_global_constant("datum->syntax-object", 
			     scheme_make_folding_prim(datum_to_syntax,
						      "datum->syntax-object",
						      2, 3, 1),
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
  scheme_add_global_constant("syntax-position", 
			     scheme_make_folding_prim(syntax_pos,
						      "syntax-position",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax-source", 
			     scheme_make_folding_prim(syntax_src,
						      "syntax-source",
						      1, 1, 1),
			     env);
  scheme_add_global_constant("syntax->list", 
			     scheme_make_folding_prim(syntax_to_list,
						      "syntax->list",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("syntax-original?", 
			     scheme_make_prim_w_arity(syntax_original_p,
						      "syntax-original?",
						      1, 1),
			     env);
  scheme_add_global_constant("syntax-property", 
			     scheme_make_prim_w_arity(syntax_property,
						      "syntax-property",
						      2, 3),
			     env);

  scheme_add_global_constant("bound-identifier=?", 
			     scheme_make_prim_w_arity(bound_eq,
						      "bound-identifier=?",
						      2, 2),
			     env);
  scheme_add_global_constant("free-identifier=?", 
			     scheme_make_prim_w_arity(free_eq,
						      "free-identifier=?",
						      2, 2),
			     env);
  scheme_add_global_constant("module-identifier=?", 
			     scheme_make_prim_w_arity(module_eq,
						      "module-identifier=?",
						      2, 2),
			     env);
  scheme_add_global_constant("module-transformer-identifier=?", 
			     scheme_make_prim_w_arity(module_trans_eq,
						      "module-transformer-identifier=?",
						      2, 2),
			     env);

  scheme_add_global_constant("identifier-binding", 
			     scheme_make_prim_w_arity(module_binding,
						      "identifier-binding",
						      1, 1),
			     env);
  scheme_add_global_constant("identifier-transformer-binding", 
			     scheme_make_prim_w_arity(module_trans_binding,
						      "identifier-transformer-binding",
						      1, 1),
			     env);

  scheme_add_global_constant("syntax-source-module", 
			     scheme_make_folding_prim(syntax_src_module,
						      "syntax-source-module",
						      1, 1, 1),
			     env);

  REGISTER_SO(barrier_symbol);
  barrier_symbol = scheme_intern_symbol("*");

  REGISTER_SO(source_symbol);
  REGISTER_SO(origin_symbol);
  REGISTER_SO(lexical_symbol);
  source_symbol = scheme_make_symbol("source");
  origin_symbol = scheme_intern_symbol("origin");
  lexical_symbol = scheme_intern_symbol("lexical");

  REGISTER_SO(mark_id);
}

/*========================================================================*/
/*                       stx creation and maintenance                     */
/*========================================================================*/

Scheme_Object *scheme_make_stx(Scheme_Object *val, 
			       long line, long col, 
			       Scheme_Object *src,
			       Scheme_Object *props)
{
  Scheme_Stx *stx;

  stx = MALLOC_ONE_TAGGED(Scheme_Stx);
  stx->type = scheme_stx_type;
  stx->val = val;
  stx->line = line;
  stx->col = col;
  stx->src = src;
  stx->wraps = scheme_null;
  stx->props = props;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_graph_stx(Scheme_Object *stx, long line, long col)
/* Sets the "is graph" flag */
{
  ((Scheme_Stx *)stx)->hash_code |= STX_GRAPH_FLAG;

  return stx;
}

Scheme_Object *scheme_stx_track(Scheme_Object *naya, 
				Scheme_Object *old,
				Scheme_Object *origin)
/* Maintain properties for an expanded expression */
{
  Scheme_Stx *nstx = (Scheme_Stx *)naya;
  Scheme_Stx *ostx = (Scheme_Stx *)old;
  Scheme_Object *ne, *oe, *e1, *e2;
  Scheme_Object *wraps;
  long lazy_prefix;

  if (nstx->props) {
    if (SAME_OBJ(nstx->props, STX_SRCTAG)) {
      /* Retain 'source tag. */
      ne = scheme_make_pair(scheme_make_pair(source_symbol, 
					     scheme_true),
			    scheme_null);
    } else
      ne = nstx->props;
  } else
    ne = scheme_null;
  
  if (ostx->props) {
    if (SAME_OBJ(ostx->props, STX_SRCTAG)) {
      /* Drop 'source, add 'origin. */
      oe = NULL;
    } else {
      Scheme_Object *p, *a;
      int mod = 0, add = 1;

      oe = ostx->props;

      /* Drop 'source, add 'origin if not there */
      for (p = oe; SCHEME_PAIRP(p); p = SCHEME_CDR(p)) {
	a = SCHEME_CAR(SCHEME_CAR(p));
	if (SAME_OBJ(a, source_symbol))
	  mod = 1;
	else if (SAME_OBJ(a, origin_symbol))
	  mod = 1;
      }

      if (mod) {
	Scheme_Object *first = scheme_null, *last = NULL;

	for (; SCHEME_PAIRP(oe); oe = SCHEME_CDR(oe)) {
	  a = SCHEME_CAR(SCHEME_CAR(oe));
	  if (!SAME_OBJ(a, source_symbol)) {
	    if (!SAME_OBJ(a, origin_symbol)) {
	      p = scheme_make_pair(SCHEME_CAR(oe), scheme_null);
	    } else {
	      p = scheme_make_pair(scheme_make_pair(a, 
						    scheme_make_pair(SCHEME_CDR(SCHEME_CAR(oe)),
								     origin)),
				   scheme_null);
	      add = 0;
	    }

	    if (last)
	      SCHEME_CDR(last) = p;
	    else
	      first = p;
	    last = p;
	  }
	}

	oe = first;
      } 
      if (add) {
	oe = scheme_make_pair(scheme_make_pair(origin_symbol, origin),
			      oe);
      }
    }
  } else {
    /* Add 'origin. */
    oe = NULL;
  }

  if (!oe)
    oe= scheme_make_pair(scheme_make_pair(origin_symbol, origin),
			 scheme_null);

  /* Merge ne and oe (ne takes precedence). */
  
  /* First, check for overlap: */
  for (e1 = ne; SCHEME_PAIRP(e1); e1 = SCHEME_CDR(e1)) {
    Scheme_Object *a;
    a = SCHEME_CAR(SCHEME_CAR(e1));
    for (e2 = oe; SCHEME_PAIRP(e2); e2 = SCHEME_CDR(e2)) {
      if (SAME_OBJ(SCHEME_CAR(SCHEME_CAR(e2)), a)) {
	break;
      }
    }
    if (!SCHEME_NULLP(e1))
      break;
  }

  if (SCHEME_NULLP(e1)) {
    /* Can just append props info (probably the common case). */
    if (!SCHEME_NULLP(oe))
      ne = scheme_append(ne, oe);
  } else {
    /* Have to perform an actual merge: */
    Scheme_Object *first = scheme_null, *last = NULL, *p;

    for (e1 = ne; SCHEME_PAIRP(e1); e1 = SCHEME_CDR(e1)) {
      Scheme_Object *a, *v;
      a = SCHEME_CAR(SCHEME_CAR(e1));
      v = SCHEME_CDR(SCHEME_CAR(e1));
      for (e2 = oe; SCHEME_PAIRP(e2); e2 = SCHEME_CDR(e2)) {
	if (SAME_OBJ(SCHEME_CAR(SCHEME_CAR(e2)), a)) {
	  v = scheme_make_pair(v, SCHEME_CDR(SCHEME_CAR(e2)));
	  break;
	}
      }

      p = scheme_make_pair(scheme_make_pair(a, v), scheme_null);
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
    }

    for (e1 = oe; SCHEME_PAIRP(e1); e1 = SCHEME_CDR(e1)) {
      Scheme_Object *a, *v;
      a = SCHEME_CAR(SCHEME_CAR(e1));
      v = SCHEME_CDR(SCHEME_CAR(e1));
      for (e2 = ne; SCHEME_PAIRP(e2); e2 = SCHEME_CDR(e2)) {
	if (SAME_OBJ(SCHEME_CAR(SCHEME_CAR(e2)), a)) {
	  v = NULL;
	  break;
	}
      }

      if (v) {
	p = scheme_make_pair(scheme_make_pair(a, v), scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
      }
    }

    ne = first;
  }

  /* Clone nstx, keeping wraps, changing props to ne */

  wraps = nstx->wraps;
  lazy_prefix = nstx->lazy_prefix;

  nstx = (Scheme_Stx *)scheme_make_stx(nstx->val, nstx->line, nstx->col, nstx->src, ne);

  nstx->wraps = wraps;
  nstx->lazy_prefix = lazy_prefix;

  return (Scheme_Object *)nstx;
}

/******************** marks ********************/

Scheme_Object *scheme_new_mark()
{
  mark_id = scheme_add1(1, &mark_id);
  return mark_id;
}

Scheme_Object *add_remove_mark(Scheme_Object *wraps, Scheme_Object *m, long *lp)
{
  if (SCHEME_PAIRP(wraps) &&
      SAME_OBJ(m, SCHEME_CAR(wraps))
      && *lp) {
    --(*lp);
    return SCHEME_CDR(wraps);
  } else {
    (*lp)++;
    return scheme_make_pair(m, wraps);
  }
}

Scheme_Object *scheme_add_remove_mark(Scheme_Object *o, Scheme_Object *m)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;
  long lp;

  lp = stx->lazy_prefix;
  wraps = add_remove_mark(stx->wraps, m, &lp);

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->line, stx->col, stx->src, stx->props);
  stx->wraps = wraps;
  stx->lazy_prefix = lp;

  return (Scheme_Object *)stx;
}

/******************** lexical renames ********************/

Scheme_Object *scheme_make_rename(Scheme_Object *newname, int c)
{
  Scheme_Object *v;
  int i;

  v = scheme_make_vector((2 * c) + 2, NULL);
  SCHEME_VEC_ELS(v)[0] = newname;
  if (c > 15) {
    Scheme_Hash_Table *ht;
    ht = scheme_make_hash_table(SCHEME_hash_ptr);
    SCHEME_VEC_ELS(v)[1] = (Scheme_Object *)ht;
  } else 
    SCHEME_VEC_ELS(v)[1] = scheme_false;

  for (i = 0; i < c; i++) {
    SCHEME_VEC_ELS(v)[2 + c + i] = scheme_void;
  }

  return v;
}

void scheme_set_rename(Scheme_Object *rnm, int pos, Scheme_Object *oldname)
{
  SCHEME_VEC_ELS(rnm)[2 + pos] = oldname;

  /* Add ht mapping, if there's a hash table: */
  if (!SCHEME_FALSEP(SCHEME_VEC_ELS(rnm)[1])) {
    Scheme_Hash_Table *ht;
    ht = (Scheme_Hash_Table *)SCHEME_VEC_ELS(rnm)[1];
    if (scheme_hash_get(ht, oldname))
      pos = -1; /* -1 means multiple entries matching a name */
    scheme_hash_set(ht, SCHEME_STX_VAL(oldname), scheme_make_integer(pos));
  }
}

/******************** module renames ********************/

Scheme_Object *scheme_make_module_rename(long phase, int nonmodule)
{
  Module_Renames *mr;
  Scheme_Hash_Table *ht;

  mr = MALLOC_ONE_TAGGED(Module_Renames);
  mr->type = scheme_rename_table_type;

  ht = scheme_make_hash_table(SCHEME_hash_ptr);

  mr->ht = ht;
  mr->phase = phase;
  mr->nonmodule = nonmodule;

  if (!krn) {
    REGISTER_SO(krn);
    krn = mr;
  }

  return (Scheme_Object *)mr;
}

void scheme_extend_module_rename_with_kernel(Scheme_Object *mrn)
{
  /* Don't use on a non-module namespace, where renames may need
     to be removed... */
  ((Module_Renames *)mrn)->plus_kernel = 1;
}

void scheme_extend_module_rename(Scheme_Object *mrn,
				 Scheme_Object *modname, 
				 Scheme_Object *localname, 
				 Scheme_Object *exname)
{
  Scheme_Object *elem;

  if (SAME_OBJ(localname, exname))
    elem = modname;
  else
    elem = scheme_make_pair(modname, exname);

  scheme_hash_set(((Module_Renames *)mrn)->ht, localname, elem);
}

void scheme_append_module_rename(Scheme_Object *src, Scheme_Object *dest)
{
  Scheme_Hash_Table *ht, *hts;
  int i;

  if (((Module_Renames *)dest)->plus_kernel)
    ((Module_Renames *)src)->plus_kernel = 1;

  ht = ((Module_Renames *)dest)->ht;
  hts = ((Module_Renames *)src)->ht;
  
  /* Mappings in src overwrite mappings in dest: */

  for (i = hts->size; i--; ) {
    if (hts->vals[i]) {
      scheme_hash_set(ht, hts->keys[i], hts->vals[i]);
    }
  }
}

void scheme_remove_module_rename(Scheme_Object *mrn,
				 Scheme_Object *localname)
{
  scheme_hash_set(((Module_Renames *)mrn)->ht, localname, NULL);
}

/******************** wrap manipulations ********************/

Scheme_Object *scheme_add_rename(Scheme_Object *o, Scheme_Object *rename)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;
  long lp;

  wraps = scheme_make_pair(rename, stx->wraps);
  lp = stx->lazy_prefix + 1;

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->line, stx->col, stx->src, stx->props);
  stx->wraps = wraps;
  stx->lazy_prefix = lp;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_add_mark_barrier(Scheme_Object *o)
{
  return scheme_add_rename(o, barrier_symbol);
}

Scheme_Object *scheme_stx_phase_shift(Scheme_Object *stx, long shift,
				      Scheme_Object *old_midx, Scheme_Object *new_midx)
/* Shifts the phase on a syntax object in a module. A 0 shift might be used
   just to re-direct relative module paths. */
{
  Scheme_Object *vec;
  
  vec = scheme_make_vector(3, NULL);
  SCHEME_VEC_ELS(vec)[0] = scheme_make_integer(shift);
  SCHEME_VEC_ELS(vec)[1] = (new_midx ? old_midx : scheme_false);
  SCHEME_VEC_ELS(vec)[2] = (new_midx ? new_midx : scheme_false);

  return scheme_add_rename(stx, scheme_box(vec));
}

static Scheme_Object *propagate_wraps(Scheme_Object *o, 
				      int len, Scheme_Object **_wl,
				      Scheme_Object *owner_wraps)
{
  int i, mutable;
  Scheme_Object *wl, *a;

  /* Would adding the wraps generate a list equivalent to owner_wraps? */
  {
    Scheme_Stx *stx = (Scheme_Stx *)o;
    Scheme_Object *p1 = owner_wraps;

    /* Find list after |wl| items in owner_wraps: */
    for (i = 0; i < len; i++) {
      p1 = SCHEME_CDR(p1);
    }
    /* p1 is the list after wl... */
    
    if (SAME_OBJ(stx->wraps, p1)) {
      long lp = stx->lazy_prefix + len;
      stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->line, stx->col, stx->src, stx->props);
      stx->wraps = owner_wraps;
      stx->lazy_prefix = lp;
      return (Scheme_Object *)stx;
    }
  }

  wl = *_wl;
  if (!wl) {
    wl = scheme_make_vector(len, NULL);
    for (i = 0; i < len; owner_wraps = SCHEME_CDR(owner_wraps), i++) {
      SCHEME_VEC_ELS(wl)[i] = SCHEME_CAR(owner_wraps);
    }
    *_wl = wl;
  }

  mutable = 0;

  for (i = len; i--; ) {
    a = SCHEME_VEC_ELS(wl)[i];
    if (SCHEME_NUMBERP(a)) {
      if (!i || !SAME_OBJ(a, SCHEME_VEC_ELS(wl)[i - 1])) {
	if (mutable) {
	  long lp = ((Scheme_Stx *)o)->lazy_prefix;
	  a = add_remove_mark(((Scheme_Stx *)o)->wraps, a, &lp);
	  ((Scheme_Stx *)o)->wraps = a;
	  ((Scheme_Stx *)o)->lazy_prefix = lp;
	} else {
	  o = scheme_add_remove_mark(o, a);
	  mutable = 1;
	}
      } else
	i--;
    } else {
      if (mutable) {
	a = scheme_make_pair(a, ((Scheme_Stx *)o)->wraps);
	((Scheme_Stx *)o)->wraps = a;
	((Scheme_Stx *)o)->lazy_prefix += 1;
      } else {
	o = scheme_add_rename(o, a);
	mutable = 1;
      }
    }
  }

  return o;
}

Scheme_Object *scheme_stx_content(Scheme_Object *o)
/* Propagates wraps while getting a syntax object's content. */
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  if (HAS_SUBSTX(stx->val) && stx->lazy_prefix) {
    Scheme_Object *v = stx->val, *result;
    Scheme_Object *here_wraps;
    Scheme_Object *ml = NULL;
    int wl_count = 0;
    
    here_wraps = stx->wraps;
    wl_count = stx->lazy_prefix;
    stx->lazy_prefix = 0;

    if (SCHEME_PAIRP(v)) {
      Scheme_Object *last = NULL, *first = NULL;

      while (SCHEME_PAIRP(v)) {
	Scheme_Object *p;
	result = propagate_wraps(SCHEME_CAR(v), wl_count, &ml, here_wraps);
	p = scheme_make_immutable_pair(result, scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
	v = SCHEME_CDR(v);
      }
      if (!SCHEME_NULLP(v)) {
	result = propagate_wraps(v, wl_count, &ml, here_wraps);
	if (last)
	  SCHEME_CDR(last) = result;
	else
	  first = result;
      }
      v = first;
    } else if (SCHEME_BOXP(v)) {
      result = propagate_wraps(SCHEME_BOX_VAL(v), wl_count, &ml, here_wraps);
      v = scheme_box(result);
    } else if (SCHEME_VECTORP(v)) {
      Scheme_Object *v2;
      int size = SCHEME_VEC_SIZE(v), i;
      
      v2 = scheme_make_vector(size, NULL);
      
      for (i = 0; i < size; i++) {
	result = propagate_wraps(SCHEME_VEC_ELS(v)[i], wl_count, &ml, here_wraps);
	SCHEME_VEC_ELS(v2)[i] = result;
      }
      
      v = v2;
    }

    stx->val = v;
  }

  return stx->val;
}

/*========================================================================*/
/*                           stx comparison                               */
/*========================================================================*/

static int same_marks(Scheme_Object *awl, Scheme_Object *bwl, int a_ignore_barrier)
/* Compares the marks in two wraps lists */
{
  Scheme_Object *acur_mark, *bcur_mark;

  while (1) {
    /* Skip over renames and cancelled marks: */
    acur_mark = NULL;
    while (1) {
      if (SCHEME_NULLP(awl))
	break;
      if (SCHEME_NUMBERP(SCHEME_CAR(awl))) {
	if (acur_mark) {
	  if (SAME_OBJ(acur_mark, SCHEME_CAR(awl))) {
	    acur_mark = NULL;
	    awl = SCHEME_CDR(awl);
	  } else
	    break;
	} else {
	  acur_mark = SCHEME_CAR(awl);
	  awl = SCHEME_CDR(awl);
	}
      } else if (!a_ignore_barrier && SAME_OBJ(SCHEME_CAR(awl), barrier_symbol)) {
	awl = scheme_null;
      } else
	awl = SCHEME_CDR(awl);
    }
    bcur_mark = NULL;
    while (1) {
      if (SCHEME_NULLP(bwl))
	break;
      if (SCHEME_NUMBERP(SCHEME_CAR(bwl))) {
	if (bcur_mark) {
	  if (SAME_OBJ(bcur_mark, SCHEME_CAR(bwl))) {
	    bcur_mark = NULL;
	    bwl = SCHEME_CDR(bwl);
	  } else
	    break;
	} else {
	  bcur_mark = SCHEME_CAR(bwl);
	  bwl = SCHEME_CDR(bwl);
	}
      } else if (SAME_OBJ(SCHEME_CAR(bwl), barrier_symbol)) {
	bwl = scheme_null;
      } else
	bwl = SCHEME_CDR(bwl);
    }

    /* Same mark? */
    if (!SAME_OBJ(acur_mark, bcur_mark))
      return 0;

    /* Done if both reached the end: */
    if (SCHEME_NULLP(awl) && SCHEME_NULLP(bwl))
      return 1;
  }
}

#define QUICK_STACK_SIZE 10

static Scheme_Object *resolve_env(Scheme_Object *a, long phase, 
				  int w_mod, Scheme_Object **get_name)
/* Module binding ignored if w_mod is 0.
   If module bound, result is module idx, and get_name is set to source name.
   If lexically bound, result is env id, and a get_name is set to scheme_undefined.
   If neither, result is #f and get_name is unchanged. */
{
  Scheme_Object *wraps = ((Scheme_Stx *)a)->wraps;
  Scheme_Object *o_rename_stack = scheme_null;
  Scheme_Object *mresult = scheme_false;
  Scheme_Object *modidx_shift_to = NULL, *modidx_shift_from = NULL;
  Scheme_Object *rename_stack[QUICK_STACK_SIZE];
  int stack_pos = 0;
  int is_in_module = 0;
  int skip_remaining_lexes = 0;

  while (1) {
    if (SCHEME_NULLP(wraps)) {
      /* See rename case for info on rename_stack: */
      Scheme_Object *result;

      result = scheme_false;
      while (!SCHEME_NULLP(o_rename_stack)) {
	if (SAME_OBJ(SCHEME_CAAR(o_rename_stack), result))
	  result = SCHEME_CDR(SCHEME_CAR(o_rename_stack));
	o_rename_stack = SCHEME_CDR(o_rename_stack);
      }
      while (stack_pos) {
	if (SAME_OBJ(rename_stack[stack_pos - 1], result))
	  result = rename_stack[stack_pos - 2];
	stack_pos -= 2;
      }
      if (SCHEME_FALSEP(result))
	result = mresult;
      else if (get_name)
	*get_name = scheme_undefined;
      return result;
    } else if (SCHEME_RENAMESP(SCHEME_CAR(wraps)) && w_mod) {
      /* Module rename: */
      Module_Renames *mrn = (Module_Renames *)SCHEME_CAR(wraps);
      if (!is_in_module || !mrn->nonmodule) {
	if (!mrn->nonmodule)
	  is_in_module = 1;
	
	if (phase == mrn->phase) {
	  Scheme_Object *rename;
	  
	  rename = scheme_hash_get(mrn->ht, SCHEME_STX_VAL(a));
	  if (!rename && mrn->plus_kernel)
	    rename = scheme_hash_get(krn->ht, SCHEME_STX_VAL(a));
	  
	  if (rename) {
	    /* match; set mresult, which is used in the case of no lexical capture: */
	    if (SCHEME_PAIRP(rename))
	      mresult = SCHEME_CAR(rename);
	    else
	      mresult = rename;
	    
	    if (modidx_shift_from)
	      mresult = scheme_modidx_shift(mresult,
					    modidx_shift_from,
					    modidx_shift_to);

	    if (get_name) {
	      if (SCHEME_PAIRP(rename))
		*get_name = SCHEME_CDR(rename);
	      else
		*get_name = SCHEME_STX_VAL(a);
	    }
	  } else
	    mresult = scheme_false;
	}
      }
    } else if (SCHEME_BOXP(SCHEME_CAR(wraps)) && w_mod) {
      /* Phase shift */
      Scheme_Object *vec, *n, *dest, *src;
      vec = SCHEME_PTR_VAL(SCHEME_CAR(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      phase -= SCHEME_INT_VAL(n);
      
      src = SCHEME_VEC_ELS(vec)[1];
      dest = SCHEME_VEC_ELS(vec)[2];

      if (!modidx_shift_to) {
	modidx_shift_to = dest;
      } else if (!SAME_OBJ(modidx_shift_from, dest)) {
	modidx_shift_to = scheme_modidx_shift(dest,
					      modidx_shift_from,
					      modidx_shift_to);
      }

      modidx_shift_from = src;
    } else if (SCHEME_VECTORP(SCHEME_CAR(wraps))
	       && !skip_remaining_lexes) {
      /* Lexical rename: */
      Scheme_Object *rename, *renamed;
      int ri, c, istart, iend;

      rename = SCHEME_CAR(wraps);

      c = (SCHEME_VEC_SIZE(rename) - 2) >> 1;

      /* Get index from hash table, if there is one: */
      if (!SCHEME_FALSEP(SCHEME_VEC_ELS(rename)[1])) {
	void *pos;
	pos = scheme_hash_get((Scheme_Hash_Table *)(SCHEME_VEC_ELS(rename)[1]), SCHEME_STX_VAL(a));
	if (pos) {
	  istart = SCHEME_INT_VAL(pos);
	  if (istart < 0) {
	    /* -1 indicates multiple slots matching this name. */
	    istart = 0;
	    iend = c;
	  } else
	    iend = istart + 1;
	} else {
	  istart = 0;
	  iend = 0;
	}
      } else {
	istart = 0;
	iend = c;
      }

      for (ri = istart; ri < iend; ri++) {
	renamed = SCHEME_VEC_ELS(rename)[2+ri];
	if (SAME_OBJ(SCHEME_STX_VAL(a), SCHEME_STX_SYM(renamed))) {
	  if (SCHEME_SYMBOLP(renamed) 
	      || same_marks(((Scheme_Stx *)renamed)->wraps, wraps, 0)) {
	    Scheme_Object *other_env, *envname;

	    if (SCHEME_SYMBOLP(renamed)) {
	      /* Simplified table; skip future tables */
	      skip_remaining_lexes = 1;
	      other_env = scheme_false;
	      envname = SCHEME_VEC_ELS(rename)[2+c+ri];
	    } else {
	      envname = SCHEME_VEC_ELS(rename)[0];
	      other_env = SCHEME_VEC_ELS(rename)[2+c+ri];
	    	      
	      if (SCHEME_VOIDP(other_env)) {
		other_env = resolve_env(renamed, 0, 0, NULL);
		SCHEME_VEC_ELS(rename)[2+c+ri] = other_env;
	      }
	    }
	    
	    /* If it turns out that we're going to return
	       other_env, then return envname instead. */
	    if (stack_pos < QUICK_STACK_SIZE) {
	      rename_stack[stack_pos++] = envname;
	      rename_stack[stack_pos++] = other_env;
	    } else {
	      o_rename_stack = scheme_make_pair(scheme_make_pair(other_env, envname),
						o_rename_stack);
	    }

	    break;
	  }
	}
      }
    }

    wraps = SCHEME_CDR(wraps);
  }
}

static Scheme_Object *get_module_src_name(Scheme_Object *a, long phase)
/* Gets a module source name under the assumption that the identifier
   is not lexically renamed. This is used as a quick pre-test for
   module-identifer=?. */
{
  Scheme_Object *wraps = ((Scheme_Stx *)a)->wraps;
  Scheme_Object *result;
  int is_in_module = 0;

  result = NULL;

  while (1) {
    if (SCHEME_NULLP(wraps)) {
      if (!result)
	return SCHEME_STX_VAL(a);
      else
	return result;
    } else if (SCHEME_RENAMESP(SCHEME_CAR(wraps))) {
      Module_Renames *mrn = (Module_Renames *)SCHEME_CAR(wraps);

      if (!is_in_module || !mrn->nonmodule) {
	if (!mrn->nonmodule)
	  is_in_module = 1;
	
	if (phase == mrn->phase) {
	  /* Module rename: */
	  Scheme_Object *rename;
	  
	  rename = scheme_hash_get(mrn->ht, SCHEME_STX_VAL(a));
	  if (!rename && mrn->plus_kernel)
	    rename = scheme_hash_get(krn->ht, SCHEME_STX_VAL(a));
	  
	  if (rename) {
	    /* match; set result: */
	    if (SCHEME_PAIRP(rename))
	      result = SCHEME_CDR(rename);
	    else
	      result = SCHEME_STX_VAL(a);
	  } else
	    result = NULL;
	}
      }
    } else if (SCHEME_BOXP(SCHEME_CAR(wraps))) {
      /* Phase shift */
      Scheme_Object *n, *vec;
      vec = SCHEME_PTR_VAL(SCHEME_CAR(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      phase -= SCHEME_INT_VAL(n);
    }
    
    /* Keep looking: */
    wraps = SCHEME_CDR(wraps);
  }
}

int scheme_stx_free_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  Scheme_Object *asym, *bsym;

  if (!a || !b)
    return (a == b);

  if (SCHEME_STXP(a))
    asym = SCHEME_STX_VAL(a);
  else
    asym = a;
  if (SCHEME_STXP(b))
    bsym = SCHEME_STX_VAL(b);
  else
    bsym = b;

  /* Same name? */
  if (!SAME_OBJ(asym, bsym))
    return 0;

  if ((a == asym) || (b == bsym))
    return 1;
  
  a = resolve_env(a, phase, 1, NULL);
  b = resolve_env(b, phase, 1, NULL);

  a = scheme_module_resolve(a);
  b = scheme_module_resolve(b);

  /* Same binding environment? */
  return SAME_OBJ(a, b);
}

int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  Scheme_Object *asym, *bsym;

  if (!a || !b)
    return (a == b);

  if (SCHEME_STXP(a))
    asym = get_module_src_name(a, phase);
  else
    asym = a;
  if (SCHEME_STXP(b))
    bsym = get_module_src_name(b, phase);
  else
    bsym = b;

  /* Same name? */
  if (!SAME_OBJ(asym, bsym))
    return 0;

  if ((a == asym) || (b == bsym))
    return 1;
  
  a = resolve_env(a, phase, 1, NULL);
  b = resolve_env(b, phase, 1, NULL);

  a = scheme_module_resolve(a);
  b = scheme_module_resolve(b);

  /* Same binding environment? */
  return SAME_OBJ(a, b);
}

Scheme_Object *scheme_stx_module_name(Scheme_Object **a, long phase)
/* If module bound, result is module idx, and a is set to source name.
   If lexically bound, result is scheme_undefined and a is unchanged. 
   If neither, result is NULL and a is unchanged. */
{
  if (SCHEME_STXP(*a)) {
    Scheme_Object *modname, *exname = NULL;

    modname = resolve_env(*a, phase, 1, &exname);
    
    if (exname) {
      if (SAME_OBJ(exname, scheme_undefined)) {
	return scheme_undefined;
      } else {
	*a = exname;
	return modname;
      }
    } else
      return NULL;
  } else
    return NULL;
}

int scheme_stx_env_bound_eq(Scheme_Object *a, Scheme_Object *b, Scheme_Object *uid, long phase)
{
  Scheme_Object *asym, *bsym;

  if (!a || !b)
    return (a == b);

  if (SCHEME_STXP(a))
    asym = SCHEME_STX_VAL(a);
  else
    asym = a;
  if (SCHEME_STXP(b))
    bsym = SCHEME_STX_VAL(b);
  else
    bsym = b;

  /* Same name? */
  if (!SAME_OBJ(asym, bsym))
    return 0;

  if ((a == asym) || (b == bsym))
    return 1;

  if (!uid)
    if (!same_marks(((Scheme_Stx *)a)->wraps, ((Scheme_Stx *)b)->wraps, 0))
      return 0;

  a = resolve_env(a, phase, 0, NULL);
  a = scheme_module_resolve(a);

  if (uid)
    b = uid;
  else {
    b = resolve_env(b, phase, 0, NULL);
    b = scheme_module_resolve(b);
  }

  /* Same binding environment? */
  return SAME_OBJ(a, b);
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  return scheme_stx_env_bound_eq(a, b, NULL, phase);
}

Scheme_Object *scheme_stx_source_module(Scheme_Object *stx, int resolve)
{
  /* Inspect the wraps to look for a self-modidx shift: */
  Scheme_Object *w, *srcmod = scheme_false, *chain_from = NULL;

  w = ((Scheme_Stx *)stx)->wraps;

  while (!SCHEME_NULLP(w)) {
    if (SCHEME_BOXP(SCHEME_CAR(w))) {
      /* Phase shift:  */
      Scheme_Object *vec, *dest, *src;

      vec = SCHEME_PTR_VAL(SCHEME_CAR(w));
      
      src = SCHEME_VEC_ELS(vec)[1];
      dest = SCHEME_VEC_ELS(vec)[2];

      if (!chain_from) {
	srcmod = dest;
      } else if (!SAME_OBJ(chain_from, dest)) {
	srcmod = scheme_modidx_shift(dest,
				     chain_from,
				     srcmod);
      }

      chain_from = src;
    }

    w = SCHEME_CDR(w);
  }

  if (SCHEME_TRUEP(srcmod) && resolve)
    srcmod = scheme_module_resolve(srcmod);

  return srcmod;
}

/*========================================================================*/
/*                           stx and lists                                */
/*========================================================================*/

int scheme_stx_list_length(Scheme_Object *list)
{
  int len;

  if (SCHEME_STXP(list))
    list = SCHEME_STX_VAL(list);

  len = 0;
  while (!SCHEME_NULLP(list)) {
    len++;
    if (SCHEME_STXP(list))
      list = SCHEME_STX_VAL(list);
    if (SCHEME_PAIRP(list))
      list = SCHEME_CDR(list);
    else
      list = scheme_null;
  }

  return len;
}

int scheme_stx_proper_list_length(Scheme_Object *list)
{
  int len;
  Scheme_Object *turtle;

  if (SCHEME_STXP(list))
    list = SCHEME_STX_VAL(list);

  len = 0;
  turtle = list;
  while (SCHEME_PAIRP(list)) {
    len++;

    list = SCHEME_CDR(list);
    if (SCHEME_STXP(list))
      list = SCHEME_STX_VAL(list);

    if (!SCHEME_PAIRP(list))
      break;
    len++;
    list = SCHEME_CDR(list);
    if (SCHEME_STXP(list))
      list = SCHEME_STX_VAL(list);

    if (SAME_OBJ(turtle, list))
      break;

    turtle = SCHEME_CDR(turtle);
    if (SCHEME_STXP(turtle))
      turtle = SCHEME_STX_VAL(turtle);

  }
  
  if (SCHEME_NULLP(list))
    return len;

  return -1;
}

Scheme_Object *scheme_flatten_syntax_list(Scheme_Object *lst, int *islist)
{
  Scheme_Object *l = lst, *lflat, *first, *last;

  /* Check whether the list ends in a null: */
  while (SCHEME_PAIRP(l)) {
    l = SCHEME_CDR(l);
  }

  if (SCHEME_NULLP(l)) {
    /* Yes. We're done: */
    if (islist)
      *islist = 1;
    return lst;
  }

  if (islist)
    *islist = 0;

  lflat = NULL;

  /* Is it a syntax object, possibly with a list? */
  if (SCHEME_STXP(l)) {
    l = scheme_stx_content(l);
    if (SCHEME_NULLP(l) || SCHEME_PAIRP(l)) {
      int lislist;

      lflat = scheme_flatten_syntax_list(l, &lislist);
      if (!lislist) {
	/* Not a list. Can't flatten this one. */
	return lst;
      }
    } else {
      /* Not a syntax list. No chance of flattening. */
      return lst;
    }
  } else {
    /* No. No chance of flattening, then. */
    return lst;
  }

  /* Need to flatten, end with lflat */

  if (islist)
    *islist = 1;

  first = last = NULL;
  for (l = lst; SCHEME_PAIRP(l); l = SCHEME_CDR(l)) {
    Scheme_Object *p;
    p = scheme_make_immutable_pair(SCHEME_CAR(l), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }

  if (last)
    SCHEME_CDR(last) = lflat;
  else
    first = lflat;

  return first;
}

/*========================================================================*/
/*                            wraps->datum                                */
/*========================================================================*/

/* Used for marshalling syntax objects. Note that we build a reverse
   list for wraps. (Unmarshaller will reverse it back.) */

static int same_list(Scheme_Object *a, Scheme_Object *b)
{
  Scheme_Object *a1, *b1;

  while (SCHEME_PAIRP(a) && SCHEME_PAIRP(b)) {
    a1 = SCHEME_CAR(a);
    b1 = SCHEME_CAR(b);
    
    if (!SAME_OBJ(a1, b1))
      return 0;

    a = SCHEME_CDR(a);
    b = SCHEME_CDR(b);
  }

  return SCHEME_NULLP(a) && SCHEME_NULLP(b);
}

static void simplify_lex_renames(Scheme_Object *w)
{
  Scheme_Object *stack = scheme_null, *prev = NULL;
  Scheme_Object *v, *v2, *stx, *name;
  long size, vsize, psize, i, j, pos;

  while (!SCHEME_NULLP(w)) {
    if (SCHEME_VECTORP(SCHEME_CAR(w))) {

      v = SCHEME_CAR(w);
      if ((SCHEME_VEC_SIZE(v) > 2) /* a simplified vec can be empty */
	  && !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[2])) {
	/* Need to simplify, but do deepest first: */
	stack = scheme_make_pair(w, stack);
      } else {
	if (!prev)
	  prev = v;
	/* No non-simplified table can follow a simplified one */
	break;
      }
    }
    
    w = SCHEME_CDR(w);
  }

  while (!SCHEME_NULLP(stack)) {
    w = SCHEME_CAR(stack);

    v = SCHEME_CAR(w);

    vsize = (SCHEME_VEC_SIZE(v) - 2) / 2;
    if (prev)
      psize = (SCHEME_VEC_SIZE(prev) - 2) / 2;
    else
      psize = 0;

    /* Initial size; may shrink: */
    size = vsize + psize;

    v2 = scheme_make_vector(2 + (2 * size), NULL);

    pos = 0; /* counter for used slots */

    for (i = 0; i < vsize; i++) {
      stx = SCHEME_VEC_ELS(v)[2+i];
      name = SCHEME_STX_VAL(stx);
      SCHEME_VEC_ELS(v2)[2+pos] = name;
      if (same_marks(((Scheme_Stx *)stx)->wraps, w, 0)) {
	/* Either this name is in prev, in which case
	   the answer must match this rename's target, or
	   this rename's answer applies. */
	int ok = 0;

	if (prev) {
	  for (j = 0; j < psize; j++) {
	    if (SAME_OBJ(SCHEME_VEC_ELS(prev)[2+j], name)) {
	      ok = 1;
	      break;
	    }
	  }
	} else
	  ok = 1;

	if (ok) {
	  SCHEME_VEC_ELS(v2)[2+size+pos] = SCHEME_VEC_ELS(v)[0];
	  pos++;
	}
      }
    }

    if (prev) {
      /* Check for elements in prev not in v; copy them over: */
      for (i = 0; i < psize; i++) {
	for (j = 0; j < vsize; j++) {
	  if (SAME_OBJ(SCHEME_VEC_ELS(prev)[2+i],
		       SCHEME_STX_VAL(SCHEME_VEC_ELS(v)[2+j])))
	    break;
	}

	if (j < vsize) {
	  /* Copy: */
	  SCHEME_VEC_ELS(v2)[2+pos] = SCHEME_VEC_ELS(prev)[2+i];
	  SCHEME_VEC_ELS(v2)[2+size+pos] = SCHEME_VEC_ELS(prev)[2+psize+i];
	  pos++;
	}
      }
    }
    
    if (pos != size) {
      /* Shrink simplified vector */
      v = v2;
      v2 = scheme_make_vector(2 + (2 * pos), NULL);
      for (i = 0; i < pos; i++) {
	SCHEME_VEC_ELS(v2)[2+i] = SCHEME_VEC_ELS(prev)[2+i];
	SCHEME_VEC_ELS(v2)[2+pos+i] = SCHEME_VEC_ELS(prev)[2+size+i];
      }
    }

    SCHEME_VEC_ELS(v2)[0] = scheme_false;
    SCHEME_VEC_ELS(v2)[1] = scheme_false;

    SCHEME_CAR(w) = v2;

    prev = v2;
    
    stack = SCHEME_CDR(stack);
  }
}

static Scheme_Object *wraps_to_datum(Scheme_Object *w_in, 
				     Scheme_Hash_Table *rns)
{
  Scheme_Object *stack, *a, *w = w_in;
  int did_lex_rename = 0;

  a = scheme_hash_get(rns, w_in);
  if (a)
    return SCHEME_CAR(a);

  stack = scheme_null;

  simplify_lex_renames(w);

  while (!SCHEME_NULLP(w)) {
    a = SCHEME_CAR(w);
    if (SCHEME_NUMBERP(a)) {
      /* Mark numbers get parenthesized */
      if (SCHEME_PAIRP(SCHEME_CDR(w)) && SAME_OBJ(a, SCHEME_CADR(w)))
	w = SCHEME_CDR(w); /* delete cancelled mark */
      else
	stack = scheme_make_pair(scheme_make_pair(a, scheme_null), stack);
    } else if (SCHEME_VECTORP(a)) {
      if (!did_lex_rename) {
	if (SCHEME_VEC_SIZE(a) > 2) {
	  Scheme_Object *local_key;
	  
	  local_key = scheme_hash_get(rns, a);
	  if (local_key) {
	    stack = scheme_make_pair(local_key, stack);
	  } else {
	    local_key = scheme_make_integer(rns->count);
	    scheme_hash_set(rns, a, local_key);
	    
	    /* Since this is a simplified table, we can steal the first
	       slot for local_key: */
	    
	    SCHEME_VEC_ELS(a)[0] = local_key;
	    
	    stack = scheme_make_pair(a, stack);
	  }
	}
	/* else empty simplified vector, which we drop */

	did_lex_rename = 1;
      }
    } else if (SCHEME_RENAMESP(a)) {
      Module_Renames *mrn = (Module_Renames *)a;
      int redundant = 0;
      
      {
	/* Check for later [non]module rename at the same phase: */
	long shift = 0;	
	Scheme_Object *l;
	
	for (l = SCHEME_CDR(w); !SCHEME_NULLP(l); l = SCHEME_CDR(l)) {
	  if (SCHEME_RENAMESP(SCHEME_CAR(l))) {
	    Module_Renames *lrn = (Module_Renames *)SCHEME_CAR(l);
	    if (((!!lrn->nonmodule) == (!!mrn->nonmodule))
		&& ((lrn->phase + shift) == mrn->phase)) {
	      /* mrn is redundant */
	      redundant = 1;
	      break;
	    }
	  } else if (SCHEME_BOXP(SCHEME_CAR(l))) {
	    shift += SCHEME_INT_VAL(SCHEME_VEC_ELS(SCHEME_PTR_VAL(SCHEME_CAR(l)))[0]);
	  }
	}
      }

      if (!redundant) {
	if (mrn->nonmodule) {
	  stack = scheme_make_pair(((mrn->phase == 0)
				    ? scheme_true
				    : scheme_false), 
				   stack);
	} else {
	  Scheme_Object *local_key;
	  
	  local_key = scheme_hash_get(rns, (Scheme_Object *)mrn);
	  if (local_key) {
	    stack = scheme_make_pair(local_key, stack);
	  } else {
	    /* Convert hash table to list: */
	    int i, j, count = 0;
	    Scheme_Object *l;
	    
	    for (i = mrn->ht->size; i--; ) {
	      if (mrn->ht->vals[i])
		count++;
	    }

	    l = scheme_make_vector(count * 2, NULL);
	    
	    for (i = mrn->ht->size, j = 0; i--; ) {
	      if (mrn->ht->vals[i]) {
		SCHEME_VEC_ELS(l)[j++] = mrn->ht->keys[i];
		SCHEME_VEC_ELS(l)[j++] = mrn->ht->vals[i];
	      }
	    }
	    
	    local_key = scheme_make_integer(rns->count);
	    scheme_hash_set(rns, a, local_key);
	    
	    l = scheme_make_pair(scheme_make_integer(mrn->phase), l);
	    if (mrn->plus_kernel)
	      l = scheme_make_pair(scheme_true,l);
	    l = scheme_make_pair(local_key, l);
	    
	    stack = scheme_make_pair(l, stack);
	  }
	}
      }
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
      stack = scheme_make_pair(a, stack);
    } else {
      /* box, a phase shift */
      /* Any more rename tables? */
      Scheme_Object *l = SCHEME_CDR(w);
      while (!SCHEME_NULLP(l)) {
	if (SCHEME_RENAMESP(SCHEME_CAR(l)))
	  break;
	l = SCHEME_CDR(l);
      }
      /* If l is scheme_null, don't need the phase shift */
      if (SCHEME_PAIRP(l)) {
	stack = scheme_make_pair(a, stack);
      }
    }

    w = SCHEME_CDR(w);
  }

  /* Double-check for equivalent list in table (after simplificiation: */
  {
    int i;

    for (i = rns->size; i--; ) {
      if (rns->vals[i]) {
	if (SCHEME_PAIRP(rns->vals[i])) {
	  if (same_list(SCHEME_CDR(rns->vals[i]), stack)) {
	    return SCHEME_CAR(rns->vals[i]);
	  }
	}
      }
    }
  }

  /* Create a key for this wrap set: */
  a = scheme_make_integer(rns->count);
  scheme_hash_set(rns, w_in, scheme_make_pair(a, stack));
  
  return scheme_make_pair(a, stack);
}

/*========================================================================*/
/*                           syntax->datum                                */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht,
					    int with_marks,
					    Scheme_Hash_Table *rns);

static Scheme_Object *syntax_to_datum_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table **ht = (Scheme_Hash_Table **)p->ku.k.p2;
  Scheme_Hash_Table *rns = (Scheme_Hash_Table *)p->ku.k.p3;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;

  return syntax_to_datum_inner(o, ht, p->ku.k.i1, rns);
}
#endif

static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht,
					    int with_marks,
					    Scheme_Hash_Table *rns)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *ph, *v, *result;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)ht;
      p->ku.k.i1 = with_marks;
      p->ku.k.p3 = (void *)rns;
      return scheme_handle_stack_overflow(syntax_to_datum_k);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (stx->hash_code & STX_GRAPH_FLAG) {
    if (!*ht)
      *ht = scheme_make_hash_table(SCHEME_hash_ptr);
    
    ph = scheme_hash_get(*ht, (Scheme_Object *)stx);

    if (ph)
      return ph;
    else {
      ph = scheme_alloc_small_object();
      ph->type = scheme_placeholder_type;
      
      scheme_hash_set(*ht, (Scheme_Object *)stx, (Scheme_Object *)ph);
    }
  } else 
    ph = NULL;

  if (with_marks) {
    /* Propagate marks: */
    scheme_stx_content((Scheme_Object *)stx);
  }

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    
    while (SCHEME_PAIRP(v)) {
      Scheme_Object *a;

      a = syntax_to_datum_inner(SCHEME_CAR(v), ht, with_marks, rns);
      
      p = scheme_make_pair(a, scheme_null);
      
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
      v = SCHEME_CDR(v);
    }
    if (!SCHEME_NULLP(v)) {
      v = syntax_to_datum_inner(v, ht, with_marks, rns);
      SCHEME_CDR(last) = v;
    }
    
    result = first;
  } else if (SCHEME_BOXP(v)) {
    v = syntax_to_datum_inner(SCHEME_BOX_VAL(v), ht, with_marks, rns);
    result = scheme_box(v);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    Scheme_Object *r, *a;
    
    r = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i], ht, with_marks, rns);
      SCHEME_VEC_ELS(r)[i] = a;
    }
    
    result = r;
  } else if ((with_marks == 1) && SCHEME_SYMBOLP(v)) {
    result = scheme_make_pair(v, stx->wraps);
  } else
    result = v;

  if (with_marks > 1)
    result = scheme_make_pair(result, wraps_to_datum(stx->wraps, rns));

  if (ph)
    SCHEME_PTR_VAL(ph) = result;

  return result;
}

Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx, int with_marks,
				      Scheme_Hash_Table *rns)
{
  Scheme_Hash_Table *ht = NULL;
  Scheme_Object *v;

  v = syntax_to_datum_inner(stx, &ht, with_marks, rns);

  if (ht)
    v = scheme_resolve_placeholders(v, 0);

  return v;
}

/*========================================================================*/
/*                           syntax is graph?                             */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static int syntax_is_graph_inner(Scheme_Object *o);

static Scheme_Object *syntax_is_graph_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;

  p->ku.k.p1 = NULL;

  return (Scheme_Object *)syntax_is_graph_inner(o);
}
#endif

static int syntax_is_graph_inner(Scheme_Object *o)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *v;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      return (int)scheme_handle_stack_overflow(syntax_is_graph_k);
    }
  }
#endif
  SCHEME_USE_FUEL(1);

  if (stx->hash_code & STX_GRAPH_FLAG)
    return 1;

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    while (SCHEME_PAIRP(v)) {
      if (syntax_is_graph_inner(SCHEME_CAR(v)))
	return 1;
      v = SCHEME_CDR(v);
    }
    if (!SCHEME_NULLP(v)) {
      if (syntax_is_graph_inner(v))
	return 1;
    }
    return 0;
  } else if (SCHEME_BOXP(v)) {
    return syntax_is_graph_inner(SCHEME_BOX_VAL(v));
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    
    for (i = 0; i < size; i++) {
      if (syntax_is_graph_inner(SCHEME_VEC_ELS(v)[i]))
	return 1;
    }
    
    return 0;
  } else
    return 0;
}

int scheme_syntax_is_graph(Scheme_Object *stx)
{
  return syntax_is_graph_inner(stx);
}

/*========================================================================*/
/*                            datum->wraps                                */
/*========================================================================*/

static Scheme_Object *datum_to_wraps(Scheme_Object *w,
				     Scheme_Hash_Table *rns)
{
  Scheme_Object *stack, *a, *wraps_key;

  /* rns maps numbers (table indices) to renaming tables, and negative
     numbers (negated fixnum marks) and symbols (interned marks) to marks */

  if (SCHEME_INTP(w))
    return scheme_hash_get(rns, w);

  stack = scheme_null;

  wraps_key = SCHEME_CAR(w);
  w = SCHEME_CDR(w);

  while (!SCHEME_NULLP(w)) {
    a = SCHEME_CAR(w);
    if (SCHEME_NUMBERP(a)) {
      /* Re-use rename table or env rename */
      a = scheme_hash_get(rns, a);
      if (!a) {
	scheme_read_err(scheme_false, -1, -1, 0,
			"read (compiled): unknown rename table index: %d",
			SCHEME_INT_VAL(a));
      }
      stack = scheme_make_pair(a, stack);
    } else if (SCHEME_PAIRP(a) 
	       && SCHEME_NULLP(SCHEME_CDR(a))
	       && SCHEME_NUMBERP(SCHEME_CAR(a))) {
      /* Mark */
      Scheme_Object *n;

      a = SCHEME_CAR(a);

      if (SCHEME_INTP(a))
	a = scheme_make_integer(-SCHEME_INT_VAL(a));
      else
	a = scheme_intern_symbol(scheme_number_to_string(10, a));

      /* Picked a mapping yet? */
      n = scheme_hash_get(rns, a);
      if (!n) {
	/* Map marshalled mark to a new mark. */
	n = scheme_new_mark();
	scheme_hash_set(rns, a, n);
      }

      stack = scheme_make_pair(n, stack);
    } else if (SCHEME_VECTORP(a)) {
      Scheme_Object *local_key = SCHEME_VEC_ELS(a)[0];
      
      scheme_hash_set(rns, local_key, a);

      stack = scheme_make_pair(a, stack);
    } else if (SCHEME_PAIRP(a)) {
      /* A rename table:
           - (<index-num> [#t] <phase-num> . #(<table-elem> ...))
	where a <table-elem> is actually two values, one of:
           - <exname> <modname>
           - <exname> (<modname> . <defname>)
      */
      Scheme_Object *local_key;
      Module_Renames *mrn;
      Scheme_Object *p, *key;
      int plus_kernel, i, count;
      long phase;
      
      local_key = SCHEME_CAR(a);
      a = SCHEME_CDR(a);
      
      /* Convert list to rename table: */
      
      if (SCHEME_BOOLP(SCHEME_CAR(a))) {
	plus_kernel = 1;
	a = SCHEME_CDR(a);
      } else
	plus_kernel = 0;

      phase = SCHEME_INT_VAL(SCHEME_CAR(a));
      a = SCHEME_CDR(a);

      mrn = (Module_Renames *)scheme_make_module_rename(phase, 0);
      mrn->plus_kernel = plus_kernel;

      count = SCHEME_VEC_SIZE(a);
      for (i = 0; i < count; i+= 2) {
	key = SCHEME_VEC_ELS(a)[i];
	p = SCHEME_VEC_ELS(a)[i+1];
	  
	scheme_hash_set(mrn->ht, key, p);
      }

      scheme_hash_set(rns, local_key, (Scheme_Object *)mrn);

      stack = scheme_make_pair((Scheme_Object *)mrn, stack);
    } else if (SAME_OBJ(a, scheme_true)) {
      /* current env rename */
      Scheme_Env *env = (Scheme_Env *)scheme_get_param(scheme_current_thread->config, MZCONFIG_ENV);
      stack = scheme_make_pair(env->rename, stack);
    } else if (SCHEME_FALSEP(a)) {
      /* current exp-env rename */
      Scheme_Env *env = (Scheme_Env *)scheme_get_param(scheme_current_thread->config, MZCONFIG_ENV);
      scheme_prepare_exp_env(env);
      if (!env->exp_env->rename) {
	Scheme_Object *rn;
	rn = scheme_make_module_rename(1, 1);
	env->exp_env->rename = rn;
      }
      stack = scheme_make_pair(env->exp_env->rename, stack);
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
      stack = scheme_make_pair(a, stack);
    } else {
      /* must be a box for a phase shift */
      stack = scheme_make_pair(a, stack);
    }

    w = SCHEME_CDR(w);
  }

  scheme_hash_set(rns, wraps_key, stack);

  return stack;
}

/*========================================================================*/
/*                           datum->syntax                                */
/*========================================================================*/


#ifdef DO_STACK_CHECK
static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
					    Scheme_Stx *stx_src,
					    Scheme_Stx *stx_wraps,
					    Scheme_Hash_Table *ht);

static Scheme_Object *datum_to_syntax_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Stx *stx_src = (Scheme_Stx *)p->ku.k.p2;
  Scheme_Stx *stx_wraps = (Scheme_Stx *)p->ku.k.p3;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return datum_to_syntax_inner(o, stx_src, stx_wraps, ht);
}
#endif

static Scheme_Object *datum_to_syntax_inner(Scheme_Object *o, 
					    Scheme_Stx *stx_src,
					    Scheme_Stx *stx_wraps, /* or rename table */
					    Scheme_Hash_Table *ht)
{
  Scheme_Object *result, *ph = NULL, *wraps;

  if (SCHEME_STXP(o))
    return o;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)stx_src;
      p->ku.k.p3 = (void *)stx_wraps;
      p->ku.k.p4 = (void *)ht;
      return scheme_handle_stack_overflow(datum_to_syntax_k);
    }
  }
#endif

  SCHEME_USE_FUEL(1);

  if (ht) {
    if (HAS_SUBSTX(o)) {
      long val;

      val = (long)scheme_hash_get(ht, o);
      
      if (val != 1) {
	if (val & 0x1) {
	  ph = scheme_alloc_small_object();
	  ph->type = scheme_placeholder_type;
	  scheme_hash_set(ht, o, (Scheme_Object *)ph);
	} else {
	  return (Scheme_Object *)val;
	}
      }
    }
  }

  if ((Scheme_Object *)SCHEME_HASHTP(stx_wraps)) {
    wraps = SCHEME_CDR(o);
    o = SCHEME_CAR(o);
  } else
    wraps = NULL;

  if (SCHEME_PAIRP(o)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    
    /* Check whether it's all immutable conses with
       syntax inside */
    p = o;
    while (SCHEME_IMMUTABLE_PAIRP(p)) {
      if (!SCHEME_STXP(SCHEME_CAR(p)))
	break;
      p = SCHEME_CDR(p);
    }
    if (SCHEME_NULLP(p) || SCHEME_STXP(p)) {
      result = o;
    } else {
      /* Build up a new list while converting elems */
      while (SCHEME_PAIRP(o)) {
	Scheme_Object *a;
      
	if (wraps) {
	  if (!SCHEME_PAIRP(SCHEME_CAR(o)))
	    break;
	}

	if (ht && last) {
	  if ((long)scheme_hash_get(ht, o) != 1) {
	    /* cdr is shared. Stop here. */
	    break;
	  }
	}

	a = datum_to_syntax_inner(SCHEME_CAR(o), stx_src, stx_wraps, ht);
      
	p = scheme_make_immutable_pair(a, scheme_null);
      
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
	o = SCHEME_CDR(o);
      }
      if (!SCHEME_NULLP(o)) {
	o = datum_to_syntax_inner(o, stx_src, stx_wraps, ht);
	SCHEME_CDR(last) = o;
      }

      result = first;
    }
  } else if (SCHEME_BOXP(o)) {
    o = datum_to_syntax_inner(SCHEME_PTR_VAL(o), stx_src, stx_wraps, ht);
    result = scheme_box(o);
    SCHEME_SET_BOX_IMMUTABLE(result);
  } else if (SCHEME_VECTORP(o)) {
    int size = SCHEME_VEC_SIZE(o), i;
    Scheme_Object *a;

    result = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = datum_to_syntax_inner(SCHEME_VEC_ELS(o)[i], stx_src, stx_wraps, ht);
      SCHEME_VEC_ELS(result)[i] = a;
    }

    SCHEME_SET_VECTOR_IMMUTABLE(result);
  } else {
    result = o;
  }

  if (SCHEME_FALSEP((Scheme_Object *)stx_src))
    result = scheme_make_stx(result, -1, -1, scheme_false, NULL);
  else
    result = scheme_make_stx(result, stx_src->line, stx_src->col, stx_src->src, NULL);

  if (wraps) {
    wraps = datum_to_wraps(wraps, (Scheme_Hash_Table *)stx_wraps);
    ((Scheme_Stx *)result)->wraps = wraps;
  } else if (SCHEME_FALSEP((Scheme_Object *)stx_wraps)) {
    /* wraps already nulled */
  } else {
    /* Note: no propagation will be needed for SUBSTX */
    ((Scheme_Stx *)result)->wraps = stx_wraps->wraps;
  }
  
  if (ph) {
    ((Scheme_Stx *)result)->hash_code |= STX_GRAPH_FLAG;
    SCHEME_PTR_VAL(ph) = result;
  }

  return result;
}

Scheme_Object *scheme_datum_to_syntax(Scheme_Object *o, 
				      Scheme_Object *stx_src,
				      Scheme_Object *stx_wraps,
				      int can_graph, int copy_props)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v;

  if (!SCHEME_FALSEP(stx_src) && !SCHEME_STXP(stx_src))
    return o;

  if (SCHEME_STXP(o))
    return o;

  if (can_graph && HAS_SUBSTX(o))
    ht = scheme_setup_datum_graph(o, 0);
  else
    ht = NULL;

  v = datum_to_syntax_inner(o, 
			    (Scheme_Stx *)stx_src, 
			    (Scheme_Stx *)stx_wraps,
			    ht);

  if (ht)
    v = scheme_resolve_placeholders(v, 1);

  if (copy_props)
    ((Scheme_Stx *)v)->props = ((Scheme_Stx *)stx_src)->props;

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
    scheme_wrong_type("syntax-object->datum", "syntax", 0, argc, argv);
    
#if STX_DEBUG
  if (argc == 2)
      return scheme_syntax_to_datum(argv[0], 1, scheme_make_hash_table(SCHEME_hash_ptr));
#endif


  return scheme_syntax_to_datum(argv[0], 0, NULL);
}

static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv)
{
  if (!SCHEME_FALSEP(argv[0]) && !SCHEME_STXP(argv[0]))
    scheme_wrong_type("datum->syntax-object", "syntax or #f", 0, argc, argv);
  if (argc > 2)
    if (!SCHEME_FALSEP(argv[2]) && !SCHEME_STXP(argv[2]))
      scheme_wrong_type("datum->syntax-object", "syntax or #f", 2, argc, argv);
    
  return scheme_datum_to_syntax(argv[1], 
				(argc > 2) ? argv[2] : scheme_false, 
				argv[0], 1, 0);
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
    
  if (stx->line < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->line);
}

static Scheme_Object *syntax_col(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-column", "syntax", 0, argc, argv);
    
  if (stx->line < 0) /* => col, if present, is really position */
    return scheme_false;
  else
    return scheme_make_integer(stx->col);
}

static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-position", "syntax", 0, argc, argv);
    
  /* line < 0  => col, if present, is really position */

  if ((stx->line >= 0) || (stx->col < 0))
    return scheme_false;
  else
    return scheme_make_integer(stx->col);
}

static Scheme_Object *syntax_src(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-src", "syntax", 0, argc, argv);

  return stx->src;
}

static Scheme_Object *syntax_to_list(int argc, Scheme_Object **argv)
{
  Scheme_Object *l;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax->list", "syntax", 0, argc, argv);

  l = scheme_stx_content(argv[0]);
  if (SCHEME_NULLP(l))
    return scheme_null;
  else if (SCHEME_PAIRP(l)) {
    int islist;
    l = scheme_flatten_syntax_list(l, &islist);
    if (islist)
      return l;
    else
      return scheme_false;
  } else
    return scheme_false;
}

static Scheme_Object *syntax_original_p(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx;

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-original?", "syntax", 0, argc, argv);

  stx = (Scheme_Stx *)argv[0];

  if (stx->props) {
    if (SAME_OBJ(stx->props, STX_SRCTAG)) {
      /* Check for marks... */
    } else {
      Scheme_Object *e;

      for (e = stx->props; SCHEME_PAIRP(e); e = SCHEME_CDR(e)) {
	if (SAME_OBJ(source_symbol, SCHEME_CAR(SCHEME_CAR(e)))) {
	  break;
	}
      }

      if (SCHEME_NULLP(e))
	return scheme_false;
    }
  } else
    return scheme_false;

  if (same_marks(stx->wraps, scheme_null, 1))
    return scheme_true;
  else
    return scheme_false;
}

Scheme_Object *scheme_stx_property(Scheme_Object *_stx,
				   Scheme_Object *key,
				   Scheme_Object *val)
{
  Scheme_Stx *stx;
  Scheme_Object *l;

  stx = (Scheme_Stx *)_stx;

  if (stx->props) {
    if (SAME_OBJ(stx->props, STX_SRCTAG)) {
      if (val)
	l = scheme_make_pair(scheme_make_pair(source_symbol, scheme_true),
			     scheme_null);
      else
	l = NULL;
    } else {
      Scheme_Object *e;

      for (e = stx->props; SCHEME_PAIRP(e); e = SCHEME_CDR(e)) {
	if (SAME_OBJ(key, SCHEME_CAR(SCHEME_CAR(e)))) {
	  if (val)
	    break;
	  else
	    return SCHEME_CDR(SCHEME_CAR(e));
	}
      }

      if (SCHEME_NULLP(e))
	l = stx->props;
      else {
	/* Remove existing binding: */
	Scheme_Object *first = scheme_null, *last = NULL, *p;

	for (e = stx->props; SCHEME_PAIRP(e); e = SCHEME_CDR(e)) {
	  if (SAME_OBJ(key, SCHEME_CAR(SCHEME_CAR(e)))) {
	    p = SCHEME_CDR(e);
	    e = NULL;
	  } else {
	    p = scheme_make_pair(SCHEME_CDR(SCHEME_CAR(e)), scheme_null);
	  }

	  if (last)
	    SCHEME_CDR(last) = p;
	  else
	    first = p;
	  last = p;

	  if (!e)
	    break;
	}
	
	l = first;
      }
    }
  } else
    l = scheme_null;

  if (val) {
    Scheme_Object *wraps;
    long lazy_prefix;
    
    l = scheme_make_pair(scheme_make_pair(key, val), l);

    wraps = stx->wraps;
    lazy_prefix = stx->lazy_prefix;

    stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->line, stx->col, stx->src, l);

    stx->wraps = wraps;
    stx->lazy_prefix = lazy_prefix;

    return (Scheme_Object *)stx;
  } else
    return scheme_false;
}

static Scheme_Object *syntax_property(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-property", "syntax", 0, argc, argv);

  return scheme_stx_property(argv[0],
			     argv[1],
			     (argc > 2) ? argv[2] : NULL);
}

#define SCHEME_STX_IDP(o) (SCHEME_STXP(o) && SCHEME_SYMBOLP(SCHEME_STX_VAL(o)))

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type("bound-identifier=?", "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type("bound-identifier=?", "identifier syntax", 1, argc, argv);

  return (scheme_stx_bound_eq(argv[0], argv[1],
			      (p->current_local_env
			       ? p->current_local_env->genv->phase
			       : 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *free_eq(int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type("free-identifier=?", "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type("free-identifier=?", "identifier syntax", 1, argc, argv);

  return (scheme_stx_free_eq(argv[0], argv[1],
			     (p->current_local_env
			      ? p->current_local_env->genv->phase
			      : 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *module_eq(int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type("module-identifier=?", "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type("module-identifier=?", "identifier syntax", 1, argc, argv);

  return (scheme_stx_module_eq(argv[0], argv[1],
			       (p->current_local_env
				? p->current_local_env->genv->phase
				: 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *module_trans_eq(int argc, Scheme_Object **argv)
{
  Scheme_Thread *p = scheme_current_thread;

  if (!SCHEME_STX_IDP(argv[0]))
    scheme_wrong_type("module-transformer-identifier=?", "identifier syntax", 0, argc, argv);
  if (!SCHEME_STX_IDP(argv[1]))
    scheme_wrong_type("module-transformer-identifier=?", "identifier syntax", 1, argc, argv);

  return (scheme_stx_module_eq(argv[0], argv[1],
			       1 + (p->current_local_env
				    ? p->current_local_env->genv->phase
				    : 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *do_module_binding(char *name, int argc, Scheme_Object **argv, int dphase)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *a, *m;

  a = argv[0];

  if (!SCHEME_STXP(a) || !SCHEME_STX_SYM(a))
    scheme_wrong_type(name, "identifier syntax", 0, argc, argv);

  m = scheme_stx_module_name(&a, dphase + (p->current_local_env
					   ? p->current_local_env->genv->phase
					   : 0));

  if (!m)
    return scheme_false;
  else if (SAME_OBJ(m, scheme_undefined))
    return lexical_symbol;
  else
    return scheme_make_pair(m, a);
}

static Scheme_Object *module_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-binding", argc, argv, 0);
}

static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-transformer-binding", argc, argv, 1);
}

static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-source-module", "syntax", 0, argc, argv);

  return scheme_stx_source_module(argv[0], 1);
}

/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_STXOBJ_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rename_table_type, mark_rename_table);
}

END_XFORM_SKIP;

#endif
