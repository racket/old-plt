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

/* FIXME: syntax->list and resolve_env need stack checks. */

#define STX_GRAPH_FLAG 0x1

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);
static Scheme_Object *graph_syntax_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv);
static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_to_datum_wraps(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_e(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_line(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_col(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_to_list(int argc, Scheme_Object **argv);

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv);
static Scheme_Object *free_eq(int argc, Scheme_Object **argv);
static Scheme_Object *module_eq(int argc, Scheme_Object **argv);

static Scheme_Object *phase_uninterned;

#define HOME_MAP(a) ((a) ? ((Scheme_Env *)a)->modules : NULL)

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj))

/* Wraps:

   A wrap is a list of wrap-elems.

   - A wrap-elem <num> is a mark
   - A wrap-elem (vector <sym> <stx> ... <sym-or-#f> ...) is a lexical rename
                          env   var      var-resolved
                                         #f => not yet computed
   - A wrap-elem <hash-table> is a module rename set
         the hash table maps renamed syms to modname-srcname pairs,
         and maps phase_uninterned to phase
   - A wrap-elem (box (cons <num> <env>)) is a phase shift by <num>
         with phase-2 imports acessible via <env>

   For object with sub-syntax:

    The wraps field is
     (cons <local wraps> <lazy markswraps>), and
    #f means (cons null null).

    The reason we keep local wraps, even for parens,
    is that the wraps for a list might have to be used
    as wraps for #%app, etc.
*/

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
						      3, 3, 1),
			     env);

  scheme_add_global_constant("syntax->datum/wraps", 
			     scheme_make_folding_prim(syntax_to_datum_wraps,
						      "syntax->datum/wraps",
						      1, 1, 1),
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
  scheme_add_global_constant("syntax->list", 
			     scheme_make_folding_prim(syntax_to_list,
						      "syntax->list",
						      1, 1, 1),
			     env);

  scheme_add_global_constant("bound-identifier=?", 
			     scheme_make_folding_prim(bound_eq,
						      "bound-identifier=?",
						      2, 2, 1),
			     env);
  scheme_add_global_constant("free-identifier=?", 
			     scheme_make_folding_prim(free_eq,
						      "free-identifier=?",
						      2, 2, 1),
			     env);
  scheme_add_global_constant("module-identifier=?", 
			     scheme_make_folding_prim(module_eq,
						      "module-identifier=?",
						      2, 2, 1),
			     env);

  REGISTER_SO(phase_uninterned);
  phase_uninterned = scheme_make_symbol("phase");
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
  if (HAS_SUBSTX(val))
    stx->wraps = scheme_false;
  else
    stx->wraps = scheme_null;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_graph_stx(Scheme_Object *stx, long line, long col)
{
  ((Scheme_Stx *)stx)->hash_code |= STX_GRAPH_FLAG;

  return stx;
}

static Scheme_Object *mark_id = scheme_make_integer(0);

Scheme_Object *scheme_new_mark()
{
  mark_id = scheme_add1(1, &mark_id);
  return mark_id;
}

Scheme_Object *add_remove_mark(Scheme_Object *wraps, Scheme_Object *m)
{
#if CHECK_STX
  if (!SCHEME_NUMBERP(m))
    scheme_signal_error("internal error: mark is not a number");
#endif

  if (SCHEME_PAIRP(wraps) &&
      SAME_OBJ(m, SCHEME_CAR(wraps)))
    return SCHEME_CDR(wraps);
  else
    return scheme_make_pair(m, wraps);
}

Scheme_Object *scheme_add_remove_mark(Scheme_Object *o, Scheme_Object *m)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;

#if CHECK_STX
  if (!SCHEME_STXP(o))
    scheme_signal_error("internal error: not syntax");
#endif

  if (HAS_SUBSTX(stx->val)) {
    Scheme_Object *here_wraps, *lazy_wraps;
    wraps = stx->wraps;

    here_wraps = SCHEME_FALSEP(stx->wraps) ? scheme_null : SCHEME_CAR(stx->wraps);
    here_wraps = add_remove_mark(here_wraps, m);
    lazy_wraps = SCHEME_FALSEP(stx->wraps) ? scheme_null : SCHEME_CDR(stx->wraps);
    lazy_wraps = add_remove_mark(lazy_wraps, m);
    
    wraps = scheme_make_pair(here_wraps, lazy_wraps);
  } else {
    wraps = add_remove_mark(stx->wraps, m);
  }

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->line, stx->col, stx->src);
  stx->wraps = wraps;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_rename(Scheme_Object *newname, int c)
{
  Scheme_Object *v;
  int i;

  v = scheme_make_vector((2 * c) + 1, NULL);
  SCHEME_VEC_ELS(v)[0] = newname;

  for (i = 0; i < c; i++)
    SCHEME_VEC_ELS(v)[c + 1 + i] = scheme_false;

  return v;
}

void scheme_set_rename(Scheme_Object *rnm, int pos, Scheme_Object *oldname)
{
  SCHEME_VEC_ELS(rnm)[pos + 1] = oldname;
}

Scheme_Object *scheme_make_module_rename(long phase)
{
  Scheme_Hash_Table *ht;

  ht = scheme_hash_table(7, SCHEME_hash_ptr, 0, 0);
  scheme_add_to_table(ht, (const char *)phase_uninterned, scheme_make_integer(phase), 0);
  
  return (Scheme_Object *)ht;
}

void scheme_extend_module_rename(Scheme_Object *mrn,
				 Scheme_Object *modname, 
				 Scheme_Object *localname, 
				 Scheme_Object *exname)
{
  scheme_add_to_table((Scheme_Hash_Table *)mrn, (const char *)localname,
		      scheme_make_pair(modname, exname), 0);
}

void scheme_append_module_rename(Scheme_Object *src, Scheme_Object *dest)
{
  Scheme_Hash_Table *ht, *hts;
  Scheme_Bucket **bs, *b;
  int i;

  ht = (Scheme_Hash_Table *)dest;
  hts = (Scheme_Hash_Table *)src;
  
  /* Mappings in src overwrite mappings in dest: */

  bs = hts->buckets;
  for (i = hts->size; i--; ) {
    b = bs[i];
    if (b && b->val) {
      scheme_add_to_table(ht, b->key, b->val, 0);
    }
  }
}

Scheme_Object *scheme_add_rename(Scheme_Object *o, Scheme_Object *rename)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;

#if CHECK_STX
  if (!SCHEME_STXP(o))
    scheme_signal_error("internal error: not syntax");
  if (!SCHEME_STXP(name))
    scheme_signal_error("internal error: name not syntax");
  if (!SCHEME_SYMBOLP(newname))
    scheme_signal_error("internal error: new name not symbol");
#endif

  if (HAS_SUBSTX(stx->val)) {
    Scheme_Object *here_wraps, *lazy_wraps;
    wraps = stx->wraps;

    here_wraps = SCHEME_FALSEP(stx->wraps) ? scheme_null : SCHEME_CAR(stx->wraps);
    here_wraps = scheme_make_pair(rename, here_wraps);
    lazy_wraps = SCHEME_FALSEP(stx->wraps) ? scheme_null : SCHEME_CDR(stx->wraps);
    lazy_wraps = scheme_make_pair(rename, lazy_wraps);

    wraps = scheme_make_pair(here_wraps, lazy_wraps);
  } else {
    wraps = scheme_make_pair(rename, stx->wraps);
  }
  
  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->line, stx->col, stx->src);
  stx->wraps = wraps;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_stx_phase_shift(Scheme_Object *stx, long shift, Scheme_Env *home)
{
  return scheme_add_rename(stx, scheme_box(scheme_make_pair(scheme_make_integer(shift),
							    (Scheme_Object *)home)));
}

static Scheme_Object *propagate_wraps(Scheme_Object *o, Scheme_Object *wl)
{
  while (!SCHEME_NULLP(wl)) {
    if (SCHEME_NUMBERP(SCHEME_CAR(wl)))
      o = scheme_add_remove_mark(o, SCHEME_CAR(wl));
    else
      o = scheme_add_rename(o, SCHEME_CAR(wl));
    
    wl = SCHEME_CDR(wl);
  }

  return o;
}

Scheme_Object *scheme_stx_content(Scheme_Object *o)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  if (HAS_SUBSTX(stx->val) 
      && !SCHEME_FALSEP(stx->wraps)
      && !SCHEME_NULLP(SCHEME_CDR(stx->wraps))) {
    Scheme_Object *v = stx->val, *result;
    Scheme_Object *wraps, *here_wraps;
    Scheme_Object *ml = scheme_null;
    
    here_wraps = SCHEME_CAR(stx->wraps);
    wraps = SCHEME_CDR(stx->wraps);
    
    /* Reverse the list of wraps, to preserve order: */
    while (!SCHEME_NULLP(wraps)) {
      ml = scheme_make_pair(SCHEME_CAR(wraps), ml);
      wraps = SCHEME_CDR(wraps);
    }

    if (SCHEME_PAIRP(v)) {
      Scheme_Object *last = NULL, *first = NULL;

      while (SCHEME_PAIRP(v)) {
	Scheme_Object *p;
	result = propagate_wraps(SCHEME_CAR(v), ml);
	p = scheme_make_pair(result, scheme_null);
	if (last)
	  SCHEME_CDR(last) = p;
	else
	  first = p;
	last = p;
	v = SCHEME_CDR(v);
      }
      if (!SCHEME_NULLP(v)) {
	result = propagate_wraps(v, ml);
	if (last)
	  SCHEME_CDR(last) = result;
	else
	  first = result;
      }
      v = first;
    } else if (SCHEME_BOXP(v)) {
      result = propagate_wraps(SCHEME_BOX_VAL(v), ml);
      v = scheme_box(result);
    } else if (SCHEME_VECTORP(v)) {
      Scheme_Object *v2;
      int size = SCHEME_VEC_SIZE(v), i;

      v2 = scheme_make_vector(size, NULL);

      for (i = 0; i < size; i++) {
	result = propagate_wraps(SCHEME_VEC_ELS(v)[i], ml);
	SCHEME_VEC_ELS(v2)[i] = result;
      }

      v = v2;
    }

    stx->val = v;
    stx->wraps = scheme_make_pair(here_wraps, scheme_null);
  }

  return stx->val;
}

static int same_marks(Scheme_Object *awl, Scheme_Object *bwl)
{
  while (1) {
    /* Skip over renames: */
    while (!SCHEME_NULLP(awl) && !SCHEME_NUMBERP(SCHEME_CAR(awl)))
      awl = SCHEME_CDR(awl);
    while (!SCHEME_NULLP(bwl) && !SCHEME_NUMBERP(SCHEME_CAR(bwl)))
      bwl = SCHEME_CDR(bwl);

    /* Either at end? Then the same only if both at end. */
    if (SCHEME_NULLP(awl) || SCHEME_NULLP(bwl))
      return (SCHEME_NULLP(awl) && SCHEME_NULLP(bwl));

    /* Same first mark? */
    if (!scheme_bin_eq(SCHEME_CAR(awl), SCHEME_CAR(bwl)))
      return 0;

    awl = SCHEME_CDR(awl);
    bwl = SCHEME_CDR(bwl);
  }
}

static Scheme_Object *resolve_env(Scheme_Object *a, long phase, Scheme_Object **home)
{
  Scheme_Object *wraps = ((Scheme_Stx *)a)->wraps;
  Scheme_Object *rename_stack = scheme_null;
  Scheme_Object *result, *mresult = scheme_false;
  int phase_found = 0;

  if (home)
    *home = NULL;

  while (1) {
    if (SCHEME_NULLP(wraps)) {
      /* See rename case for info on rename_stack: */
      result = scheme_false;
      while (!SCHEME_NULLP(rename_stack)) {
	if (SAME_OBJ(SCHEME_CAAR(rename_stack), result))
	  result = SCHEME_CDR(SCHEME_CAR(rename_stack));
	rename_stack = SCHEME_CDR(rename_stack);
      }
      if (SCHEME_FALSEP(result))
	result = mresult;
      return result;
    } else if (SCHEME_HASHTP(SCHEME_CAR(wraps)) && home && !phase_found) {
      /* Module rename: */
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)SCHEME_CAR(wraps);
      if (SAME_OBJ(scheme_make_integer(phase),
		   scheme_lookup_in_table(ht, (const char *)phase_uninterned))) {
	Scheme_Object *rename;
	
	phase_found = 1;

	rename = scheme_lookup_in_table(ht, (const char *)SCHEME_STX_VAL(a));
	if (rename) {
	  /* Match: set mresult for the case of no lexical capture: */
	  mresult = SCHEME_CAR(rename);
	}
      }
    } else if (SCHEME_BOXP(SCHEME_CAR(wraps)) && home) {
      Scheme_Object *p, *n;
      p = SCHEME_PTR_VAL(SCHEME_CAR(wraps));
      n = SCHEME_CAR(p);
      phase -= SCHEME_INT_VAL(n);
      if (phase == 1)
	*home = SCHEME_CDR(p);
    } else if (SCHEME_VECTORP(SCHEME_CAR(wraps))) {
      /* Lexical rename: */
      Scheme_Object *rename, *renamed;
      int ri, c;

      rename = SCHEME_CAR(wraps);

      c = (SCHEME_VEC_SIZE(rename) - 1) >> 1;

      for (ri = 0; ri < c; ri++) {
	renamed = SCHEME_VEC_ELS(rename)[1+ri];
	if (SAME_OBJ(SCHEME_STX_VAL(a), SCHEME_STX_VAL(renamed)))
	  break;
      }

      if (ri < c) {
	if (same_marks(((Scheme_Stx *)renamed)->wraps, wraps)) {
	  Scheme_Object *other_env, *envname;
	  
	  envname = SCHEME_VEC_ELS(rename)[0];
	  other_env = SCHEME_VEC_ELS(rename)[1+c+ri];
	  
	  if (SCHEME_FALSEP(other_env)) {
	    other_env = resolve_env(renamed, 0, NULL);
	    SCHEME_VEC_ELS(rename)[1+c+ri] = other_env;
	  }
	  
	  /* If it turns out that we're going to return
	     other_env, then return envname instead. */
	  rename_stack = scheme_make_pair(scheme_make_pair(other_env, envname),
					  rename_stack);
	}
      }
    }
    wraps = SCHEME_CDR(wraps);
  }
}

static Scheme_Object *get_module_src_name(Scheme_Object *a, int always, long phase)
{
  Scheme_Object *wraps = ((Scheme_Stx *)a)->wraps;
  Scheme_Object *result;
  int phase_found = 0;

  result = NULL;

  while (1) {
    if (SCHEME_NULLP(wraps)) {
      if (!result && always)
	return SCHEME_STX_VAL(a);
      else
	return result;
    } else if (SCHEME_HASHTP(SCHEME_CAR(wraps)) && !phase_found) {
      Scheme_Hash_Table *ht = (Scheme_Hash_Table *)SCHEME_CAR(wraps);
      if (SAME_OBJ(scheme_make_integer(phase),
		   scheme_lookup_in_table(ht, (const char *)phase_uninterned))) {
	/* Module rename: */
	Scheme_Object *rename;
	
	phase_found = 1;

	rename = scheme_lookup_in_table(ht, (const char *)SCHEME_STX_VAL(a));
	if (rename) {
	  /* Match: set result: */
	  result = SCHEME_CDR(rename);
	}
      }
    } else if (SCHEME_BOXP(SCHEME_CAR(wraps))) {
      Scheme_Object *n;
      n = SCHEME_CAR(SCHEME_PTR_VAL(SCHEME_CAR(wraps)));
      phase -= SCHEME_INT_VAL(n);
    }
    
    /* Keep looking: */
    wraps = SCHEME_CDR(wraps);
  }
}

int scheme_stx_free_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  Scheme_Object *asym, *bsym, *ahome, *bhome;

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
  
  a = resolve_env(a, phase, &ahome);
  b = resolve_env(b, phase, &bhome);

  /* Same binding environment? */
  return (SAME_OBJ(a, b) && SAME_OBJ(HOME_MAP(ahome), HOME_MAP(bhome)));
}

int scheme_stx_module_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  Scheme_Object *asym, *bsym, *ahome, *bhome;

  if (!a || !b)
    return (a == b);

  if (SCHEME_STXP(a))
    asym = get_module_src_name(a, 1, phase);
  else
    asym = a;
  if (SCHEME_STXP(b))
    bsym = get_module_src_name(b, 1, phase);
  else
    bsym = b;

  /* Same name? */
  if (!SAME_OBJ(asym, bsym))
    return 0;

  if ((a == asym) || (b == bsym))
    return 1;
  
  a = resolve_env(a, phase, &ahome);
  b = resolve_env(b, phase, &bhome);

  /* Same binding environment? */
  return (SAME_OBJ(a, b) && SAME_OBJ(HOME_MAP(ahome), HOME_MAP(bhome)));
}

Scheme_Object *scheme_stx_module_name(Scheme_Object **a, long phase, Scheme_Env **home)
{
  *home = NULL;

  if (SCHEME_STXP(*a)) {
    Scheme_Object *modname, *exname;
    
    exname = get_module_src_name(*a, 0, phase);
    if (exname) {
      modname = resolve_env(*a, phase, (Scheme_Object **)home);
      *a = exname;
      
      return modname;
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
    if (!same_marks(((Scheme_Stx *)a)->wraps, ((Scheme_Stx *)b)->wraps))
      return 0;
  
  a = resolve_env(a, phase, NULL);
  if (uid)
    b = uid;
  else
    b = resolve_env(b, phase, NULL);

  /* Same binding environment? */
  return SAME_OBJ(a, b);
}

int scheme_stx_bound_eq(Scheme_Object *a, Scheme_Object *b, long phase)
{
  return scheme_stx_env_bound_eq(a, b, NULL, phase);
}

int scheme_stx_has_binder(Scheme_Object *a, long phase)
{
  if (SCHEME_STXP(a)) {
    a = resolve_env(a, phase, NULL);
    return SCHEME_TRUEP(a);
  } else
    return 0;
}

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
  while (SCHEME_PAIRP(l))
    l = SCHEME_CDR(l);

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
    p = scheme_make_pair(SCHEME_CAR(l), scheme_null);
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
/*                           syntax->datum                                */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht,
					    int with_marks);

static Scheme_Object *syntax_to_datum_k(void)
{
  Scheme_Process *p = scheme_current_process;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table **ht = (Scheme_Hash_Table **)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return syntax_to_datum_inner(o, ht, p->ku.k.i1);
}
#endif

static Scheme_Object *syntax_to_datum_inner(Scheme_Object *o, 
					    Scheme_Hash_Table **ht,
					    int with_marks)
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
      p->ku.k.i1 = with_marks;
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

  if (with_marks) {
    /* Propagate marks: */
    scheme_stx_content((Scheme_Object *)stx);
  }
    
  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    
    while (SCHEME_PAIRP(v)) {
      Scheme_Object *a;
      
      a = syntax_to_datum_inner(SCHEME_CAR(v), ht, with_marks);
      
      p = scheme_make_pair(a, scheme_null);
      
      if (last)
	SCHEME_CDR(last) = p;
      else
	first = p;
      last = p;
      v = SCHEME_CDR(v);
    }
    if (!SCHEME_NULLP(v)) {
      v = syntax_to_datum_inner(v, ht, with_marks);
      SCHEME_CDR(last) = v;
    }
    
    result = first;
  } else if (SCHEME_BOXP(v)) {
    v = syntax_to_datum_inner(SCHEME_BOX_VAL(v), ht, with_marks);
    result = scheme_box(v);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    Scheme_Object *r, *a;
    
    r = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = syntax_to_datum_inner(SCHEME_VEC_ELS(v)[i], ht, with_marks);
      SCHEME_VEC_ELS(r)[i] = a;
    }
    
    result = r;
  } else if (with_marks && SCHEME_SYMBOLP(v)) {
    result = scheme_make_pair(v, stx->wraps);
  } else
    result = v;

  if (ph)
    SCHEME_PTR_VAL(ph) = result;

  return result;
}

Scheme_Object *scheme_syntax_to_datum(Scheme_Object *stx, int with_marks)
{
  Scheme_Hash_Table *ht = NULL;
  Scheme_Object *v;

  v = syntax_to_datum_inner(stx, &ht, with_marks);

  if (ht)
    v = scheme_resolve_placeholders(v, 0);

  return v;
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
  Scheme_Process *p = scheme_current_process;
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
					    Scheme_Stx *stx_wraps,
					    Scheme_Hash_Table *ht)
{
  Scheme_Object *result, *ph = NULL;

  if (SCHEME_STXP(o))
    return o;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
# ifndef MZ_REAL_THREADS
      Scheme_Process *p = scheme_current_process;
# endif
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

      a = datum_to_syntax_inner(SCHEME_CAR(o), stx_src, stx_wraps, ht);
      
      p = scheme_make_pair(a, scheme_null);
      
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
  } else if (SCHEME_BOXP(o)) {
    o = datum_to_syntax_inner(SCHEME_PTR_VAL(o), stx_src, stx_wraps, ht);
    result = scheme_box(o);
  } else if (SCHEME_VECTORP(o)) {
    int size = SCHEME_VEC_SIZE(o), i;
    Scheme_Object *a;

    result = scheme_make_vector(size, NULL);
    
    for (i = 0; i < size; i++) {
      a = datum_to_syntax_inner(SCHEME_VEC_ELS(o)[i], stx_src, stx_wraps, ht);
      SCHEME_VEC_ELS(result)[i] = a;
    }
  } else {
    result = o;
  }

  if (SCHEME_FALSEP((Scheme_Object *)stx_src))
    result = scheme_make_stx(result, -1, -1, scheme_false);
  else
    result = scheme_make_stx(result, stx_src->line, stx_src->col, stx_src->src);

  if (SCHEME_FALSEP((Scheme_Object *)stx_wraps)) {
    if (HAS_SUBSTX(SCHEME_STX_VAL(result)))
      ((Scheme_Stx *)result)->wraps = scheme_false;
    else
      ((Scheme_Stx *)result)->wraps = scheme_null;
  } else {
    /* Copy wraps: */
    Scheme_Object *stxwraps;

    if (HAS_SUBSTX(stx_wraps->val))
      stxwraps = SCHEME_FALSEP(stx_wraps->wraps) ? scheme_null : SCHEME_CAR(stx_wraps->wraps);
    else
      stxwraps = stx_wraps->wraps;
    
    if (HAS_SUBSTX(SCHEME_STX_VAL(result))) {
      /* No propagation will be needed: */
      Scheme_Object *wraps;
      
      if (SCHEME_NULLP(stxwraps))
	wraps = scheme_false;
      else
	wraps = scheme_make_pair(stxwraps, scheme_null);
      
      ((Scheme_Stx *)result)->wraps = wraps;
    } else {
      ((Scheme_Stx *)result)->wraps = stxwraps;
    }
  }
  
  if (ph) {
    ((Scheme_Stx *)result)->hash_code |= STX_GRAPH_FLAG;
    SCHEME_PTR_VAL(ph) = result;
  }

  return result;
}

Scheme_Object *scheme_datum_to_syntax(Scheme_Object *o, 
				      Scheme_Object *stx_src,
				      Scheme_Object *stx_wraps)
{
  Scheme_Hash_Table *ht;
  Scheme_Object *v;

  if (!SCHEME_FALSEP(stx_src) && !SCHEME_STXP(stx_src))
    return o;

  ht = scheme_setup_datum_graph(o, 0);

  v = datum_to_syntax_inner(o, 
			    (Scheme_Stx *)stx_src, 
			    (Scheme_Stx *)stx_wraps,
			    ht);

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
    
  return scheme_syntax_to_datum(argv[0], 0);
}

static Scheme_Object *syntax_to_datum_wraps(int argc, Scheme_Object **argv)
{
  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax->datum/wraps", "syntax", 0, argc, argv);
    
  return scheme_syntax_to_datum(argv[0], 1);
}

static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv)
{
  if (!SCHEME_FALSEP(argv[1]) && !SCHEME_STXP(argv[1]))
    scheme_wrong_type("datum->syntax", "syntax or #f", 1, argc, argv);
  if (!SCHEME_FALSEP(argv[2]) && !SCHEME_STXP(argv[2]))
    scheme_wrong_type("datum->syntax", "syntax or #f", 2, argc, argv);
    
  return scheme_datum_to_syntax(argv[0], argv[1], argv[2]);
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
    
  if (stx->col < 0)
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

static Scheme_Object *bound_eq(int argc, Scheme_Object **argv)
{
  Scheme_Process *p = scheme_current_process;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_STX_SYM(argv[0]))
    scheme_wrong_type("bound-identifier=?", "idenfitier syntax", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]) || !SCHEME_STX_SYM(argv[1]))
    scheme_wrong_type("bound-identifier=?", "idenfitier syntax", 1, argc, argv);

  return (scheme_stx_bound_eq(argv[0], argv[1], 
			      (p->current_local_env
			       ? p->current_local_env->genv->phase
			       : 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *free_eq(int argc, Scheme_Object **argv)
{
  Scheme_Process *p = scheme_current_process;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_STX_SYM(argv[0]))
    scheme_wrong_type("free-identifier=?", "idenfitier syntax", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]) || !SCHEME_STX_SYM(argv[1]))
    scheme_wrong_type("free-identifier=?", "idenfitier syntax", 1, argc, argv);

  return (scheme_stx_free_eq(argv[0], argv[1],
			     (p->current_local_env
			      ? p->current_local_env->genv->phase
			      : 0))
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *module_eq(int argc, Scheme_Object **argv)
{
  Scheme_Process *p = scheme_current_process;

  if (!SCHEME_STXP(argv[0]) || !SCHEME_STX_SYM(argv[0]))
    scheme_wrong_type("module-identifier=?", "idenfitier syntax", 0, argc, argv);
  if (!SCHEME_STXP(argv[1]) || !SCHEME_STX_SYM(argv[1]))
    scheme_wrong_type("module-identifier=?", "idenfitier syntax", 1, argc, argv);

  return (scheme_stx_module_eq(argv[0], argv[1],
			       (p->current_local_env
				? p->current_local_env->genv->phase
				: 0))
	  ? scheme_true
	  : scheme_false);
}
