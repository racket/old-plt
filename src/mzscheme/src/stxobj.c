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

#define STX_DEBUG 0

static Scheme_Object *syntax_p(int argc, Scheme_Object **argv);
static Scheme_Object *graph_syntax_p(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_to_datum(int argc, Scheme_Object **argv);
static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv);

static Scheme_Object *syntax_e(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_line(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_col(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_span(int argc, Scheme_Object **argv);
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
static Scheme_Object *module_binding_pos(int argc, Scheme_Object **argv);
static Scheme_Object *module_trans_binding_pos(int argc, Scheme_Object **argv);
static Scheme_Object *syntax_src_module(int argc, Scheme_Object **argv);

static Scheme_Object *barrier_symbol;

static Scheme_Object *source_symbol; /* uninterned! */
static Scheme_Object *share_symbol; /* uninterned! */
static Scheme_Object *origin_symbol;
static Scheme_Object *lexical_symbol;

static Scheme_Object *mark_id = scheme_make_integer(0);

static Scheme_Stx_Srcloc *empty_srcloc;

static Scheme_Object *empty_simplified;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

#define CONS scheme_make_pair
#define ICONS scheme_make_immutable_pair

#define HAS_SUBSTX(obj) (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) || SCHEME_BOXP(obj))

typedef struct Module_Renames {
  Scheme_Type type; /* = scheme_rename_table_type */
  MZ_HASH_KEY_EX
  char plus_kernel, nonmodule;
  long phase;
  Scheme_Object *plus_kernel_nominal_source;
  Scheme_Hash_Table *ht; /* localname ->  modidx  OR
                                          (cons modidx exportname) OR
                                          (list* modidx exportname nominal_modidx nominal_exportname) */
} Module_Renames;

static Module_Renames *krn;

#define SCHEME_RENAMESP(obj) (SAME_TYPE(SCHEME_TYPE(obj), scheme_rename_table_type))

/* Wraps:

   A wrap is a list of wrap-elems and wrap-chunks. A wrap-chunk is a
   "vector" (a scheme_wrap_chunk_type) of wrap-elems.

   Each wrap-elem has one of several shapes:

   - A wrap-elem <num> is a mark

   - A wrap-elem (vector <sym> <ht> <stx> ... <sym-or-void> ...) is a lexical rename
                         env  (sym   var      var-resolved
                              ->pos)           void => not yet computed
                              or #f  sym => mark
                                      check done, var-resolved is answer to replace #f
   - A wrap-elem (vector <any> <ht> <sym> ... <sym> ...) is also a lexical rename
                                    var       resolved
         where the variables have already been resolved and filtered (no mark
         comparison needed with the remaining wraps)

   - A wrap-elem <rename-table> is a module rename set
         the hash table maps renamed syms to modname-srcname pairs

   - A wrap-elem (box (vector <num> <midx> <midx>)) is a phase shift
         by <num>, remapping the first <midx> to the second <midx>

   - A wrap-elem '* is a mark barrier, which is applied to the
         result of an expansion so that top-level marks do not
         break re-expansions

  The lazy_prefix field of a syntax object keeps track of how many of
  the first wraps (items and chunks inthe list) need to be propagated
  to sub-syntax.  */

/*========================================================================*/
/*                            wrap chunks                                 */
/*========================================================================*/

typedef struct {
  Scheme_Type type;
  short len;
  Scheme_Object *a[1];
} Wrap_Chunk;

#define MALLOC_WRAP_CHUNK(n) (Wrap_Chunk *)scheme_malloc_tagged(sizeof(Wrap_Chunk) + ((n - 1) * sizeof(Scheme_Object *)))

/* Macros for iterating over the elements of a wrap. */

typedef struct {
  Scheme_Object *l;
  Scheme_Object *a;
  int is_limb;
  int pos;
} Wrap_Pos;

static void WRAP_POS_SET_FIRST(Wrap_Pos *w)
{
  if (!SCHEME_NULLP(w->l)) {
    Scheme_Object *a;
    a = SCHEME_CAR(w->l);
    if (SCHEME_TYPE(a) == scheme_wrap_chunk_type) {
      w->is_limb = 1;
      w->pos = 0;
      w->a = ((Wrap_Chunk *)a)->a[0];
    } else {
      w->is_limb = 0;
      w->a = a;
    }
  }
}

static
#ifndef NO_INLINE_KEYWORD
MSC_IZE(inline)
#endif
void DO_WRAP_POS_INC(Wrap_Pos *w)
{
  Scheme_Object *a;
  if (w->is_limb && (w->pos + 1 < ((Wrap_Chunk *)SCHEME_CAR(w->l))->len)) {
    a = SCHEME_CAR(w->l);
    w->pos++;
    w->a = ((Wrap_Chunk *)a)->a[w->pos];
  } else {
    w->l = SCHEME_CDR(w->l);
    if (!SCHEME_NULLP(w->l)) {
      a = SCHEME_CAR(w->l);
      if (SCHEME_TYPE(a) == scheme_wrap_chunk_type) {
	w->is_limb = 1;
	w->pos = 0;
	w->a = ((Wrap_Chunk *)a)->a[0];
      } else {
	w->is_limb = 0;
	w->a = a;
      }
    } else
      w->is_limb = 0;
  }
}

#define WRAP_POS Wrap_Pos
#define WRAP_POS_INIT(w, wr) w.l = wr; WRAP_POS_SET_FIRST(&w)

#define WRAP_POS_INC(w) DO_WRAP_POS_INC(&w)

#define WRAP_POS_INIT_END(w) w.l = scheme_null
#define WRAP_POS_END_P(w) SCHEME_NULLP(w.l)
#define WRAP_POS_FIRST(w) w.a
#define WRAP_POS_COPY(w, w2) w.l = (w2).l; w.a = (w2).a; w.is_limb= (w2).is_limb; w.pos = (w2).pos

/* Walking backwards through one chunk: */

static void DO_WRAP_POS_REVINIT(Wrap_Pos *w, Scheme_Object *k)
{
  Scheme_Object *a;
  a = SCHEME_CAR(k);
  if (SCHEME_TYPE(a) == scheme_wrap_chunk_type) {
    w->is_limb = 1;
    w->l = k;
    w->pos = ((Wrap_Chunk *)a)->len - 1;
    w->a = ((Wrap_Chunk *)a)->a[w->pos];
  } else {
    w->l = k;
    w->a = a;
    w->is_limb = 0;
    w->pos = 0;
  }
}

#define WRAP_POS_KEY(w) w.l
#define WRAP_POS_REVINIT(w, k) DO_WRAP_POS_REVINIT(&w, k)
#define WRAP_POS_REVEND_P(w) (w.pos < 0)
#define WRAP_POS_DEC(w) --w.pos; if (w.pos >= 0) w.a = ((Wrap_Chunk *)SCHEME_CAR(w.l))->a[w.pos]

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
						      2, 4, 1),
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
  scheme_add_global_constant("syntax-span", 
			     scheme_make_folding_prim(syntax_span,
						      "syntax-span",
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

  scheme_add_global_constant("identifier-binding-export-position",
			     scheme_make_prim_w_arity(module_binding_pos,
						      "identifier-binding-export-position",
						      1, 1),
			     env);
  scheme_add_global_constant("identifier-transformer-binding-export-position",
			     scheme_make_prim_w_arity(module_trans_binding_pos,
						      "identifier-transformer-binding-export-position",
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
  REGISTER_SO(share_symbol);
  REGISTER_SO(origin_symbol);
  REGISTER_SO(lexical_symbol);
  source_symbol = scheme_make_symbol("source"); /* not interned! */
  share_symbol = scheme_make_symbol("share"); /* not interned! */
  origin_symbol = scheme_intern_symbol("origin");
  lexical_symbol = scheme_intern_symbol("lexical");

  REGISTER_SO(mark_id);

  REGISTER_SO(empty_srcloc);
  empty_srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  empty_srcloc->type = scheme_rt_srcloc;
#endif
  empty_srcloc->src = scheme_false;
  empty_srcloc->line = -1;
  empty_srcloc->col = -1;
  empty_srcloc->pos = -1;

  REGISTER_SO(empty_simplified);
  empty_simplified = scheme_make_vector(2, scheme_false);
}

/*========================================================================*/
/*                       stx creation and maintenance                     */
/*========================================================================*/

Scheme_Object *scheme_make_stx(Scheme_Object *val, 
			       Scheme_Stx_Srcloc *srcloc,
			       Scheme_Object *props)
{
  Scheme_Stx *stx;

  stx = MALLOC_ONE_TAGGED(Scheme_Stx);
  stx->type = scheme_stx_type;
  stx->hash_code = HAS_SUBSTX(val) ? STX_SUBSTX_FLAG : 0;
  stx->val = val;
  stx->srcloc = srcloc;
  stx->wraps = scheme_null;
  stx->props = props;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_make_stx_w_offset(Scheme_Object *val, 
					long line, long col, long pos, long span,
					Scheme_Object *src,
					Scheme_Object *props)
{
  Scheme_Stx_Srcloc *srcloc;

  if (SAME_TYPE(SCHEME_TYPE(src), scheme_stx_offset_type)) {
    Scheme_Stx_Offset *o = (Scheme_Stx_Offset *)src;

    if (pos >= 0) {
      if (o->pos < 0)
	pos = -1;
      else
	pos += o->pos;
    }
    if ((col >= 0) && (o->col >= 0)) {
      if (line == 1)
	col += o->col;
    } else
      col = -1;
    if ((line >= 0) && (o->line >= 0))
      line += o->line;
    else
      line = -1;

    if (pos < 0) line = -1;
    if (line < 0) col = -1;
    if (col < 0) line = -1;

    src = o->src;
  }

  srcloc = MALLOC_ONE_RT(Scheme_Stx_Srcloc);
#ifdef MZTAG_REQUIRED
  srcloc->type = scheme_rt_srcloc;
#endif
  srcloc->src = src;
  srcloc->line = line;
  srcloc->col = col;
  srcloc->pos = pos;
  srcloc->span = span;
   
  return scheme_make_stx(val, srcloc, props);
}

Scheme_Object *scheme_make_graph_stx(Scheme_Object *stx, long line, long col, long pos)
/* Sets the "is graph" flag */
{
  Scheme_Object *tmp, *key;

  ((Scheme_Stx *)stx)->hash_code |= STX_GRAPH_FLAG;
  
  /* Add back-pointing property to track sharing 
     independent of marks. */
  key = scheme_new_mark();
  tmp = scheme_stx_property(stx, share_symbol, key);
  ((Scheme_Stx *)stx)->props = ((Scheme_Stx *)tmp)->props;

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
  Scheme_Object *wraps, *modinfo_cache;
  long lazy_prefix;
  int graph;

  if (nstx->props) {
    if (SAME_OBJ(nstx->props, STX_SRCTAG)) {
      /* Retain 'source tag. */
      ne = ICONS(ICONS(source_symbol, scheme_true), scheme_null);
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

      /* Drop 'source and 'share, add 'origin if not there */
      for (p = oe; SCHEME_PAIRP(p); p = SCHEME_CDR(p)) {
	a = SCHEME_CAR(SCHEME_CAR(p));
	if (SAME_OBJ(a, source_symbol) || SAME_OBJ(a, share_symbol))
	  mod = 1;
	else if (SAME_OBJ(a, origin_symbol))
	  mod = 1;
      }

      if (mod) {
	Scheme_Object *first = scheme_null, *last = NULL;

	for (; SCHEME_PAIRP(oe); oe = SCHEME_CDR(oe)) {
	  a = SCHEME_CAR(SCHEME_CAR(oe));
	  if (!SAME_OBJ(a, source_symbol) && !SAME_OBJ(a, share_symbol)) {
	    if (!SAME_OBJ(a, origin_symbol)) {
	      p = ICONS(SCHEME_CAR(oe), scheme_null);
	    } else {
	      p = ICONS(ICONS(a, ICONS(origin, 
				       SCHEME_CDR(SCHEME_CAR(oe)))),
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
	oe = ICONS(ICONS(origin_symbol, 
			 ICONS(origin, scheme_null)),
		  oe);
      }
    }
  } else {
    /* Add 'origin. */
    oe = NULL;
  }

  if (!oe)
    oe = ICONS(ICONS(origin_symbol, 
		     ICONS(origin, scheme_null)),
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
	  v = ICONS(v, SCHEME_CDR(SCHEME_CAR(e2)));
	  break;
	}
      }

      p = ICONS(ICONS(a, v), scheme_null);
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
	p = ICONS(ICONS(a, v), scheme_null);
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

  graph = (nstx->hash_code & STX_GRAPH_FLAG);

  wraps = nstx->wraps;
  if (nstx->hash_code & STX_SUBSTX_FLAG) {
    modinfo_cache = NULL;
    lazy_prefix = nstx->u.lazy_prefix;
  } else {
    modinfo_cache = nstx->u.modinfo_cache;
    lazy_prefix = 0;
  }

  nstx = (Scheme_Stx *)scheme_make_stx(nstx->val, nstx->srcloc, ne);

  nstx->wraps = wraps;
  if (modinfo_cache)
    nstx->u.modinfo_cache = modinfo_cache;
  else
    nstx->u.lazy_prefix = lazy_prefix;

  if (graph)
    nstx->hash_code |= STX_GRAPH_FLAG;

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
    return CONS(m, wraps);
  }
}

Scheme_Object *scheme_add_remove_mark(Scheme_Object *o, Scheme_Object *m)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *wraps;
  long lp;
  int graph;

  graph = (stx->hash_code & STX_GRAPH_FLAG);

  if (stx->hash_code & STX_SUBSTX_FLAG)
    lp = stx->u.lazy_prefix;
  else
    lp = 1;

  wraps = add_remove_mark(stx->wraps, m, &lp);

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
  stx->wraps = wraps;

  if (stx->hash_code & STX_SUBSTX_FLAG)
    stx->u.lazy_prefix = lp;
  /* else cache should stay zeroed */

  if (graph)
    stx->hash_code |= STX_GRAPH_FLAG;

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
    if (scheme_hash_get(ht, SCHEME_STX_VAL(oldname)))
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

void scheme_extend_module_rename_with_kernel(Scheme_Object *mrn, Scheme_Object *nominal_mod)
{
  /* Don't use on a non-module namespace, where renames may need
     to be removed... */
  ((Module_Renames *)mrn)->plus_kernel = 1;
  ((Module_Renames *)mrn)->plus_kernel_nominal_source = nominal_mod;
}

void scheme_extend_module_rename(Scheme_Object *mrn,
				 Scheme_Object *modname,     /* actual source module */
				 Scheme_Object *localname,   /* name in local context */
				 Scheme_Object *exname,      /* name in definition context  */
				 Scheme_Object *nominal_mod, /* nominal source module */
				 Scheme_Object *nominal_ex)  /* nominal import before local renaming */
{
  Scheme_Object *elem;

  if (SAME_OBJ(modname, nominal_mod)
      && SAME_OBJ(exname, nominal_ex)) {
    if (SAME_OBJ(localname, exname))
      elem = modname;
    else
      elem = CONS(modname, exname);
  } else {
    elem = CONS(modname, CONS(exname, CONS(nominal_mod, nominal_ex)));
  }
  
  scheme_hash_set(((Module_Renames *)mrn)->ht, localname, elem);
}

void scheme_append_module_rename(Scheme_Object *src, Scheme_Object *dest)
{
  Scheme_Hash_Table *ht, *hts;
  int i;

  if (((Module_Renames *)src)->plus_kernel) {
    ((Module_Renames *)dest)->plus_kernel = 1;
    ((Module_Renames *)dest)->plus_kernel_nominal_source = ((Module_Renames *)src)->plus_kernel_nominal_source;
  }

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
  int graph;

  graph = (stx->hash_code & STX_GRAPH_FLAG);

  wraps = CONS(rename, stx->wraps);
  if (stx->hash_code & STX_SUBSTX_FLAG)
    lp = stx->u.lazy_prefix + 1;
  else
    lp = 0;

  stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
  stx->wraps = wraps;

  stx->u.lazy_prefix = lp; /* same as zeroing cache if no SUBSTX */

  if (graph)
    stx->hash_code |= STX_GRAPH_FLAG;

  return (Scheme_Object *)stx;
}

Scheme_Object *scheme_add_mark_barrier(Scheme_Object *o)
{
  return scheme_add_rename(o, barrier_symbol);
}

Scheme_Object *scheme_stx_phase_shift(Scheme_Object *stx, long shift,
				      Scheme_Object *old_midx, Scheme_Object *new_midx)
/* Shifts the phase on a syntax object in a module. A 0 shift might be
   used just to re-direct relative module paths. new_midx might be
   NULL to shift without redirection. */
{
  if (shift || new_midx) {
    Scheme_Object *vec;
  
    vec = scheme_make_vector(3, NULL);
    SCHEME_VEC_ELS(vec)[0] = scheme_make_integer(shift);
    SCHEME_VEC_ELS(vec)[1] = (new_midx ? old_midx : scheme_false);
    SCHEME_VEC_ELS(vec)[2] = (new_midx ? new_midx : scheme_false);

    return scheme_add_rename(stx, scheme_box(vec));
  } else
    return (Scheme_Object *)stx;
}

static Scheme_Object *propagate_wraps(Scheme_Object *o, 
				      int len, Scheme_Object **_ml,
				      Scheme_Object *owner_wraps)
{
  int i;
  Scheme_Object *ml;

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
      long lp;
      int graph;

      graph = (stx->hash_code & STX_GRAPH_FLAG);

      if (stx->hash_code & STX_SUBSTX_FLAG)
	lp = stx->u.lazy_prefix + len;
      else
	lp = 0;

      stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, stx->props);
      stx->wraps = owner_wraps;
      stx->u.lazy_prefix = lp; /* same as zeroing cache if no SUBSTX */

      if (graph)
	stx->hash_code |= STX_GRAPH_FLAG;

      return (Scheme_Object *)stx;
    }
  }

  ml = *_ml;
  if (!ml) {
    if (len > 1) {
      Wrap_Chunk *wc;
      Scheme_Object *l, *a;
      int count = 0, j;
    
      for (i = 0, l = owner_wraps; i < len; i++, l = SCHEME_CDR(l)) {
	a = SCHEME_CAR(l);
	if (SAME_TYPE(SCHEME_TYPE(a), scheme_wrap_chunk_type)) {
	  count += ((Wrap_Chunk *)a)->len;
	} else if (SCHEME_NUMBERP(a)) {
	  if ((i >= len) || !SAME_OBJ(a, SCHEME_CADR(l)))
	    count++;
	  else {
	    /* Cancelled marks */
	    i++;
	    l= SCHEME_CDR(l);
	  }
	} else
	  count++;
      }

      if (!count) {
	ml = scheme_null; /* everything disappeared! */
      } else {
	wc = MALLOC_WRAP_CHUNK(count);
	wc->type = scheme_wrap_chunk_type;
	wc->len = count;
	
	j = 0;
	for (i = 0, l = owner_wraps; i < len; i++, l = SCHEME_CDR(l)) {
	  a = SCHEME_CAR(l);
	  if (SAME_TYPE(SCHEME_TYPE(a), scheme_wrap_chunk_type)) {
	    int k, cl = ((Wrap_Chunk *)a)->len;
	    for (k = 0; k < cl; k++) {
	      wc->a[j++] = ((Wrap_Chunk *)a)->a[k];
	    }
	  }  else if (SCHEME_NUMBERP(a)) {
	    if ((i >= len) || !SAME_OBJ(a, SCHEME_CADR(l)))
	      wc->a[j++] = a;
	    else {
	      /* Cancelled marks */
	      i++;
	      l= SCHEME_CDR(l);
	    }
	  } else
	    wc->a[j++] = a;
	}

	if (count == 1) /* in case mark removal left only one */
	  ml = wc->a[0];
	else
	  ml = (Scheme_Object *)wc;
      }
    } else
      ml = SCHEME_CAR(owner_wraps);

    *_ml = ml;
  }

  if (SCHEME_NUMBERP(ml))
    return scheme_add_remove_mark(o, ml);
  else if (SCHEME_NULLP(ml))
    return o;
  else
    return scheme_add_rename(o, ml);
}

Scheme_Object *scheme_stx_content(Scheme_Object *o)
/* Propagates wraps while getting a syntax object's content. */
{
  Scheme_Stx *stx = (Scheme_Stx *)o;

  if ((stx->hash_code & STX_SUBSTX_FLAG) && stx->u.lazy_prefix) {
    Scheme_Object *v = stx->val, *result;
    Scheme_Object *here_wraps;
    Scheme_Object *ml = NULL;
    int wl_count = 0;
    
    here_wraps = stx->wraps;
    wl_count = stx->u.lazy_prefix;
    stx->u.lazy_prefix = 0;

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

static int same_marks(WRAP_POS *_awl, WRAP_POS *_bwl, int a_ignore_barrier)
/* Compares the marks in two wraps lists */
{
  WRAP_POS awl;
  WRAP_POS bwl;
  Scheme_Object *acur_mark, *bcur_mark;

  WRAP_POS_COPY(awl, *_awl);
  WRAP_POS_COPY(bwl, *_bwl);

  while (1) {
    /* Skip over renames and cancelled marks: */
    acur_mark = NULL;
    while (1) {
      if (WRAP_POS_END_P(awl))
	break;
      if (SCHEME_NUMBERP(WRAP_POS_FIRST(awl))) {
	if (acur_mark) {
	  if (SAME_OBJ(acur_mark, WRAP_POS_FIRST(awl))) {
	    acur_mark = NULL;
	    WRAP_POS_INC(awl);
	  } else
	    break;
	} else {
	  acur_mark = WRAP_POS_FIRST(awl);
	  WRAP_POS_INC(awl);
	}
      } else if (!a_ignore_barrier && SAME_OBJ(WRAP_POS_FIRST(awl), barrier_symbol)) {
	WRAP_POS_INIT_END(awl);
      } else {
	WRAP_POS_INC(awl);
      }
    }
    bcur_mark = NULL;
    while (1) {
      if (WRAP_POS_END_P(bwl))
	break;
      if (SCHEME_NUMBERP(WRAP_POS_FIRST(bwl))) {
	if (bcur_mark) {
	  if (SAME_OBJ(bcur_mark, WRAP_POS_FIRST(bwl))) {
	    bcur_mark = NULL;
	    WRAP_POS_INC(bwl);
	  } else
	    break;
	} else {
	  bcur_mark = WRAP_POS_FIRST(bwl);
	  WRAP_POS_INC(bwl);
	}
      } else if (SAME_OBJ(WRAP_POS_FIRST(bwl), barrier_symbol)) {
	WRAP_POS_INIT_END(bwl);
      } else {
	WRAP_POS_INC(bwl);
      }
    }

    /* Same mark? */
    if (!SAME_OBJ(acur_mark, bcur_mark))
      return 0;

    /* Done if both reached the end: */
    if (WRAP_POS_END_P(awl) && WRAP_POS_END_P(bwl))
      return 1;
  }
}

#define QUICK_STACK_SIZE 10

static Scheme_Object *resolve_env(Scheme_Object *a, long phase, 
				  int w_mod, Scheme_Object **get_names)
/* Module binding ignored if w_mod is 0.
   If module bound, result is module idx, and get_names[0] is set to source name,
     get_names[1] is set to the nominal source module, and get_names[2] is set to
     the nominal source module's export.
   If lexically bound, result is env id, and a get_names[0] is set to scheme_undefined.
   If neither, result is #f and get_names is unchanged. */
{
  WRAP_POS wraps;
  Scheme_Object *o_rename_stack = scheme_null;
  Scheme_Object *mresult = scheme_false;
  Scheme_Object *modidx_shift_to = NULL, *modidx_shift_from = NULL;
  Scheme_Object *rename_stack[QUICK_STACK_SIZE];
  int stack_pos = 0;
  int is_in_module = 0;

  WRAP_POS_INIT(wraps, ((Scheme_Stx *)a)->wraps);

  while (1) {
    if (WRAP_POS_END_P(wraps)) {
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
      else if (get_names)
	get_names[0] = scheme_undefined;
      return result;
    } else if (SCHEME_RENAMESP(WRAP_POS_FIRST(wraps)) && w_mod) {
      /* Module rename: */
      Module_Renames *mrn = (Module_Renames *)WRAP_POS_FIRST(wraps);
      if (!is_in_module || !mrn->nonmodule) {
	if (!mrn->nonmodule)
	  is_in_module = 1;
	
	if (phase == mrn->phase) {
	  Scheme_Object *rename, *nominal = NULL;
	  
	  rename = scheme_hash_get(mrn->ht, SCHEME_STX_VAL(a));
	  if (!rename && mrn->plus_kernel) {
	    rename = scheme_hash_get(krn->ht, SCHEME_STX_VAL(a));
	    nominal = mrn->plus_kernel_nominal_source;
	  }
	  
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

	    if (get_names) {
	      if (SCHEME_PAIRP(rename)) {
		rename = SCHEME_CDR(rename);
		if (SCHEME_PAIRP(rename)) {
		  get_names[0] = SCHEME_CAR(rename);
		  get_names[1] = SCHEME_CADR(rename);
		  get_names[2] = SCHEME_CDDR(rename);
		} else {
		  get_names[0] = rename;
		  get_names[2] = NULL; /* finish below */
		}
	      } else {
		get_names[0] = SCHEME_STX_VAL(a);
		get_names[2] = NULL; /* finish below */
	      }

	      if (!get_names[2]) {
		get_names[2] = get_names[0];
		if (nominal)
		  get_names[1] = nominal;
		else
		  get_names[1] = mresult;
	      }
	    }
	  } else
	    mresult = scheme_false;
	}
      }
    } else if (SCHEME_BOXP(WRAP_POS_FIRST(wraps)) && w_mod) {
      /* Phase shift */
      Scheme_Object *vec, *n, *dest, *src;
      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      phase -= SCHEME_INT_VAL(n);
      
      src = SCHEME_VEC_ELS(vec)[1];
      dest = SCHEME_VEC_ELS(vec)[2];

      /* If src is #f, shift is just for phase; no redirection */

      if (!SCHEME_FALSEP(src)) {
	if (!modidx_shift_to) {
	  modidx_shift_to = dest;
	} else if (!SAME_OBJ(modidx_shift_from, dest)) {
	  modidx_shift_to = scheme_modidx_shift(dest,
						modidx_shift_from,
						modidx_shift_to);
	}
	
	modidx_shift_from = src;
      }
    } else if (SCHEME_VECTORP(WRAP_POS_FIRST(wraps))) {
      /* Lexical rename: */
      Scheme_Object *rename, *renamed;
      int ri, c, istart, iend;

      rename = WRAP_POS_FIRST(wraps);

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
	  int same;

	  if (SCHEME_SYMBOLP(renamed))
	    same= 1;
	  else {
	    WRAP_POS w2;
	    WRAP_POS_INIT(w2, ((Scheme_Stx *)renamed)->wraps);
	    same = same_marks(&w2, &wraps, 0);
	  }

	  if (same) {
	    Scheme_Object *other_env, *envname;

	    if (SCHEME_SYMBOLP(renamed)) {
	      /* Simplified table */
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
	      o_rename_stack = CONS(CONS(other_env, envname),
				    o_rename_stack);
	    }

	    break;
	  }
	}
      }
    }

    WRAP_POS_INC(wraps);
  }
}

static Scheme_Object *get_module_src_name(Scheme_Object *a, long phase)
/* Gets a module source name under the assumption that the identifier
   is not lexically renamed. This is used as a quick pre-test for
   module-identifier=?. */
{
  WRAP_POS wraps;
  Scheme_Object *result;
  int is_in_module = 0;

  if (((Scheme_Stx *)a)->u.modinfo_cache)
    return ((Scheme_Stx *)a)->u.modinfo_cache;

  WRAP_POS_INIT(wraps, ((Scheme_Stx *)a)->wraps);

  result = NULL;

  while (1) {
    if (WRAP_POS_END_P(wraps)) {
      if (!result)
	result = SCHEME_STX_VAL(a);

      ((Scheme_Stx *)a)->u.modinfo_cache = result;
      
      return result;
    } else if (SCHEME_RENAMESP(WRAP_POS_FIRST(wraps))) {
      Module_Renames *mrn = (Module_Renames *)WRAP_POS_FIRST(wraps);

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
	    if (SCHEME_PAIRP(rename)) {
	      result = SCHEME_CDR(rename);
	      if (SCHEME_PAIRP(result))
		result = SCHEME_CAR(result);
	    } else
	      result = SCHEME_STX_VAL(a);
	  } else
	    result = NULL;
	}
      }
    } else if (SCHEME_BOXP(WRAP_POS_FIRST(wraps))) {
      /* Phase shift */
      Scheme_Object *n, *vec;
      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(wraps));
      n = SCHEME_VEC_ELS(vec)[0];
      phase -= SCHEME_INT_VAL(n);
    }
    
    /* Keep looking: */
    WRAP_POS_INC(wraps);
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

Scheme_Object *scheme_stx_module_name(Scheme_Object **a, long phase, 
				      Scheme_Object **nominal_modidx,
				      Scheme_Object **nominal_name)
/* If module bound, result is module idx, and a is set to source name.
   If lexically bound, result is scheme_undefined and a is unchanged. 
   If neither, result is NULL and a is unchanged. */
{
  if (SCHEME_STXP(*a)) {
    Scheme_Object *modname, *names[3];

    names[0] = NULL;

    modname = resolve_env(*a, phase, 1, names);
    
    if (names[0]) {
      if (SAME_OBJ(names[0], scheme_undefined)) {
	return scheme_undefined;
      } else {
	*a = names[0];
	if (nominal_modidx)
	  *nominal_modidx = names[1];
	if (nominal_name)
	  *nominal_name = names[2];
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

  if (!uid) {
    WRAP_POS aw;
    WRAP_POS bw;
    WRAP_POS_INIT(aw, ((Scheme_Stx *)a)->wraps);
    WRAP_POS_INIT(bw, ((Scheme_Stx *)b)->wraps);
    if (!same_marks(&aw, &bw, 0))
      return 0;
  }

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
  WRAP_POS w;
  Scheme_Object *srcmod = scheme_false, *chain_from = NULL;

  WRAP_POS_INIT(w, ((Scheme_Stx *)stx)->wraps);

  while (!WRAP_POS_END_P(w)) {
    if (SCHEME_BOXP(WRAP_POS_FIRST(w))) {
      /* Phase shift:  */
      Scheme_Object *vec, *dest, *src;

      vec = SCHEME_PTR_VAL(WRAP_POS_FIRST(w));
      
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

    WRAP_POS_INC(w);
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
   list for wraps. (Unmarshaller will reverse it back.) 

   The wraps->datum tools are also used to simplify syntax object (to
   minimize the occupied space among a set of objects). */

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

static void simplify_lex_renames(Scheme_Object *wraps, Scheme_Hash_Table *lex_cache)
{
  WRAP_POS w;
  WRAP_POS prev;
  WRAP_POS w2;
  Scheme_Object *stack = scheme_null, *key, *old_key;
  Scheme_Object *v, *v2, *v2l, *stx, *name;
  long size, vsize, psize, i, j, pos;

  /* Although it makes no sense to simplify the rename table itself,
     we can simplify it in the context of a particular wrap suffix.
     (But don't mutate the wrap list, because that will stomp on
     tables that might be needed by a propoagation.)
     
     A lex_cache maps wrap starts w to simplified tables. A lex_cache
     is modified by this function, only. */

  WRAP_POS_INIT(w, wraps);
  WRAP_POS_INIT_END(prev);

  old_key = NULL;

  while (!WRAP_POS_END_P(w)) {
    if (SCHEME_VECTORP(WRAP_POS_FIRST(w))) {
      key = WRAP_POS_KEY(w);
      if (!SAME_OBJ(key, old_key))
	v = scheme_hash_get(lex_cache, key);
      else
	v = NULL;
      old_key = key;

      if (v) {
	/* Tables here are already simplified. */
	WRAP_POS_COPY(prev, w);
	/* No non-simplified table can follow a simplified one */
	break;
      } else {
	v = WRAP_POS_FIRST(w);
	if ((SCHEME_VEC_SIZE(v) > 2) /* a simplified vec can be empty */
	    && !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[2])) {
	  /* Need to simplify, but do deepest first: */
	  if (SCHEME_NULLP(stack) || !SAME_OBJ(SCHEME_CAR(stack), key))
	    stack = CONS(key, stack);
	} else {
	  if (WRAP_POS_END_P(prev))
	    WRAP_POS_COPY(prev, w);
	  /* No non-simplified table can follow a simplified one */
	  break;
	}
      }
    }
    
    WRAP_POS_INC(w);
  }

  while (!SCHEME_NULLP(stack)) {
    key = SCHEME_CAR(stack);
    v2l = scheme_null;

    WRAP_POS_REVINIT(w, key);

    while (!WRAP_POS_REVEND_P(w)) {
      v = WRAP_POS_FIRST(w);

      if (SCHEME_VECTORP(v)
	  && (SCHEME_VEC_SIZE(v) > 2) /* a simplified vec can be empty */
	  && !SCHEME_SYMBOLP(SCHEME_VEC_ELS(v)[2])) {
	/* This is the place to simplify: */

	vsize = (SCHEME_VEC_SIZE(v) - 2) / 2;

	/* Initial size; may shrink: */
	size = vsize;

	v2 = scheme_make_vector(2 + (2 * size), NULL);

	pos = 0; /* counter for used slots */

	for (i = 0; i < vsize; i++) {
	  stx = SCHEME_VEC_ELS(v)[2+i];
	  name = SCHEME_STX_VAL(stx);
	  SCHEME_VEC_ELS(v2)[2+pos] = name;
	  WRAP_POS_INIT(w2, ((Scheme_Stx *)stx)->wraps);
	  if (same_marks(&w2, &w, 0)) {
	    /* Either this name is in prev, in which case
	       the answer must match this rename's target, or
	       this rename's answer applies. */
	    int ok = 0;

	    if (!WRAP_POS_END_P(prev)) {
	      WRAP_POS w3;
	      Scheme_Object *vp, *other_env;

	      other_env = SCHEME_VEC_ELS(v)[2+vsize+i];
	      if (SCHEME_VOIDP(other_env)) {
		other_env = resolve_env(stx, 0, 0, NULL);
		SCHEME_VEC_ELS(v)[2+vsize+i] = other_env;
	      }

	      WRAP_POS_COPY(w3, prev);
	      for (; !WRAP_POS_END_P(w3); WRAP_POS_INC(w3)) {
		vp = WRAP_POS_FIRST(w3);
		if (SCHEME_VECTORP(vp)) {
		  psize = (SCHEME_VEC_SIZE(vp) - 2) / 2;
		  for (j = 0; j < psize; j++) {
		    if (SAME_OBJ(SCHEME_VEC_ELS(vp)[2+j], name)) {
		      ok = SAME_OBJ(SCHEME_VEC_ELS(vp)[2+psize+j], other_env);
		      break;
		    }
		  }
		  if (j < size)
		    break;
		}
	      }
	      if (WRAP_POS_END_P(w3))
		ok = SCHEME_FALSEP(other_env);
	    } else
	      ok = 1;

	    if (ok) {
	      SCHEME_VEC_ELS(v2)[2+size+pos] = SCHEME_VEC_ELS(v)[0];
	      pos++;
	    }
	  }
	}

	if (pos != size) {
	  /* Shrink simplified vector */
	  if (!pos)
	    v2 = empty_simplified;
	  else {
	    v = v2;
	    v2 = scheme_make_vector(2 + (2 * pos), NULL);
	    for (i = 0; i < pos; i++) {
	      SCHEME_VEC_ELS(v2)[2+i] = SCHEME_VEC_ELS(v)[2+i];
	      SCHEME_VEC_ELS(v2)[2+pos+i] = SCHEME_VEC_ELS(v)[2+size+i];
	    }
	  }
	}

	SCHEME_VEC_ELS(v2)[0] = scheme_false;
	SCHEME_VEC_ELS(v2)[1] = scheme_false;

	v2l = CONS(v2, v2l);

	WRAP_POS_COPY(prev, w);
      }

      WRAP_POS_DEC(w);
    }

    scheme_hash_set(lex_cache, key, v2l);

    stack = SCHEME_CDR(stack);
  }
}

static Scheme_Object *wraps_to_datum(Scheme_Object *w_in, 
				     Scheme_Hash_Table *rns,
				     int just_simplify)
{
  Scheme_Object *stack, *a, *old_key, *simplifies = scheme_null;
  WRAP_POS w;
  Scheme_Hash_Table *lex_cache;
  int stack_size = 0;

  a = scheme_hash_get(rns, w_in);
  if (a) {
    if (just_simplify)
      return SCHEME_CDR(a);
    else
      return SCHEME_CAR(a);
  }

  WRAP_POS_INIT(w, w_in);

  stack = scheme_null;

  lex_cache = (Scheme_Hash_Table *)scheme_hash_get(rns, scheme_void);
  if (!lex_cache) {
    lex_cache = scheme_make_hash_table(SCHEME_hash_ptr);
    scheme_hash_set(rns, scheme_void, (Scheme_Object *)lex_cache);
  }

  /* Ensures that all lexical tables in w have been simplified */
  simplify_lex_renames(w_in, lex_cache);

  while (!WRAP_POS_END_P(w)) {
    a = WRAP_POS_FIRST(w);
    old_key = WRAP_POS_KEY(w);
    WRAP_POS_INC(w);
    if (SCHEME_NUMBERP(a)) {
      /* Mark numbers get parenthesized */
      if (!WRAP_POS_END_P(w) && SAME_OBJ(a, WRAP_POS_FIRST(w)))
	WRAP_POS_INC(w); /* delete cancelled mark */
      else {
	if (just_simplify)
	  stack = CONS(a, stack);
	else
	  stack = CONS(CONS(a, scheme_null), stack);
	stack_size++;
      }
    } else if (SCHEME_VECTORP(a)) {
      if (SCHEME_VEC_SIZE(a) > 2) {

	if (!SCHEME_SYMBOLP(SCHEME_VEC_ELS(a)[2])) {
	  /* a is not a simplified table; need to look it up; */
	  /* if simplifies is non-null, then we already have. */
	  if (SCHEME_NULLP(simplifies)) {
	    simplifies = scheme_hash_get(lex_cache, old_key);
	    /* assert: a is not NULL; see the simplify_lex_rename() call above */
	    if (!simplifies)
	      *(long *)0x0 = 1;
	    if (!SCHEME_PAIRP(simplifies))
	      *(long *)0x0 = 1;
	  }
	  a = SCHEME_CAR(simplifies);
	  /* used up one simplification: */
	  simplifies = SCHEME_CDR(simplifies);
	  if (!SCHEME_LISTP(simplifies))
	      *(long *)0x0 = 1;
	}
	  
	if (just_simplify) {
	  stack = CONS(a, stack);
	} else {
	  Scheme_Object *local_key;
	    
	  local_key = scheme_hash_get(rns, a);
	  if (local_key) {
	    stack = CONS(local_key, stack);
	  } else {
	    local_key = scheme_make_integer(rns->count);
	    scheme_hash_set(rns, a, local_key);
	      
	    /* Since this is a simplified table, we can steal the first
	       slot for local_key: */
	      
	    SCHEME_VEC_ELS(a)[0] = local_key;
	      
	    stack = CONS(a, stack);
	  }
	}
	stack_size++;
      }
      /* else empty simplified vector, which we drop */
    } else if (SCHEME_RENAMESP(a)) {
      Module_Renames *mrn = (Module_Renames *)a;
      int redundant = 0;
      
      {
	/* Check for later [non]module rename at the same phase: */
	long shift = 0;	
	WRAP_POS l;
	
	WRAP_POS_COPY(l,w);

	for (; !WRAP_POS_END_P(l); WRAP_POS_INC(l)) {
	  if (SCHEME_RENAMESP(WRAP_POS_FIRST(l))) {
	    Module_Renames *lrn = (Module_Renames *)WRAP_POS_FIRST(l);
	    if (((!!lrn->nonmodule) == (!!mrn->nonmodule))
		&& ((lrn->phase + shift) == mrn->phase)) {
	      /* mrn is redundant */
	      redundant = 1;
	      break;
	    }
	  } else if (SCHEME_BOXP(WRAP_POS_FIRST(l))) {
	    shift += SCHEME_INT_VAL(SCHEME_VEC_ELS(SCHEME_PTR_VAL(WRAP_POS_FIRST(l)))[0]);
	  }
	}
      }

      if (!redundant) {
	if (just_simplify) {
	  stack = CONS((Scheme_Object *)mrn, stack);
	} else {
	  if (mrn->nonmodule) {
	    stack = CONS(((mrn->phase == 0)
			  ? scheme_true
			  : scheme_false), 
			 stack);
	  } else {
	    Scheme_Object *local_key;
	  
	    local_key = scheme_hash_get(rns, (Scheme_Object *)mrn);
	    if (local_key) {
	      stack = CONS(local_key, stack);
	    } else {
	      /* Convert hash table to list: */
	      int i, j, count = 0;
	      Scheme_Object *l, *idi;
	    
	      for (i = mrn->ht->size; i--; ) {
		if (mrn->ht->vals[i])
		  count++;
	      }

	      l = scheme_make_vector(count * 2, NULL);
	    
	      for (i = mrn->ht->size, j = 0; i--; ) {
		if (mrn->ht->vals[i]) {
		  SCHEME_VEC_ELS(l)[j++] = mrn->ht->keys[i];
		  idi = mrn->ht->vals[i];
		  /* Drop info on nominals, if any: */
		  if (SCHEME_PAIRP(idi) && SCHEME_PAIRP(SCHEME_CDR(idi))) {
		    idi = CONS(SCHEME_CAR(idi), SCHEME_CADR(idi));
		  }
		  SCHEME_VEC_ELS(l)[j++] = idi;
		}
	      }
	    
	      local_key = scheme_make_integer(rns->count);
	      scheme_hash_set(rns, a, local_key);
	    
	      l = CONS(scheme_make_integer(mrn->phase), l);
	      if (mrn->plus_kernel) {
		l = CONS(scheme_true,l);
		/* note: information on nominals intentially omitted */
	      }
	      l = CONS(local_key, l);
	    
	      stack = CONS(l, stack);
	    }
	  }
	}
	stack_size++;
      }
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
      stack = CONS(a, stack);
      stack_size++;
    } else {
      /* box, a phase shift */
      /* Any more rename tables? */
      WRAP_POS l;
      WRAP_POS_COPY(l, w);
      while (!WRAP_POS_END_P(l)) {
	if (SCHEME_RENAMESP(WRAP_POS_FIRST(l)))
	  break;
	WRAP_POS_INC(l);
      }
      /* If l is the end, don't need the phase shift */
      if (!WRAP_POS_END_P(l)) {
	stack = CONS(a, stack);
	stack_size++;
      }
    }
  }

  if (just_simplify) {
    if (stack_size) {
      /* Convert to a chunk: */
      Wrap_Chunk *wc;
      int i;
      wc = MALLOC_WRAP_CHUNK(stack_size);
      wc->type = scheme_wrap_chunk_type;
      wc->len = stack_size;
      for (i = stack_size; i--; stack = SCHEME_CDR(stack)) {
	wc->a[i] = SCHEME_CAR(stack);
      }
      stack = CONS((Scheme_Object *)wc, scheme_null);
    } else
      stack= scheme_null;
  }

  /* Double-check for equivalent list in table (after simplificiation): */
  if (just_simplify) {
    int i, j;

    for (i = rns->size; i--; ) {
      a = rns->vals[i];
      if (a) {
	if (SCHEME_PAIRP(a)) {
	  a = SCHEME_CADR(a);
	  if (((Wrap_Chunk *)a)->len == stack_size) {
	    Wrap_Chunk *ac, *bc;
	    ac = (Wrap_Chunk *)SCHEME_CAR(stack);
	    bc = (Wrap_Chunk *)a;
	    for (j = 0; j < stack_size; j++) {
	      if (!SAME_OBJ(ac->a[j], bc->a[j]))
		break;
	    }
	    if (j >= stack_size) {
	      if (just_simplify)
		return SCHEME_CDR(rns->vals[i]);
	      else
		return SCHEME_CAR(rns->vals[i]);
	    }
	  }
	}
      }
    }
  } else {
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
  scheme_hash_set(rns, w_in, CONS(a, stack));
  
  if (just_simplify)
    return stack;
  else
    return CONS(a, stack);
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
    Scheme_Object *key;

    if (!*ht)
      *ht = scheme_make_hash_table(SCHEME_hash_ptr);
    
    key = scheme_stx_property((Scheme_Object *)stx, share_symbol, NULL);
    if (SCHEME_FALSEP(key)) {
      scheme_signal_error("bad 'share key");
    }

    ph = scheme_hash_get(*ht, key);

    if (ph)
      return ph;
    else {
      ph = scheme_alloc_small_object();
      ph->type = scheme_placeholder_type;
      
      scheme_hash_set(*ht, key, (Scheme_Object *)ph);
    }
  } else 
    ph = NULL;

  if (with_marks) {
    /* Propagate wraps: */
    scheme_stx_content((Scheme_Object *)stx);
  }

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    Scheme_Object *first = NULL, *last = NULL, *p;
    
    while (SCHEME_PAIRP(v)) {
      Scheme_Object *a;

      a = syntax_to_datum_inner(SCHEME_CAR(v), ht, with_marks, rns);
      
      p = CONS(a, scheme_null);
      
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
    result = CONS(v, stx->wraps);
  } else
    result = v;

  if (with_marks > 1)
    result = CONS(result, wraps_to_datum(stx->wraps, rns, 0));

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
  Scheme_Object *a, *wraps_key;
  int stack_size;
  Wrap_Chunk *wc;

  /* rns maps numbers (table indices) to renaming tables, and negative
     numbers (negated fixnum marks) and symbols (interned marks) to marks.*/

  if (SCHEME_INTP(w))
    return scheme_hash_get(rns, w);

  wraps_key = SCHEME_CAR(w);
  w = SCHEME_CDR(w);

  stack_size = scheme_list_length(w);
  if (!stack_size) {
    scheme_hash_set(rns, wraps_key, scheme_null);
    return scheme_null;
  } else if (stack_size < 2) {
    wc = NULL;
  } else {
    wc = MALLOC_WRAP_CHUNK(stack_size);
    wc->type = scheme_wrap_chunk_type;
    wc->len = stack_size;
  }

  a = NULL;

  while (!SCHEME_NULLP(w)) {
    a = SCHEME_CAR(w);
    if (SCHEME_NUMBERP(a)) {
      /* Re-use rename table or env rename */
      a = scheme_hash_get(rns, a);
      if (!a) {
	scheme_read_err(scheme_false, NULL, -1, -1, -1, -1, 0,
			"read (compiled): unknown rename table index: %d",
			SCHEME_INT_VAL(a));
      }
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

      a = n;
    } else if (SCHEME_VECTORP(a)) {
      Scheme_Object *local_key = SCHEME_VEC_ELS(a)[0];
      
      scheme_hash_set(rns, local_key, a);
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
      /* note: information on nominals has been dropped */

      count = SCHEME_VEC_SIZE(a);
      for (i = 0; i < count; i+= 2) {
	key = SCHEME_VEC_ELS(a)[i];
	p = SCHEME_VEC_ELS(a)[i+1];
	  
	scheme_hash_set(mrn->ht, key, p);
      }

      scheme_hash_set(rns, local_key, (Scheme_Object *)mrn);

      a = (Scheme_Object *)mrn;
    } else if (SAME_OBJ(a, scheme_true)) {
      /* current env rename */
      Scheme_Env *env = (Scheme_Env *)scheme_get_param(scheme_current_thread->config, MZCONFIG_ENV);
      
      a = env->rename;
    } else if (SCHEME_FALSEP(a)) {
      /* current exp-env rename */
      Scheme_Env *env = (Scheme_Env *)scheme_get_param(scheme_current_thread->config, MZCONFIG_ENV);
      scheme_prepare_exp_env(env);
      if (!env->exp_env->rename) {
	Scheme_Object *rn;
	rn = scheme_make_module_rename(1, 1);
	env->exp_env->rename = rn;
      }
      a = env->exp_env->rename;
    } else if (SCHEME_SYMBOLP(a)) {
      /* mark barrier */
    } else {
      /* must be a box for a phase shift */
    }

    if (wc)
      wc->a[--stack_size] = a;

    w = SCHEME_CDR(w);
  }

  if (wc)
    a = (Scheme_Object *)wc;
  a = CONS(a, scheme_null);

  scheme_hash_set(rns, wraps_key, a);

  return a;
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

    if (size)
      SCHEME_SET_VECTOR_IMMUTABLE(result);
  } else {
    result = o;
  }

  if (SCHEME_FALSEP((Scheme_Object *)stx_src))
    result = scheme_make_stx(result, empty_srcloc, NULL);
  else
    result = scheme_make_stx(result, stx_src->srcloc, NULL);

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
    scheme_make_graph_stx(result, -1, -1, -1);
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
/*                              simplify                                  */
/*========================================================================*/

#ifdef DO_STACK_CHECK
static void simplify_syntax_inner(Scheme_Object *o,
				  Scheme_Hash_Table *rns);

static Scheme_Object *simplify_syntax_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *rns = (Scheme_Hash_Table *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  simplify_syntax_inner(o, rns);

  return NULL;
}
#endif

static void simplify_syntax_inner(Scheme_Object *o, 
				  Scheme_Hash_Table *rns)
{
  Scheme_Stx *stx = (Scheme_Stx *)o;
  Scheme_Object *v;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      p->ku.k.p1 = (void *)o;
      p->ku.k.p2 = (void *)rns;
      scheme_handle_stack_overflow(simplify_syntax_k);
      return;
    }
  }
#endif
  SCHEME_USE_FUEL(1);
 
  /* Propagate wraps: */
  scheme_stx_content((Scheme_Object *)stx);

  if (rns) {
    v = wraps_to_datum(stx->wraps, rns, 1);
    stx->wraps = v;
  }

  v = stx->val;
  
  if (SCHEME_PAIRP(v)) {
    while (SCHEME_PAIRP(v)) {
      simplify_syntax_inner(SCHEME_CAR(v), rns);
      v = SCHEME_CDR(v);
    }
    if (!SCHEME_NULLP(v)) {
      simplify_syntax_inner(v, rns);
    }
  } else if (SCHEME_BOXP(v)) {
    simplify_syntax_inner(SCHEME_BOX_VAL(v), rns);
  } else if (SCHEME_VECTORP(v)) {
    int size = SCHEME_VEC_SIZE(v), i;
    
    for (i = 0; i < size; i++) {
      simplify_syntax_inner(SCHEME_VEC_ELS(v)[i], rns);
    }
  }
}

Scheme_Object *scheme_new_stx_simplify_cache()
{
  return (Scheme_Object *)scheme_make_hash_table(SCHEME_hash_ptr);
}

void scheme_simplify_stx(Scheme_Object *stx, Scheme_Object *cache)
{
  if (cache) {
    Scheme_Hash_Table *rns;

    rns = (Scheme_Hash_Table *)cache;

    simplify_syntax_inner(stx, rns);
  }
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

static int nonneg_exact_or_false_p(Scheme_Object *o)
{
  return SCHEME_FALSEP(o) || scheme_nonneg_exact_p(o);
}

static int pos_exact_or_false_p(Scheme_Object *o)
{
  return (SCHEME_FALSEP(o)
	  || (SCHEME_INTP(o) && (SCHEME_INT_VAL(o) > 0))
	  || (SCHEME_BIGNUMP(o) && SCHEME_BIGPOS(o)));
}

static Scheme_Object *datum_to_syntax(int argc, Scheme_Object **argv)
{
  Scheme_Object *src = scheme_false, *properties = NULL;

  if (!SCHEME_FALSEP(argv[0]) && !SCHEME_STXP(argv[0]))
    scheme_wrong_type("datum->syntax-object", "syntax or #f", 0, argc, argv);
  if (argc > 2) {
    int ll;

    src = argv[2];

    ll = scheme_proper_list_length(src);

    if (!SCHEME_FALSEP(src) 
	&& !SCHEME_STXP(src)
	&& !((ll == 5)
	     && pos_exact_or_false_p(SCHEME_CADR(src))
	     && pos_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(src)))
	     && pos_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(src))))
	     && nonneg_exact_or_false_p(SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(src)))))))
      scheme_wrong_type("datum->syntax-object", "syntax, source location list, or #f", 2, argc, argv);

    if (argc > 3) {
      if (!SCHEME_FALSEP(argv[3])) {
	if (!SCHEME_STXP(argv[3]))
	  scheme_wrong_type("datum->syntax-object", "syntax or #f", 3, argc, argv);
	properties = ((Scheme_Stx *)argv[3])->props;
      }
    }

    if (ll == 5) {
      /* line--column--pos--span format */
      Scheme_Object *line, *col, *pos, *span;
      line = SCHEME_CADR(src);
      col = SCHEME_CADR(SCHEME_CDR(src));
      pos = SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(src)));
      span = SCHEME_CADR(SCHEME_CDR(SCHEME_CDR(SCHEME_CDR(src))));
      src = SCHEME_CAR(src);
      
      if (SCHEME_FALSEP(line) != SCHEME_FALSEP(col))
	scheme_arg_mismatch("datum->syntax-object", 
			    "line and column positions must both be numbers or #f in: ", 
			    argv[2]);

      /* Too-large positions go to unknown */
      if (SCHEME_BIGNUMP(line) || SCHEME_BIGNUMP(col)) {
	line = scheme_make_integer(-1);
	col = scheme_make_integer(-1);
      }
      if (SCHEME_BIGNUMP(pos))
	pos = scheme_make_integer(-1);
      if (span && SCHEME_BIGNUMP(span))
	span = scheme_make_integer(-1);

      src = scheme_make_stx_w_offset(scheme_false,
				     SCHEME_FALSEP(line) ? -1 : SCHEME_INT_VAL(line),
				     SCHEME_FALSEP(col) ? -1 : SCHEME_INT_VAL(col),
				     SCHEME_FALSEP(pos) ? -1 : SCHEME_INT_VAL(pos),
				     SCHEME_FALSEP(span) ? -1 : SCHEME_INT_VAL(span),
				     src,
				     NULL);
    }
  }
  
  src = scheme_datum_to_syntax(argv[1], src, argv[0], 1, 0);

  if (properties) {
    if (!((Scheme_Stx *)src)->props)
      ((Scheme_Stx *)src)->props = properties;
  }

  return src;
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
    
  if (stx->srcloc->line < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->line);
}

static Scheme_Object *syntax_col(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-column", "syntax", 0, argc, argv);
    
  if (stx->srcloc->col < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->col);
}

static Scheme_Object *syntax_pos(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-position", "syntax", 0, argc, argv);
    
  if (stx->srcloc->pos < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->pos);
}

static Scheme_Object *syntax_span(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-span", "syntax", 0, argc, argv);
    
  if (stx->srcloc->span < 0)
    return scheme_false;
  else
    return scheme_make_integer(stx->srcloc->span);
}

static Scheme_Object *syntax_src(int argc, Scheme_Object **argv)
{
  Scheme_Stx *stx = (Scheme_Stx *)argv[0];

  if (!SCHEME_STXP(argv[0]))
    scheme_wrong_type("syntax-source", "syntax", 0, argc, argv);

  return stx->srcloc->src;
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
  WRAP_POS awl;
  WRAP_POS ewl;

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

  WRAP_POS_INIT(awl, stx->wraps);
  WRAP_POS_INIT_END(ewl);

  if (same_marks(&awl, &ewl, 1))
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
	l = CONS(CONS(source_symbol, scheme_true),
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
	    p = CONS(SCHEME_CDR(SCHEME_CAR(e)), scheme_null);
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
    Scheme_Object *wraps, *modinfo_cache;
    long lazy_prefix;
    int graph;
    
    graph = (stx->hash_code & STX_GRAPH_FLAG);

    l = CONS(CONS(key, val), l);

    wraps = stx->wraps;
    if (stx->hash_code & STX_SUBSTX_FLAG) {
      modinfo_cache = NULL;
      lazy_prefix = stx->u.lazy_prefix;
    } else {
      modinfo_cache = stx->u.modinfo_cache;
      lazy_prefix = 0;
    }

    stx = (Scheme_Stx *)scheme_make_stx(stx->val, stx->srcloc, l);

    stx->wraps = wraps;
    if (modinfo_cache)
      stx->u.modinfo_cache = modinfo_cache;
    else
      stx->u.lazy_prefix = lazy_prefix; /* same as NULL modinfo if no SUBSTX */

    if (graph)
      stx->hash_code |= STX_GRAPH_FLAG;

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

static Scheme_Object *do_module_binding(char *name, int argc, Scheme_Object **argv, 
					int dphase, int get_position)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *a, *m, *nom_mod, *nom_a;

  a = argv[0];

  if (!SCHEME_STXP(a) || !SCHEME_STX_SYMBOLP(a))
    scheme_wrong_type(name, "identifier syntax", 0, argc, argv);

  m = scheme_stx_module_name(&a, dphase + (p->current_local_env
					   ? p->current_local_env->genv->phase
					   : 0),
			     &nom_mod, &nom_a);

  if (!m)
    return scheme_false;
  else if (SAME_OBJ(m, scheme_undefined)) {
    if (get_position)
      return scheme_false;
    else
      return lexical_symbol;
  } else {
    if (get_position) {
      int pos;

      m = scheme_module_resolve(m);
      pos = scheme_module_export_position(m, scheme_get_env(scheme_config), a);
      if (pos < 0)
	return scheme_false;
      else
	return scheme_make_integer(pos);
    } else
      return CONS(m, CONS(a, CONS(nom_mod, CONS(nom_a, scheme_null))));
  }
}

static Scheme_Object *module_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-binding", argc, argv, 0, 0);
}

static Scheme_Object *module_trans_binding(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-transformer-binding", argc, argv, 1, 0);
}

static Scheme_Object *module_binding_pos(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-binding-export-position", argc, argv, 0, 1);
}

static Scheme_Object *module_trans_binding_pos(int argc, Scheme_Object **argv)
{
  return do_module_binding("identifier-transformer-binding-export-position", argc, argv, 1, 1);
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
  GC_REG_TRAV(scheme_rt_srcloc, mark_srcloc);
  GC_REG_TRAV(scheme_wrap_chunk_type, mark_wrapchunk);
}

END_XFORM_SKIP;

#endif
