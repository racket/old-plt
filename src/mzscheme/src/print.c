/*
  MzScheme
  Copyright (c) 2004 PLT Scheme, Inc.
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
#include "schmach.h"
#include "schcpt.h"
#include <ctype.h>
#ifdef USE_STACKAVAIL
# include <malloc.h>
#endif

/* Flag for debugging compiled code in printed form: */
#define NO_COMPACT 0

#define PRINT_MAXLEN_MIN 3

/* locals */
#define MAX_PRINT_BUFFER 500

#define quick_print_struct quick_can_read_compiled
#define quick_print_graph quick_can_read_graph
#define quick_print_box quick_can_read_box
#define quick_print_vec_shorthand quick_square_brackets_are_parens
#define quick_print_hash_table quick_curly_braces_are_parens
/* Don't use can_read_pipe_quote or case_sens! */

static void print_to_port(char *name, Scheme_Object *obj, Scheme_Object *port, 
			  int notdisplay, long maxl, Scheme_Thread *p,
			  Scheme_Config *config);
static int print(Scheme_Object *obj, int notdisplay, int compact, 
		 Scheme_Hash_Table *ht,
		 Scheme_Hash_Table *symtab, Scheme_Hash_Table *rnht,
		 Scheme_Thread *p);
static void print_string(Scheme_Object *string, int notdisplay, Scheme_Thread *p);
static void print_pair(Scheme_Object *pair, int notdisplay, int compact, 
		       Scheme_Hash_Table *ht, 
		       Scheme_Hash_Table *symtab, Scheme_Hash_Table *rnht, 
		       Scheme_Thread *p);
static void print_vector(Scheme_Object *vec, int notdisplay, int compact, 
			 Scheme_Hash_Table *ht, 
			 Scheme_Hash_Table *symtab, Scheme_Hash_Table *rnht, 
			 Scheme_Thread *p);
static void print_char(Scheme_Object *chobj, int notdisplay, Scheme_Thread *p);
static char *print_to_string(Scheme_Object *obj, long * volatile len, int write,
			     Scheme_Object *port, long maxl,
			     Scheme_Thread * volatile p, Scheme_Config *config);

static Scheme_Object *quote_link_symbol = NULL;
static char *quick_buffer = NULL;

static char compacts[_CPT_COUNT_];

static Scheme_Hash_Table *global_constants_ht;

#define print_compact(p, v) print_this_string(p, &compacts[v], 0, 1)

#define PRINTABLE_STRUCT(obj, p) (scheme_inspector_sees_part(obj, p->quick_inspector, -1))

#define HAS_SUBSTRUCT(obj, qk) \
   (SCHEME_PAIRP(obj) || SCHEME_VECTORP(obj) \
    || (qk(p->quick_print_box, 1) && SCHEME_BOXP(obj)) \
    || (qk(p->quick_print_struct  \
	   && SCHEME_STRUCTP(obj) \
	   && PRINTABLE_STRUCT(obj, p), 0)) \
    || (qk(p->quick_print_hash_table, 1) && SCHEME_HASHTP(obj)))
#define ssQUICK(x, isbox) x
#define ssQUICKp(x, isbox) (p ? x : isbox)
#define ssALL(x, isbox) 1
#define ssALLp(x, isbox) isbox

#ifdef MZ_PRECISE_GC
# define ZERO_SIZED(closure) !(closure->closure_size)
#else
# define ZERO_SIZED(closure) closure->zero_sized
#endif


void scheme_init_print(Scheme_Env *env)
{
  int i;

  REGISTER_SO(quick_buffer);
  
  quick_buffer = (char *)scheme_malloc_atomic(100);
  
  REGISTER_SO(quote_link_symbol);
  
  quote_link_symbol = scheme_intern_symbol("-q");
  
  for (i = 0; i < _CPT_COUNT_; i++) {
    compacts[i] = i;
  }
}

Scheme_Object *scheme_make_svector(mzshort c, mzshort *a)
{
  Scheme_Object *o;
  o = scheme_alloc_object();

  o->type = scheme_svector_type;
  SCHEME_SVEC_LEN(o) = c;
  SCHEME_SVEC_VEC(o) = a;

  return o;
}

void
scheme_debug_print (Scheme_Object *obj)
{
  scheme_write(obj, scheme_orig_stdout_port);
  fflush (stdout);
}

static void *print_to_port_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj, *port;

  port = (Scheme_Object *)p->ku.k.p1;
  obj = (Scheme_Object *)p->ku.k.p2;

  print_to_port(p->ku.k.i2 ? "write" : "display", 
		obj, port,
		p->ku.k.i2, p->ku.k.i1,
		p, p->config);

  return NULL;
}

static void do_handled_print(Scheme_Object *obj, Scheme_Object *port,
			     Scheme_Object *proc, long maxl)
{
  Scheme_Object *a[2];

  a[0] = obj;
  
  if (maxl > 0) {
    a[1] = scheme_make_string_output_port();
  } else
    a[1] = port;
  
  scheme_apply_multi(scheme_write_proc, 2, a);
  
  if (maxl > 0) {
    char *s;
    long len;

    s = scheme_get_sized_string_output(a[1], &len);
    if (len > maxl)
      len = maxl;

    scheme_write_string(s, len, port);
  }
}

void scheme_write_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl)
{
  if (((Scheme_Output_Port *)port)->write_handler)
    do_handled_print(obj, port, scheme_write_proc, maxl);
  else {
    Scheme_Thread *p = scheme_current_thread;
    
    p->ku.k.p1 = port;
    p->ku.k.p2 = obj;
    p->ku.k.i1 = maxl;
    p->ku.k.i2 = 1;
    
    (void)scheme_top_level_do(print_to_port_k, 0);
  }
}

void scheme_write(Scheme_Object *obj, Scheme_Object *port)
{
  scheme_write_w_max(obj, port, -1);
}

void scheme_display_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl)
{
  if (((Scheme_Output_Port *)port)->display_handler)
    do_handled_print(obj, port, scheme_display_proc, maxl);
  else {
    Scheme_Thread *p = scheme_current_thread;
    
    p->ku.k.p1 = port;
    p->ku.k.p2 = obj;
    p->ku.k.i1 = maxl;
    p->ku.k.i2 = 0;
    
    (void)scheme_top_level_do(print_to_port_k, 0);
  }
}

void scheme_display(Scheme_Object *obj, Scheme_Object *port)
{
  scheme_display_w_max(obj, port, -1);
}

static void *print_to_string_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *obj;
  long *len, maxl;
  int iswrite;

  obj = (Scheme_Object *)p->ku.k.p1;
  len = (long *)p->ku.k.p2;
  maxl = p->ku.k.i1;
  iswrite = p->ku.k.i2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return (void *)print_to_string(obj, len, iswrite, 
				 NULL, maxl,
				 p, p->config);
}

char *scheme_write_to_string_w_max(Scheme_Object *obj, long *len, long maxl)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = len;
  p->ku.k.i1 = maxl;
  p->ku.k.i2 = 1;

  return (char *)scheme_top_level_do(print_to_string_k, 0);
}

char *scheme_write_to_string(Scheme_Object *obj, long *len)
{
  return scheme_write_to_string_w_max(obj, len, -1);
}

char *scheme_display_to_string_w_max(Scheme_Object *obj, long *len, long maxl)
{
  Scheme_Thread *p = scheme_current_thread;

  p->ku.k.p1 = obj;
  p->ku.k.p2 = len;
  p->ku.k.i1 = maxl;
  p->ku.k.i2 = 0;

  return (char *)scheme_top_level_do(print_to_string_k, 0);
}

char *scheme_display_to_string(Scheme_Object *obj, long *len)
{
  return scheme_display_to_string_w_max(obj, len, -1);
}

void
scheme_internal_write(Scheme_Object *obj, Scheme_Object *port, 
		      Scheme_Config *config)
{
  print_to_port("write", obj, port, 1, -1, scheme_current_thread, config);
}

void
scheme_internal_display(Scheme_Object *obj, Scheme_Object *port, 
		      Scheme_Config *config)
{
  print_to_port("display", obj, port, 0, -1, scheme_current_thread, config);
}

#ifdef DO_STACK_CHECK
static int check_cycles(Scheme_Object *, Scheme_Thread *, Scheme_Hash_Table *);

static Scheme_Object *check_cycle_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p2;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;

  return check_cycles(o, p, ht)
    ? scheme_true : scheme_false;
}
#endif

static int check_cycles(Scheme_Object *obj, Scheme_Thread *p, Scheme_Hash_Table *ht)
{
  Scheme_Type t;

  t = SCHEME_TYPE(obj);

#ifdef DO_STACK_CHECK
#define CHECK_COUNT_START 50
  {
    static int check_counter = CHECK_COUNT_START;

    if (!--check_counter) {
      check_counter = CHECK_COUNT_START;
      SCHEME_USE_FUEL(CHECK_COUNT_START);
      {
#include "mzstkchk.h"
	{
	  scheme_current_thread->ku.k.p1 = (void *)obj;
	  scheme_current_thread->ku.k.p2 = (void *)ht;
	  return SCHEME_TRUEP(scheme_handle_stack_overflow(check_cycle_k));
	}
      }
    }
  }
#else
  SCHEME_USE_FUEL(1);
#endif

  if (SCHEME_PAIRP(obj)
      || (p->quick_print_box && SCHEME_BOXP(obj))
      || SCHEME_VECTORP(obj)
      || (p->quick_print_struct 
	  && (SAME_TYPE(t, scheme_structure_type)
	      || SAME_TYPE(t, scheme_proc_struct_type))
	  && PRINTABLE_STRUCT(obj, p))
      || (p->quick_print_hash_table
	  && SAME_TYPE(t, scheme_hash_table_type))) {
    if (scheme_hash_get(ht, obj))
      return 1;
    scheme_hash_set(ht, obj, (Scheme_Object *)0x1);
  } else 
    return 0;

  if (SCHEME_PAIRP(obj)) {
    if (check_cycles(SCHEME_CAR(obj), p, ht))
      return 1;
    if (check_cycles(SCHEME_CDR(obj), p, ht))
      return 1;
  } else if (SCHEME_BOXP(obj)) {
    /* got here => printable */
    if (check_cycles(SCHEME_BOX_VAL(obj), p, ht))
      return 1;
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;

    len = SCHEME_VEC_SIZE(obj);
    for (i = 0; i < len; i++) {
      if (check_cycles(SCHEME_VEC_ELS(obj)[i], p, ht)) {
	return 1;
      }
    }
  } else if (SAME_TYPE(t, scheme_structure_type)
	     || SAME_TYPE(t, scheme_proc_struct_type)) {
    /* got here => printable */
    int i = SCHEME_STRUCT_NUM_SLOTS(obj);

    while (i--) {
      if (scheme_inspector_sees_part(obj, p->quick_inspector, i)) {
	if (check_cycles(((Scheme_Structure *)obj)->slots[i], p, ht)) {
	  return 1;
	}
      }
    }
  }  else if (SCHEME_HASHTP(obj)) {
    /* got here => printable */
    Scheme_Hash_Table *t;
    Scheme_Object **keys, **vals, *val;
    int i;
    
    t = (Scheme_Hash_Table *)obj;
    keys = t->keys;
    vals = t->vals;
    for (i = t->size; i--; ) {
      if (vals[i]) {
	val = vals[i];
	if (check_cycles(keys[i], p, ht))
	  return 1;
	if (check_cycles(val, p, ht))
	  return 1;
      }
    }
  }

  scheme_hash_set(ht, obj, NULL);

  return 0;
}

#ifdef MZ_XFORM
START_XFORM_SKIP;
#endif

/* The fast cycle-checker plays a dangerous game: it changes type
   tags. No GCs can occur here, and no thread switches. If the fast
   version takes to long, we back out to the general case. (We don't
   even check for stack overflow, so keep the max limit low.) */

static int fast_checker_counter;

static int check_cycles_fast(Scheme_Object *obj, Scheme_Thread *p)
{
  Scheme_Type t;
  int cycle = 0;

  t = SCHEME_TYPE(obj);
  if (t < 0)
    return 1;

  if (fast_checker_counter-- < 0)
    return -1;

  if (SCHEME_PAIRP(obj)) {
    obj->type = -t;
    cycle = check_cycles_fast(SCHEME_CAR(obj), p);
    if (!cycle)
      cycle = check_cycles_fast(SCHEME_CDR(obj), p);
    obj->type = t;
  } else if (p->quick_print_box && SCHEME_BOXP(obj)) {
    obj->type = -t;
    cycle = check_cycles_fast(SCHEME_BOX_VAL(obj), p);
    obj->type = t;
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;

    obj->type = -t;
    len = SCHEME_VEC_SIZE(obj);
    for (i = 0; i < len; i++) {
      cycle = check_cycles_fast(SCHEME_VEC_ELS(obj)[i], p);
      if (cycle)
	break;
    }
    obj->type = t;
  } else if (p->quick_print_struct 
	     && (SAME_TYPE(t, scheme_structure_type)
		 || SAME_TYPE(t, scheme_proc_struct_type))
	     && PRINTABLE_STRUCT(obj, p)) {
    int i = SCHEME_STRUCT_NUM_SLOTS(obj);

    obj->type = -t;
    while (i--) {
      if (scheme_inspector_sees_part(obj, p->quick_inspector, i)) {
	cycle = check_cycles_fast(((Scheme_Structure *)obj)->slots[i], p);
	if (cycle)
	  break;
      }
    }
    obj->type = t;
  } else if (p->quick_print_hash_table
	     && SCHEME_HASHTP(obj)) {
    if (!((Scheme_Hash_Table *)obj)->count)
      cycle = 0;
    else
      /* don't bother with fast checks for non-empty hash tables */
      cycle = -1;
  } else
    cycle = 0;

  return cycle;
}

#ifdef MZ_XFORM
END_XFORM_SKIP;
#endif

#ifdef DO_STACK_CHECK
static void setup_graph_table(Scheme_Object *obj, Scheme_Hash_Table *ht,
			      int *counter, Scheme_Thread *p);

static Scheme_Object *setup_graph_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p2;
  int *counter = (int *)p->ku.k.p3;
  Scheme_Thread *pp = (Scheme_Thread *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  setup_graph_table(o, ht, counter, pp);

  return scheme_false;
}
#endif

static void setup_graph_table(Scheme_Object *obj, Scheme_Hash_Table *ht,
			      int *counter, Scheme_Thread *p)
{
  if (HAS_SUBSTRUCT(obj, ssQUICKp)) {
    Scheme_Object *v;

#ifdef DO_STACK_CHECK
    {
# include "mzstkchk.h"
      {
	scheme_current_thread->ku.k.p1 = (void *)obj;
	scheme_current_thread->ku.k.p2 = (void *)ht;
	scheme_current_thread->ku.k.p3 = (void *)counter;
	scheme_current_thread->ku.k.p4 = (void *)p;
	scheme_handle_stack_overflow(setup_graph_k);
	return;
      }
    }
#endif

    v = scheme_hash_get(ht, obj);

    if (!v)
      scheme_hash_set(ht, obj, (Scheme_Object *)0x1);
    else {
      if ((long)v == 1) {
	*counter += 2;
	scheme_hash_set(ht, obj, (Scheme_Object *)(long)*counter);
      }
      return;
    }
  } else
    return;

  SCHEME_USE_FUEL(1);

  if (SCHEME_PAIRP(obj)) {
    setup_graph_table(SCHEME_CAR(obj), ht, counter, p);
    setup_graph_table(SCHEME_CDR(obj), ht, counter, p);
  } else if ((!p || p->quick_print_box) && SCHEME_BOXP(obj)) {
    setup_graph_table(SCHEME_BOX_VAL(obj), ht, counter, p);
  } else if (SCHEME_VECTORP(obj)) {
    int i, len;

    len = SCHEME_VEC_SIZE(obj);
    for (i = 0; i < len; i++) {
      setup_graph_table(SCHEME_VEC_ELS(obj)[i], ht, counter, p);
    }
  } else if (p && SCHEME_STRUCTP(obj)) { /* got here => printable */
    int i = SCHEME_STRUCT_NUM_SLOTS(obj);

    while (i--) {
      if (scheme_inspector_sees_part(obj, p->quick_inspector, i))
	setup_graph_table(((Scheme_Structure *)obj)->slots[i], ht, counter, p);
    }
  } else if (p && SCHEME_HASHTP(obj)) { /* got here => printable */
    Scheme_Hash_Table *t;
    Scheme_Object **keys, **vals, *val;
    int i;
    
    t = (Scheme_Hash_Table *)obj;
    keys = t->keys;
    vals = t->vals;
    for (i = t->size; i--; ) {
      if (vals[i]) {
	val = vals[i];
	setup_graph_table(keys[i], ht, counter, p);
	setup_graph_table(val, ht, counter, p);
      }
    }
  }
}

Scheme_Hash_Table *scheme_setup_datum_graph(Scheme_Object *o, int for_print)
{
  Scheme_Hash_Table *ht;
  int counter = 1;

  ht = scheme_make_hash_table(SCHEME_hash_ptr);
  setup_graph_table(o, ht, &counter, 
		    for_print ? scheme_current_thread : NULL);

  if (counter > 1)
    return ht;
  else
    return NULL;
}

static char *
print_to_string(Scheme_Object *obj, 
		long * volatile len, int write,
		Scheme_Object *port, long maxl, 
		Scheme_Thread * volatile p,
		Scheme_Config *config)
{
  Scheme_Hash_Table * volatile ht;
  char *ca;
  int cycles;

  p->print_allocated = 50;
  ca = (char *)scheme_malloc_atomic(p->print_allocated);
  p->print_buffer = ca;
  p->print_position = 0;
  p->print_maxlen = maxl;
  p->print_port = port;

  p->quick_print_graph = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_GRAPH));
  p->quick_print_box = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_BOX));
  p->quick_print_struct = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_STRUCT));
  p->quick_print_vec_shorthand = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_VEC_SHORTHAND));
  p->quick_print_hash_table = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_PRINT_HASH_TABLE));
  p->quick_can_read_pipe_quote = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_CAN_READ_PIPE_QUOTE));
  p->quick_case_sens = SCHEME_TRUEP(scheme_get_param(config, MZCONFIG_CASE_SENS));
  p->quick_inspector = scheme_get_param(config, MZCONFIG_INSPECTOR);

  if (p->quick_print_graph)
    cycles = 1;
  else {
    fast_checker_counter = 50;
    cycles = check_cycles_fast(obj, p);
    if (cycles == -1) {
      ht = scheme_make_hash_table(SCHEME_hash_ptr);
      cycles = check_cycles(obj, p, ht);
    }
  }

  if (cycles)
    ht = scheme_setup_datum_graph(obj, 1);
  else
    ht = NULL;

  if ((maxl <= PRINT_MAXLEN_MIN) 
      || !scheme_setjmp(p->print_escape))
    print(obj, write, 0, ht, NULL, NULL, p);

  p->print_buffer[p->print_position] = '\0';

  if (len)
    *len = p->print_position;

  p->quick_inspector = NULL;

  return p->print_buffer;
}

static void 
print_to_port(char *name, Scheme_Object *obj, Scheme_Object *port, int notdisplay,
	      long maxl, Scheme_Thread *p, Scheme_Config *config)
{
  Scheme_Output_Port *op;
  char *str;
  long len;
  
  op = (Scheme_Output_Port *)port;
  if (op->closed)
    scheme_raise_exn(MZEXN_I_O_PORT_CLOSED, port, "%s: output port is closed", name);

  str = print_to_string(obj, &len, notdisplay, port, maxl, p, config);

  scheme_write_string(str, len, port);
}

static void print_this_string(Scheme_Thread *p, const char *str, int offset, int autolen)
{
  long len;
  char *oldstr;

  if (!autolen) {
    return;
  } else if (autolen > 0)
    len = autolen;
  else
    len = strlen(str + offset);

  if (!p->print_buffer) {
    /* Just getting the length */
    p->print_position += len;
    return;
  }


  if (len + p->print_position + 1 > p->print_allocated) {
    if (len + 1 >= p->print_allocated)
      p->print_allocated = 2 * p->print_allocated + len + 1;
    else
      p->print_allocated = 2 * p->print_allocated;

    oldstr = p->print_buffer;
    {
      char *ca;
      ca = (char *)scheme_malloc_atomic(p->print_allocated);
      p->print_buffer = ca;
    }
    memcpy(p->print_buffer, oldstr, p->print_position);
  }

  memcpy(p->print_buffer + p->print_position, str + offset, len);
  p->print_position += len;

  SCHEME_USE_FUEL(len);
  
  if (p->print_maxlen > PRINT_MAXLEN_MIN)  {
    if (p->print_position > p->print_maxlen) {
      long l = p->print_maxlen;

      p->print_buffer[l] = 0;
      p->print_buffer[l - 1] = '.';
      p->print_buffer[l - 2] = '.';
      p->print_buffer[l - 3] = '.';

      p->print_position = l;

      scheme_longjmp(p->print_escape, 1);
    }
  } else if (p->print_position > MAX_PRINT_BUFFER) {
    if (p->print_port) {
      p->print_buffer[p->print_position] = 0;
      scheme_write_string(p->print_buffer, p->print_position, p->print_port);
      
      p->print_position = 0;
    }
  }
}

static void print_compact_number(Scheme_Thread *p, long n)
{
  unsigned char s[5];

  if (n < 0) {
    if (n > -256) {
      s[0] = 254;
      s[1] = (unsigned char)(-n);
      print_this_string(p, (char *)s, 0, 2);
      return;
    } else {
      n = -n;
      s[0] = 255;
    }
  } else if (n < 252) {
    s[0] = (unsigned char)n;
    print_this_string(p, (char *)s, 0, 1);
    return;
  } else if (n < 0x10000) {
    s[0] = 252;
    s[1] = (unsigned char)(n & 0xFF);
    s[2] = (unsigned char)((n >> 8) & 0xFF);
    print_this_string(p, (char *)s, 0, 3);
    return;
  } else {
    s[0] = 253;
  }

  s[1] = (unsigned char)(n & 0xFF);
  s[2] = (unsigned char)((n >> 8) & 0xFF);
  s[3] = (unsigned char)((n >> 16) & 0xFF);
  s[4] = (unsigned char)((n >> 24) & 0xFF);  
  
  print_this_string(p, (char *)s, 0, 5);
}

static void print_string_in_angle(Scheme_Thread *p, const char *start, const char *prefix, int slen)
{
  /* Used to do something special for type symbols. No more. */
  print_this_string(p, prefix, 0, -1);
  print_this_string(p, start, 0, slen);
}

#ifdef DO_STACK_CHECK

static Scheme_Object *print_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Scheme_Object *o = (Scheme_Object *)p->ku.k.p1;
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p->ku.k.p2;
  Scheme_Hash_Table *symtab = (Scheme_Hash_Table *)p->ku.k.p3;
  Scheme_Hash_Table *rnht = (Scheme_Hash_Table *)p->ku.k.p4;

  p->ku.k.p1 = NULL;
  p->ku.k.p2 = NULL;
  p->ku.k.p3 = NULL;
  p->ku.k.p4 = NULL;

  return print(o, 
	       p->ku.k.i1, 
	       p->ku.k.i2, 
	       ht,
	       symtab, rnht,
	       p) 
    ? scheme_true : scheme_false;
}
#endif

static int
print_substring(Scheme_Object *obj, int notdisplay, int compact, Scheme_Hash_Table *ht,
		Scheme_Hash_Table *symtab, Scheme_Hash_Table *rnht, 
		Scheme_Thread *p, char **result, long *rlen)
{
  int closed;
  long save_alloc, save_pos, save_maxl;
  char *save_buf;
  Scheme_Object *save_port;

  save_alloc = p->print_allocated;
  save_buf = p->print_buffer;
  save_pos = p->print_position;
  save_maxl = p->print_maxlen;
  save_port = p->print_port;
  
  /* If result is NULL, just measure the output. */
  if (result) {
    char *ca;
    p->print_allocated = 50;
    ca = (char *)scheme_malloc_atomic(p->print_allocated);
    p->print_buffer = ca;
  } else {
    p->print_allocated = 0;
    p->print_buffer = NULL;
  }
  p->print_position = 0;
  p->print_port = NULL;

  closed = print(obj, notdisplay, compact, ht, symtab, rnht, p);

  if (result)
    *result = p->print_buffer;
  *rlen = p->print_position;

  p->print_allocated = save_alloc;
  p->print_buffer = save_buf;
  p->print_position = save_pos;
  p->print_maxlen = save_maxl;
  p->print_port = save_port;
  
  return closed;
}

static void print_escaped(Scheme_Thread *p, int notdisplay, 
			  Scheme_Object *obj, Scheme_Hash_Table *ht)
{
  char *r;
  long len;

  print_substring(obj, notdisplay, 0, ht, NULL, NULL, p, &r, &len);

  print_compact(p, CPT_ESCAPE);
  print_compact_number(p, len);
  print_this_string(p, r, 0, len);
}

static void cannot_print(Scheme_Thread *p, int notdisplay, 
			 Scheme_Object *obj, Scheme_Hash_Table *ht)
{
  scheme_raise_exn(MZEXN_APPLICATION_TYPE, 
		   obj,
		   scheme_intern_symbol("code with only printable constants"),
		   "%s: cannot marshal constant that is embedded in compiled code: %V",
		   notdisplay ? "write" : "display",
		   obj);
}

#ifdef SGC_STD_DEBUGGING
static void printaddress(Scheme_Thread *p, Scheme_Object *o)
{
  char buf[40];
  sprintf(buf, ":%lx", (long)o);
  print_this_string(p, buf, 0, -1);
}
# define PRINTADDRESS(p, obj) printaddress(p, obj)
#else
# define PRINTADDRESS(p, obj) /* empty */
#endif

static void print_named(Scheme_Object *obj, const char *kind,
			const char *s, int len, Scheme_Thread *p)
{
  print_this_string(p, "#<", 0, 2);
  print_this_string(p, kind, 0, -1);

  if (s) {
    print_this_string(p, ":", 0, 1);

    print_this_string(p, s, 0, len);
  }
   
  PRINTADDRESS(p, obj);
  print_this_string(p, ">", 0, 1);
}

static int
print(Scheme_Object *obj, int notdisplay, int compact, Scheme_Hash_Table *ht,
      Scheme_Hash_Table *symtab, Scheme_Hash_Table *rnht, Scheme_Thread *p)
{
  int closed = 0;

#if NO_COMPACT
  compact = 0;
#endif

#ifdef DO_STACK_CHECK
#define PRINT_COUNT_START 20
  {
    static int check_counter = PRINT_COUNT_START;

    if (!--check_counter) {
      check_counter = PRINT_COUNT_START;
      {
#include "mzstkchk.h"
	{
	  p->ku.k.p1 = (void *)obj;
	  p->ku.k.i1 = notdisplay;
	  p->ku.k.i2 = compact;
	  p->ku.k.p2 = (void *)ht;
	  return SCHEME_TRUEP(scheme_handle_stack_overflow(print_k));
	}
      }
    }
  }
#endif

  /* Built-in functions, exception types, eof, prop:waitable, ... */
  if (compact && (SCHEME_PROCP(obj) 
		  || SCHEME_STRUCT_TYPEP(obj) 
		  || SCHEME_EOFP(obj)
		  || SAME_TYPE(scheme_struct_property_type, SCHEME_TYPE(obj)))) {
    /* Check whether this is a global constant */
    Scheme_Object *val;
    val = scheme_hash_get(global_constants_ht, obj);
    if (val) {
      /* val is a scheme_variable_type object, instead of something else */
      obj = val;
    }
  }

  if (ht && HAS_SUBSTRUCT(obj, ssQUICK)) {
    long val;
    
    val = (long)scheme_hash_get(ht, obj);
    
    if (val) {
      if (val != 1) {
	if (compact) {
	  print_escaped(p, notdisplay, obj, ht);
	  return 1;
	} else {
	  if (val > 0) {
	    sprintf(quick_buffer, "#%ld=", (val - 3) >> 1);
	    print_this_string(p, quick_buffer, 0, -1);
	    scheme_hash_set(ht, obj, (Scheme_Object *)(-val));
	  } else {
	    sprintf(quick_buffer, "#%ld#", ((-val) - 3) >> 1);
	    print_this_string(p, quick_buffer, 0, -1);
	    return 0;
	  }
	}
      }
    }
  }

  if (SCHEME_SYMBOLP(obj))
    {
      int l;
      Scheme_Object *idx;

      if (compact)
	idx = scheme_hash_get(symtab, obj);
      else
	idx = NULL;

      if (idx) {
	print_compact(p, CPT_SYMREF);
	l = SCHEME_INT_VAL(idx);
	print_compact_number(p, l);
      } else if (compact) {
	int weird;

	weird = SCHEME_SYM_WEIRDP(obj);
	l = SCHEME_SYM_LEN(obj);
	if (!weird && (l < CPT_RANGE(SMALL_SYMBOL))) {
	  unsigned char s[1];
	  s[0] = l + CPT_SMALL_SYMBOL_START;
	  print_this_string(p, (char *)s, 0, 1);
	} else {
	  print_compact(p, (weird ? CPT_WEIRD_SYMBOL : CPT_SYMBOL));
	  if (weird) {
	    print_compact_number(p, SCHEME_SYM_UNINTERNEDP(obj) ? 1 : 0);
	  }
	  print_compact_number(p, l);
	  /* Note: the written symbol table will preserve equivalence
             of uninterned symbols for a single compiled
             expression. */
	}
	print_this_string(p, scheme_symbol_val(obj), 0, l);

	idx = scheme_make_integer(symtab->count);
	scheme_hash_set(symtab, obj, idx);
	
	l = SCHEME_INT_VAL(idx);
	print_compact_number(p, l);
      } else if (notdisplay) {
	const char *s;
	
	s = scheme_symbol_name_and_size(obj, (unsigned int *)&l, 
					((p->quick_can_read_pipe_quote 
					  ? SCHEME_SNF_PIPE_QUOTE
					  : SCHEME_SNF_NO_PIPE_QUOTE)
					 | (p->quick_case_sens
					    ? 0
					    : SCHEME_SNF_NEED_CASE)));
	print_this_string(p, s, 0, l);
      } else {
	print_this_string(p, (char *)obj, ((char *)(SCHEME_SYM_VAL(obj))) - ((char *)obj), 
			  SCHEME_SYM_LEN(obj));
      }
    }
  else if (SCHEME_STRINGP(obj))
    {
      if (compact) {
	int l;

	print_compact(p, CPT_STRING);
	l = SCHEME_STRTAG_VAL(obj);
	print_compact_number(p, l);
	print_this_string(p, SCHEME_STR_VAL(obj), 0, l);
      } else {
	print_string(obj, notdisplay, p);
	closed = 1;
      }
    }
  else if (SCHEME_CHARP(obj))
    {
      if (compact) {
	char s[1];
	print_compact(p, CPT_CHAR);
	s[0] = SCHEME_CHAR_VAL(obj);
	print_this_string(p, s, 0, 1);
      } else
	print_char(obj, notdisplay, p);
    }
  else if (SCHEME_INTP(obj))
    {
      if (compact) {
	long v = SCHEME_INT_VAL(obj);
	if (v >= 0 && v < CPT_RANGE(SMALL_NUMBER)) {
	  unsigned char s[1];
	  s[0] = (unsigned char)(v + CPT_SMALL_NUMBER_START);
	  print_this_string(p, (char *)s, 0, 1);
	} else {
	  print_compact(p, CPT_INT);
	  print_compact_number(p, v);
	}
      } else {
	sprintf(quick_buffer, "%ld", SCHEME_INT_VAL(obj));
	print_this_string(p, quick_buffer, 0, -1);
      }
    }
  else if (SCHEME_NUMBERP(obj))
    {
      if (compact) {
	print_escaped(p, notdisplay, obj, ht);
	closed = 1;
      } else
	print_this_string(p, scheme_number_to_string(10, obj), 0, -1);
    }
  else if (SCHEME_NULLP(obj))
    {
      if (compact) {
	print_compact(p, CPT_NULL);
      } else {
	print_this_string(p, "()", 0, 2);
	closed = 1;
      }
    }
  else if (SCHEME_PAIRP(obj))
    {
      print_pair(obj, notdisplay, compact, ht, symtab, rnht, p);
      closed = 1;
    }
  else if (SCHEME_VECTORP(obj))
    {
      print_vector(obj, notdisplay, compact, ht, symtab, rnht, p);
      closed = 1;
    }
  else if ((compact || p->quick_print_box) && SCHEME_BOXP(obj))
    {
      if (compact && !p->quick_print_box) {
	closed = print(scheme_protect_quote(obj), notdisplay, compact, ht, symtab, rnht, p);
      } else {
	if (compact)
	  print_compact(p, CPT_BOX);
	else
	  print_this_string(p, "#&", 0, 2);
	closed = print(SCHEME_BOX_VAL(obj), notdisplay, compact, ht, symtab, rnht, p);
      }
    }
  else if ((compact || p->quick_print_hash_table) && SCHEME_HASHTP(obj))
    {
      if (compact && !p->quick_print_hash_table) {
	closed = print(scheme_protect_quote(obj), notdisplay, compact, ht, symtab, rnht, p);
      } else {
	if (compact) {
	  print_escaped(p, notdisplay, obj, ht);
	} else {
	  Scheme_Hash_Table *t;
	  Scheme_Object **keys, **vals, *val;
	  int i, size, did_one = 0;

	  print_this_string(p, "#hash", 0, 5);
	  if (!scheme_is_hash_table_equal(obj))
	    print_this_string(p, "eq", 0, 2);
	  print_this_string(p, "(", 0, 1);

	  t = (Scheme_Hash_Table *)obj;
	  keys = t->keys;
	  vals = t->vals;
	  size = t->size;
	  for (i = 0; i < size; i++) {
	    if (vals[i]) {
	      if (did_one)
		print_this_string(p, " ", 0, 1);
	      print_this_string(p, "(", 0, 1);
	      val = vals[i];
	      print(keys[i], notdisplay, compact, ht, symtab, rnht, p);
	      print_this_string(p, " . ", 0, 3);
	      print(val, notdisplay, compact, ht, symtab, rnht, p);
	      print_this_string(p, ")", 0, 1);
	      did_one = 1;
	    }
	  }
	  print_this_string(p, ")", 0, 1);

	  closed = 1;
	}
      }
    }
  else if (SAME_OBJ(obj, scheme_true))
    {
      if (compact)
	print_compact(p, CPT_TRUE);
      else
	print_this_string(p, "#t", 0, 2);
    }
  else if (SAME_OBJ(obj, scheme_false))
    {
      if (compact)
	print_compact(p, CPT_FALSE);
      else
	print_this_string(p, "#f", 0, 2);
    }
  else if (compact && SAME_OBJ(obj, scheme_void))
    {
      print_compact(p, CPT_VOID);
    }
  else if (SCHEME_STRUCTP(obj))
    {
      if (compact)
	cannot_print(p, notdisplay, obj, ht);
      else {
	int pb;

	pb = p->quick_print_struct && PRINTABLE_STRUCT(obj, p);

	if (pb) {
	  obj = scheme_struct_to_vector(obj, NULL, p->quick_inspector);
	  closed = print(obj, notdisplay, compact, ht, symtab, rnht, p);
	} else {
	  Scheme_Object *src;

	  if (SCHEME_PROC_STRUCTP(obj)) {
	    /* Name by procedure? */
	    src = scheme_proc_struct_name_source(obj);
	  } else
	    src = obj;

	  if (SAME_OBJ(src, obj)) {
	    print_this_string(p, "#<struct:", 0, 9);
	    {
	      int l;
	      const char *s;
	      Scheme_Object *name = SCHEME_STRUCT_NAME_SYM(obj);
	      
	      s = scheme_symbol_name_and_size(name, (unsigned int *)&l, 
					      (p->quick_print_struct
					       ? SCHEME_SNF_FOR_TS
					       : (p->quick_can_read_pipe_quote 
						  ? SCHEME_SNF_PIPE_QUOTE
						  : SCHEME_SNF_NO_PIPE_QUOTE)));
	      print_this_string(p, s, 0, l);
	    }
	    PRINTADDRESS(p, obj);
	    print_this_string(p, ">", 0, 1);
	  } else {
	    closed = print(src, notdisplay, compact, ht, symtab, rnht, p);
	  }
	}
      }

      closed = 1;
    }
  else if (SCHEME_PRIMP(obj) && ((Scheme_Primitive_Proc *)obj)->name)
    {
      if (compact) {
	cannot_print(p, notdisplay, obj, ht);
      } else {
	print_this_string(p, "#<", 0, 2);
	print_string_in_angle(p, ((Scheme_Primitive_Proc *)obj)->name, "primitive:", -1);
	PRINTADDRESS(p, obj);
	print_this_string(p, ">", 0, 1);
      }
      closed = 1;
    }
  else if (SCHEME_CLSD_PRIMP(obj) && ((Scheme_Closed_Primitive_Proc *)obj)->name)
    {
      if (compact)
	cannot_print(p, notdisplay, obj, ht);
      else {
	if (SCHEME_STRUCT_PROCP(obj)) {
	  print_named(obj, "struct-procedure", 
		      ((Scheme_Closed_Primitive_Proc *)obj)->name, 
		      -1, p);
	} else {
	  print_this_string(p, "#<", 0, 2);
	  print_string_in_angle(p, ((Scheme_Closed_Primitive_Proc *)obj)->name, 
				SCHEME_GENERICP(obj) ? "" : "primitive:", -1);
	  PRINTADDRESS(p, obj);
	  print_this_string(p, ">", 0, 1);
	}
      }

      closed = 1;
    }
  else if (SCHEME_CLOSUREP(obj)) 
    {
      if (compact) {
	Scheme_Closed_Compiled_Procedure *closure = (Scheme_Closed_Compiled_Procedure *)obj;
	if (ZERO_SIZED(closure)) {
	  /* Print original code: */
	  compact = print(SCHEME_COMPILED_CLOS_CODE(closure), notdisplay, compact, ht, symtab, rnht, p);
	} else
	  cannot_print(p, notdisplay, obj, ht);
      } else {
	int len;
	const char *s;
	s = scheme_get_proc_name(obj, &len, 0);
	
	print_named(obj, "procedure", s, len, p);
      }
      closed = 1;
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_type_type))
    {
      if (compact) {
	cannot_print(p, notdisplay, obj, ht);
      } else {
	print_this_string(p, "#<", 0, 2);
	print_string_in_angle(p, scheme_symbol_val(((Scheme_Struct_Type *)obj)->name),
			      "struct-type:",
			      SCHEME_SYM_LEN(((Scheme_Struct_Type *)obj)->name));
	PRINTADDRESS(p, obj);
	print_this_string(p, ">", 0, 1);
      }
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_struct_property_type))
    {
      if (compact) {
	cannot_print(p, notdisplay, obj, ht);
      } else {
	print_this_string(p, "#<", 0, 2);
	print_string_in_angle(p, scheme_symbol_val(((Scheme_Struct_Property *)obj)->name),
			      "struct-type-property:", 
			      SCHEME_SYM_LEN(((Scheme_Struct_Property *)obj)->name));
	PRINTADDRESS(p, obj);
	print_this_string(p, ">", 0, 1);
      }
    }
  else if (SCHEME_THREADP(obj) && (((Scheme_Thread *)obj)->name))
    {
      Scheme_Thread *t = (Scheme_Thread *)obj;
      print_this_string(p, "#<thread:", 0, 9);
      print_this_string(p, scheme_symbol_val(t->name), 0, SCHEME_SYM_LEN(t->name));
      print_this_string(p, ">", 0, 1);
    }
  else if (SCHEME_INPORTP(obj))
    {
      if (compact) {
	cannot_print(p, notdisplay, obj, ht);
      } else {
	Scheme_Input_Port *ip;
	ip = (Scheme_Input_Port *)obj;
	print_this_string(p, "#", 0, 1);
	print_this_string(p, scheme_symbol_val(ip->sub_type), 0, SCHEME_SYM_LEN(ip->sub_type));
      }
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_regexp_type))
    {
       if (compact) {
	 print_escaped(p, notdisplay, obj, ht);
       } else {
	 Scheme_Object *src;
	 src = scheme_regexp_source(obj);
	 if (src) {
	   print_this_string(p, "#rx", 0, 3);
	   print_string(src, 1, p);
	 } else if (compact)
	   cannot_print(p, notdisplay, obj, ht);
	 else
	   print_this_string(p, "#<regexp>", 0, 9);
	 closed = 1;
       }
    }
  else if (SCHEME_OUTPORTP(obj))
    {
      if (compact) {
	cannot_print(p, notdisplay, obj, ht);
      } else {
	Scheme_Output_Port *op;
	op = (Scheme_Output_Port *)obj;
	print_this_string(p, "#", 0, 1);
	print_this_string(p, scheme_symbol_val(op->sub_type), 0, SCHEME_SYM_LEN(op->sub_type));
      }
    }
  else if (SCHEME_CPTRP(obj))
    {
      if (compact) {
	cannot_print(p, notdisplay, obj, ht);
      } else {
	print_this_string(p, "#<c-pointer:", 0, 12);
	print_this_string(p, SCHEME_CPTR_TYPE(obj), 0, -1);
	print_this_string(p, ">", 0, 1);
      }
    }
  else if (SCHEME_STXP(obj))
    {
      if (compact) {
	if (scheme_syntax_is_graph(obj))
	  print_compact(p, CPT_GSTX);
	else
	  print_compact(p, CPT_STX);
	
	/* "2" in scheme_syntax_to_datum() call preserves wraps. */
	closed = print(scheme_syntax_to_datum(obj, 2, rnht), 
		       notdisplay, 1, ht, symtab, rnht, p);
      } else {
	Scheme_Stx *stx = (Scheme_Stx *)obj;
	if ((stx->srcloc->line >= 0) || (stx->srcloc->pos >= 0)) {
	  print_this_string(p, "#<syntax:", 0, 9);
	  if (stx->srcloc->src && SCHEME_STRINGP(stx->srcloc->src)) {
	    print_this_string(p, SCHEME_STR_VAL(stx->srcloc->src), 0, SCHEME_STRLEN_VAL(stx->srcloc->src));
	    print_this_string(p, ":", 0, 1);
	  }
	  if (stx->srcloc->line >= 0)
	    sprintf(quick_buffer, "%ld:%ld", stx->srcloc->line, stx->srcloc->col-1);
	  else
	    sprintf(quick_buffer, ":%ld", stx->srcloc->pos);
	  print_this_string(p, quick_buffer, 0, -1);
	  print_this_string(p, ">", 0, 1);
	} else
	  print_this_string(p, "#<syntax>", 0, 9);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_module_index_type)) 
    {
      int l;
      Scheme_Object *idx;

      idx = scheme_hash_get(symtab, obj);
      if (idx) {
	print_compact(p, CPT_SYMREF);
	l = SCHEME_INT_VAL(idx);
	print_compact_number(p, l);
      } else {
	idx = scheme_make_integer(symtab->count);
	scheme_hash_set(symtab, obj, idx);	
	l = SCHEME_INT_VAL(idx);

	print_compact(p, CPT_MODULE_INDEX);
	print_compact_number(p, l);
	print(((Scheme_Modidx *)obj)->path, notdisplay, 1, ht, symtab, rnht, p);
	print(((Scheme_Modidx *)obj)->base, notdisplay, 1, ht, symtab, rnht, p);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_module_variable_type)) 
    {
      int l;
      Scheme_Object *idx;

      idx = scheme_hash_get(symtab, obj);
      if (idx) {
	print_compact(p, CPT_SYMREF);
	l = SCHEME_INT_VAL(idx);
	print_compact_number(p, l);
      } else {
	Module_Variable *mv;

	idx = scheme_make_integer(symtab->count);
	scheme_hash_set(symtab, obj, idx);
	l = SCHEME_INT_VAL(idx);

	print_compact(p, CPT_MODULE_VAR);
	print_compact_number(p, l);
	mv = (Module_Variable *)obj;
	print(mv->modidx, notdisplay, 1, ht, symtab, rnht, p);
	print(mv->sym, notdisplay, 1, ht, symtab, rnht, p);
	print_compact_number(p, mv->pos);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_variable_type)
	   && (((Scheme_Bucket_With_Flags *)obj)->flags & GLOB_HAS_REF_ID))
    {
      int pos;
      pos = ((Scheme_Bucket_With_Ref_Id *)obj)->id;
      print_compact(p, CPT_REFERENCE);
      print_compact_number(p, pos);
    }   
  else if (compact 
	   && (SAME_TYPE(SCHEME_TYPE(obj), scheme_local_type)
	       || SAME_TYPE(SCHEME_TYPE(obj), scheme_local_unbox_type)))
    {
      int unbox = SAME_TYPE(SCHEME_TYPE(obj), scheme_local_unbox_type);
      Scheme_Local *loc = (Scheme_Local *)obj;
      if (loc->position < CPT_RANGE(SMALL_LOCAL)) {
	unsigned char s[1];
	s[0] = loc->position + (unbox 
				? CPT_SMALL_LOCAL_UNBOX_START 
				: CPT_SMALL_LOCAL_START);
	print_this_string(p, (char *)s, 0, 1);
      } else {
	print_compact(p, unbox ? CPT_LOCAL_UNBOX : CPT_LOCAL);
	print_compact_number(p, loc->position);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_application_type))
    {
      Scheme_App_Rec *app;
      int i;

      app = (Scheme_App_Rec *)obj;

      if (app->num_args < CPT_RANGE(SMALL_APPLICATION)) {
	unsigned char s[1];
	s[0] = CPT_SMALL_APPLICATION_START + app->num_args;
	print_this_string(p, (char *)s, 0, 1);
      } else {
	print_compact(p, CPT_APPLICATION);
	print_compact_number(p, app->num_args);
      }

      for (i = 0; i < app->num_args + 1; i++) {
	closed = print(scheme_protect_quote(app->args[i]), notdisplay, 1, NULL, symtab, rnht, p);
      }
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_application2_type))
    {
      Scheme_App2_Rec *app;
      unsigned char s[1];

      app = (Scheme_App2_Rec *)obj;

      s[0] = CPT_SMALL_APPLICATION_START + 1;
      print_this_string(p, (char *)s, 0, 1);

      print(scheme_protect_quote(app->rator), notdisplay, 1, NULL, symtab, rnht, p);
      closed = print(scheme_protect_quote(app->rand), notdisplay, 1, NULL, symtab, rnht, p);
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_application3_type))
    {
      Scheme_App3_Rec *app;
      unsigned char s[1];

      app = (Scheme_App3_Rec *)obj;

      s[0] = CPT_SMALL_APPLICATION_START + 2;
      print_this_string(p, (char *)s, 0, 1);

      print(scheme_protect_quote(app->rator), notdisplay, 1, NULL, symtab, rnht, p);
      print(scheme_protect_quote(app->rand1), notdisplay, 1, NULL, symtab, rnht, p);
      closed = print(scheme_protect_quote(app->rand2), notdisplay, 1, NULL, symtab, rnht, p);
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_let_one_type))
    {
      Scheme_Let_One *lo;

      lo = (Scheme_Let_One *)obj;

      print_compact(p, CPT_LET_ONE);
      print(scheme_protect_quote(lo->value), notdisplay, 1, NULL, symtab, rnht, p);
      closed = print(scheme_protect_quote(lo->body), notdisplay, 1, NULL, symtab, rnht, p);
    }
  else if (compact && SAME_TYPE(SCHEME_TYPE(obj), scheme_branch_type))
    {
      Scheme_Branch_Rec *b;

      b = (Scheme_Branch_Rec *)obj;

      print_compact(p, CPT_BRANCH);
      print(scheme_protect_quote(b->test), notdisplay, 1, NULL, symtab, rnht, p);
      print(scheme_protect_quote(b->tbranch), notdisplay, 1, NULL, symtab, rnht, p);
      closed = print(scheme_protect_quote(b->fbranch), notdisplay, 1, NULL, symtab, rnht, p);
    }
  else if (SAME_TYPE(SCHEME_TYPE(obj), scheme_quote_compilation_type))
    {
      Scheme_Hash_Table *q_ht;
      Scheme_Object *v;
      int counter = 1, qpht, qpb;

      v = SCHEME_PTR_VAL(obj);

      /* A quoted expression may have graph structure. We assume that
	 this structure is local within the quoted expression. */

      qpht = p->quick_print_hash_table;
      qpb = p->quick_print_box;
      /* Boxes and hash tables can be literals, so we need to
	 enable printing as we write compiled code: */
      p->quick_print_hash_table = 1;
      p->quick_print_box = 1;
      
      q_ht = scheme_make_hash_table(SCHEME_hash_ptr);
      setup_graph_table(v, q_ht, &counter, p);

      if (compact)
	print_compact(p, CPT_QUOTE);
      else {
#if !NO_COMPACT
	/* Doesn't happen: */
	scheme_signal_error("internal error: non-compact quote compilation");
	return 0;
#endif
      }

      compact = print(v, notdisplay, 1, q_ht, symtab, rnht, p);

      p->quick_print_hash_table = qpht;
      p->quick_print_box = qpb;
    }
  else if (
#if !NO_COMPACT
	   compact && 
#endif
	   SAME_TYPE(SCHEME_TYPE(obj), scheme_svector_type))
    {
      mzshort l, *v;
      l = SCHEME_SVEC_LEN(obj);
      v = (mzshort *)SCHEME_SVEC_VEC(obj);
      
#if NO_COMPACT
      print_this_string(p, "[", 0, 1);
      {
	int i; 
	char s[10];

	for (i = 0; i < l; i++) {
	  if (i)
	    print_this_string(p, " ", 0, 1);
	  sprintf(s, "%d", (int)v[i]);
	  print_this_string(p, s, 0, -1);
	}
      }
      print_this_string(p, "]", 0, 1);
#else
      if (l < CPT_RANGE(SMALL_SVECTOR)) {
	unsigned char s[1];
	s[0] = l + CPT_SMALL_SVECTOR_START;
	print_this_string(p, (char *)s, 0, 1);
      } else {
	print_compact(p, CPT_SVECTOR);
	print_compact_number(p, l);
      }
      while (l--) {
	int n = v[l];
	print_compact_number(p, n);
      }
#endif
    }
  else if (scheme_type_writers[SCHEME_TYPE(obj)]
#if !NO_COMPACT
	   && (compact || SAME_TYPE(SCHEME_TYPE(obj), scheme_compilation_top_type))
#endif
	   )
    {
      Scheme_Type t = SCHEME_TYPE(obj);
      Scheme_Object *v;
      long slen;

      if (t >= _scheme_last_type_) {
	/* Doesn't happen: */
	scheme_signal_error("internal error: bad type with writer");
	return 0;
      }

      if (!global_constants_ht) {
	REGISTER_SO(global_constants_ht);
	global_constants_ht = scheme_map_constants_to_globals();
      }

      if (compact) {
	if (t < CPT_RANGE(SMALL_MARSHALLED)) {
	  unsigned char s[1];
	  s[0] = t + CPT_SMALL_MARSHALLED_START;
	  print_this_string(p, (char *)s, 0, 1);
	} else {
	  print_compact(p, CPT_MARSHALLED);
	  print_compact_number(p, t);
	}
      } else {
	print_this_string(p, "#~", 0, 2);
#if NO_COMPACT
	if (t < _scheme_last_type_) {
	  sprintf (quick_buffer, "%ld", (long)SCHEME_TYPE(obj));
	  print_this_string(p, quick_buffer, 0, -1);
	} else
	  print_this_string(p, scheme_get_type_name(t), 0, -1);
#endif
      }

      {
	Scheme_Type_Writer writer;
	writer = scheme_type_writers[t];
	v = writer(obj);
      }

      if (compact)
	closed = print(v, notdisplay, 1, NULL, symtab, rnht, p);
      else {
	/* Symtab services both symbols and module paths (modidxs) */
	symtab = scheme_make_hash_table(SCHEME_hash_ptr);
	rnht = scheme_make_hash_table(SCHEME_hash_ptr);

	/* "print" the string once to get a measurement and symtab size */
	print_substring(v, notdisplay, 1, NULL, symtab, rnht, p, NULL, &slen);

	/* Remember version: */
	print_compact_number(p, strlen(MZSCHEME_VERSION));
	print_this_string(p, MZSCHEME_VERSION, 0, -1);

	print_compact_number(p, symtab->count);
	print_compact_number(p, slen);

	/* Make symtab and rnht again to ensure the same results */
	symtab = scheme_make_hash_table(SCHEME_hash_ptr);
	rnht = scheme_make_hash_table(SCHEME_hash_ptr);

	closed = print(v, notdisplay, 1, NULL, symtab, rnht, p);
      }
    } 
  else 
    {
      if (compact)
	cannot_print(p, notdisplay, obj, ht);
      else {
	char *s;
	long len = -1;
	s = scheme_get_type_name((SCHEME_TYPE(obj)));
	print_this_string(p, "#", 0, 1);
#ifdef SGC_STD_DEBUGGING
	len = strlen(s) - 1;
#endif
	print_this_string(p, s, 0, len);
#ifdef SGC_STD_DEBUGGING
	PRINTADDRESS(p, obj);
	print_this_string(p, ">", 0, 1);
#endif
      }

      closed = 1;
    }

  return (closed || compact);
}

static void
print_string(Scheme_Object *string, int notdisplay, Scheme_Thread *p)
{
  char *str, minibuf[8], *esc;
  int len, a, i;

  len = SCHEME_STRTAG_VAL(string);
  str = SCHEME_STR_VAL(string);

  if (notdisplay) {
    print_this_string(p, "\"", 0, 1);

    for (a = i = 0; i < len; i++) {
      /* Escape-sequence handling by Eli Barzilay. */
      switch (((unsigned char *)str)[i]) {
      case '\"': esc = "\\\""; break;
      case '\\': esc = "\\\\"; break;
      case '\a': esc = "\\a";  break;
      case '\b': esc = "\\b";  break;
      case 27: esc = "\\e";  break;
      case '\f': esc = "\\f";  break;
      case '\n': esc = "\\n";  break;
      case '\r': esc = "\\r";  break;
      case '\t': esc = "\\t";  break;
      case '\v': esc = "\\v";  break;
      default:
	if ((scheme_locale_on || (((unsigned char *)str)[i] <= 127))
	    && isprint(((unsigned char *)str)[i])) {
	  esc = NULL;
	} else {
	  sprintf(minibuf,
                  ((i+1>=len) || (str[i+1] < '0') || (str[i+1] > '7')) ? "\\%o" : "\\%03o",
                  ((unsigned char *)str)[i]);
	  esc = minibuf;
	}
	break;
      }

      if (esc) {
        if (a < i)
	  print_this_string(p, str, a, i-a);
        print_this_string(p, esc, 0, -1);
        a = i+1;
      }
    }
    if (a < i)
      print_this_string(p, str, a, i-a);

    print_this_string(p, "\"", 0, 1);
  } else if (len) {
    print_this_string(p, str, 0, len);
  }
}

static void
print_pair(Scheme_Object *pair, int notdisplay, int compact, 
	   Scheme_Hash_Table *ht, 
	   Scheme_Hash_Table *symtab, Scheme_Hash_Table *rnht, 
	   Scheme_Thread *p)
{
  Scheme_Object *cdr;
  int super_compact = 0;

  if (compact) {
    int c = 0;
    Scheme_Object *pr;

    pr = pair;
    while (SCHEME_PAIRP(pr)) {
      if (ht)
	if ((long)scheme_hash_get(ht, pr) != 1) {
	  c = -1;
	  break;
	}
      c++;
      pr = SCHEME_CDR(pr);
    }

    if (c > -1) {
      super_compact = 1;
      if (c < CPT_RANGE(SMALL_LIST)) {
	unsigned char s[1];
	s[0] = c + (SCHEME_NULLP(pr) 
		    ? CPT_SMALL_PROPER_LIST_START
		    : CPT_SMALL_LIST_START);
	print_this_string(p, (char *)s, 0, 1);
      } else {
	print_compact(p, CPT_LIST);
	print_compact_number(p, c);
	super_compact = -1;
      }
    }
  }

  if (compact) {
    if (!super_compact)
      print_compact(p, CPT_PAIR);
  } else
    print_this_string(p, "(", 0, 1);

  print(SCHEME_CAR(pair), notdisplay, compact, ht, symtab, rnht, p);

  cdr = SCHEME_CDR (pair);
  while (SCHEME_PAIRP(cdr)) {
    if (ht && !super_compact) {
      if ((long)scheme_hash_get(ht, cdr) != 1) {
	/* This needs a tag */
	if (!compact)
	  print_this_string(p, " . ", 0, 3);
	(void)print(cdr, notdisplay, compact, ht, symtab, rnht, p);
	if (!compact)
	  print_this_string(p, ")", 0, 1);
	return;
      }
    }
    if (compact && !super_compact)
      print_compact(p, CPT_PAIR);
    if (!compact)
      print_this_string(p, " ", 0, 1);
    print(SCHEME_CAR(cdr), notdisplay, compact, ht, symtab, rnht, p);
    cdr = SCHEME_CDR(cdr);
  }

  if (!SCHEME_NULLP(cdr)) {
    if (!compact)
      print_this_string(p, " . ", 0, 3);
    print(cdr, notdisplay, compact, ht, symtab, rnht, p);
  } else if (compact && (super_compact < 1))
    print_compact(p, CPT_NULL);

  if (!compact)
    print_this_string(p, ")", 0, 1);
}

static void
print_vector(Scheme_Object *vec, int notdisplay, int compact, 
	     Scheme_Hash_Table *ht, 
	     Scheme_Hash_Table *symtab, Scheme_Hash_Table *rnht, 
	     Scheme_Thread *p)
{
  int i, size, common = 0;
  Scheme_Object **elems;

  size = SCHEME_VEC_SIZE(vec);

  if (compact) {
    print_compact(p, CPT_VECTOR);
    print_compact_number(p, size);
  } else {
    elems = SCHEME_VEC_ELS(vec);
    for (i = size; i--; common++) {
      if (!i || (elems[i] != elems[i - 1]))
	break;
    }
    elems = NULL; /* Precise GC: because VEC_ELS is not aligned */
    
    if (notdisplay && p->quick_print_vec_shorthand) {
      char buffer[100];
      sprintf(buffer, "#%d(", size);
      print_this_string(p, buffer, 0, -1);
      size -= common;
    } else
      print_this_string(p, "#(", 0, 2);
  }

  for (i = 0; i < size; i++) {
    print(SCHEME_VEC_ELS(vec)[i], notdisplay, compact, ht, symtab, rnht, p);
    if (i < (size - 1))
      if (!compact)
	print_this_string(p, " ", 0, 1);
  }

  if (!compact)
    print_this_string(p, ")", 0, 1);
}

static void
print_char(Scheme_Object *charobj, int notdisplay, Scheme_Thread *p)
{
  char ch, minibuf[8], *str;
  int len = -1;

  ch = SCHEME_CHAR_VAL(charobj);
  if (notdisplay) {
    switch ( ch )
      {
      case '\0':
	str = "#\\nul";
	break;
      case '\n':
	str = "#\\newline";
	break;
      case '\t':
	str = "#\\tab";
	break;
      case 0xb:
	str = "#\\vtab";
	break;
      case ' ':
	str = "#\\space";
	break;
      case '\r':
	str = "#\\return";
	break;
      case '\f':
	str = "#\\page";
	break;
      case '\b':
	str = "#\\backspace";
	break;
      case 0x7f:
	str = "#\\rubout";
	break;
      default:
	if ((scheme_locale_on || (((unsigned char )ch) <= 127))
	    && isprint(((unsigned char)ch))) {
	  minibuf[0] = '#';
	  minibuf[1] = '\\';
	  minibuf[2] = ch;
	  minibuf[3] = 0;
	} else {
	  sprintf(minibuf, "#\\%03o", (unsigned char)ch);
	}
	str = minibuf;
	break;
      }
  } else {
    minibuf[0] = ch;
    minibuf[1] = 0;
    str = minibuf;
    len = 1;
  }

  print_this_string(p, str, 0, len);
}

/***************************************************/

Scheme_Object *scheme_protect_quote(Scheme_Object *expr)
{
  if (HAS_SUBSTRUCT(expr, ssALLp)) {
    Scheme_Object *q;
    q = scheme_alloc_small_object();
    q->type = scheme_quote_compilation_type;
    SCHEME_PTR_VAL(q) = expr;
    return q;
  } else
    return expr;
}
