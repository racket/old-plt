/*
  MzScheme
  Copyright (c) 1995 Matthew Flatt

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
#include <string.h>
#include <memory.h>

Scheme_Type_Reader *scheme_type_readers;
Scheme_Type_Writer *scheme_type_writers;

static char **type_names;
static Scheme_Type maxtype, allocmax;

#ifdef MEMORY_COUNTING_ON
long scheme_type_table_count;
#endif

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void
scheme_init_type (Scheme_Env *env)
{
  long n;

  if (!scheme_starting_up)
    return;

  REGISTER_SO(type_names);
  REGISTER_SO(scheme_type_readers);
  REGISTER_SO(scheme_type_writers);
  
  maxtype = _scheme_last_type_;
  allocmax = maxtype + 10;

  type_names = MALLOC_N(char *, allocmax);
  scheme_type_readers = MALLOC_N_ATOMIC(Scheme_Type_Reader, allocmax);
  n = allocmax * sizeof(Scheme_Type_Reader);
  memset((char *)scheme_type_readers, 0, n);

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += n;
  scheme_misc_count += (allocmax * sizeof(char *));
#endif

  scheme_type_writers = MALLOC_N_ATOMIC(Scheme_Type_Writer, allocmax);
  n = allocmax * sizeof(Scheme_Type_Writer);
  memset((char *)scheme_type_writers, 0, n);

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += n;
#endif

#define set_name(t, n) type_names[t] = n

  set_name(scheme_true_type, "<true>");
  set_name(scheme_false_type, "<false>");
  set_name(scheme_char_type, "<char>");
  set_name(scheme_envunbox_type, "<env-auto-unbox>");
  set_name(scheme_local_type, "<local-code>");
  set_name(scheme_local_unbox_type, "<local-unbox-code>");
  set_name(scheme_variable_type, "<variable-code>");
  set_name(scheme_application_type, "<application-code>");
  set_name(scheme_compiled_unclosed_procedure_type, "<procedure-semi-code>");
  set_name(scheme_unclosed_procedure_type, "<procedure-code>");
  set_name(scheme_syntax_type, "<syntax-code>");
  set_name(scheme_compiled_syntax_type, "<syntax-semi-code>");
  set_name(scheme_branch_type, "<branch-code>");
  set_name(scheme_sequence_type, "<sequence-code>");
  set_name(scheme_case_lambda_sequence_type, "<case-lambda-code>");
  set_name(scheme_begin0_sequence_type, "<begin0-code>");
  set_name(scheme_with_cont_mark_type, "<with-continuation-mark-code>");

  set_name(scheme_let_value_type, "<let-value-code>");
  set_name(scheme_let_void_type, "<let-void-code>");
  set_name(scheme_compiled_let_value_type, "<let-value-semi-code>");
  set_name(scheme_compiled_let_void_type, "<let-void-semi-code>");
  set_name(scheme_letrec_type, "<letrec-code>");
  set_name(scheme_let_one_type, "<let-one-code>");
  set_name(scheme_quote_compilation_type, "<quote-code>");

  set_name(scheme_eval_waiting_type, "<eval-waiting>");
  set_name(scheme_void_type, "<void>");
  set_name(scheme_prim_type, "<primitive>");
  set_name(scheme_closed_prim_type, "<primitive-closure>");
  set_name(scheme_closure_type, "<closure-form>");
  set_name(scheme_linked_closure_type, "<procedure>");
  set_name(scheme_cont_type, "<continuation>");
  set_name(scheme_tail_call_waiting_type, "<tail-call-waiting>");
  set_name(scheme_null_type, "<empty-list>");
  set_name(scheme_pair_type, "<pair>");
  set_name(scheme_box_type, "<box>");
  set_name(scheme_integer_type, "<fixnum-integer>");
  set_name(scheme_double_type, "<inexact-number>");
  set_name(scheme_float_type, "<inexact-number*>");
  set_name(scheme_object_type, "<object>");
  set_name(scheme_class_type, "<class>");
  set_name(scheme_class_data_type, "<class-code>");
  set_name(scheme_generic_type, "<unknown-external>");
  set_name(scheme_undefined_type, "<undefined>");
  set_name(scheme_eof_type, "<eof>");
  set_name(scheme_input_port_type, "<input-port>");
  set_name(scheme_output_port_type, "<output-port>");
  set_name(scheme_process_type, "<thread>");
  set_name(scheme_promise_type, "<promise>");
  set_name(scheme_string_type, "<string>");
  set_name(scheme_struct_info_type, "<struct-info>");
  set_name(scheme_structure_type, "<struct>");
  set_name(scheme_symbol_type, "<symbol>");
  set_name(scheme_syntax_compiler_type, "<syntax>");
  set_name(scheme_macro_type, "<macro>");
  set_name(scheme_id_macro_type, "<id-macro>");
  set_name(scheme_vector_type, "<vector>");
  set_name(scheme_bignum_type, "<bignum-integer>");
  set_name(scheme_escaping_cont_type, "<escape-continuation>");
  set_name(scheme_sema_type, "<semaphore>");
  set_name(scheme_hash_table_type, "<hash-table>");
  set_name(scheme_case_closure_type, "<procedure>");
  set_name(scheme_generic_data_type, "<generic-data>");
  set_name(scheme_multiple_values_type, "<multiple-values>");
  set_name(scheme_placeholder_type, "<placeholder>");
  set_name(scheme_weak_box_type, "<weak-box>");
  set_name(scheme_rational_type, "<fractional-number>");
  set_name(scheme_complex_type, "<complex-number>");
  set_name(scheme_complex_izi_type, "<inexactly-real-number>");
  set_name(scheme_struct_type_type, "<struct-type>");
  set_name(scheme_exp_time_type, "<expansion-time-value>");
  set_name(scheme_listener_type, "<tcp-listener>");
  set_name(scheme_namespace_type, "<namespace>");
  set_name(scheme_config_type, "<parameterization>");
  set_name(scheme_defaulting_config_type, "<parameterization-defaulting-marker>");
  set_name(scheme_will_executor_type, "<will-executor>");
  set_name(scheme_interface_type, "<interface>");
  set_name(scheme_random_state_type, "<pseudo-random-generator>");
  set_name(scheme_regexp_type, "<regexp>");

  set_name(scheme_unit_type, "<unit>");
  set_name(scheme_compiled_unit_type, "<unit-code>");
  set_name(scheme_unit_body_data_type, "<unit-body-code>");
  set_name(scheme_unit_compound_data_type, "<compound-unit-code>");
  set_name(scheme_invoke_unit_data_type, "<invoke-unit-code>");

  set_name(scheme_interface_data_type, "<interface-code>");

  set_name(scheme_compilation_top_type, "<compiled-code>");
  set_name(scheme_svector_type, "<short-vector>");

  set_name(scheme_manager_type, "<custodian>");
  set_name(scheme_cont_mark_set_type, "<continuation-mark-set>");
  
  set_name(scheme_reserved_3_type, "<reserved3>");

  set_name(_scheme_values_types_, "<resurrected>");
  set_name(_scheme_compiled_values_types_, "<internal>");

#ifdef MZ_PRECISE_GC
  register_traversers();
#endif
}

Scheme_Type scheme_make_type(const char *name)
{
  if (maxtype == allocmax) {
    /* Expand arrays */
    void *naya;
    long n;
    
    allocmax += 20;

    naya = scheme_malloc(allocmax * sizeof(char *));
    memcpy(naya, type_names, maxtype * sizeof(char *));
    type_names = (char **)naya;

    naya = scheme_malloc_atomic(n = allocmax * sizeof(Scheme_Type_Reader));
    memset((char *)naya, 0, n);
    memcpy(naya, scheme_type_readers, maxtype * sizeof(Scheme_Type_Reader));
    scheme_type_readers = (Scheme_Type_Reader *)naya;

    naya = scheme_malloc_atomic(n = allocmax * sizeof(Scheme_Type_Writer));
    memset((char *)naya, 0, n);
    memcpy(naya, scheme_type_writers, maxtype * sizeof(Scheme_Type_Writer));
    scheme_type_writers = (Scheme_Type_Writer *)naya;

#ifdef MEMORY_COUNTING_ON
  scheme_type_table_count += 20 * (sizeof(Scheme_Type_Reader)
				   + sizeof(Scheme_Type_Writer));
  scheme_misc_count += (20 * sizeof(char *));
#endif
  }
  
  type_names[maxtype] = scheme_strdup(name);

  return maxtype++;
}

char *scheme_get_type_name(Scheme_Type t)
{
  if (t < 0 || t >= maxtype)
    return "<bad-value>";
  return type_names[t];
}

int scheme_find_type(Scheme_Object *ts)
{
  int i;
  char *str;

  str = SCHEME_TSYM_VAL(ts);

  for (i = maxtype; i--; ) {
    if (type_names[i]) {
      char *ts = type_names[i] + 1, *s = str;
      while (*s && *ts && (*s == *ts)) {
	s++;
	ts++;
      }
      if (!*s && *ts == '>')
	return i;
    }
  }

  scheme_signal_error("bad type name: %s", str);
  return -1;
}

void scheme_install_type_reader(Scheme_Type t, Scheme_Type_Reader f)
{
  if (t < 0 || t >= maxtype)
    return;

  scheme_type_readers[t] = f;
}

void scheme_install_type_writer(Scheme_Type t, Scheme_Type_Writer f)
{
  if (t < 0 || t >= maxtype)
    return;

  scheme_type_writers[t] = f;
}

int scheme_num_types(void)
{
  return maxtype;
}

/***********************************************************************/

#ifdef MZ_PRECISE_GC

static int bad_trav(void *p, Mark_Proc mark)
{
  printf("Shouldn't get here.\n");
  exit(-1);
}

static int variable_obj(void *p, Mark_Proc mark)
{
  /* FIXME */
  return sizeof(Scheme_Bucket);
}

static int local_obj(void *p, Mark_Proc mark)
{
  return sizeof(Scheme_Local);
}

static int second_of_cons(void *p, Mark_Proc mark)
{
  if (mark)
    PTR2_VAL((Scheme_Object *)p) = mark(PTR2_VAL((Scheme_Object *)p));
  return sizeof(Scheme_Object);
}

static int second_of_cons(void *p, Mark_Proc mark)
{
  if (mark)
    ((Scheme_Small_Object *)p)->u.ptr_value = mark(((Scheme_Small_Object *)p)->u.ptr_value);

  return sizeof(Scheme_Small_Object);
}

static int app_rec(void *p, Mark_Proc mark)
{
  Scheme_App_Rec *r = (Scheme_App_Rec *)p;

  if (mark) {
    int i = r->num_args;
    while (i--)
      r->args[i] = mark(r->args[i]);
  }

  return (sizeof(Scheme_App_Rec) 
	  + ((r->num_args - 1) * sizeof(Scheme_Object *))
	  + (r->num_args * sizeof(char)));
}

static int seq_rec(void *p, Mark_Proc mark)
{
  Scheme_Sequence *s = (Scheme_Sequence *)p;

  if (mark) {
    int i = s->count;
    while (i--)
      s->array[i] = mark(s->array[i]);
  }

  return (sizeof(Scheme_Sequence)
	  + ((s->count - 1) * sizeof(Scheme_Object *)));
}

static int branch_rec(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)p;
    
    b->test = mark(b->test);
    b->tbranch = mark(b->tbranch);
    b->fbranch = mark(b->fbranch);
  }

  return sizeof(Scheme_Branch_Rec);
}

static int unclosed_proc(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Closure_Compilation_Data *d = (Scheme_Closure_Compilation_Data *)p;

    if (d->name)
      d->name = mark(d->name);
    data->code = mark(data->code);
    data->closure_map = mark(data->closure_map);
  }

  return sizeof(Scheme_Closure_Compilation_Data);
}

static int let_value(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Let_Value *l = (Scheme_Let_Value *)p;

    l->value = mark(l->value);
    l->body = mark(l->body);
  }

  return sizeof(Scheme_Let_Value);
}

static int let_void(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Let_Void *l = (Scheme_Let_Void *)p;

    l->body = mark(l->body);
  }

  return sizeof(Scheme_Let_Void);
}

static int letrec(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Letrec *l = (Scheme_Letrec *)p;
    int i = l->count;

    while (i--)
      l->procs[i] = mark(l->procs[i]);
    l->body = mark(l->body);
  }

  return sizeof(Scheme_Letrec);
}

static int let_one(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Let_One *l = (Scheme_Let_One *)p;

    l->value = mark(l->value);
    l->body = mark(l->body);
  }

  return sizeof(Scheme_Let_One);
}

static int with_cont_mark(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_With_Continuation_Mark *w = (Scheme_With_Continuation_Mark *)p;

    w->key = mark(w->key);
    w->val = mark(w->val);
    w->body = mark(w->body);
  }

  return sizeof(Scheme_Let_One);
}

static int comp_unclosed_proc(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Closure_Compilation_Data *c = (Scheme_Closure_Compilation_Data *)p;

    c->closure_map = mark(c->closure_map);
    c->code = mark(c->code);
    c->name = mark(c->name);
  }

  return sizeof(Scheme_Closure_Compilation_Data);
}

static int comp_let_value(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Compiled_Let_Value *c = (Scheme_Compiled_Let_Value *)p;

    c->flags = mark(c->flags);
    c->value = mark(c->value);
    c->body = mark(c->body);
  }

  return sizeof(Scheme_Compiled_Let_Value);
}

static int let_header(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Let_Header *h = (Scheme_Let_Header *)p;

    h->body = mark(h->body);
  }

  return sizeof(Scheme_Let_Header);
}

static int prim_proc(void *p, Mark_Proc mark)
{
  return sizeof(Scheme_Primitive_Proc);
}

static int closed_prim_proc(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Closed_Primitive_Proc *c = (Scheme_Closed_Primitive_Proc *)p;

    SCHEME_CLSD_PRIM_DATA(prim) = mark(SCHEME_CLSD_PRIM_DATA(prim));
  }

  return sizeof(Scheme_Closed_Primitive_Proc);
}

static int linked_closure(void *p, Mark_Proc mark)
{
  Scheme_Closed_Compiled_Procedure *c = (Scheme_Closed_Compiled_Procedure *)p;

  if (mark) {
    int i = c->closure_size;
    while (i--)
      c->vals[i] = mark(c->vals[i]);
    c->code = mark(c->code);
  }
  
  return (sizeof(Scheme_Closed_Compiled_Procedure)
	  + (c->closure_size - 1) * sizeof(Scheme_Object *));
}

static int case_closure(void *p, Mark_Proc mark)
{
  Scheme_Case_Lambda *c = (Scheme_Case_Lambda *)p;

  if (mark) {
    int i;

    for (i = c->count; i--; )
      c->array[i] = mark(c->array[i]);
  }

  return (sizeof(Scheme_Case_Lambda)
	  + ((c->count - 1) * sizeof(Scheme_Object *)));
}

static void mark_cjs(Scheme_Continuation_Jump_State *cjs, Mark_Proc mark)
{
}

static int cont_proc(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Cont *c = (Scheme_Cont *)p;

    c->dw = mark(c->dw);
    c->ok = mark(c->ok);
    c->home = mark(c->home);
    c->current_local_env = mark(c->current_local_env);
    c->save_overflow = mark(c->save_overflow);

    mark_cjs(&c->cjs, mark);
  }

  return sizeof(Scheme_Cont);
}

static int escaping_cont_proc(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Escaping_Cont *c = (Scheme_Escaping_Cont *)p;

    c->home = mark(c->home);
    c->ok = mark(c->ok);
    c->f = mark(c->f);

    mark_cjs(&c->cjs, mark);
  }

  return sizeof(Scheme_Escaping_Cont);
}

static int char_obj(void *p, Mark_Proc mark)
{
  return sizeof(Scheme_Small_Object);
}

static int bignum_obj(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Bignum *b = (Scheme_Bignum *)p;

    b->digits = mark(b->digits);
  }

  return sizeof(Scheme_Bignum);
}

static int rational_obj(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Rational *r = (Scheme_Rational *)p;

    r->num = mark(r->num);
    r->denom = mark(r->denom);
  }

  return sizeof(Scheme_Rational);
}

static int float_obj(void *p, Mark_Proc mark)
{
#ifdef MZ_USE_SINGLE_FLOATS
  return sizeof(Scheme_Float);
#else
  return bad_trav(p, mark);
#endif
}

static int double_obj(void *p, Mark_Proc mark)
{
  return sizeof(Scheme_Double);
}

static int complex_obj(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Complex *c = (Scheme_Complex *)p;

    c->r = mark(c->r);
    c->i = mark(c->i);
  }

  return sizeof(Scheme_Complex);
}

static int string_obj(void *p, Mark_Proc mark)
{
  if (mark) {
    SCHEME_STR_VAL(p) = mark(SCHEME_STR_VAL(p));
  }

  return sizeof(Scheme_Object);
}

static int symbol_obj(void *p, Mark_Proc mark)
{
  Scheme_Symbol *s = (Scheme_Symbol *)p;

  return sizeof(Scheme_Symbol) + s->len;
}

static int cons_cell(void *p, Mark_Proc mark)
{
  if (mark) {
    Scheme_Object *o = (Scheme_Object *)p;

    SCHEME_CAR(o) = mark(SCHEME_CAR(o));
    SCHEME_CDR(o) = mark(SCHEME_CDR(o));
  }

  return sizeof(Scheme_Object);
}

static int vector_obj(void *p, Mark_Proc mark)
{
  Scheme_Vector *vec = (Scheme_Vector *)p;

  if (mark) {
    for (i = vec->size; i--; )
      vec->els[i] = mark(vec->els[i]);
  }

  return (sizeof(Scheme_Vector) 
	  + ((vec->size - 1) * sizeof(Scheme_Object *)));
}

static void register_traversers(void)
{
  GC_register_traverser(scheme_variable_type, variable_obj);
  GC_register_traverser(scheme_local_type, local_obj);
  GC_register_traverser(scheme_local_unbox_type, local_obj);
  GC_register_traverser(scheme_syntax_type, second_of_cons);
  GC_register_traverser(scheme_application_type, app_rec);
  GC_register_traverser(scheme_sequence_type, seq_rec);
  GC_register_traverser(scheme_branch_type, branch_rec);
  GC_register_traverser(scheme_unclosed_procedure_type, unclosed_proc);
  GC_register_traverser(scheme_let_value_type, let_value);
  GC_register_traverser(scheme_let_void_type, let_void);
  GC_register_traverser(scheme_letrec_type, letrec);
  GC_register_traverser(scheme_let_one_type, let_one);
  GC_register_traverser(scheme_with_cont_mark_type, with_cont_mark);

  GC_register_traverser(_scheme_values_types_, bad_trav);
  
  GC_register_traverser(scheme_compiled_unclosed_procedure_type, comp_unclosed_proc);
  GC_register_traverser(scheme_compiled_let_value_type, comp_let_value);
  GC_register_traverser(scheme_compiled_let_void_type, let_header);
  GC_register_traverser(scheme_compiled_syntax_type, second_of_cons);

  GC_register_traverser(scheme_quote_compilation_type, small_object);

  GC_register_traverser(_scheme_compiled_values_types_, bad_trav);

  GC_register_traverser(scheme_prim_type, prim_proc);
  GC_register_traverser(scheme_closed_prim_type, closed_prim_proc);
  GC_register_traverser(scheme_linked_closure_type, linked_closure);
  GC_register_traverser(scheme_case_closure_type, case_closure);
  GC_register_traverser(scheme_cont_type, cont_proc);
  GC_register_traverser(scheme_escaping_cont_type, escaping_cont_proc);

  GC_register_traverser(scheme_char_type, char_obj);
  GC_register_traverser(scheme_integer_type, bad_trav);
  GC_register_traverser(scheme_bignum_type, bignum_obj);
  GC_register_traverser(scheme_rational_type, rational_obj);
  GC_register_traverser(scheme_float_type,  float_trav);
  GC_register_traverser(scheme_double_type, double_trav);
  GC_register_traverser(scheme_complex_izi_type, complex_obj);
  GC_register_traverser(scheme_complex_type, complex_obj);
  GC_register_traverser(scheme_string_type, string_obj);
  GC_register_traverser(scheme_symbol_type, symbol_obj);
  GC_register_traverser(scheme_null_type, char_obj); /* small */
  GC_register_traverser(scheme_pair_type, cons_cell);
  GC_register_traverser(scheme_vector_type, vector_obj);
  GC_register_traverser(scheme_closure_type, bad_trav); /* not used anymore */

  GC_register_traverser(scheme_input_port_type, input_port);
  GC_register_traverser(scheme_output_port_type, output_port);
  GC_register_traverser(scheme_eof_type, char_obj); /* small */
  GC_register_traverser(scheme_true_type, char_obj); /* small */
  GC_register_traverser(scheme_false_type, char_obj); /* small */
  GC_register_traverser(scheme_void_type, char_obj);  /* small */
  GC_register_traverser(scheme_syntax_compiler_type, syntax_compiler);
  GC_register_traverser(scheme_macro_type, macro);
  GC_register_traverser(scheme_promise_type, promise);
  GC_register_traverser(scheme_box_type, box);
  GC_register_traverser(scheme_process_type, process);
  GC_register_traverser(scheme_object_type, object_val);
  GC_register_traverser(scheme_class_type, class_val);
  GC_register_traverser(scheme_structure_type, struct_val);
  GC_register_traverser(scheme_generic_type, generic_val);
  GC_register_traverser(scheme_cont_mark_set_type, cont_mark_set);
  GC_register_traverser(scheme_sema_type, sema_val);
  GC_register_traverser(scheme_hash_table_type, hash_table_val);
  GC_register_traverser(scheme_generic_data_type, generic_data_val);
  GC_register_traverser(scheme_weak_box_type, box);
  GC_register_traverser(scheme_struct_type_type, struct_type_val);
  GC_register_traverser(scheme_id_macro_type, id_macro);
  GC_register_traverser(scheme_unit_type, unit_val);
  GC_register_traverser(scheme_exp_time_type, exp_time_val);
  GC_register_traverser(scheme_listener_type, listener_val);
  GC_register_traverser(scheme_namespace_type, namespace_val);
  GC_register_traverser(scheme_config_type, config_val);
  GC_register_traverser(scheme_defaulting_config_type, defaulting_config_val);
  GC_register_traverser(scheme_will_executor_type, will_executor);
  GC_register_traverser(scheme_interface_type, interface_val);
  GC_register_traverser(scheme_manager_type, manager_val);
  GC_register_traverser(scheme_random_state_type, random_state);
  GC_register_traverser(scheme_regexp_type, regexp);
  
  GC_register_traverser(scheme_compilation_top_type, compilation_top);

  GC_register_traverser(scheme_envunbox_type, bad_trav);
  GC_register_traverser(scheme_eval_waiting_type, bad_trav);
  GC_register_traverser(scheme_tail_call_waiting_type, bad_trav);
  GC_register_traverser(scheme_class_data_type, class_data_val);
  GC_register_traverser(scheme_undefined_type, char_val); /* small */
  GC_register_traverser(scheme_struct_info_type, struct_info_val);
  GC_register_traverser(scheme_multiple_values_type, bad_trav);
  GC_register_traverser(scheme_placeholder_type, char_obj); /* small */
  GC_register_traverser(scheme_case_lambda_sequence_type, case_seq);
  GC_register_traverser(scheme_begin0_sequence_type, begin0_seq);

  GC_register_traverser(scheme_compiled_unit_type, comp_unit);
  GC_register_traverser(scheme_unit_body_data_type, unit_body);
  GC_register_traverser(scheme_unit_body_closure_data_type, unit_body_closure);
  GC_register_traverser(scheme_unit_compound_data_type, compound_unit_data);
  GC_register_traverser(scheme_invoke_unit_data_type, invoke_unit_data);

  GC_register_traverser(scheme_interface_data_type, interface_data);

  GC_register_traverser(scheme_svector_type, svector_val);
}

#endif
