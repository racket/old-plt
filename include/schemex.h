/*
  MzScheme
  Copyright (c) 2004 PLT Scheme, Inc.
  Copyright (c) 1995-2001 Matthew Flatt
  All rights reserved.

  Please see the full copyright in the documentation.

  Originally based on:
  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* MzScheme function prototypes */
/* Macros generally shouldn't go in this file; it is used both to
   prototype functions, and as a parsing source for
   declaring scheme_extension_table */

/* The scheme_extension_table "parser" is picky; don't leave a space
   between a function name and it's opening parameter parenthesis. */

/* After this START tag, all comments should start & end on same line */

typedef struct {
/*========================================================================*/
/*                       setjmpup (continuations)                         */
/*========================================================================*/
void (*scheme_init_jmpup_buf)(Scheme_Jumpup_Buf *b);
int (*scheme_setjmpup_relative)(Scheme_Jumpup_Buf *b, void *base,
				       void * volatile start, Scheme_Jumpup_Buf *cont);
void (*scheme_longjmpup)(Scheme_Jumpup_Buf *b);
void (*scheme_reset_jmpup_buf)(Scheme_Jumpup_Buf *b);
#ifdef USE_MZ_SETJMP
int (*scheme_mz_setjmp)(mz_pre_jmp_buf b);
void (*scheme_mz_longjmp)(mz_pre_jmp_buf b, int v);
#endif
void (*scheme_clear_escape)(void);
Scheme_Jumpup_Buf_Holder *(*scheme_new_jmpupbuf_holder)(void);
/*========================================================================*/
/*                                parameters                              */
/*========================================================================*/
Scheme_Object *(*scheme_make_config)(Scheme_Config *base);
Scheme_Object *(*scheme_branch_config)(void);
int (*scheme_new_param)(void);
Scheme_Object *(*scheme_param_config)(char *name, Scheme_Object *pos,
					     int argc, Scheme_Object **argv,
					     int arity,
					     Scheme_Prim *check, char *expected,
					     int isbool);
Scheme_Object *(*scheme_register_parameter)(Scheme_Prim *function, char *name, int which);
Scheme_Env *(*scheme_get_env)(Scheme_Config *config);
/*========================================================================*/
/*                                threads                                 */
/*========================================================================*/
#ifndef LINK_EXTENSIONS_BY_TABLE
Scheme_Thread *scheme_current_thread;
volatile int scheme_fuel_counter;
#else
Scheme_Thread **scheme_current_thread_ptr;
volatile int *scheme_fuel_counter_ptr;
#endif
void (*scheme_out_of_fuel)(void);
Scheme_Object *(*scheme_thread)(Scheme_Object *thunk, Scheme_Config *config);
Scheme_Object *(*scheme_thread_w_custodian)(Scheme_Object *thunk, Scheme_Config *config,
						   Scheme_Custodian *mgr);
Scheme_Object *(*scheme_thread_w_custodian_killkind)(Scheme_Object *thunk, Scheme_Config *config,
							    Scheme_Custodian *mgr, int normal_kill);
void (*scheme_kill_thread)(Scheme_Thread *p);
void (*scheme_break_thread)(Scheme_Thread *p);
void (*scheme_thread_block)(float sleep_time);
void (*scheme_swap_thread)(Scheme_Thread *process);
void (*scheme_making_progress)();
void (*scheme_weak_suspend_thread)(Scheme_Thread *p);
void (*scheme_weak_resume_thread)(Scheme_Thread *p);
int (*scheme_block_until)(Scheme_Ready_Fun f, Scheme_Needs_Wakeup_Fun, Scheme_Object *, float);
int (*scheme_in_main_thread)(void);
void (*scheme_cancel_sleep)(void);
int (*scheme_tls_allocate)();
void (*scheme_tls_set)(int pos, void *v);
void *(*scheme_tls_get)(int pos);
Scheme_Custodian *(*scheme_make_custodian)(Scheme_Custodian *);
Scheme_Custodian_Reference *(*scheme_add_managed)(Scheme_Custodian *m, Scheme_Object *o,
							 Scheme_Close_Custodian_Client *f, void *data,
							 int strong);
void (*scheme_custodian_check_available)(Scheme_Custodian *m, const char *who, const char *what);
void (*scheme_remove_managed)(Scheme_Custodian_Reference *m, Scheme_Object *o);
void (*scheme_close_managed)(Scheme_Custodian *m);
void (*scheme_schedule_custodian_close)(Scheme_Custodian *c);
void (*scheme_add_custodian_extractor)(Scheme_Type t, Scheme_Custodian_Extractor e);
void (*scheme_add_atexit_closer)(Scheme_Exit_Closer_Func f);
void (*scheme_add_waitable)(Scheme_Type type,
				   Scheme_Ready_Fun ready,
				   Scheme_Needs_Wakeup_Fun wakeup,
				   Scheme_Wait_Filter_Fun filter,
				   int can_redirect);
void (*scheme_add_waitable_through_sema)(Scheme_Type type,
						Scheme_Wait_Sema_Fun sema,
						Scheme_Wait_Filter_Fun filter);
int (*scheme_is_waitable)(Scheme_Object *o);
Scheme_Object *(*scheme_object_wait_multiple)(int argc, Scheme_Object *argv[]);
Scheme_Object *(*scheme_object_wait_multiple_enable_break)(int argc, Scheme_Object *argv[]);
Scheme_Object *(*scheme_make_waitable_set)(int argc, Scheme_Object **argv);
void (*scheme_add_swap_callback)(Scheme_Closure_Func f, Scheme_Object *data);
Scheme_Object *(*scheme_call_enable_break)(Scheme_Prim *prim, int argc, Scheme_Object *argv[]);
int (*scheme_close_should_force_port_closed)();
void (*scheme_push_kill_action)(Scheme_Kill_Action_Func f, void *d);
void (*scheme_pop_kill_action)();
/*========================================================================*/
/*                              error handling                            */
/*========================================================================*/
void (*scheme_signal_error)(const char *msg, ...);
void (*scheme_raise_exn)(int exnid, ...);
void (*scheme_warning)(char *msg, ...);
void (*scheme_raise)(Scheme_Object *exn);
void (*scheme_wrong_count)(const char *name, int minc, int maxc,
				  int argc, Scheme_Object **argv);
void (*scheme_wrong_count_m)(const char *name, int minc, int maxc,
				    int argc, Scheme_Object **argv,
				    int is_method);
void (*scheme_case_lambda_wrong_count)(const char *name, int argc,
					      Scheme_Object **argv, int is_method, int count, ...);
void (*scheme_wrong_type)(const char *name, const char *expected,
				 int which, int argc,
				 Scheme_Object **argv);
void (*scheme_arg_mismatch)(const char *name, const char *msg, Scheme_Object *o);
void (*scheme_wrong_return_arity)(const char *where,
					 int expected, int got,
					 Scheme_Object **argv,
					 const char *context_detail, ...);
void (*scheme_unbound_global)(Scheme_Bucket *b);
Scheme_Object *(*scheme_dynamic_wind)(void (*pre)(void *),
					     Scheme_Object *(* volatile act)(void *),
					     void (* volatile post)(void *),
					     Scheme_Object *(*jmp_handler)(void *),
					     void * volatile data);
/*========================================================================*/
/*                                 types                                  */
/*========================================================================*/
Scheme_Type (*scheme_make_type)(const char *name);
char *(*scheme_get_type_name)(Scheme_Type type);
/*========================================================================*/
/*                              constants                                 */
/*========================================================================*/
Scheme_Object *scheme_eof;
Scheme_Object *(*scheme_make_eof)(void);
Scheme_Object *scheme_null;
Scheme_Object *(*scheme_make_null)(void);
Scheme_Object *scheme_true;
Scheme_Object *(*scheme_make_true)(void);
Scheme_Object *scheme_false;
Scheme_Object *(*scheme_make_false)(void);
Scheme_Object *scheme_void;
Scheme_Object *(*scheme_make_void)(void);
Scheme_Object *scheme_undefined;
Scheme_Object *scheme_tail_call_waiting;
Scheme_Object *scheme_multiple_values;
/*========================================================================*/
/*                              evaluation                                */
/*========================================================================*/
Scheme_Object *(*scheme_eval)(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *(*scheme_eval_multi)(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *(*scheme_eval_compiled)(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *(*scheme_eval_compiled_multi)(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *(*_scheme_eval_compiled)(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *(*_scheme_eval_compiled_multi)(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *(*scheme_apply)(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
Scheme_Object *(*scheme_apply_multi)(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
Scheme_Object *(*scheme_apply_eb)(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
Scheme_Object *(*scheme_apply_multi_eb)(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
Scheme_Object *(*scheme_apply_to_list)(Scheme_Object *rator, Scheme_Object *argss);
Scheme_Object *(*scheme_eval_string)(const char *str, Scheme_Env *env);
Scheme_Object *(*scheme_eval_string_multi)(const char *str, Scheme_Env *env);
Scheme_Object *(*scheme_eval_string_all)(const char *str, Scheme_Env *env, int all);
Scheme_Object *(*_scheme_apply_known_closed_prim)(Scheme_Object *rator, int argc,
					       Scheme_Object **argv);
Scheme_Object *(*_scheme_apply_known_closed_prim_multi)(Scheme_Object *rator, int argc,
						     Scheme_Object **argv);
Scheme_Object *(*_scheme_apply_closed_prim)(Scheme_Object *rator, int argc,
					 Scheme_Object **argv);
Scheme_Object *(*_scheme_apply_closed_prim_multi)(Scheme_Object *rator, int argc,
					       Scheme_Object **argv);
Scheme_Object *(*scheme_values)(int c, Scheme_Object **v);
Scheme_Object *(*scheme_check_one_value)(Scheme_Object *v);
/* Tail calls - only use these when you're writing new functions/syntax */
Scheme_Object *(*scheme_tail_apply)(Scheme_Object *f, int n, Scheme_Object **arg);
Scheme_Object *(*scheme_tail_apply_no_copy)(Scheme_Object *f, int n, Scheme_Object **arg);
Scheme_Object *(*scheme_tail_apply_to_list)(Scheme_Object *f, Scheme_Object *l);
Scheme_Object *(*scheme_tail_eval_expr)(Scheme_Object *obj);
void (*scheme_set_tail_buffer_size)(int s);
Scheme_Object *(*scheme_force_value)(Scheme_Object *);
void (*scheme_set_cont_mark)(Scheme_Object *key, Scheme_Object *val);
void (*scheme_push_continuation_frame)(Scheme_Cont_Frame_Data *);
void (*scheme_pop_continuation_frame)(Scheme_Cont_Frame_Data *);
void (*scheme_temp_dec_mark_depth)();
void (*scheme_temp_inc_mark_depth)();
Scheme_Object *(*scheme_current_continuation_marks)(void);
/* Internal */
Scheme_Object *(*scheme_do_eval)(Scheme_Object *obj, int _num_rands, Scheme_Object **rands, int val);
Scheme_Object *(*scheme_eval_compiled_stx_string)(Scheme_Object *expr, Scheme_Env *env,
							 long shift, Scheme_Object *modidx);
Scheme_Object *(*scheme_load_compiled_stx_string)(const char *str, long len);
Scheme_Object *(*scheme_compiled_stx_symbol)(Scheme_Object *stx);
Scheme_Object *(*scheme_eval_compiled_sized_string)(const char *str, int len, Scheme_Env *env);
/*========================================================================*/
/*                           memory management                            */
/*========================================================================*/
/* The core allocator functions depend on the GC. Macros in scheme.h */
/*  map to the apporpriate core allocation function. */
#ifndef SCHEME_NO_GC
# ifndef SCHEME_NO_GC_PROTO
void *(*GC_malloc)(size_t size_in_bytes);
void *(*GC_malloc_atomic)(size_t size_in_bytes);
#  ifdef MZ_PRECISE_GC
void *(*GC_malloc_one_tagged)(size_t size_in_bytes);
void *(*GC_malloc_atomic_uncollectable)(size_t size_in_bytes);
void *(*GC_malloc_array_tagged)(size_t size_in_bytes);
#  else
void *(*GC_malloc_stubborn)(size_t size_in_bytes);
void *(*GC_malloc_uncollectable)(size_t size_in_bytes);
#  endif
# endif
#endif
void *(*scheme_malloc_eternal)(size_t n);
void (*scheme_end_stubborn_change)(void *p);
void *(*scheme_calloc)(size_t num, size_t size);
char *(*scheme_strdup)(const char *str);
char *(*scheme_strdup_eternal)(const char *str);
void *(*scheme_malloc_fail_ok)(void *(*f)(size_t), size_t);
#ifndef MZ_PRECISE_GC
void (*scheme_weak_reference)(void **p);
void (*scheme_weak_reference_indirect)(void **p, void *v);
void (*scheme_unweak_reference)(void **p);
#endif
void (*scheme_add_finalizer)(void *p, void (*f)(void *p, void *data), void *data);
void (*scheme_add_finalizer_once)(void *p, void (*f)(void *p, void *data), void *data);
void (*scheme_add_scheme_finalizer)(void *p, void (*f)(void *p, void *data), void *data);
void (*scheme_add_scheme_finalizer_once)(void *p, void (*f)(void *p, void *data), void *data);
void (*scheme_register_finalizer)(void *p,
					 void (*f)(void *p, void *data), void *data,
					 void (**oldf)(void *p, void *data),
					 void **olddata);
void (*scheme_remove_all_finalization)(void *p);
void (*scheme_dont_gc_ptr)(void *p);
void (*scheme_gc_ptr_ok)(void *p);
void (*scheme_collect_garbage)(void);
#ifdef MZ_PRECISE_GC
void **GC_variable_stack;
void (*GC_register_traversers)(short tag, Size_Proc size, Mark_Proc mark, Fixup_Proc fixup,
				      int is_constant_size, int is_atomic);
void *(*GC_resolve)(void *p);
void (*GC_mark)(const void *p);
void (*GC_fixup)(void *p);
#endif
/*========================================================================*/
/*                             hash tables                                */
/*========================================================================*/
Scheme_Bucket_Table *(*scheme_make_bucket_table)(int size_hint, int type);
void (*scheme_add_to_table)(Scheme_Bucket_Table *table, const char *key, void *val, int);
void (*scheme_change_in_table)(Scheme_Bucket_Table *table, const char *key, void *new_val);
void *(*scheme_lookup_in_table)(Scheme_Bucket_Table *table, const char *key);
Scheme_Bucket *(*scheme_bucket_from_table)(Scheme_Bucket_Table *table, const char *key);
int (*scheme_bucket_table_equal)(Scheme_Bucket_Table *t1, Scheme_Bucket_Table *t2);
Scheme_Hash_Table *(*scheme_make_hash_table)(int type);
Scheme_Hash_Table *(*scheme_make_hash_table_equal)();
void (*scheme_hash_set)(Scheme_Hash_Table *table, Scheme_Object *key, Scheme_Object *val);
Scheme_Object *(*scheme_hash_get)(Scheme_Hash_Table *table, Scheme_Object *key);
int (*scheme_hash_table_equal)(Scheme_Hash_Table *t1, Scheme_Hash_Table *t2);
int (*scheme_is_hash_table_equal)(Scheme_Object *o);
/*========================================================================*/
/*                   basic Scheme value constructors                      */
/*========================================================================*/
Scheme_Object *(*scheme_make_prim)(Scheme_Prim *prim);
Scheme_Object *(*scheme_make_noneternal_prim)(Scheme_Prim *prim);
Scheme_Object *(*scheme_make_closed_prim)(Scheme_Closed_Prim *prim, void *data);
Scheme_Object *(*scheme_make_prim_w_arity)(Scheme_Prim *prim, const char *name,
					mzshort mina, mzshort maxa);
Scheme_Object *(*scheme_make_folding_prim)(Scheme_Prim *prim,
					const char *name,
					mzshort mina, mzshort maxa,
					short functional);
Scheme_Object *(*scheme_make_noneternal_prim_w_arity)(Scheme_Prim *prim,
						   const char *name,
						   mzshort mina, mzshort maxa);
Scheme_Object *(*scheme_make_closed_prim_w_arity)(Scheme_Closed_Prim *prim,
					       void *data, const char *name,
					       mzshort mina, mzshort maxa);
Scheme_Object *(*scheme_make_folding_closed_prim)(Scheme_Closed_Prim *prim,
					       void *data, const char *name,
					       mzshort mina, mzshort maxa,
					       short functional);
Scheme_Object *(*scheme_make_prim_w_everything)(Scheme_Prim *fun, int eternal,
						       const char *name,
						       mzshort mina, mzshort maxa,
						       short folding,
						       mzshort minr, mzshort maxr);
Scheme_Object *(*scheme_make_closed_prim_w_everything)(Scheme_Closed_Prim *fun,
							      void *data,
							      const char *name,
							      mzshort mina, mzshort maxa,
							      short folding,
							      mzshort minr, mzshort maxr);
void (*scheme_prim_is_method)(Scheme_Object *o);
Scheme_Object *(*scheme_make_pair)(Scheme_Object *car, Scheme_Object *cdr);
Scheme_Object *(*scheme_make_immutable_pair)(Scheme_Object *car, Scheme_Object *cdr);
Scheme_Object *(*scheme_make_string)(const char *chars);
Scheme_Object *(*scheme_make_sized_string)(char *chars, long len, int copy);
Scheme_Object *(*scheme_make_sized_offset_string)(char *chars, long d, long len, int copy);
Scheme_Object *(*scheme_make_immutable_sized_string)(char *chars, long len, int copy);
Scheme_Object *(*scheme_make_string_without_copying)(char *chars);
Scheme_Object *(*scheme_alloc_string)(int size, char fill);
Scheme_Object *(*scheme_append_string)(Scheme_Object *, Scheme_Object *);
Scheme_Object *(*scheme_make_vector)(int size, Scheme_Object *fill);
Scheme_Object *(*scheme_make_integer_value)(long i);
Scheme_Object *(*scheme_make_integer_value_from_unsigned)(unsigned long i);
Scheme_Object *(*scheme_make_integer_value_from_long_long)(unsigned long lowhalf, unsigned long hihalf);
Scheme_Object *(*scheme_make_integer_value_from_unsigned_long_long)(unsigned long lowhalf, unsigned long hihalf);
Scheme_Object *(*scheme_make_double)(double d);
#ifdef MZ_USE_SINGLE_FLOATS
Scheme_Object *(*scheme_make_float)(float f) ;
#endif
Scheme_Object *(*scheme_make_char)(char ch);
Scheme_Object *(*scheme_make_sema)(long v);
void (*scheme_post_sema)(Scheme_Object *o);
void (*scheme_post_sema_all)(Scheme_Object *o);
int (*scheme_wait_sema)(Scheme_Object *o, int just_try);
Scheme_Object **scheme_char_constants;
Scheme_Object *(*scheme_make_channel)();
int (*scheme_get_int_val)(Scheme_Object *o, long *v);
int (*scheme_get_unsigned_int_val)(Scheme_Object *o, unsigned long *v);
double (*scheme_real_to_double)(Scheme_Object *r);
Scheme_Object *(*scheme_make_cptr)(void *cptr, const char *typestr);
const char *(*scheme_get_proc_name)(Scheme_Object *p, int *len, int for_error);
/*========================================================================*/
/*                               bignums                                  */
/*========================================================================*/
Scheme_Object *(*scheme_make_bignum)(long v);
Scheme_Object *(*scheme_make_bignum_from_unsigned)(unsigned long v);
double (*scheme_bignum_to_double)(const Scheme_Object *n);
Scheme_Object *(*scheme_bignum_from_double)(double d);
#ifdef MZ_USE_SINGLE_FLOATS
float (*scheme_bignum_to_float)(const Scheme_Object *n);
Scheme_Object *(*scheme_bignum_from_float)(float d);
#else
# define scheme_bignum_to_float scheme_bignum_to_double
# define scheme_bignum_from_float scheme_bignum_from_double
#endif
char *(*scheme_bignum_to_string)(const Scheme_Object *n, int radix);
char *(*scheme_bignum_to_allocated_string)(const Scheme_Object *n, int radix, int alloc);
Scheme_Object *(*scheme_read_bignum)(const char *str, int offset, int radix);
Scheme_Object *(*scheme_bignum_normalize)(const Scheme_Object *n);
/*========================================================================*/
/*                              rationals                                 */
/*========================================================================*/
Scheme_Object *(*scheme_make_rational)(const Scheme_Object *r, const Scheme_Object *d);
double (*scheme_rational_to_double)(const Scheme_Object *n);
Scheme_Object *(*scheme_rational_from_double)(double d);
#ifdef MZ_USE_SINGLE_FLOATS
float (*scheme_rational_to_float)(const Scheme_Object *n);
Scheme_Object *(*scheme_rational_from_float)(float d);
#else
# define scheme_rational_to_float scheme_rational_to_double
# define scheme_rational_from_float scheme_rational_from_double
#endif
Scheme_Object *(*scheme_rational_normalize)(const Scheme_Object *n);
Scheme_Object *(*scheme_rational_numerator)(const Scheme_Object *n);
Scheme_Object *(*scheme_rational_denominator)(const Scheme_Object *n);
/*========================================================================*/
/*                              complexes                                 */
/*========================================================================*/
Scheme_Object *(*scheme_make_complex)(const Scheme_Object *r, const Scheme_Object *i);
Scheme_Object *(*scheme_complex_normalize)(const Scheme_Object *n);
Scheme_Object *(*scheme_complex_real_part)(const Scheme_Object *n);
Scheme_Object *(*scheme_complex_imaginary_part)(const Scheme_Object *n);
/* Exact/inexact: */
int (*scheme_is_exact)(Scheme_Object *n);
int (*scheme_is_inexact)(Scheme_Object *n);
/*========================================================================*/
/*                 macros, syntax, and compilation                        */
/*========================================================================*/
Scheme_Object *(*scheme_expand)(Scheme_Object *form, Scheme_Env *env);
Scheme_Object *(*scheme_compile)(Scheme_Object *form, Scheme_Env *env, int writeable);
/*========================================================================*/
/*                               ports                                    */
/*========================================================================*/
Scheme_Object *(*scheme_read)(Scheme_Object *port);
Scheme_Object *(*scheme_read_syntax)(Scheme_Object *port, Scheme_Object *stxsrc);
void (*scheme_write)(Scheme_Object *obj, Scheme_Object *port);
void (*scheme_display)(Scheme_Object *obj, Scheme_Object *port);
void (*scheme_write_w_max)(Scheme_Object *obj, Scheme_Object *port, long maxl);
void (*scheme_display_w_max)(Scheme_Object *obj, Scheme_Object *port, long maxl);
void (*scheme_write_string)(const char *str, long len, Scheme_Object *port);
long (*scheme_put_string)(const char *who, Scheme_Object *port,
				 const char *str, long d, long len,
				 int rarely_block);
char *(*scheme_write_to_string)(Scheme_Object *obj, long *len);
char *(*scheme_display_to_string)(Scheme_Object *obj, long *len);
char *(*scheme_write_to_string_w_max)(Scheme_Object *obj, long *len, long maxl);
char *(*scheme_display_to_string_w_max)(Scheme_Object *obj, long *len, long maxl);
void (*scheme_debug_print)(Scheme_Object *obj);
void (*scheme_flush_output)(Scheme_Object *port);
char *(*scheme_format)(char *format, int flen, int argc, Scheme_Object **argv, long *rlen);
void (*scheme_printf)(char *format, int flen, int argc, Scheme_Object **argv);
int (*scheme_getc)(Scheme_Object *port);
int (*scheme_peekc)(Scheme_Object *port);
int (*scheme_peekc_skip)(Scheme_Object *port, Scheme_Object *skip);
int (*scheme_getc_special_ok)(Scheme_Object *port);
int (*scheme_peekc_special_ok)(Scheme_Object *port);
int (*scheme_peekc_special_ok_skip)(Scheme_Object *port, Scheme_Object *skip);
void (*scheme_ungetc)(int ch, Scheme_Object *port);
int (*scheme_char_ready)(Scheme_Object *port);
int (*scheme_peekc_is_ungetc)(Scheme_Object *port);
void (*scheme_need_wakeup)(Scheme_Object *port, void *fds);
long (*scheme_get_string)(const char *who,
				 Scheme_Object *port,
				 char *buffer, long offset, long size,
				 int only_avail,
				 int peek, Scheme_Object *peek_skip);
long (*scheme_get_chars)(Scheme_Object *port, long size, char *buffer, int offset);
long (*scheme_tell)(Scheme_Object *port);
long (*scheme_output_tell)(Scheme_Object *port);
long (*scheme_tell_line)(Scheme_Object *port);
long (*scheme_tell_column)(Scheme_Object *port);
void (*scheme_count_lines)(Scheme_Object *port);
void (*scheme_close_input_port)(Scheme_Object *port);
void (*scheme_close_output_port)(Scheme_Object *port);
Scheme_Object *(*scheme_make_port_type)(const char *name);
Scheme_Input_Port *(*scheme_make_input_port)(Scheme_Object *subtype, void *data,
						    Scheme_Get_String_Fun get_string_fun,
						    Scheme_Peek_String_Fun peek_string_fun,
						    Scheme_In_Ready_Fun char_ready_fun,
						    Scheme_Close_Input_Fun close_fun,
						    Scheme_Need_Wakeup_Input_Fun need_wakeup_fun,
						    int must_close);
Scheme_Output_Port *(*scheme_make_output_port)(Scheme_Object *subtype,
						      void *data,
						      Scheme_Write_String_Fun write_string_fun,
						      Scheme_Out_Ready_Fun ready_fun,
						      Scheme_Close_Output_Fun close_fun,
						      Scheme_Need_Wakeup_Output_Fun need_wakeup_fun,
						      int must_close);
Scheme_Object *(*scheme_open_input_file)(const char *name, const char *who);
Scheme_Object *(*scheme_open_output_file)(const char *name, const char *who);
Scheme_Object *(*scheme_make_file_input_port)(FILE *fp);
Scheme_Object *(*scheme_make_named_file_input_port)(FILE *fp, const char *filename);
Scheme_Object *(*scheme_make_file_output_port)(FILE *fp);
Scheme_Object *(*scheme_make_string_input_port)(const char *str);
Scheme_Object *(*scheme_make_sized_string_input_port)(const char *str, long len);
Scheme_Object *(*scheme_make_string_output_port)();
char *(*scheme_get_string_output)(Scheme_Object *);
char *(*scheme_get_sized_string_output)(Scheme_Object *, long *len);
void (*scheme_pipe)(Scheme_Object **read, Scheme_Object **write);
void (*scheme_pipe_with_limit)(Scheme_Object **write, Scheme_Object **read, int maxsize);
long (*scheme_set_file_position)(Scheme_Object *port, long pos);
int (*scheme_file_exists)(char *filename);
int (*scheme_directory_exists)(char *dirname);
char *(*scheme_expand_filename)(char* filename, int ilen, const char *errorin, int *ex, int guards);
char *(*scheme_os_getcwd)(char *buf, int buflen, int *actlen, int noexn);
int (*scheme_os_setcwd)(char *buf, int noexn);
char *(*scheme_getdrive)(void);
Scheme_Object *(*scheme_split_pathname)(const char *path, int len, Scheme_Object **base, int *isdir);
Scheme_Object *(*scheme_build_pathname)(int argc, Scheme_Object **argv);
#ifdef MACINTOSH_EVENTS
char *(*scheme_mac_spec_to_path)(mzFSSpec *spec);
int (*scheme_mac_path_to_spec)(const char *filename, mzFSSpec *spec);
#endif
void *(*scheme_alloc_fdset_array)(int count, int permanent);
void *(*scheme_init_fdset_array)(void *fdarray, int count);
void *(*scheme_get_fdset)(void *fdarray, int pos);
void (*scheme_fdzero)(void *fd);
void (*scheme_fdset)(void *fd, int pos);
void (*scheme_fdclr)(void *fd, int pos);
int (*scheme_fdisset)(void *fd, int pos);
void (*scheme_add_fd_handle)(void *h, void *fds, int repost);
void (*scheme_add_fd_eventmask)(void *fds, int mask);
void (*scheme_security_check_file)(const char *who, const char *filename, int guards);
void (*scheme_security_check_network)(const char *who, const char *host, int port, int client);
int (*scheme_get_host_address)(const char *address, int id, void *result);
/*========================================================================*/
/*                        namespace/environment                           */
/*========================================================================*/
Scheme_Object *(*scheme_make_namespace)(int argc, Scheme_Object *argv[]);
void (*scheme_add_namespace_option)(Scheme_Object *key, void (*f)(Scheme_Env *));
void (*scheme_require_from_original_env)(Scheme_Env *env, int syntax_only);
void (*scheme_add_global)(const char *name, Scheme_Object *val, Scheme_Env *env);
void (*scheme_add_global_symbol)(Scheme_Object *name, Scheme_Object *val,
			      Scheme_Env *env);
Scheme_Object *(*scheme_make_envunbox)(Scheme_Object *value);
Scheme_Object *(*scheme_lookup_global)(Scheme_Object *symbol, Scheme_Env *env);
Scheme_Bucket *(*scheme_global_bucket)(Scheme_Object *symbol, Scheme_Env *env);
Scheme_Bucket *(*scheme_global_keyword_bucket)(Scheme_Object *symbol, Scheme_Env *env);
Scheme_Bucket *(*scheme_module_bucket)(Scheme_Object *mod, Scheme_Object *var, int pos, Scheme_Env *env);
Scheme_Bucket *(*scheme_exptime_global_bucket)(Scheme_Object *symbol, Scheme_Env *env);
Scheme_Bucket *(*scheme_exptime_module_bucket)(Scheme_Object *mod, Scheme_Object *var, int pos, Scheme_Env *env);
Scheme_Object *(*scheme_builtin_value)(const char *name); /* convenience */
void (*scheme_set_global_bucket)(char *proc, Scheme_Bucket *var, Scheme_Object *val,
			      int set_undef);
void (*scheme_install_macro)(Scheme_Bucket *b, Scheme_Object *v);
void (*scheme_save_initial_module_set)(Scheme_Env *env);
Scheme_Env *(*scheme_primitive_module)(Scheme_Object *name, Scheme_Env *for_env);
void (*scheme_finish_primitive_module)(Scheme_Env *env);
Scheme_Object *(*scheme_make_modidx)(Scheme_Object *path,
				  Scheme_Object *base,
				  Scheme_Object *resolved);
Scheme_Object *(*scheme_declare_module)(Scheme_Object *shape, Scheme_Invoke_Proc ivk,
				     Scheme_Invoke_Proc sivk, void *data, Scheme_Env *env);
/*========================================================================*/
/*                                symbols                                 */
/*========================================================================*/
Scheme_Object *(*scheme_intern_symbol)(const char *name);
Scheme_Object *(*scheme_intern_exact_symbol)(const char *name, unsigned int len);
Scheme_Object *(*scheme_make_symbol)(const char *name); /* Make uninterned */
Scheme_Object *(*scheme_make_exact_symbol)(const char *name, unsigned int len); /* Exact case */
const char *(*scheme_symbol_name)(Scheme_Object *sym);
const char *(*scheme_symbol_name_and_size)(Scheme_Object *sym, unsigned int *l, int flags);
char *(*scheme_symbol_val)(Scheme_Object *sym);
/*========================================================================*/
/*                                structs                                 */
/*========================================================================*/
Scheme_Object **(*scheme_make_struct_values)(Scheme_Object *struct_type,
					  Scheme_Object **names,
					  int count, int flags);
Scheme_Object **(*scheme_make_struct_names)(Scheme_Object *base,
					 Scheme_Object *field_names,
					 int flags, int *count_out);
Scheme_Object *(*scheme_make_struct_type)(Scheme_Object *base,
				       Scheme_Object *parent,
				       Scheme_Object *inspector,
				       int num_fields, int num_uninit_fields,
				       Scheme_Object *uninit_val,
				       Scheme_Object *properties);
Scheme_Object *(*scheme_make_struct_instance)(Scheme_Object *stype,
					   int argc,
					   Scheme_Object **argv);
Scheme_Object *(*scheme_make_struct_exptime)(Scheme_Object **names, int count,
						    Scheme_Object *super_sym,
						    Scheme_Object *super_exptime,
						    int flags);
int (*scheme_is_struct_instance)(Scheme_Object *type, Scheme_Object *v);
Scheme_Object *(*scheme_struct_ref)(Scheme_Object *s, int pos);
void (*scheme_struct_set)(Scheme_Object *s, int pos, Scheme_Object *v);
Scheme_Object *(*scheme_make_struct_type_property)(Scheme_Object *name);
Scheme_Object *(*scheme_make_struct_type_property_w_guard)(Scheme_Object *name, Scheme_Object *guard);
Scheme_Object *(*scheme_struct_type_property_ref)(Scheme_Object *prop, Scheme_Object *s);
/*========================================================================*/
/*                              utilities                                 */
/*========================================================================*/
int (*scheme_eq)(Scheme_Object *obj1, Scheme_Object *obj2);
int (*scheme_eqv)(Scheme_Object *obj1, Scheme_Object *obj2);
int (*scheme_equal)(Scheme_Object *obj1, Scheme_Object *obj2);
#ifdef MZ_PRECISE_GC
long (*scheme_hash_key)(Scheme_Object *o);
#endif
long (*scheme_equal_hash_key)(Scheme_Object *o);
long (*scheme_equal_hash_key2)(Scheme_Object *o);
Scheme_Object *(*scheme_build_list)(int argc, Scheme_Object **argv);
void (*scheme_make_list_immutable)(Scheme_Object *l);
int (*scheme_list_length)(Scheme_Object *list);
int (*scheme_proper_list_length)(Scheme_Object *list);
Scheme_Object *(*scheme_alloc_list)(int size);
Scheme_Object *(*scheme_map_1)(Scheme_Object *(*f)(Scheme_Object*),
			    Scheme_Object *l);
Scheme_Object *(*scheme_car)(Scheme_Object *pair);
Scheme_Object *(*scheme_cdr)(Scheme_Object *pair);
Scheme_Object *(*scheme_cadr)(Scheme_Object *pair);
Scheme_Object *(*scheme_caddr)(Scheme_Object *pair);
Scheme_Object *(*scheme_vector_to_list)(Scheme_Object *vec);
Scheme_Object *(*scheme_list_to_vector)(Scheme_Object *list);
Scheme_Object *(*scheme_append)(Scheme_Object *lstx, Scheme_Object *lsty);
Scheme_Object *(*scheme_box)(Scheme_Object *v);
Scheme_Object *(*scheme_unbox)(Scheme_Object *obj);
void (*scheme_set_box)(Scheme_Object *b, Scheme_Object *v);
Scheme_Object *(*scheme_make_weak_box)(Scheme_Object *v);
Scheme_Object *(*scheme_load)(const char *file);
Scheme_Object *(*scheme_load_extension)(const char *filename, Scheme_Env *env);
void (*scheme_register_extension_global)(void *ptr, long size);
long (*scheme_get_seconds)(void);
long (*scheme_get_milliseconds)(void);
double (*scheme_get_inexact_milliseconds)(void);
long (*scheme_get_process_milliseconds)(void);
char *(*scheme_banner)(void);
char *(*scheme_version)(void);
int (*scheme_check_proc_arity)(const char *where, int a,
			    int which, int argc, Scheme_Object **argv);
char *(*scheme_make_provided_string)(Scheme_Object *o, int count, int *len);
char *(*scheme_make_args_string)(char *s, int which, int argc, Scheme_Object **argv, long *len);
void (*scheme_no_dumps)(char *why);
const char *(*scheme_system_library_subpath)();
void (*scheme_signal_received)(void);
#ifndef SCHEME_EX_INLINE
} Scheme_Extension_Table;
#endif
