/*
  MzScheme
  Copyright (c) 1995-2000 Matthew Flatt
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

/* START */

/*========================================================================*/
/*                       setjmpup (continuations)                         */
/*========================================================================*/

void scheme_init_jmpup_buf(Scheme_Jumpup_Buf *b);
int scheme_setjmpup_relative(Scheme_Jumpup_Buf *b, void *base, 
			     void *start, Scheme_Jumpup_Buf *cont);
void scheme_longjmpup(Scheme_Jumpup_Buf *b);
void scheme_reset_jmpup_buf(Scheme_Jumpup_Buf *b);

#ifdef USE_MZ_SETJMP
int scheme_setjmp(mz_jmp_buf b);
void scheme_longjmp(mz_jmp_buf b, int v);
#endif

void scheme_clear_escape(void);

/*========================================================================*/
/*                                parameters                              */
/*========================================================================*/

Scheme_Object *scheme_make_config(Scheme_Config *base);
Scheme_Object *scheme_branch_config(void);
int scheme_new_param(void);

Scheme_Object *scheme_param_config(char *name, Scheme_Object *pos,
				   int argc, Scheme_Object **argv,
				   int arity, 
				   Scheme_Prim *check, char *expected,
				   int isbool);
Scheme_Object *scheme_register_parameter(Scheme_Prim *function, char *name, int which);
Scheme_Env *scheme_get_env(Scheme_Config *config);

/*========================================================================*/
/*                                threads                                 */
/*========================================================================*/

#ifdef MZ_REAL_THREADS
Scheme_Process *scheme_get_current_process();
#else
#ifndef LINK_EXTENSIONS_BY_TABLE
extern Scheme_Process *scheme_current_process;
extern volatile int scheme_fuel_counter;
#else
extern Scheme_Process **scheme_current_process_ptr;
extern volatile int *scheme_fuel_counter_ptr;
#endif
#endif

#ifndef NO_SCHEME_THREADS
Scheme_Object *scheme_thread(Scheme_Object *thunk, Scheme_Config *config);
Scheme_Object *scheme_thread_w_manager(Scheme_Object *thunk, Scheme_Config *config,
				       Scheme_Manager *mgr);
void scheme_kill_thread(Scheme_Process *p);
#endif
void scheme_break_thread(Scheme_Process *p);

#ifndef MZ_REAL_THREADS
void scheme_process_block(float sleep_time);
void scheme_swap_process(Scheme_Process *process);
#else
void scheme_process_block_w_process(float sleep_time, Scheme_Process *p);
#endif

void scheme_weak_suspend_thread(Scheme_Process *p);
void scheme_weak_resume_thread(Scheme_Process *p);

int scheme_block_until(int (*f)(Scheme_Object *), void (*fdfd)(Scheme_Object *, void *), void *, float);

int scheme_in_main_thread(void);

int scheme_tls_allocate();
void scheme_tls_set(int pos, void *v);
void *scheme_tls_get(int pos);

Scheme_Manager *scheme_make_manager(Scheme_Manager *);
Scheme_Manager_Reference *scheme_add_managed(Scheme_Manager *m, Scheme_Object *o, 
					     Scheme_Close_Manager_Client *f, void *data, 
					     int strong);
void scheme_remove_managed(Scheme_Manager_Reference *m, Scheme_Object *o);
void scheme_close_managed(Scheme_Manager *m);

/*========================================================================*/
/*                              error handling                            */
/*========================================================================*/

void scheme_signal_error(char *msg, ...);
void scheme_raise_exn(int exnid, ...);
void scheme_warning(char *msg, ...);

void scheme_wrong_count(const char *name, int minc, int maxc, int argc,
			Scheme_Object **argv);
void scheme_case_lambda_wrong_count(const char *name, int argc, 
				    Scheme_Object **argv, int count, ...);
void scheme_wrong_type(const char *name, const char *expected, 
		       int which, int argc,
		       Scheme_Object **argv);
void scheme_arg_mismatch(const char *name, const char *msg, Scheme_Object *o);
void scheme_wrong_return_arity(const char *where, 
			       int expected, int got,
			       Scheme_Object **argv,
			       const char *context_detail, ...);
void scheme_unbound_global(Scheme_Object *name) ;

Scheme_Object *scheme_dynamic_wind(void (*pre)(void *),
				   Scheme_Object *(*act)(void *),
				   void (*post)(void *), 
				   Scheme_Object *(*jmp_handler)(void *),
				   void *data);

/*========================================================================*/
/*                                 types                                  */
/*========================================================================*/

Scheme_Type scheme_make_type(const char *name);

char *scheme_get_type_name(Scheme_Type type);

/* Type readers & writers for compiled code data */
void scheme_install_type_reader(Scheme_Type type, Scheme_Type_Reader f);
void scheme_install_type_writer(Scheme_Type type, Scheme_Type_Writer f);

/*========================================================================*/
/*                              constants                                 */
/*========================================================================*/

extern Scheme_Object scheme_eof[1];
extern Scheme_Object scheme_null[1];
extern Scheme_Object scheme_true[1];
extern Scheme_Object scheme_false[1];
extern Scheme_Object scheme_void[1];
extern Scheme_Object scheme_undefined[1];
extern Scheme_Object *scheme_tail_call_waiting;
extern Scheme_Object *scheme_multiple_values;

/*========================================================================*/
/*                              evaluation                                */
/*========================================================================*/

Scheme_Object *scheme_eval(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *scheme_eval_multi(Scheme_Object *obj, Scheme_Env *env);

Scheme_Object *scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *_scheme_eval_compiled(Scheme_Object *obj, Scheme_Env *env);
Scheme_Object *_scheme_eval_compiled_multi(Scheme_Object *obj, Scheme_Env *env);

#ifndef MZ_REAL_THREADS
Scheme_Object *scheme_apply(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
Scheme_Object *scheme_apply_multi(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
Scheme_Object *scheme_apply_eb(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
Scheme_Object *scheme_apply_multi_eb(Scheme_Object *rator, int num_rands, Scheme_Object **rands);
#else
Scheme_Object *scheme_apply_wp(Scheme_Object *rator, int num_rands, Scheme_Object **rands,
			       Scheme_Process *p);
Scheme_Object *scheme_apply_multi_wp(Scheme_Object *rator, int num_rands, Scheme_Object **rands,
				     Scheme_Process *p);
Scheme_Object *scheme_apply_eb_wp(Scheme_Object *rator, int num_rands, Scheme_Object **rands,
				  Scheme_Process *p);
Scheme_Object *scheme_apply_multi_eb_wp(Scheme_Object *rator, int num_rands, Scheme_Object **rands,
					Scheme_Process *p);
#endif
Scheme_Object *scheme_apply_to_list(Scheme_Object *rator, Scheme_Object *argss);
Scheme_Object *scheme_eval_string(const char *str, Scheme_Env *env);
Scheme_Object *scheme_eval_string_multi(const char *str, Scheme_Env *env);
Scheme_Object *scheme_eval_string_all(const char *str, Scheme_Env *env, int all);

Scheme_Object *_scheme_apply_known_closed_prim(Scheme_Object *rator, int argc,
					       Scheme_Object **argv);
Scheme_Object *_scheme_apply_known_closed_prim_multi(Scheme_Object *rator, int argc,
						     Scheme_Object **argv);
Scheme_Object *_scheme_apply_closed_prim(Scheme_Object *rator, int argc,
					 Scheme_Object **argv);
Scheme_Object *_scheme_apply_closed_prim_multi(Scheme_Object *rator, int argc,
					       Scheme_Object **argv);

Scheme_Object *scheme_values(int c, Scheme_Object **v);

Scheme_Object *scheme_check_one_value(Scheme_Object *v);

/* Tail calls - only use these when you're writing new functions/syntax */
Scheme_Object *scheme_tail_apply(Scheme_Object *f, int n, Scheme_Object **arg);
Scheme_Object *scheme_tail_apply_no_copy(Scheme_Object *f, int n, Scheme_Object **arg);
Scheme_Object *scheme_tail_apply_to_list(Scheme_Object *f, Scheme_Object *l);

Scheme_Object *scheme_tail_eval_expr(Scheme_Object *obj);

void scheme_set_tail_buffer_size(int s);
Scheme_Object *scheme_force_value(Scheme_Object *);

void scheme_set_cont_mark(Scheme_Object *key, Scheme_Object *val);
void scheme_push_continuation_frame(Scheme_Cont_Frame_Data *);
void scheme_pop_continuation_frame(Scheme_Cont_Frame_Data *);
void scheme_temp_dec_mark_depth();
void scheme_temp_inc_mark_depth();

Scheme_Object *scheme_current_continuation_marks(void);

/* Internal */
#ifndef MZ_REAL_THREADS
Scheme_Object *scheme_do_eval(Scheme_Object *obj, int _num_rands, Scheme_Object **rands, int val);
#else
Scheme_Object *scheme_do_eval_w_process(Scheme_Object *obj, int _num_rands, Scheme_Object **rands, int val, Scheme_Process *p);
#endif

/*========================================================================*/
/*                           memory management                            */
/*========================================================================*/

/* The core allocator functions depend on the GC. Macros in scheme.h */
/*  map to the apporpriate core allocation function. */

#ifndef SCHEME_NO_GC
# ifndef SCHEME_NO_GC_PROTO
void *GC_malloc(size_t size_in_bytes);
void *GC_malloc_atomic(size_t size_in_bytes);
#  ifdef MZ_PRECISE_GC
void *GC_malloc_one_tagged(size_t size_in_bytes);
void *GC_malloc_atomic_uncollectable(size_t size_in_bytes);
void *GC_malloc_array_tagged(size_t size_in_bytes);
#  else
void *GC_malloc_stubborn(size_t size_in_bytes);
void *GC_malloc_uncollectable(size_t size_in_bytes);
#  endif
# endif
#endif

void *scheme_malloc_eternal(size_t n);
void scheme_end_stubborn_change(void *p);

void *scheme_calloc(size_t num, size_t size);

char *scheme_strdup(const char *str);
char *scheme_strdup_eternal(const char *str);

void *scheme_malloc_fail_ok(void *(*f)(size_t), size_t);

#ifndef MZ_PRECISE_GC
void scheme_weak_reference(void **p);
void scheme_weak_reference_indirect(void **p, void *v);
void scheme_unweak_reference(void **p);
#endif
void scheme_add_finalizer(void *p, void (*f)(void *p, void *data), void *data);
void scheme_add_finalizer_once(void *p, void (*f)(void *p, void *data), void *data);
void scheme_add_scheme_finalizer(void *p, void (*f)(void *p, void *data), void *data);
void scheme_add_scheme_finalizer_once(void *p, void (*f)(void *p, void *data), void *data);
void scheme_register_finalizer(void *p, 
			       void (*f)(void *p, void *data), void *data,
			       void (**oldf)(void *p, void *data), 
			       void **olddata);
void scheme_remove_all_finalization(void *p);

void scheme_dont_gc_ptr(void *p);
void scheme_gc_ptr_ok(void *p);

void scheme_collect_garbage(void);

/*========================================================================*/
/*                             hash tables                                */
/*========================================================================*/

Scheme_Hash_Table *scheme_hash_table(int size, int type, 
				      int w_const, int forever);
void scheme_add_to_table(Scheme_Hash_Table *table, const char *key, void *val, int);
void scheme_change_in_table(Scheme_Hash_Table *table, const char *key, void *new_val);
void *scheme_lookup_in_table(Scheme_Hash_Table *table, const char *key);
Scheme_Bucket *scheme_bucket_from_table(Scheme_Hash_Table *table, const char *key);

/*========================================================================*/
/*                   basic Scheme value constructors                      */
/*========================================================================*/

Scheme_Object *scheme_make_prim(Scheme_Prim *prim);
Scheme_Object *scheme_make_noneternal_prim(Scheme_Prim *prim);
Scheme_Object *scheme_make_closed_prim(Scheme_Closed_Prim *prim, void *data);
Scheme_Object *scheme_make_prim_w_arity(Scheme_Prim *prim, const char *name,
					short mina, short maxa);
Scheme_Object *scheme_make_folding_prim(Scheme_Prim *prim, 
					const char *name,
					short mina, short maxa,
					short functional);
Scheme_Object *scheme_make_noneternal_prim_w_arity(Scheme_Prim *prim, 
						   const char *name, 
						   short mina, short maxa);
Scheme_Object *scheme_make_closed_prim_w_arity(Scheme_Closed_Prim *prim, 
					       void *data, const char *name,
					       short mina, short maxa);
Scheme_Object *scheme_make_folding_closed_prim(Scheme_Closed_Prim *prim, 
					       void *data, const char *name,
					       short mina, short maxa,
					       short functional);

Scheme_Object *scheme_make_pair(Scheme_Object *car, Scheme_Object *cdr);
Scheme_Object *scheme_make_string(const char *chars);
Scheme_Object *scheme_make_sized_string(char *chars, long len, int copy);
Scheme_Object *scheme_make_sized_offset_string(char *chars, long d, long len, int copy);
Scheme_Object *scheme_make_immutable_sized_string(char *chars, long len, int copy);
Scheme_Object *scheme_make_string_without_copying(char *chars);
Scheme_Object *scheme_alloc_string(int size, char fill);
Scheme_Object *scheme_append_string(Scheme_Object *, Scheme_Object *);
Scheme_Object *scheme_make_vector(int size, Scheme_Object *fill);
Scheme_Object *scheme_make_integer_value(long i);
Scheme_Object *scheme_make_integer_value_from_unsigned(unsigned long i);
Scheme_Object *scheme_make_double(double d);
#ifdef MZ_USE_SINGLE_FLOATS
Scheme_Object *scheme_make_float(float f) ;
#endif
Scheme_Object *scheme_make_char(char ch);
#ifndef NO_SCHEME_THREADS
Scheme_Object *scheme_make_sema(long v);
void scheme_post_sema(Scheme_Object *o);
int scheme_wait_sema(Scheme_Object *o, int just_try);
#endif
extern Scheme_Object **scheme_char_constants;

int scheme_get_int_val(Scheme_Object *o, long *v);
int scheme_get_unsigned_int_val(Scheme_Object *o, unsigned long *v);

double scheme_real_to_double(Scheme_Object *r);

const char *scheme_get_proc_name(Scheme_Object *p, int *len, int for_error);

/*========================================================================*/
/*                               bignums                                  */
/*========================================================================*/

Scheme_Object *scheme_make_bignum(long v);
Scheme_Object *scheme_make_bignum_from_unsigned(unsigned long v);
double scheme_bignum_to_double(const Scheme_Object *n);
Scheme_Object *scheme_bignum_from_double(double d);
#ifdef MZ_USE_SINGLE_FLOATS
float scheme_bignum_to_float(const Scheme_Object *n);
Scheme_Object *scheme_bignum_from_float(float d);
#else
# define scheme_bignum_to_float scheme_bignum_to_double
# define scheme_bignum_from_float scheme_bignum_from_double
#endif
char *scheme_bignum_to_string(const Scheme_Object *n, int radix);
Scheme_Object *scheme_read_bignum(const char *str, int offset, int radix);
Scheme_Object *scheme_bignum_normalize(const Scheme_Object *n);

long scheme_double_to_int(const char *where, double d) ;

/*========================================================================*/
/*                              rationals                                 */
/*========================================================================*/

Scheme_Object *scheme_make_rational(const Scheme_Object *r, const Scheme_Object *d);
double scheme_rational_to_double(const Scheme_Object *n);
Scheme_Object *scheme_rational_from_double(double d);
#ifdef MZ_USE_SINGLE_FLOATS
float scheme_rational_to_float(const Scheme_Object *n);
Scheme_Object *scheme_rational_from_float(float d);
#else
# define scheme_rational_to_float scheme_rational_to_double
# define scheme_rational_from_float scheme_rational_from_double
#endif
Scheme_Object *scheme_rational_normalize(const Scheme_Object *n);
Scheme_Object *scheme_rational_numerator(const Scheme_Object *n);
Scheme_Object *scheme_rational_denominator(const Scheme_Object *n);

/*========================================================================*/
/*                              complexes                                 */
/*========================================================================*/

Scheme_Object *scheme_make_complex(const Scheme_Object *r, const Scheme_Object *i);
Scheme_Object *scheme_complex_normalize(const Scheme_Object *n);
Scheme_Object *scheme_complex_real_part(const Scheme_Object *n);
Scheme_Object *scheme_complex_imaginary_part(const Scheme_Object *n);

/* Exact/inexact: */
int scheme_is_exact(Scheme_Object *n);
int scheme_is_inexact(Scheme_Object *n);

/*========================================================================*/
/*                 macros, syntax, and compilation                        */
/*========================================================================*/

Scheme_Object *scheme_expand(Scheme_Object *form, Scheme_Env *env);

Scheme_Object *scheme_compile(Scheme_Object *form, Scheme_Env *env, int writeable);

Scheme_Object *scheme_link(Scheme_Object *compiled, Scheme_Env *env);

/*========================================================================*/
/*                               ports                                    */
/*========================================================================*/

Scheme_Object *scheme_read(Scheme_Object *port);
Scheme_Object *scheme_read_syntax(Scheme_Object *port, Scheme_Object *stxsrc);
void scheme_write(Scheme_Object *obj, Scheme_Object *port);
void scheme_display(Scheme_Object *obj, Scheme_Object *port);
void scheme_write_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl);
void scheme_display_w_max(Scheme_Object *obj, Scheme_Object *port, long maxl);
void scheme_write_string(const char *str, long len, Scheme_Object *port);
void scheme_write_offset_string(const char *str, long d, long len, Scheme_Object *port);
char *scheme_write_to_string(Scheme_Object *obj, long *len);
char *scheme_display_to_string(Scheme_Object *obj, long *len);
char *scheme_write_to_string_w_max(Scheme_Object *obj, long *len, long maxl);
char *scheme_display_to_string_w_max(Scheme_Object *obj, long *len, long maxl);
void scheme_debug_print(Scheme_Object *obj);
void scheme_flush_output(Scheme_Object *port);

char *scheme_format(char *format, int flen, int argc, Scheme_Object **argv, int *rlen);
void scheme_printf(char *format, int flen, int argc, Scheme_Object **argv);

int scheme_getc(Scheme_Object *port);
int scheme_peekc(Scheme_Object *port);
void scheme_ungetc(int ch, Scheme_Object *port);
int scheme_char_ready(Scheme_Object *port);
int scheme_peekc_is_ungetc(Scheme_Object *port);
void scheme_need_wakeup(Scheme_Object *port, void *fds);
long scheme_get_chars(Scheme_Object *port, long size, char *buffer, int offset);
long scheme_tell(Scheme_Object *port);
long scheme_output_tell(Scheme_Object *port);
long scheme_tell_line(Scheme_Object *port);
long scheme_tell_column(Scheme_Object *port);
void scheme_count_lines(Scheme_Object *port);
void scheme_close_input_port(Scheme_Object *port);
void scheme_close_output_port(Scheme_Object *port);
int scheme_are_all_chars_ready(Scheme_Object *port);

Scheme_Object *scheme_make_port_type(const char *name);
Scheme_Input_Port *scheme_make_input_port(Scheme_Object *subtype, void *data,
					  int (*getc_fun)(Scheme_Input_Port*),
					  int (*peekc_fun)(Scheme_Input_Port*),
					  int (*char_ready_fun)
					  (Scheme_Input_Port*),
					  void (*close_fun)
					  (Scheme_Input_Port*),
					  void (*need_wakeup_fun)
					  (Scheme_Input_Port*, void *),
					  int must_close);
Scheme_Output_Port *scheme_make_output_port(Scheme_Object *subtype,
					    void *data,
					    void (*write_string_fun)
					    (char*, long, long, Scheme_Output_Port*),
					    void (*close_fun)
					    (Scheme_Output_Port*),
					    int must_close);

Scheme_Object *scheme_make_file_input_port(FILE *fp);
Scheme_Object *scheme_make_named_file_input_port(FILE *fp, const char *filename);
Scheme_Object *scheme_make_file_output_port(FILE *fp);

Scheme_Object *scheme_make_string_input_port(const char *str);
Scheme_Object *scheme_make_sized_string_input_port(const char *str, long len);
Scheme_Object *scheme_make_string_output_port();
char *scheme_get_string_output(Scheme_Object *);
char *scheme_get_sized_string_output(Scheme_Object *, int *len);

void scheme_pipe(Scheme_Object **write, Scheme_Object **read);
void scheme_pipe_with_limit(Scheme_Object **write, Scheme_Object **read, int maxsize);

int scheme_file_exists(char *filename);
int scheme_directory_exists(char *dirname);
char *scheme_expand_filename(char* filename, int ilen, char *errorin, int *ex);

char *scheme_os_getcwd(char *buf, int buflen, int *actlen, int noexn);
int scheme_os_setcwd(char *buf, int noexn);
char *scheme_getdrive(void);

Scheme_Object *scheme_split_pathname(const char *path, int len, Scheme_Object **base, int *isdir);
Scheme_Object *scheme_build_pathname(int argc, Scheme_Object **argv);

#ifdef USE_MAC_FILE_TOOLBOX
char *scheme_build_mac_filename(FSSpec *spec, int given_dir);
int scheme_mac_path_to_spec(const char *filename, FSSpec *spec, long *type);
#endif

void *scheme_alloc_fdset_array(int count, int permanent);
void *scheme_init_fdset_array(void *fdarray, int count);
void *scheme_get_fdset(void *fdarray, int pos);
void scheme_fdzero(void *fd);
void scheme_fdset(void *fd, int pos);
void scheme_fdclr(void *fd, int pos);
int scheme_fdisset(void *fd, int pos);
void scheme_add_fd_handle(void *h, void *fds, int repost);
void scheme_add_fd_eventmask(void *fds, int mask);

int scheme_return_eof_for_error();

/*========================================================================*/
/*                        namespace/environment                           */
/*========================================================================*/

Scheme_Object *scheme_make_namespace(int argc, Scheme_Object *argv[]);
void scheme_add_namespace_option(Scheme_Object *key, void (*f)(Scheme_Env *));

void scheme_add_global(const char *name, Scheme_Object *val, Scheme_Env *env);
void scheme_add_global_keyword(const char *name, Scheme_Object *v, Scheme_Env *env);
void scheme_add_global_constant(const char *name, Scheme_Object *v, Scheme_Env *env);
void scheme_remove_global(const char *name, Scheme_Env *env);

void scheme_add_global_symbol(Scheme_Object *name, Scheme_Object *val, 
			      Scheme_Env *env);
void scheme_remove_global_symbol(Scheme_Object *name, Scheme_Env *env);
void scheme_add_global_constant_symbol(Scheme_Object *name, Scheme_Object *v, Scheme_Env *env);

Scheme_Object *scheme_make_envunbox(Scheme_Object *value);

Scheme_Object *scheme_lookup_global(Scheme_Object *symbol, Scheme_Env *env);

Scheme_Bucket *scheme_global_bucket(Scheme_Object *symbol, Scheme_Env *env);
Scheme_Bucket *scheme_global_keyword_bucket(Scheme_Object *symbol, Scheme_Env *env);

void scheme_set_global_bucket(char *proc, Scheme_Bucket *var, Scheme_Object *val,
			      int set_undef);

/*========================================================================*/
/*                                symbols                                 */
/*========================================================================*/

Scheme_Object *scheme_intern_symbol(const char *name);
Scheme_Object *scheme_intern_exact_symbol(const char *name, int len);
Scheme_Object *scheme_make_symbol(const char *name); /* Make uninterned */
Scheme_Object *scheme_make_exact_symbol(const char *name, int len); /* Exact case */
const char *scheme_symbol_name(Scheme_Object *sym);
const char *scheme_symbol_name_and_size(Scheme_Object *sym, int *l, int flags);
char *scheme_symbol_val(Scheme_Object *sym);

/*========================================================================*/
/*                                structs                                 */
/*========================================================================*/

Scheme_Object **scheme_make_struct_values(Scheme_Object *struct_type,
					  Scheme_Object **names,
					  int count, int flags);
Scheme_Object **scheme_make_struct_names(Scheme_Object *base, 
					 Scheme_Object *field_names,
					 int flags, int *count_out);
Scheme_Object *scheme_make_struct_type(Scheme_Object *base, 
				       Scheme_Object *parent, 
				       Scheme_Object *inspector,
				       int num_fields);
Scheme_Object *scheme_make_struct_instance(Scheme_Object *stype,
					   int argc,
					   Scheme_Object **argv);
int scheme_is_struct_instance(Scheme_Object *type, Scheme_Object *v);
Scheme_Object *scheme_struct_ref(Scheme_Object *s, int pos);
void scheme_struct_set(Scheme_Object *s, int pos, Scheme_Object *v);

/*========================================================================*/
/*                              utilities                                 */
/*========================================================================*/

int scheme_eq(Scheme_Object *obj1, Scheme_Object *obj2);
int scheme_eqv(Scheme_Object *obj1, Scheme_Object *obj2);
int scheme_equal(Scheme_Object *obj1, Scheme_Object *obj2);

#ifdef MZ_PRECISE_GC
long scheme_hash_key(Scheme_Object *o);
#endif

Scheme_Object *scheme_build_list(int argc, Scheme_Object **argv);

int scheme_list_length(Scheme_Object *list);
int scheme_proper_list_length(Scheme_Object *list);

Scheme_Object *scheme_alloc_list(int size);
Scheme_Object *scheme_map_1(Scheme_Object *(*f)(Scheme_Object*),
			    Scheme_Object *l);

Scheme_Object *scheme_car(Scheme_Object *pair);
Scheme_Object *scheme_cdr(Scheme_Object *pair);
Scheme_Object *scheme_cadr(Scheme_Object *pair);
Scheme_Object *scheme_caddr(Scheme_Object *pair);

Scheme_Object *scheme_vector_to_list(Scheme_Object *vec);
Scheme_Object *scheme_list_to_vector(Scheme_Object *list);

Scheme_Object *scheme_append(Scheme_Object *lstx, Scheme_Object *lsty);

Scheme_Object *scheme_box(Scheme_Object *v);
Scheme_Object *scheme_unbox(Scheme_Object *obj);
void scheme_set_box(Scheme_Object *b, Scheme_Object *v);

Scheme_Object *scheme_make_weak_box(Scheme_Object *v);

Scheme_Object *scheme_load(const char *file);
Scheme_Object *scheme_load_extension(const char *filename, Scheme_Env *env);
void scheme_register_extension_global(void *ptr, long size);

long scheme_get_milliseconds(void);
long scheme_get_process_milliseconds(void);

char *scheme_banner(void);
char *scheme_version(void);

int scheme_check_proc_arity(const char *where, int a,
			    int which, int argc, Scheme_Object **argv);

char *scheme_make_provided_string(Scheme_Object *o, int count, int *len);
char *scheme_make_args_string(char *s, int which, int argc, Scheme_Object **argv, long *len);

void scheme_no_dumps(char *why);

const char *scheme_system_library_subpath();

void scheme_signal_received(void);
