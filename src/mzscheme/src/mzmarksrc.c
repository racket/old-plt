
START type;

variable_obj {
  Scheme_Bucket *b = (Scheme_Bucket *)p;

 mark:
  gcMARK(b->key);
  gcMARK(b->val);

  if (((Scheme_Bucket_With_Flags *)b)->flags & GLOB_HAS_HOME_PTR)
      gcMARK(((Scheme_Bucket_With_Home *)b)->home);

 size:
  ((((Scheme_Bucket_With_Flags *)b)->flags & GLOB_HAS_HOME_PTR)
   ? gcBYTES_TO_WORDS(sizeof(Scheme_Bucket_With_Home))
   : gcBYTES_TO_WORDS(sizeof(Scheme_Bucket_With_Ref_Id)));
}

local_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Local));
}

second_of_cons {
 mark:
  gcMARK(SCHEME_PTR2_VAL((Scheme_Object *)p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Object));
}

twoptr_obj {
 mark:
  gcMARK(SCHEME_PTR1_VAL((Scheme_Object *)p));
  gcMARK(SCHEME_PTR2_VAL((Scheme_Object *)p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Object));
}

iptr_obj {
 mark:
  gcMARK(SCHEME_IPTR_VAL((Scheme_Object *)p));
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Object));
}

small_object {
 mark:
  gcMARK(((Scheme_Small_Object *)p)->u.ptr_value);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Small_Object));
}

app_rec {
  Scheme_App_Rec *r = (Scheme_App_Rec *)p;

 mark:
  int i = r->num_args + 1;
  while (i--) 
    gcMARK(r->args[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_App_Rec) 
		    + (r->num_args * sizeof(Scheme_Object *))
		    + ((r->num_args + 1) * sizeof(char))));
}

seq_rec {
  Scheme_Sequence *s = (Scheme_Sequence *)p;

 mark:
  int i = s->count;
  while (i--)
    gcMARK(s->array[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Sequence)
		    + ((s->count - 1) * sizeof(Scheme_Object *))));
}

branch_rec {
 mark:
  Scheme_Branch_Rec *b = (Scheme_Branch_Rec *)p;
  
  gcMARK(b->test);
  gcMARK(b->tbranch);
  gcMARK(b->fbranch);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Branch_Rec));
}

unclosed_proc {
 mark:
  Scheme_Closure_Compilation_Data *d = (Scheme_Closure_Compilation_Data *)p;

  if (d->name)
    gcMARK(d->name);
  gcMARK(d->code);
  gcMARK(d->closure_map);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Closure_Compilation_Data));
}

let_value {
 mark:
  Scheme_Let_Value *l = (Scheme_Let_Value *)p;
  
  gcMARK(l->value);
  gcMARK(l->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Value));
}

let_void {
 mark:
  Scheme_Let_Void *l = (Scheme_Let_Void *)p;

  gcMARK(l->body);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Void));
}

letrec {
 mark:
  Scheme_Letrec *l = (Scheme_Letrec *)p;
  
  gcMARK(l->procs);
  gcMARK(l->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Letrec));
}

let_one {
 mark:
  Scheme_Let_One *l = (Scheme_Let_One *)p;
  
  gcMARK(l->value);
  gcMARK(l->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_One));
}

with_cont_mark {
 mark:
  Scheme_With_Continuation_Mark *w = (Scheme_With_Continuation_Mark *)p;

  gcMARK(w->key);
  gcMARK(w->val);
  gcMARK(w->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_With_Continuation_Mark));
}

comp_unclosed_proc {
 mark:
  Scheme_Closure_Compilation_Data *c = (Scheme_Closure_Compilation_Data *)p;
  
  gcMARK(c->closure_map);
  gcMARK(c->code);
  gcMARK(c->name);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Closure_Compilation_Data));
}

comp_let_value {
 mark:
  Scheme_Compiled_Let_Value *c = (Scheme_Compiled_Let_Value *)p;

  gcMARK(c->flags);
  gcMARK(c->value);
  gcMARK(c->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compiled_Let_Value));
}

let_header {
 mark:
  Scheme_Let_Header *h = (Scheme_Let_Header *)p;
  
  gcMARK(h->body);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Let_Header));
}

prim_proc {
  Scheme_Primitive_Proc *prim = (Scheme_Primitive_Proc *)p;

 mark:
  gcMARK(prim->name);

 size:
  ((prim->flags & SCHEME_PRIM_IS_MULTI_RESULT)
   ? gcBYTES_TO_WORDS(sizeof(Scheme_Prim_W_Result_Arity))
   : gcBYTES_TO_WORDS(sizeof(Scheme_Primitive_Proc)));
}

closed_prim_proc {
  Scheme_Closed_Primitive_Proc *c = (Scheme_Closed_Primitive_Proc *)p;

 mark:
  gcMARK(c->name);
  gcMARK(SCHEME_CLSD_PRIM_DATA(c));
  
 size:
  ((c->flags & SCHEME_PRIM_IS_MULTI_RESULT)
   ? gcBYTES_TO_WORDS(sizeof(Scheme_Closed_Prim_W_Result_Arity))
   : gcBYTES_TO_WORDS(sizeof(Scheme_Closed_Primitive_Proc)));
}

linked_closure {
  Scheme_Closed_Compiled_Procedure *c = (Scheme_Closed_Compiled_Procedure *)p;

 mark:
  int i = c->closure_size;
  while (i--)
    gcMARK(c->vals[i]);
  gcMARK(c->code);
  
 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Closed_Compiled_Procedure)
		    + (c->closure_size - 1) * sizeof(Scheme_Object *)));
}

case_closure {
  Scheme_Case_Lambda *c = (Scheme_Case_Lambda *)p;

 mark:
  int i;
  
  for (i = c->count; i--; )
    gcMARK(c->array[i]);
  gcMARK(c->name);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Case_Lambda)
		    + ((c->count - 1) * sizeof(Scheme_Object *))));
}

cont_proc {
 mark:
  Scheme_Cont *c = (Scheme_Cont *)p;
  
  gcMARK(c->dw);
  gcMARK(c->common);
  gcMARK(c->ok);
  gcMARK(c->home);
  gcMARK(c->current_local_env);
  gcMARK(c->save_overflow);
  gcMARK(c->runstack_copied);
  gcMARK(c->cont_mark_stack_copied);
  
  MARK_jmpup(&c->buf);
  MARK_cjs(&c->cjs);
  MARK_stack_state(&c->ss);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont));
}

mark_dyn_wind {
 mark:
  Scheme_Dynamic_Wind *dw = (Scheme_Dynamic_Wind *)p;
  
  gcMARK(dw->data);
  gcMARK(dw->current_local_env);
  gcMARK(dw->cont);
  gcMARK(dw->prev);
    
  MARK_stack_state(&dw->envss);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Dynamic_Wind));
}

mark_overflow {
 mark:
  Scheme_Overflow *o = (Scheme_Overflow *)p;

  gcMARK(o->prev);
  MARK_jmpup(&o->cont);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Overflow));
}

escaping_cont_proc {
 mark:
  Scheme_Escaping_Cont *c = (Scheme_Escaping_Cont *)p;

  gcMARK(c->home);
  gcMARK(c->ok);
  gcMARK(c->f);

  MARK_cjs(&c->cjs);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Escaping_Cont));
}

char_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Small_Object));
}

bignum_obj {
  Scheme_Bignum *b = (Scheme_Bignum *)p;

 mark:
  if (!b->allocated_inline) {
    gcMARK(b->digits);
  } else
    b->digits = ((Small_Bignum *)b)->v;

 size:
  ((!b->allocated_inline)
   ? gcBYTES_TO_WORDS(sizeof(Scheme_Bignum))
   : ((b->allocated_inline > 1)
      ? gcBYTES_TO_WORDS(sizeof(Small_Bignum) + sizeof(bigdig))
      : gcBYTES_TO_WORDS(sizeof(Small_Bignum))));
}

rational_obj {
 mark:
  Scheme_Rational *r = (Scheme_Rational *)p;
  
  gcMARK(r->num);
  gcMARK(r->denom);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Rational));
}

float_obj {
 mark:
 size:
#ifdef MZ_USE_SINGLE_FLOATS
  gcBYTES_TO_WORDS(sizeof(Scheme_Float));
#else
  0;
#endif
}

double_obj {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Double));
}

complex_obj {
 mark:
  Scheme_Complex *c = (Scheme_Complex *)p;
  
  gcMARK(c->r);
  gcMARK(c->i);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Complex));
}

string_obj {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  gcMARK(SCHEME_STR_VAL(o));

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Object));
}

symbol_obj {
  Scheme_Symbol *s = (Scheme_Symbol *)p;

 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Symbol) + s->len - 3);
}

cons_cell {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;
  
  gcMARK(SCHEME_CAR(o));
  gcMARK(SCHEME_CDR(o));

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Object));
}

vector_obj {
  Scheme_Vector *vec = (Scheme_Vector *)p;

 mark:
  int i;
  for (i = vec->size; i--; )
    gcMARK(vec->els[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Vector) 
		    + ((vec->size - 1) * sizeof(Scheme_Object *))));
}

input_port {
 mark:
  Scheme_Input_Port *ip = (Scheme_Input_Port *)p;
  
  gcMARK(ip->sub_type);
  gcMARK(ip->port_data);
  gcMARK(ip->name);
  gcMARK(ip->ungotten);
  gcMARK(ip->read_handler);
  gcMARK(ip->mref);
#ifdef MZ_REAL_THREADS
  gcMARK(ip->sema);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Input_Port));
}

output_port {
 mark:
  Scheme_Output_Port *op = (Scheme_Output_Port *)p;

  gcMARK(op->sub_type);
  gcMARK(op->port_data);
  gcMARK(op->display_handler);
  gcMARK(op->write_handler);
  gcMARK(op->print_handler);
  gcMARK(op->mref);
#ifdef MZ_REAL_THREADS
  gcMARK(op->sema);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Output_Port));
}


syntax_compiler {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Object));
}

process_val {
 mark:
  Scheme_Process *pr = (Scheme_Process *)p;
  
  gcMARK(pr->next);
  gcMARK(pr->prev);
  
  MARK_cjs(&pr->cjs);

  gcMARK(pr->config);

  {
    Scheme_Object **rs = pr->runstack_start;
    gcFIXUP_TYPED_NOW(Scheme_Object **, pr->runstack_start);
    pr->runstack = pr->runstack_start + (pr->runstack - rs);
  }
  gcMARK(pr->runstack_saved);
  
  gcMARK(pr->cont_mark_stack_segments);
  
  MARK_jmpup(&pr->jmpup_buf);
  
  gcMARK(pr->cc_ok);
  gcMARK(pr->ec_ok);
  gcMARK(pr->dw);
  
  gcMARK(pr->nester);
  gcMARK(pr->nestee);
  
  gcMARK(pr->blocker);
  gcMARK(pr->overflow);
  
  gcMARK(pr->current_local_env);
  gcMARK(pr->current_local_mark);
  gcMARK(pr->current_local_name);
  
  gcMARK(pr->print_buffer);
  gcMARK(pr->print_port);
  
  gcMARK(pr->overflow_reply);
  
  gcMARK(pr->tail_buffer);
  
  gcMARK(pr->ku.k.p1);
  gcMARK(pr->ku.k.p2);
  gcMARK(pr->ku.k.p3);
  gcMARK(pr->ku.k.p4);
  
#ifdef MZ_REAL_THREADS
  gcMARK(pr->done_sema);
#endif
  
  gcMARK(pr->list_stack);
  
  gcMARK(pr->rn_memory);
  
  gcMARK(pr->kill_data);
  gcMARK(pr->private_kill_data);
  gcMARK(pr->private_kill_next);
  
  gcMARK(pr->user_tls);
  
  gcMARK(pr->mr_hop);
  gcMARK(pr->mref);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Process));
}

cont_mark_set_val {
 mark:
  Scheme_Cont_Mark_Set *s = (Scheme_Cont_Mark_Set *)p;
  gcMARK(s->chain);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont_Mark_Set));
}

sema_val {
 mark:
  Scheme_Sema *s = (Scheme_Sema *)p;

#if SEMAPHORE_WAITING_IS_COLLECTABLE
  gcMARK(s->first);
  gcMARK(s->last);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Sema));
}

hash_table_val {
 mark:
  Scheme_Hash_Table *ht = (Scheme_Hash_Table *)p;

  gcMARK(ht->buckets);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Hash_Table));
}

namespace_val {
 mark:
  Scheme_Env *e = (Scheme_Env *)p;

  gcMARK(e->module);
  gcMARK(e->module_registry);

  gcMARK(e->rename);
  gcMARK(e->et_rename);

  gcMARK(e->init);

  gcMARK(e->syntax);
  gcMARK(e->exp_env);

  gcMARK(e->shadowed_syntax);

  gcMARK(e->link_midx);

  gcMARK(e->toplevel);
  gcMARK(e->modpair);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Env));
}

random_state_val {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Random_State));
}

compilation_top_val {
 mark:
  Scheme_Compilation_Top *t = (Scheme_Compilation_Top *)p;
  gcMARK(t->code);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compilation_Top));
}

svector_val {
 mark:
  Scheme_Object *o = (Scheme_Object *)p;

  gcMARK(SCHEME_SVEC_VEC(o));

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Object));
}

stx_val {
 mark:
  Scheme_Stx *stx = (Scheme_Stx *)p;
  gcMARK(stx->val);
  gcMARK(stx->src);
  gcMARK(stx->wraps);
  gcMARK(stx->props);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Stx));
}

module_val {
 mark:
  Scheme_Module *m = (Scheme_Module *)p;
  gcMARK(m->modname);

  gcMARK(m->et_imports);
  gcMARK(m->imports);

  gcMARK(m->body);
  gcMARK(m->et_body);

  gcMARK(m->exports);
  gcMARK(m->export_srcs);
  gcMARK(m->export_src_names);

  gcMARK(m->kernel_exclusion);

  gcMARK(m->indirect_exports);
  gcMARK(m->self_modidx);

  gcMARK(m->accessible);

  gcMARK(m->primitive);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Module));
}

modidx_val {
 mark:
  Scheme_Modidx *modidx = (Scheme_Modidx *)p;

  gcMARK(modidx->path);
  gcMARK(modidx->base);
  gcMARK(modidx->resolved);
  gcMARK(modidx->shift_cache);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Modidx));
}

END type;

/**********************************************************************/

START env;

mark_comp_env {
 mark:
  Scheme_Full_Comp_Env *e = (Scheme_Full_Comp_Env *)p;

  gcMARK(e->base.uid);
  gcMARK(e->base.genv);
  gcMARK(e->base.next);
  gcMARK(e->base.values);
  gcMARK(e->base.renames);
  
  gcMARK(e->data.stat_dists);
  gcMARK(e->data.sd_depths);
  gcMARK(e->data.constants);
  gcMARK(e->data.use);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Full_Comp_Env));
}

mark_resolve_info {
 mark:
  Resolve_Info *i = (Resolve_Info *)p;
  
  gcMARK(i->old_pos);
  gcMARK(i->new_pos);
  gcMARK(i->flags);
  gcMARK(i->next);

 size:
  gcBYTES_TO_WORDS(sizeof(Resolve_Info));
}


END env;

/**********************************************************************/

START eval;

mark_comp_info {
 mark:
  Scheme_Compile_Info *i = (Scheme_Compile_Info *)p;
  
  gcMARK(i->value_name);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Compile_Info));
}

mark_saved_stack {
 mark:
  Scheme_Saved_Stack *saved = (Scheme_Saved_Stack *) p;
  Scheme_Object **old = saved->runstack_start;
  
  gcMARK(saved->prev);
  gcFIXUP_TYPED_NOW(Scheme_Object **, saved->runstack_start);
  saved->runstack = saved->runstack_start + (saved->runstack - old);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Saved_Stack));
}

mark_eval_in_env {
 mark:
  Eval_In_Env *ee = (Eval_In_Env *)p;
  
  gcMARK(ee->e);
  gcMARK(ee->config);
  gcMARK(ee->namespace);
  gcMARK(ee->old);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Eval_In_Env));
}

END eval;

/**********************************************************************/

START file;

mark_reply_item {
 mark:
  ReplyItem *r = (ReplyItem *)p;
  
  gcMARK(r->next);

 size:
  gcBYTES_TO_WORDS(sizeof(ReplyItem));
}

END file;

/**********************************************************************/

START fun;

mark_closure_info {
 mark:
  Closure_Info *i = (Closure_Info *)p;
  
  gcMARK(i->local_flags);
  gcMARK(i->real_closure_map);

 size:
  gcBYTES_TO_WORDS(sizeof(Closure_Info));
}

mark_dyn_wind_cell {
 mark:
  Scheme_Dynamic_Wind_List *l = (Scheme_Dynamic_Wind_List *)p;
  
  gcMARK(l->dw);
  gcMARK(l->next);
  
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Dynamic_Wind_List));
}

mark_dyn_wind_info {
 mark:
  Dyn_Wind *d = (Dyn_Wind *)p;
  
  gcMARK(d->pre);
  gcMARK(d->act);
  gcMARK(d->post);

 size:
   gcBYTES_TO_WORDS(sizeof(Dyn_Wind));
}

mark_cont_mark_chain {
 mark:
  Scheme_Cont_Mark_Chain *c = (Scheme_Cont_Mark_Chain *)p;
  
  gcMARK(c->key);
  gcMARK(c->val);
  gcMARK(c->next);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Cont_Mark_Chain));
}

END fun;

/**********************************************************************/

START portfun;

mark_breakable {
 mark:
  Breakable *b = (Breakable *)p;
    
  gcMARK(b->config);
  gcMARK(b->orig_param_val);
  gcMARK(b->argv);

 size:
  gcBYTES_TO_WORDS(sizeof(Breakable));
}

mark_load_handler_data {
 mark:
  LoadHandlerData *d = (LoadHandlerData *)p;
    
  gcMARK(d->config);
  gcMARK(d->port);
  gcMARK(d->p);
  gcMARK(d->stxsrc);

 size:
  gcBYTES_TO_WORDS(sizeof(LoadHandlerData));
}

mark_load_data {
 mark:
  LoadData *d = (LoadData *)p;
  
  gcMARK(d->filename);
  gcMARK(d->config);
  gcMARK(d->load_dir);
  gcMARK(d->old_load_dir);

 size:
  gcBYTES_TO_WORDS(sizeof(LoadData));
}

mark_indexed_string {
 mark:
  Scheme_Indexed_String *is = (Scheme_Indexed_String *)p;
    
  gcMARK(is->string);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Indexed_String));
}

mark_pipe {
 mark:
  Scheme_Pipe *pp = (Scheme_Pipe *)p;
    
  gcMARK(pp->buf);
  gcMARK(pp->wakeup_on_read);
#ifdef MZ_REAL_THREADS
  gcMARK(pp->wait_sem);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Pipe));
}

END portfun;

/**********************************************************************/

START port;

#ifdef WINDOWS_PROCESSES
mark_thread_memory {
 mark:
  Scheme_Thread_Memory *tm = (Scheme_Thread_Memory *)p;
  gcMARK(tm->prev);
  gcMARK(tm->next);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Thread_Memory));
}
#endif

mark_input_file {
 mark:
  Scheme_Input_File *i = (Scheme_Input_File *)p;

  gcMARK(i->f);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Input_File));
}

#if defined(WIN32_FD_HANDLES) || defined(USE_BEOS_PORT_THREADS)
mark_tested_input_file {
 mark:
  Tested_Input_File *tip = (Tested_Input_File *)p;
  
  gcMARK(tip->fp);
#ifdef WIN32_FD_HANDLES
  gcMARK(tip->thread_memory);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Tested_Input_File));
}

mark_tcp_select_info {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(Tcp_Select_Info));
}
#endif

mark_output_file {
 mark:
  Scheme_Output_File *o = (Scheme_Output_File *)p;

  gcMARK(o->f);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Output_File));
}

#ifdef USING_TESTED_OUTPUT_FILE
mark_tested_output_file {
 mark:
  Tested_Output_File *top = (Tested_Output_File *)p;
  
  gcMARK(top->fp);
#ifdef WIN32_FD_HANDLES
  gcMARK(top->thread_memory);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(Tested_Output_File));
}
#endif

#ifdef USE_FD_PORTS
mark_input_fd {
 mark:
  Scheme_FD *fd = (Scheme_FD *)p;

  gcMARK(fd->buffer);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_FD));
}
#endif

#if defined(UNIX_PROCESSES)
mark_system_child {
 mark:
  System_Child *sc = (System_Child *)p;

  gcMARK(sc->next);

 size:
  gcBYTES_TO_WORDS(sizeof(System_Child));
}
#endif

#ifdef BEOS_PROCESSES
mark_beos_process {
 mark:
 size:
  gcBYTES_TO_WORDS(sizeof(BeOSProcess));
}
#endif

#ifdef USE_OSKIT_CONSOLE
mark_oskit_console_input {
 mark:
  osk_console_input *c = (osk_console_input *)p;
    
  gcMARK(c->buffer);
  gcMARK(c->next);

 size:
  gcBYTES_TO_WORDS(sizeof(osk_console_input));
}
#endif

END port;

/**********************************************************************/

START network;

mark_listener {
 mark:
  listener_t *l = (listener_t *)p;

  gcMARK(l->mref);
#ifdef USE_MAC_TCP
  gcMARK(l->datas);
#endif

 size:
  gcBYTES_TO_WORDS(sizeof(listener_t));
}

#ifdef USE_TCP
mark_tcp {
 mark:
  Scheme_Tcp *tcp = (Scheme_Tcp *)p;

  gcMARK(tcp->b.buffer);
# ifdef USE_MAC_TCP
  gcMARK(tcp->tcp);
  gcMARK(tcp->activeRcv);
# endif

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Tcp));
}

# ifdef USE_MAC_TCP
mark_write_data {
 mark:
  WriteData *d = (WriteData *)p;
    
  gcMARK(d->xpb);

 size:
  gcBYTES_TO_WORDS(sizeof(WriteData));
}
# endif
#endif

END network;

/**********************************************************************/

START process;

mark_config_val {
 mark:
  Scheme_Config *c = (Scheme_Config *)p;
  int i;
    
  for (i = max_configs; i--; ) {
    gcMARK(c->configs[i]);
  }
  gcMARK(c->extensions);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Config)
		    + ((max_configs - 1) * sizeof(Scheme_Object*))));
}

mark_will_executor_val {
 mark:
  WillExecutor *e = (WillExecutor *)p;
  
  gcMARK(e->sema);
  gcMARK(e->first);
  gcMARK(e->last);

 size:
  gcBYTES_TO_WORDS(sizeof(WillExecutor));
}

mark_manager_val {
 mark:
  Scheme_Manager *m = (Scheme_Manager *)p;
  
  gcMARK(m->boxes);
  gcMARK(m->mrefs);
  gcMARK(m->closers);
  gcMARK(m->data);
  
  gcMARK(m->parent);
  gcMARK(m->sibling);
  gcMARK(m->children);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Manager));
}

mark_process_hop {
 mark:
  Scheme_Process_Manager_Hop *hop = (Scheme_Process_Manager_Hop *)p;

  gcMARK(hop->p);

 size:
   gcBYTES_TO_WORDS(sizeof(Scheme_Process_Manager_Hop));
}

mark_namespace_option {
 mark:
  Scheme_NSO *o = (Scheme_NSO *)p;

  gcMARK(o->key);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_NSO));
}

mark_param_data {
 mark:
  ParamData *d = (ParamData *)p;

  gcMARK(d->key);
  gcMARK(d->guard);
  gcMARK(d->defval);

 size:
   gcBYTES_TO_WORDS(sizeof(ParamData));
}

mark_will {
 mark:
  ActiveWill *w = (ActiveWill *)p;
  
  gcMARK(w->o);
  gcMARK(w->proc);
  gcMARK(w->w);
  gcMARK(w->next);

 size:
  gcBYTES_TO_WORDS(sizeof(ActiveWill));
}

mark_will_registration {
 mark:
  WillRegistration *r = (WillRegistration *)p;
 
  gcMARK(r->proc);
  gcMARK(r->w);

 size:
  gcBYTES_TO_WORDS(sizeof(WillRegistration));
}

END process;

/**********************************************************************/

START salloc;

mark_finalization {
 mark:
  Finalization *f = (Finalization *)p;
  
  gcMARK(f->data);
  gcMARK(f->next);
  gcMARK(f->prev);

 size:
  gcBYTES_TO_WORDS(sizeof(Finalization));
}

mark_finalizations {
 mark:
  Finalizations *f = (Finalizations *)p;

  gcMARK(f->scheme_first);
  gcMARK(f->scheme_last);
  gcMARK(f->prim_first);
  gcMARK(f->prim_last);
  gcMARK(f->ext_data);

 size:
  gcBYTES_TO_WORDS(sizeof(Finalizations));
}

END salloc;

/**********************************************************************/

START sema;

mark_breakable_wait {
 mark:
  BreakableWait *w = (BreakableWait *)p;
    
  gcMARK(w->config);
  gcMARK(w->orig_param_val);
  gcMARK(w->sema);

 size:
  gcBYTES_TO_WORDS(sizeof(BreakableWait));
}

mark_sema_waiter {
 mark:
  Scheme_Sema_Waiter *w = (Scheme_Sema_Waiter *)p;

  gcMARK(w->p);
  gcMARK(w->prev);
  gcMARK(w->next);

 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Sema_Waiter));
}

END sema;

/**********************************************************************/

START struct;

mark_struct_val {
  Scheme_Structure *s = (Scheme_Structure *)p;
  Scheme_Struct_Type *stype = (Scheme_Struct_Type *)GC_resolve(s->stype);

 mark:
  int i;

  gcFIXUP_TYPED_NOW(Scheme_Struct_Type *, s->stype);
  stype = s->stype; /* In case we just moved it */

  for(i = stype->num_slots; i--; )
    gcMARK(s->slots[i]);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Structure) 
		    + ((stype->num_slots - 1) * sizeof(Scheme_Object *))));
}

mark_struct_type_val {
  Scheme_Struct_Type *t = (Scheme_Struct_Type *)p;

 mark:
  int i;
  for (i = t->name_pos + 1; i--; ) {
    gcMARK(t->parent_types[i]);
  }
  gcMARK(t->type_name);
  gcMARK(t->inspector);

 size:
  gcBYTES_TO_WORDS((sizeof(Scheme_Struct_Type)
		    + (t->name_pos * sizeof(Scheme_Struct_Type *))));
}

mark_struct_info_val {
 mark:
  Struct_Info *i = (Struct_Info *)p;

  gcMARK(i->name);
  gcMARK(i->fields);
  gcMARK(i->parent_type_expr);
  gcMARK(i->memo_names);

 size:
  gcBYTES_TO_WORDS(sizeof(Struct_Info));
}

mark_struct_proc_info {
 mark:
  Struct_Proc_Info *i = (Struct_Proc_Info *)p;

  gcMARK(i->struct_type);
  gcMARK(i->func_name);

 size:
  gcBYTES_TO_WORDS(sizeof(Struct_Proc_Info));
}

mark_inspector {
 mark:
  Scheme_Inspector *i = (Scheme_Inspector *)p;
  gcMARK(i->superior);
 size:
  gcBYTES_TO_WORDS(sizeof(Scheme_Inspector));
}

END struct;

/**********************************************************************/

START syntax;

END syntax;

/**********************************************************************/

START regexp;

mark_regexp {
  regexp *r = (regexp *)p;
 mark:
 size:
  gcBYTES_TO_WORDS((sizeof(regexp) + r->regsize));
}

END regexp;

/**********************************************************************/

START stxobj;

mark_rename_table {
 mark:
  Module_Renames *rn = (Module_Renames *)p;
  gcMARK(rn->ht);
 size:
  gcBYTES_TO_WORDS(sizeof(Module_Renames));
}

END stxobj;

/**********************************************************************/

#define GC_REG_TRAV(type, base) GC_register_traversers(type, base ## _SIZE, base ## _MARK, base ## _FIXUP)
