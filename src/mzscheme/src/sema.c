/*
  MzScheme
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
*/

#include "schpriv.h"

#ifndef NO_SCHEME_THREADS

static Scheme_Object *make_sema(int n, Scheme_Object **p);
static Scheme_Object *semap(int n, Scheme_Object **p);
static Scheme_Object *hit_sema(int n, Scheme_Object **p);
static Scheme_Object *block_sema_p(int n, Scheme_Object **p);
static Scheme_Object *block_sema(int n, Scheme_Object **p);
static Scheme_Object *block_sema_breakable(int n, Scheme_Object **p);
static Scheme_Object *make_sema_repost(int n, Scheme_Object **p);

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/* For object-wait: */
static Scheme_Object *sema_identity(Scheme_Object *s, int *repost)
{
  *repost = 0;
  return s;
}

static Scheme_Object *sema_for_repost(Scheme_Object *s, int *repost)
{
  *repost = 1;
  return SCHEME_PTR_VAL(s);
}

void scheme_init_sema(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

  scheme_add_global_constant("make-semaphore", 
			     scheme_make_prim_w_arity(make_sema,
						      "make-semaphore", 
						      0, 1), 
			     env);
  scheme_add_global_constant("semaphore?", 
			     scheme_make_folding_prim(semap,
						      "semaphore?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("semaphore-post", 
			     scheme_make_prim_w_arity(hit_sema, 
						      "semaphore-post", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-try-wait?", 
			     scheme_make_prim_w_arity(block_sema_p, 
						      "semaphore-try-wait?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-wait", 
			     scheme_make_prim_w_arity(block_sema, 
						      "semaphore-wait", 
						      1, 1), 
			     env);
  scheme_add_global_constant("semaphore-wait/enable-break", 
			     scheme_make_prim_w_arity(block_sema_breakable, 
						      "semaphore-wait/enable-break", 
						      1, 1), 
			     env);

  scheme_add_global_constant("make-semaphore-peek", 
			     scheme_make_prim_w_arity(make_sema_repost,
						      "make-semaphore-peek", 
						      1, 1), 
			     env);

  scheme_add_waitable_through_sema(scheme_sema_type, sema_identity, NULL);
  scheme_add_waitable_through_sema(scheme_semaphore_repost_type, sema_for_repost, NULL);
}

Scheme_Object *scheme_make_sema(long v)
{
  Scheme_Sema *sema;

  sema = MALLOC_ONE_TAGGED(Scheme_Sema);
  sema->value = v;

  sema->type = scheme_sema_type;

  return (Scheme_Object *)sema;
}

static Scheme_Object *make_sema(int n, Scheme_Object **p)
{
  long v;

  if (n) {
    if (!SCHEME_INTP(p[0])) {
      if (!SCHEME_BIGNUMP(p[0]) || !SCHEME_BIGPOS(p[0]))
	scheme_wrong_type("make-semaphore", "non-negative exact integer", 0, n, p);
    }

    if (!scheme_get_int_val(p[0], &v)) {
      scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		       "make-semaphore: starting value %s is too large",
		       scheme_make_provided_string(p[0], 0, NULL));
    } else if (v < 0)
      scheme_wrong_type("make-semaphore", "non-negative exact integer", 0, n, p);
  } else
    v = 0;

  return scheme_make_sema(v);
}

static Scheme_Object *make_sema_repost(int n, Scheme_Object **p)
{
  Scheme_Object *o;

  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("make-semaphore-peek", "semaphore", 0, n, p);
  
  o = scheme_alloc_small_object();
  o->type = scheme_semaphore_repost_type;
  SCHEME_PTR_VAL(o) = p[0];

  return o;
}

static Scheme_Object *semap(int n, Scheme_Object **p)
{
  return SCHEME_SEMAP(p[0]) ? scheme_true : scheme_false;
}

void scheme_post_sema(Scheme_Object *o)
{
  Scheme_Sema *t = (Scheme_Sema *)o;

  int v;

  if (t->value < 0) return;

  v = t->value + 1;
  if (v > t->value) {
    t->value = v;

    if (t->first) {
      Scheme_Sema_Waiter *w;

      w = t->first;

      t->first = w->next;
      if (!w->next)
	t->last = NULL;
      else
	t->first->prev = NULL;
      
      w->in_line = 0;
      w->prev = NULL;
      w->next = NULL;
      scheme_weak_resume_thread(w->p);
    }

    return;
  }

  scheme_raise_exn(MZEXN_MISC,
		   "semaphore-post: the maximum post count has already been reached");
}

void scheme_post_sema_all(Scheme_Object *o)
{
  Scheme_Sema *t = (Scheme_Sema *)o;

  while (t->first) {
    scheme_post_sema(o);
  }
  t->value = -1;
}

static Scheme_Object *hit_sema(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-post", "semaphore", 0, n, p);

  scheme_post_sema(p[0]);

  return scheme_void;
}

typedef struct {
  MZTAG_IF_REQUIRED
  Scheme_Config *config;
  Scheme_Object *orig_param_val;
  Scheme_Object *sema;
} BreakableWait;

static void pre_breakable_wait(void *data)
{
  BreakableWait *bw = (BreakableWait *)data;

  bw->orig_param_val = scheme_get_param(bw->config, MZCONFIG_ENABLE_BREAK);
  scheme_set_param(bw->config, MZCONFIG_ENABLE_BREAK, scheme_true);
}

static Scheme_Object *do_breakable_wait(void *data)
{
  BreakableWait *bw = (BreakableWait *)data;

  /* Need to check for a break, in case one was queued and we just enabled it: */
  {
    Scheme_Thread *p = scheme_current_thread;
    if (p->external_break) {
      if (scheme_can_break(p, p->config)) {
	scheme_thread_block_w_thread(0.0, p);
	p->ran_some = 1;
      }
    }
  }

  scheme_wait_sema(bw->sema, 0);
  return scheme_void;
}

static void post_breakable_wait(void *data)
{
  BreakableWait *bw = (BreakableWait *)data;
  scheme_set_param(bw->config, MZCONFIG_ENABLE_BREAK, bw->orig_param_val);
}

static int out_of_line(Scheme_Object *a)
{
  Scheme_Thread *p;
  int n, i;
  Scheme_Sema_Waiter *w;

  /* Out of one line? */
  n = SCHEME_INT_VAL(((Scheme_Object **)a)[0]);
  for (i = 0; i < n; i++) {
    w = (((Scheme_Sema_Waiter ***)a)[1])[i];
    if (!w->in_line)
      return 1;
  }

  /* Suspended break? */
  p = ((Scheme_Thread **)a)[2];
  if (p->external_break) {
    int v;
    p->suspend_break = 0;
    v = scheme_can_break(p, p->config);
    p->suspend_break = 1;
    if (v)
      return 1; 
  }

  return 0;
}

static void get_into_line(Scheme_Sema *sema, Scheme_Sema_Waiter *w)
{
  w->in_line = 1;
  w->prev = sema->last;
  if (sema->last)
    sema->last->next = w;
  else
    sema->first = w;
  sema->last = w;
  w->next = NULL;
}

static void get_outof_line(Scheme_Sema *sema, Scheme_Sema_Waiter *w)
{
  w->in_line = 0;
  if (w->prev)
    w->prev->next = w->next;
  else
    sema->first = w->next;
  if (w->next)
    w->next->prev = w->prev;
  else
    sema->last = w->prev;
}

int scheme_wait_semas(int n, Scheme_Object **o, int just_try)
{
  Scheme_Sema **semas = (Scheme_Sema **)o;
  int v, i;

  if (just_try) {
    /* assert: n == 1 */
    Scheme_Sema *sema = semas[0];
    if (just_try > 0) {
      if (sema->value) {
	if (sema->value > 0)
	  --sema->value;
	v = 1;
      } else
	v = 0;
    } else {
      BreakableWait *bw;
      bw = MALLOC_ONE_RT(BreakableWait);

#ifdef MZTAG_REQUIRED
      bw->type = scheme_rt_breakable_wait;
#endif
      bw->sema = (Scheme_Object *)sema;
      bw->config = scheme_config;

      scheme_dynamic_wind(pre_breakable_wait, 
			  do_breakable_wait, 
			  post_breakable_wait, 
			  NULL, bw);

      return 1;
    }
  } else {
    for (i = 0; i < n; i++) {
      if (semas[i]->value) {
	if (semas[i]->value > 0)
	  --semas[i]->value;
	break;
      }
    }

    if (i >= n) {
      Scheme_Sema_Waiter **ws, *w;

      ws = MALLOC_N(Scheme_Sema_Waiter*, n);
      for (i = 0; i < n; i++) {
	w = MALLOC_ONE_RT(Scheme_Sema_Waiter);
	ws[i] = w;
#ifdef MZTAG_REQUIRED
	w->type = scheme_rt_sema_waiter;
#endif
	w->p = scheme_current_thread;
      }
      
      while (1) {
	int out_of_a_line;

	/* Get into line */
	for (i = 0; i < n; i++) {
	  if (!ws[i]->in_line) {
	    get_into_line(semas[i], ws[i]);
	  }
	}

	if (!scheme_current_thread->next) {
	  void **a;
	  a = MALLOC_N(void*, 3);
	  /* We're not allowed to suspend the main thread. Delay
	     breaks so we get a chance to clean up. */
	  scheme_current_thread->suspend_break = 1;
	  a[0] = scheme_make_integer(n);
	  a[1] = ws;
	  a[2] = scheme_current_thread;
	  
	  scheme_block_until(out_of_line, NULL, (Scheme_Object *)a, (float)0.0);
	  scheme_current_thread->suspend_break = 0;
	} else {
	  /* Mark the thread to indicate that we need to clean up
	     if the thread is killed. */
	  scheme_current_thread->running |= MZTHREAD_NEED_KILL_CLEANUP;
	  scheme_weak_suspend_thread(scheme_current_thread);
	  scheme_current_thread->running -= MZTHREAD_NEED_KILL_CLEANUP;
	}

	/* We've been resumed. But was it for the semaphore, or a signal? */
	out_of_a_line = 0;
	
	for (i = 0; i < n; i++) {
	  if (!ws[i]->in_line) {
	    out_of_a_line = 1;
	    if (semas[i]->value) {
	      if (semas[i]->value > 0)
		--(semas[i]->value);
	      /* If we get the post, we must return WITHOUT BLOCKING. 
		 MrEd, for example, depends on this special property, which insures
		 that the thread can't be broken or killed between
		 receiving the post and returning. */

	      break;
	    }
	    /* otherwise, someone stole the post, so we keep looking */
	  }
	}

	if (!out_of_a_line) {
	  /* We weren't woken by any semaphore. Get out of line, block once 
	     (to handle breaks/kills) and then loop to get back into line. */
	  for (i = 0; i < n; i++) {
	    get_outof_line(semas[i], ws[i]);
	  } 
	  
	  scheme_thread_block(0); /* ok if it returns multiple times */ 
	  /* [but why would it return multiple times?! there must have been a reason...] */
	}

	if (scheme_current_thread->running & MZTHREAD_KILLED) {
	  /* We've been killed! Disown the semaphore post, then die by
	     calling scheme_thread_block. */
	  i = -1;
	}

	/* We got a post from semas[i]. Did any later semaphore pick us? */
	{
	  int j;

	  for (j = i+1; j < n; j++) {
	    if (!ws[j]->in_line) {
	      if (semas[j]->value) {
		/* Consume the value and repost, because no one else
		   has been told to go, and we're accepting a different post. */
		if (semas[j]->value > 0)
		  --semas[j]->value;
		scheme_post_sema((Scheme_Object *)(semas[j]));
	      }
	    }
	  }
	}

	/* If we're done, get out of all lines that we're still in. */
	if (i < n) {
	  int j;
	  for (j = 0; j < n; j++) {
	    if (ws[j]->in_line)
	      get_outof_line(semas[j], ws[j]);
	  }
	}

	if (i == -1) {
	  scheme_thread_block(0); /* dies */
	}

	if (i < n)
	  break;

	/* Otherwise: someone stole the post! Loop to get back in line an try again. */
      }
    }
    v = i + 1;
  }

  return v;
}

int scheme_wait_sema(Scheme_Object *o, int just_try)
{
  Scheme_Object *a[1];

  a[0] = o;

  return scheme_wait_semas(1, a, just_try);
}

static Scheme_Object *block_sema_p(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-try-wait?", "sema", 0, n, p);

  return scheme_wait_sema(p[0], 1) ? scheme_true : scheme_false;
}

static Scheme_Object *block_sema(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-wait", "sema", 0, n, p);

  scheme_wait_sema(p[0], 0);

  return scheme_void;
}

static Scheme_Object *block_sema_breakable(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-wait/enable-break", "sema", 0, n, p);

  scheme_wait_sema(p[0], -1);

  return scheme_void;
}

/**********************************************************************/
/*                           Precise GC                               */
/**********************************************************************/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_SEMA_C
#include "mzmark.c"

static void register_traversers(void)
{
  GC_REG_TRAV(scheme_rt_breakable_wait, mark_breakable_wait);
  GC_REG_TRAV(scheme_rt_sema_waiter, mark_sema_waiter);
}

END_XFORM_SKIP;

#endif

#endif /* NO_SCHEME_THREADS */
