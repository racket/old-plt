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

static Scheme_Object *make_channel(int n, Scheme_Object **p);
static Scheme_Object *make_channel_put(int n, Scheme_Object **p);
static Scheme_Object *channel_p(int n, Scheme_Object **p);

static int channel_get_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo);
static int channel_put_ready(Scheme_Object *ch);

static int pending_break(Scheme_Thread *p);

int scheme_main_was_once_suspended;

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

/* For object-wait: */
static int sema_ready(Scheme_Object *s)
{
  return scheme_wait_sema(s, 1);
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

  scheme_add_global_constant("make-channel", 
			     scheme_make_prim_w_arity(make_channel,
						      "make-channel",
						      0, 0), 
			     env);
  scheme_add_global_constant("make-channel-put-waitable", 
			     scheme_make_prim_w_arity(make_channel_put,
						      "make-channel-put-waitable",
						      2, 2), 
			     env);
  scheme_add_global_constant("channel?", 
			     scheme_make_folding_prim(channel_p,
						      "channel?",
						      1, 1, 1), 
			     env);  

  scheme_add_waitable(scheme_sema_type, sema_ready, NULL, NULL, 0);
  scheme_add_waitable_through_sema(scheme_semaphore_repost_type, sema_for_repost, NULL);
  scheme_add_waitable(scheme_channel_type, (Scheme_Ready_Fun)channel_get_ready, NULL, NULL, 1);
  scheme_add_waitable(scheme_channel_put_type, channel_put_ready, NULL, NULL, 0);
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
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("make-semaphore-peek", "semaphore", 0, n, p);
 
  return scheme_make_sema_repost(p[0]);
}
 
Scheme_Object *scheme_make_sema_repost(Scheme_Object *sema)
{
  Scheme_Object *o;

  o = scheme_alloc_small_object();
  o->type = scheme_semaphore_repost_type;
  SCHEME_PTR_VAL(o) = sema;

  return o;
}

static Scheme_Object *semap(int n, Scheme_Object **p)
{
  return SCHEME_SEMAP(p[0]) ? scheme_true : scheme_false;
}

void scheme_post_sema(Scheme_Object *o)
{
  Scheme_Sema *t = (Scheme_Sema *)o;
  int v, consumed;

  if (t->value < 0) return;

  v = t->value + 1;
  if (v > t->value) {
    t->value = v;

    while (t->first) {
      Scheme_Sema_Waiter *w;

      w = t->first;

      t->first = w->next;
      if (!w->next)
	t->last = NULL;
      else
	t->first->prev = NULL;
      
      if ((!w->waiting || !w->waiting->result) && !pending_break(w->p)) {
	if (w->waiting) {
	  w->waiting->result = w->waiting_i + 1;
	  if (w->waiting->disable_break)
	    scheme_set_param(w->waiting->disable_break->config, MZCONFIG_ENABLE_BREAK, scheme_false);
	  if (!w->waiting->reposts || !w->waiting->reposts[w->waiting_i]) {
	    t->value -= 1;
	    consumed = 1;
	  } else
	    consumed = 0;
	} else {
	  /* In this case, we will remove the waiter from line, but
	     someone else might grab the post. This is unfair, but it
	     can help improve throughput when multiple threads synchronize
	     on a lock. */
	  consumed = 1;
	}
	w->picked = 1;
      } else
	consumed = 0;

      w->in_line = 0;
      w->prev = NULL;
      w->next = NULL;

      if (w->picked) {
	scheme_weak_resume_thread(w->p);
	if (consumed)
	  break;
      }
      /* otherwise, loop to find one we can wake up */
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
  scheme_check_break_now();

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
    if (w->picked)
      return 1;
  }

  /* Suspended break? */
  p = ((Scheme_Thread **)a)[2];
  if (p->external_break) {
    int v;
    --p->suspend_break;
    v = scheme_can_break(p, p->config);
    p->suspend_break++;
    if (v)
      return 1; 
  }

  /* Suspended by user? */
  if ((p->running & MZTHREAD_USER_SUSPENDED)
      || scheme_main_was_once_suspended)
    return 1;

  return 0;
}

static void get_into_line(Scheme_Sema *sema, Scheme_Sema_Waiter *w)
{
  Scheme_Sema_Waiter *last, *first;
  
  if (SCHEME_SEMAP(sema)) {
    last = sema->last;
    first = sema->first;
  } else if (SCHEME_CHANNELP(sema)) {
    last = ((Scheme_Channel *)sema)->get_last;
    first = ((Scheme_Channel *)sema)->get_first;
  } else {
    last = ((Scheme_Channel_Put *)sema)->ch->put_last;
    first = ((Scheme_Channel_Put *)sema)->ch->put_first;
  }

  w->in_line = 1;
  w->picked = 0;
  w->prev = last;
  if (last)
    last->next = w;
  else
    first = w;
  last = w;
  w->next = NULL;

  if (SCHEME_SEMAP(sema)) {
    sema->last = last;
    sema->first = first;
  } else if (SCHEME_CHANNELP(sema)) {
    ((Scheme_Channel *)sema)->get_last = last;
    ((Scheme_Channel *)sema)->get_first = first;
  } else {
    ((Scheme_Channel_Put *)sema)->ch->put_last = last;
    ((Scheme_Channel_Put *)sema)->ch->put_first = first;
  }
}

static void get_outof_line(Scheme_Sema *sema, Scheme_Sema_Waiter *w)
{
  Scheme_Sema_Waiter *last, *first;

  if (SCHEME_SEMAP(sema)) {
    last = sema->last;
    first = sema->first;
  } else if (SCHEME_CHANNELP(sema)) {
    last = ((Scheme_Channel *)sema)->get_last;
    first = ((Scheme_Channel *)sema)->get_first;
  } else {
    last = ((Scheme_Channel_Put *)sema)->ch->put_last;
    first = ((Scheme_Channel_Put *)sema)->ch->put_first;
  }

  w->in_line = 0;
  if (w->prev)
    w->prev->next = w->next;
  else
    first = w->next;
  if (w->next)
    w->next->prev = w->prev;
  else
    last = w->prev;

  if (SCHEME_SEMAP(sema)) {
    sema->last = last;
    sema->first = first;
  } else if (SCHEME_CHANNELP(sema)) {
    ((Scheme_Channel *)sema)->get_last = last;
    ((Scheme_Channel *)sema)->get_first = first;
  } else {
    ((Scheme_Channel_Put *)sema)->ch->put_last = last;
    ((Scheme_Channel_Put *)sema)->ch->put_first = first;
  }
}

static int try_channel(Scheme_Sema *sema, Waiting *waiting, int pos, Scheme_Object **result)
{
  if (SCHEME_CHANNELP(sema)) {
    Scheme_Channel *ch = (Scheme_Channel *)sema;
    Scheme_Sema_Waiter *w = ch->put_first, *next;
    int picked = 0;

    while (w) {
      if (w->waiting == waiting) {
	/* can't synchronize with self */
	w = w->next;
      } else {
	Scheme_Channel_Put *chp = (Scheme_Channel_Put *)w->waiting->set->argv[w->waiting_i];
	
	if (!w->waiting->result && !pending_break(w->p)) {
	  w->picked = 1;
	  w->waiting->result = w->waiting_i + 1;
	  if (w->waiting->disable_break)
	    scheme_set_param(w->waiting->disable_break->config, MZCONFIG_ENABLE_BREAK, scheme_false);
	  if (result)
	    *result = chp->val;
	  if (waiting) {
	    waiting->result = pos + 1;
	    if (waiting->disable_break)
	      scheme_set_param(waiting->disable_break->config, MZCONFIG_ENABLE_BREAK, scheme_false);
	    waiting->set->argv[pos] = chp->val;
	  }
	  picked = 1;
	  scheme_weak_resume_thread(w->p);
	}
	
	next = w->next;
	get_outof_line((Scheme_Sema *)chp, w);
	w = next;
	
	if (picked)
	  return 1;
      }
    }

    return 0;
  } else {
    Scheme_Channel_Put *chp = (Scheme_Channel_Put *)sema;
    Scheme_Sema_Waiter *w = chp->ch->get_first, *next;
    int picked = 0;

    while (w) {
      if (w->waiting == waiting) {
	/* can't synchronize with self */
	w = w->next;
      } else {
	Scheme_Channel *ch = (Scheme_Channel *)w->waiting->set->argv[w->waiting_i];
	
	if (!w->waiting->result && !pending_break(w->p)) {
	  w->picked = 1;
	  w->waiting->set->argv[w->waiting_i] = chp->val;
	  w->waiting->result = w->waiting_i + 1;
	  if (w->waiting->disable_break)
	    scheme_set_param(w->waiting->disable_break->config, MZCONFIG_ENABLE_BREAK, scheme_false);
	  if (waiting) {
	    waiting->result = pos + 1;
	    if (waiting->disable_break)
	      scheme_set_param(waiting->disable_break->config, MZCONFIG_ENABLE_BREAK, scheme_false);
	  }
	  picked = 1;
	  scheme_weak_resume_thread(w->p);
	}
	
	next = w->next;
	get_outof_line((Scheme_Sema *)ch, w);
	w = next;
	
	if (picked)
	  return 1;
      }
    }

    return 0;    
  }
}

int scheme_wait_semas_chs(int n, Scheme_Object **o, int just_try, Waiting *waiting)
{
  Scheme_Sema **semas = (Scheme_Sema **)o;
  int v, i, ii;

  if (just_try) {
    /* assert: n == 1, !waiting */
    Scheme_Sema *sema = semas[0];
    if (just_try > 0) {
      if (sema->type == scheme_sema_type) {
	if (sema->value) {
	  if (sema->value > 0)
	    --sema->value;
	  v = 1;
	} else
	  v = 0;
      } else {
	v = try_channel(sema, waiting, 0, NULL);
      }
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
    int start_pos;

    if (n > 1) {
      if (waiting)
	start_pos = waiting->start_pos;
      else
	start_pos = scheme_rand((Scheme_Random_State *)scheme_get_param(scheme_config, MZCONFIG_SCHEDULER_RANDOM_STATE));
    } else
      start_pos = 0;

    /* Initial poll */
    i = 0;
    for (ii = 0; ii < n; ii++) {
      /* Randomized start position for poll ensures fairness: */
      i = (start_pos + ii) % n;

      if (semas[i]->type == scheme_sema_type) {
	if (semas[i]->value) {
	  if ((semas[i]->value > 0) && (!waiting || !waiting->reposts || !waiting->reposts[i]))
	    --semas[i]->value;
	  break;
	}
      } else if (try_channel(semas[i], waiting, i, NULL))
	break;
    }

    if (ii >= n) {
      Scheme_Sema_Waiter **ws, *w;

      ws = MALLOC_N(Scheme_Sema_Waiter*, n);
      for (i = 0; i < n; i++) {
	w = MALLOC_ONE_RT(Scheme_Sema_Waiter);
	ws[i] = w;
#ifdef MZTAG_REQUIRED
	w->type = scheme_rt_sema_waiter;
#endif
	w->p = scheme_current_thread;
	w->waiting = waiting;
	w->waiting_i = i;
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

	  /* We're not allowed to suspend the main thread. Delay
	     breaks so we get a chance to clean up. */
	  scheme_current_thread->suspend_break++;

	  a = MALLOC_N(void*, 3);
	  a[0] = scheme_make_integer(n);
	  a[1] = ws;
	  a[2] = scheme_current_thread;
	  
	  scheme_main_was_once_suspended = 0;

	  scheme_block_until(out_of_line, NULL, (Scheme_Object *)a, (float)0.0);
	  
	  --scheme_current_thread->suspend_break;
	} else {
	  /* Mark the thread to indicate that we need to clean up
	     if the thread is killed. */
	  scheme_current_thread->running += MZTHREAD_NEED_KILL_CLEANUP;
	  scheme_weak_suspend_thread(scheme_current_thread);
	  if (scheme_current_thread->running & MZTHREAD_NEED_KILL_CLEANUP)
	    scheme_current_thread->running -= MZTHREAD_NEED_KILL_CLEANUP;
	}

	/* We've been resumed. But was it for the semaphore, or a signal? */
	out_of_a_line = 0;
	
	/* If we get the post, we must return WITHOUT BLOCKING. 
	   MrEd, for example, depends on this special property, which ensures
	   that the thread can't be broken or killed between
	   receiving the post and returning. */

	if (!waiting) {
	  /* Poster can't be sure that we really will get it,
	     so we have to decrement the sema count here. */
	  i = 0;
	  for (ii = 0; ii < n; ii++) {
	    i = (start_pos + ii) % n;
	    if (ws[i]->picked) {
	      out_of_a_line = 1;
	      if (semas[i]->value) {
		if (semas[i]->value > 0)
		  --(semas[i]->value);
		break;
	      }
	    }
	  }
	  if (ii >= n)
	    i = n;
	} else {
	  if (waiting->result) {
	    out_of_a_line = 1;
	    i = waiting->result - 1;
	  } else {
	    out_of_a_line = 0;
	    i = n;
	  }
	}

	if (!out_of_a_line) {
	  /* We weren't woken by any semaphore/channel. Get out of line, block once 
	     (to handle breaks/kills) and then loop to get back into line. */
	  for (i = 0; i < n; i++) {
	    if (ws[i]->in_line)
	      get_outof_line(semas[i], ws[i]);
	  }
	  
	  scheme_thread_block(0); /* ok if it returns multiple times */ 
	  scheme_current_thread->ran_some = 1;
	  /* [but why would it return multiple times?! there must have been a reason...] */
	} else {

	  if ((scheme_current_thread->running & MZTHREAD_KILLED)
	      || (scheme_current_thread->running & MZTHREAD_USER_SUSPENDED)) {
	    /* We've been killed or suspended! */
	    i = -1;
	  }

	  /* We got a post from semas[i], or we were killed. 
	     Did any (other) semaphore pick us?
	     (This only happens when waiting == NULL.) */
	  if (!waiting) {
	    int j;

	    for (j = 0; j < n; j++) {
	      if (j != i) {
		if (ws[j]->picked) {
		  if (semas[j]->value) {
		    /* Consume the value and repost, because no one else
		       has been told to go, and we're accepting a different post. */
		    if (semas[j]->value > 0)
		      --semas[j]->value;
		    scheme_post_sema((Scheme_Object *)semas[j]);
		  }
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
	    scheme_thread_block(0); /* dies or suspends */
	    scheme_current_thread->ran_some = 1;
	  }

	  if (i < n)
	    break;
	}

	/* Otherwise: !waiting and someone stole the post, or we were
	   suspended and we have to start over. Either way, poll then
	   loop to get back in line an try again. */
	for (ii = 0; ii < n; ii++) {
	  i = (start_pos + ii) % n;

	  if (semas[i]->type == scheme_sema_type) {
	    if (semas[i]->value) {
	      if ((semas[i]->value > 0) && (!waiting || !waiting->reposts || !waiting->reposts[i]))
		--semas[i]->value;
	      break;
	    }
	  } else if (try_channel(semas[i], waiting, i, NULL))
	    break;
	}

	if (ii < n) {
	  /* Get out of any line that we still might be in: */
	  int j;
	  for (j = 0; j < n; j++) {
	    if (ws[j]->in_line)
	      get_outof_line(semas[j], ws[j]);
	  }

	  break;
	}

	if (!waiting) {
	  /* Looks like this thread is a victim of unfair semaphore access.
	     Go into fair mode by allocating a waiting: */
	  waiting = MALLOC_ONE_RT(Waiting);
#ifdef MZTAG_REQUIRED
	  waiting->type = scheme_rt_waiting;
#endif
	  waiting->start_pos = start_pos;

	  /* Get out of all lines, and set waiting field before we get back in line: */
	  {
	    int j;
	    for (j = 0; j < n; j++) {
	      if (ws[j]->in_line)
		get_outof_line(semas[j], ws[j]);
	      ws[j]->waiting = waiting;
	    }
	  }
	}
	/* Back to top of loop to wait again */
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

  return scheme_wait_semas_chs(1, a, just_try, NULL);
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

  /* In case a break appeared after wwe received the post,
     check for a break, because scheme_wait_sema() won't: */
  scheme_check_break_now();

  return scheme_void;
}

static Scheme_Object *block_sema_breakable(int n, Scheme_Object **p)
{
  if (!SCHEME_SEMAP(p[0]))
    scheme_wrong_type("semaphore-wait/enable-break", "sema", 0, n, p);

  scheme_wait_sema(p[0], -1);

  return scheme_void;
}

static int pending_break(Scheme_Thread *p)
{
  if (p->running & (MZTHREAD_KILLED | MZTHREAD_USER_SUSPENDED))
    return 1;

  if (p->external_break) {
    int v;

    if (!p->next) {
      /* if p is the main thread, it must have a suspension
	 to block on a channel or semaphore: */
      --p->suspend_break;
    }

    v = scheme_can_break(p, p->config);

    if (!p->next)
      p->suspend_break++;

    return v;
  }

  return 0;
}

/**********************************************************************/
/*                            Channels                                */
/**********************************************************************/

Scheme_Object *scheme_make_channel()
{
  Scheme_Channel *c;

  c = MALLOC_ONE_TAGGED(Scheme_Channel);
  c->type = scheme_channel_type;
  
  return (Scheme_Object *)c;
}

static Scheme_Object *make_channel(int n, Scheme_Object **p)
{
  return scheme_make_channel();
}

static Scheme_Object *make_channel_put(int argc, Scheme_Object **argv)
{
  Scheme_Channel_Put *cp;

  if (!SCHEME_CHANNELP(argv[0]))
    scheme_wrong_type("make-channel-put-waitable", "channel", 0, argc, argv);

  cp = MALLOC_ONE_TAGGED(Scheme_Channel_Put);
  cp->type = scheme_channel_put_type;
  cp->ch = (Scheme_Channel *)argv[0];
  cp->val = argv[1];

  return (Scheme_Object *)cp;
}

static Scheme_Object *channel_p(int n, Scheme_Object **p)
{
  return (SCHEME_CHANNELP(p[0])
	  ? scheme_true
	  : scheme_false);
}

static int channel_get_ready(Scheme_Object *ch, Scheme_Schedule_Info *sinfo)
{
  Scheme_Object *result;

  if (try_channel((Scheme_Sema *)ch, NULL, 0, &result)) {
    scheme_set_wait_target(sinfo, result, NULL, NULL, 0, 0);
    return 1;
  }

  return 0;
}

static int channel_put_ready(Scheme_Object *ch)
{
  if (try_channel((Scheme_Sema *)ch, NULL, 0, NULL))
    return 1;
  
  return 0;
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
