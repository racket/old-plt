
/* We have two experimental GC implementations right now: a simple
   two-space copying collector, and a compacting collector. */

#define USE_COPYING 0

#if USE_COPYING
# include "copy.c"
#else
# include "compact.c"
#endif
