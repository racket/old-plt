
/* We have two experimental GC implementations, but the copying
   version is currently out-of-date. */

#define USE_COPYING 0

#if USE_COPYING
# include "copy.c"
#else
# include "compact.c"
#endif
