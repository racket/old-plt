
/* We have two experimental GC implementations.
   The copying version is mainly for debugging, since
   it can move data on every collection. */

#define USE_DEBUGGING_COLLECTOR 1

#if USE_DEBUGGING_COLLECTOR
# include "copy.c"
#else
# include "compact.c"
#endif
