
/* We have several experimental GC implementations.

   The copying version is mainly for debugging, since it can move data
   on every collection. */

#if !defined(USE_NEW_3M_GC) && !defined(USE_COMPACT_3M_GC) && !defined(USE_COPYING_3M_GC)
# define USE_COMPACT_3M_GC
#endif

#ifdef USE_NEW_3M_GC
# include "newgc.c"
#endif
#ifdef USE_COMPACT_3M_GC
# include "compact.c"
#endif
#ifdef USE_COPYING_3M_GC
# include "copy.c"
#endif
