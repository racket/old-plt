
/* We have several experimental GC implementations.

   The copying version is mainly for debugging, since it can move data
   on every collection. */
#if !defined(USE_ACCT_3M_GC) && !defined(USE_PRECISE_ACCT_3M_GC) && !defined(USE_COMPACT_3M_GC) && !defined(USE_COPYING_3M_GC)
# define USE_COMPACT_3M_GC
#endif

#ifdef USE_ACCT_3M_GC
#define NEWGC_ACCNT
# include "newgc.c"
#endif

#ifdef USE_PRECISE_ACCT_3M_GC
# define NEWGC_ACCNT
# define NEWGC_USE_HEADER
# define NEWGC_PRECISE
# include "newgc.c"
#endif

#ifdef USE_COMPACT_3M_GC
# include "compact.c"
#endif

#ifdef USE_COPYING_3M_GC
# include "newgc.c"
#endif
