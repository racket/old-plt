
/* We have several experimental GC implementations.

   The copying version is mainly for debugging, since it can move data
   on every collection. */
#ifdef NEWGC_BTC_ACCOUNT
# include "newgc.c"
# define COLLECTOR_INCLUDED
#endif

#if defined(NEWGC_MEMORY_TRACE) && !defined(COLLECTOR_INCLUDED)
# include "newgc.c"
# define COLLECTOR_INCLUDED
#endif

#ifdef USE_COPYING_3M_GC
# include "newgc.c"
# define COLLECTOR_INCLUDED
#endif

#ifndef COLLECTOR_INCLUDED
# include "compact.c"
#endif

