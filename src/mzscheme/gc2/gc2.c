
/* We have several experimental GC implementations. */

#define USE_COMPACT_3M_GC

#ifdef USE_COMPACT_3M_GC
# include "compact.c"
# define COLLECTOR_INCLUDED
#endif

#ifdef USE_COPYING_3M_GC
# include "newgc.c"
# define COLLECTOR_INCLUDED
#endif

#ifndef COLLECTOR_INCLUDED
# include "newgc.c"
#endif

