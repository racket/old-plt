
#ifdef OS_X
# include <Carbon/Carbon.h>
#else
# ifdef __MWERKS__
#  if defined(__powerc)
#   include <MacHeadersPPC>
#  else
#   include <MacHeaders68K>
#  endif
# endif
#endif

#define FOR_STARTER
#define FOR_MRED
