
#define INCLUDE_WITHOUT_PATHS

#ifdef __MWERKS__
#if defined(__powerc)
#include <MacHeadersPPC>
#else
#include <MacHeaders68K>
#endif
#endif

#define WXUNUSED(x)

#define OPERATOR_NEW_ARRAY

/* code added by JBC because compiler no longer handles keyword 
   'far' for PPC */
   
#if defined(__powerc)
#  define WX_FAR /**/
# else
#  define WX_FAR far
# endif
