
#define INCLUDE_WITHOUT_PATHS

#ifdef OS_X
  #define OPAQUE_TOOLBOX_STRUCTS 1
  #define TARGET_API_MAC_CARBON 1

  #define SGC_STD_DEBUGGING 1

  #include <Carbon/Carbon.h>
#else
  #ifdef __MWERKS__
  #if defined(__powerc)
  #include <MacHeadersPPC>
  #else
  #include <MacHeaders68K>
  #endif
  #endif
#endif

#define WXUNUSED(x)

#define OPERATOR_NEW_ARRAY

/* code added by JBC because compiler no longer handles keyword 
   'far' for PPC */
   
#ifdef __MWERKS__
# if defined(__powerc)
#   define WX_FAR /**/
#  else
#   define WX_FAR far
#  endif
#endif
