
#define USE_COMPILED_MACROS 1

#ifdef MZSCHEME_SOMETHING_OMITTED
/* .inc files are not, by default, compatible with
   omissions. If you make a version with omissions
   and then run `mzmake cmacro', then USE_COMPILED_MACROS
   can be set to 1. */
# undef USE_COMPILED_MACROS
# define USE_COMPILED_MACROS 0
#endif

#if defined(__MWERKS__) && !defined(powerc)
#define MZCOMPILED_STRING_FAR far
#else
#define MZCOMPILED_STRING_FAR /**/
#endif

#if USE_COMPILED_MACROS
extern Scheme_Object *scheme_eval_compiled_sized_string(const char *str, int len, Scheme_Env *env);
#endif
