
# include "../../mzscheme/utils/xcglue.h"

void wxsScheme_setup(Scheme_Env *env);

#ifndef wx_msw
# undef USE_METAFILE
# define USE_METAFILE 0
#endif
