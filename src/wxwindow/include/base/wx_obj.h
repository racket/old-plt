/*
 * File:	wx_obj.h
 * Purpose:	Top level object for wxWindows
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_objh
#define wxb_objh

#include "common.h"

/* Even with normal GC, MrEd for Windows registers globals: */
#define WX_REGISTER_GLOBAL_MEMORY

#include "../../../wxcommon/wxGC.h"

#ifndef WXGC_CLEANUP_CLASS
# define WXGC_CLEANUP_CLASS gc_cleanup
#endif

#define WXGC_IGNORE(base, ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
#define WXGC_ATOMIC (AtomicGC)
#define WXGC_NO_CLEANUP FALSE
#define DELETE_OBJ delete
#define DELETE_VAL delete
#define COPYSTRING_TO_ALIGNED(s, d) (s + d)

class wxObject : public WXGC_CLEANUP_CLASS
{
 public:
  WXTYPE __type;

  wxObject(void);
  wxObject(Bool cleanup);
  virtual ~wxObject(void);
};


#endif // wx_objh
