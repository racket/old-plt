/*
 * File:	wx_obj.h
 * Purpose:	Top level object for wxWindows
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_objh
#define wxb_objh

#include "common.h"

#ifdef IN_CPROTO
typedef       void    *wxObject ;
#else

#include "wxGC.h"

#define WXGC_IGNORE(base, ptr) GC_general_register_disappearing_link((void **)&(ptr), NULL)
#define WXGC_ATOMIC (AtomicGC)
#define WXGC_NO_CLEANUP FALSE
#define DELETE_OBJ delete
#define DELETE_VAL delete
#define COPYSTRING_TO_ALIGNED(x, d) (x + d)

class wxObject : public gc_cleanup
{
  public:
  WXTYPE __type;
  wxObject(void);
  wxObject(Bool cleanup);
  wxObject(Bool cleanup, WXTYPE t);
  virtual ~wxObject(void);
};

#endif // IN_CPROTO
#endif // wx_objh
