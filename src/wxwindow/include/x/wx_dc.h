/*
 * File:	wx_dc.h
 * Purpose:	wxDC device context declaration (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_dc.h	1.2 5/9/94" */


#ifndef wx_dch
#define wx_dch

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_dc.h"

#ifdef IN_CPROTO
typedef       void    *wxDC ;
#else

class wxDC: public wxbDC
{
  DECLARE_ABSTRACT_CLASS(wxDC)

 public:
  wxDC(void) {}
  ~wxDC(void) {}
};

/*
 * Note: the following is contributed by Alexey Iskhakov, and _should_
 * get round out-by-1 errors.
 * Hopefully it makes the old code redundant but if there are
 * problems for anyone (Matthew Flatt?), let's discuss it again.
 * JACS 21/5/95
 */

// Logical to device
// Absolute
#define XLOG2DEV(x) (int)floor((((x) - logical_origin_x)*logical_scale_x*user_scale_x + device_origin_x + 0.5))
#define YLOG2DEV(y) (int)floor((((y) - logical_origin_y)*logical_scale_y*user_scale_y + device_origin_y + 0.5))

// Logical to device without the device translation
#define XLOG2DEV_2(x) (int)floor((((x) - logical_origin_x)*logical_scale_x*user_scale_x + 0.5))
#define YLOG2DEV_2(y) (int)floor((((y) - logical_origin_y)*logical_scale_y*user_scale_y + 0.5))

// Relative
#define XLOG2DEVREL(x) (int)floor(((x)*logical_scale_x*user_scale_x + 0.5))
#define YLOG2DEVREL(y) (int)floor(((y)*logical_scale_y*user_scale_y + 0.5))

// Device to logical
// Absolute
#define XDEV2LOG(x) (((x) - device_origin_x)/(logical_scale_x*user_scale_x) + logical_origin_x)
#define YDEV2LOG(y) (((y) - device_origin_y)/(logical_scale_y*user_scale_y) + logical_origin_y)

// Relative
#define XDEV2LOGREL(x) ((float)(x)/(logical_scale_x*user_scale_x))
#define YDEV2LOGREL(y) ((float)(y)/(logical_scale_y*user_scale_y))

#ifdef MACH_TRUNCS_NEG_UP
static inline int ROUND(float f)
{
  if (f > 0) 
    return (int)(f + 0.5); 
  else {
    int v = (int)f;
    if (v - f > 0.5)
      return v - 1;
    else
      return v;
  }
}
#else
#define ROUND(x) (int)((x) + 0.5)
#endif

/******* OLD CODE
// Logical to device
// Absolute
#define XLOG2DEV(x) ROUND(((x) - logical_origin_x)*logical_scale_x*user_scale_x + device_origin_x)
#define YLOG2DEV(y) ROUND(((y) - logical_origin_y)*logical_scale_y*user_scale_y + device_origin_y)

// Logical to device without the device translation
#define XLOG2DEV_2(x) ROUND(((x) - logical_origin_x)*logical_scale_x*user_scale_x)
#define YLOG2DEV_2(y) ROUND(((y) - logical_origin_y)*logical_scale_y*user_scale_y)

// Relative
#define XLOG2DEVREL(x) ROUND((x)*logical_scale_x*user_scale_x)
#define YLOG2DEVREL(y) ROUND((y)*logical_scale_y*user_scale_y)

// Device to logical
// Absolute. Kari Grano <grano@research.nokia.com> recommends the non-rounding version;
#define XDEV2LOG(x) (((x) - device_origin_x)/(logical_scale_x*user_scale_x) + logical_origin_x)
#define YDEV2LOG(y) (((y) - device_origin_y)/(logical_scale_y*user_scale_y) + logical_origin_y)

// Relative
#define XDEV2LOGREL(x) ROUND((float)(x)/(logical_scale_x*user_scale_x))
#define YDEV2LOGREL(y) ROUND((float)(y)/(logical_scale_y*user_scale_y))

*/

#endif // IN_CPROTO
#endif // wx_dch
