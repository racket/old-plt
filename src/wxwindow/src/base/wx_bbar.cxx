/*
 * File:	wx_bbar.cc
 * Purpose:	Almost identical to wxToolBar programmer interface, but optimized
 *              for Windows to give better button-press and toggle feedback.
 *              By default, 16x15 pixel bitmaps are required under Windows.
 *              You can change this by calling wxButtonBar::SetDefaultSize,
 *              _before_ you start adding tools. See test.cc.
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

// static const char sccsid[] = "%W% %G%";

#if defined(_MSC_VER)
# include "wx.h"
#else

#endif
