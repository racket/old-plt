/*
 * File:	wx.h
 * Purpose:	Window library main include file
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx.h	1.2 5/9/94" */

#ifndef wx_wxh
#define wx_wxh

#include "wx_setup.h"           // Which features to include - user editable
#include "common.h"

#if WX_USE_MEMORY_TRACING
#include "wx_mem.h"
#endif

// #include "wx_mgstr.h"        // Simple language support: a file of strings
#include "wx_win.h"
#include "wx_frame.h"
#include "wx_dc.h"
#include "wx_dccan.h"
#include "wx_dcmem.h"
#include "wx_dcps.h"
// #include "wx_mf.h"           // Not included as standard
#include "wx_canvs.h"
#include "wx_panel.h"
#include "wx_dcpan.h"
#include "wx_buttn.h"
#include "wx_check.h"
#include "wx_choic.h"
// #include "wx_clipb.h"        // Not included as standard
#include "wx_messg.h"
#include "wx_lbox.h"
#include "wx_rbox.h"
#include "wx_txt.h"
#include "wx_mtxt.h"
#include "wx_slidr.h"
#include "wx_group.h"
#if USE_GAUGE
#include "wx_gauge.h"
#endif
#include "wx_menu.h"
#include "wx_text.h"
#include "wx_main.h"
#include "wx_stdev.h"
#include "wx_list.h"
// #include "wx_form.h"         // Not included as standard
#include "wx_gdi.h"
#include "wx_dialg.h"
// #include "wx_enhdg.h"        // Not included as standard
// #include "wx_ipc.h"          // Not included as standard
// #include "wx_timer.h"        // Not included as standard
#include "wx_utils.h"
#include "wx_cmdlg.h"

#if USE_WX_RESOURCES
#include "wx_res.h"
#endif

#if USE_CONSTRAINTS
#include "wx_lay.h"
#endif

#ifdef wx_msw
#include "wx_wmgr.h"
#include "wx_privt.h"
#include "wx_itemp.h"

/* I don't know where this is coming from, but it makes a mess. */
#undef GetStyle
#undef SetStyle
#undef GetFont
#undef SetFont
#undef SetBitmap

#endif

// #include "wx_scrol.h"        // Not included as standard
// #include "wx_vlbox.h"        // Not included as standard

#endif // wx_wxh
