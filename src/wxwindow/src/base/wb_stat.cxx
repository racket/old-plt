#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "wx_panel.h"

#endif

#if USE_EXTENDED_STATICS

#include <wb_stat.h>
#include <wx_dcpan.h>
#include <math.h>

wxbStaticItem::wxbStaticItem(void)
   {
     x_draw = xpos = 0;
     y_draw = ypos = 0;
     w_draw = width = 0;
     h_draw = height = 0;
     panel = NULL;
     dc = NULL;
   }

wxbStaticItem::wxbStaticItem(wxPanel *the_panel,int x, int y, int w, int h)
   {
     x_draw = xpos = x;
     y_draw = ypos = y;
     w_draw = width = w;
     h_draw = height = h;
     panel = the_panel;
     dc = panel -> GetPanelDC();
   }

wxbStaticItem::~wxbStaticItem(void)
   {
   }

#endif
