/*
 * File:    	wb_scrol.h
 * Purpose: 	Scrollbar items
 * Author:      Sergey Krasnov (ksa@orgland.ru)
 * Created: 	1995
 * Updated:
 * Copyright:
 */

/* sccsid[] = "%W% %G%" */

#ifndef wxb_scrolh
#define wxb_scrolh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

/*
These defines were put in base/wx_types.h and wx_stdev.h
#define wxTYPE_SCROLL_BAR                   21
#define wxEVENT_TYPE_SCROLLBAR_COMMAND     (EVENT_TYPES_FIRST + 15)
*/

#ifdef IN_CPROTO
typedef       void    *wxbScrollBar ;
#else

// Scrollbar item
class wxbScrollBar: public wxItem
{
public:
    wxbScrollBar(void);
    wxbScrollBar(wxPanel *panel, wxFunction func,
                int direction = wxVERTICAL,
                int x = -1, int y = -1, int width = -1, int height = -1,
                long style = 0, char *name = "scrollbar");
    ~wxbScrollBar(void);

    virtual void SetValue(int viewStart) = 0;
    virtual int GetValue(void) = 0;

    void Command(wxCommandEvent& event);
    void ProcessCommand(wxCommandEvent& event);
};

#endif // IN_CPROTO
#endif // wxb_scrolh
