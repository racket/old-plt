/*
 * File:	wx_scrol.h
 * Purpose: Scrollbar panel item (MSW version)
 * Author:  Sergey Krasnov (ksa@orgland.ru)
 * Created: 1995
 * Updated:
 * Copyright:
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_scrolh
#define wx_scrolh

#include "wb_scrol.h"

#ifdef IN_CPROTO
typedef       void    *wxScrollBar ;
#else

// Scrollbar item
class wxScrollBar: public wxbScrollBar
{
  DECLARE_DYNAMIC_CLASS(wxScrollBar)

public:
    int pageSize;
    int viewSize;
    wxScrollBar(void);
    wxScrollBar(wxPanel *panel, wxFunction func,
                int direction = wxVERTICAL,
                int x = -1, int y = -1, int width = -1, int height = -1,
                long style = 0, char *name = "scrollbar");
    ~wxScrollBar(void);

    Bool Create(wxPanel *panel, wxFunction func,
                int direction = wxVERTICAL,
                int x = -1, int y = -1, int width = -1, int height = -1,
                long style = 0, char *name = "scrollbar");

    virtual void SetValue(int viewStart);
    virtual int GetValue(void);
    virtual char *GetLabel(void);
    virtual void SetLabel(char *label);
    void SetPageLength(int pageLength);
    void SetObjectLength(int objectLength);
    void SetViewLength(int viewLength);
    void GetValues(int *viewStart, int *viewLength, int *objectLength,
                    int *pageLength);

    void SetBackgroundColour(wxColour *col);
    void SetLabelColour(wxColour *col);
    void SetButtonColour(wxColour *col);
};

#endif // IN_CPROTO
#endif // wx_scrolh
