/*
 * File:	wx_scrol.h
 * Purpose: Scrollbar panel item (X version)
 * Author:  Sergey Krasnov (ksa@orgland.ru) 
 * Created: 1995
 * Updated:
 * Copyright:
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_scrolh
#define wx_scrolh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_scrol.h"

#ifdef IN_CPROTO
typedef       void    *wxScrollBar ;
#else

// Scrollbar
class wxScrollBar: public wxbScrollBar
{
  DECLARE_DYNAMIC_CLASS(wxScrollBar)

public:
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
    void GetSize(int *width, int *height);
    void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
    void ChangeColour(void) ;
    void Enable(Bool enable);
    virtual void Show(Bool show);
//  void SetPixelsPerUnit(int pixelsPerUnit);
//  void GetValues(int *viewStart, int *viewLength, int *objectLength,
//                  int *pixelsPerUnit, int *pageLength);
};

#endif // IN_CPROTO
#endif // wx_scrolh
