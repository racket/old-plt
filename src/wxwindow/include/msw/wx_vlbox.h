/*
 * File:    wx_vlbox.h
 * Purpose: Virtual lixt box item (MSW version)
 * Author:  Sergey Krasnov (ksa@orgland.ru) 
 * Created: 1995
 * Updated:
 * Copyright:
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_vlbox
#define wx_vlbox

#include "wb_vlbox.h"

#ifdef IN_CPROTO
typedef       void    *wxVirtListBox;
#else

class wxVirtListBox: public wxbVirtListBox
{
  DECLARE_DYNAMIC_CLASS(wxVirtListBox)

public:
    wxVirtListBox(void);
    wxVirtListBox(wxPanel * panel, wxFunction func,
                CPFPCPI get_optx, char *garg,
               char *Title, int nrows = -1,
               Bool Multiple = wxSINGLE|wxNEEDED_SB,
               int x = -1, int y = -1, int width = -1, int height = -1,
               long style = 0, char *name = "virtLlistBox");

    ~wxVirtListBox();
    Bool Create(wxPanel * panel, wxFunction func,
                CPFPCPI get_optx, char *garg,
               char *Title, int nrows = -1,
               Bool Multiple = wxSINGLE|wxNEEDED_SB,
               int x = -1, int y = -1, int width = -1, int height = -1,
               long style = 0, char *name = "listBox");

    virtual void SetRowsNumber(int nRowsNew);
    virtual Bool Show(Bool show);
    virtual void GetSize(int *w, int *h);
    virtual void GetPosition(int *x, int *y);
    virtual void SetSize(int x, int y, int w, int h, int sizeFlags = wxSIZE_AUTO);
    char *GetLabel(void);
    void SetLabel(char *label);
    void SetBackgroundColour(wxColour*col) ;
    void SetLabelColour(wxColour*col) ;
    void SetButtonColour(wxColour*col) ;
protected:
	int listBoxVOffset;
    inline int ScrollBarHeight(void);
   	void CheckListBoxSize(void);
    virtual void SetRedraw(Bool redraw);
    virtual void SetListSelection(int n, Bool select);
};

#endif // IN_CPROTO
#endif // wx_vlbox
