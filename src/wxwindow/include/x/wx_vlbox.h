/*
 * File:        wx_vlbox.h
 * Purpose:     Virtual list box items (X version)
 * Author:
 * Created: 	1995
 * Updated:
 * Copyright:
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_vlbox
#define wx_vlbox

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
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

    virtual Bool Show(Bool show);
    virtual void SetRowsNumber(int nRowsNew);
    virtual void GetSize(int *w, int *h);
    virtual void GetPosition(int *x, int *y);
    virtual void SetSize(int x, int y, int w, int h, int sizeFlags = wxSIZE_AUTO);
    void SetSize(int width, int height) { wxItem::SetSize(width, height); }
    char *GetLabel(void);
    void SetLabel(char *label);
    void ChangeColour(void);
protected:
    virtual void SetRedraw(Bool redraw);
    virtual void SetListSelection(int n, Bool select);
    inline int ScrollBarHeight(void);
    void CheckListBoxSize(void);
    int itemHeight;
#ifdef wx_motif
	int listBoxTopOffcet;
	int listBoxLeftOffcet;
	int listBoxItemTopOffcet;
#endif
};

#endif // IN_CPROTO
#endif // wx_vlbox
