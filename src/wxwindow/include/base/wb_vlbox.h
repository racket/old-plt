/*
 * File:    	wb_vlbox.h
 * Purpose:     Virtual list box  items
 * Author:      Sergey Krasnov (ksa@orgland.ru) 
 * Created: 	1995
 * Updated:
 * Copyright:
 */

/* sccsid[] = "%W% %G%" */

#ifndef wb_vlbox
#define wb_vlbox

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_item.h"

/*
These defines were put in base/wx_types.h and wx_stdev.h
#define wxTYPE_VIRT_LIST_BOX                   22
#define wxEVENT_TYPE_VIRT_LISTBOX_COMMAND     (EVENT_TYPES_FIRST + 16)
*/

#define wxERROR                 (-1)
typedef char *(*CPFPCPI)(char *, int);

// Set TRUNCATE_LABEL to 1 if you want adjust label size
#define TRUNCATE_LABEL      0

#ifdef IN_CPROTO
typedef       void    *wxbVirtListBox;
#else

class wxListBox;
class wxScrollBar;

class wxbVirtListBox: public wxItem
{
public:
    CPFPCPI GetOptx;
    char *gArg;
    wxbVirtListBox(void);
    wxbVirtListBox(wxPanel * panel, wxFunction func,
                CPFPCPI get_optx, char *garg,
             	char *Title, int nrows = -1,
             	Bool Multiple = wxSINGLE|wxNEEDED_SB,
             	int x = -1, int y = -1, int width = -1, int height = -1,
             	long style = 0, char *name = "virtLlistBox");

    ~wxbVirtListBox();

    virtual void Enable(Bool enable);
    virtual Bool Show(Bool show) = 0;
    virtual void SetRowsNumber(int nRowsNew) = 0; // set vizible rows number
    void SetSelection(int n);
    void HideSelection(void);
    int SearchItem(char * searchStr, Bool searchDown, Bool searchCase);
	void SetViewStart(int nViewStart, Bool Refresh = FALSE);
	void Refresh(void);
	inline int GetViewStart(void)				{ return nLastViewStart; }
	inline int GetSelection(void) 				{ return Selection; }
    inline int GetRowsNumber(void)              { return nRows; }
    inline int GetChoicesNumber(void)           { return nItems; }
    // GetChoicesNumber returns current "virtual" number of choices

    void Command(wxCommandEvent& event);
    void ProcessCommand (wxCommandEvent & event);

protected:
    int nRows;
    int nItems;
    int nLastViewStart;
    int Selection;
    Bool WasOnBottom;
    Bool Refresh_;
    Bool IsVizible;
    wxScrollBar *scrollBar;
    wxListBox *listBox;
    Bool InsertString(int& nViewStart, int pos);
    void GoToBottom(int& nViewStart);
    virtual void SetRedraw(Bool redraw) = 0;
    virtual void SetListSelection(int n, Bool select) = 0;
};

#endif // IN_CPROTO
#endif // wb_vlbox
