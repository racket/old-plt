/*
 * File:	wx_lbox.h
 * Purpose:	List box panel item
 * Author:	Julian Smart/Cecil Coupe (mac version)
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_lboxh
#define wx_lboxh

#include "wb_lbox.h"
#include "wxLabelArea.h"
/* #include <Lists.h> */
/* begin A List includes */
#include "TheAList.h"
#include "AListOptimizations.h"
#include "LongCoords.h"
/* end A List includes */


#ifdef IN_CPROTO
typedef       void    *wxListBox ;
#else

class wxBitmap;
class wxPanel;
class wxMessage;
class wxList;

// List box item
class wxListBox: public wxbListBox
{
 public:
	wxLabelArea*	cListTitle;
	ALReference		cListReference;
	int				cHaveVScroll;
	int				cKeycnt;		// next key (number)
	wxList*			cDataList;		// List of ClientData(s) per ListBox Entry
//	unsigned long   cLastClickTime;  // mflatt: for double-clicking
//	Cell			cLastClickCell;  // mflatt: for double-clicking
	wxArea*			cBorderArea; 	// mflatt: for showing keyboard focus
//	wxArea*         cThinBorderArea; // mflatt: box around list
	
  wxListBox(
  		wxPanel *panel, 
  		wxFunction func, 
  		char *Title,
		Bool Multiple = wxSINGLE|wxNEEDED_SB,
		int x = -1, 
		int y = -1, 
		int width = -1, 
		int height = -1,
		int N = 0, 
		char **Choices = NULL,
		long style = 0, 
	        wxFont *_font = NULL,
		char *name = "listBox",
		WXTYPE		objectType = wxTYPE_LIST_BOX
		);
		
  ~wxListBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title, 
  		Bool Multiple = FALSE,
		int x = -1, 
		int y = -1, 
		int width = -1, 
		int height = -1,
		int N = 0, 
		char **Choices = NULL,
		long style = 0, 
		char *name = "listBox"
	);
  void Append(char *Item);
  void Append(char *Item, char *Client_data);
  void Set(int N, char *Choices[]);
  void Clear(void);
  void SetSelection(int N, Bool select = TRUE, Bool just_one = FALSE);
  void SetOneSelection(int N);
  void SetString(int N, char *s);
  // Get client data
  char *GetClientData(int N);
  void SetClientData(int N, char *Client_data);

  void Deselect(int N);

  // For single choice list item only
  int GetSelection(void);
  void Delete(int N);
  
  // For single or multiple choice list item
  int GetSelections(int **list_selections);
  Bool Selected(int N);
#if 0 // CJC
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }
  void GetPosition(int *x, int *y);
#endif
  void InsertItems(int nItems, char **Items, int pos);
  // Set the specified item at the first visible item
  // or scroll to max range.
  void SetFirstItem(int N) ;

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;
  // Windows-specific code to set the horizontal extent of
  // the listbox, if necessary. If s is non-NULL, it's
  // used to calculate the horizontal extent.
  // Otherwise, all strings are used.
  void SetHorizontalExtent(char *s = NULL);

  void OnSetFocus(void);
  void OnKillFocus(void);
  
  void DoShow(Bool);
  
  virtual void Paint(void);
  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnChar(wxKeyEvent *event); // mflatt
  virtual void OnClientAreaDSize(int dW, int dH, int dX, int dY);
  virtual void MaybeMoveControls();

  void MoveBox(int dW, int dH, int dX, int dY);

  char *GetLabel(void);
  void SetLabel(char *label);
  
  int NumberOfVisibleItems();
  int GetFirstItem();

  virtual Bool WantsFocus(void); // mflatt
  virtual void InternalGray(int gray);
  virtual void ReleaseCurrentDC(int really = 0);
  virtual void Activate(Bool gray);
protected:

  virtual void ChangeToGray(Bool gray);
};

#endif // IN_CPROTO
#endif // wx_lboxh
