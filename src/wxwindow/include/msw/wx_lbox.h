/*
 * File:	wx_lbox.h
 * Purpose:	List box panel item
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_lboxh
#define wx_lboxh

#include "wb_lbox.h"

#ifdef IN_CPROTO
typedef       void    *wxListBox ;
#else

// List box item
class wxListBox: public wxbListBox
{
  DECLARE_DYNAMIC_CLASS(wxListBox)
 private:
  HWND static_label;
  char **user_data;
 public:
  wxListBox(void);
  wxListBox(wxPanel *panel, wxFunction func, char *Title,
             Bool Multiple = wxSINGLE|wxNEEDED_SB,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             long style = 0, char *name = "listBox");
  ~wxListBox(void);

  Bool Create(wxPanel *panel, wxFunction func, char *Title, Bool Multiple = FALSE,
             int x = -1, int y = -1, int width = -1, int height = -1,
             int N = 0, char **Choices = NULL,
             long style = 0, char *name = "listBox");
  BOOL MSWCommand(UINT param, WORD id);

  void Append(char *Item);
  void Append(char *Item, char *Client_data);
  void Set(int N, char *Choices[]);
  int FindString(char *s);
  void Clear(void);
  void SetSelection(int N, Bool select = TRUE);

  virtual void ChangeToGray(Bool gray);
  
  void Deselect(int N);

  // For single choice list item only
  int GetSelection(void);
  void Delete(int N);
  char *GetClientData(int N);
  void SetClientData(int N, char *Client_data);
  void SetString(int N, char *s);

  // For single or multiple choice list item
  int GetSelections(int **list_selections);
  Bool Selected(int N);
  char *GetString(int N);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void GetSize(int *x, int *y);
  void GetPosition(int *x, int *y);
  char *GetLabel(void);
  void SetLabel(char *label);

  // Set the specified item at the first visible item
  // or scroll to max range.
  void SetFirstItem(int N) ;
  void SetFirstItem(char *s) ;

  void InsertItems(int nItems, char **Items, int pos);

  void SetBackgroundColour(wxColour*col) ;
  void SetLabelColour(wxColour*col) ;
  void SetButtonColour(wxColour*col) ;
  // Windows-specific code to set the horizontal extent of
  // the listbox, if necessary. If s is non-NULL, it's
  // used to calculate the horizontal extent.
  // Otherwise, all strings are used.
  void SetHorizontalExtent(char *s = NULL);

  virtual Bool Show(Bool s);
};

#endif // IN_CPROTO
#endif // wx_lboxh
