/*
 * File:	wx_lbox.h
 * Purpose:	Declares list box item (X version)
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_lbox.h	1.2 5/9/94" */

#ifndef wx_lboxh
#define wx_lboxh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wb_lbox.h"

#ifdef IN_CPROTO
typedef       void    *wxListBox ;
#else

// List box item
class wxListBox: public wxbListBox
{
  DECLARE_DYNAMIC_CLASS(wxListBox)

 private:
#ifdef wx_motif
  wxList clientDataList; // List mapping positions->client data
#endif

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
  void Append(char *Item);
  void Append(char *Item, char *Client_data);
  void Set(int N, char *Choices[]);
  int FindString(char *s);
  void Clear(void);
  void SetSelection(int N, Bool select = TRUE);
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
  char *GetString(int N);
  void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
  void SetSize(int width, int height) { wxItem::SetSize(width, height); }

  void InsertItems(int nItems, char **Items, int pos);

  int NumberOfVisibleItems(void);

  // Set the specified item at the first visible item
  // or scroll to max range.
  void SetFirstItem(int N) ;
  void SetFirstItem(char *s) ;

  int GetFirstItem();

  void ChangeColour(void) ;
};

#endif // IN_CPROTO
#endif // wx_lboxh
