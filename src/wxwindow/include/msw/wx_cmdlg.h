/*
 * File:	wx_cmdlg.h
 * Purpose:	Common dialogs: MS Windows declarations
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_cmdlgh
#define wx_cmdlgh

#ifdef __GNUG__
#pragma interface
#endif

#include "wx_setup.h"

// Generic common dialogs are in wb_cmdlg.h
#include "wb_cmdlg.h"

// Windows common dialog header
#include "commdlg.h"

/*
 * COLOUR DIALOG
 */

class wxColourDialog: public wxDialogBox
{
 DECLARE_DYNAMIC_CLASS(wxColourDialog)
 protected:
  wxColourData colourData;
  wxWindow *dialogParent;
 public:
  wxColourDialog(void);
  wxColourDialog(wxWindow *parent, wxColourData *data = NULL);

  Bool Create(wxWindow *parent, wxColourData *data = NULL);

  Bool Show(Bool show);
  wxColourData& GetColourData(void) { return colourData; }
};

/*
 * FONT DIALOG
 */
 
class wxFontDialog: public wxDialogBox
{
 DECLARE_DYNAMIC_CLASS(wxFontDialog)
 protected:
  wxWindow *dialogParent;
  wxFontData fontData;
 public:
  wxFontDialog(void);
  wxFontDialog(wxWindow *parent, wxFontData *data = NULL);

  Bool Create(wxWindow *parent, wxFontData *data = NULL);

  Bool Show(Bool show);
  wxFontData& GetFontData(void) { return fontData; }
};

/*
 * The following code was formerly in wx_dialg.h.
 */
 

int wxMessageBox(char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
  wxWindow *parent = NULL, int x = -1, int y = -1);

// An extended version of wxFileSelector

char *wxFileSelectorEx(char *message = "Select a file", char *default_path = NULL,
                     char *default_filename = NULL, int *indexDefaultExtension = NULL,
                     char *wildcard = "*.*", int flags = 0,
                     wxWindow *parent = NULL, int x = -1, int y = -1);
#endif
