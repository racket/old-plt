/*
 * File:	wb_cmdlg.h
 * Purpose:	Common dialogs: generic declarations
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 */

/* sccsid[] = "%W% %G%" */

#ifndef wb_cmdlgh
#define wb_cmdlgh

#ifdef __GNUG__
#pragma interface
#endif

#include "wx_setup.h"
#include "wx_gdi.h"

// Handy dialog functions (will be converted into classes at some point)
char *wxGetTextFromUser(char *message, char *caption = "Input text",
                        char *default_value = "", wxWindow *parent = NULL,
                        int x = -1, int y = -1, Bool centre = TRUE);

#define wxCHOICE_HEIGHT 150
#define wxCHOICE_WIDTH 200

char *wxGetSingleChoice(char *message, char *caption,
                        int n, char *choices[], wxWindow *parent = NULL,
                        int x = -1, int y = -1, Bool centre = TRUE,
                        int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);

// Same as above but gets position in list of strings, instead of string,
// or -1 if no selection
int wxGetSingleChoiceIndex(char *message, char *caption,
                           int n, char *choices[], wxWindow *parent = NULL,
                           int x = -1, int y = -1, Bool centre = TRUE,
                           int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);

// Return client data instead
char *wxGetSingleChoiceData(char *message, char *caption,
                            int n, char *choices[], char *client_data[],
                            wxWindow *parent = NULL, int x = -1, int y = -1,
                            Bool centre = TRUE,
                            int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);
                           
int wxGetMultipleChoice(char *message, char *caption,
			  int n, char *choices[], 
			  int nsel, int * selection,
			  wxWindow *parent = NULL, int x = -1 , int y = -1, Bool centre = TRUE,
			  int width = wxCHOICE_WIDTH, int height = wxCHOICE_HEIGHT);

// type is an 'or' (|) of wxOK, wxCANCEL, wxYES_NO
// Returns wxYES/NO/OK/CANCEL
int wxbMessageBox(char *message, char *caption = "Message", long style = wxOK|wxCENTRE,
  wxWindow *parent = NULL, int x = -1, int y = -1);

#define wxOPEN 1
#define wxSAVE 2
#define wxOVERWRITE_PROMPT 4
#define wxHIDE_READONLY 8

// Generic file load dialog
char * wxLoadFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);

// Generic file save dialog
char * wxSaveFileSelector(char *what = "Text", char *extension = "txt", char *default_name = NULL);
// File selector
char *wxFileSelector(char *message = "Select a file", char *default_path = NULL,
                     char *default_filename = NULL, char *default_extension = NULL,
                     char *wildcard = "*.*", int flags = 0,
                     wxWindow *parent = NULL, int x = -1, int y = -1);

#endif
