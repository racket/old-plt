/*
 * File:	wb_item.h
 * Purpose:	Declares panel items base class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_item.h	1.2 5/9/94" */

#ifndef wxb_itemh
#define wxb_itemh

#ifdef __GNUG__
#ifdef __GNUG__
#pragma interface
#endif
#endif

#include "common.h"
#include "wx_panel.h"
#include "wx_stdev.h"

#ifdef IN_CPROTO
typedef       void    *wxbItem ;
#else

class wxFont ;

// General item class
class wxbItem: public wxWindow
{
 public:
 
    wxbItem(void);

    // Constructor (given parentArea)
    wxbItem(char* windowName, wxArea* parentArea, int x, int y, int width, int height,
	    long style);
    // Constructor (given parentWindow)
    wxbItem(char* windowName, wxWindow* parentWindow, int x, int y, int width, int height,
	    long style);
    // Constructor (given objectType; i.e., menu or menuBar)
    wxbItem(char* windowName);


   ~wxbItem(void);

   wxFont *buttonFont ;
   wxFont *labelFont ;
   wxColour *backColour ;
   wxColour *labelColour;
   wxColour *buttonColour;

   int labelPosition;
   virtual void SetLabel(char *label) = 0;
   virtual char *GetLabel(void) = 0;

   virtual void Command(wxCommandEvent *event);        // Simulates an event
   virtual void ProcessCommand(wxCommandEvent *event); // Calls the callback and 

   // inline virtual void Show(Bool show) {};
   virtual int GetLabelPosition(void);
   virtual void SetLabelPosition(int pos);

  // Places item in centre of panel - so can't be used BEFORE panel->Fit()
  void Centre(int direction = wxHORIZONTAL);

  inline virtual wxFont  *GetLabelFont(void)        { return labelFont ; }
  inline virtual wxFont  *GetButtonFont(void)       { return buttonFont ; }
  inline virtual wxColour*GetBackgroundColour(void) { return backColour ; }
  inline virtual wxColour*GetLabelColour(void)      { return labelColour ; }
  inline virtual wxColour*GetButtonColour(void)     { return buttonColour ; }

  virtual void SetBackgroundColour(wxColour*col) = 0 ;
  virtual void SetLabelColour(wxColour*col) = 0 ;
  virtual void SetButtonColour(wxColour*col) = 0 ;
};

#endif // IN_CPROTO
#endif // wxb_itemh
