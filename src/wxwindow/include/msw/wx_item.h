/*
 * File:	wx_item.h
 * Purpose:	Declares base panel item class
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_item.h	1.2 5/9/94" */

#ifndef wx_itemh
#define wx_itemh

#include "wb_item.h"

#ifdef IN_CPROTO
typedef       void    *wxItem ;
#else

// General item class
class wxBrush;
class wxFont;
class wxColour;
class wxItem: public wxbItem
{
  DECLARE_ABSTRACT_CLASS(wxItem)

 protected:
   Bool isFafa ;      // because we can mix Fafa/non-Fafa controls
 public:
   wxList subControls; // For controls like radiobuttons which are really composite
   FARPROC oldWndProc; // For subclassed controls
   wxBrush *backBrush ;
   int mswLastXPos, mswLastYPos;
   int mswLastEvent;
   Bool isBeingDeleted; // Fudge because can't access parent
                        // when being deleted (don't know why)

   wxItem(void);
   ~wxItem(void);

   void GetSize(int *width, int *height);
   void GetPosition(int *x, int *y);
   void SetSize(int x, int y, int width, int height, int sizeFlags = wxSIZE_AUTO);
   void SetClientSize(int width, int height);
   void SetFocus(void);
   void SetLabel(char *label);
   char *GetLabel(void);

   Bool Show(Bool show);
   float GetCharHeight(void);
   float GetCharWidth(void);

   inline virtual void SetBackgroundColour(wxColour*col) { backColour = col; };
   inline virtual void SetLabelColour(wxColour*col) { labelColour = col ; };
   inline virtual void SetButtonColour(wxColour*col) { buttonColour = col ; };

   // Windows subclassing
   void SubclassControl(HWND hWnd);
   void UnsubclassControl(HWND hWnd);

   // For ownerdraw items
   virtual Bool MSWOnDraw(DRAWITEMSTRUCT *WXUNUSED(item)) { return FALSE; };
   virtual Bool MSWOnMeasure(MEASUREITEMSTRUCT *WXUNUSED(item)) { return FALSE; };
};

long NewId(wxItem *i);
void DoneIds(wxItem *i);

#endif // IN_CPROTO
#endif // wx_itemh
