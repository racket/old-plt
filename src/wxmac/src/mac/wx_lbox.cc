/*
 * File:	wx_lbox.cc
 * Purpose:	Panel item listBox implementation (Macintosh version)
 * Author:	Cecil Coupe (Jul 8, 1995)
 * Created:	1994
 * Updated:	
 *		11/1/95 - not deleting client data on delete or clear
 * Copyright:	(c) 2004 PLT Scheme, Inc.
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

#include "common.h"
#include "wx_lbox.h"
#include "wx_utils.h"
#include "wx_rbox.h"
#include "wx_rbut.h"
#include "wx_messg.h"
#include "wxMacDC.h"
#include "wx_stdev.h"
#include "wx_gdi.h"
#include "wx_area.h"
#include "wxBorderArea.h"
#include "wxRectBorder.h"
#include "wxTimeScale.h"

#define MEANING_CHARACTER	'0'
/* 
   A wxWindows (Macintosh version) wxListBox has these (sub)components:
   1.  a Title
   old 2.  a Mac List Manager "control"  ; NOT ANYMORE:
   2.  an "A List" control
   3.  Optional Scrollbars (vertical only)
   4.  BorderBox drawn round the list control but not the title or scrollbar

   These are grouped/managed/defined by:
   Creating the Mac list box control in the ClientArea
   Creating a wxBorderArea (The Toolbox list mgr does not draw borders around
   the control)
   Creating the wxLabelArea for the title (aka label);

   wxItem protocol needed for showing the wxListBox on a panel:
   1. A constructor of course thats makes a stab at sizing the Mac list box
   and groups the items as described above.
   2  A wxListBox::Paint method that calls LUpdate on the Mac list box
   and calls OnPaint() - to draw the wxLabelArea and wxBorderArea.
   3. wxListBox::OnClientAreaDSize is needed to actually resize or reposition
   the Mac List Box (after the label and bounding box are created).

   The Mac list manager requires certain Mac events:
   1. Update - call LUpdate which handles the scroll bar and the list control
   2. MouseDown (but not all mouse events, trust me)
   3. Activate ???

   mflatt: Anytime the list might redraw itself, call SetCurrentDC() first; this
   gets the GrafPtr into the proper coordinate system.

   List Manager hints:
   1. Note that SetRect(...) and Rect x = {...} args are in different order
   This causes me all kinds of errors and the List Mgr often doesn't
   display anything if the BoundsRect is wrong!

   To do:
   1. Compute a cell height based on the font used (currently hardcoded at 12)
   Also, the font used is different from wxText and RadioButtons.
   2. Load the list in the constructor if N is non zero
   3. This could be leaking memory ?
   */

// FIXME: Catch SetFont and fixup cell size and indent

#define DefItemWidth 50
#define KSBWidth	15	// number of pixels of a vertical scroll bars' width
#define VIEW_RECT_OFFSET 3

// Constructor

wxListBox::wxListBox(
		     wxPanel *parentPanel, 
		     wxFunction func,
		     char *Title, 
		     Bool Multiple,
		     int x, 
		     int y, 
		     int width, 
		     int height,
		     int N, 
		     char **Choices, 
		     long style, 
		     char *windowName,
		     WXTYPE		objectType
		     ) 
: wxbListBox (parentPanel, func, Title, Multiple, x, y, width, height, N, Choices, style, windowName)
{
  cDataList = new wxList(wxKEY_INTEGER);
  Create(parentPanel, func, Title, Multiple, x, y, width, height, N, Choices,
         style, windowName);
}

Boolean cellWasClicked = false;
Boolean MyClickInCell(ALCellPtr const theCell, Point mouseLoc, EventModifiers modifiers, short numberClicks, ALReference hAL);
#ifdef OS_X
static ALHiliteCellUPP origHilite;
#endif

Boolean MyClickInCell(ALCellPtr const theCell, Point mouseLoc, EventModifiers modifiers, short numberClicks, ALReference hAL)
{
  cellWasClicked = true;

  return true;
}

static void MyDrawCell(ALData cellData, ALCellPtr cell, const Rect *cellRect, ALReference hAL)
{
  if (cellData) {
    FontInfo fontInfo;
    ::GetFontInfo(&fontInfo);
    MoveTo(cellRect->left, fontInfo.ascent + cellRect->top);

    ForeColor(blackColor);
    CopyPascalStringToC(*(StringHandle)cellData, wxBuffer);
    TextMode(srcOr);
    DrawLatin1Text(wxBuffer, 0, -1, 0);
  }
}

#ifdef OS_X
static void MyHiliteCell(ALData cellData, ALCellPtr cell, 
			 Boolean active, Boolean doOutline,
			 const Rect *cellRect, ALReference hAL)
{
  /* Hiliting on top of anti-aliased text looks bad. To make it
     look good, highlight an empty cell, then re-paint the text. */
  EraseRect(cellRect);
  origHilite(cellData, cell, active, doOutline, cellRect, hAL);
  MyDrawCell(cellData, cell,cellRect, hAL);
}
#endif

static ALClickCellUPP MyClickInCellUPP = NewALClickCellProc(MyClickInCell);
static ALDrawCellUPP MyDrawCellUPP = NewALDrawCellProc(MyDrawCell);
#ifdef OS_X
static ALHiliteCellUPP MyHiliteCellUPP = NewALHiliteCellProc(MyHiliteCell);
#endif

Bool wxListBox::Create(wxPanel *panel, wxFunction func,
                       char *Title, Bool Multiple,
                       int x, int y, int width, int height,
                       int N, char **Choices, long style, char *name)
{
  float lblWidth, lblHeight;
  float tWidth, tHeight, tDescent;
  int boxHeight, boxWidth;
  CGrafPtr theMacGrafPort;
  int vscrollwanted = TRUE;
  LongRect dataRect = {0, 0, 0 ,1 };
  int cellwid;
  Point cellsize;
  Rect viewRect;
  OSErr result;  
  unsigned long flags;

  SetEraser(wxWHITE_BRUSH);

  selected = -1;
  selections = 0;
  multiple = Multiple & wxMULTIPLE_MASK;
  no_items = 0;
  cKeycnt =0;

  if (!buttonFont)
    buttonFont = wxNORMAL_FONT;

  font = buttonFont;
  
  if (Title)
    Title = wxItemStripLabel(Title);
  
  Callback(func);
  SetCurrentDC();
  theMacGrafPort = cMacDC->macGrafPort();

  GetTextExtent(Title, &lblWidth, &lblHeight, NULL, NULL, labelFont);
  GetTextExtent("X", &tWidth, &tHeight, &tDescent, NULL, font);
  
  if (width < 0) {
    cWindowWidth = (int)((labelPosition == wxVERTICAL ? 0 : lblWidth) + DefItemWidth + KSBWidth + 2 * VIEW_RECT_OFFSET);
  }
  if (height < 0) {
    cWindowHeight = (int)((labelPosition == wxVERTICAL ? lblHeight : 0) + 4 * tHeight + 2 * VIEW_RECT_OFFSET) ;
  }
  
  boxHeight = cWindowHeight;
  boxWidth = cWindowWidth;
  
  // mflatt: wxNEEDED_SB = 0; if it's on, this code doesn't notice:
  // int vscrollwanted = (style & (wxNEEDED_SB | wxALWAYS_SB)) || 
  //	(Multiple & (wxNEEDED_SB | wxALWAYS_SB));
  // Since the choice is either NEEDED or ALWAYS, presumably we always want it on: 
  // start with no rows or columns, thats what the '1' {means in B, R}
  cellwid = boxWidth - (vscrollwanted ? KSBWidth : 0);
  cellsize.v = (int)tHeight;
  cellsize.h = cellwid;
  ::SetRect(&viewRect, VIEW_RECT_OFFSET, VIEW_RECT_OFFSET, cellwid - VIEW_RECT_OFFSET, boxHeight - VIEW_RECT_OFFSET);
  cHaveVScroll = vscrollwanted;			// needed by OnClientAreaDSize or Paint

  flags = (alDoVertScroll
	   | alDoDynamicScroll
	   | alDoDrawFocus
	   | alDoDrawRect
#ifndef OS_X
	   | alDoDrawOffscreen
#endif
	   | alDoRowsOnly);
  
  if (!(multiple & (wxMULTIPLE | wxEXTENDED))) {
    flags = flags | alDoSelOnlyOne;
  }
  
  ::OffsetRect(&viewRect,SetOriginX,SetOriginY);
  result = ::ALNew(GetWindowFromPort(theMacGrafPort), &viewRect, &dataRect, cellsize,
		   flags, &cListReference);
  
  if (result != noErr) {
    wxOutOfMemory();
  }
  
  ALSetInfo(alClickCellHook,&MyClickInCellUPP,cListReference);
  ALSetInfo(alDrawCellHook,&MyDrawCellUPP,cListReference);
#ifdef OS_X
  ALGetInfo(alHiliteCellHook,&origHilite,cListReference);
  ALSetInfo(alHiliteCellHook,&MyHiliteCellUPP,cListReference);
#endif

  ReleaseCurrentDC();

  if (Title) {
    cListTitle = new wxLabelArea(this, Title, labelFont,
				 labelPosition == wxVERTICAL ? wxTop : wxLeft);
  } else
    cListTitle = NULL;
  
  if (N)
    this->Set(N, Choices);
  
  {
    wxWindow *p;
    p = GetParent();
    if (p->IsHidden())
      DoShow(FALSE);
  }
  if (style & wxINVISIBLE)
    Show(FALSE);
  InitInternalGray();

  OnClientAreaDSize(1, 1, 1, 1);

  return TRUE;
}

wxListBox::~wxListBox(void)
{
  if (cDataList)
    DELETE_OBJ cDataList;
  ::ALDispose(cListReference);
}

//------------ Event Handling --------------------------------------
void wxListBox::Paint(void)
{
  RgnHandle visibleRgn;

  if (cHidden) return;

  SetCurrentDC();
  visibleRgn = NewRgn();
  if (visibleRgn) {
    GetPortVisibleRegion(cMacDC->macGrafPort(),visibleRgn);
    ::ALUpdate(visibleRgn, cListReference);
    DisposeRgn(visibleRgn);
  }
  ReleaseCurrentDC();
  
  wxWindow::Paint();
}

void wxListBox::OnClientAreaDSize(int dW, int dH, int dX, int dY)
{
  if (cHidden) return;

  if (dX || dY) {
    // Changing the position
    cMacDC->setCurrentUser(NULL); // macDC no longer valid
  }
  
  MoveBox(dW, dH, dX, dY);

  wxWindow::OnClientAreaDSize(dW, dH, dX, dY);
}

void wxListBox::OnEvent(wxMouseEvent *event)
{
  if (event->ButtonDown()) {
    int startH;
    int startV;
    Point startPt;
    int modifiers = 0;
    Bool doubleclick;
    ALCell cell;

    event->Position(&startH,&startV); // client c.s.

    SetCurrentDC();
    
    startPt.v = startV + SetOriginY; // port c.s.
    startPt.h = startH + SetOriginX;
    if (event->shiftDown)
      modifiers += shiftKey;
    if (event->altDown)
      modifiers += optionKey;
    if (event->metaDown
	|| ((multiple & wxEXTENDED) && !event->shiftDown))
      modifiers += cmdKey;
    if (event->rightDown)  // mflatt: right button is control-click
      modifiers += controlKey;

    wxTracking();

    doubleclick = ::ALClick(startPt, modifiers, UNSCALE_TIMESTAMP(event->timeStamp),cListReference);

    ReleaseCurrentDC();
    
    ALLastClick(&cell, cListReference);

    if (cell.v > -1) {
      wxCommandEvent *commandEvent;
      
      if (cellWasClicked && doubleclick)
	commandEvent = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND);
      else
	commandEvent = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);

      cellWasClicked = false;			
  
      ProcessCommand(commandEvent);    
    }
  }
}

// mflatt:
void wxListBox::OnChar(wxKeyEvent *event)
{
  int move = 0;

  switch (event->KeyCode()) {
  case WXK_UP:
  case WXK_LEFT:
    move = -1;
    break;
  case WXK_DOWN:
  case WXK_RIGHT:
    move = 1;
    break;
  case WXK_RETURN:
  case /* WXK_ENTER */ 3:
    wxPanel *panel;
    panel = (wxPanel *)GetParent();
    panel->OnDefaultAction(this);
    break;
  }

  if (move) {
    ALCell now, next, save;
    wxCommandEvent *commandEvent;

    now.h = now.v = 0;

    SetCurrentDC();

    if (ALGetSelect(TRUE, &now, cListReference)) {
      if (!(ALFeatureFlag(alFSelOnlyOne,alBitTest,cListReference)) && (move > 0)) {
	// moving forward for multiple selections: find last selected
	ALCell next;

	next.h = now.h;
	next.v = now.v + 1;
	while (ALGetSelect(TRUE, &next, cListReference)) {
	  now.v = next.v;
	  next.v++;
	}
      }
    } else if (move < 0)
      now.v = 1;
    else if (move > 0)
      now.v = no_items - 2;

    next.h = now.h;
    next.v = now.v + move;
    if (next.v < 0 || next.v >= no_items) {
      save.h = now.h;
      save.v = now.v;
      next.v = now.v;
    } else
      save.h = save.v = -1;

    if ((!event->shiftDown && !event->metaDown) || 
	(ALFeatureFlag(alFSelOnlyOne,alBitTest,cListReference))) {
      int i;
      LongPt next;
      next.h = now.h;
      for (i = 0; i < no_items; i++) {
	if (i != save.v) {
	  next.v = i;
	  ALSetSelect(FALSE, &next, cListReference);
	}
      }
    }

    ALSetSelect(TRUE, &next, cListReference);
    
    {
      Point cellSize;
      Rect rView, rect;

      ALGetCellSize(&cellSize, cListReference);
      ALGetViewRect(&rView, cListReference);

      ALGetCellRect(&rect, &next, cListReference);
      if (rect.top < rView.top) {
	int diff = (rect.top - rView.top);
	int amt = diff / cellSize.v;
	if (diff % cellSize.v)
	  --amt;
	ALScrollCells(0, -amt, cListReference);
      } else if (rect.bottom > rView.bottom) {
	int diff = rect.bottom - rView.bottom;
	int amt = diff / cellSize.v;
	if (diff % cellSize.v)
	  amt++;
	ALScrollCells(0, -amt, cListReference);
      }
    }

    ReleaseCurrentDC();

    commandEvent = new wxCommandEvent(wxEVENT_TYPE_LISTBOX_COMMAND);
    ProcessCommand(commandEvent);
  }
}

Bool wxListBox::WantsFocus(void)
{
  return TRUE;
}

// ---------------- Routines to Handle the List Contents ------------------------------------

void wxListBox::Delete(int N)
{ 
  // (JDH) need to add code here to take care of clientDataList
  wxNode *node;
  node = cDataList->Find((long)N);  // get item from list
  if (node) cDataList->DeleteNode(node);    // if existed then delete from list
  node = cDataList->First();                // we now have to adjust all keys that 
  while (node)                                  // are >=N+1
    { if (node->integer_key >= (long)(N+1))      // very ugly C++ wise but no other way 
	node->integer_key--;                     // to look at or change key value
      node = node->Next();
    }
  SetCurrentDC();
  ALDelRow(1, N, cListReference);
  ReleaseCurrentDC();
  no_items --;
  cKeycnt--;
}

// Append an item to the list box
void wxListBox::Append(char *Item, char *Client_data)
{
  Str255 temp;
  StringHandle stringHandle;
  LongPt cell;

  CopyCStringToPascal(Item,temp);
  stringHandle = NewString(temp);
  
  SetCurrentDC();
  //LSetDrawingMode(FALSE, cListHandle);
  cell.v = no_items;
  cell.h = 0;		// Point = {v, h} so Cell = {row, col}
  ALAddRow(1,no_items, cListReference);
  ALSetCell((void *)stringHandle, &cell, cListReference);
  // LDraw(cell, cListHandle); // mflatt: can't get this to work; co-ordinate problems?
  // LSetDrawingMode(TRUE, cListHandle);
  cDataList->Append(no_items, (wxObject *)Client_data);
  no_items ++;
  if (Client_data) cKeycnt++;
  
  ReleaseCurrentDC();
}

void wxListBox::Append(char *Item)
{
  Append(Item, NULL);
}

void wxListBox::Set(int n, char *choices[])
{
  Str255 temp;
  LongPt cell = {0, 0};		// Point = {v, h} so Cell = {row, col}
  StringHandle stringHandle;

  if (no_items > 0) {
    this->Clear();
  }
  SetCurrentDC();
  // LSetDrawingMode(FALSE, cListHandle);
  ALAddRow(n,0,cListReference); // add all the rows up front.
  for (cell.v = 0; cell.v < n; cell.v++) {
    CopyCStringToPascal(choices[cell.v],temp);
    stringHandle = NewString(temp);
    cDataList->Append(cell.v, (wxObject *)NULL);
    ALSetCell(stringHandle ,&cell, cListReference);
  }
  no_items = cell.v;
  ReleaseCurrentDC();
}

Boolean MyStringCompare(Handle, ConstStr255Param);

Boolean MyStringCompare(Handle cellData, ConstStr255Param searchData) {
  return EqualString((unsigned char *)*cellData,searchData,(unsigned char)TRUE,(unsigned char)TRUE);
}

ALSearchUPP strcmpUPP = NewALSearchProc(MyStringCompare);

int wxListBox::FindString(char *s)
{
  LongPt cell;
  StringPtr pstr;
  char *ss;
  int result;

  cell.h = cell.v = 0;

  ss = new char[strlen(s) + 1];
  pstr = (StringPtr)ss;
  
  CopyCStringToPascal(s,pstr);
  
  result = (ALSearch(pstr, strcmpUPP, &cell, cListReference) ? cell.v : -1);

  return result;
}

void wxListBox::Clear(void)
{
  if (no_items<=0)
    return ;
  cDataList->Clear ();
  SetCurrentDC();
  ALDelRow(ALGetNumberRows(cListReference), 0, cListReference);
  ReleaseCurrentDC();
  no_items = 0;
  cKeycnt = 0;
}

// Find string for position
char *wxListBox::GetString(int N)
{
  if ((N < 0) || (N >= no_items))
    return NULL;

  {
    LongPt cell = {N, 0};
    StringHandle stringHandle;
    OSErr result;
    
    result = ALGetCell((void **)&stringHandle, &cell, cListReference);
    if (result != noErr) {
      return NULL;
    }
    
    CopyPascalStringToC(*stringHandle,wxBuffer);
    return wxBuffer;
  }
}

void wxListBox::SetString(int N, char *s)
{
  LongPt cell = {N, 0};
  StringHandle oldHandle; 
  Str255 temp;
  StringHandle newHandle;

  SetCurrentDC();
  ALGetCell((void **)&oldHandle,&cell,cListReference);
  CopyCStringToPascal(s,temp);
  newHandle = NewString(temp);
  ALSetCell(newHandle, &cell, cListReference);
  DisposeHandle((Handle)oldHandle);
  ReleaseCurrentDC();
}

char *wxListBox::GetClientData(int N)
{
  wxNode *cdt;

  cdt = cDataList->Find(N);
  if (cdt)
    return((char *)cdt->Data());
  else
    return NULL;
}

void wxListBox::SetClientData(int N, char *s)
{
  wxNode *cdt;

  cdt = cDataList->Find(N);
  if (cdt)
    cdt->SetData((wxObject *)s);
}

// Undocumented !
void wxListBox::InsertItems(int nItems, char **Items, int pos) 
{
  //LSetDrawingMode(FALSE, cListHandle);
  LongPt cell = {pos, 0};		// Point = {v, h} so Cell = {row, col}
  int n;
  StringHandle stringHandle;
  Str255 temp;

  ALAddRow(nItems,cell.v,cListReference);
  for (n = 0;  n < nItems; cell.v++, n++) {
    cDataList->Append(cell.v, (wxObject *)NULL);
    CopyCStringToPascal(Items[n],temp);
    stringHandle = NewString(temp);
    ALSetCell(stringHandle, &cell, cListReference);
  }
  no_items = no_items + nItems;
  // LSetDrawingMode(TRUE, cListHandle);
  // Paint();
}

// ------ Manage Selections ----------------------

void wxListBox::SetFirstItem(int N)
{
  int vi, fi;

  vi = NumberOfVisibleItems();
  fi = GetFirstItem();
  
  if (N > no_items - vi)
    N = no_items - vi;
  if (N < 0)
    return;

  if (N == fi)
    return;

  SetCurrentDC();

  ALScrollCells(0, fi - N, cListReference);

  ReleaseCurrentDC();
}

void wxListBox::SetFirstItem(char *s) 
{
  int N;

  N = FindString(s) ;

  if (N>=0)
    SetFirstItem(N) ;
}

int wxListBox::GetFirstItem()
{
  Rect rect, crect;
  Point size;
  LongPt next = {0, 0};
  int item, d;
  
  ALGetCellSize(&size, cListReference);     
  ALGetViewRect(&rect, cListReference);
  ALGetCellRect(&crect, &next, cListReference);
  d = rect.top - crect.top;
  if (d < 0)
    return 0;
  item = d / size.v;
  if (d % size.v)
    item++;
  
  return item;
}

int wxListBox::NumberOfVisibleItems()
{
  Rect view;
  Point size;
  int r;
     
  ALGetCellSize(&size, cListReference);     
  ALGetViewRect(&view, cListReference);

  r = (view.bottom - view.top) / size.v;
  if (r < 1)
    r  = 1;
     
  return r;
}

void wxListBox::SetSelection(int N, Bool select, Bool just_one)
{
  Boolean onlyOne;
  
  if (N < 0 || (N >= no_items))
    return;

  SetCurrentDC();

  onlyOne = ALFeatureFlag(alFSelOnlyOne,alBitTest,cListReference);

  if (select && (just_one || onlyOne)) {
    int s;
    s = GetSelection();
    if (s == N)
      return;
    if (s >= 0) {
      LongPt cell = {s, 0};
      ALSetSelect(FALSE, &cell, cListReference);
    }
  }

  {
    LongPt cell = {N, 0};
    ALSetSelect(select, &cell, cListReference);
  }

  ReleaseCurrentDC();
}

void wxListBox::SetOneSelection(int N)
{
  SetSelection(N, TRUE, TRUE);
}

Bool wxListBox::Selected(int N)
{
  LongPt cell = {N, 0};
  return ALGetSelect(FALSE, &cell, cListReference);
}

void wxListBox::Deselect(int N)
{
  LongPt cell = {N, 0};
  SetCurrentDC();
  ALSetSelect(FALSE, &cell, cListReference);
  ReleaseCurrentDC();
}

// Return number of selections and an array of selected integers
// Use selections field to store data, which will be cleaned up
// by destructor if necessary.
int wxListBox::GetSelections(int **list_selections)
{
  LongPt cell = {0, 0};
  long n;

  n = ALGetNumberSelected(cListReference);
  
  if (n <= 0)
    return 0;
  cell.h = 0; cell.v = 0;
  selections = new int[n];
  n = 0;
  while (ALGetSelect(TRUE, &cell, cListReference)) {
    selections[n++] = cell.v++;
  }
  *list_selections = selections;
  return n;
}

// Get single selection, for single choice list items
int wxListBox::GetSelection(void)
{
  int r;
  LongPt cell = {0, 0};

  r = ALGetSelect(TRUE, &cell, cListReference);
  if (r == FALSE)
    return -1;
  else
    return cell.v;
}

void wxListBox::SetBackgroundColour(wxColour*col)
{
} 

void wxListBox::SetLabelColour(wxColour*col)
{
}

void wxListBox::SetButtonColour(wxColour*col) 
{
}

char* wxListBox::GetLabel(void)
{
  return (cListTitle ? cListTitle->GetLabel() : NULL);
}

void wxListBox::SetLabel(char *label)
{
  if (cListTitle) cListTitle->SetLabel(label);
}

//-----------------------------------------------------------------------------
void wxListBox::OnSetFocus()
{
  SetCurrentDC();
  ALSetFocus(kControlListBoxPart,cListReference);
  ReleaseCurrentDC();

  wxWindow::OnSetFocus();
}

//-----------------------------------------------------------------------------
void wxListBox::OnKillFocus()
{
  SetCurrentDC();  
  ALSetFocus(kControlFocusNoPart,cListReference);
  ReleaseCurrentDC();

  wxWindow::OnKillFocus();
}

//-----------------------------------------------------------------------------

void wxListBox::DoShow(Bool on)
{
  if (!CanShow(on))
    return;
  
  ALFeatureFlag(alFInhibitRedraw, on ? alBitClear : alBitSet, cListReference);

  wxWindow::DoShow(on);

  if (on) {
    /* May need to do some things we skipped while hidden: */
    int v;
    OnClientAreaDSize(1, 1, 1, 1);
    v = OS_Active();
    ALActivate(cActive && v, cListReference);
  }
}

void wxListBox::InternalGray(int gray_amt)
{
  if (cListTitle) {
    wxLabelArea *la;
    wxWindow *w;
    la = (wxLabelArea *)cListTitle;
    w = la->GetMessage();
    w->InternalGray(gray_amt);
  }
  wxItem::InternalGray(gray_amt);
}

void wxListBox::ChangeToGray(Bool gray)
{
  if (cHidden) return;
  
  SetCurrentDC();
  ALActivate(!gray && cActive, cListReference);
  ReleaseCurrentDC();

  wxWindow::ChangeToGray(gray);
}

void wxListBox::Activate(Bool on)
{
  if (!cHidden) {
    int v;
    SetCurrentDC();
    v = OS_Active();
    ALActivate(on && v, cListReference);
    ReleaseCurrentDC();
  }

  wxItem::Activate(on);
}

void wxListBox::ReleaseCurrentDC(int really)
{
  wxWindow::ReleaseCurrentDC(1);
}

void wxListBox::MoveBox(int dW, int dH, int dX, int dY)
{  
  Rect viewRect;
  int clientWidth;
  wxArea *carea;

  SetCurrentDC();
   
  carea = ClientArea();

  viewRect.top = VIEW_RECT_OFFSET + 1;
  viewRect.bottom = carea->Height() - VIEW_RECT_OFFSET;
  viewRect.left = VIEW_RECT_OFFSET;
  viewRect.right = carea->Width() - VIEW_RECT_OFFSET;
  
  clientWidth = carea->Width() - VIEW_RECT_OFFSET;
  /*if (cHaveVScroll)*/ clientWidth -= KSBWidth;

  OffsetRect(&viewRect,SetOriginX,SetOriginY);

  if (dW || dH || dX || dY)
    ALSetViewRect(&viewRect, cListReference);

  if (dW || dH) {
    // Changing the size
    Rect cellRect;
    LongPt cell = {0,0};
    Point size;

    ALGetCellRect(&cellRect,&cell,cListReference);
    size.v = cellRect.bottom - cellRect.top;
    size.h = clientWidth;
    ALSetCellSize(size, cListReference);
  }

  if (!cHidden && (dW || dH || dX || dY)) {
    ::InvalWindowRect(GetWindowFromPort(cMacDC->macGrafPort()),&viewRect);
  }
  
  ReleaseCurrentDC();
}

void wxListBox::MaybeMoveControls()
{
  MoveBox(1, 1, 0, 0);
}
