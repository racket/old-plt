/*
 * File:	wx_lay.h
 * Purpose:	Experimental constraint-based layout classes
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

#ifndef wx_layh
#define wx_layh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"

class wxWindow;

// X stupidly defines these in X.h
#ifdef Above
#undef Above
#endif
#ifdef Below
#undef Below
#endif

#define wxLAYOUT_DEFAULT_MARGIN 0

enum wxEdge { wxLeft, wxTop, wxRight, wxBottom, wxWidth, wxHeight,
  wxCentre, wxCenter = wxCentre, wxCentreX, wxCentreY };
enum wxRelationship { wxUnconstrained = 0,
                      wxAsIs,
                      wxPercentOf,
                      wxAbove,
                      wxBelow,
                      wxLeftOf,
                      wxRightOf,
                      wxSameAs,
                      wxAbsolute };

class wxLayoutConstraints;
class wxIndividualLayoutConstraint: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxIndividualLayoutConstraint)

 protected:
   // 'This' window is the parent or sibling of otherWin
   wxWindow *otherWin;

   wxEdge myEdge;
   wxRelationship relationship;
   int margin;
   int value;
   int percent;
   wxEdge otherEdge;
   Bool done;

 public:
   wxIndividualLayoutConstraint(void);
   ~wxIndividualLayoutConstraint(void);

  void Set(wxRelationship rel, wxWindow *otherW, wxEdge otherE, int val = 0, int marg = wxLAYOUT_DEFAULT_MARGIN);

  //
  // Sibling relationships
  //
  void LeftOf(wxWindow *sibling, int marg = wxLAYOUT_DEFAULT_MARGIN);
  void RightOf(wxWindow *sibling, int marg = wxLAYOUT_DEFAULT_MARGIN);
  void Above(wxWindow *sibling, int marg = wxLAYOUT_DEFAULT_MARGIN);
  void Below(wxWindow *sibling, int marg = wxLAYOUT_DEFAULT_MARGIN);

  //
  // 'Same edge' alignment
  //
  void SameAs(wxWindow *otherW, wxEdge edge, int marg = wxLAYOUT_DEFAULT_MARGIN);

  // The edge is a percentage of the other window's edge
  void PercentOf(wxWindow *otherW, wxEdge wh, int per);

  //
  // Edge has absolute value
  //
  void Absolute(int val);

  //
  // Dimension is unconstrained
  //
  inline void Unconstrained(void) { relationship = wxUnconstrained; }

  //
  // Dimension is 'as is' (use current size settings)
  //
  inline void AsIs(void) { relationship = wxAsIs; }

  //
  // Accessors
  //
  inline wxWindow *GetOtherWindow(void) { return otherWin; }
  inline wxEdge GetMyEdge(void) { return myEdge; }
  inline void SetEdge(wxEdge which) { myEdge = which; }
  inline void SetValue(int v) { value = v; }
  inline int GetMargin(void) { return margin; }
  inline int GetValue(void) { return value; }
  inline int GetPercent(void) { return percent; }
  inline int GetOtherEdge(void) { return otherEdge; }
  inline Bool GetDone(void) { return done; }
  inline void SetDone(Bool d) { done = d; }
  inline wxRelationship GetRelationship(void) { return relationship; }
  inline void SetRelationship(wxRelationship r) { relationship = r; }

  // Reset constraint if it mentions otherWin
  Bool ResetIfWin(wxWindow *otherW);

  // Try to satisfy constraint
  Bool SatisfyConstraint(wxLayoutConstraints *constraints, wxWindow *win);

  // Get the value of this edge or dimension, or if this
  // is not determinable, -1.
  int GetEdge(wxEdge which, wxWindow *thisWin, wxWindow *other);
};

class wxLayoutConstraints: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxLayoutConstraints)

 public:
  // Edge constraints
  wxIndividualLayoutConstraint left;
  wxIndividualLayoutConstraint top;
  wxIndividualLayoutConstraint right;
  wxIndividualLayoutConstraint bottom;
  // Size constraints
  wxIndividualLayoutConstraint width;
  wxIndividualLayoutConstraint height;
  // Centre constraints
  wxIndividualLayoutConstraint centreX;
  wxIndividualLayoutConstraint centreY;

  wxLayoutConstraints(void);
  ~wxLayoutConstraints(void);

  Bool SatisfyConstraints(wxWindow *win, int *noChanges);
};

Bool wxOldDoLayout(wxWindow *win);

/*

Algorithm:

 Each sizer has a Layout function.

 wxExpandSizer::Layout                  ; E.g. for resizeable windows
 
   - parent size must be known (i.e. called
      from OnSize or explicitly)
   - call Layout on each child to give it a chance to resize
     (e.g. child shrinks around its own children):
     stop when all children return TRUE, or no change
   - evaluate constraints on self to set size

 wxShrinkSizer::Layout                  ; E.g. fit-to-contents windows
                                        ; Perhaps 2 rowcols, one above other.
 
   - call Layout on each child to give it a chance to resize
     (e.g. child shrinks around its own children):
     stop when each returns TRUE, or no change
   - fit around children
        (what if some want to be centred? E.g. OK/Cancel rowcol.
         - done by centring e.g. bottom sizer w.r.t. top sizer.
           (sibling relationship only))
   - evaluate own constraints (e.g. may be below another window)
   - IF parent is a real window (remember: a real window can
     have only one child sizer, although a sizer can have several child
     (real) windows), then resize this parent WITHOUT invoking Layout
     again.
     Frame and dialog box OnSizes can check if the sizer is a shrink
     sizer; if not, can call layout. Maybe have virtual Bool AutoSizeLayout()
     to determine this.

How to relayout if a child sizer/window changes? Need to go all the way
to the top of the hierarchy and call Layout() again.
     
 wxRowColSizer::Layout

   - Similar to wxShrinkSizer only instead of shrinking to fit
     contents, more sophisticated layout of contents, and THEN
     shrinking (possibly).
   - Do the same parent window check/setsize as for wxShrinkSizer.
 
*/

typedef enum {
    wxSizerShrink,
    wxSizerExpand,
    wxSizerNone
} wxSizerBehaviour;

#define wxTYPE_SIZER        90

class wxSizer: public wxWindow
{
  DECLARE_DYNAMIC_CLASS(wxSizer)

 private:
 protected:
  wxSizerBehaviour sizerBehaviour;
  int borderX;
  int borderY;
  int sizerWidth;
  int sizerHeight;
  int sizerX;
  int sizerY;
 public:
  wxSizer(void);
  wxSizer(wxWindow *parent, wxSizerBehaviour behav = wxSizerNone);
  ~wxSizer(void);

  Bool Create(wxWindow *parent, wxSizerBehaviour behav = wxSizerNone);
  virtual void SetSize(int x, int y, int w, int h, int flags = wxSIZE_AUTO);
  // Avoid compiler warning
  void SetSize(int w, int h) { wxWindow::SetSize(w, h); }
  virtual void Move(int x, int y);
  virtual void GetSize(int *w, int *h);
  inline virtual void GetClientSize(int *w, int *h) { GetSize(w, h); }
  virtual void GetPosition(int *x, int *y);

  inline void SizerSetSize(int x, int y, int w, int h)
    { SetSize(x, y, w, h); }
  inline void SizerMove(int x, int y)
    { Move(x, y); }

  inline void SetBorder(int w, int h) { borderX = w; borderY = h; }
  inline int GetBorderX(void) { return borderX ; }
  inline int GetBorderY(void) { return borderY ; }

  virtual void AddSizerChild(wxWindow *child);
  virtual void RemoveSizerChild(wxWindow *child);

  inline virtual void SetBehaviour(wxSizerBehaviour b) { sizerBehaviour = b; }
  inline virtual wxSizerBehaviour GetBehaviour(void) { return sizerBehaviour; }

  virtual Bool LayoutPhase1(int *);
  virtual Bool LayoutPhase2(int *);
};

#define wxSIZER_ROWS  TRUE
#define wxSIZER_COLS  FALSE

class wxRowColSizer: public wxSizer
{
  DECLARE_DYNAMIC_CLASS(wxRowColSizer)

 private:
 protected:
  Bool rowOrCol;
  int rowOrColSize;
  int xSpacing;
  int ySpacing;
 public:
  // rowOrCol = TRUE to be laid out in rows, otherwise in columns.
  wxRowColSizer(void);
  wxRowColSizer(wxWindow *parent, Bool rowOrCol = wxSIZER_ROWS, int rowsOrColSize = 20, wxSizerBehaviour = wxSizerShrink);
  ~wxRowColSizer(void);

  Bool Create(wxWindow *parent, Bool rowOrCol = wxSIZER_ROWS, int rowsOrColSize = 20, wxSizerBehaviour = wxSizerShrink);
  void SetSize(int x, int y, int w, int h, int flags = wxSIZE_AUTO);
  // Avoid compiler warning
  void SetSize(int w, int h) { wxSizer::SetSize(w, h); }

  inline virtual void SetRowOrCol(Bool rc) { rowOrCol = rc; }
  inline virtual Bool GetRowOrCol(void) { return rowOrCol; }
  inline virtual void SetRowsOrColSize(int n) { rowOrColSize = n; }
  inline virtual int GetRowOrColSize(void) { return rowOrColSize; }
  inline virtual void SetSpacing(int x, int y) { xSpacing = x; ySpacing = y; }
  inline virtual void GetSpacing(int *x, int *y) { *x = xSpacing; *y = ySpacing; }

  Bool LayoutPhase1(int *);
  Bool LayoutPhase2(int *);
};


#endif
