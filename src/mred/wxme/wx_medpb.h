
/* Included by wx_media.h */

class wxMediaPasteboard : public wxMediaBuffer
{
 protected:
  Bool ReadInsert(wxSnip *snip);
  void InsertPasteSnip(wxSnip *snip, wxBufferData *);
  void InsertPasteString(wxchar *str);
 public:
  wxMediaPasteboard();
  ~wxMediaPasteboard();

  virtual void OnEvent(wxMouseEvent *event);
  virtual void OnChar(wxKeyEvent *event);
  virtual wxCursor *AdjustCursor(wxMouseEvent *event);
  virtual void Refresh(float localx, float localy, float w, float h, 
		       int show_caret, wxColour *c);
  virtual void OwnCaret(Bool ownit);
  virtual void BlinkCaret();
  virtual void SizeCacheInvalid(void);
  void GetExtent(float *w, float *h);

  virtual void OnDefaultEvent(wxMouseEvent *event);
  virtual void OnDefaultChar(wxKeyEvent *event);

  virtual void OnDoubleClick(wxSnip *, wxMouseEvent *event);

  /* Callbacks for the wxSnipAdmin: */
  virtual Bool ScrollTo(wxSnip *, float localx, float localy, 
			float w, float h, Bool refresh, int bias = 0);
  virtual void SetCaretOwner(wxSnip *, int = wxFOCUS_IMMEDIATE);
  virtual void Resized(wxSnip *, Bool redraw_now);
  virtual Bool Recounted(wxSnip *, Bool redraw_now);
  virtual void NeedsUpdate(wxSnip *, float localx, float localy, 
			   float w, float h);
  virtual Bool ReleaseSnip(wxSnip *);

  float ScrollLineLocation(long line);
  long NumScrollLines();
  long FindScrollLine(float y);

  wxMediaBuffer *CopySelf(void);
  void CopySelfTo(wxMediaBuffer *b);
  float GetDescent(void);
  float GetSpace(void);
  wxchar *GetFlattenedText(long *got=NULL);
  void GetCenter(float *x, float *y);

  void Clear();
  void Cut(Bool extend=FALSE, long time=0);
  void Copy(Bool extend=FALSE, long time=0);
  void Paste(long time=0);
  void Kill(long time=0);
  void SelectAll(void);

  virtual void DoCopy(long time, Bool extend);
  virtual void DoPaste(long time);

  void SetSelected(wxSnip *);
  void AddSelected(wxSnip *);
  void RemoveSelected(wxSnip *);
  void NoSelected();
  void AddSelected(float x, float y, float w, float h);

  void Insert(wxSnip *snip, wxSnip *before, float x, float y);
  void Insert(wxSnip *snip, float x, float y);
  void Insert(wxSnip *snip);
  void Insert(wxSnip *snip, wxSnip *before);

  void Delete();
  void Delete(wxSnip *snip);
  void Remove(wxSnip *snip);

  void Erase(void);

  void ChangeStyle(wxStyleDelta *delta);
  void ChangeStyle(wxStyle *style, wxSnip *snip = NULL);
  void ChangeStyle(wxStyleDelta *delta, wxSnip *snip);

  Bool ReallyCanEdit(int op);

  Bool GetSnipLocation(wxSnip *thesnip, float *x = NULL, float *y = NULL, Bool bottomRight=FALSE);

  void SetFilename(char *, Bool temp = FALSE);

  Bool WriteToFile(wxMediaStreamOut *);
  Bool ReadFromFile(wxMediaStreamIn *, Bool overwritestyle = FALSE);
  Bool SaveFile(char *filename = NULL, int format = wxMEDIA_FF_STD, Bool showErrors = TRUE);
  int InsertPort(Scheme_Object *port, int format = wxMEDIA_FF_GUESS, Bool replaceStyles = TRUE);

  void StyleHasChanged(wxStyle *style);

  /* For making a lot of changes to be displayed at once: */
  void BeginEditSequence(Bool undoable = TRUE, Bool interruptSeqs = TRUE);
  void EndEditSequence(void);
  Bool RefreshDelayed(void);
  Bool InEditSequence(void);
  Bool LocationsUpToDate(void);

  void SetMaxWidth(float w);
  void SetMinWidth(float w);
  float GetMaxWidth();
  float GetMinWidth();
  void SetMinHeight(float h);
  void SetMaxHeight(float w);
  float GetMinHeight();
  float GetMaxHeight();

  virtual void PrintToDC(wxDC *print, int page = -1);
  virtual void *BeginPrint(wxDC *, Bool);
  virtual void EndPrint(wxDC *, void *);
  virtual Bool HasPrintPage(wxDC *dc, int page);

  void AddPasteboardFunctions(wxKeymap *tab);

  wxSnip *FindSnip(float x, float y, wxSnip *after=NULL);
  wxSnip *FindFirstSnip(void);

  Bool IsSelected(wxSnip *asnip);
  wxSnip *FindNextSelectedSnip(wxSnip *start);
  
  void MoveTo(wxSnip *, float x, float y);
  void Move(wxSnip *, float x, float y);
  void Move(float x, float y);

  Bool Resize(wxSnip *, float w, float h);

  void Raise(wxSnip *snip);
  void Lower(wxSnip *snip);
  void SetBefore(wxSnip *snip, wxSnip *before);
  void SetAfter(wxSnip *snip, wxSnip *after);

  Bool GetDragable();
  void SetDragable(Bool);

  Bool GetSelectionVisible();
  void SetSelectionVisible(Bool);

  virtual void SetSnipData(wxSnip *, wxBufferData *);
  virtual wxBufferData *GetSnipData(wxSnip *snip);

  virtual void InvalidateBitmapCache(float x=0.0, float y=0.0,
				     float w=-1.0, float h=-1.0);

#if ALLOW_X_STYLE_SELECTION
  virtual Bool OwnXSelection(Bool on, Bool update, Bool force);
#endif

  /* Override these for your own use: */
  virtual void OnChange(void);
  virtual Bool CanInsert(wxSnip *, wxSnip *, float x, float y);
  virtual void OnInsert(wxSnip *, wxSnip *, float x, float y);
  virtual void AfterInsert(wxSnip *, wxSnip *, float x, float y);
  virtual Bool CanDelete(wxSnip *);
  virtual void OnDelete(wxSnip *);
  virtual void AfterDelete(wxSnip *);
  virtual Bool CanMoveTo(wxSnip *, float x, float y, Bool dragging);
  virtual void OnMoveTo(wxSnip *, float x, float y, Bool dragging);
  virtual void AfterMoveTo(wxSnip *, float x, float y, Bool dragging);
  virtual Bool CanResize(wxSnip *, float w, float h);
  virtual void OnResize(wxSnip *, float w, float h);
  virtual void AfterResize(wxSnip *, float w, float h, Bool did);
  virtual Bool CanReorder(wxSnip *, wxSnip *, Bool);
  virtual void OnReorder(wxSnip *, wxSnip *, Bool);
  virtual void AfterReorder(wxSnip *, wxSnip *, Bool);

  virtual Bool CanSelect(wxSnip *, Bool on);
  virtual void OnSelect(wxSnip *, Bool on);
  virtual void AfterSelect(wxSnip *, Bool on);

  virtual Bool CanInteractiveMove(wxMouseEvent *);
  virtual void OnInteractiveMove(wxMouseEvent *);
  virtual void AfterInteractiveMove(wxMouseEvent *);
  virtual Bool CanInteractiveResize(wxSnip *snip);
  virtual void OnInteractiveResize(wxSnip *snip);
  virtual void AfterInteractiveResize(wxSnip *snip);

  virtual void InteractiveAdjustMouse(float *x, float *y);
  virtual void InteractiveAdjustResize(wxSnip *s, float *x, float *y);
  virtual void InteractiveAdjustMove(wxSnip *s, float *x, float *y);

  void SetScrollStep(float s);
  float GetScrollStep(void);

  Bool IsLockedForWrite() { return writeLocked; }
  Bool IsLockedForFlow() { return flowLocked; }

 private:
  Bool dragable, selectionVisible;

  wxSnip *snips, *lastSnip;
  wxList *snipLocationList;

  wxStandardSnipAdmin *snipAdmin;
  
  long lastTime;
  float startX, startY;
  float lastX, lastY;

  float origX, origY, origW, origH;
  
  float maxWidth, minWidth, minHeight, maxHeight;

  Bool keepSize, dragging, rubberband;

  int needResize;

  wxSnip *resizing;
  float sizedxm, sizedym;

  float scrollStep;

  float totalWidth, totalHeight, realWidth, realHeight;

  float updateLeft, updateRight, updateTop, updateBottom;
  Bool updateNonempty, noImplicitUpdate;

  Bool sizeCacheInvalid;
  int writeLocked;
  Bool flowLocked;

  int sequence;

  int delayedscrollbias;
  wxSnip *delayedscrollsnip;
  float delayedscrollX, delayedscrollY, delayedscrollW, delayedscrollH;

  Bool sequenceStreak;

  Bool changed;

  void InitDragging(wxMouseEvent *);
  void FinishDragging(wxMouseEvent *);

  void DoSelect(wxSnip *, Bool on);

  void DoEventResize(float eventX, float eventY);
  void DoEventMove(float eventX, float eventY);

  Bool _Delete(wxSnip *, wxDeleteSnipRecord *del);

  Bool InsertFile(const char *who, Scheme_Object *f, const char *filename, Bool clearStyles, Bool showErrors);

  void Draw(wxDC *dc, float dx, float dy, 
	    float cx, float cy, float cw, float ch, 
	    int show_caret, wxColour *bg);

  Bool FindDot(wxSnipLocation *loc, float x, float y,
	       float *dxm, float *dym);

  void Update(float x, float y, float w, float h);
  void UpdateSnip(wxSnip *);
  void UpdateLocation(wxSnipLocation *);
  void UpdateSelected();
  void UpdateAll();
  void UpdateNeeded();

  void CheckRecalc();

  void RubberBand(float x, float y, float w, float h);

  void _ChangeStyle(wxStyle *style, wxStyleDelta *delta, wxSnip *snip);

  wxSnip *SnipSetAdmin(wxSnip *snip, wxSnipAdmin *a);
};
