/*								-*- C++ -*-
 * File:		wxtree.h
 * Purpose:	Tree library
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wx_treeh
#define wx_treeh

#ifdef __GNUG__
#pragma interface
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
    /* sccsid[] = "%W% %G%" */
#endif

class wxTreeLayout: public wxObject
{
 DECLARE_ABSTRACT_CLASS(wxTreeLayout)
 protected:
  long parentNode;
  float lastY;
  float lastX;
  float xSpacing;
  float ySpacing;
  float topMargin;
  float leftMargin;
  wxDC *treeDC;
  Bool orientation; // TRUE for top-to-bottom, FALSE for left-to-right

 public:
  wxTreeLayout(wxDC *dc = NULL);

  // Redefine these
  virtual void GetChildren(long id, wxList& list) = 0;
  virtual long GetNextNode(long id) = 0;
  virtual long GetNodeParent(long id) = 0;
  virtual float GetNodeX(long id) = 0;
  virtual float GetNodeY(long id) = 0;
  virtual void SetNodeX(long id, float x) = 0;
  virtual void SetNodeY(long id, float y) = 0;
  virtual void ActivateNode(long id, Bool active) = 0;
  virtual Bool NodeActive(long id) = 0;

  // Optional redefinition
  void Initialize(void);
  inline virtual void SetNodeName(long WXUNUSED(id), char *WXUNUSED(name)) {}
  inline virtual char *GetNodeName(long WXUNUSED(id)) { return NULL; }
  virtual void GetNodeSize(long id, float *x, float *y);
  virtual void Draw(void);
  virtual void DrawNodes(void);
  virtual void DrawBranches(void);
  virtual void DrawNode(long id);
  virtual void DrawBranch(long from, long to);

  // Don't redefine
  virtual void DoLayout(long topNode = -1);

  // Accessors -- don't redefine
  virtual void SetTopNode(long id);
  virtual long GetTopNode(void);
  inline virtual void SetDC(wxDC *dc){ treeDC = dc; }
  inline virtual wxDC *GetDC(void) { return treeDC; }
  inline void SetSpacing(float x, float y) { xSpacing = x; ySpacing = y; }
  inline float GetXSpacing(void) { return xSpacing; }
  inline float GetYSpacing(void) { return ySpacing; }
  inline void SetMargins(float x, float y) { leftMargin = x; topMargin = y; }
  inline float GetTopMargin(void) { return topMargin; }
  inline float GetLeftMargin(void) { return leftMargin; }

  inline Bool GetOrientation(void) { return orientation; }
  inline void SetOrientation(Bool or) { orientation = or; }
 private:
  void CalcLayout(long node_id, int level);
};

class _storedNode
{
 public:
  char name[40];
  float x, y;
  long parentId;
  Bool active;
};

class wxStoredTree: public wxTreeLayout
{
 DECLARE_DYNAMIC_CLASS(wxStoredTree)
 private:
  _storedNode *nodes;
  int num;
  int maxNodes;
 public:
  wxStoredTree(wxDC *dc = NULL, int noNodes = 200);
  ~wxStoredTree(void);
  void Initialize(int n);

  virtual void GetChildren(long id, wxList& list);
  virtual long GetNextNode(long id);
  virtual long GetNodeParent(long id);
  virtual float GetNodeX(long id);
  virtual float GetNodeY(long id);
  virtual void SetNodeX(long id, float x);
  virtual void SetNodeY(long id, float y);
  virtual void SetNodeName(long id, char *name);
  virtual char *GetNodeName(long id);
  virtual void ActivateNode(long id, Bool active);
  virtual Bool NodeActive(long id);

  virtual void AddChild(char *name, char *parent = NULL);
  virtual long NameToId(char *name);
};

#endif // wx_treeh
