/*								-*- C++ -*-
 * File:		wxgraph.h
 * Purpose:	Graph library for wxWindows
 */

#ifndef wx_graphh
#define wx_graphh

#ifdef __GNUG__
#pragma interface
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
#   include <wx_hash.h>
#   include <stdio.h>
#endif

class wxHashTable;
class wxGraphNode;
class wxGraphEdge;
class wxGraphLayout: public wxObject
{
  DECLARE_ABSTRACT_CLASS(wxGraphLayout)
 private:
  int debuglevel;     /* value 0-9, controls the amount of debug output */
  int postscript; /* file is postscript */
  Bool flexscale;  /* flexible scaling of x and y indep., not square */
  int rotation;	   /* rotation of final graph, * 90 degrees */
  void set_scale();
  float scale[2][3];
  float curr_box[4];
  int max_x, max_y;
  wxHashTable *nodetable;
  wxGraphNode *firstnode;
  int n_nodes;
  float bbox[4];
  wxGraphNode **vplace;
  wxGraphNode **hplace;

  void do_scale(float *x, float *y);
  void mult_y( float f);
  void mult_x( float f);
  void translate( float dx, float dy);
  void rotate( int i);
  void place_graph();
  void verbose( char *fmt, ...);

  void set_node_incount( wxGraphNode *list);
  void select_start_nodes( wxGraphNode *fromlist, wxGraphNode **tolist);
  void vertical_place( wxGraphNode **startlist);
  void dfs_vplace( wxGraphNode *node, int y);
  void uncycle_graph();
  void vert_compact();
  void reduce_density();
  void horizontal_place( int leftbound);
  void horizontal_assign( int startrow, int endrow, int f, int b);
  void hor_move_node( wxGraphNode *node, int inweight, int outweight);
  void hor_distrib_row( wxGraphNode *row);
  void horizontal_shift( int leftbound);
  void link_node(wxGraphNode **listp, wxGraphNode *node);
  void unlink_node(wxGraphNode *node);
  void link_edge(wxGraphEdge *edge, wxGraphNode *fromnode, wxGraphNode *tonode);
  void unlink_edge(wxGraphEdge *edge);

  // JACS
  wxHashTable *nodeHashTable;
  wxHashTable *arcHashTable;
  virtual wxGraphNode *_AddNode(long id, char *name = NULL);
  
 protected:
  float lastY;
  float xSpacing;
  float ySpacing;
  float topMargin;
  float leftMargin;
  wxDC *graphDC;

 public:
  wxGraphLayout(wxDC *dc = NULL);
  ~wxGraphLayout(void);

  // Redefine these
  virtual float GetNodeX(long id);
  virtual float GetNodeY(long id);
  virtual void SetNodeX(long id, float x);
  virtual void SetNodeY(long id, float y);
  virtual void ActivateNode(long id, Bool active);
  virtual Bool NodeActive(long id);

  // Optional redefinition
  virtual void Initialize(void);
  virtual void SetNodeName(long id, char *name);
  virtual char *GetNodeName(long id);
  virtual void GetNodeSize(long id, float *x, float *y);
  virtual void Draw(void);
  virtual void DrawNodes(void);
  virtual void DrawArcs(void);
  virtual void DrawNode(long id);
  virtual void DrawArc(long from, long to);

  // Don't redefine
  virtual void DoLayout(void);
  virtual void Clear(void);
  virtual void AddNode(long id, char *name = NULL);
  virtual void AddArc(long id, long fromId, long toId, char *name = NULL, Bool hide = FALSE);
  virtual void SetBoundingBox(float x1, float y1, float x2, float y2);

  // Accessors -- don't redefine
  inline virtual void SetDC(wxDC *dc){ graphDC = dc; }
  inline virtual wxDC *GetDC(void) { return graphDC; }
  inline void SetSpacing(float x, float y) { xSpacing = x; ySpacing = y; }
  inline float GetXSpacing(void) { return xSpacing; }
  inline float GetYSpacing(void) { return ySpacing; }
  inline void SetMargins(float x, float y)
    { leftMargin = x; topMargin = y; }
  inline float GetTopMargin(void) { return topMargin; }
  inline float GetLeftMargin(void) { return leftMargin; }
  inline void SetRotation(int rot)
    { rotation = rot; }
  inline int GetRotation(void)
    { return rotation; }
 private:
  void Layout(long node_id, int level);
};

#endif // wx_graphh
  
