
#ifndef wb_rgnh
#define wb_rgnh

#include "wx_dc.h"

class wxPSRgn;

#ifdef UseXtRegions
typedef Region XtRegion;
#else
typedef void *XtRegion;
#endif

#if defined(WX_USE_CAIRO) || defined(wx_mac) || defined(wx_msw)
# define WX_USE_PATH_RGN
#endif

#ifdef WX_USE_PATH_RGN
class wxPathRgn;
#endif

class wxRegion : public wxObject 
{
 public:
#ifdef wx_msw
  HRGN rgn;
#endif
#ifdef wx_x
  XtRegion rgn;
#endif
#ifdef wx_mac
  RgnHandle rgn;
#endif
#ifdef WX_USE_PATH_RGN
  wxPathRgn *prgn;
  double *geometry;
# ifdef wx_mac
  int npaths;
  CGMutablePathRef *paths;
# endif
# ifdef wx_msw
  int npaths;
  Gdiplus::GraphicsPath **paths;
# endif
#endif
  wxDC *dc;
  wxPSRgn *ps;
  char is_ps, locked, no_prgn;

  wxRegion(wxDC *dc, wxRegion *r = NULL, Bool no_prgn = FALSE);
  ~wxRegion();

  inline wxDC *GetDC() { return dc; }
  
  void SetRectangle(double x, double y, double width, double height);
  void SetRoundedRectangle(double x, double y, double width, double height, double radius = 20.0);
  void SetEllipse(double x, double y, double width, double height);
  void SetPolygon(int n, wxPoint points[], double xoffset = 0, double yoffset = 0, 
		  int fillStyle=wxODDEVEN_RULE);
  void SetArc(double x, double y, double w, double h, double start, double end);

  void Union(wxRegion *);
  void Intersect(wxRegion *);
  void Subtract(wxRegion *);

  void BoundingBox(double *x, double *y, double *w, double *h);

  Bool Empty();
  Bool ReallyEmpty();
  
  void Cleanup();

  /* PS Stuff */
  void Put(const char *s);
  void Put(double d);

#ifdef WX_USE_PATH_RGN
  void Install(long target);  
#endif
};

/************************************************************/

class wxPSRgn : public wxObject
{
 public:
  int is_intersect;
  wxPSRgn() { is_intersect = 0; }
  virtual char *GetString() = 0;
  virtual wxPSRgn *Lift() = 0;
#ifdef RGN_DEBUGGING_PRINTS
  virtual void DebugPrint() = 0;
#endif
};

class wxPSRgn_Atomic : public wxPSRgn
{
 public:
  char *s, *debug_name;
  wxPSRgn_Atomic(char *ps, char *dn) { s = ps; debug_name = dn; }
  char *GetString() { return s; }
  wxPSRgn *Lift() { return this; }
#ifdef RGN_DEBUGGING_PRINTS
  void DebugPrint() { printf("%s%lx", debug_name, (long)this); }
#endif
};

class wxPSRgn_Composite : public wxPSRgn
{
 public:
  wxPSRgn *a, *b;
  char *MakeString(const char *prefix, const char *infix, const char *suffix);

  int FlattenIntersects(wxPSRgn **l, wxPSRgn *r, int i);
};

class wxPSRgn_Union : public wxPSRgn_Composite
{
 public:
  wxPSRgn_Union(wxPSRgn *ra, wxPSRgn *rb) { a = ra; b = rb; }
  char *GetString();
  wxPSRgn *Lift();
#ifdef RGN_DEBUGGING_PRINTS
  void DebugPrint() { printf("("); a->DebugPrint(); printf(" U "); b->DebugPrint(); printf(")"); }
#endif
};

class wxPSRgn_Intersect : public wxPSRgn_Composite
{
 public:
  wxPSRgn_Intersect(wxPSRgn *ra, wxPSRgn *rb) { a = ra; b = rb; is_intersect = 1; }
  char *GetString();
  wxPSRgn *Lift();
#ifdef RGN_DEBUGGING_PRINTS
  void DebugPrint() { printf("("); a->DebugPrint(); printf(" n "); b->DebugPrint(); printf(")"); }
#endif
};

class wxPSRgn_Diff : public wxPSRgn_Composite
{
 public:
  wxPSRgn_Diff(wxPSRgn *ra, wxPSRgn *rb) { a = ra; b = rb; }
  char *GetString();
  wxPSRgn *Lift();
#ifdef RGN_DEBUGGING_PRINTS
  void DebugPrint() { printf("("); a->DebugPrint(); printf(" \\ "); b->DebugPrint(); printf(")"); }
#endif
};

/************************************************************/

#ifdef WX_USE_PATH_RGN

class wxPathRgn : public wxObject
{
 public:
  wxPathRgn();
  ~wxPathRgn();
  virtual void Install(long target, Bool reverse) = 0;
  virtual wxPathRgn *Lift();
  virtual Bool IsIntersect();
  int FlattenIntersects(wxPathRgn **l, wxPathRgn *r, int i);
};

class wxRectanglePathRgn : public wxPathRgn
{
 public:
  double x;
  double y;
  double width;
  double height;
  wxRectanglePathRgn(double x, double y, double width, double height);
  virtual void Install(long target, Bool reverse);
};

class wxRoundedRectanglePathRgn : public wxPathRgn
{
 public:
  double x;
  double y;
  double width;
  double height;
  double radius;
  wxRoundedRectanglePathRgn(double x, double y, double width, double height, double radius);
  virtual void Install(long target, Bool reverse);
};

class wxPolygonPathRgn : public wxPathRgn
{
 public:
  int n;
  wxPoint *points;
  double xoffset;
  double yoffset;
  int fillStyle;
  wxPolygonPathRgn(int n, wxPoint points[], double xoffset = 0, double yoffset = 0, 
		   int fillStyle=wxODDEVEN_RULE);
  virtual void Install(long target, Bool reverse);
};

class wxArcPathRgn : public wxPathRgn
{
 public:
  double x;
  double y;
  double w;
  double h;
  double start;
  double end;
  wxArcPathRgn(double x, double y, double w, double h, double start, double end);
  virtual void Install(long target, Bool reverse);
};

class wxUnionPathRgn : public wxPathRgn
{
 public:
  wxPathRgn *a, *b;
  wxUnionPathRgn(wxPathRgn *f, wxPathRgn *s);
  virtual void Install(long target, Bool reverse);
  virtual wxPathRgn *Lift();
};

class wxIntersectPathRgn : public wxPathRgn
{
 public:
  wxPathRgn *a, *b;
  wxIntersectPathRgn(wxPathRgn *f, wxPathRgn *s);
  virtual void Install(long target, Bool reverse);
  virtual wxPathRgn *Lift();
  virtual Bool IsIntersect();
};

class wxDiffPathRgn : public wxPathRgn
{
 public:
  wxPathRgn *a, *b;
  wxDiffPathRgn(wxPathRgn *f, wxPathRgn *s);
  virtual void Install(long target, Bool reverse);
  virtual wxPathRgn *Lift();
};

#endif

#endif
