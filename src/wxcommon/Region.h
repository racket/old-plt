
#ifndef wb_rgnh
#define wb_rgnh

#include "wx_dc.h"

class wxPSRgn;

#ifdef UseXtRegions
typedef Region XtRegion;
#else
typedef void *XtRegion;
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
  wxDC *dc;
  wxPSRgn *ps;
  short is_ps, locked;

  wxRegion(wxDC *dc, wxRegion *r = NULL);
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
  
  void Cleanup();

  /* PS Stuff */
  void Put(const char *s);
  void Put(double d);
};

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

#endif
