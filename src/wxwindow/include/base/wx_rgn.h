
#ifndef wb_rgnh
#define wb_rgnh

#include "wx_dc.h"

class wxRegion : public wxObject 
{
 public:
#ifdef wx_msw
  HRGN rgn;
#endif
  wxDC *dc;
  char *ps_string;
  int is_ps;

  wxRegion(wxDC *dc, wxRegion *r = NULL);
  ~wxRegion();

  inline wxDC *GetDC() { return dc; }
  
  void SetRectangle(float x, float y, float width, float height);
  void SetRoundedRectangle(float x, float y, float width, float height, float radius = 20.0);
  void SetEllipse(float x, float y, float width, float height);
  void SetPolygon(int n, wxPoint points[], float xoffset = 0, float yoffset = 0, 
		  int fillStyle=wxODDEVEN_RULE);
  /* void SetArc(float x1,float y1,float x2,float y2,float xc,float yc); */

  void Union(wxRegion *);
  void Intersect(wxRegion *);
  void Subtract(wxRegion *);

  void BoundingBox(float *x, float *y, float *w, float *h);

  Bool Empty();
  
  void Cleanup();

  /* PS Stuff */
  wxRegion& operator<<(const char *s);
  wxRegion& operator<<(double d);
};

#endif
