/********************************************************/
/*                       Regions                        */
/********************************************************/

wxRegion::wxRegion(wxDC *_dc, wxRegion *r)
{
  dc = _dc;
  is_ps = wxSubType(dc->__type, wxTYPE_DC_POSTSCRIPT);
  
#ifdef wx_msw
  rgn = NULL;
#endif

  if (r) Union(r);
}

wxRegion::~wxRegion()
{
  Cleanup();
}

void wxRegion::Cleanup()
{  
#ifdef wx_msw
  if (rgn) {
    DeleteObject(rgn);
    rgn = NULL;
  }
#endif
}

void wxRegion::SetRectangle(float x, float y, float width, float height)
{
  Cleanup();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  width = dc->LogicalToDeviceXRel(x);
  height = dc->LogicalToDeviceYRel(y);

  if (is_ps) {
    *this << "newpath\n";
    *this << x << " " << y << " moveto\n";
    *this << (x + width) << " " << y << " lineto\n";
    *this << (x + width) << " " << (y + height) << " lineto\n";
    *this << x << " " <<  (y + height) << " lineto\n";
    *this << "closepath\n";
  }

#ifdef wx_msw
  rgn = CreateRectRgn(x, y, x + width, y + height);
#endif
}

void wxRegion::SetRoundedRectangle(float x, float y, 
				   float width, float height, float radius)
{
  Cleanup();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  width = dc->LogicalToDeviceXRel(x);
  height = dc->LogicalToDeviceYRel(y);

  // A negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'
  if (radius < 0.0) {
    float smallest = 0.0;
    if (width < height)
      smallest = width;
    else
      smallest = height;
    radius = (float)(- radius * smallest);
  } else
    radius = dc->LogicalToDeviceXRel(radius);

#ifdef wx_msw
  rgn = CreateRoundRectRgn(x, y, x + width, y + height, radius, radius);
#endif
}

void wxRegion::SetEllipse(float x, float y, float width, float height)
{
  Cleanup();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  width = dc->LogicalToDeviceXRel(x);
  height = dc->LogicalToDeviceYRel(y);

#ifdef wx_msw
  rgn = CreateEllipticRgn(x, y, x + width, y + height);
#endif
}

void wxRegion::SetPolygon(int n, wxPoint points[], float xoffset, float yoffset, int fillStyle)
{
  Cleanup();

  POINT *cpoints = new POINT[n];
  int i;
  for (i = 0; i < n; i++) {
    cpoints[i].x = dc->LogicalToDeviceX(points[i].x + xoffset);
    cpoints[i].y = dc->LogicalToDeviceY(points[i].y + yoffset);
  }

#ifdef wx_msw
  rgn = CreatePolygonRgn(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);
#endif
}

/* void wxRegion::SetArc(float x1,float y1,float x2,float y2,float xc,float yc); */

void wxRegion::Union(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) return;

  if (is_ps) {
    if (!ps_string)
      ps_string = r->ps_string;
    else {
      /* Join PS... */
    }
  }

#ifdef wx_msw
  if (!rgn) {
    rgn = CreateRectRgn(0, 0, 1, 1);
    CombineRgn(rgn, r->rgn, rgn, RGN_COPY);
  } else
    CombineRgn(rgn, r->rgn, rgn, RGN_OR);
#endif
}

void wxRegion::Intersect(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) {
    Cleanup();
    return;
  }

#ifdef wx_msw
  if (!rgn) return;
  CombineRgn(rgn, r->rgn, rgn, RGN_AND);
#endif

  if (Empty()) {
    ps_string = NULL;
  } else {
    /* Intersect PS... */
  }
}

void wxRegion::Subtract(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) return;

#ifdef wx_msw
  if (!rgn) return;
  CombineRgn(rgn, rgn, r->rgn, RGN_DIFF);
#endif

  if (Empty()) {
    ps_string = NULL;
  } else {
    /* Subtract PS... */
  }
}

void wxRegion::BoundingBox(float *x, float *y, float *w, float *h)
{
#ifdef wx_msw
  RECT r;

  GetRgnBox(rgn, &r);

  *x = r.left;
  *y = r.top;
  *w = r.right - r.left;
  *h = r.bottom - r.top;
#endif

  *x = dc->DeviceToLogicalX(*x);
  *y = dc->DeviceToLogicalY(*y);
  *w = dc->DeviceToLogicalXRel(*w);
  *h = dc->DeviceToLogicalYRel(*h);
}

Bool wxRegion::Empty()
{
#ifdef wx_msw
  if (!rgn) return TRUE;

  RECT r;
  return (GetRgnBox(rgn, &r) == NULLREGION);
#endif
}

wxRegion& wxRegion::operator<<(const char *s)
{
  long l = strlen(s);
  long psl = strlen(ps_string);

  char *naya = new WXGC_ATOMIC char[l + psl + 1];
  memcpy(naya, s, l);
  memcpy(naya + l, ps_string, psl);
  naya[psl + l] = 0;
  
  ps_string = naya;

  return *this;
}

wxRegion& wxRegion::operator<<(double d)
{
  char s[100];
  sprintf(s, "%lf", d);
  return *this << s;
}
