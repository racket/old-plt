
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
#ifdef wx_x
  rgn = NULL;
#endif
#ifdef wx_mac
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
#ifdef wx_x
  if (rgn) {
    XDestroyRegion(rgn);
    rgn = NULL;
  }
#endif
#ifdef wx_mac
  if (rgn) {
    DisposeRgn(rgn);
    rgn = NULL;
  }
#endif
}

void wxRegion::SetRectangle(float x, float y, float width, float height)
{
  Cleanup();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  width = dc->LogicalToDeviceXRel(width);
  height = dc->LogicalToDeviceYRel(height);

  if (is_ps) {
    ps = new wxPSRgn_Atomic("");
    *this << x << " " << y << " moveto\n";
    *this << (x + width) << " " << y << " lineto\n";
    *this << (x + width) << " " << (y - height) << " lineto\n";
    *this << x << " " <<  (y - height) << " lineto\n";
    *this << "closepath\n";
  }

#ifdef wx_msw
  rgn = CreateRectRgn(x, y, x + width, y + height);
#endif
#ifdef wx_x
  rgn = XCreateRegion();
  XRectangle r;
  r.x = x;
  r.y = y;
  r.width = width;
  r.height = height;
  XUnionRectWithRegion(&r, rgn, rgn);
#endif
#ifdef wx_mac
  rgn = NewRgn();
  SetRectRgn(rgn, x, y, x + width, y + height);
#endif
}

void wxRegion::SetRoundedRectangle(float x, float y, float width, float height, float radius)
{
  Cleanup();

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

#ifndef wx_x
  if (ps) {
#endif
    wxRegion *lt = new wxRegion(dc);
    wxRegion *rt = new wxRegion(dc);
    wxRegion *lb = new wxRegion(dc);
    wxRegion *rb = new wxRegion(dc);
    wxRegion *w = new wxRegion(dc);
    wxRegion *h = new wxRegion(dc);
    wxRegion *r;

    lt->SetEllipse(x, y, 2 * radius, 2 * radius);
    rt->SetEllipse(x + width - 2 * radius, y, 2 * radius, 2 * radius);
    rb->SetEllipse(x + width - 2 * radius, y + height - 2 * radius, 2 * radius, 2 * radius);
    lb->SetEllipse(x, y + height - 2 * radius, 2 * radius, 2 * radius);

    w->SetRectangle(x, y + radius, width, height - 2 * radius);
    h->SetRectangle(x + radius, y, width - 2 * radius, height);

    r = lt;
    r->Union(rt);
    r->Union(lb);
    r->Union(rb);
    r->Union(w);
    r->Union(h);

    ps = r->ps;
#ifdef wx_x
    /* A little hack: steal rgn from r: */
    rgn = r->rgn;
    r->rgn = NULL;
#else
  }
#endif

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  width = dc->LogicalToDeviceXRel(width);
  height = dc->LogicalToDeviceYRel(height);
  int xradius = dc->LogicalToDeviceXRel(radius);
  int yradius = dc->LogicalToDeviceYRel(radius);

#ifdef wx_msw
  rgn = CreateRoundRectRgn(x, y, x + width, y + height, xradius, yradius);
#endif
#ifdef wx_mac
  rgn = NewRgn();
  OpenRgn();
  Rect r;
  SetRect(&r, x, y, x + width, y + height);
  FrameRoundRect(&r, xradius, yradius);
  CloseRgn(rgn);
#endif
}

void wxRegion::SetEllipse(float x, float y, float width, float height)
{
  Cleanup();

  x = dc->LogicalToDeviceX(x);
  y = dc->LogicalToDeviceY(y);
  width = dc->LogicalToDeviceXRel(width);
  height = dc->LogicalToDeviceYRel(height);

#ifdef wx_msw
  rgn = CreateEllipticRgn(x, y, x + width, y + height);
#endif
#ifdef wx_mac
  rgn = NewRgn();
  OpenRgn();
  Rect r;
  SetRect(&r, x, y, x + width, y + height);
  FrameOval(&r);
  CloseRgn(rgn);
#endif

#ifdef wx_x
  int iwidth = (int)width + 2;
  int is_odd = iwidth & 0x1;
  int x_extent = (int)((iwidth + 1) / 2) + is_odd, i, dx, dy;
  float w2 = (x_extent - 1) * (x_extent - 1);
  XPoint *p = new XPoint[(4 * x_extent) - (2 * is_odd)];

  dx = x + width / 2;
  dy = y + height / 2;

  for (i = 0; i < x_extent; i++) {
    float y = (height / width) * sqrt(w2 - (i * i));
    p[i].x = i + dx;
    p[i].y = y + dy;
    p[2 * x_extent - i - 1].x = i + dx;
    p[2 * x_extent - i - 1].y = -y + dy;
    p[2 * x_extent + i - is_odd].x = -i + dx;
    p[2 * x_extent + i - is_odd].y = -y + dy;
    if (i || !is_odd) {
      p[4 * x_extent - i - 1 - 2 * is_odd].x = -i + dx;
      p[4 * x_extent - i - 1 - 2 * is_odd].y = y + dy;
    }
  }
  rgn = XPolygonRegion(p, 4 * x_extent, WindingRule);
#endif
}

void wxRegion::SetPolygon(int n, wxPoint points[], float xoffset, float yoffset, int fillStyle)
{
  Cleanup();

#ifdef wx_x
# define POINT XPoint
#endif
#ifdef wx_mac
# define POINT MyPoint
  typedef struct { int x, y; } MyPoint;
#endif

  POINT *cpoints = new POINT[n];
  int i;
  for (i = 0; i < n; i++) {
    cpoints[i].x = dc->LogicalToDeviceX(points[i].x + xoffset);
    cpoints[i].y = dc->LogicalToDeviceY(points[i].y + yoffset);
  }

#ifdef wx_msw
  rgn = CreatePolygonRgn(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);
#endif
#ifdef wx_x
  rgn = XPolygonRegion(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? EvenOddRule : WindingRule);
#endif
#ifdef wx_mac
  rgn = NewRgn();
  OpenRgn();
  MoveTo(cpoints[0].x, cpoints[0].y);
  for (i = 0; i < n; i++)
    LineTo(cpoints[i].x, cpoints[i].y);
  LineTo(cpoints[n - 1].x, cpoints[n - 1].y);
  CloseRgn(rgn);
#endif
}

void wxRegion::SetArc(float x, float y, float w, float h, float start, float end)
{
  SetEllipse(x, y, w, h);

  if (start == end) return;

  wxRegion *r = new wxRegion(dc);

  static double pi;
  if (!pi)
    pi = 2 * asin(1);

  start = fmod(start, 2*pi);
  end = fmod(end, 2*pi);
  if (start < 0)
    start += 2*pi;
  if (end < 0)
    end += 2*pi;

  float cx = x + w/2;
  float cy = y + h/2;

  wxPoint a[20];
  int n;

  a[0].x = (w / 2) * cos(end) + cx;
  a[0].y = (h / 2) * (-sin(end)) + cy;

  a[1].x = cx;
  a[1].y = cy;

  a[2].x = (w / 2) * cos(start) + cx;
  a[2].y = (h / 2) * (-sin(start)) + cy;

  n = 3;

  int saw_start = 0, saw_end = 0, closed = 0;

  if (!saw_start && (start < (pi / 2)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y;
  } else
    closed = saw_start;

  if (!saw_start && (start < pi))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = x;
    a[n++].y = cy;
  } else
    closed = saw_start;

  if (!saw_start && (start < (1.5 * pi)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y + h;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h;
  } else
    closed = saw_start;

  saw_start = 1;
  saw_end = (end > start);
  
  if (saw_start && !closed) {
    a[n].x = x + w;
    a[n++].y = y + h;
  }
  if (saw_start && !saw_end) {
    a[n].x = x + w;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y;    
  } else
    closed = saw_start;
  
  if (!saw_end && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y;
  }
  if (saw_start && !saw_end) {
    a[n].x = x;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x;
    a[n++].y = y + h;
  } 
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h;
  } else
    closed = saw_start;

  if (!closed) {
    a[n].x = x + w;
    a[n++].y = y + h;
  }

  r->SetPolygon(n, a);

  Intersect(r);
}

void wxRegion::Union(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) return;

  if (is_ps) {
    if (!ps)
      ps = r->ps;
    else
      ps = new wxPSRgn_Union(ps, r->ps);
  }

#ifdef wx_msw
  if (!rgn) {
    rgn = CreateRectRgn(0, 0, 1, 1);
    CombineRgn(rgn, r->rgn, rgn, RGN_COPY);
  } else
    CombineRgn(rgn, r->rgn, rgn, RGN_OR);
#endif
#ifdef wx_x
  if (!rgn)
    rgn = XCreateRegion();
  XUnionRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn)
    rgn = NewRgn();
  UnionRgn(rgn, r->rgn, rgn);
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
#ifdef wx_x
  if (!rgn) return;
  XIntersectRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) return;
  SectRgn(rgn, r->rgn, rgn);
#endif

  if (Empty()) {
    Cleanup();
    ps = NULL;
  } else
    ps = new wxPSRgn_Union(ps, r->ps);
}

void wxRegion::Subtract(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->Empty()) return;

#ifdef wx_msw
  if (!rgn) return;
  CombineRgn(rgn, rgn, r->rgn, RGN_DIFF);
#endif
#ifdef wx_x
  if (!rgn) return;
  XSubtractRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) return;
  DiffRgn(rgn, r->rgn, rgn);
#endif

  if (Empty()) {
    Cleanup();
    ps = NULL;
  } else
    ps = new wxPSRgn_Diff(ps, r->ps);
}

void wxRegion::BoundingBox(float *x, float *y, float *w, float *h)
{
  if (Empty()) {
    *x = *y = *w = *h = 0;
    return;
  } else {
#ifdef wx_msw
    RECT r;

    GetRgnBox(rgn, &r);
  
    *x = r.left;
    *y = r.top;
    *w = r.right - r.left;
    *h = r.bottom - r.top;
#endif
#ifdef wx_x
    XRectangle r;
    
    XClipBox(rgn, &r);
    
    *x = r.x;
    *y = r.y;
    *w = r.width;
    *h = r.height;
#endif
#ifdef wx_mac
    *x = (*rgn)->rgnBBox.left;
    *y = (*rgn)->rgnBBox.top;
    *w = (*rgn)->rgnBBox.bottom - *x;
    *y = (*rgn)->rgnBBox.right - *x;
#endif
    
    *x = dc->DeviceToLogicalX(*x);
    *y = dc->DeviceToLogicalY(*y);
    *w = dc->DeviceToLogicalXRel(*w);
    *h = dc->DeviceToLogicalYRel(*h);
  }
}

Bool wxRegion::Empty()
{
#ifdef wx_msw
  if (!rgn) return TRUE;

  RECT r;
  return (GetRgnBox(rgn, &r) == NULLREGION);
#endif
#ifdef wx_x
  if (!rgn) return TRUE;
  return XEmptyRegion(rgn);
#endif
#ifdef wx_mac
  if (!rgn) return TRUE;
  return EmptyRgn(rgn);
#endif
}

wxRegion& wxRegion::operator<<(const char *s)
{
  long l = strlen(s);
  long psl = strlen(((wxPSRgn_Atomic *)ps)->s);

  char *naya = new WXGC_ATOMIC char[l + psl + 1];
  memcpy(naya, ((wxPSRgn_Atomic *)ps)->s, psl);
  memcpy(naya + psl, s, l);
  naya[psl + l] = 0;
  
  ((wxPSRgn_Atomic *)ps)->s = naya;

  return *this;
}

wxRegion& wxRegion::operator<<(double d)
{
  char s[100];
  sprintf(s, "%lf", d);
  return *this << s;
}

/***************************************************************************************/

char *wxPSRgn_Union::GetString()
{
  return MakeString("", "", "");
}

char *wxPSRgn_Composite::MakeString(const char *prefix, const char *infix, const char *suffix)
{
  char *sa = a->GetString();
  char *sb = b->GetString();
  int plen = strlen(prefix), ilen = strlen(infix), slen = strlen(suffix);
  int alen = strlen(sa);
  int blen = strlen(sb);
  char *sr = new WXGC_ATOMIC char[alen + blen + plen + ilen + slen + 1];

  memcpy(sr, prefix, plen);
  memcpy(sr + plen, sa, alen);
  memcpy(sr + plen + alen, infix, ilen);
  memcpy(sr + plen + alen + ilen, sb, blen);
  memcpy(sr + plen + alen + ilen + blen, suffix, slen);
  sr[plen + alen + ilen + blen + slen] = 0;

  return sr;
}

int wxPSRgn_Composite::FlattenIntersects(wxPSRgn **l, wxPSRgn *r, int i)
{
  if (r->is_intersect)
    return FlattenIntersects(l, ((wxPSRgn_Composite *)r)->b, 
			     FlattenIntersects(l, ((wxPSRgn_Composite *)r)->a, i));
  
  if (l)
    l[i] = r;

  return i + 1;
}


wxPSRgn *wxPSRgn_Union::Lift()
{
  wxPSRgn *la = a->Lift();
  wxPSRgn *lb = b->Lift();
  wxPSRgn *r = NULL, **al, **bl;
  int na, nb, i, j;

  if (!la->is_intersect
      && !lb->is_intersect
      && (a == la) && (b == lb))
    return this;

  na = FlattenIntersects(NULL, la, 0);
  nb = FlattenIntersects(NULL, lb, 0);

  al = new wxPSRgn*[na];
  bl = new wxPSRgn*[nb];

  FlattenIntersects(al, la, 0);
  FlattenIntersects(bl, lb, 0);

  for (i = 0; i < na; i++) {
    for (j = 0; j < nb; j++) {
      wxPSRgn *c;
      c = new wxPSRgn_Union(al[i], bl[j]);
      if (r)
	r = new wxPSRgn_Intersect(r, c);
      else
	r = c;
    }
  }

  return r;
}


char *wxPSRgn_Intersect::GetString()
{
  return MakeString("", "clip\nnewpath\n", "");
}

wxPSRgn *wxPSRgn_Intersect::Lift()
{
  wxPSRgn *la = a->Lift();
  wxPSRgn *lb = b->Lift();

  if ((la == a) && (lb == b))
    return this;
  else
    return new wxPSRgn_Intersect(la, lb);
}


char *wxPSRgn_Diff::GetString()
{
  return MakeString("", "clip\nnewpath\n", "");
}

wxPSRgn *wxPSRgn_Diff::Lift()
{
  wxPSRgn *la = a->Lift();
  wxPSRgn *lb = b->Lift();
  wxPSRgn *r = NULL, **al, **bl;
  int na, nb, i;

  if (!la->is_intersect
      && !lb->is_intersect
      && (a == la) && (b == lb))
    return this;

  if (lb->is_intersect) {
    /* A \ (B n C) = (A \ B) u (A \ C) */
    nb = FlattenIntersects(NULL, lb, 0);
    bl = new wxPSRgn*[nb];
    FlattenIntersects(bl, lb, 0);
    
    for (i = 0; i < nb; i++) {
      wxPSRgn *s = new wxPSRgn_Diff(la, bl[i]);
      if (r)
	r = new wxPSRgn_Union(r, s);
      else
	r = s;
    }

    return r->Lift(); /* Handles intersections in la */
  } else {
    /* (A n B) \ C = (A \ C) n (B \ C)   [note: C has no intersections] */
    na = FlattenIntersects(NULL, la, 0);
    al = new wxPSRgn*[na];
    FlattenIntersects(al, la, 0);
    
    for (i = 0; i < na; i++) {
      wxPSRgn *s = new wxPSRgn_Diff(al[i], lb);
      if (r)
	r = new wxPSRgn_Intersect(r, s);
      else
	r = s;
    }

    return r;
  }
}
