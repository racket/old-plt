/********************************************************/
/*                       Regions                        */
/********************************************************/

#ifndef wxPI
# define wxPI 3.141592653589793
#endif

#define CAIRO_DEV ((cairo_t *)target)

#ifdef wx_mac
typedef struct {
  CGMutablePathRef path;
  CGAffineTransform xform;
  CGMutablePathRef *paths;
  int npaths, apaths;
} PathTarget;
# define CGPATH ((PathTarget *)target)->path
# define CGXFORM (&((PathTarget *)target)->xform)
# define PathTargetPath_t CGMutablePathRef
#endif

#ifdef wx_msw
typedef struct {
  GraphicsPath *path;
  GraphicsPath **paths;
  int npaths, apaths;
} PathTarget;
# define GP ((PathTarget *)target)->path
# define PathTargetPath_t GraphicsPath*
#endif

wxRegion::wxRegion(wxDC *_dc, wxRegion *r, Bool _no_prgn)
{
  dc = _dc;
  is_ps = wxSubType(dc->__type, wxTYPE_DC_POSTSCRIPT);
  locked = 0;
 
#ifdef wx_msw
  rgn = NULL;
#endif
#ifdef wx_x
  rgn = NULL;
#endif
#ifdef wx_mac
  rgn = NULL;
#endif
#ifdef WX_USE_PATH_RGN
  prgn = NULL;
  no_prgn = _no_prgn;
  if (!no_prgn) {
    double *a, x, y, xs, ys;
    dc->GetDeviceOrigin(&x, &y);
    dc->GetUserScale(&xs, &ys);
    if ((xs != 1) || (ys != 1)
	|| (x != 0) || (y != 0)) {
      a = new WXGC_ATOMIC double[4];
      a[0] = x;
      a[1] = y;
      a[2] = xs;
      a[3] = ys;
      geometry = a;
    }
  }
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
#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    prgn = NULL;
# if defined(wx_mac) || defined(wx_msw)
    if (paths) {
      int i;
      for (i = 0; i < npaths; i++) {
#  ifdef wx_mac
	CGPathRelease(paths[i]);
#  endif
#  ifdef wx_msw
	wxGPathRelease(paths[i]);
#  endif
      }
      paths = NULL;
    }
# endif
  }
#endif
}

void wxRegion::SetRectangle(double x, double y, double width, double height)
{
  double xw, yh;
  int ix, iy, iw, ih;

  Cleanup();

#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    prgn = new wxRectanglePathRgn(x, y, width, height);
  }
#endif

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToDeviceX(x);
  y = dc->FLogicalToDeviceY(y);
  xw = dc->FLogicalToDeviceX(xw);
  width = xw - x;
  yh = dc->FLogicalToDeviceY(yh);
  height = yh - y;

  if (is_ps) {
    wxPSRgn *ra;

    height = -height;

    ra = new wxPSRgn_Atomic("", "rect");
    ps = ra;
    Put(x); Put(" "); Put(y); Put(" moveto\n");
    Put(x + width); Put(" "); Put(y); Put(" lineto\n");
    Put(x + width); Put(" "); Put(y - height); Put(" lineto\n");
    Put(x); Put(" "); Put(y - height); Put(" lineto\n");
    Put("closepath\n");

    /* So bitmap-based region is right */
    y  = -y;
  }

  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;

#ifdef wx_msw
  rgn = CreateRectRgn(ix, iy, ix + iw, iy + ih);
#endif
#ifdef wx_x
  {
    XRectangle r;
    rgn = XCreateRegion();
    r.x = ix;
    r.y = iy;
    r.width = iw;
    r.height = ih;
    XUnionRectWithRegion(&r, rgn, rgn);
  }
#endif
#ifdef wx_mac
  rgn = NewRgn();
  SetRectRgn(rgn, ix, iy, ix + iw, iy + ih);
#endif
}

void wxRegion::SetRoundedRectangle(double x, double y, double width, double height, double radius)
{
  wxRegion *lt, *rt, *lb, *rb, *w, *h, *r;
  int ix, iy, iw, ih;
  double xw, yh;
#if defined(wx_msw) || defined(wx_mac)
  int xradius, yradius;
#endif

  Cleanup();

  // A negative radius value is interpreted to mean
  // 'the proportion of the smallest X or Y dimension'
  if (radius < 0.0) {
    double smallest = 0.0;
    if (width < height)
      smallest = width;
    else
      smallest = height;
    radius = (double)(- radius * smallest);
  } else
    radius = dc->FLogicalToDeviceXRel(radius);

#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    prgn = new wxRoundedRectanglePathRgn(x, y, width, height, radius);
  }
#endif

#ifndef wx_x
  if (is_ps) {
#endif

    lt = new wxRegion(dc, NULL, TRUE);
    rt = new wxRegion(dc, NULL, TRUE);
    lb = new wxRegion(dc, NULL, TRUE);
    rb = new wxRegion(dc, NULL, TRUE);
    w = new wxRegion(dc, NULL, TRUE);
    h = new wxRegion(dc, NULL, TRUE);

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

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToDeviceX(x);
  y = dc->FLogicalToDeviceY(y);
  width = dc->FLogicalToDeviceX(xw) - x;
  height = dc->FLogicalToDeviceY(yh) - y;
#if defined(wx_msw) || defined(wx_mac)
  xradius = (int)(dc->FLogicalToDeviceXRel(radius));
  yradius = (int)(dc->FLogicalToDeviceYRel(radius));
#endif

  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;

  if (is_ps) {
    height = -height;

    /* So bitmap-based region is right */
    y = -y;
  }

#ifdef wx_msw
  rgn = CreateRoundRectRgn(ix, iy, ix + iw, iy + ih, xradius, yradius); // SET-ORIGIN FLAGGED
#endif
#ifdef wx_mac
  /* This code uses the current port. We don't know what the current
     port might be, so we have to pick one to be sure that QuickDraw
     is allowed. */
  {
    CGrafPtr savep;
    GDHandle savegd;
    
    ::GetGWorld(&savep, &savegd);  
    ::SetGWorld(wxGetGrafPtr(), GetMainDevice());

    rgn = NewRgn();
    OpenRgn();
    {
      Rect r2;
      SetRect(&r2, ix, iy, ix + iw, iy + ih);
      FrameRoundRect(&r2, xradius, yradius); // SET-ORIGIN FLAGGED
      CloseRgn(rgn);
    }

    ::SetGWorld(savep, savegd);
  }    
#endif
}

void wxRegion::SetEllipse(double x, double y, double width, double height)
{
  double xw, yh;
#if defined(wx_msw) || defined(wx_mac)
  int ix, iy, iw, ih;
#endif

  Cleanup();

#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    prgn = new wxArcPathRgn(x, y, width, height, 0, 2 * wxPI);
  }
#endif

  xw = x + width;
  yh = y + height;
  x = dc->FLogicalToDeviceX(x);
  y = dc->FLogicalToDeviceY(y);
  width = dc->FLogicalToDeviceX(xw) - x;
  height = dc->FLogicalToDeviceY(yh) - y;

  if (is_ps) {
    wxPSRgn *ra;

    height = -height;

    ra = new wxPSRgn_Atomic("", "ellipse");
    ps = ra;
    Put(x + width / 2); Put(" "); Put(y - height / 2); Put(" moveto\n");
    Put(x + width / 2); Put(" "); Put(y - height / 2); Put(" ");
    Put(width / 2); Put(" "); Put(height / 2); Put(" 0 360 ellipse\n");
    Put("closepath\n");

    /* So bitmap-based region is right */
    y = -y;
  }

#if defined(wx_msw) || defined(wx_mac)
  ix = (int)floor(x);
  iy = (int)floor(y);
  iw = ((int)floor(x + width)) - ix;
  ih = ((int)floor(y + height)) - iy;
#endif

#ifdef wx_msw
  rgn = CreateEllipticRgn(ix, iy, ix + iw, iy + ih);
#endif
#ifdef wx_mac
  /* This code uses the current port. We don't know what the current
     port might be, so we have to pick one to be sure that QuickDraw
     is allowed. */
  {
    CGrafPtr savep;
    GDHandle savegd;
    
    ::GetGWorld(&savep, &savegd);  
    ::SetGWorld(wxGetGrafPtr(), GetMainDevice());

    rgn = NewRgn();
    OpenRgn();
    {
      Rect r;
      SetRect(&r, ix, iy, ix + iw, iy + ih);
      FrameOval(&r);
      CloseRgn(rgn);
    }

    ::SetGWorld(savep, savegd);
  }
#endif

#ifdef wx_x
  {
    int npoints;
    XPoint *p;
    p = wxEllipseToPolygon(width, height, x, y, &npoints);
    rgn = XPolygonRegion(p, npoints - 1, WindingRule);
  }
#endif
}

#ifdef wx_x
# define POINT XPoint
#endif
#ifdef wx_mac
# define POINT MyPoint
  typedef struct { int x, y; } MyPoint;
#endif

typedef struct { double x, y; } FPoint;

void wxRegion::SetPolygon(int n, wxPoint points[], double xoffset, double yoffset, int fillStyle)
{
  POINT *cpoints;
  FPoint *fpoints;
  int i, v;
  double vf;

  Cleanup();

  if (n < 2)
    return;

#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    prgn = new wxPolygonPathRgn(n, points, xoffset, yoffset, fillStyle);
  }
#endif

  cpoints = new POINT[n];
  fpoints = (is_ps ? new FPoint[n] : (FPoint *)NULL);
  for (i = 0; i < n; i++) {
    v = dc->LogicalToDeviceX(points[i].x + xoffset);
    cpoints[i].x = v;
    v = dc->LogicalToDeviceY(points[i].y + yoffset);
    cpoints[i].y = v;
    if (fpoints) {
      vf = dc->FLogicalToDeviceX(points[i].x + xoffset);
      fpoints[i].x = vf;
      vf = dc->FLogicalToDeviceY(points[i].y + yoffset);
      fpoints[i].y = vf;
    }
  }

  if (is_ps) {
    wxPSRgn *ra;
    ra = new wxPSRgn_Atomic("", "poly");
    ps = ra;
    Put(fpoints[0].x); Put(" "); Put(fpoints[0].y); Put(" moveto\n");
    for (i = 1; i < n; i++) {
      Put(fpoints[i].x); Put(" "); Put(fpoints[i].y); Put(" lineto\n");
    }
    Put("closepath\n");

    /* So bitmap-based region is right */
    for (i = 0; i < n; i++) {
      cpoints[i].y = -cpoints[i].y;
    }
  }

#ifdef wx_msw
  rgn = CreatePolygonRgn(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? ALTERNATE : WINDING);
#endif
#ifdef wx_x
  rgn = XPolygonRegion(cpoints, n, (fillStyle == wxODDEVEN_RULE) ? EvenOddRule : WindingRule);
#endif
#ifdef wx_mac
  /* This code uses the current port. We don't know what the current
     port might be, so we have to pick one to be sure that QuickDraw
     is allowed. */
  {
    CGrafPtr savep;
    GDHandle savegd;
    
    ::GetGWorld(&savep, &savegd);  
    ::SetGWorld(wxGetGrafPtr(), GetMainDevice());

    rgn = NewRgn();
    OpenRgn();
    MoveTo(cpoints[0].x, cpoints[0].y);
    for (i = 0; i < n; i++) {
      LineTo(cpoints[i].x, cpoints[i].y);
    }
    LineTo(cpoints[0].x, cpoints[0].y);
    CloseRgn(rgn);

    ::SetGWorld(savep, savegd);
  }
#endif
}

void wxRegion::SetArc(double x, double y, double w, double h, double start, double end)
{
  wxRegion *r;
  static double pi;
  int saw_start = 0, saw_end = 0, closed = 0;
  double cx, cy;
  wxPoint *a;
  int n;
#ifdef WX_USE_PATH_RGN
  char save_no_prgn;
#endif

#ifdef MZ_PRECISE_GC
  a = (wxPoint *)GC_malloc_atomic(sizeof(wxPoint) * 20);
#else
  a = new wxPoint[20];
#endif

#ifdef WX_USE_PATH_RGN
  save_no_prgn = no_prgn;
  if (!no_prgn) {
    prgn = new wxArcPathRgn(x, y, w, h, start, end);
    no_prgn = 1;
  }
#endif

  SetEllipse(x, y, w, h);

  if (start == end) return;

  r = new wxRegion(dc, NULL, TRUE);

  if (!pi)
    pi = 2 * asin((double)1.0);

  start = fmod((double)start, 2*pi);
  end = fmod((double)end, 2*pi);
  if (start < 0)
    start += 2*pi;
  if (end < 0)
    end += 2*pi;

  cx = x + w/2;
  cy = y + h/2;

  a[0].x = ((w+2) / 2) * cos(end) + cx;
  a[0].y = ((h+2) / 2) * (-sin(end)) + cy;

  a[1].x = cx;
  a[1].y = cy;

  a[2].x = ((w+2) / 2) * cos(start) + cx;
  a[2].y = ((h+2) / 2) * (-sin(start)) + cy;

  n = 3;

  if (!saw_start && (start < (pi / 2)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w + 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y - 2;
  } else
    closed = saw_start;

  if (!saw_start && (start < pi))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = x - 2;
    a[n++].y = cy;
  } else
    closed = saw_start;

  if (!saw_start && (start < (1.5 * pi)))
    saw_start = 1;
  if (!saw_end && (end > start) && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y + h + 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h + 2;
  } else
    closed = saw_start;

  saw_start = 1;
  saw_end = (end > start);
  
  if (saw_start && !closed) {
    a[n].x = x + w + 2;
    a[n++].y = y + h + 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = x + w + 2;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (pi / 2)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x + w + 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y - 2; 
  } else
    closed = saw_start;
  
  if (!saw_end && (end < pi))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y - 2;
  }
  if (saw_start && !saw_end) {
    a[n].x = x - 2;
    a[n++].y = cy;    
  } else
    closed = saw_start;

  if (!saw_end && (end < (1.5 * pi)))
    saw_end = 1;
  if (saw_start && !closed) {
    a[n].x = x - 2;
    a[n++].y = y + h + 2;
  } 
  if (saw_start && !saw_end) {
    a[n].x = cx;
    a[n++].y = y + h + 2;
  } else
    closed = saw_start;

  if (!closed) {
    a[n].x = x + w + 2;
    a[n++].y = y + h + 2;
  }

  r->SetPolygon(n, a);

  Intersect(r);

#ifdef WX_USE_PATH_RGN
  no_prgn = save_no_prgn;
#endif
}

void wxRegion::Union(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->ReallyEmpty()) return;

  if (is_ps) {
    if (!ps)
      ps = r->ps;
    else {
      wxPSRgn *ru;
      ru = new wxPSRgn_Union(ps, r->ps);
      ps = ru;
    }
  }

#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    if (!r->prgn) abort();
    if (!prgn)
      prgn = r->prgn;
    else {
      wxPathRgn *pr;
      pr = new wxUnionPathRgn(prgn, r->prgn);
      prgn = pr;
    }
  }
#endif

#ifdef wx_msw
  if (!rgn) {
    rgn = CreateRectRgn(0, 0, 1, 1); // SET-ORIGIN FLAGGED
    CombineRgn(rgn, r->rgn, rgn, RGN_COPY);
  } else
    CombineRgn(rgn, r->rgn, rgn, RGN_OR);
#endif
#ifdef wx_x
  if (!rgn) {
    rgn = XCreateRegion();
  }
  XUnionRegion(rgn, r->rgn, rgn);
#endif
#ifdef wx_mac
  if (!rgn) {
    rgn = NewRgn();
  }
  UnionRgn(rgn, r->rgn, rgn);
#endif
}

void wxRegion::Intersect(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->ReallyEmpty()) {
    Cleanup();
    ps = NULL;
    return;
  }

#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    wxPathRgn *pr;
    if (!r->prgn) abort();
    pr = new wxIntersectPathRgn(prgn, r->prgn);
    prgn = pr;
  }
#endif

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

  if (ReallyEmpty()) {
    Cleanup();
    ps = NULL;
  } else if (is_ps) {
    wxPSRgn *ri;
    ri = new wxPSRgn_Intersect(ps, r->ps);
    ps = ri;
  }
}

void wxRegion::Subtract(wxRegion *r)
{
  if (r->dc != dc) return;
  if (r->ReallyEmpty()) return;

#ifdef WX_USE_PATH_RGN
  if (!no_prgn) {
    /* wxDiffPathRgn is only half a subtract; the result must be intersected with the first part */
    wxPathRgn *pr;
    if (!r->prgn) abort();
    pr = new wxDiffPathRgn(prgn, r->prgn);
    pr = new wxIntersectPathRgn(prgn, pr);
    prgn = pr;
  }
#endif

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

  if (ReallyEmpty()) {
    Cleanup();
    ps = NULL;
    return;
  } else if (is_ps) {
    /* wxPSRgn_Diff is only half a subtract; the result must be intersected with the first part */
    wxPSRgn *rd, *ri;
    rd = new wxPSRgn_Diff(ps, r->ps);
    ri = new wxPSRgn_Intersect(ps, rd);
    ps = ri;
  }
}
  
void wxRegion::BoundingBox(double *x, double *y, double *w, double *h)
{
  if (Empty()) {
    *x = *y = *w = *h = 0;
    return;
  } else {
    double v;
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
    {
      Rect r;
      GetRegionBounds(rgn,&r);
      *x = r.left;
      *y = r.top;
      *w = r.right - *x;
      *h = r.bottom - *y;
    }
#endif

    if (is_ps) {
      /* Bitmap-based region is stored upside-down */
      *y = -(*y);
    }
    
    v = dc->DeviceToLogicalX((int)*x);
    *x = v;
    v = dc->DeviceToLogicalY((int)*y);
    *y = v;
    v = dc->DeviceToLogicalXRel((int)*w);
    *w = v;
    v = dc->DeviceToLogicalYRel((int)*h);
    *h = v;
  }
}

Bool wxRegion::Empty()
{
#ifdef wx_msw
  RECT r;
  if (!rgn) return TRUE;

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

Bool wxRegion::ReallyEmpty()
{
  return Empty() && !prgn;
}

void wxRegion::Put(const char *s)
{
  long l, psl;
  char *naya;

  l = strlen(s);
  psl = strlen(((wxPSRgn_Atomic *)ps)->s);

  naya = new WXGC_ATOMIC char[l + psl + 1];
  memcpy(naya, ((wxPSRgn_Atomic *)ps)->s, psl);
  memcpy(naya + psl, s, l);
  naya[psl + l] = 0;
  
  ((wxPSRgn_Atomic *)ps)->s = naya;
}

void wxRegion::Put(double d)
{
  char s[100];
  sprintf(s, "%f", d);
  Put(s);
}

#ifdef WX_USE_PATH_RGN
void wxRegion::Install(long target)
{
  if (prgn) {
#ifdef WX_USE_CAIRO
    cairo_matrix_t *m;
    m = cairo_matrix_create();
    cairo_current_matrix(CAIRO_DEV, m);
    cairo_default_matrix(CAIRO_DEV);
    cairo_translate(CAIRO_DEV, geometry ? geometry[0] : 0, geometry ? geometry[1] : 0);
    cairo_scale(CAIRO_DEV, geometry ? geometry[2] : 1, geometry ? geometry[3] : 1);
    cairo_init_clip(CAIRO_DEV);
    cairo_new_path(CAIRO_DEV);
#endif
#ifdef wx_mac
    CGContextRef cg = (CGContextRef)target;
    CGAffineTransform xform;
    PathTarget *t;
    CGMutablePathRef path;
    int i;
    
    if (paths) {
      for (i = 0; i < npaths; i++) {
	CGContextBeginPath(cg);
	CGContextAddPath(cg, paths[i]);
	CGContextClip(cg);
      }
      return;
    }
  
    path = CGPathCreateMutable();
  
    t = (PathTarget *)malloc(sizeof(PathTarget));
    t->path = path;
    xform = CGAffineTransformMakeTranslation(geometry ? geometry[0] : 0, geometry ? geometry[1] : 0);
    xform = CGAffineTransformScale(xform, geometry ? geometry[2] : 1, geometry ? geometry[3] : 1);
    t->xform = xform;

    t->paths = NULL;
    t->npaths = 0;
    t->apaths = 0;
    
    target = (long)t;
#endif
#ifdef wx_msw
    Graphics *g = (Graphics *)target;
    GraphicsPath *gp;
    PathTarget *t;
	int i;

    if (paths) {
      wxGSetClip(g, paths[0], CombineModeReplace);
      for (i = 1; i < npaths; i++) {
	wxGSetClip(g, paths[i], CombineModeIntersect);
      }
      return;
    }
    
    gp = wxGPathNew(FillModeWinding);

    t = (PathTarget *)malloc(sizeof(PathTarget));
    t->path = gp;
    t->npaths = 0;
    t->apaths = 0;

    target = (long)t;
#endif

    prgn->Install(target, 0);

#ifdef WX_USE_CAIRO
    cairo_clip(CAIRO_DEV);
    cairo_new_path(CAIRO_DEV);
    cairo_set_matrix(CAIRO_DEV, m);
    cairo_matrix_destroy(m);
#endif
#if defined(wx_mac) || defined(wx_msw)
    npaths = t->npaths + 1;
    paths = new WXGC_ATOMIC PathTargetPath_t[npaths];
    for (i = 0; i < npaths - 1; i++) {
      paths[i] = t->paths[i];
    }
    paths[npaths - 1] = t->path;

    free(t);
# ifdef wx_mac
    for (i = 0; i < npaths; i++) {
      CGContextBeginPath(cg);
      CGContextAddPath(cg, paths[i]);
      CGContextClip(cg);
    }
# endif
# ifdef wx_msw
    {
      Matrix *m;
      m = wxGMatrixNew();
      wxGMatrixTranslate(m, geometry ? geometry[0] : 0, geometry ? geometry[1] : 0);
      wxGMatrixScale(m, geometry ? geometry[2] : 1, geometry ? geometry[3] : 1);
      wxGPathTransform(paths[0], m);
      wxGSetClip(g, paths[0], CombineModeReplace);
      for (i = 1; i < npaths; i++) {
	wxGPathTransform(paths[i], m);
	wxGSetClip(g, paths[i], CombineModeIntersect);
      }
      wxGMatrixRelease(m);
    }
# endif
#endif
  }
}
#endif

/***************************************************************************************/

char *wxPSRgn_Union::GetString()
{
  return MakeString("", "", "");
}

char *wxPSRgn_Composite::MakeString(const char *prefix, const char *infix, const char *suffix)
{
  char *sa, *sb;
  int plen, ilen, slen;
  int alen, blen;
  char *sr;

  sa = a->GetString();
  sb = b->GetString();
  plen = strlen(prefix);
  ilen = strlen(infix);
  slen = strlen(suffix);
  alen = strlen(sa);
  blen = strlen(sb);
  sr = new WXGC_ATOMIC char[alen + blen + plen + ilen + slen + 1];

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
  wxPSRgn *la, *lb;
  wxPSRgn *r = NULL, **al, **bl;
  int na, nb, i, j;

  la = a->Lift();
  lb = b->Lift();

  if (!la->is_intersect
      && !lb->is_intersect
      && (a == la) && (b == lb))
    return this;

  /* (A n B) U (C n D) = (A U C) n (A U D) n (B U C) n (B U D) */

  /* count: */
  na = FlattenIntersects(NULL, la, 0);
  nb = FlattenIntersects(NULL, lb, 0);

  al = new wxPSRgn*[na];
  bl = new wxPSRgn*[nb];

  /* flatten: */
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
  wxPSRgn *la, *lb;

  la = a->Lift();
  lb = b->Lift();

  if ((la == a) && (lb == b))
    return this;
  else
    return new wxPSRgn_Intersect(la, lb);
}


char *wxPSRgn_Diff::GetString()
{
  /* Subtract by making the paths the reverse of each other,
     so winding numbers will zero out in the overlap.
     Doesn't work for multiple regions, and the reverse of
     a path doesn't exclude the boundary enclosed by the
     original path. */
  return MakeString("", "reversepath\n", "reversepath\n");
}

wxPSRgn *wxPSRgn_Diff::Lift()
{
  wxPSRgn *la, *lb;
  wxPSRgn *r = NULL, **al, **bl;
  int na, nb, i;

  la = a->Lift();
  lb = b->Lift();

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
      wxPSRgn *s;
      s = new wxPSRgn_Diff(la, bl[i]);
      if (r) {
	r = new wxPSRgn_Union(r, s);
      } else
	r = s;
    }

    return r->Lift(); /* Handles intersections in la */
  } else {
    /* (A n B) - C = (A - C) n (B - C)   [note: C has no intersections] */
    na = FlattenIntersects(NULL, la, 0);
    al = new wxPSRgn*[na];
    FlattenIntersects(al, la, 0);
    
    for (i = 0; i < na; i++) {
      wxPSRgn *s;
      s = new wxPSRgn_Diff(al[i], lb);
      if (r) {
	r = new wxPSRgn_Intersect(r, s);
      } else
	r = s;
    }

    return r;
  }
}

/***************************************************************************************/

#ifdef WX_USE_PATH_RGN

wxPathRgn::wxPathRgn()
: wxObject(FALSE)
{
}

wxPathRgn::~wxPathRgn()
{
}

wxRectanglePathRgn::wxRectanglePathRgn(double _x, double _y, double _width, double _height)
{
  x = _x;
  y = _y;
  width = _width;
  height = _height;
}

void wxRectanglePathRgn::Install(long target, Bool reverse)
{
#ifdef WX_USE_CAIRO
  cairo_move_to(CAIRO_DEV, x, y);
  if (reverse) {
    cairo_rel_line_to(CAIRO_DEV, 0, height);
    cairo_rel_line_to(CAIRO_DEV, width, 0);
    cairo_rel_line_to(CAIRO_DEV, 0, -height);
  } else {
    cairo_rel_line_to(CAIRO_DEV, width, 0);
    cairo_rel_line_to(CAIRO_DEV, 0, height);
    cairo_rel_line_to(CAIRO_DEV, -width, 0);
  }
  cairo_close_path(CAIRO_DEV);
#endif
#ifdef wx_mac
  CGPathMoveToPoint(CGPATH, CGXFORM, x, y);
  if (reverse) {
    CGPathAddLineToPoint(CGPATH, CGXFORM, x, y + height);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width, y + height);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width, y);
  } else {
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width, y);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width, y + height);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x, y + height);
  }
  CGPathCloseSubpath(CGPATH);
#endif
#ifdef wx_msw
  if (reverse) {
    wxGPathAddLine(GP, x, y, x, y + height);
    wxGPathAddLine(GP, x, y + height, x + width, y + height);
    wxGPathAddLine(GP, x + width, y + height, x + width, y);
  } else {
    wxGPathAddLine(GP, x, y, x + width, y);
    wxGPathAddLine(GP, x + width, y, x + width, y + height);
    wxGPathAddLine(GP, x + width, y + height, x, y + height);
  }
  wxGPathCloseFigure(GP);
#endif
}

wxRoundedRectanglePathRgn::wxRoundedRectanglePathRgn(double _x, double _y, double _width, double _height, double _radius)
{
  x = _x;
  y = _y;
  width = _width;
  height = _height;
  radius = _radius;

  if (radius < 0) {
    if (width > height)
      radius = radius * height;
    else
      radius = radius * width;
  }
}

void wxRoundedRectanglePathRgn::Install(long target, Bool reverse)
{
#ifdef WX_USE_CAIRO
  double w = width, h = height;
  if (reverse) {
    cairo_move_to(CAIRO_DEV, x, y + radius);
    cairo_line_to(CAIRO_DEV, x, y + h - radius);
    cairo_arc_negative(CAIRO_DEV, x + radius, y + h - radius, radius, wxPI, 0.5 * wxPI);
    cairo_line_to(CAIRO_DEV, x + w - radius, y + h);
    cairo_arc_negative(CAIRO_DEV, x + w - radius, y + h - radius, radius, 0.5 * wxPI, 0);
    cairo_line_to(CAIRO_DEV, x + w, y + radius);
    cairo_arc_negative(CAIRO_DEV, x + w - radius, y + radius, radius, 2 * wxPI, 1.5 * wxPI);
    cairo_line_to(CAIRO_DEV, x + radius, y);
    cairo_arc_negative(CAIRO_DEV, x + radius, y + radius, radius, 1.5 * wxPI, wxPI);
    cairo_line_to(CAIRO_DEV, x, y + radius);
  } else {
    cairo_move_to(CAIRO_DEV, x, y + radius);
    cairo_arc(CAIRO_DEV, x + radius, y + radius, radius, wxPI, 1.5 * wxPI);
    cairo_line_to(CAIRO_DEV, x + w - radius, y);
    cairo_arc(CAIRO_DEV, x + w - radius, y + radius, radius, 1.5 * wxPI, 2 * wxPI);
    cairo_line_to(CAIRO_DEV, x + w, y + h - radius);
    cairo_arc(CAIRO_DEV, x + w - radius, y + h - radius, radius, 0, 0.5 * wxPI);
    cairo_line_to(CAIRO_DEV, x + radius, y + h);
    cairo_arc(CAIRO_DEV, x + radius, y + h - radius, radius, 0.5 * wxPI, wxPI);
    cairo_line_to(CAIRO_DEV, x, y + radius);
  }
  cairo_close_path(CAIRO_DEV);
#endif
#ifdef wx_mac
  if (reverse) {
    CGPathMoveToPoint(CGPATH, CGXFORM, x + radius, y);
    CGPathAddArc(CGPATH, CGXFORM, x + radius, y + radius, radius, 1.5 * wxPI, 1.0 * wxPI, TRUE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x, y + height - radius);
    CGPathAddArc(CGPATH, CGXFORM, x + radius, y + height - radius, radius, 1.0 * wxPI, 0.5 * wxPI, TRUE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width - radius, y + height);
    CGPathAddArc(CGPATH, CGXFORM, x + width - radius, y + height - radius, radius, 0.5 * wxPI, 0, TRUE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width, y + radius);
    CGPathAddArc(CGPATH, CGXFORM, x + width - radius, y + radius, radius, 2 * wxPI, 1.5 * wxPI, TRUE);
  } else {
    CGPathMoveToPoint(CGPATH, CGXFORM, x + radius, y);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width - radius, y);
    CGPathAddArc(CGPATH, CGXFORM, x + width - radius, y + radius, radius, 1.5 * wxPI, 2 * wxPI, FALSE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + width, y + height - radius);
    CGPathAddArc(CGPATH, CGXFORM, x + width - radius, y + height - radius, radius, 0, 0.5 * wxPI, FALSE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x + radius, y + height);
    CGPathAddArc(CGPATH, CGXFORM, x + radius, y + height - radius, radius, 0.5 * wxPI, 1.0 * wxPI, FALSE);
    CGPathAddLineToPoint(CGPATH, CGXFORM, x, y + radius);
    CGPathAddArc(CGPATH, CGXFORM, x + radius, y + radius, radius, 1.0 * wxPI, 1.5 * wxPI, FALSE);
  }
  CGPathCloseSubpath(CGPATH);
#endif
#ifdef wx_msw
  if (reverse) {
    wxGPathAddArc(GP, x, y, radius * 2, radius * 2, 270, -90);
    wxGPathAddLine(GP, x, y + radius, x, y + height - radius);
    wxGPathAddArc(GP, x, y + height - 2 * radius, 2 * radius, 2 * radius, 180, -90);
    wxGPathAddLine(GP, x + radius, y + height, x + width - radius, y + height);
    wxGPathAddArc(GP, x + width - 2 * radius, y + height - 2 * radius, 2 * radius, 2 * radius, 90, -90);
    wxGPathAddLine(GP, x + width, y + height - radius, x + width, y + radius);
    wxGPathAddArc(GP, x + width - 2 * radius, y, radius * 2, radius * 2, 360, -90);
  } else {
    wxGPathAddArc(GP, x, y, radius * 2, radius * 2, 180, 90);
    wxGPathAddLine(GP, x + radius, y, x + width - radius, y);
    wxGPathAddArc(GP, x + width - 2 * radius, y, radius * 2, radius * 2, 270, 90);
    wxGPathAddLine(GP, x + width, y + radius, x + width, y + height - radius);
    wxGPathAddArc(GP, x + width - 2 * radius, y + height - 2 * radius, 2 * radius, 2 * radius, 0, 90);
    wxGPathAddLine(GP, x + width - radius, y + height, x + radius, y + height);
    wxGPathAddArc(GP, x, y + height - 2 * radius, 2 * radius, 2 * radius, 90, 90);
  }
  wxGPathCloseFigure(GP);
#endif
}

wxPolygonPathRgn::wxPolygonPathRgn(int _n, wxPoint _points[], double _xoffset, double _yoffset, int _fillStyle)
{
  n = _n;
  points = _points;
  xoffset = _xoffset;
  yoffset = _yoffset;
  fillStyle = _fillStyle;
}

void wxPolygonPathRgn::Install(long target, Bool reverse)
{
#ifdef WX_USE_CAIRO
  int i;
  if (reverse) {
    cairo_move_to(CAIRO_DEV, points[n-1].x + xoffset, points[n-1].y + yoffset);
    for (i = n-1; i--; ) {
      cairo_line_to(CAIRO_DEV, points[i].x + xoffset, points[i].y + yoffset);
    }
  } else {
    cairo_move_to(CAIRO_DEV, points[0].x + xoffset, points[0].y + yoffset);
    for (i = 1; i < n; i++) {
      cairo_line_to(CAIRO_DEV, points[i].x + xoffset, points[i].y + yoffset);
    }
  }
  cairo_close_path(CAIRO_DEV);
#endif
#ifdef wx_mac
  int i;
  if (reverse) {
    CGPathMoveToPoint(CGPATH, CGXFORM, points[n-1].x + xoffset, points[n-1].y + yoffset);
    for (i = n-1; i--; ) {
      CGPathAddLineToPoint(CGPATH, CGXFORM, points[i].x + xoffset, points[i].y + yoffset);
    }
  } else {
    CGPathMoveToPoint(CGPATH, CGXFORM, points[0].x + xoffset, points[0].y + yoffset);
    for (i = 1; i < n; i++) {
      CGPathAddLineToPoint(CGPATH, CGXFORM, points[i].x + xoffset, points[i].y + yoffset);
    }
  }
  CGPathCloseSubpath(CGPATH);
#endif
#ifdef wx_msw
  int i;
  if (reverse) {
    for (i = n - 1; i--; ) {
      wxGPathAddLine(GP,
		     points[i+1].x + xoffset, points[i+1].y + yoffset,
		     points[i].x + xoffset, points[i].y + yoffset);
    }
  } else {
    for (i = 0; i < n - 1; i++) {
      wxGPathAddLine(GP,
		     points[i].x + xoffset, points[i].y + yoffset,
		     points[i+1].x + xoffset, points[i+1].y + yoffset);
    }
  }
  wxGPathCloseFigure(GP);
#endif
}

wxArcPathRgn::wxArcPathRgn(double _x, double _y, double _w, double _h, double _start, double _end)
{
  x = _x;
  y = _y;
  w = _w;
  h = _h;
  start = _start;
  end = _end;
}

void wxArcPathRgn::Install(long target, Bool reverse)
{
#ifdef WX_USE_CAIRO
  cairo_matrix_t *m;
  m = cairo_matrix_create();
  cairo_current_matrix(CAIRO_DEV, m);
  cairo_translate(CAIRO_DEV, x, y);
  cairo_scale(CAIRO_DEV, w, h);
  if ((start != 0.0) || (end != (2 * wxPI)))
    cairo_move_to(CAIRO_DEV, 0.5, 0.5);
  if (!reverse)
    cairo_arc(CAIRO_DEV, 0.5, 0.5, 0.5, -end, -start);
  else
    cairo_arc_negative(CAIRO_DEV, 0.5, 0.5, 0.5, -start, -end);
  cairo_set_matrix(CAIRO_DEV, m);
  cairo_matrix_destroy(m);
  cairo_close_path(CAIRO_DEV);
#endif
#ifdef wx_mac
  CGAffineTransform xform;
  xform = CGAffineTransformTranslate(*CGXFORM, x, y);
  xform = CGAffineTransformScale(xform, w, h);
  if ((start != 0.0) || (end != (2 * wxPI)))
    CGPathMoveToPoint(CGPATH, &xform, 0.5, 0.5);
  if (!reverse)
    CGPathAddArc(CGPATH, &xform, 0.5, 0.5, 0.5, (2 * wxPI) - end, (2 * wxPI) - start, FALSE);
  else
    CGPathAddArc(CGPATH, &xform, 0.5, 0.5, 0.5, (2 * wxPI) - start, (2 * wxPI) - end, TRUE);
  CGPathCloseSubpath(CGPATH);
#endif
#ifdef wx_msw
  double init, span;
  if ((start == 0.0) && (end == 2 * wxPI)) {
    if (reverse) {
      wxGPathAddArc(GP, x, y, w, h, 360.0, -360.0);
    } else {
      wxGPathAddArc(GP, x, y, w, h, 0.0, 360.0);
    }
  } else {
    init = (2 * wxPI - start) * 180 / wxPI;
    init = fmod(init, 360.0);
    if (init < 0.0)
      init += 360.0;
    
    span = (start - end) * 180 / wxPI;
    span = fmod(span, 360.0);
    if (span > 0)
      span -= 360.0;
    if (reverse) {
      wxGPathAddPie(GP, x, y, w, h, init + span, -span);
    } else {
      wxGPathAddPie(GP, x, y, w, h, init, span);
    }
  }
  wxGPathCloseFigure(GP);
#endif
}

wxUnionPathRgn::wxUnionPathRgn(wxPathRgn *_f, wxPathRgn *_s)
{
  if (!_f || !_s)
    abort();
  a = _f;
  b = _s;
}

void wxUnionPathRgn::Install(long target, Bool reverse)
{
  a->Install(target, reverse);
  b->Install(target, reverse);
}

wxIntersectPathRgn::wxIntersectPathRgn(wxPathRgn *_f, wxPathRgn *_s)
{
  if (!_f || !_s)
    abort();
  a = _f;
  b = _s;
}

void wxIntersectPathRgn::Install(long target, Bool reverse)
{
  a->Install(target, reverse);
#ifdef WX_USE_CAIRO
  cairo_clip(CAIRO_DEV);
  cairo_new_path(CAIRO_DEV);
#endif
#if defined(wx_mac) || defined(wx_msw)
  {
    PathTarget *t = (PathTarget *)target;
    PathTargetPath_t path;
    int i;

    if (t->npaths + 1 >= t->apaths) {
      PathTargetPath_t *naya;
      int n = (t->apaths + 5) * 2;
      
      naya = new WXGC_ATOMIC PathTargetPath_t[n];
      for (i = 0; i < t->npaths; i++) {
	naya[i] = t->paths[i];
      }

      t->paths = naya;
      t->apaths = n;
    }
    t->paths[t->npaths++] = t->path;
# ifdef wx_mac
    path = CGPathCreateMutable();
# endif
# ifdef wx_msw
    path = wxGPathNew(FillModeWinding);
# endif
    t->path = path;
  }
#endif
  b->Install(target, reverse);
}

wxDiffPathRgn::wxDiffPathRgn(wxPathRgn *_f, wxPathRgn *_s)
{
  if (!_f || !_s)
    abort();
  a = _f;
  b = _s;
}

void wxDiffPathRgn::Install(long target, Bool reverse)
{
  a->Install(target, reverse);
  b->Install(target, !reverse);
}

wxPathRgn *wxPathRgn::Lift()
{
  return this;
}

Bool wxPathRgn::IsIntersect()
{
  return FALSE;
}

int wxPathRgn::FlattenIntersects(wxPathRgn **l, wxPathRgn *r, int i)
{
  if (r->IsIntersect())
    return FlattenIntersects(l, ((wxIntersectPathRgn *)r)->b, 
			     FlattenIntersects(l, ((wxIntersectPathRgn *)r)->a, i));
  
  if (l)
    l[i] = r;

  return i + 1;
}

wxPathRgn *wxUnionPathRgn::Lift()
{
  wxPathRgn *la, *lb;
  wxPathRgn *r = NULL, **al, **bl;
  int na, nb, i, j;

  la = a->Lift();
  lb = b->Lift();

  if (!la->IsIntersect()
      && !lb->IsIntersect()
      && (a == la) && (b == lb))
    return this;

  /* (A n B) U (C n D) = (A U C) n (A U D) n (B U C) n (B U D) */

  /* count: */
  na = FlattenIntersects(NULL, la, 0);
  nb = FlattenIntersects(NULL, lb, 0);

  al = new wxPathRgn*[na];
  bl = new wxPathRgn*[nb];

  /* flatten: */
  FlattenIntersects(al, la, 0);
  FlattenIntersects(bl, lb, 0);

  for (i = 0; i < na; i++) {
    for (j = 0; j < nb; j++) {
      wxPathRgn *c;
      c = new wxUnionPathRgn(al[i], bl[j]);
      if (r)
	r = new wxIntersectPathRgn(r, c);
      else
	r = c;
    }
  }

  return r;
}

wxPathRgn *wxIntersectPathRgn::Lift()
{
  wxPathRgn *la, *lb;

  la = a->Lift();
  lb = b->Lift();

  if ((la == a) && (lb == b))
    return this;
  else
    return new wxIntersectPathRgn(la, lb);
}

Bool wxIntersectPathRgn::IsIntersect()
{
  return TRUE;
}

wxPathRgn *wxDiffPathRgn::Lift()
{
  wxPathRgn *la, *lb;
  wxPathRgn *r = NULL, **al, **bl;
  int na, nb, i;

  la = a->Lift();
  lb = b->Lift();

  if (!la->IsIntersect()
      && !lb->IsIntersect()
      && (a == la) && (b == lb))
    return this;

  if (lb->IsIntersect()) {
    /* A \ (B n C) = (A \ B) u (A \ C) */
    nb = FlattenIntersects(NULL, lb, 0);
    bl = new wxPathRgn*[nb];
    FlattenIntersects(bl, lb, 0);
    
    for (i = 0; i < nb; i++) {
      wxPathRgn *s;
      s = new wxDiffPathRgn(la, bl[i]);
      if (r) {
	r = new wxUnionPathRgn(r, s);
      } else
	r = s;
    }

    return r->Lift(); /* Handles intersections in la */
  } else {
    /* (A n B) - C = (A - C) n (B - C)   [note: C has no intersections] */
    na = FlattenIntersects(NULL, la, 0);
    al = new wxPathRgn*[na];
    FlattenIntersects(al, la, 0);
    
    for (i = 0; i < na; i++) {
      wxPathRgn *s;
      s = new wxDiffPathRgn(al[i], lb);
      if (r) {
	r = new wxIntersectPathRgn(r, s);
      } else
	r = s;
    }

    return r;
  }
}

#endif
