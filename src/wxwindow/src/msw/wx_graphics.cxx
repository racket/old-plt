
#define WX_GRAPHICS_EXPORT __declspec(dllexport)
#define WX_GPROC(x) x
#include "wx_graphics.h"

/* This file effectively converts the GDI+ API from C++ to C, to make
   GDI+ easier to use via LoadModule() and GetProcAddress(). */

/* ********************************************************************** */

Graphics *wxGMake(HDC dc)
{
  return new Graphics(dc);
}

void wxGRelease(Graphics *g)
{
  delete g;
}

GraphicsState wxGSave(Graphics *g)
{
  return g->Save();
}

void wxGRestore(Graphics *g, GraphicsState s)
{
  g->Restore(s);
}

void wxGResetTransform(Graphics *g)
{
  g->ResetTransform();
}

void wxGTranslate(Graphics *g, double x, double y)
{
  g->TranslateTransform(x, y);
}

void wxGScale(Graphics *g, double x, double y)
{
  g->ScaleTransform(x, y);
}

void wxGDrawLine(Graphics *g, Pen *p, double x1, double y1, double x2, double y2)
{
  g->DrawLine(p, x1, y1, x2, y2);
}

void wxGDrawLines(Graphics *g, Pen *p, PointF *pts, int n)
{
  g->DrawLines(g, p, pts, n);
}

void wxGFillRectangleColor(Graphics *g, COLORREF c, double x, double y, double w, double h)
{
  Color col(c);
  Brush b(col);
  g->FillRectangle(b, x, y, w, h);
}

void wxGFillRectangle(Graphics *g, Brush *b, double x, double y, double w, double h)
{
  g->FillRectangle(b, x, y, w, h);
}

void wxGDrawRectangle(Graphics *g, Pen *p, double x, double y, double w, double h)
{
  g->DrawRectangle(p, x, y, w, h);
}

void wxGFillPie(Graphics *g, Brush *b, double x, double y, double w, double h, double start, double span)
{
  g->FillPie(b, x, y, w, h, start, span);
}

void wxGDrawArc(Graphics *g, Pen *p, double x, double y, double w, double h, double start, double span)
{
  g->DrawArc(b, x, y, w, h, start, span);
}

void wxGFillPolygon(Graphics *g, Brush *b, PointF *pts, int n, FillMode m)
{
  g->FillPolygon(b, pts, n, m);
}

void wxGDrawPolygon(Graphics *g, Pen *p, PointF *pts, int n)
{
  g->DrawPolygon(p, pts, n);
}

void wxGFillPath(Graphics *g, Brush *b, GraphicsPath *gp)
{
  g->FillPath(b, gp);
}

void wxGDrawPath(Graphics *g, Pen *p, GraphicsPath *gp)
{
  g->DrawPath(p, gp);
}

GraphicsPath *wxGPathNew(FillMode m)
{
  return new GraphicsPath(m);
}

void wxGPathRelease(GraphicsPath *gp)
{
  delete gp;
}

void wxGPathAddArc(GraphicsPath *gp, double x, double y, double w, double h, double start, double span)
{
  gp->AddArc(x, y, w, h, start, span);
}

void wxGPathAddLine(GraphicsPath *gp, double x1, double y1, double x2, double y2)
{
  gp->AddLine(x, y, w, h, start, span);
}

void wxGPathCloseFigure(GraphicsPath *gp)
{
  gp->CloseFigure();
}


Brush *wxGBrushNew(COLORREF c)
{
  Color col(c);
  return new Brush(col);
}

void wxGBrushRelease(Brush *b)
{
  delete b;
}

Pen *wxGPenNew(COLORREF c, double pw, LineCap cap, LineJoin join)
{
  Pen *p;
  Color col(c);
  p = new Pen(col, pw);
  p->SetEndCap(cap);
  p->SetLineJoin(join);

  return p;
}

void wxGPenRelease(Pen *p)
{
  delete p;
}

/* ********************************************************************** */

static GdiplusStartupInput gdiplusStartupInput;
static ULONG_PTR           gdiplusToken;

void wxGStarup()
{
  GdiplusStartup(&gdiplusToken, &gdiplusStartupInput, NULL);
}

void wxGShutdown()
{
  GdiplusShutdown(gdiplusToken);
}
