/*
 * File:        wb_dc.cc
 * Purpose:     Device context implementation
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * Copyright:   (c) 2004 PLT Scheme, Inc.
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include <math.h>

#include "xfspline.cxx"

// Default constructor
wxbDC::wxbDC(void)
{
  __type = wxTYPE_DC;
  min_x = 0; min_y = 0; max_x = 0; max_y = 0;
  title = NULL;
  clipping = NULL;
  autoSetting = TRUE;
  dcOptimize = TRUE;
  current_bk_mode = wxTRANSPARENT;
}

wxbDC::~wxbDC(void)
{
}

void wxbDC::DrawPolygon(wxList *list, float xoffset, float yoffset,int fillStyle)
{
  int n;
  wxPoint *points, *point;
  wxNode *node;

  int i = 0;

  n = list->Number();
  points = new wxPoint[n];

  for(node = list->First(); node; node = node->Next()) {
    point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawPolygon(n, points, xoffset, yoffset,fillStyle);
}

void wxbDC::DrawLines(wxList *list, float xoffset, float yoffset)
{
  int n;
  wxPoint *points, *point;
  wxNode *node;

  int i = 0;

  n = list->Number();
  points = new wxPoint[n];

  for(node = list->First(); node; node = node->Next()) {
    point = (wxPoint *)node->Data();
    points[i].x = point->x;
    points[i++].y = point->y;
  }
  DrawLines(n, points, xoffset, yoffset);
}

void wxbDC::SetTextForeground(wxColour *colour)
{
  if (colour)
    current_text_foreground->CopyFrom(colour);
}

void wxbDC::SetTextBackground(wxColour *colour)
{
  if (colour)
    current_text_background->CopyFrom(colour);
}

void wxbDC::SetBackgroundMode(int mode)
{
  current_bk_mode = mode;
}

int wxbDC::GetBackgroundMode(void)
{
  return current_bk_mode;
}

void wxbDC::GetSize(float *width, float *height)
{
  if (!(min_x == 1000.0 && min_y == 1000.0 && max_x == -1000.0 && max_y == -1000.0))
  {
    *width = (float)(max_x - min_x);
    *height = (float)(max_y - min_y);
  } else {
    *width = 0.0;
    *height = 0.0;
  }
}

#if USE_SPLINES
// Make a 3-point spline
void wxbDC::DrawSpline(float x1, float y1, float x2, float y2, float x3, float y3)
{
  wxList *point_list;
  wxPoint *point1;
  wxPoint *point2;
  wxPoint *point3;
  wxNode *node;
  wxPoint *p;

  point_list = new wxList;

  point1 = new wxPoint;
  point1->x = x1; point1->y = y1;
  point_list->Append((wxObject*)point1);

  point2 = new wxPoint;
  point2->x = x2; point2->y = y2;
  point_list->Append((wxObject*)point2);

  point3 = new wxPoint;
  point3->x = x3; point3->y = y3;
  point_list->Append((wxObject*)point3);

  DrawSpline(point_list);

  for(node = point_list->First(); node; node = node->Next()) {
    p = (wxPoint *)node->Data();
    delete p;
  }
  delete point_list;
}
#endif

wxColour *wxbDC::GetBackground(void)
{
  wxColour *c;
  c  = new wxColour;
  c->CopyFrom(current_background_color);
  return c;
}

void wxbDC::SetLogicalOrigin(float x, float y)
{
  logical_origin_x = x;
  logical_origin_y = y;
}

void wxbDC::SetDeviceOrigin(float x, float y)
{
  device_origin_x = x;
  device_origin_y = y;
}

void wxbDC::SetLogicalScale(float x, float y)
{
  logical_scale_x = x;
  logical_scale_y = y;
}

void wxbDC::CalcBoundingBox(float x, float y)
{
  if (x < min_x) min_x = x;
  if (y < min_y) min_y = y;
  if (x > max_x) max_x = x;
  if (y > max_y) max_y = y;
}

void wxbDC::TryColour(wxColour *src, wxColour *dest)
{
  dest = src;
}

wxbMemoryDC::wxbMemoryDC(void) { }
wxbMemoryDC::wxbMemoryDC(wxCanvasDC *WXUNUSED(old_dc)) { }
wxbMemoryDC::~wxbMemoryDC(void) { }

wxbCanvasDC::wxbCanvasDC(void) { }
wxbCanvasDC::~wxbCanvasDC(void) { }

