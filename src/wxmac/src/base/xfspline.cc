/*
 * File:	xfspline.cc
 * Purpose:	
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 */

#include "wx.h"

void wxbDC::DrawSpline(int n, wxPoint pts[])
{
  wxList list;
  for (int i=0; i<n; ++i)
    list.Append((wxObject*)&pts[i]);
  DrawSpline(&list);
}

// defines and static declarations for DrawSpline

#define half(z1,z2)	float((z1+z2)/2.0)

static void wx_quadratic_spline(float a1, float b1, float a2, float b2,
				float a3, float b3, float a4, float b4);
static void wx_clear_stack(void);
static int  wx_spline_pop(float *x1, float *y1, float *x2, float *y2,
			  float *x3, float *y3, float *x4, float *y4);
static void wx_spline_push(float x1, float y1, float x2, float y2,
			   float x3, float y3, float x4, float y4);
static Bool wx_spline_add_point(float x, float y);
static void wx_spline_draw_point_array(wxbDC *dc);

static wxList *wx_spline_point_list;

void wxRegisterSplinePointList();

void wxRegisterSplinePointList()
{
  wxREGGLOB(wx_spline_point_list);
  wx_spline_point_list = new wxList;
}

void wxbDC::DrawSpline(wxList *pts)
{
  wxPoint *p;
  float  cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4;
  float  x1,  y1,  x2 , y2;

  wxNode *node = pts->First();
  p = (wxPoint*)node->Data();
  x1 = p->x; y1 = p->y;

  node = node->Next();
  p = (wxPoint *)node->Data();
  x2 = p->x; y2 = p->y;

  cx1 = half(x1, x2);  cy1 = half(y1, y2);
  cx2 = half(cx1, x2); cy2 = half(cy1, y2);

  wx_spline_add_point(x1, y1);

  while ((node=node->Next()) != NULL) {
    p = (wxPoint*)node->Data();
    x1  = x2;	      y1  = y2;
    x2  = p->x;	      y2  = p->y;
    cx4 = half(x1, x2);   cy4 = half(y1, y2);
    cx3 = half(x1, cx4);  cy3 = half(y1, cy4);

    wx_quadratic_spline(cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4);

    cx1 = cx4;	      cy1 = cy4;
    cx2 = half(cx1, x2);  cy2 = half(cy1, y2);
  }
  wx_spline_add_point(cx1, cy1);
  wx_spline_add_point(x2, y2);
  wx_spline_draw_point_array(this);
}

/********************* CURVES FOR SPLINES *****************************

  The following spline drawing routine is from

  "An Algorithm for High-Speed Curve Generation"
  by George Merrill Chaikin,
  Computer Graphics and Image Processing, 3, Academic Press,
  1974, 346-349.

  and

  "On Chaikin's Algorithm" by R. F. Riesenfeld,
  Computer Graphics and Image Processing, 4, Academic Press,
  1975, 304-310.

  ***********************************************************************/

#define THRESHOLD 5

/* iterative version */

static void wx_quadratic_spline(float a1, float b1, float a2, float b2,
				float a3, float b3, float a4, float b4)
{
  register float  xmid, ymid;
  float           x1, y1, x2, y2, x3, y3, x4, y4;
  int             counter = 10000; /* At most this many points */

  wx_clear_stack();
  wx_spline_push(a1, b1, a2, b2, a3, b3, a4, b4);

  while (wx_spline_pop(&x1, &y1, &x2, &y2, &x3, &y3, &x4, &y4)) {
    if (!counter--)
      break;
    xmid = half(x2, x3);
    ymid = half(y2, y3);
    if (fabs(x1 - xmid) < THRESHOLD && fabs(y1 - ymid) < THRESHOLD &&
	fabs(xmid - x4) < THRESHOLD && fabs(ymid - y4) < THRESHOLD) {
      wx_spline_add_point(x1, y1);
      wx_spline_add_point(xmid, ymid);
    } else {
      wx_spline_push(xmid, ymid, half(xmid, x3), half(ymid, y3),
		     half(x3, x4), half(y3, y4), x4, y4);
      wx_spline_push(x1, y1, half(x1, x2), half(y1, y2),
		     half(x2, xmid), half(y2, ymid), xmid, ymid);
    }
  }
}

// utilities used by spline drawing routines

typedef struct wx_spline_stack_struct {
  float  x1, y1, x2, y2, x3, y3, x4, y4;
} Stack;

#define SPLINE_STACK_DEPTH  20
static Stack  wx_spline_stack[SPLINE_STACK_DEPTH];
static Stack  *wx_stack_top;
static int    wx_stack_count;

static void wx_clear_stack(void)
{
  wx_stack_top = wx_spline_stack;
  wx_stack_count = 0;
}

static void wx_spline_push(float x1, float y1, float x2, float y2,
			   float x3, float y3, float x4, float y4)
{
  if (wx_stack_count >= SPLINE_STACK_DEPTH) {
    /* Just drop it. */
    return;
  }

  wx_stack_top->x1 = x1;    wx_stack_top->y1 = y1;
  wx_stack_top->x2 = x2;    wx_stack_top->y2 = y2;
  wx_stack_top->x3 = x3;    wx_stack_top->y3 = y3;
  wx_stack_top->x4 = x4;    wx_stack_top->y4 = y4;
  wx_stack_top++;
  wx_stack_count++;
}

int wx_spline_pop(float *x1, float *y1, float *x2, float *y2,
                  float *x3, float *y3, float *x4, float *y4)
{
  if (wx_stack_count == 0)
    return (0);
  wx_stack_top--;
  wx_stack_count--;
  *x1 = wx_stack_top->x1;    *y1 = wx_stack_top->y1;
  *x2 = wx_stack_top->x2;    *y2 = wx_stack_top->y2;
  *x3 = wx_stack_top->x3;    *y3 = wx_stack_top->y3;
  *x4 = wx_stack_top->x4;    *y4 = wx_stack_top->y4;
  return (1);
}

static Bool wx_spline_add_point(float x, float y)
{
  wxPoint *point = new wxPoint ;
  point->x = x;
  point->y = y;
  wx_spline_point_list->Append((wxObject*)point);
  return TRUE;
}

static void wx_spline_draw_point_array(wxbDC *dc)
{
  dc->DrawLines(wx_spline_point_list, 0.0, 0.0);
  wxNode *node = wx_spline_point_list->First();
  while (node) {
    wx_spline_point_list->DeleteNode(node);
    node = wx_spline_point_list->First();
  }
}
