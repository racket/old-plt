/*								-*- C++ -*-
 *
 * Purpose: basic device context
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "DC.h"
#endif

#define  Uses_wxDC
#define  Uses_wxList
#include "wx.h"

#include <math.h>

//-----------------------------------------------------------------------------
// create and destroy wxDC
//-----------------------------------------------------------------------------

wxDC::wxDC(void)
{
    wxColour *c;

    __type = wxTYPE_DC;

    device = wxDEVICE_NONE;

    auto_setting = optimize = ok = Colour = FALSE;

    mm_to_pix_x = mm_to_pix_y = 1.0; // to be safe
    logical_origin_x = logical_origin_y
	= device_origin_x = device_origin_y
	= origin_x = origin_y = 0.0;
    logical_scale_x = logical_scale_y
	= user_scale_x = user_scale_y
	= scale_x = scale_y = 1.0;
    max_x = max_y = -100000.0;
    min_x = min_y =  100000.0;

    c = new wxColour(wxWHITE);
    current_background_color = c;
    current_brush = wxTRANSPARENT_BRUSH;
    current_cmap = wxAPP_COLOURMAP;
    current_font = wxSWISS_FONT;
    current_map_mode = MM_TEXT;
    current_pen = wxBLACK_PEN;
    current_text_alignment = wxALIGN_TOP_LEFT;
    c = new wxColour(wxWHITE);
    current_text_bg = c;
    current_text_bgmode = wxTRANSPARENT;
    c = new wxColour(wxBLACK);
    current_text_fg = c;
}

wxColour *wxDC::GetBackground(void){
  wxColour *c;
  c = new wxColour();
  c->CopyFrom(current_background_color);
  return c;
}

//-----------------------------------------------------------------------------
// Set scale and origin
//-----------------------------------------------------------------------------

void wxDC::ComputeScaleAndOrigin(void)
{
    scale_x =  logical_scale_x * user_scale_x;
    scale_y =  logical_scale_y * user_scale_y;
    origin_x = logical_origin_x - (device_origin_x / scale_x);
    origin_y = logical_origin_y - (device_origin_y / scale_y);
}

void wxDC::SetDeviceOrigin(float x, float y)
{
    device_origin_x = x;
    device_origin_y = y;
    ComputeScaleAndOrigin();
}

void wxDC::SetLogicalOrigin(float x, float y)
{
    logical_origin_x = x;
    logical_origin_y = y;
    ComputeScaleAndOrigin();
}

void wxDC::SetLogicalScale(float xs, float ys)
{
    logical_scale_x = xs;
    logical_scale_y = ys;
    ComputeScaleAndOrigin();
}

void wxDC::SetMapMode(int mode)
{
    switch (mode) {
    case MM_TWIPS:
	SetLogicalScale(twips2mm*mm_to_pix_x, twips2mm*mm_to_pix_y);
	break;
    case MM_POINTS:
	SetLogicalScale(pt2mm*mm_to_pix_x, pt2mm*mm_to_pix_y);
	break;
    case MM_METRIC:
	SetLogicalScale(mm_to_pix_x, mm_to_pix_y);
	break;
    case MM_LOMETRIC:
	SetLogicalScale(mm_to_pix_x/10.0, mm_to_pix_y/10.0);
	break;
    default:
    case MM_TEXT:
	SetLogicalScale(1.0, 1.0);
	break;
    }
}

void wxDC::SetUserScale(float xs, float ys)
{
    user_scale_x = xs;
    user_scale_y = ys;
    ComputeScaleAndOrigin();
    SetFont(current_font);
    SetPen(current_pen);
}

//-----------------------------------------------------------------------------
// Bounding Box
//-----------------------------------------------------------------------------

void wxDC::CalcBoundingBox(float x, float y)
{
    if (x < min_x) min_x = x;
    if (y < min_y) min_y = y;
    if (x > max_x) max_x = x;
    if (y > max_y) max_y = y;
}

//-----------------------------------------------------------------------------
// spline code, uses protected virtual method DrawOpenSpline, from XFIG
//-----------------------------------------------------------------------------

/*
 * FIG : Facility for Interactive Generation of figures
 * Copyright (c) 1985 by Supoj Sutanthavibul
 */

void wxDC::DrawSpline(int n, wxPoint pts[])
{
    wxList *list;
    list = new wxList;
    for (int i=0; i<n; ++i) {
      list->Append((wxObject*)&pts[i]);
    }
    DrawSpline(list);
    DELETE_OBJ list;
}

void wxDC::DrawSpline(wxList *pts)
{
    DrawOpenSpline(pts);
}

void wxDC::DrawSpline(float x1, float y1, float x2, float y2,
		      float x3,float y3)
{
    wxList *list;
    wxPoint *point1, *point2, *point3;

    list = new wxList;

    point1 = new wxPoint;
    point1->x = x1; point1->y = y1;
    list->Append((wxObject*)point1);

    point2 = new wxPoint;
    point2->x = x2; point2->y = y2;
    list->Append((wxObject*)point2);

    point3 = new wxPoint;
    point3->x = x3; point3->y = y3;
    list->Append((wxObject*)point3);

    DrawSpline(list);

    DELETE_OBJ list;
}

//-----------------------------------------------------------------------------
// wxDC::DrawOpenSpline(wxList *pts), may be virtually overridden by any child
//-----------------------------------------------------------------------------

// defines and static declarations for DrawOpenSpline

#define half(z1,z2)	(float)((z1+z2)/2.0)
#define wx_round(a)	(float)((int)(a+0.5))

static void wx_quadratic_spline(float a1, float b1, float a2, float b2,
				float a3, float b3, float a4, float b4);
static void wx_clear_stack(void);
static int  wx_spline_pop(float *x1, float *y1, float *x2, float *y2,
			  float *x3, float *y3, float *x4, float *y4);
static void wx_spline_push(float x1, float y1, float x2, float y2,
			   float x3, float y3, float x4, float y4);
static Bool wx_spline_add_point(float x, float y);
static void wx_spline_draw_point_array(wxDC *dc);

static wxList *wx_spline_point_list;

void wxDC::DrawOpenSpline(wxList *pts)
{
    wxPoint *p;
    float  cx1, cy1, cx2, cy2, cx3, cy3, cx4, cy4;
    float  x1,  y1,  x2 , y2;
    wxNode *node;

    node = pts->First();
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
    wx_spline_add_point(wx_round(cx1), wx_round(cy1));
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
            wx_spline_add_point(wx_round(x1), wx_round(y1));
            wx_spline_add_point(wx_round(xmid), wx_round(ymid));
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
    wxPoint *point;

    if (!wx_spline_point_list) {
      wxREGGLOB(wx_spline_point_list);
      wx_spline_point_list = new wxList;
    }

    point = new wxPoint;
    point->x = x;
    point->y = y;
    wx_spline_point_list->Append((wxObject*)point);
    return TRUE;
}

static void wx_spline_draw_point_array(wxDC *dc)
{
  if (wx_spline_point_list) {
    wxNode *node;
    dc->DrawLines(wx_spline_point_list, 0.0, 0.0);
    node = wx_spline_point_list->First();
    while (node) {
	wxPoint *point;
	point = (wxPoint *)node->Data();
	DELETE_OBJ point;
	wx_spline_point_list->DeleteNode(node);
	node = wx_spline_point_list->First();
    }
  }
}
