/*
 * File:		wxgraph.cc
 * Purpose:	Graph library for wxWindows
 */

#ifdef __GNUG__
#pragma implementation "wxgraph.h"
#endif

#ifndef wx_xt
    // wxWindows standard include mechanism
    // For compilers that support precompilation, includes "wx.h".
#   include "wx_prec.h"
#   ifdef __BORLANDC__
#	pragma hdrstop
#   endif
#   ifndef WX_PRECOMP
#	include "wx.h"
#   endif
#   include <stdio.h>
#   include <stdlib.h>
#   include <wx_hash.h>
#   include "wxgraph.h"
#   include "wxgraphp.h"
#else // wx_xt
    // The Xt port uses another include mechanism
#   define  Uses_wxGraphLayout
#   define  Uses_wxHashTable
#   include "wx.h"
#   include "DrawingClasses/wxgraphp.h"
#endif // #ifndef wx_xt

#include <stdarg.h>
#include <math.h>

#define Exch( x1, x2)	{float h=x1; x1=x2; x2=h;}

/*
 * Abstract graph
 */

IMPLEMENT_ABSTRACT_CLASS(wxGraphLayout, wxObject)

wxGraphLayout::wxGraphLayout(wxDC *dc)
{
  // Private variables
  firstnode = NULL;
  vplace = NULL;
  hplace = NULL;
  n_nodes = 0;
  max_x = 0;
  max_y = 0;
  debuglevel = 0;
  rotation = 0;
  flexscale = FALSE;

  nodeHashTable = NULL;
  arcHashTable = NULL;

  Clear();

  nodeHashTable = new wxHashTable(wxKEY_INTEGER);
  arcHashTable = new wxHashTable(wxKEY_INTEGER);
  bbox[0] = 0.0;
  bbox[1] = 0.0;
  bbox[2] = 400.0;
  bbox[3] = 400.0;

  curr_box[0] = 0.0;
  curr_box[1] = 0.0;
  curr_box[2] = 400.0;
  curr_box[3] = 400.0;

  // Protected variables
  xSpacing = 16;
  ySpacing = 20;
  topMargin = 5;
  leftMargin = 5;
  graphDC = dc;
}

wxGraphLayout::~wxGraphLayout(void)
{
  Clear();
  delete nodeHashTable;
  delete arcHashTable;
}


void wxGraphLayout::AddNode(long id, char *name)
{
  _AddNode(id, name);
}

wxGraphNode *wxGraphLayout::_AddNode(long id, char *name)
{
  wxGraphNode *node;
  char nameBuf[20];
  if (!name)
  {
    sprintf(nameBuf, "%ld", id);
    name = nameBuf;
  }

  node = (wxGraphNode *)nodeHashTable->Get(id);
        
        if (!node)
	{	node = new wxGraphNode;
		node->name = copystring(name);
		node->id = id;
                nodeHashTable->Put(id, (wxObject *)node);
		link_node( &firstnode, node);
		n_nodes++;
	}
	return node;
}

void wxGraphLayout::AddArc(long id, long fromId, long toId,
			   char *WXUNUSED(name), Bool hide)
{
  wxGraphEdge *edge;
  wxGraphNode *fromnode, *tonode;
	
  fromnode = _AddNode(fromId);
  tonode = _AddNode(toId);

  edge = new wxGraphEdge;
  edge->id = id;
  arcHashTable->Put(id, (wxObject *)edge);

  if (fromnode == tonode || hide)
  {/* self-edges are not really added to the data structure */
    /* since the disturb current algorithmes, and do not */
    /* contribute to placement anyway */
    /* trick here to allow just later printing: */
    edge->to = tonode;
    edge->from = fromnode;
  }
  else
    link_edge( edge, fromnode, tonode);

  if (debuglevel >= 4)
    verbose( "read_edge: edge from %s to %s has adress %ld\n",
            fromnode->name, tonode->name, (long)(edge));
}

void wxGraphLayout::DoLayout(void)
{
  place_graph();
  set_scale();
  
  nodeHashTable->BeginFind();
  wxNode *wxnode = nodeHashTable->Next();
  while (wxnode)
  {
    wxGraphNode *node = (wxGraphNode *)wxnode->Data();
    do_scale(&(node->x), &(node->y));
    
    SetNodeX(node->id, node->x);
    SetNodeY(node->id, node->y);
    wxnode = nodeHashTable->Next();
  }
}

void wxGraphLayout::Clear(void)
{
  // Delete structures
  if (nodeHashTable)
  {
    nodeHashTable->BeginFind();
    wxNode *wxnode = nodeHashTable->Next();
    while (wxnode)
    {
      wxGraphNode *node = (wxGraphNode *)wxnode->Data();
      delete node;
      wxnode = nodeHashTable->Next();
    }
    nodeHashTable->Clear();
  }

  if (arcHashTable)
  {
    arcHashTable->BeginFind();
    wxNode *wxnode = arcHashTable->Next();
    while (wxnode)
    {
      wxGraphEdge *edge = (wxGraphEdge *)wxnode->Data();
      delete edge;
      wxnode = arcHashTable->Next();
    }
    arcHashTable->Clear();
  }

  firstnode = NULL;

  if (vplace) delete[] vplace;
  if (hplace) delete[] hplace;
  vplace = NULL;
  hplace = NULL;
  n_nodes = 0;
  max_x = 0;
  max_y = 0;

  bbox[0] = 0.0;
  bbox[1] = 0.0;
  bbox[2] = 400.0;
  bbox[3] = 400.0;

  for (int i = 0; i < 3; i++)
    for (int j = 0; j < 2; j++)
      scale[j][i] = 0.0;
}

void wxGraphLayout::Draw()
{
  if (graphDC)
    graphDC->Clear();
  DrawArcs();
  DrawNodes();
}

void wxGraphLayout::DrawNodes(void)
{
  nodeHashTable->BeginFind();
  wxNode *wxnode = nodeHashTable->Next();
  while (wxnode)
  {
    wxGraphNode *node = (wxGraphNode *)wxnode->Data();
    DrawNode(node->id);
    wxnode = nodeHashTable->Next();
  }
}

void wxGraphLayout::DrawArcs(void)
{
  arcHashTable->BeginFind();
  wxNode *wxnode = arcHashTable->Next();
  while (wxnode)
  {
    wxGraphEdge *edge = (wxGraphEdge *)wxnode->Data();
    DrawArc(edge->from->id, edge->to->id);
    wxnode = arcHashTable->Next();
  }
}

void wxGraphLayout::DrawNode(long id)
{
  char buf[80];
  char *name = GetNodeName(id);
  if (name)
    sprintf(buf, "%s", name);
  else
    sprintf(buf, "<unnamed>");

  float x = 80;
  float y = 20;
  if (graphDC)
  {
    graphDC->GetTextExtent(buf, &x, &y);
    float x1 = GetNodeX(id);
    float y1 = GetNodeY(id);
    float margin = 3;
    graphDC->SetBrush(wxCYAN_BRUSH);
    graphDC->SetPen(wxBLACK_PEN);
    graphDC->SetTextBackground(wxCYAN);
    graphDC->DrawRoundedRectangle((float)(x1 - margin), (float)(y1 - (y/2.0) - margin),
                                  (float)(x+2*margin), (float)(y+2*margin), -0.3);
    graphDC->DrawText(buf, x1, (float)(y1 - (y/2.0)));
  }
}

void wxGraphLayout::DrawArc(long from, long to)
{
  if (graphDC)
    graphDC->DrawLine(GetNodeX(from), GetNodeY(from),
                              GetNodeX(to), GetNodeY(to));
}

void wxGraphLayout::Initialize(void)
{
}

void wxGraphLayout::GetNodeSize(long id, float *x, float *y)
{
  char *name = GetNodeName(id);
  if (name && graphDC)
    graphDC->GetTextExtent(name, x, y);
  else
  {
    *x = 70; *y = 20;
  }
}

void wxGraphLayout::SetBoundingBox(float x1, float y1, float x2, float y2)
{
  bbox[0] = x1;
  bbox[1] = y1;
  bbox[2] = x2;
  bbox[3] = y2;
}

float wxGraphLayout::GetNodeX(long id)
{
  wxGraphNode *node = (wxGraphNode *)nodeHashTable->Get(id);
  if (node)
    return node->x;
  else return (float)0.0;
}

float wxGraphLayout::GetNodeY(long id)
{
  wxGraphNode *node = (wxGraphNode *)nodeHashTable->Get(id);
  if (node)
    return node->y;
  else return (float)0.0;
}

void wxGraphLayout::SetNodeX(long WXUNUSED(id), float WXUNUSED(x))
{
}

void wxGraphLayout::SetNodeY(long WXUNUSED(id), float WXUNUSED(y))
{
}

void wxGraphLayout::SetNodeName(long WXUNUSED(id), char *WXUNUSED(name))
{
}

char *wxGraphLayout::GetNodeName(long id)
{
  wxGraphNode *node = (wxGraphNode *)nodeHashTable->Get(id);
  if (node) return node->name;
  else return NULL;
}

void wxGraphLayout::ActivateNode(long WXUNUSED(id), Bool WXUNUSED(active))
{
}

Bool wxGraphLayout::NodeActive(long WXUNUSED(id))
{
  return TRUE;
}

void wxGraphLayout::place_graph()
{
  wxGraphNode *startlist = NULL;
  int leftbound = 0;
  int overall_max_y = 0;
  set_node_incount(firstnode);

  while (firstnode) /* still unplaced nodes left */
  {  /* maybe more then one cycle needed when */
     /* there are disconnected cyclic subgraphs */

    if (debuglevel >= 2)
      verbose( "Start placement: leftbound = %d\n", leftbound);

    select_start_nodes( firstnode, &startlist);
    vertical_place( &startlist);

    if (max_y > overall_max_y) overall_max_y = max_y;
      /* remove loops from graph, allows easier formulation */
      /* of horizontal placement */
      uncycle_graph();

    vert_compact();
    reduce_density();

    /* place each next graph to the right */
    horizontal_place( leftbound);
    leftbound = DIST + max_x;
  }

  max_y = overall_max_y;
}

/********* local functions ******/

void wxGraphLayout::set_node_incount(wxGraphNode *list)
{
  wxGraphNode *node;
  wxGraphEdge *edge;
  int n;

  for (node = list; node; node = node->next)
  {
    if (node->flag == -1) /* artificial 'super' node */
    n = 9999;
    else
    {
      n = 0;
      for (edge=node->inedges; edge; edge=edge->next_inedge)
        n++;
      if (node->super) /* don't count artificial in-edge */
        n--;
    }
    node->incount = n;
    if (debuglevel >= 4)
      verbose( "node %s incount = %d\n", node->name, n);
  }
}

void wxGraphLayout::select_start_nodes(wxGraphNode *fromlist, wxGraphNode **tolist)
{
  int min_incount, cnt;
  wxGraphNode *node, *nextnode;

  min_incount = fromlist->incount;
  for (node = fromlist; node; node = node->next)
    if (node->incount < min_incount)
      min_incount = node->incount;
	
  if (debuglevel>=4)
    verbose( "select_start_list: take nodes with incount==%d\n", min_incount);

  for (node = fromlist; node; node = nextnode)
  {
    nextnode = node->next;
    cnt = node->super ? node->super->incount : node->incount;
    if (cnt == min_incount)
    { /* add this node */
      unlink_node( node);
      link_node( tolist, node);
      if (debuglevel>=5)
        verbose( "node %s on startlist\n", node->name);
    }
  }
}

void wxGraphLayout::vertical_place(wxGraphNode **startlist)
{
  if (vplace)
  { /* clear (previous) placement */
    int i;
    for (i=0; i<n_nodes; i++)
      vplace[i] = NULL;
  } else
  { /* array to hold list of nodes for each vertical pos */
    vplace = new wxGraphNode *[n_nodes];
      for (int i = 0; i < n_nodes; i++)
        vplace[i] = NULL;
  }

  max_y = 0;
  while (*startlist)
    dfs_vplace( *startlist, 0);
}

void wxGraphLayout::dfs_vplace(wxGraphNode *node, int y)
{	/* node->flag==0: untouched, ==1: now on path, ==2: placed */
	wxGraphEdge *edge, *se;
	wxGraphNode *n;

	
	if (node->flag==1 || node->super && node->super->flag==1)
	{	if (debuglevel>=5)
			verbose( "dfs_vplace( node %s, y=%d): on path!\n",
				node->name, y);
		return;  /* cycle detected */
	}

	if (node->flag == 2 && node->y >= y)
	{	if (debuglevel>=5)
			verbose( "dfs_vplace( node %s, y=%d): is on %g\n",
				node->name, y, node->y);
		return;  /* is already correctly placed */
	}

	/* now perform placement */
	if (y > max_y) max_y = y;

	if (debuglevel>=4)
		verbose( "dfs_vplace:( node %s move to y=%d)\n",
			node->name, y);


	if (node->super)
	{	if (node->super->y != y)
		{	node->super->y = y;
			unlink_node( node->super);
			link_node( &vplace[y], node->super);
		}
		if (debuglevel>=5)
			verbose( "dfs_vplace: move super-node '%s'\n",
				node->super->name);
		node->super->flag = 1;
		for (se=node->super->outedges; se; se=se->next_outedge)
		{	n = se->to;
			n->y = y;
			n->flag = 1;
			if (debuglevel>=5)
				verbose( "dfs_vplace: '%s' glues "
					 "'%s' to y=%d\n",
					 node->name, n->name, y);
			unlink_node( n); /* remove node from its list */
			link_node( &vplace[y], n);
			for (edge=n->outedges; edge; edge=edge->next_outedge)
				dfs_vplace( edge->to, y+1);
			n->flag = 2;
		}
		node->super->flag = -1;
	} else
	{	node->y = (float)y;
		node->flag = 1;
		unlink_node( node); /* remove node from its list */
		link_node( &vplace[y], node);
		for (edge = node->outedges; edge; edge = edge->next_outedge)
			dfs_vplace( edge->to, y+1);
		node->flag = 2;
	}
}

void wxGraphLayout::vert_compact()
{	/* move nodes down to compact the graph */
	int y, high;
	wxGraphNode *n, *next;
	wxGraphEdge *e;

	for (y=max_y-1; y>=0; y--)
	for (n=vplace[y]; n; n=next)
	{	next = n->next;
		if (n->super || !n->outedges) continue;

		high = max_y;
		for( e=n->outedges; e; e = e->next_outedge)
			if (e->to->y < high) high = (int)e->to->y;

		high--;
		if (high > y) /* can move node down */
		{	n->y = (float)high;
			unlink_node( n); /* remove node from its list */
			link_node( &vplace[high], n);
		}
	}
}

void wxGraphLayout::reduce_density()
{	/* move nodes up if this reduces the total wire length */
	/* i.e. the node has more up- than down-edges */
	int y, low, n_down, n_up;
	wxGraphNode *n, *next;
	wxGraphEdge *e;

	for (y=1; y<max_y; y++)
	for (n=vplace[y]; n; n=next)
	{	next = n->next;
		if (n->super || !n->inedges) continue;

		low = 0;
		n_down = n_up = 0;
		for( e=n->outedges; e; e = e->next_outedge)
			n_down++;
		for( e=n->inedges; e; e = e->next_inedge)
		{	if (e->from->y > low) low = (int)e->from->y;
			n_up++;
		}

		low++;
		if (low < y && n_down <= n_up) /* can move node up */
		{	n->y = (float)low;
			unlink_node( n); /* remove node from its list */
			link_node( &vplace[low], n);
		}
	}
}

void wxGraphLayout::uncycle_graph()
{	/* reverse all edges which point upwards (from high to low y) */
	int y;
	wxGraphNode *from, *to, *next;
	wxGraphEdge *edge, *nextedge;

	for (y=0; y<=max_y; y++)
	for (from=vplace[y]; from; from=next)
	{	next = from->next;
		for( edge=from->outedges; edge; edge = nextedge)
		{	nextedge = edge->next_outedge;
			to = edge->to;
			if (from->flag == -1)
			{	/* remove edges from 'super'nodes */
				unlink_edge( edge);
			} else if (to->y < y)
			{	/* reverse edge */
				if (debuglevel >= 3)
					verbose( "reverse edge from "
					 	"%s to %s\n",
					 	from->name, to->name);
				unlink_edge( edge);
				link_edge( edge, to, from);
				edge->reversed = TRUE;
			}
		}
		if (from->flag == -1) unlink_node( from);
	}
}

void wxGraphLayout::horizontal_place( int leftbound)
{
	horizontal_assign( 0, max_y+1, 1, 0);
	horizontal_assign( max_y-1 , -1, 1, 0);
	horizontal_assign( 1, max_y+1, 2, 1);
	horizontal_assign( max_y-1 , -1, 2, 2);
	/*
	horizontal_assign( max_y/2 + 1, max_y+1, 1, 0);
	horizontal_assign( (max_y*2+1)/3, -1, 2, 1);
	horizontal_assign( (max_y+1)/3, max_y+1, 2, 1);
	*/

	horizontal_shift( leftbound);
}

void wxGraphLayout::horizontal_assign( int startrow, int endrow,
		int forwardweight, int backweight)
{
	wxGraphNode *node;
	int i;
	int inc = (startrow<=endrow) ? 1 : -1;

	if (debuglevel>=3)
		verbose( "horizontal_assign: rows %d to %d, weight %d and %d\n",
			startrow, endrow, forwardweight, backweight);

	for (i=startrow; i!=endrow; i+=inc)
	{	for (node=vplace[i]; node; node=node->next)
			if (inc > 0)
				hor_move_node( node, forwardweight, backweight);
			else	hor_move_node( node, backweight, forwardweight);
		hor_distrib_row( vplace[i]);
	}
}

void wxGraphLayout::hor_move_node(wxGraphNode *node, int inweight, int outweight)
{
	wxGraphEdge *edge;
	int no = 0;
	int sum = 0;
	int w;

	if (inweight)
	for (edge=node->inedges; edge; edge=edge->next_inedge)
	{	w = inweight;
		if (inweight > 1 && edge->to->y - edge->from->y > 1)
			w--; /* decrease weight of long edges */
		no += w;
		sum += (int)(w * edge->from->x);
		if (debuglevel >= 8)
			verbose( "   due to node %s: no=%d, sum=%d\n",
				edge->from->name, no, sum);
	}

	if (outweight)
	for (edge=node->outedges; edge; edge=edge->next_outedge)
	{	w = outweight;
		if (outweight > 1 && edge->to->y - edge->from->y > 1)
			w--; /* decrease weight of long edges */
		no += w;
		sum += (int)(w * edge->to->x);
		if (debuglevel >= 8)
			verbose( "   due to node %s: no=%d, sum=%d\n",
				edge->to->name, no, sum);
	}

	sum += (sum>0) ? no/2 : -no/2; /* forces nice rounding */

	if (no>0) node->x = (float)(sum/no);

	if (debuglevel >= 4)
		verbose( "node %s moved to x=%g\n", node->name, node->x);
}

void wxGraphLayout::hor_distrib_row(wxGraphNode *row)
{
	wxGraphNode *node;
	int i, w, c, lastcol, odd, midx;

	if (!row) return;
	if (!hplace)
        {
          hplace = new wxGraphNode *[n_nodes];
          for (i = 0; i < n_nodes; i++)
            hplace[i] = NULL;
        }

	/* copy row-list into array */
	for( w=0, node=row; node; node=node->next, w++)
		hplace[w] = node;

	if (w<=1) return;

	/* sort array */
	qsort( (void *)hplace, w, sizeof(wxGraphNode *), 
		(int (*)(const void *, const void *))wxGraphColumnCompare);

	/* reassign column numbers to legal values */
	odd = w&1;
	midx = (int)(odd ? hplace[(w-1)/2]->x
		   : (hplace[(w-2)/2]->x + hplace[w/2]->x)/2);
	/* 1. from center to right */
	lastcol = midx - (odd ? 0 : DIST/2);
	for (i=(w+1)/2; i<w; i++)
	{	lastcol += DIST;
		c = (int)hplace[i]->x;
		if (c < lastcol) c = lastcol;
		else	lastcol = c;
		hplace[i]->x = (float)c;
	}
	/* 2. from center to left */
	lastcol = midx + (odd ? 0 : DIST/2);
	for (i=(w-2)/2; i>=0; i--)
	{	lastcol -= DIST;
		c = (int)hplace[i]->x;
		if (c > lastcol) c = lastcol;
		else	lastcol = c;
		hplace[i]->x = (float)c;
	}

	if (debuglevel >= 4)
	{	verbose( "Distrib_row: nodes moved to");
		for (i=0; i<w; i++)
			verbose( " %s=%g", hplace[i]->name, hplace[i]->x);
		verbose( "\n");
	}
}

int wxGraphColumnCompare(wxGraphNode **n1, wxGraphNode **n2)
{
	int diff = (int)((*n1)->x - (*n2)->x);

	if (diff < 0) return( -1);
	else if (diff > 0) return( 1);
	else return( 0);
}

void wxGraphLayout::horizontal_shift( int leftbound)
{
	int min, max, x, y, shift;
	wxGraphNode *node;

	min = max = (int)(vplace[0]->x);

	for (y=0; y<=max_y; y++)
	for (node=vplace[y]; node; node=node->next)
	{	x = (int)node->x;
		if (x < min) min = x;
		else if (x > max) max = x;
	}

	shift = leftbound - min;

	for (y=0; y<=max_y; y++)
	for (node=vplace[y]; node; node=node->next)
		node->x += shift;
	
	max_x = max + shift;
}

void wxGraphLayout::verbose( char *fmt, ...)
{
	va_list args;

	va_start( args, fmt);
	vfprintf( stderr, fmt, args);
	va_end( args);
}

/*
 * scale.c
 *
 */

void wxGraphLayout::set_scale()
{
	/* graph is originally placed with */
	/* 0 <= x <= max_x, right positive, unit distance 'DIST' */
	/* 0 <= y <= max_y, down positive, unit distance 1. */
	/* now setup a transformation matrix to scale this to */
	/* the requested bounding box bbox[4] = {xll, yll, xur, yur} */

	scale[0][0] = 1.0;
	scale[1][1] = 1.0;
	curr_box[2] = max_x;
	curr_box[3] = max_y;

	/* Turn original y-placement to unit distances in up+ coords */
//	mult_y( -DIST);

	/* perform requested rotation */
	if (rotation) rotate( rotation);
/*
	if (!boxarg && !postscript)
	{	// no bbox known, keep default scaling, move to 0,0 as ll
		translate( -curr_box[0], -curr_box[1]);
	} else
*/
	{	/* choose scaling factor, is always positive ! */
		float mx, my, mm;

		/* prevent division by 0 */
		if (0.0 == curr_box[2] - curr_box[0])
		{	curr_box[0] -= (float)(0.5*DIST);
			curr_box[2] += (float)(0.5*DIST);
		}
		if (0.0 == curr_box[3] - curr_box[1])
		{	curr_box[1] -= (float)(0.5*DIST);
			curr_box[3] += (float)(0.5*DIST);
		}

		mx = (bbox[2] - bbox[0])/(curr_box[2] - curr_box[0]);
		my = (bbox[3] - bbox[1])/(curr_box[3] - curr_box[1]);

//		mm = (float)(margin/100.0);
                mm = 0.0;

		mx *= (float)(1.0 - 2.0*mm);
		my *= (float)(1.0 - 2.0*mm);

		if (!flexscale)
		{	if (mx > my) mx = my;
			else	my = mx;
		}
	
		/* now transform to bbox */
		/* sizing */
		mult_x( mx);
		mult_y( my);

		/* move to lower-left = 0,0 */
//		translate( bbox[0] - curr_box[0], bbox[1] - curr_box[1]);

		/* move to requested margin */
//		translate( mm*(bbox[2]-bbox[0]), mm*(bbox[3]-bbox[1]));
                translate(bbox[0], bbox[1]);
	}

	if (debuglevel >= 2)
	{	verbose( "Transformation matrix is:  %g %g %g\n",
			scale[0][0], scale[0][1], scale[0][2]);
		verbose( "                           %g %g %g\n",
			scale[1][0], scale[1][1], scale[1][2]);
		verbose( "Graph bbox after scaling is %g %g %g %g\n",
			curr_box[0], curr_box[1], curr_box[2], curr_box[3]);
	}
}

void wxGraphLayout::do_scale(float *x, float *y)
{
  float xin = *x;
  float yin = *y;

  *x = (float)(scale[0][0] * xin + scale[0][1] * yin + scale[0][2]);
  *y = (float)(scale[1][0] * xin + scale[1][1] * yin + scale[1][2]);
}

void wxGraphLayout::mult_y( float f)
{
	int i;

	for (i=0; i<3; i++)
		scale[1][i] *= f;

	curr_box[1] *= f;
	curr_box[3] *= f;
	if (f < 0.0) Exch( curr_box[1], curr_box[3]);
}

void wxGraphLayout::mult_x( float f)
{
	int i;
	for (i=0; i<3; i++)
		scale[0][i] *= f;

	curr_box[0] *= f;
	curr_box[2] *= f;
	if (f < 0.0) Exch( curr_box[0], curr_box[2]);
}

void wxGraphLayout::translate( float dx, float dy)
{
	scale[0][2] += dx;
	scale[1][2] += dy;

	curr_box[0] += dx;
	curr_box[2] += dx;
	curr_box[1] += dy;
	curr_box[3] += dy;
}

void wxGraphLayout::rotate( int n)
{	/* rotate clockwise over n*90 degrees */
	int j;
	float h;

	/* transform n to be within 0 <= n <= 3 */
	if (n<0)
	{	mult_y( -1);
		n = -n;
	}
	n = n & 3;

	while (n--)
	{	/* rotate over 90 degrees */
		for (j=0; j<3; j++)
		{	Exch( scale[0][j], scale[1][j]);
			scale[0][j] *= -1;
		}
		
		h = curr_box[0];
		curr_box[0] = - curr_box[3];
		curr_box[3] = curr_box[2];
		curr_box[2] = - curr_box[1];
		curr_box[1] = h;
	}
}

void wxGraphLayout::link_node(wxGraphNode **listp, wxGraphNode *node)
{
 (node)->next = *(listp);
 if (*(listp))
  (*(listp))->prevp = &((node)->next);
 *(listp) = node;
 (node)->prevp = listp;
}

void wxGraphLayout::unlink_node(wxGraphNode *node)
{
  if ((node)->next)
    (node)->next->prevp = (node)->prevp;
  if ((node)->prevp)
   *((node)->prevp) = (node)->next;
  (node)->next = NULL;
  (node)->prevp = NULL;
}

void wxGraphLayout::link_edge(wxGraphEdge *edge, wxGraphNode *fromnode, wxGraphNode *tonode)
{
  edge->from = fromnode;
  edge->to = tonode;
  edge->next_outedge = (fromnode)->outedges;
  (fromnode)->outedges = edge;
  edge->next_inedge = (tonode)->inedges;
  (tonode)->inedges = edge;
}

void wxGraphLayout::unlink_edge(wxGraphEdge *edge)
{
  wxGraphEdge **Ep;
  for (Ep=&((edge)->from->outedges); *Ep; Ep=&((*Ep)->next_outedge))
    if (*Ep == edge) break;
  if (*Ep) *Ep = (edge)->next_outedge;
  for (Ep=&((edge)->to->inedges); *Ep; Ep=&((*Ep)->next_inedge))
    if (*Ep == edge) break;
  if (*Ep) *Ep = (edge)->next_inedge;
  (edge)->to = NULL;
  (edge)->from = NULL;
}
