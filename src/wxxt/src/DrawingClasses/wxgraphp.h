/*								-*- C++ -*-
 * File:		wxgraphp.h
 * Purpose:	Graph library -- private header
 *
 *                       wxWindows 1.50
 * Copyright (c) 1993 Artificial Intelligence Applications Institute,
 *                   The University of Edinburgh
 *
 *                     Author: Julian Smart
 *                        Date: 7-9-93
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice, author statement and this permission
 * notice appear in all copies of this software and related documentation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, EXPRESS,
 * IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL THE ARTIFICIAL INTELLIGENCE APPLICATIONS INSTITUTE OR THE
 * UNIVERSITY OF EDINBURGH BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY OF
 * DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#ifndef wx_graphph
#define wx_graphph

#ifndef wx_xt
    // wxWindows standard include mechanism
#   include <wx_hash.h>
#endif

/****************** typedefs **************************************/

class wxGraphEdge;
class wxGraphNode
{
 public:
  float x;
  float y;
  int flag;
  int incount;
  char *name;
  long id; // JACS
  wxGraphNode *next;
  wxGraphNode **prevp; /* doublelinked list by pointing to last 'next'*/
  wxGraphNode *super;  /* to represent a nodeset with same y */
  wxGraphEdge *inedges;
  wxGraphEdge *outedges;
  wxGraphNode(void)
  {
    x = 0; y = 0; flag = 0; incount = 0; name = NULL; id = 0; next = NULL;
    prevp = NULL; super = NULL; inedges = NULL; outedges = NULL;
  }
  ~wxGraphNode(void)
  { if (name) delete[] name; }
};

class wxGraphEdge
{
 public:
  long id; // JACS
  wxGraphNode *from;
  wxGraphNode *to;
  wxGraphEdge *next_inedge;
  wxGraphEdge *next_outedge;
  Bool reversed;
  wxGraphEdge(void)
  {
    id = 0; from = NULL; to = NULL; next_inedge = NULL; next_outedge = NULL;
    reversed = FALSE;
  }
};

#define DIST 4		/* node distances in horizontal placement */
int wxGraphColumnCompare(wxGraphNode **n1, wxGraphNode **n2);

#endif // wx_graphph
