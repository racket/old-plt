/*								-*- C++ -*-
 * $Id: Layout.cc,v 1.4 1999/11/22 20:29:35 mflatt Exp $
 *
 * Purpose: layout classes
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
#pragma implementation "Layout.h"
#endif

#define  Uses_wxLayout
#define  Uses_wxWindow
#define  Uses_wxPanel
#define  Uses_wxFrame
#define  Uses_wxTypeTree
#include "wx.h"

//-----------------------------------------------------------------------------
// wxLayoutConstraint
//-----------------------------------------------------------------------------

wxLayoutConstraints::wxLayoutConstraints(void)
: wxObject(FALSE)
{
    __type = wxTYPE_CONSTRAINTS;
  
    left.myEdge    = wxLeft;
    top.myEdge     = wxTop;
    right.myEdge   = wxRight;
    bottom.myEdge  = wxBottom;
    centreX.myEdge = wxCentreX;
    centreY.myEdge = wxCentreY;
    width.myEdge   = wxWidth;
    height.myEdge  = wxHeight;
}

Bool wxLayoutConstraints::SatisfyConstraints(wxWindow *child)
{
    Bool changes = FALSE;

    if (!width.done)
	changes |= width.SatisfyConstraint(this, child);
    if (!height.done)
	changes |= height.SatisfyConstraint(this, child);
    if (!left.done)
	changes |= left.SatisfyConstraint(this, child);
    if (!top.done)
	changes |= top.SatisfyConstraint(this, child);
    if (!right.done)
	changes |= right.SatisfyConstraint(this, child);
    if (!bottom.done)
	changes |= bottom.SatisfyConstraint(this, child);
    if (!centreX.done)
	changes |= centreX.SatisfyConstraint(this, child);
    if (!centreY.done)
	changes |= centreY.SatisfyConstraint(this, child);

    return changes;
}

void wxLayoutConstraints::UnDone(void)
{
    left.done    = FALSE;
    top.done     = FALSE;
    right.done   = FALSE;
    bottom.done  = FALSE;
    centreX.done = FALSE;
    centreY.done = FALSE;
    width.done   = FALSE;
    height.done  = FALSE;
}

//-----------------------------------------------------------------------------
// wxIndividualLayoutConstraint
//-----------------------------------------------------------------------------

wxIndividualLayoutConstraint::wxIndividualLayoutConstraint(void)
  : wxObject(FALSE)
{
    otherWin	 = NULL;
    otherEdge	 = wxTop;
    myEdge	 = wxTop;
    relationship = wxUnconstrained;
    margin = value = percent = 0;
    done	 = FALSE;

    WXGC_IGNORE(otherWin);
}

void wxIndividualLayoutConstraint::Set(wxRelationship rel, wxWindow *otherW,
				       wxEdge otherE, int val, int marg)
{
    relationship = rel;
    otherWin     = otherW;
    otherEdge    = otherE;
    value        = val;
    margin       = marg;
}

void wxIndividualLayoutConstraint::PercentOf(wxWindow *otherW, wxEdge wh, int per)
{
    otherWin = otherW;
    relationship = wxPercentOf;
    percent = per;
    otherEdge = wh;
}

Bool wxIndividualLayoutConstraint::ResetIfWin(wxWindow *otherW)
{
    if (otherW == otherWin) {
	otherWin     = NULL;
	otherEdge    = wxTop;
	myEdge	     = wxTop;
	relationship = wxUnconstrained;
	margin = value = percent = 0;
	done	     = FALSE;
	return TRUE;
    }
    return FALSE;
}

//-----------------------------------------------------------------------------
// wxIndividualLayoutConstraint: Try to satisfy constraint
//-----------------------------------------------------------------------------

Bool wxIndividualLayoutConstraint::SatisfyConstraint(wxLayoutConstraints *constraints,
						     wxWindow *win)
{
    int edge_pos;

    if (relationship == wxAbsolute || done == TRUE) {
	return (done = TRUE);
    }

    edge_pos = (win && otherWin) ? GetEdge(otherEdge, win, otherWin) : -1;
    switch (myEdge) {
    case wxLeft:
	switch (relationship) {
        case wxLeftOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxRightOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (constraints->right.done && constraints->width.done) {
		// compute using right edge and width
		value = constraints->right.value - constraints->width.value + margin;
		done = TRUE;
	    } else if (constraints->centreX.done && constraints->width.done) {
		// compute using centreX and width
		value = int(constraints->centreX.value - (constraints->width.value/2)
			    + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxRight:
	switch (relationship) {
        case wxLeftOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxRightOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) - margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (constraints->left.done && constraints->width.done) {
		// compute using left edge and width
		value = constraints->left.value + constraints->width.value - margin;
		done = TRUE;
	    } else if (constraints->centreX.done && constraints->width.done) {
		// compute using centreX and width
		value = int(constraints->centreX.value + (constraints->width.value/2)
			    - margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxTop:
	switch (relationship) {
        case wxAbove:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxBelow:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (constraints->bottom.done && constraints->height.done) {
		// compute using bottom edge and height
		value = constraints->bottom.value - constraints->height.value + margin;
		done = TRUE;
	    } else if (constraints->centreY.done && constraints->height.done) {
		// compute using centreY and height
		value = int(constraints->centreY.value - (constraints->height.value/2)
			    + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxBottom:
	switch (relationship) {
        case wxAbove:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxBelow:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) - margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (constraints->top.done && constraints->height.done) {
		// compute using top edge and height
		value = constraints->top.value + constraints->height.value - margin;
		done = TRUE;
	    } else if (constraints->centreY.done && constraints->height.done) {
		// compute using centreY and height
		value = int(constraints->centreY.value + (constraints->height.value/2)
			    - margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxCentreX:
	switch (relationship) {
        case wxLeftOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxRightOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (constraints->left.done && constraints->width.done) {
		// compute using left edge and width
		value = int(constraints->left.value + (constraints->width.value/2)
			    + margin);
		done = TRUE;
	    } else if (constraints->right.done && constraints->width.done) {
		// compute using right edge and width
		value = int(constraints->right.value - (constraints->width.value/2)
			    + margin);
		done = TRUE;
	    } else if (constraints->left.done && constraints->right.done) {
		// compute using left and right edge
		value = int(constraints->left.value 
			    + (constraints->right.value-constraints->left.value)/2
			    + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxCentreY:
	switch (relationship) {
        case wxAbove:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos - margin;
		done = TRUE;
	    }
	    break;
        case wxBelow:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = edge_pos + margin;
		done = TRUE;
	    }
	    break;
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01) + margin);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (constraints->top.done && constraints->height.done) {
		// compute using top edge and height
		value = int(constraints->top.value + (constraints->height.value/2)
			    + margin);
		done = TRUE;
	    } else if (constraints->bottom.done && constraints->height.done) {
		// compute using bottom edge and height
		value = int(constraints->bottom.value - (constraints->height.value/2)
			    + margin);
		done = TRUE;
	    } else if (constraints->top.done && constraints->bottom.done) {
		// compute using top and bottom edge
		value = int(constraints->top.value 
			    + (constraints->bottom.value-constraints->top.value)/2
			    + margin);
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxWidth:
	switch (relationship) {
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01));
		done = TRUE;
	    }
	    break;
        case wxAsIs:
	    if (win) {
		int h;
		win->GetSize(&value, &h);
		done = TRUE;
	    }
	    break;
	case wxUnconstrained:
	    if (constraints->left.done && constraints->right.done) {
		// compute using left and right edge
		value = constraints->right.value - constraints->left.value;
		done = TRUE;
	    } else if (constraints->left.done && constraints->centreX.done) {
		// compute using left edge and centreX
		value = (constraints->centreX.value - constraints->left.value) * 2;
		done = TRUE;
	    } else if (constraints->right.done && constraints->centreX.done) {
		// compute using right edge and centreX
		value = (constraints->right.value - constraints->centreX.value) * 2;
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    case wxHeight:
	switch (relationship) {
        case wxPercentOf:
	    if (edge_pos != -1) { // otherWin has satisfying edge
		value = (int)(edge_pos*(((float)percent)*0.01));
		done = TRUE;
	    }
	    break;
        case wxAsIs:
	    if (win) {
		int w;
		win->GetSize(&w, &value);
		done = TRUE;
	    }
	case wxUnconstrained:
	    if (constraints->top.done && constraints->bottom.done) {
		// compute using top and bottom edge
		value = constraints->bottom.value - constraints->top.value;
		done = TRUE;
	    } else if (constraints->top.done && constraints->centreY.done) {
		// compute using top edge and centreY
		value = (constraints->centreY.value - constraints->top.value) * 2;
		done = TRUE;
	    } else if (constraints->bottom.done && constraints->centreY.done) {
		// compute using right edge and centreX
		value = (constraints->bottom.value - constraints->centreY.value) * 2;
		done = TRUE;
	    }
	    break;
	default:
	  break;
	}
	break; // goto bottom and return FALSE
    }
    return done;
}

//-----------------------------------------------------------------------------
// wxIndividualLayoutConstraint: get edge of other if obtainable
//-----------------------------------------------------------------------------

int wxIndividualLayoutConstraint::GetEdge(wxEdge which, wxWindow *thisWin,
					  wxWindow *other)
{
    if (!other)
	return -1;
    if ((wxWindow*)thisWin->GetParent() == other) { // dimension is obtainable immediately
	// Compute size of client area of parent
	int w, h; other->GetClientSize(&w, &h);
	switch (which) {
	case wxLeft: case wxTop:	    return 0;
	case wxRight: case wxWidth:	    return w;
	case wxBottom: case wxHeight:	    return h;
	case wxCentreX:			    return (w/2);
	case wxCentreY:			    return (h/2);
	}
    } else {
	wxLayoutConstraints *constr;
	wxIndividualLayoutConstraint *iconstr = NULL;
	constr = other->GetConstraints();
	switch (which) {
	case wxLeft:	    iconstr = &(constr->left); break;
	case wxTop:	    iconstr = &(constr->top); break;
	case wxRight:	    iconstr = &(constr->right); break;
	case wxBottom:	    iconstr = &(constr->bottom); break;
	case wxWidth:	    iconstr = &(constr->width); break;
	case wxHeight:	    iconstr = &(constr->height); break;
	case wxCentreX:	    iconstr = &(constr->centreX); break;
	case wxCentreY:	    iconstr = &(constr->centreY); break;
	}
	if (iconstr->done)
	    return (iconstr->value);
    }
    return -1;
}

//-----------------------------------------------------------------------------
// do layout
//-----------------------------------------------------------------------------

void wxWindow::Layout(void)
{
    /*
     * Main constrained layout algorithm. Look at all the child
     * windows, and their constraints (if any).
     * The idea is to keep iterating through the constraints
     * until all left, right, bottom and top edges, and widths and heights,
     * are known (or no change occurs and we've failed to resolve all
     * constraints).
     *
     * If the user has not specified a dimension or edge, it will be
     * be calculated from the other known values. E.g. If we know
     * the right hand edge and the left hand edge, we now know the width.
     * The snag here is that this means we must specify absolute dimensions
     * twice (in constructor and in constraint), if we wish to use the
     * constraint notation to just set the position, for example.
     * Otherwise, if we only set ONE edge and no dimension, it would never
     * find the other edge.
     *
     * Algorithm:
     *
     *    Mark all constraints as not done.
     *
     *    iterations = 0;
     *    until (no change) or (iterations >= max iterations)
     *    {
     *        For each child:
     *        {
     *            Calculate all constraints
     *        }
     *        ++iterations;
     *    }
     *
     *    For each child:
     *        Set each calculated position and size
     *
     */

    wxChildNode *node;
    wxWindow *child;
    wxLayoutConstraints *constr;

    // Layout only if children
    if (children->Number() == 0)
	return;

    // reset all constraints to NOT done
    for (node = children->First(); node; node = node->Next()) {
	child  = (wxWindow *)node->Data();
	if (wxSubType(child->__type, wxTYPE_FRAME))
	    continue;
	constr = child->GetConstraints();
	constr->UnDone();
    }
    // iterate through child until (no changes) || (no left iterations)
    {
      int  left_iterations = wxLAYOUT_MAX_ITERATIONS;
      Bool changes;
      do {
	changes = FALSE;
	for (node = children->First(); node; node = node->Next()) {
	  child  = (wxWindow *)node->Data();
	  if (wxSubType(child->__type, wxTYPE_FRAME))
	    continue;
	  child->GetConstraints();
	  changes |= constr->SatisfyConstraints(child);
	}
      } while (changes && --left_iterations);
    }
    // set sizes and positions as computed above
    for (node = children->First(); node; node = node->Next()) {
	child  = (wxWindow *)node->Data();
	if (wxSubType(child->__type, wxTYPE_FRAME))
	    continue;
	constr = child->GetConstraints();
	if (constr->left.done && constr->right.done
	&& constr->width.done && constr->height.done) {
	    // Configure calls OnSize()
	    child->Configure(constr->left.value,  constr->top.value,
			     constr->width.value, constr->height.value);
	    // layout child
	    child->Layout();
	}
    }
}

void wxPanel::Layout(void)
{
    wxWindow::Layout();
}

void wxFrame::Layout(void)
{
    wxWindow *one_child   = NULL;
    int      num_children = 0;
    wxWindow *child;
    wxChildNode *node;

    // check if frame has only ONE child
    if (children) {
      for (node = children->First(); node; node = node->Next()) {
	child = (wxWindow*)(node->Data());
	if ( child && !wxSubType(child->__type, wxTYPE_FRAME) ) {
	  // skip menubar and status line for computation
	  int i;
	  for (i = 0; i < num_status; i++) {
	    if (child == (wxWindow*)status[i])
	      break;
	  }
	  if (child == (wxWindow*)menubar || i < num_status) {
	    continue;
	  }
	  one_child = child; ++num_children;
	}
      }
    }

    // ONE child shall fit into frame
    if (num_children == 1) {
      int ww, hh;
      GetClientSize(&ww, &hh);
      one_child->SetSize(/* PANEL_HMARGIN */ 0, /* PANEL_VMARGIN */ 0,
			 ww /* -2*PANEL_HMARGIN */, hh /*-2*PANEL_VMARGIN */);
    }

    // layout window (necessary for ONE child too because of menubar and status)
    wxWindow::Layout();
}
