///////////////////////////////////////////////////////////////////////////////
// File:	wxMargin.cc
// Purpose:	wxMargin (Macintosh version)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "wxMargin.h"

/**********************************************************************
  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING
  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING
  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING  WARNING
  
  Do not write allocating expressions in this file, because it is
  skipped for precise GC. (It's skipped because we don't want to
  deal with by-reference and overloaded operators.)
 **********************************************************************/

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

//=============================================================================
// Construction methods
//=============================================================================

//-----------------------------------------------------------------------------
wxMargin::wxMargin(int margin)
{
  left = margin;
  top = margin;
  right = margin;
  bottom = margin;
}

//-----------------------------------------------------------------------------
wxMargin::wxMargin(int margin, Direction direction)
{
  left = (direction & wxLeft ? margin : 0);
  top = (direction & wxTop ? margin : 0);
  right = (direction & wxRight ? margin : 0);
  bottom = (direction & wxBottom ? margin : 0);
}

//-----------------------------------------------------------------------------
wxMargin::wxMargin(const wxMargin& margin)
{
  left = margin.left;
  top = margin.top;
  right = margin.right;
  bottom = margin.bottom;
}

//-----------------------------------------------------------------------------
wxMargin::~wxMargin(void)	// destructor
{
}

//=============================================================================
// Overloaded operator methods
//=============================================================================

wxMargin& wxMargin::operator +=(wxMargin margin)
{
  left += margin.left;
  top += margin.top;
  right += margin.right;
  bottom += margin.bottom;
  return *this;
}

//=============================================================================
// Getter and setter methods
//=============================================================================

//-----------------------------------------------------------------------------
void wxMargin::SetMargin(wxMargin margin, Direction direction)
{
  if (direction & wxLeft) left = margin.left;
  if (direction & wxTop) top = margin.top;
  if (direction & wxRight) right = margin.right;
  if (direction & wxBottom) bottom = margin.bottom;
}

//-----------------------------------------------------------------------------
void wxMargin::SetMargin(int margin, Direction direction)
{
  if (direction & wxLeft) left = margin;
  if (direction & wxTop) top = margin;
  if (direction & wxRight) right = margin;
  if (direction & wxBottom) bottom = margin;
}

//-----------------------------------------------------------------------------
int wxMargin::Offset(Direction direction)
{
  int result = 0;
  if (direction & wxLeft) result += left;
  if (direction & wxTop) result += top;
  if (direction & wxRight) result += right;
  if (direction & wxBottom) result += bottom;

  return result;
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif
