///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan3.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 3)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#ifndef WX_CARBON
# include <QuickDraw.h>
#endif
#include "wx_dccan.h"
#include "wx_utils.h"

extern CGrafPtr wxMainColormap;

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawText(const char* text, float x, float y, Bool use16, int d)
{
  if (!Ok()) return;
  
  SetCurrentDC();

  wxMacSetCurrentTool(kTextTool);

  FontInfo fontInfo;
  Point start, end;
  ::GetFontInfo(&fontInfo);
  start.h = XLOG2DEV(x);
  start.v = YLOG2DEV(y + fontInfo.ascent);
  MoveTo(start.h + SetOriginX, start.v + SetOriginY); // move pen to start drawing text
  int theStrlen = strlen(text+d);
  ::DrawText(text+d, 0, theStrlen); // WCH: kludge, mac procedure same name as wxWindows method
  
  // mflatt: look at pen, use distance travelled instead of calculating 
  // the length of the string (again)
  float w, h;
  ::GetPen(&end);
  w = (end.h - start.h) / (logical_scale_x * user_scale_x);
  h = (end.v - start.v) / (logical_scale_y * user_scale_y);

  CalcBoundingBox(x + w, y + h);
  CalcBoundingBox(x, y);
}

//-----------------------------------------------------------------------------
float wxCanvasDC::GetCharHeight(void)
     //-----------------------------------------------------------------------------
{
  int theCharHeight;
  if (font)
    theCharHeight = (int)font->GetCharHeight();
  else
    theCharHeight = 12;

  return XDEV2LOGREL(theCharHeight);
}

//-----------------------------------------------------------------------------
float wxCanvasDC::GetCharWidth(void)
     //-----------------------------------------------------------------------------
{
  int theCharWidth;
  if (font)
    theCharWidth = (int)font->GetCharWidth();
  else
    theCharWidth = 12;

  return XDEV2LOGREL(theCharWidth);
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetTextExtent(const char* string, float* x, float* y, float* descent,
			       float* internalLeading, wxFont* the_font, Bool use16,
			       int d)
{
  float x2, y2, descent2, externalLeading2;
  if (the_font)
    {
      the_font->GetTextExtent((char *)string+d, &x2, &y2, &descent2, &externalLeading2, use16);
    }
  else if (font)
    {
      font->GetTextExtent((char *)string+d, &x2, &y2, &descent2, &externalLeading2, use16);
    }
  else
    {
      *x = -1;
      *y = -1;
      if (descent) *descent = 0.0;
      if (internalLeading) *internalLeading = 0.0;
      return;
    }

  *x = XDEV2LOGREL(x2);
  *y = YDEV2LOGREL(y2);
  if (descent) *descent = descent2;
  if (internalLeading) *internalLeading = 0.0;
}
