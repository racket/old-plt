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

static ATSUStyle theATSUstyle;
static TextToUnicodeInfo t2uinfo;

static OSStatus atsuSetStyleFromGrafPtr( ATSUStyle iStyle );

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
  start.v = YLOG2DEV(y) + fontInfo.ascent; /* ascent is already scaled */
  MoveTo(start.h + SetOriginX, start.v + SetOriginY); // move pen to start drawing text

  DrawLatin1Text(text, d, -1, use16);

  // look at pen, use distance travelled instead of calculating 
  // the length of the string (again)
  float w, h;
  ::GetPen(&end);
  w = (end.h - start.h) / (logical_scale_x * user_scale_x);
  h = (end.v - start.v) / (logical_scale_y * user_scale_y);

  CalcBoundingBox(x + w, y + h);
  CalcBoundingBox(x, y);

  ReleaseCurrentDC();
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
    the_font->GetTextExtent((char *)string+d, &x2, &y2, &descent2, &externalLeading2, use16, user_scale_y);
  else if (font)
    font->GetTextExtent((char *)string+d, &x2, &y2, &descent2, &externalLeading2, use16, user_scale_y);
  else {
    *x = -1;
    *y = -1;
    if (descent) *descent = 0.0;
    if (internalLeading) *internalLeading = 0.0;
    return;
  }

  *x = XDEV2LOGREL(x2);
  *y = YDEV2LOGREL(y2);
  if (descent) *descent = YDEV2LOGREL(descent2);
  if (internalLeading) *internalLeading = 0.0;
}

//----------------------------------------------------------------------

void DrawLatin1Text(const char *text, int d, int theStrlen, int bit16)
{
  int i;

  if (theStrlen < 0)
    theStrlen = strlen(text+d);
  
  /* Check whether we need to go into Unicode mode to get Latin-1 output: */
  for (i = 0; i < theStrlen; i++) {
    if (((unsigned char *)text)[i + d] > 127)
      break;
  }

  if (i >= theStrlen) {
    /* it's all ASCII, where MacRoman == Latin-1 */
    ::DrawText(text+d, 0, theStrlen); // WCH: kludge, mac procedure same name as wxWindows method
  } else {
    ATSUTextLayout layout;
    ByteCount ubytes, converted, usize;
    UniCharCount ulen;
    ATSUStyle style;
    char *unicode;

    if (!theATSUstyle) {
      CreateTextToUnicodeInfoByEncoding(kTextEncodingISOLatin1, &t2uinfo);
      ATSUCreateStyle(&theATSUstyle);
    }

    usize = theStrlen * 2;
    unicode = new WXGC_ATOMIC char[usize];
    ulen = theStrlen;

    ConvertFromTextToUnicode(t2uinfo, theStrlen, text + d, 0,
			     0, NULL,
			     NULL, NULL,
			     usize, &converted, &ubytes,
			     (UniCharArrayPtr)unicode);

    atsuSetStyleFromGrafPtr(theATSUstyle);

    style = theATSUstyle;

    ATSUCreateTextLayoutWithTextPtr((UniCharArrayPtr)unicode,
				    kATSUFromTextBeginning,
				    kATSUToTextEnd,
				    theStrlen,
				    1,
				    &ulen,
				    &style,
				    &layout);

    ATSUDrawText(layout, 
		 kATSUFromTextBeginning,
		 kATSUToTextEnd,
		 kATSUUseGrafPortPenLoc,
		 kATSUUseGrafPortPenLoc);

    ATSUDisposeTextLayout(layout);
  }
}

/************************************************************************/
/************************************************************************/

 

/* This code comes from an Apple example: */


/*
 This is just like ATSUFONDtoFontID except that it also returns the intrinsic
 style of the selected font. This information is needed to correctly adjust the
 QD style bits. See the implementation of atsuSetStyleFromGrafPtr for an example.
 
 NB: On Mac OS 9 or later, just call through to FMGetFontFromFontFamilyInstance,
 which is the preferred function.
*/

static OSStatus
atsuFONDtoFontID( short    iFONDNumber,
      StyleParameter iFONDStyle,
      ATSUFontID *  oFontID,
       StyleParameter * oIntrinsicStyle )
{
 return FMGetFontFromFontFamilyInstance( iFONDNumber, iFONDStyle, oFontID, oIntrinsicStyle );
}

#define apple_require(x, y) if (!(x)) return status;

static OSStatus
atsuSetStyleFromGrafPtr( ATSUStyle iStyle )
{
 OSStatus status = noErr;
 GrafPtr iGrafPtr;
 
 ATSUAttributeTag  theTags[] = { kATSUFontTag,
           kATSUSizeTag,
           kATSUQDBoldfaceTag,
           kATSUQDItalicTag,
           kATSUQDUnderlineTag,
           kATSUQDCondensedTag,
           kATSUQDExtendedTag,
           kATSUColorTag };
 ByteCount    theSizes[] = { sizeof(ATSUFontID),
           sizeof(Fixed),
           sizeof(Boolean),
           sizeof(Boolean),
           sizeof(Boolean),
           sizeof(Boolean),
           sizeof(Boolean),
           sizeof(RGBColor) };
 ATSUAttributeValuePtr theValues[ sizeof(theTags) / sizeof(ATSUAttributeTag) ];
 
 ATSUFontID   atsuFont;
 Fixed    atsuSize;
 RGBColor   textColor;
 Boolean    isBold, isItalic, isUnderline, isCondensed, isExtended;
 short    txFont, txSize;
 SInt16    txFace, intrinsicStyle;
 
 
 GetPort( &iGrafPtr );

 txFont = GetPortTextFont(iGrafPtr);
 txSize = GetPortTextSize(iGrafPtr);
 txFace = GetPortTextFace(iGrafPtr);
 
 status = atsuFONDtoFontID( txFont, txFace, &atsuFont, &intrinsicStyle );
 apple_require( status == noErr, EXIT );
 
 // Need to adjust the QD style bits based on the intrinsic style of the font.
 // Otherwise, you can end up doing things like artifically bolding an already-bold font.
 txFace &= ~intrinsicStyle;
 
 isBold = ( txFace & bold ) != 0;
 isItalic = ( txFace & italic ) != 0;
 isUnderline = ( txFace & underline ) != 0;
 isCondensed = ( txFace & condense ) != 0;
 isExtended = ( txFace & extend ) != 0;
 
 if ( txSize == 0 )
  txSize = (short) ( GetScriptVariable( FontToScript( txFont ), smScriptPrefFondSize ) & 0xFFFFU ); // this would already be set correctly in a brand-new style
 atsuSize = Long2Fix( txSize );
 
 GetForeColor( &textColor );
 
 // C doesn't allow this to be done in an initializer, so we have to fill in the pointers here.
 theValues[0] = &atsuFont;
 theValues[1] = &atsuSize;
 theValues[2] = &isBold;
 theValues[3] = &isItalic;
 theValues[4] = &isUnderline;
 theValues[5] = &isCondensed;
 theValues[6] = &isExtended;
 theValues[7] = &textColor;
 
 status = ATSUSetAttributes( iStyle, sizeof(theTags) / sizeof(ATSUAttributeTag), theTags, theSizes, theValues );

 return status;
}
