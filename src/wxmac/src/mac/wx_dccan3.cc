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
static OSStatus atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace);
static double DrawMeasLatin1Text(const char *text, int d, int theStrlen, int bit16,
				 int just_meas, int given_font, 
				 short txFont, short txSize, short txFace,
				 int again);

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

  DrawLatin1Text(text, d, -1, use16, TRUE);

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

/*****************************************************************************/
/*                          ATSU-based text (OS 9)                           */
/*****************************************************************************/

//----------------------------------------------------------------------

#define ALWAYS_USE_ATSU 1

void DrawLatin1Text(const char *text, int d, int theStrlen, int bit16, Bool qd_spacing)
{
  int i;
  int is_sym = 0;
  int again = 0;

  if (theStrlen < 0)
    theStrlen = strlen(text+d);
  
  {
    GrafPtr iGrafPtr;
    GetPort( &iGrafPtr );
    if (GetPortTextFont(iGrafPtr) == 23)
      is_sym = 1;
  }

  while (theStrlen) {
    /* Check whether we need to go into Unicode mode to get Latin-1 output: */
    if (is_sym)
      /* Symbol font hack: don't convert */
      i = theStrlen;
    else if (ALWAYS_USE_ATSU)
      i = 0;
    else {
      for (i = 0; i < theStrlen; i++) {
	if (((unsigned char *)text)[i + d] > 127)
	  break;
      }
    }

    if (i) {
      /* Up to i, it's all ASCII, where MacRoman == Latin-1 */
      ::DrawText(text+d, 0, i); // WCH: kludge, mac procedure same name as wxWindows method

      d += i;
      theStrlen -= i;
    }

    if (theStrlen) {
      int amt;

      amt = theStrlen;

      (void)DrawMeasLatin1Text(text, d, amt, bit16, 0, 0, 0, 0, 0, again);
	  
      d += amt;
      theStrlen -= amt;
      again = 1;
    }
  }
}

void GetLatin1TextWidth(const char *text, int d, int theStrlen, 
			short txFont, short txSize, short txFace,
			int bit16, float scale,
			float* x, float* y,
			float* descent, float* externalLeading,
			Bool qd_spacing)
{
  FontInfo fontInfo;
  int fsize;
  const char *meas = NULL;
  int i;
  int again = 0;

  if (text) {
    if (theStrlen < 0)
      theStrlen = strlen(text+d);
    
    if (txFont != 23) {
      if (ALWAYS_USE_ATSU) {
	i = 0;
      } else {
	/* Check whether we need to go into Unicode mode to get Latin-1 *x output: */
	for (i = 0; i < theStrlen; i++) {
	  if (((unsigned char *)text)[i + d] > 127)
	    break;
	}
      }
    } else
      /* Symbol font hack: don't convert */
      i = theStrlen;
    
    if (i >= theStrlen) {
      meas = text;
    }
  } else
    theStrlen = 0;
  
  fsize = (int)floor(txSize * scale);

  *x = wxTextFontInfo(txFont, fsize, txFace,
		      &fontInfo, (char *)meas, 
		      d, theStrlen);

  if (meas) {
    /* it's all ASCII, where MacRoman == Latin-1 */
    /* so *x is right */
  } else if (text) {
    if (qd_spacing) {
      /* Need to split the string into parts */
      *x = 0;
      while (theStrlen) {
	if (ALWAYS_USE_ATSU) {
	  i = 0;
	} else {
	  for (i = 0; i < theStrlen; i++) {
	    if (((unsigned char *)text)[i + d] > 127)
	      break;
	  }
	}

	/* Measure the leasing ASCII part, if any: */
	if (i) {
	  *x += wxTextFontInfo(txFont, fsize, txFace,
			       &fontInfo, 
			       (char *)text, d, i);
	  d += i;
	  theStrlen -= i;
	}

	/* Measure one Latin-1 part: */
	if (theStrlen) {
	  int amt;

	  amt = theStrlen;
      
	  *x += DrawMeasLatin1Text(text, d, amt, bit16,
				   1, 1, 
				   txFont, fsize, txFace,
				   again);
	  d += amt;
	  theStrlen -= amt;
	  again = 1;
	}
      }
    } else {
      *x = DrawMeasLatin1Text(text, d, theStrlen, bit16,
			      1, 1, 
			      txFont, fsize, txFace,
			      again);
      again = 1;
    }
  }

  *y = fontInfo.ascent + fontInfo.descent; // height
  if (descent) *descent = fontInfo.descent;
  if (externalLeading) *externalLeading = fontInfo.leading;
}

#define QUICK_UBUF_SIZE 256

static double DrawMeasLatin1Text(const char *text, int d, int theStrlen, int bit16,
				 int just_meas, int given_font, 
				 short txFont, short txSize, short txFace,
				 int again)
{
  ATSUTextLayout layout;
  ByteCount ubytes, converted, usize;
  UniCharCount ulen;
  char *unicode, u_buf[QUICK_UBUF_SIZE];
  double result = 0;
  ATSLineLayoutOptions ll_attribs;
  ATSUAttributeTag  ll_theTags[] = { kATSULineLayoutOptionsTag };
  ByteCount    ll_theSizes[] = { sizeof(ATSLineLayoutOptions) };
  ATSUAttributeValuePtr ll_theValues[ sizeof(ll_theTags) / sizeof(ATSUAttributeTag) ];

  if (!theATSUstyle) {
    CreateTextToUnicodeInfoByEncoding(kTextEncodingISOLatin1, &t2uinfo);
    ATSUCreateStyle(&theATSUstyle);
  }

  usize = theStrlen * 2;
  if (usize > QUICK_UBUF_SIZE)
    unicode = new WXGC_ATOMIC char[usize];
  else
    unicode = u_buf;
  ulen = theStrlen;

  ConvertFromTextToUnicode(t2uinfo, theStrlen, text + d, 0,
			   0, NULL,
			   NULL, NULL,
			   usize, &converted, &ubytes,
			   (UniCharArrayPtr)unicode);

  if (!again) {
    if (given_font)
      atsuSetStyleFromGrafPtrParams(theATSUstyle, txFont, txSize, txFace);
    else
      atsuSetStyleFromGrafPtr(theATSUstyle);
  }

  ATSUCreateTextLayoutWithTextPtr((UniCharArrayPtr)unicode,
				  kATSUFromTextBeginning,
				  kATSUToTextEnd,
				  theStrlen,
				  1,
				  &ulen,
				  &theATSUstyle,
				  &layout);

  {
    ll_attribs = (kATSLineFractDisable 
		  | kATSLineDisableAutoAdjustDisplayPos
		  | kATSLineDisableAllLayoutOperations
		  | kATSLineUseDeviceMetrics);
    ll_theValues[0] = &ll_attribs;
    ATSUSetLayoutControls(layout, 1, ll_theTags, ll_theSizes, ll_theValues);
  }

  {
    ATSTrapezoid bounds;
    ItemCount actual;

    ATSUGetGlyphBounds(layout,
		       0, 0,
		       kATSUFromTextBeginning,
		       kATSUToTextEnd,
		       kATSUseDeviceOrigins,
		       1,
		       &bounds,
		       &actual);
    
    result = (Fix2X(bounds.upperRight.x) - Fix2X(bounds.upperLeft.x));
    if (result < 0)
      result = 0;
  }

  if (!just_meas) {
    GrafPtr iGrafPtr;
    Point start;

    GetPort( &iGrafPtr );
    GetPen(&start);

    if (GetPortTextMode(iGrafPtr) == srcCopy) {
      Rect theRect;
      FontInfo fontInfo;
      ::GetFontInfo(&fontInfo);
      theRect.left = start.h;
      theRect.top = start.v - fontInfo.ascent;
      theRect.bottom = start.v + fontInfo.descent;
      theRect.right = theRect.left + (int)ceil(result);
      EraseRect(&theRect);
    }

    ATSUDrawText(layout, 
		 kATSUFromTextBeginning,
		 kATSUToTextEnd,
		 kATSUUseGrafPortPenLoc,
		 kATSUUseGrafPortPenLoc);

    MoveTo(start.h + (int)floor(result), start.v);
  }

  ATSUDisposeTextLayout(layout);

  return result;
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
atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace)
{
 OSStatus status = noErr;
 
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
 SInt16    intrinsicStyle;
 
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

static OSStatus
atsuSetStyleFromGrafPtr( ATSUStyle iStyle )
{
 short    txFont, txSize;
 SInt16   txFace;
 GrafPtr iGrafPtr;

 GetPort( &iGrafPtr );

 txFont = GetPortTextFont(iGrafPtr);
 txSize = GetPortTextSize(iGrafPtr);
 txFace = GetPortTextFace(iGrafPtr);
 
 return atsuSetStyleFromGrafPtrParams(iStyle, txFont, txSize, txFace);
}
