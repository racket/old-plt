///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan3.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 3)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_dccan.h"
#include "wx_utils.h"

extern CGrafPtr wxMainColormap;

static ATSUStyle theATSUstyle;
static TextToUnicodeInfo t2uinfo;

static OSStatus atsuSetStyleFromGrafPtr(ATSUStyle iStyle, int smoothing);
static OSStatus atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace, int smoothing);
static double DrawMeasLatin1Text(const char *text, int d, int theStrlen, int bit16,
				 int just_meas, int given_font, 
				 short txFont, short txSize, short txFace,
				 int again, int qd_spacing, int smoothing);

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawText(const char* text, float x, float y, Bool use16, int d)
{
  FontInfo fontInfo;
  Point start, end;
  float w, h;

  if (!Ok()) return;
  
  SetCurrentDC();

  wxMacSetCurrentTool(kTextTool);

  ::GetFontInfo(&fontInfo);
  start.h = XLOG2DEV(x);
  start.v = YLOG2DEV(y) + fontInfo.ascent; /* ascent is already scaled */
  MoveTo(start.h + SetOriginX, start.v + SetOriginY); // move pen to start drawing text

  DrawLatin1Text(text, d, -1, use16, TRUE, font->GetEffectiveSmoothing(user_scale_y));

  // look at pen, use distance travelled instead of calculating 
  // the length of the string (again)
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
    the_font->GetTextExtent((char *)string, d, &x2, &y2, &descent2, &externalLeading2, use16, user_scale_y);
  else if (font)
    font->GetTextExtent((char *)string, d, &x2, &y2, &descent2, &externalLeading2, use16, user_scale_y);
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
/*                              ATSU-based text                              */
/*****************************************************************************/

//----------------------------------------------------------------------

#ifdef OS_X
static int always_use_atsu = 1;
# define ALWAYS_USE_ATSU always_use_atsu
#else
# define ALWAYS_USE_ATSU 0
#endif

void wxCheckATSUCapability()
{
#ifdef OS_X
  /* Disable always_use_atsu if the part we need isn't there */
  SInt32 res;
  Gestalt(gestaltATSUVersion, &res);
  if (res <  (7 << 16) /* gestaltATSUUpdate6 */)
    always_use_atsu = 0;
#endif
}

void DrawLatin1Text(const char *text, int d, int theStrlen, int bit16, Bool qd_spacing, int smoothing)
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
    else if (!qd_spacing || ALWAYS_USE_ATSU)
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

      if (!qd_spacing || ALWAYS_USE_ATSU)
	amt = theStrlen;
      else
	amt = 1;

      (void)DrawMeasLatin1Text(text, d, amt, bit16, 0, 0, 0, 0, 0, again, qd_spacing, smoothing);
	  
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
      if (!qd_spacing || ALWAYS_USE_ATSU) {
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
    if (!qd_spacing || ALWAYS_USE_ATSU) {
      *x = DrawMeasLatin1Text(text, d, theStrlen, bit16,
			      1, 1, 
			      txFont, fsize, txFace,
			      again, qd_spacing, wxSMOOTHING_DEFAULT);
      again = 1;
    } else {
      /* Need to split the string into parts */
      *x = 0;
      while (theStrlen) {
	for (i = 0; i < theStrlen; i++) {
	  if (((unsigned char *)text)[i + d] > 127)
	    break;
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

	  amt = 1;
      
	  *x += DrawMeasLatin1Text(text, d, amt, bit16,
				   1, 1, 
				   txFont, fsize, txFace,
				   again, qd_spacing,
				   wxSMOOTHING_DEFAULT);
	  d += amt;
	  theStrlen -= amt;
	  again = 1;
	}
      }
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
				 int again, int qd_spacing, int smoothing)
{
  ATSUTextLayout layout;
  ByteCount ubytes, converted, usize;
  UniCharCount ulen;
  char *unicode, u_buf[QUICK_UBUF_SIZE];
  double result = 0;
  ATSLineLayoutOptions ll_attribs;
  GC_CAN_IGNORE ATSUAttributeTag  ll_theTags[] = { kATSULineLayoutOptionsTag 
#ifdef OS_X
						   , kATSUCGContextTag
# define lxNUM_TAGS 2
#else
# define lxNUM_TAGS 1
#endif
  };
  GC_CAN_IGNORE ByteCount    ll_theSizes[] = { sizeof(ATSLineLayoutOptions) 
#ifdef OS_X
					       , sizeof(CGContextRef)
#endif
  };
  ATSUAttributeValuePtr ll_theValues[ lxNUM_TAGS /* = sizeof(ll_theTags) / sizeof(ATSUAttributeTag) */ ];
#ifdef OS_X
  CGrafPtr qdp;
  CGContextRef cgctx;
  Rect portRect;
  RGBColor eraseColor;
  int use_cgctx = always_use_atsu && (smoothing != wxSMOOTHING_PARTIAL);
# define xOS_X_ONLY(x) x
#else
# define use_cgctx 0
# define xOS_X_ONLY(x) 0
#endif

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

#ifdef OS_X
  GetPort(&qdp);
  if (!just_meas) {
    Rect r = { -1, -1, 0, 0};

    GetPortBounds(qdp, &portRect); 
    GetBackColor(&eraseColor);

    EraseRect(&r);
  }

  if (use_cgctx && QDBeginCGContext(qdp, &cgctx))
    use_cgctx = 0;

  if (use_cgctx && !just_meas) {
    /* Make clipping regions match (including BeginUpdate effect) */
    RgnHandle clipRgn;
    clipRgn = NewRgn();
    if (clipRgn) {
      RgnHandle visRgn;
      visRgn = NewRgn();
      if (visRgn) {
	GetPortClipRegion(qdp, clipRgn);
	GetPortVisibleRegion(qdp, visRgn);
	SectRgn(clipRgn, visRgn, clipRgn);
	ClipCGContextToRegion(cgctx, &portRect, clipRgn);
	DisposeRgn(visRgn);
      }
      DisposeRgn(clipRgn);
    }
  }
#endif

  if (!again) {
    if (given_font)
      atsuSetStyleFromGrafPtrParams(theATSUstyle, txFont, txSize, txFace, smoothing);
    else
      atsuSetStyleFromGrafPtr(theATSUstyle, smoothing);
  }

  ATSUCreateTextLayoutWithTextPtr((UniCharArrayPtr)unicode,
				  kATSUFromTextBeginning,
				  kATSUToTextEnd,
				  theStrlen,
				  1,
				  &ulen,
				  &theATSUstyle,
				  &layout);

  if (qd_spacing || use_cgctx) {
    if (qd_spacing) {
#if 1
      /* We write down a literal constant, because the constants aren't
	 in 10.1 */
      ll_attribs = 0x11f4040;
#else
      ll_attribs = (kATSLineFractDisable 
		    | kATSLineDisableAutoAdjustDisplayPos
		    | kATSLineDisableAllLayoutOperations
		    | kATSLineUseDeviceMetrics);
#endif
      ll_theValues[0] = &ll_attribs;
    } else {
      ll_theTags[0] = ll_theTags[1];
      ll_theSizes[0] = ll_theSizes[1];
    }
#ifdef OS_X
    ll_theValues[(qd_spacing ? 1 : 0)] = &cgctx;
#endif
    ATSUSetLayoutControls(layout, (lxNUM_TAGS - (use_cgctx ? 0 : 1) - (qd_spacing ? 0 : 1)),
			  ll_theTags, ll_theSizes, ll_theValues);
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

#ifdef OS_X
    iGrafPtr = qdp;
#else    
    GetPort(&iGrafPtr);
#endif
    GetPen(&start);
    
    if (GetPortTextMode(iGrafPtr) == srcCopy) {
      Rect theRect;
      FontInfo fontInfo;
      ::GetFontInfo(&fontInfo);
      theRect.left = start.h;
      theRect.top = start.v - fontInfo.ascent;
      theRect.bottom = start.v + fontInfo.descent;
      theRect.right = theRect.left + (int)floor(result);
#ifdef OS_X
      if (use_cgctx) {
	CGRect cgr;
	cgr.origin.x = theRect.left;
	cgr.origin.y = portRect.top + (portRect.bottom - theRect.bottom);
	cgr.size.width = theRect.right - theRect.left;
	cgr.size.height = theRect.bottom - theRect.top;
	CGContextSetRGBFillColor(cgctx, 
				 (float)eraseColor.red / 65535.0,
				 (float)eraseColor.green / 65535.0,
				 (float)eraseColor.blue / 65535.0,
				 1.0);
	CGContextFillRect(cgctx, cgr);
      } else
#endif
	EraseRect(&theRect);
    }
    
    {
      Fixed sx, sy;

      sx = (use_cgctx 
	    ? Long2Fix(start.h) 
	    : kATSUUseGrafPortPenLoc);
      sy = (use_cgctx 
	    ? xOS_X_ONLY(Long2Fix(portRect.top + (portRect.bottom - start.v)))
	    : kATSUUseGrafPortPenLoc);

      ATSUDrawText(layout, 
		   kATSUFromTextBeginning,
		   kATSUToTextEnd,
		   sx, sy);
    }

#ifdef OS_X
    if (use_cgctx) {
      /* I don't think this flush is supposed to be
	 necessary. However, sometimes text gets lost in the draw.ss
	 test without it. (Notably, text gets lost only when drawing
	 directly to the screen, and not when drawing to a bitmap. */
      CGContextSynchronize(cgctx);

      QDEndCGContext(qdp, &cgctx);
    }
#endif

    /* QuickDraw is back again in OS X: */
    if (!just_meas)
      MoveTo(start.h + (int)floor(result), start.v);
  } else {
#ifdef OS_X
    if (use_cgctx) {
      QDEndCGContext(qdp, &cgctx);
    }
#endif
  }

  ATSUDisposeTextLayout(layout);

#ifdef OS_X
  if (use_cgctx) {
  }
#endif

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
atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace, int smoothing)
{
 OSStatus status = noErr;

#define xNUM_TAGS 9
 
 GC_CAN_IGNORE ATSUAttributeTag  theTags[] = { kATSUFontTag,
					       kATSUSizeTag,
					       kATSUQDBoldfaceTag,
					       kATSUQDItalicTag,
					       kATSUQDUnderlineTag,
					       kATSUQDCondensedTag,
					       kATSUQDExtendedTag,
					       kATSUColorTag,
					       kATSUStyleRenderingOptionsTag };
 GC_CAN_IGNORE ByteCount    theSizes[] = { sizeof(ATSUFontID),
					   sizeof(Fixed),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(RGBColor),
					   sizeof(ATSStyleRenderingOptions) };
 ATSUAttributeValuePtr theValues[ xNUM_TAGS /* = sizeof(theTags) / sizeof(ATSUAttributeTag) */ ];
 
 ATSUFontID   atsuFont;
 Fixed    atsuSize;
 RGBColor   textColor;
 Boolean    isBold, isItalic, isUnderline, isCondensed, isExtended;
 SInt16    intrinsicStyle;
 ATSStyleRenderingOptions options = kATSStyleNoOptions;
 
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
 
 if ( txSize == 0 ) {
   // this would already be set correctly in a brand-new style
   txSize = (short) ( GetScriptVariable( FontToScript( txFont ), smScriptPrefFondSize ) & 0xFFFFU );
 }
 atsuSize = Long2Fix( txSize );
 
 GetForeColor( &textColor );

#ifdef OS_X
 if (smoothing == wxSMOOTHING_OFF)
   options = kATSStyleNoAntiAliasing;
 else if (smoothing == wxSMOOTHING_ON)
   options = kATSStyleApplyAntiAliasing;
#else
 options = 0;
#endif

 // C doesn't allow this to be done in an initializer, so we have to fill in the pointers here.
 theValues[0] = &atsuFont;
 theValues[1] = &atsuSize;
 theValues[2] = &isBold;
 theValues[3] = &isItalic;
 theValues[4] = &isUnderline;
 theValues[5] = &isCondensed;
 theValues[6] = &isExtended;
 theValues[7] = &textColor;
 theValues[8] = &options;

 status = ATSUSetAttributes( iStyle, xNUM_TAGS, theTags, theSizes, theValues );

 return status;
}

static OSStatus
atsuSetStyleFromGrafPtr(ATSUStyle iStyle, int smoothing)
{
 short    txFont, txSize;
 SInt16   txFace;
 GrafPtr iGrafPtr;

 GetPort( &iGrafPtr );

 txFont = GetPortTextFont(iGrafPtr);
 txSize = GetPortTextSize(iGrafPtr);
 txFace = GetPortTextFace(iGrafPtr);
 
 return atsuSetStyleFromGrafPtrParams(iStyle, txFont, txSize, txFace, smoothing);
}
