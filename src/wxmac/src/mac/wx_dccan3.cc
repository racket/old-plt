///////////////////////////////////////////////////////////////////////////////
// File:	wx_dccan3.cc
// Purpose:	Canvas device context implementation (Macintosh version) (part 3)
// Author:	Bill Hale
// Created:	1994
// Updated:	
// Copyright:  (c) 2004 PLT Scheme, Inc.
// Copyright:  (c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
///////////////////////////////////////////////////////////////////////////////

#include "common.h"
#include "wx_dccan.h"
#include "wx_utils.h"

extern CGrafPtr wxMainColormap;

static ATSUStyle theATSUstyle;
static TextToUnicodeInfo t2uinfo;

static OSStatus atsuSetStyleFromGrafPtr(ATSUStyle iStyle, int smoothing, float angle, float scale_y);
static OSStatus atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace, int smoothing, 
					       float angle, float scale_y);
static double DrawMeasLatin1Text(const char *text, int d, int theStrlen, int bit16,
				 int just_meas, int given_font, 
				 short txFont, short txSize, short txFace,
				 int again, int qd_spacing, int smoothing,
				 float angle, int sym_map,
				 float scale_x, float scale_y,
				 float pen_delta_x, int with_delta,
				 float pen_start_x, float pen_start_y, float ddx, float ddy, int with_start);

#ifndef FloatToFixed
# define FloatToFixed(a) ((Fixed)((float) (a) * fixed1)) 
#endif

static int symbol_map[] = { 0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 8704, 0, 8707, 0, 0, 8717,
			    0, 0, 8727, 0, 0, 8722, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    8773, 913, 914, 935, 916, 917, 934, 915,
			    919, 921, 977, 922, 923, 924, 925, 927,
			    928, 920, 929, 931, 932, 933, 962, 937,
			    926, 936, 918, 0, 8756, 0, 8869, 0,
			    0, 945, 946, 967, 948, 949, 966, 947,
			    951, 953, 981, 954, 955, 956, 957, 959,
			    960, 952, 961, 963, 964, 965, 982, 969,
			    958, 968, 950, 0, 0, 0, 8764, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0, 0, 0, 0, 0,
			    0, 978, 8242, 8804, 8260, 8734, 402, 9827,
			    9830, 9829, 9824, 8596, 8592, 8593, 8594, 8595,
			    0, 177, 8243, 8805, 215, 8733, 8706, 8729,
			    247, 8800, 8801, 8776, 8230, 9168, 9135, 8629,
			    8501, 8465, 8476, 8472, 8855, 8853, 8709, 8745,
			    8746, 8835, 8839, 8836, 8834, 8838, 8712, 8713,
			    8736, 8711, 174, 169, 8482, 8719, 8730, 8901,
			    172, 8743, 8744, 8660, 8656, 8657, 8658, 8659,
			    9674, 9001, 174, 169, 8482, 8721, 9115, 9116,
			    9117, 9121, 9122, 9123, 9127, 9128, 9129, 9130,
			    8364, 9002, 8747, 8992, 9134, 8993, 9118, 9119,
			    9120, 9124, 9125, 9126, 9131, 9132, 9133, 0 };

//-----------------------------------------------------------------------------
void wxCanvasDC::DrawText(const char* text, float x, float y, Bool use16, int d, float angle)
{
  FontInfo fontInfo;
  float w;

  if (!Ok()) return;
  
  SetCurrentDC();

  wxMacSetCurrentTool(kTextTool);

  ::GetFontInfo(&fontInfo);
  
  w = DrawLatin1Text(text, d, -1, use16, TRUE, font->GetEffectiveSmoothing(user_scale_y), angle,
		     user_scale_x, user_scale_y,
		     1,
		     x + (fontInfo.ascent * sin(angle)) - logical_origin_x,
		     y + (fontInfo.ascent * cos(angle)) - logical_origin_y, 
		     device_origin_x + SetOriginX,
		     device_origin_y + SetOriginY);

  CalcBoundingBox(x + w, y + fontInfo.ascent + fontInfo.descent);
  CalcBoundingBox(x, y);

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
float wxCanvasDC::GetCharHeight(void)
     //-----------------------------------------------------------------------------
{
  int theCharHeight;
  if (font) {
    /* We provide the scale only for font selection. 
       The result is unscaled (as we need it). */
    theCharHeight = (int)font->GetCharHeight(user_scale_x, user_scale_y);
  } else
    theCharHeight = 12;

  return theCharHeight;
}

//-----------------------------------------------------------------------------
float wxCanvasDC::GetCharWidth(void)
     //-----------------------------------------------------------------------------
{
  int theCharWidth;
  if (font) {
    /* We provide the scale only for font selection.
       The result is unscaled (as we need it). */
    theCharWidth = (int)font->GetCharWidth(user_scale_x, user_scale_y);
  } else
    theCharWidth = 12;

  return theCharWidth;
}

//-----------------------------------------------------------------------------
void wxCanvasDC::GetTextExtent(const char* string, float* x, float* y, float* descent,
			       float* internalLeading, wxFont* the_font, Bool use16,
			       int d)
{
  float x2, y2, descent2, externalLeading2;

  /* Note: extent result is unscaled. We provide scales only in case it matters
     in font selection. */

  if (the_font)
    the_font->GetTextExtent((char *)string, d, &x2, &y2, &descent2, &externalLeading2, use16, 
			    user_scale_x, user_scale_y);
  else if (font)
    font->GetTextExtent((char *)string, d, &x2, &y2, &descent2, &externalLeading2, use16,
			user_scale_x, user_scale_y);
  else {
    *x = -1;
    *y = -1;
    if (descent) *descent = 0.0;
    if (internalLeading) *internalLeading = 0.0;
    return;
  }

  *x = x2;
  *y = y2;
  if (descent) *descent = descent2;
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

double DrawLatin1Text(const char *text, int d, int theStrlen, int bit16, Bool qd_spacing, int smoothing, float angle,
		      float scale_x, float scale_y, int use_start, float start_x, float start_y, float ddx, float ddy)
{
  int i;
  int is_sym = 0;
  int again = 0;
  float pen_delta = 0.0;
  int move_pen_at_end;

  if (theStrlen < 0)
    theStrlen = strlen(text+d);
  
  {
    GrafPtr iGrafPtr;
    GetPort( &iGrafPtr );
    if (GetPortTextFont(iGrafPtr) == 23)
      is_sym = 1;
  }

  move_pen_at_end = qd_spacing && ALWAYS_USE_ATSU && !use_start;

  while (theStrlen) {
    /* Check whether we need to go into Unicode mode to get Latin-1 output: */
    if (!qd_spacing || ALWAYS_USE_ATSU) {
      i = 0;
    } else if (is_sym)
      /* Symbol font hack: don't convert */
      i = theStrlen;
    else {
      for (i = 0; i < theStrlen; i++) {
	if (((unsigned char *)text)[i + d] > 127)
	  break;
      }
    }

    if (i) {
      /* Up to i, it's all ASCII, where MacRoman == Latin-1 */
      int reset_size = 0;
      Point pen_start, pen_end;

      if (scale_y != 1.0) {
	GrafPtr iGrafPtr;
	int ssize;

	GetPort( &iGrafPtr );
	reset_size = GetPortTextSize(iGrafPtr);
	ssize = (int)floor(scale_y * reset_size);
	if (!ssize)
	  ssize = 1;
	::TextSize(ssize);
      }

      if (use_start) {
	MoveTo((short)floor((start_x * scale_x) + ddx), 
	       (short)floor((start_y * scale_y) + ddy));
	use_start = 0;
      }
      ::GetPen(&pen_start);

      ::DrawText(text+d, 0, i);

      if (reset_size)
	::TextSize(reset_size);

      ::GetPen(&pen_end);
      pen_delta += (pen_end.h - pen_start.h);

      d += i;
      theStrlen -= i;
    }

    if (theStrlen) {
      int amt;

      if (!qd_spacing || ALWAYS_USE_ATSU)
	amt = theStrlen;
      else
	amt = 1;

      pen_delta += DrawMeasLatin1Text(text, d, amt, bit16, 0, 0, 0, 0, 0, again, 
				      qd_spacing, smoothing, angle, is_sym,
				      scale_x, scale_y,
				      pen_delta, move_pen_at_end || use_start,
				      start_x, start_y, ddx, ddy, use_start);
	  
      d += amt;
      theStrlen -= amt;
      again = 1;
    }
  }

  if (move_pen_at_end) {
    Point start;
    GetPen(&start);
    MoveTo(start.h + (int)floor(pen_delta * scale_x), start.v);
  }

  return pen_delta;
}

void GetLatin1TextWidth(const char *text, int d, int theStrlen, 
			short txFont, short txSize, short txFace,
			int bit16, float scale_y,
			float* x, float* y,
			float* descent, float* externalLeading,
			Bool qd_spacing, float scale_x)
{
  FontInfo fontInfo;
  const char *meas = NULL;
  int i, is_sym = (txFont == 23);

  if (text) {
    if (theStrlen < 0)
      theStrlen = strlen(text+d);
    
    if (!qd_spacing || ALWAYS_USE_ATSU) {
      i = 0;
    } else {
      if (!is_sym) {
	/* Check whether we need to go into Unicode mode to get Latin-1 *x output: */
	for (i = 0; i < theStrlen; i++) {
	  if (((unsigned char *)text)[i + d] > 127)
	    break;
	}
      } else
	/* Symbol font hack: don't convert */
	i = theStrlen;
    }
    
    if (i >= theStrlen) {
      meas = text;
    }
  } else
    theStrlen = 0;

  /* gets ascent, etc., and gets width if meas is non-NULL: */
  *x = wxTextFontInfo(txFont, txSize, txFace,
		      &fontInfo, (char *)meas, 
		      d, theStrlen);

  if (meas) {
    /* it's all ASCII, where MacRoman == Latin-1 */
    /* so *x is right */
  } else if (text) {
    if (!qd_spacing || ALWAYS_USE_ATSU) {
      *x = DrawMeasLatin1Text(text, d, theStrlen, bit16,
			      1, 1, 
			      txFont, txSize, txFace,
			      0, qd_spacing, wxSMOOTHING_DEFAULT, 0.0, is_sym,
			      scale_x, scale_y,
			      0.0, 0, 0.0, 0.0, 0.0, 0.0, 0);
    } else {
      /* Need to split the string into parts */
      int again = 0;
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

	/* Measure the leading ASCII part, if any: */
	if (i) {
	  *x += wxTextFontInfo(txFont, txSize, txFace,
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
				   txFont, txSize, txFace,
				   again, qd_spacing,
				   wxSMOOTHING_DEFAULT, 0.0, is_sym,
				   scale_x, scale_y, 
				   0.0, 0, 0.0, 0.0, 0.0, 0.0, 0);
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
				 int again, int qd_spacing, int smoothing,
				 float angle, int is_sym,
				 float scale_x, float scale_y,
				 float pen_delta, int use_pen_delta,
				 float start_x, float start_y, float ddx, float ddy, int with_start)
{
  ATSUTextLayout layout;
  ByteCount usize;
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
  int need_convert;
#define JUSTDELTA(v, s, d) (need_convert ? v : ((v - d) / s))
#define COORDCONV(v, s, d) (need_convert ? ((v * s) + d) : v)
#ifdef OS_X
  CGrafPtr qdp;
  CGContextRef cgctx;
  Rect portRect;
  RGBColor eraseColor;
  int use_cgctx = (always_use_atsu 
		   && ((smoothing != wxSMOOTHING_PARTIAL) || (scale_x != scale_y)));
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

  if (is_sym) {
    int i, v, m;
    for (i = 0; i < theStrlen; i++) {
      v = ((unsigned char *)text)[d+i];
      m = symbol_map[v];
      ((UniCharArrayPtr)unicode)[i] = (m ? m : v);
    }
  } else {
    ByteCount ubytes, converted;
    ConvertFromTextToUnicode(t2uinfo, theStrlen, text + d, 0,
			     0, NULL,
			     NULL, NULL,
			     usize, &converted, &ubytes,
			     (UniCharArrayPtr)unicode);
  }

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

  if (use_cgctx) {
    /* Set scale */
    if (!just_meas) {
      long h;
      h = portRect.bottom - portRect.top;
      CGContextTranslateCTM(cgctx, 
			    (with_start ? ddx : 0), 
			    h - (with_start ? ddy : 0));
      ddx = 0;
      ddy = 0;
    }
    CGContextScaleCTM(cgctx, scale_x, scale_y);
  }
#endif

  if (!again) {
    if (given_font)
      atsuSetStyleFromGrafPtrParams(theATSUstyle, txFont, txSize, txFace, smoothing, 
				    angle, (use_cgctx || just_meas) ? 1.0 : scale_y);
    else
      atsuSetStyleFromGrafPtr(theATSUstyle, smoothing, angle, 
			      (use_cgctx || just_meas) ? 1.0 : scale_y);
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

  if (angle != 0.0) {
    GC_CAN_IGNORE ATSUAttributeTag  r_theTags[] = { kATSULineRotationTag };
    GC_CAN_IGNORE ByteCount    r_theSizes[] = { sizeof(Fixed) };
    ATSUAttributeValuePtr r_theValues[1];
    Fixed deg_angle;

    deg_angle = FloatToFixed(angle * 180 / 3.14159);
    r_theValues[0] = &deg_angle;
    ATSUSetLayoutControls(layout, 1, r_theTags, r_theSizes, r_theValues); 
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
    if (!use_cgctx && !just_meas) {
      result = result / scale_y;
    }
  }

  if (!just_meas) {
    GrafPtr iGrafPtr;
    Point start;

#ifdef OS_X
    iGrafPtr = qdp;
#else    
    GetPort(&iGrafPtr);
#endif

    if (!with_start) {
      GetPen(&start);
      start_x = start.h;
      start_y = start.v;
      ddx = 0;
      ddy = 0;
      need_convert = 0;
    } else {
      need_convert = 1;
    }
    
    if ((angle == 0.0) && (GetPortTextMode(iGrafPtr) == srcCopy)) {
      FontInfo fontInfo;
      ::GetFontInfo(&fontInfo);
#ifdef OS_X
      if (use_cgctx) {
	CGRect cgr;
	float rt, rl, rr, rb;

	rl = JUSTDELTA(start_x, scale_x, ddx) + (use_pen_delta ? pen_delta : 0.0);
	rt = JUSTDELTA(start_y, scale_y, ddy) - fontInfo.ascent;
	rb = JUSTDELTA(start_y, scale_y, ddy) + fontInfo.descent;
	rr = rl + result;

	cgr.origin.x = rl;
	cgr.origin.y = -rb;
	cgr.size.width = rr - rl;
	cgr.size.height = rb - rt;

	CGContextSetRGBFillColor(cgctx, 
				 (float)eraseColor.red / 65535.0,
				 (float)eraseColor.green / 65535.0,
				 (float)eraseColor.blue / 65535.0,
				 1.0);
	CGContextFillRect(cgctx, cgr);
      } else
#endif
	{
	  Rect theRect;
	  float rt, rl, rr, rb;
	  
	  rl = COORDCONV(start_x, scale_x, ddx) + (use_pen_delta ? (pen_delta * scale_x) : 0.0);
	  rt = COORDCONV(start_y, scale_y, ddy) - (fontInfo.ascent * scale_y);
	  rb = COORDCONV(start_y, scale_y, ddy) + (fontInfo.descent * scale_y);
	  rr = rl + (result * scale_x);

	  theRect.left = (int)floor(rl);
	  theRect.top = (int)floor(rt);
	  theRect.right = (int)floor(rr);
	  theRect.bottom = (int)floor(rb);
	  EraseRect(&theRect);
	}
    }
    
    {
      Fixed sx, sy;

      if (use_cgctx) {
	float isx;
	isx = JUSTDELTA(start_x, scale_x, ddx) + (use_pen_delta ? pen_delta : 0.0);
	sx = FloatToFixed(isx);
      } else if (with_start) {
	float isx;
	isx = COORDCONV(start_x, scale_x, ddx) + (use_pen_delta ? (pen_delta * scale_x) : 0.0);
	sx = FloatToFixed(isx);
      } else {
	sx = kATSUUseGrafPortPenLoc;
      }

      if (use_cgctx) {
	float isy;
	isy = -JUSTDELTA(start_y, scale_y, ddy);
	sy = FloatToFixed(isy);
      } else if (with_start) {
	float isy;
	isy = COORDCONV(start_y, scale_y, ddy);
	sy = FloatToFixed(isy);
      } else {
	sy = kATSUUseGrafPortPenLoc;
      }

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
    if (!just_meas && !use_pen_delta && !with_start)
      MoveTo(start.h + (int)floor(result * scale_x), start.v);
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
atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace, int smoothing, 
			       float angle, float scale_y)
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
					       kATSUStyleRenderingOptionsTag
                                               };
 GC_CAN_IGNORE ByteCount    theSizes[] = { sizeof(ATSUFontID),
					   sizeof(Fixed),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(RGBColor),
					   sizeof(ATSStyleRenderingOptions)
                                           };
 ATSUAttributeValuePtr theValues[ xNUM_TAGS /* = sizeof(theTags) / sizeof(ATSUAttributeTag) */ ];
 int tag_count;

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

 if (scale_y != 1.0)
   txSize = (short)floor(txSize * scale_y);
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

 tag_count = xNUM_TAGS;

 status = ATSUSetAttributes( iStyle, tag_count, theTags, theSizes, theValues );

 return status;
}

static OSStatus
atsuSetStyleFromGrafPtr(ATSUStyle iStyle, int smoothing, float angle, float scale_y)
{
 short    txFont, txSize;
 SInt16   txFace;
 GrafPtr iGrafPtr;

 GetPort( &iGrafPtr );

 txFont = GetPortTextFont(iGrafPtr);
 txSize = GetPortTextSize(iGrafPtr);
 txFace = GetPortTextFace(iGrafPtr);
 
 return atsuSetStyleFromGrafPtrParams(iStyle, txFont, txSize, txFace, smoothing, angle, scale_y);
}
