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
#include "scheme.h"

extern CGrafPtr wxMainColormap;

extern "C" {
  int scheme_utf8_decode(const unsigned char *s, int start, int len, 
			 unsigned int *us, int dstart, int dlen,
			 long *ipos, char utf16, int permissive);
};

static ATSUStyle theATSUstyle, theATSUqdstyle;

#define MAX_WIDTH_MAPPINGS 1024
static Scheme_Hash_Table *width_table, *old_width_table;
static Scheme_Object *table_key;

typedef struct {
  double scale_x, scale_y;
  int code;
  short txFont, txSize, txFace;
  char use_cgctx, smoothing;
} wxKey;

typedef void (*atomic_timeout_t)(void);

static void init_ATSU_style(void);
static OSStatus atsuSetStyleFromGrafPtr(ATSUStyle iStyle, int smoothing, double angle, double scale_y, int qd_spacing);
static OSStatus atsuSetStyleFromGrafPtrParams( ATSUStyle iStyle, short txFont, short txSize, SInt16 txFace, int smoothing, 
					       double angle, double scale_y, int qd_spacing);
static double DrawMeasUnicodeText(const char *text, int d, int theStrlen, int ucs4,
				  int just_meas, int given_font, 
				  short txFont, short txSize, short txFace,
				  int again, int qd_spacing, int smoothing,
				  double angle, int sym_map,
				  double scale_x, double scale_y,
				  double pen_delta_x, int with_delta,
				  double pen_start_x, double pen_start_y, double ddx, double ddy, int with_start);

#ifndef DoubleToFixed
# define DoubleToFixed(a) ((Fixed)((double) (a) * fixed1)) 
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
void wxCanvasDC::DrawText(const char* text, double x, double y, Bool combine, Bool ucs4, int d, double angle)
{
  FontInfo fontInfo;
  double w;

  if (!Ok()) return;
  
  SetCurrentDC();

  wxMacSetCurrentTool(kTextTool);

  ::GetFontInfo(&fontInfo);
  
  w = wxDrawUnicodeText(text, d, -1, ucs4, 
			!combine, font->GetEffectiveSmoothing(user_scale_y), angle,
			user_scale_x, user_scale_y,
			1,
			x + (fontInfo.ascent * sin(angle)) - logical_origin_x,
			y + (fontInfo.ascent * cos(angle)) - logical_origin_y, 
			device_origin_x + SetOriginX,
			device_origin_y + SetOriginY,
			font->GetFamily());
  
  CalcBoundingBox(x + w, y + fontInfo.ascent + fontInfo.descent);
  CalcBoundingBox(x, y);

  ReleaseCurrentDC();
}

//-----------------------------------------------------------------------------
double wxCanvasDC::GetCharHeight(void)
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
double wxCanvasDC::GetCharWidth(void)
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
void wxCanvasDC::GetTextExtent(const char* string, double* x, double* y, double* descent,
			       double* internalLeading, wxFont* the_font, 
			       Bool combine, Bool ucs4, int d)
{
  double x2, y2, descent2, externalLeading2;

  /* Note: extent result is unscaled. We provide scales only in case it matters
     in font selection. */

  if (the_font)
    the_font->GetTextExtent((char *)string, d, &x2, &y2, &descent2, &externalLeading2, 
			    !combine, ucs4, 
			    user_scale_x, user_scale_y);
  else if (font)
    font->GetTextExtent((char *)string, d, &x2, &y2, &descent2, &externalLeading2, 
			!combine, ucs4,
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

static int always_use_atsu = 1;
# define ALWAYS_USE_ATSU always_use_atsu

void wxCheckATSUCapability()
{
  /* Disable always_use_atsu if the part we need isn't there */
  SInt32 res;
  Gestalt(gestaltATSUVersion, &res);
  if (res <  (7 << 16) /* gestaltATSUUpdate6 */)
    always_use_atsu = 0;
}

double wxDrawUnicodeText(const char *text, int d, int theStrlen, int ucs4, Bool qd_spacing, int smoothing, double angle,
			 double scale_x, double scale_y, int use_start, double start_x, double start_y, double ddx, double ddy,
			 int is_sym)
{
  int i;
  int again = 0;
  double pen_delta = 0.0;
  int move_pen_at_end;

  if (theStrlen < 0) {
    if (ucs4) {
      int *t2 = (int *)text;
      for (theStrlen = d; t2[theStrlen]; theStrlen++) {
      }
      theStrlen -= d;
    } else
      theStrlen = strlen(text XFORM_OK_PLUS d);
  }

  move_pen_at_end = qd_spacing && ALWAYS_USE_ATSU && !use_start;

  while (theStrlen) {
    /* Check whether we need to go into Unicode mode: */
    if (!qd_spacing || ALWAYS_USE_ATSU || ucs4) {
      i = 0;
    } else if (is_sym == wxSYMBOL)
      /* Symbol font hack: don't convert */
      i = theStrlen;
    else {
      for (i = 0; i < theStrlen; i++) {
	if (((unsigned char *)text)[i + d] > 127)
	  break;
      }
    }

    if (i) {
      /* Up to i, it's all ASCII, where MacRoman == UTF-8 */
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

      ::DrawText(text XFORM_OK_PLUS d, 0, i);

      if (reset_size)
	::TextSize(reset_size);

      ::GetPen(&pen_end);
      pen_delta += (pen_end.h - pen_start.h);

      d += i;
      theStrlen -= i;
    }

    if (theStrlen) {
      int amt;

      if (!qd_spacing || ALWAYS_USE_ATSU || ucs4)
	amt = theStrlen;
      else
	amt = 1;

      pen_delta += DrawMeasUnicodeText(text, d, amt, ucs4, 0, 0, 0, 0, 0, again, 
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

void wxGetUnicodeTextWidth(const char *text, int d, int theStrlen, 
			   short txFont, short txSize, short txFace,
			   int ucs4, double scale_y,
			   double* x, double* y,
			   double* descent, double* externalLeading,
			   Bool qd_spacing, double scale_x,
			   int is_sym)
{
  FontInfo fontInfo;
  const char *meas = NULL;
  int i;

  if (text) {
    if (theStrlen < 0) {
      if (ucs4) {
	int *t2 = (int *)text;
	for (theStrlen = d; t2[theStrlen]; theStrlen++) {
	}
	theStrlen -= d;
      } else
	theStrlen = strlen(text XFORM_OK_PLUS d);
    }

    if (!qd_spacing || ALWAYS_USE_ATSU || ucs4) {
      i = 0;
    } else {
      if (is_sym != wxSYMBOL) {
	/* Check whether we need to go into Unicode mode to get UTF-8 output: */
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
  {
    double dx;
    dx = wxTextFontInfo(txFont, txSize, txFace,
			&fontInfo, (char *)meas, 
			d, theStrlen);
    *x = dx;
  }

  if (meas) {
    /* it's all ASCII, where MacRoman == UTF-8 */
    /* so *x is right */
  } else if (text) {
    if (!qd_spacing || ALWAYS_USE_ATSU || ucs4) {
      double dx;
      dx = DrawMeasUnicodeText(text, d, theStrlen, ucs4,
			       1, 1, 
			       txFont, txSize, txFace,
			       0, qd_spacing, wxSMOOTHING_DEFAULT, 0.0, is_sym,
			       scale_x, scale_y,
			       0.0, 0, 0.0, 0.0, 0.0, 0.0, 0);
      *x = dx;
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
	  (*x) += wxTextFontInfo(txFont, txSize, txFace,
				 &fontInfo, 
				 (char *)text, d, i);
	  d += i;
	  theStrlen -= i;
	}

	/* Measure one Latin-1 part: */
	if (theStrlen) {
	  int amt;

	  amt = 1;
      
	  (*x) += DrawMeasUnicodeText(text, d, amt, ucs4,
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

Bool wxGetUnicodeGlyphAvailable(int c, 
				short txFont, short txSize, short txFace,
				int is_sym)
{
  ATSUTextLayout layout;
  UniChar uc[1];
  UniCharArrayOffset ulen = 1, changed;
  ATSUFontID fontid;
  UniCharCount changedLen;
  OSStatus r;

  if (!theATSUstyle)
    init_ATSU_style();

  if (c > 0xFFFF)
    return FALSE;

  atsuSetStyleFromGrafPtrParams(theATSUstyle, txFont, txSize, txFace, 1, 0.0, 1.0, 1);

  uc[0] = c;
  ATSUCreateTextLayoutWithTextPtr((UniCharArrayPtr)uc,
				  kATSUFromTextBeginning,
				  kATSUToTextEnd,
				  ulen,
				  1,
				  &ulen,
				  &theATSUstyle,
				  &layout);

  
  r = ATSUMatchFontsToText (layout,
			    kATSUFromTextBeginning,
			    kATSUToTextEnd,
			    &fontid,
			    &changed,
			    &changedLen);

  ATSUDisposeTextLayout(layout);
  
  return (r != kATSUFontsNotMatched);
}


#define QUICK_UBUF_SIZE 512
static UniChar u_buf[QUICK_UBUF_SIZE];
static double widths_buf[QUICK_UBUF_SIZE];

#if 0
static long time_preprocess, time_ctx, time_style, time_layout, time_measure, time_draw;
static long time_counter, time_start;
#define START_TIME time_start = scheme_get_process_milliseconds()
#define END_TIME(x) time_ ## x += (scheme_get_process_milliseconds() - time_start)
#else
#define START_TIME /* empty */
#define END_TIME(x) /* empty */
#endif

static double DrawMeasUnicodeText(const char *text, int d, int theStrlen, int ucs4,
				  int just_meas, int given_font, 
				  short txFont, short txSize, short txFace,
				  int again, int qd_spacing, int smoothing,
				  double angle, int is_sym,
				  double scale_x, double scale_y,
				  double pen_delta, int use_pen_delta,
				  double start_x, double start_y, double ddx, double ddy, int with_start)
{
  ATSUTextLayout layout = NULL;
  UniCharCount ulen, one_ulen, delta;
  UniChar *unicode;
  double result = 0, one_res = 0;
  int need_convert, need_layout, need_size, textMode = 0;
  Scheme_Object *val;
#define JUSTDELTA(v, s, d) (need_convert ? v : ((v - d) / s))
#define COORDCONV(v, s, d) (need_convert ? ((v * s) + d) : v)
  CGrafPtr qdp;
  CGContextRef cgctx;
  Rect portRect;
  RGBColor eraseColor;
  ATSUStyle style;
  Point start;
  RgnHandle clipRgn;
  double *widths;
  int use_cgctx = (always_use_atsu 
		   && ((smoothing != wxSMOOTHING_PARTIAL) || (scale_x != scale_y)));
  FontInfo fontInfo;
	
  if (!theATSUstyle)
    init_ATSU_style();

  style = (qd_spacing ? theATSUqdstyle : theATSUstyle);

  START_TIME;

  /****************************************/
  /* Unicode conversion                  */

  if (ucs4) {
    int i, extra;
    unsigned int v;
    UniCharCount alloc_ulen;

    /* Count characters that fall outside UCS-2: */
    for (i = 0, extra = 0; i < theStrlen; i++) {
      if (((unsigned int *)text)[d+i] > 0xFFFF)
	extra++;
    }

    ulen = theStrlen + extra;
    alloc_ulen = ulen;
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = new WXGC_ATOMIC UniChar[alloc_ulen];
    else
      unicode = u_buf;
    
    /* UCS-4 -> UTF-16 conversion */
    for (i = 0, extra = 0; i < theStrlen; i++) {
      v = ((unsigned int *)text)[d+i];
      if (v > 0xFFFF) {
	v -= 0x10000;
	unicode[i+extra] = 0xD800 | ((v >> 10) & 0x3FF);
	extra++;
	unicode[i+extra] = 0xDC00 | (v & 0x3FF);
      } else
	unicode[i+extra] = v;
    }
  } else {
    UniCharCount alloc_ulen;

    /* UTF-8 -> UTF-16 conversion */
    ulen = scheme_utf8_decode((unsigned char *)text, d, 
			      theStrlen, NULL, 0, -1, 
			      NULL, 1 /*UTF-16*/, '?');
    alloc_ulen = ulen;
    if (alloc_ulen > QUICK_UBUF_SIZE)
      unicode = new WXGC_ATOMIC UniChar[alloc_ulen];
    else
      unicode = u_buf;
    ulen = scheme_utf8_decode((unsigned char *)text, d, theStrlen, 
			      (unsigned int *)unicode, 0, -1, 
			      NULL, 1 /*UTF-16*/, '?');
  }

  if (is_sym == wxSYMBOL) {
    unsigned int i;
    int v, m;
    for (i = 0; i < ulen; i++) {
      v = unicode[d+i];
      if (v < 256) {
	m = symbol_map[v];
	unicode[i] = (m ? m : v);
      }
    }
  }

  END_TIME(preprocess);
  START_TIME;

  GetPort(&qdp);

  /****************************************/
  /* Set up measure cache                 */

  if (!width_table) {
    char *s;

    wxREGGLOB(width_table);
    wxREGGLOB(old_width_table);
    wxREGGLOB(table_key);

    width_table = scheme_make_hash_table_equal();
    old_width_table = scheme_make_hash_table_equal();
    s = new WXGC_ATOMIC char[sizeof(wxKey)];
    memset(s, 0, sizeof(wxKey));
    table_key = scheme_make_sized_byte_string(s, sizeof(wxKey), 0);
  }
  if (!given_font && qd_spacing) {
    txFont = GetPortTextFont(qdp);
    txSize = GetPortTextSize(qdp);
    txFace = GetPortTextFace(qdp);
  }
  if (qd_spacing) {
    wxKey *k = (wxKey *)SCHEME_BYTE_STR_VAL(table_key);
    k->scale_x = scale_x;
    k->scale_y = scale_y;
    k->txFont = txFont;
    k->txSize = txSize;
    k->txFace = txFace;
    k->use_cgctx = (char)use_cgctx;
    k->smoothing = (char)smoothing;
  }

  if (qd_spacing) {
    /* Get all cached sizes */
    double r = 0;
    int i, all = 1;
    atomic_timeout_t old;

    if (ulen > QUICK_UBUF_SIZE)
      widths = new WXGC_ATOMIC double[ulen];
    else
      widths = widths_buf;

    old = scheme_on_atomic_timeout;
    scheme_on_atomic_timeout = NULL;
    scheme_start_atomic();
    scheme_current_thread->suspend_break++;

    for (i = 0; i < (int)ulen; i++) {
      ((wxKey *)SCHEME_BYTE_STR_VAL(table_key))->code = unicode[i];
      val = scheme_hash_get(width_table, table_key);
      if (!val) {
	val = scheme_hash_get(old_width_table, table_key);
	if (val) {
	  /* Move it to the new table, so we find it faster, and
	     so it's kept on the next rotation: */
	  Scheme_Object *new_key;
	  new_key = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(table_key), sizeof(wxKey), 1);
	  scheme_hash_set(width_table, new_key, val);
	}
      }
      if (!val) {
	all = 0;
	widths[i] = -1;
      } else {
	widths[i] = SCHEME_DBL_VAL(val);
	r += widths[i];
      }
    }

    --scheme_current_thread->suspend_break;
    scheme_end_atomic_no_swap();
    scheme_on_atomic_timeout = old;

    if (all && just_meas)
      return r;
  } else
    widths = NULL;

  END_TIME(cache);
  START_TIME;
  
  /****************************************/
  /* Set up style                         */

  if (given_font)
    atsuSetStyleFromGrafPtrParams(style, txFont, txSize, txFace, smoothing, 
				  angle, (use_cgctx || just_meas) ? 1.0 : scale_y,
				  qd_spacing);
  else
    atsuSetStyleFromGrafPtr(style, smoothing, angle, 
			    (use_cgctx || just_meas) ? 1.0 : scale_y,
			    qd_spacing);

  END_TIME(style);
  START_TIME;
  
  /****************************************/
  /* Set up port                          */

  if (!just_meas) {
    GetPortBounds(qdp, &portRect); 
    GetBackColor(&eraseColor);
    textMode = GetPortTextMode(qdp);

    if (!with_start) {
      GetPen(&start);
      start_x = start.h;
      start_y = start.v;
    }

    if ((angle == 0.0) && (textMode == srcCopy)) {
      ::GetFontInfo(&fontInfo);
    }

    if (use_cgctx) {
      /* Make clipping regions match (including BeginUpdate effect) */
      clipRgn = NewRgn();
      if (clipRgn) {
	RgnHandle visRgn;
	visRgn = NewRgn();
	if (visRgn) {
	  GetPortClipRegion(qdp, clipRgn);
	  GetPortVisibleRegion(qdp, visRgn);
	  SectRgn(clipRgn, visRgn, clipRgn);
	  DisposeRgn(visRgn);
	}
      }
    } else
      clipRgn = NULL;
  } else
    clipRgn = NULL;

  if (use_cgctx && QDBeginCGContext(qdp, &cgctx))
    use_cgctx = 0;

  if (use_cgctx && !just_meas) {
    SyncCGContextOriginWithPort(cgctx, qdp);
    if (clipRgn) {
      ClipCGContextToRegion(cgctx, &portRect, clipRgn);
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

  END_TIME(ctx);
  START_TIME;

  /****************************************/
  /* Draw/measure loop                    */
  
  /* Beware of GCing without adjusting the text layout, because the  */
  /* unicode string could move, and because GCing might attempt to   */
  /* draw a bitmap into the same port. */

  one_ulen = (qd_spacing ? 1 : ulen);

  delta = 0;
	
  while (1) {
    if (delta >= ulen)
      break;

    if (qd_spacing) {
      if (widths[delta] >= 0) {
	one_res = widths[delta];
	need_size = 0;
      } else {
	one_res = 0;
	need_size = 1;
      }
    } else {
      need_size = 1;
      one_res = 0;
    }

    need_layout = (!just_meas || need_size);
    if (need_layout) {
      if (!layout) {
	ATSUCreateTextLayoutWithTextPtr((UniCharArrayPtr)(unicode + delta),
					kATSUFromTextBeginning,
					kATSUToTextEnd,
					one_ulen,
					1,
					&one_ulen,
					&style,
					&layout);

	if (qd_spacing || use_cgctx) {
	  int cnt = 0;
	  GC_CAN_IGNORE ATSUAttributeTag ll_theTags[2];
	  GC_CAN_IGNORE ByteCount ll_theSizes[2];
	  ATSUAttributeValuePtr ll_theValues[2];
	  ATSLineLayoutOptions ll_attribs;

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
	    ll_theTags[cnt] = kATSULineLayoutOptionsTag;
	    ll_theSizes[cnt] = sizeof(ATSLineLayoutOptions);
	    ll_theValues[cnt] = &ll_attribs;
	    cnt++;
	  }

	  if (use_cgctx) {
	    ll_theTags[cnt] = kATSUCGContextTag;
	    ll_theSizes[cnt] = sizeof(CGContextRef);
	    ll_theValues[cnt] =  &cgctx;
	    cnt++;
	  }
    
	  ATSUSetLayoutControls(layout, cnt, ll_theTags, ll_theSizes, ll_theValues);
	}

	ATSUSetTransientFontMatching(layout, TRUE);
      
	if (angle != 0.0) {
	  GC_CAN_IGNORE ATSUAttributeTag  r_theTags[] = { kATSULineRotationTag };
	  GC_CAN_IGNORE ByteCount    r_theSizes[] = { sizeof(Fixed) };
	  ATSUAttributeValuePtr r_theValues[1];
	  Fixed deg_angle;
	
	  deg_angle = DoubleToFixed(angle * 180 / 3.14159);
	  r_theValues[0] = &deg_angle;
	  ATSUSetLayoutControls(layout, 1, r_theTags, r_theSizes, r_theValues); 
	}
      } else {
	ATSUSetTextPointerLocation(layout, 
				   (UniCharArrayPtr)(unicode + delta),
				   kATSUFromTextBeginning,
				   kATSUToTextEnd,
				   one_ulen);
      }
    }
      
    END_TIME(layout);
    START_TIME;

    if (need_size) {
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
    
      one_res = (Fix2X(bounds.upperRight.x) - Fix2X(bounds.upperLeft.x));
      if (one_res < 0)
	one_res = 0;

      if (qd_spacing)
	widths[delta] = one_res;
    } else if (qd_spacing)
      widths[delta] = -1.0; /* inidicates that we don't need to re-hash */
    if (!use_cgctx && !just_meas) {
      one_res = one_res / scale_y;
    }
    result += one_res;

    END_TIME(measure);
    START_TIME;

    if (!just_meas) {
      if (!with_start) {
	ddx = 0;
	ddy = 0;
	need_convert = 0;
      } else {
	need_convert = 1;
      }
    
      if ((angle == 0.0) && (textMode == srcCopy)) {
	if (use_cgctx) {
	  CGRect cgr;
	  double rt, rl, rr, rb;

	  rl = JUSTDELTA(start_x, scale_x, ddx) + (use_pen_delta ? pen_delta : 0.0);
	  rt = JUSTDELTA(start_y, scale_y, ddy) - fontInfo.ascent;
	  rb = JUSTDELTA(start_y, scale_y, ddy) + fontInfo.descent;
	  rr = rl + one_res;

	  cgr.origin.x = rl;
	  cgr.origin.y = -rb;
	  cgr.size.width = rr - rl;
	  cgr.size.height = rb - rt;

	  CGContextSetRGBFillColor(cgctx, 
				   (double)eraseColor.red / 65535.0,
				   (double)eraseColor.green / 65535.0,
				   (double)eraseColor.blue / 65535.0,
				   1.0);
	  CGContextFillRect(cgctx, cgr);
	} else {
	  Rect theRect;
	  double rt, rl, rr, rb;
	  
	  rl = COORDCONV(start_x, scale_x, ddx) + (use_pen_delta ? (pen_delta * scale_x) : 0.0);
	  rt = COORDCONV(start_y, scale_y, ddy) - (fontInfo.ascent * scale_y);
	  rb = COORDCONV(start_y, scale_y, ddy) + (fontInfo.descent * scale_y);
	  rr = rl + (one_res * scale_x);

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
	  double isx;
	  isx = JUSTDELTA(start_x, scale_x, ddx) + (use_pen_delta ? pen_delta : 0.0);
	  sx = DoubleToFixed(isx);
	} else if (with_start) {
	  double isx;
	  isx = COORDCONV(start_x, scale_x, ddx) + (use_pen_delta ? (pen_delta * scale_x) : 0.0);
	  sx = DoubleToFixed(isx);
	} else {
	  sx = kATSUUseGrafPortPenLoc;
	}

	if (use_cgctx) {
	  double isy;
	  isy = -JUSTDELTA(start_y, scale_y, ddy);
	  sy = DoubleToFixed(isy);
	} else if (with_start) {
	  double isy;
	  isy = COORDCONV(start_y, scale_y, ddy);
	  sy = DoubleToFixed(isy);
	} else {
	  sy = kATSUUseGrafPortPenLoc;
	}

	ATSUDrawText(layout, 
		     kATSUFromTextBeginning,
		     kATSUToTextEnd,
		     sx, sy);
      }
    }

    if (!with_start) {
      /* Make sure start is scaled for further iterations */
      start_x = scale_x * start_x;
      start_y = scale_y * start_y;
      with_start = 1;
    }
    if (angle == 0.0) {
      start_x += one_res;
    } else {
      start_x += one_res * cos(angle);
      start_y += one_res * sin(angle);
    }

    delta += one_ulen;
  }

  if (layout)
    ATSUDisposeTextLayout(layout);

  if (!just_meas) {
    if (use_cgctx) {
      /* I don't think this flush is supposed to be
	 necessary. However, sometimes text gets lost in the draw.ss
	 test without it. (Notably, text gets lost only when drawing
	 directly to the screen, and not when drawing to a bitmap. */
      CGContextSynchronize(cgctx);

      QDEndCGContext(qdp, &cgctx);
    }

    /* QuickDraw is back again: */
    if (!just_meas && !use_pen_delta && !with_start)
      MoveTo(start.h + (int)floor(result * scale_x), start.v);
  } else {
    if (use_cgctx) {
      QDEndCGContext(qdp, &cgctx);
    }
  }

  if (qd_spacing) {
    /* Record collected widths. (We can't record these during the
       drawing loop because it might trigger a GC, which might try to
       draw a GC bitmap, etc. */
    int j;
    atomic_timeout_t old;

    old = scheme_on_atomic_timeout;
    scheme_on_atomic_timeout = NULL;
    scheme_start_atomic();
    scheme_current_thread->suspend_break++;

    for (j = 0; j < (int)ulen; j++) {
      if (widths[j] >= 0) {
	val = scheme_make_sized_byte_string(SCHEME_BYTE_STR_VAL(table_key), sizeof(wxKey), 1);
	((wxKey *)SCHEME_BYTE_STR_VAL(val))->code = unicode[j];
	scheme_hash_set(width_table, val, scheme_make_double(widths[j]));
	if (width_table->mcount >= MAX_WIDTH_MAPPINGS) {
	  /* rotate tables, so width_table doesn't grow indefinitely,
	     but we also don't throw away recent information completely */
	  old_width_table = width_table;
	  width_table = scheme_make_hash_table_equal();
	}
      }
    }

    --scheme_current_thread->suspend_break;
    scheme_end_atomic_no_swap();
    scheme_on_atomic_timeout = old;
  }

  END_TIME(draw);
  
#if 0
  if (!((time_counter++) & 0xFF)) {
    printf("---%ld\npre %ld\nctx %ld\nstyle %ld\nlayout %ld\nmeasure %ld\ndraw %ld\n",
	   time_counter,
	   time_preprocess, time_ctx, time_style, time_layout, time_measure, time_draw);
  }
#endif

  return result;
}

/************************************************************************/
/************************************************************************/

static void init_ATSU_style(void) 
{
  /* For some reason, toggling kAllTypographicFeaturesType makes
     text drawing slower and slower. So we have separate styles,
     one with typographic features and one without. */
  ATSUFontFeatureType types[1];
  ATSUFontFeatureSelector sels[1];
  
  ATSUCreateStyle(&theATSUstyle);
  ATSUCreateStyle(&theATSUqdstyle);
  
  types[0] = kAllTypographicFeaturesType;
  sels[0] = 0;
  ATSUSetFontFeatures(theATSUstyle, 1, types, sels);
  sels[0] = 1;
  ATSUSetFontFeatures(theATSUqdstyle, 1, types, sels); 
}


/* The following code comes from an Apple example: */


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
			       double angle, double scale_y, int qd_spacing)
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
					       kATSUStyleRenderingOptionsTag,
                                               };
 GC_CAN_IGNORE ByteCount    theSizes[] = { sizeof(ATSUFontID),
					   sizeof(Fixed),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(Boolean),
					   sizeof(RGBColor),
					   sizeof(ATSStyleRenderingOptions),
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

 if (smoothing == wxSMOOTHING_OFF)
   options = kATSStyleNoAntiAliasing;
 else if (smoothing == wxSMOOTHING_ON)
   options = kATSStyleApplyAntiAliasing;

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
atsuSetStyleFromGrafPtr(ATSUStyle iStyle, int smoothing, double angle, double scale_y, int qd_spacing)
{
 short    txFont, txSize;
 SInt16   txFace;
 GrafPtr iGrafPtr;

 GetPort( &iGrafPtr );

 txFont = GetPortTextFont(iGrafPtr);
 txSize = GetPortTextSize(iGrafPtr);
 txFace = GetPortTextFace(iGrafPtr);
 
 return atsuSetStyleFromGrafPtrParams(iStyle, txFont, txSize, txFace, smoothing, angle, scale_y, qd_spacing);
}
