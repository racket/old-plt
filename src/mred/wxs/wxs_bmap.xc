
@INCLUDE prefix.xci

#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_bmt.xci

@BEGINSYMBOLS saveBitmapType > ONE
@SYM "bmp" : wxBITMAP_TYPE_BMP
@SYM "xbm" : wxBITMAP_TYPE_XBM
@SYM "xpm" : wxBITMAP_TYPE_XPM
@SYM "pict" : wxBITMAP_TYPE_PICT
@ENDSYMBOLS

static Bool IsColor(wxBitmap *bm)
{
  return (bm->GetDepth() == 1);
}

@CLASSBASE wxBitmap "bitmap" : "object"

@SET TYPE = char
@SET NOTEST = 1
@INCLUDE list.xci

@MACRO ZEROERR[p.woh] = if (!SCHEME_INTP(p[<p>]) || (SCHEME_INT_VAL(p[<p>]) < 1) || (SCHEME_INT_VAL(p[<p>]) > 10000)) scheme_wrong_type(METHODNAME("bitmap%","initialization"), "integer in [1,10000]", <p>, n, p);

@MACRO LISTENOUGH = if (scheme_proper_list_length(p[0]) < ((x1 * x2) >> 3)) scheme_arg_mismatch(METHODNAME("bitmap%","initialization"), "byte list string too short: ", p[0]);

@CREATOR (char[]/bList/ubList/cList,int,int); : : /ZEROERR[1."width"]|ZEROERR[2."height"]|LISTENOUGH|glueUncountedListSet[char.0.0.METHODNAME("bitmap%","initialization")]// <> character list
@CREATOR (int,int,bool=0); : : ZEROERR[0."width"]|ZEROERR[1."height"] <> width/height
@CREATOR (pathname,SYM[bitmapType]=0); <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();
@ m "is-color?" : bool IsColor();

@ "load-file" : bool LoadFile(pathname,SYM[bitmapType]=0);
@ "save-file" : bool SaveFile(pathname,SYM[saveBitmapType]);

@END
