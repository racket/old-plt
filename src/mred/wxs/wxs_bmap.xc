
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

@MACRO STRINGENOUGH = if (SCHEME_STRTAG_VAL(p[0]) < (((x1 * x2) + 7) >> 3)) scheme_arg_mismatch(METHODNAME("bitmap%","initialization"), "string too short: ", p[0]);

@CREATOR (string////string,rint[1|10000],rint[1|10000]); : : /STRINGENOUGH// <> string
@CREATOR (rint[1|10000],rint[1|10000],bool=0); : : <> width/height
@CREATOR (pathname////string,SYM[bitmapType]=0); <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();
@ m "is-color?" : bool IsColor();

@ "load-file" : bool LoadFile(pathname,SYM[bitmapType]=0);
@ "save-file" : bool SaveFile(pathname,SYM[saveBitmapType]);

@END
