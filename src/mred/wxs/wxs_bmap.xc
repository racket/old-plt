
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

@MACRO LISTENOUGH = if (scheme_proper_list_length(p[0]) < ((x1 * x2) >> 3)) scheme_arg_mismatch(METHODNAME("bitmap%","initialization"), "byte list string too short: ", p[0]);

@CREATOR (char[]/bList/ubList/cList,rint[1|10000],rint[1|10000]); : : /glueUncountedListSet[char.0.0.METHODNAME("bitmap%","initialization")]|LISTENOUGH// <> character list
@CREATOR (rint[1|10000],rint[1|10000],bool=0); : : <> width/height
@CREATOR (pathname,SYM[bitmapType]=0); <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();
@ m "is-color?" : bool IsColor();

@ "load-file" : bool LoadFile(pathname,SYM[bitmapType]=0);
@ "save-file" : bool SaveFile(pathname,SYM[saveBitmapType]);

@END
