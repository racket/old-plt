
@INCLUDE prefix.xci

#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_bmt.xci

@BEGINSYMBOLS saveBitmapType > ONE > PRED BUNDLE
@SYM "bmp" : wxBITMAP_TYPE_BMP
@SYM "xbm" : wxBITMAP_TYPE_XBM
@SYM "xpm" : wxBITMAP_TYPE_XPM
@SYM "jpeg" : wxBITMAP_TYPE_JPEG
@SYM "pict" : wxBITMAP_TYPE_PICT
@ENDSYMBOLS

static Bool IsColor(wxBitmap *bm)
{
  return (bm->GetDepth() != 1);
}

@CLASSBASE wxBitmap "bitmap" : "object"

@MACRO STRINGENOUGH = if (SCHEME_STRTAG_VAL(p[POFFSET]) < (((x1 * x2) + 7) >> 3)) WITH_VAR_STACK(scheme_arg_mismatch(METHODNAME("bitmap%","initialization"), "string too short: ", p[POFFSET]));

@MACRO USEALLFUEL[ok] = if (<ok>) WITH_VAR_STACK(scheme_thread_block(0.0));

@CREATOR (string////string,rint[1|10000],rint[1|10000]); : : /STRINGENOUGH// <> datastring
@CREATOR (rint[1|10000],rint[1|10000],bool=0); : : <> width/height
@CREATOR (pathname////string,SYM[bitmapType]=0); : : //USEALLFUEL[realobj->Ok()] <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();
@ m "is-color?" : bool IsColor();

@ "load-file" : bool LoadFile(pathname,SYM[bitmapType]=0);  : : //USEALLFUEL[r]
@ "save-file" : bool SaveFile(wpathname,SYM[saveBitmapType]);  : : //USEALLFUEL[1]

@ "get-loaded-mask" : wxBitmap! GetMask()
@ "set-loaded-mask" : void SetMask(wxBitmap!)

@END
