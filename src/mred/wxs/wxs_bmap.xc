
@INCLUDE prefix.xci

#include "wx_gdi.h"

@INCLUDE wxs.xci

@HEADER

@INCLUDE wxs_bmt.xci

@CLASSBASE wxBitmap "bitmap" : "object"

@SET TYPE = char
@SET NOTEST = 1
@INCLUDE list.xci

@MACRO ZEROERR[p.woh] = if ((x<p> < 1) || (x<p> > 100000)) scheme_signal_error("%s%d",METHODNAME("bitmap%","initialization")": bad " <woh> ": ", x<p>);

@MACRO NONZERODEPTH = if (x3 != 1) scheme_signal_error("%s: depth %d is illegal (only depth 1 is supported)", METHODNAME("bitmap%","initialization"), x3);
@MACRO LISTENOUGH = if (scheme_proper_list_length(p[0]) < (((x1 * x2) >> 3) * x3)) scheme_signal_error("%s", METHODNAME("bitmap%","initialization")": byte list too short");
	
@CREATOR (char[]/bList/ubList/cList,int,int,int=1); : : /ZEROERR[1."width"]|ZEROERR[2."height"]|NONZERODEPTH|LISTENOUGH|glueUncountedListSet[char.0.0.METHODNAME("bitmap%","initialization")]// <> character list
@CREATOR (int,int,int=-1); : : /ZEROERR[0."width"]|ZEROERR[1."height"] <> width/height
@CREATOR (pathname,SYM[bitmapType]=0); <> pathname

@ "get-depth" : int GetDepth();
@ "get-height" : int GetHeight();
@ "get-width" : int GetWidth();
@ "ok?" : bool Ok();

@ "load-file" : void LoadFile(pathname,SYM[bitmapType]=0);
@ "save-file" : void SaveFile(pathname,int);

@END

@CLASSBASE wxIcon "icon" : "bitmap"

@CREATOR (string, SYM[bitmapType]=0);

@END

