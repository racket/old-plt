/*
 * This code was put in wx_gdi.cc to allow loading a DIB with
 * palette from a resource. Trouble is, the resulting bitmap
 * isn't necessarily compatible with the current display,
 * and so it will cause more trouble than it's worth.
 * JACS 29/11/94
 */
    HANDLE h = FindResource(wxhInstance, bitmap_file, RT_BITMAP);
    if (!h)
      return FALSE;

    HANDLE hRes = LoadResource(wxhInstance, h);

    LPBITMAPINFOHEADER lpBitmapInfo = (LPBITMAPINFOHEADER)LockResource(hRes);
    if (!lpBitmapInfo)
  	return FALSE;
    HPALETTE hPalette = 0;
    wxMakeBitmapAndPalette(lpBitmapInfo, &ms_bitmap, &hPalette);
    UnlockResource(hRes);
    FreeResource(hRes);

    if (ms_bitmap)
    {
      ok = TRUE;
      BITMAP bm;
      GetObject(ms_bitmap, sizeof(BITMAP), (LPSTR) &bm);
      width = bm.bmWidth;
      height = bm.bmHeight;
      depth = bm.bmPlanes;
      if (hPalette)
      {
        wxColourMap *cmap = new wxColourMap;
        cmap->ms_palette = hPalette;
        SetColourMap(cmap);
      }
      return TRUE;
    }
    else
    {
      if (hPalette)
        DeleteObject(hPalette);
    }

/****************************************************************************
 *									    *
 *  FUNCTION   : wxMakeDIBPalette(lpInfo)					    *
 *									    *
 *  PURPOSE    : Given a BITMAPINFOHEADER, create a palette based on 
 *		 the color table.
 *		 
 *									    *
 *  RETURNS    : non-zero - handle of a corresponding palette 
 *		 zero - unable to create palette
 *									    *
 ****************************************************************************/

HPALETTE wxMakeDIBPalette(LPBITMAPINFOHEADER lpInfo)
{
    NPLOGPALETTE npPal;
    RGBQUAD far *lpRGB;
    HPALETTE hLogPal; 
    WORD i;

    /* since biClrUsed field was filled during the loading of the DIB,
    ** we know it contains the number of colors in the color table.
    */
    if (lpInfo->biClrUsed)
    {
	npPal = (NPLOGPALETTE)LocalAlloc(LMEM_FIXED, sizeof(LOGPALETTE) + 
				(WORD)lpInfo->biClrUsed * sizeof(PALETTEENTRY));
	if (!npPal)
	    return(FALSE);

	npPal->palVersion = 0x300;
	npPal->palNumEntries = (WORD)lpInfo->biClrUsed;

	/* get pointer to the color table */
	lpRGB = (RGBQUAD FAR *)((LPSTR)lpInfo + lpInfo->biSize);

	/* copy colors from the color table to the LogPalette structure */
	for (i = 0; i < lpInfo->biClrUsed; i++, lpRGB++)
	{
	    npPal->palPalEntry[i].peRed = lpRGB->rgbRed;
	    npPal->palPalEntry[i].peGreen = lpRGB->rgbGreen;
	    npPal->palPalEntry[i].peBlue = lpRGB->rgbBlue;
	    npPal->palPalEntry[i].peFlags = 0;
	}

	hLogPal = CreatePalette((LPLOGPALETTE)npPal);
	LocalFree((HANDLE)npPal);
	return(hLogPal);
    }

    /* 24-bit DIB with no color table.  return default palette.  Another
    ** option would be to create a 256 color "rainbow" palette to provide
    ** some good color choices.
    */
    else
	return(GetStockObject(DEFAULT_PALETTE));
}

/****************************************************************************
 *
 *  FUNCTION   : wxMakeBitmapAndPalette
 *
 *  PURPOSE    : Given a DIB LPBITMAPINFOHEADER, creates a bitmap and corresponding palette
 *               to be used for a device-dependent representation of
 *               of the image.
 *
 *  RETURNS    : TRUE  --> success. phPal and phBitmap are filled with 
 *                         appropriate handles.  Caller is responsible
 *                         for freeing objects.
 *               FALSE --> unable to create objects.  both pointer are
 *                         not valid
 *
 ****************************************************************************/
Bool wxMakeBitmapAndPalette(LPBITMAPINFOHEADER lpInfo,
			HPALETTE * phPal, HBITMAP * phBitmap)
{
    
    BOOL result = FALSE;
    HBITMAP hBitmap;
    HPALETTE hPalette, hOldPal;
    LPSTR lpBits;

    HDC hDC = GetDC(NULL);
    
    if (hPalette = wxMakeDIBPalette(lpInfo))
    {
	// Need to realize palette for converting DIB to bitmap.
	hOldPal = SelectPalette(hDC, hPalette, TRUE);
	RealizePalette(hDC);

	lpBits = (LPSTR)lpInfo + (WORD)lpInfo->biSize + 
		(WORD)lpInfo->biClrUsed * sizeof(RGBQUAD);
	hBitmap = CreateDIBitmap(hDC, lpInfo, CBM_INIT, lpBits, 
				(LPBITMAPINFO)lpInfo, DIB_RGB_COLORS);

	SelectPalette(hDC, hOldPal, TRUE);
	RealizePalette(hDC);

	if (!hBitmap)
	    DeleteObject(hPalette);
	else
	{
	    *phBitmap = hBitmap;
	    *phPal = hPalette;
	    result = TRUE;
	}
    }
    ReleaseDC(NULL, hDC);
    return(result);
}


