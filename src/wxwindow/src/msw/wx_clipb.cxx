/*
 * File:	wx_clipb.cc
 * Purpose:	Clipboard implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

#include "wx_clipb.h"
#include "wx_mf.h"

Bool wxClipboardIsOpen = FALSE;

Bool wxOpenClipboard(void)
{
  if (wxTheApp->wx_frame && !wxClipboardIsOpen) {
    wxClipboardIsOpen = (Bool)::OpenClipboard(((wxWnd *)wxTheApp->wx_frame->handle)->handle);
    return wxClipboardIsOpen;
  } else
    return FALSE;
}

Bool wxCloseClipboard(void)
{
  if (wxClipboardIsOpen)
    wxClipboardIsOpen = FALSE;
  return (Bool)::CloseClipboard();
}

Bool wxEmptyClipboard(void)
{
  return (Bool)::EmptyClipboard();
}

Bool wxIsClipboardFormatAvailable(int dataFormat)
{
  return ::IsClipboardFormatAvailable(dataFormat);
}

Bool wxSetClipboardData(int dataFormat, wxObject *obj, int width, int height)
{
  switch (dataFormat)
  {
    case wxCF_BITMAP:
    {
      wxBitmap *wxBM = (wxBitmap *)obj;

      HDC hdcMem = CreateCompatibleDC(NULL);
      HDC hdcSrc = CreateCompatibleDC(NULL);
      HBITMAP old = (HBITMAP)::SelectObject(hdcSrc, wxBM->ms_bitmap);
      HPALETTE origPal = NULL;

      wxColourMap *cm = wxBM->GetColourMap();
      if (cm && cm->ms_palette)
	origPal = SelectPalette(hdcSrc, cm->ms_palette, TRUE);

      HBITMAP hBitmap;
      BITMAP bm;

      GetObject(wxBM->ms_bitmap, sizeof(BITMAP), (LPSTR)&bm);
      hBitmap = CreateBitmapIndirect(&bm);

      if (!hBitmap)
        return FALSE;
      HBITMAP old1 = (HBITMAP)SelectObject(hdcMem, hBitmap);
      BitBlt(hdcMem, 0, 0, wxBM->GetWidth(), wxBM->GetHeight(),
             hdcSrc, 0, 0, SRCCOPY);

      // Select new bitmap out of memory DC
      SelectObject(hdcMem, old1);

      // Set the data
      Bool success = (Bool)::SetClipboardData(CF_BITMAP, hBitmap);

      // Clean up
      if (origPal)
	SelectPalette(hdcSrc, origPal, TRUE);
      SelectObject(hdcSrc, old);
      DeleteDC(hdcSrc);
      DeleteDC(hdcMem);      
      return success;
      break;
    }
#if USE_METAFILE
    case wxCF_METAFILE:
    {
      wxMetaFile *wxMF = (wxMetaFile *)obj;
      HANDLE data = GlobalAlloc(GHND, sizeof(METAFILEPICT) + 1);
      METAFILEPICT *mf = (METAFILEPICT *)GlobalLock(data);

      mf->mm = MM_TEXT;
      mf->xExt = width;
      mf->yExt = height;
      mf->hMF = (HMETAFILE)wxMF->metafile;
      GlobalUnlock(data);
      wxMF->metafile = NULL;

      return (Bool)SetClipboardData(CF_METAFILEPICT, data);
      break;
    }
#endif
    case CF_SYLK:
    case CF_DIF:
    case CF_TIFF:
    case CF_PALETTE:
    case wxCF_DIB:
    {
      return FALSE;
      break;
    }
    case wxCF_OEMTEXT:
      dataFormat = wxCF_TEXT;
    case wxCF_TEXT:
       width = strlen((char *)obj) + 1;
       height = 1;
    default:
    {
      char *s = (char *)obj;
      DWORD l;

      l = (width * height);
      HANDLE hGlobalMemory = GlobalAlloc(GHND, l);
      if (!hGlobalMemory)
        return FALSE;

      LPSTR lpGlobalMemory = (LPSTR)GlobalLock(hGlobalMemory);

      memcpy(lpGlobalMemory, s, l);

      GlobalUnlock(hGlobalMemory);
      HANDLE success = SetClipboardData(dataFormat, hGlobalMemory);
      return (Bool)success;
      break;
    }
  }
  return FALSE;
}

wxObject *wxGetClipboardData(int dataFormat, long *len)
{
  switch (dataFormat)
  {
    case wxCF_BITMAP:
    {
      BITMAP bm;
      HBITMAP hBitmap = (HBITMAP)GetClipboardData(CF_BITMAP);
      if (!hBitmap)
        return NULL;

      HDC hdcMem = CreateCompatibleDC(NULL);
      HDC hdcSrc = CreateCompatibleDC(NULL);

      HBITMAP old = (HBITMAP)::SelectObject(hdcSrc, hBitmap);
      GetObject(hBitmap, sizeof(BITMAP), (LPSTR)&bm);

      HBITMAP hNewBitmap = CreateBitmapIndirect(&bm);

      if (!hNewBitmap)
        return NULL;
        
      HBITMAP old1 = (HBITMAP)SelectObject(hdcMem, hNewBitmap);
      BitBlt(hdcMem, 0, 0, bm.bmWidth, bm.bmHeight,
             hdcSrc, 0, 0, SRCCOPY);

      // Select new bitmap out of memory DC
      SelectObject(hdcMem, old1);

      // Clean up
      SelectObject(hdcSrc, old);
      DeleteDC(hdcSrc);
      DeleteDC(hdcMem);

      // Create a new wxBitmap
      wxBitmap *wxBM = new wxBitmap;
      wxBM->ms_bitmap = hNewBitmap;
      wxBM->SetWidth(bm.bmWidth);
      wxBM->SetHeight(bm.bmHeight);
      wxBM->SetDepth(-1);
      wxBM->SetOk(TRUE);

      return wxBM;
      break;
    }
    case wxCF_METAFILE:
    case CF_SYLK:
    case CF_DIF:
    case CF_TIFF:
    case CF_PALETTE:
    case wxCF_DIB:
    {
      return FALSE;
      break;
    }
    case wxCF_OEMTEXT:
      dataFormat = wxCF_TEXT;
    case wxCF_TEXT:
    default:
    {
      HANDLE hGlobalMemory = GetClipboardData(dataFormat);
      if (!hGlobalMemory)
        return NULL;

      int hsize = (int)GlobalSize(hGlobalMemory);
      if (len)
        *len = hsize;

      char *s = new char[hsize];
      if (!s)
        return NULL;

      LPSTR lpGlobalMemory = (LPSTR)GlobalLock(hGlobalMemory);
      memcpy(s, lpGlobalMemory, GlobalSize(hGlobalMemory));

      GlobalUnlock(hGlobalMemory);

      return (wxObject *)s;
      break;
    }
  }
  return NULL;
}

int  wxEnumClipboardFormats(int dataFormat)
{
  return ::EnumClipboardFormats(dataFormat);
}

int  wxRegisterClipboardFormat(char *formatName)
{
  return ::RegisterClipboardFormat(formatName);
}

Bool wxGetClipboardFormatName(int dataFormat, char *formatName, int maxCount)
{
  return (::GetClipboardFormatName(dataFormat, formatName, maxCount) > 0);
}

/*
 * Generalized clipboard implementation by Matthew Flatt
 */

wxClipboard *wxTheClipboard = NULL;

void wxInitClipboard(void)
{
  if (!wxTheClipboard) {
    wxREGGLOB(wxTheClipboard);
    wxTheClipboard = new wxClipboard;
  }
}

 wxClipboardClient::wxClipboardClient()
{
  formats = new wxStringList;
}

wxClipboard::wxClipboard()
{
  clipOwner = NULL;
  cbString = NULL;
}

wxClipboard::~wxClipboard()
{
  if (clipOwner)
    clipOwner->BeingReplaced();
  if (cbString)
    delete[] cbString;
}

static int FormatStringToID(char *str)
{
  if (!strcmp(str, "TEXT"))
    return wxCF_TEXT;
  
  return wxRegisterClipboardFormat(str);
}

void wxClipboard::SetClipboardClient(wxClipboardClient *client, long time)
{
  Bool got_selection;

  if (clipOwner)
    clipOwner->BeingReplaced();
  clipOwner = client;
  if (cbString) {
    delete[] cbString;
    cbString = NULL;
  }

  if (wxOpenClipboard()) {
    char **formats, *data;
    int i, count;
    int ftype;
    long size;

    wxEmptyClipboard();

    formats = clipOwner->formats->ListToArray(FALSE);
	count = clipOwner->formats->Number();
    for (i = 0; i < count; i++) {
      ftype = FormatStringToID(formats[i]);
      data = clipOwner->GetData(formats[i], &size);
      if (!wxSetClipboardData(ftype, (wxObject *)data, size, 1)) {
	got_selection = FALSE;
	break;
      }
    }

    if (i >= count)
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;
  
  got_selection = FALSE; // Assume another process takes over

  if (!got_selection) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
}

wxClipboardClient *wxClipboard::GetClipboardClient()
{
  return clipOwner;
}

void wxClipboard::SetClipboardString(char *str, long time)
{
  Bool got_selection;

  if (clipOwner) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
  if (cbString)
    delete[] cbString;

  cbString = str;

  if (wxOpenClipboard()) {
    wxEmptyClipboard();
    if (!wxSetClipboardData(wxCF_TEXT, (wxObject *)str))
      got_selection = FALSE;
    else
      got_selection = wxCloseClipboard();
  } else
    got_selection = FALSE;

  got_selection = FALSE; // Assume another process takes over

  if (!got_selection) {
    delete[] cbString;
    cbString = NULL;
  }
}

char *wxClipboard::GetClipboardString(long time)
{
  char *str;
  long length;

  str = GetClipboardData("TEXT", &length, time);
  if (!str) {
    str = new char[1];
    *str = 0;
  }

  return str;
}

void wxClipboard::SetClipboardBitmap(wxBitmap *bm, long time)
{
  if (clipOwner) {
    clipOwner->BeingReplaced();
    clipOwner = NULL;
  }
  if (cbString) {
    delete[] cbString;
    cbString = NULL;
  }
  
  if (wxOpenClipboard()) {
    wxEmptyClipboard();
    wxSetClipboardData(wxCF_BITMAP, bm, 0, 0);
    wxCloseClipboard();
  }
}

wxBitmap *wxClipboard::GetClipboardBitmap(long time)
{
  wxBitmap *bm;

  if (clipOwner || cbString)
    return NULL;

  if (wxOpenClipboard()) {
    bm = (wxBitmap *)wxGetClipboardData(wxCF_BITMAP, NULL);
    wxCloseClipboard();
  } else
    bm = NULL;
  
  return bm;
}

char *wxClipboard::GetClipboardData(char *format, long *length, long time)
{
  if (clipOwner)  {
    if (clipOwner->formats->Member(format))
      return clipOwner->GetData(format, length);
    else
      return NULL;
  } else if (cbString) {
    if (!strcmp(format, "TEXT"))
      return copystring(cbString);
    else
      return NULL;
  } else {
    if (wxOpenClipboard()) {
      receivedString = (char *)wxGetClipboardData(FormatStringToID(format), 
						  length);
      wxCloseClipboard();
    } else
      receivedString = NULL;

    return receivedString;
  }
}
