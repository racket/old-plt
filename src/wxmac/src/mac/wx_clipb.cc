/*
 * File:	wx_clipb.cc
 * Purpose:	Clipboard implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993-94, AIAI, University of Edinburgh. All Rights Reserved.
 */

static const char sccsid[] = "%W% %G%";

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wx_setup.h"

#if USE_CLIPBOARD
#include "wx_clipb.h"
#include "wx_list.h"
#include "wx_main.h"
#include "wx_utils.h"
#ifndef WX_CARBON
# include <Scrap.h>
# include <TextEdit.h>
#endif

static wxList *ClipboardFormats = NULL;

#define CUSTOM_ID_START 100

class ClipboardFormat : public wxObject
{
public:
  ClipboardFormat();

  int format;
  char *name;
};

ClipboardFormat::ClipboardFormat()
: wxObject(FALSE)
{
}

static void InitFormats()
{
  ClipboardFormat *cf;

  wxREGGLOB(ClipboardFormats);
  ClipboardFormats = new wxList;

  cf = new ClipboardFormat;
  cf->name = "TEXT";
  cf->format = wxCF_TEXT;

  ClipboardFormats->Append(cf);
}

Bool wxOpenClipboard(void)
{
  return TRUE;
}

Bool wxCloseClipboard(void)
{
  return TRUE;
}

Bool wxEmptyClipboard(void)
{
  ClearCurrentScrap();
  return true;
}

Bool wxIsClipboardFormatAvailable(int dataFormat)
{
  long format;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!wxGetClipboardFormatName(dataFormat, (char *)&format, 4))
    return FALSE;

  ScrapRef scrap;
  OSErr err;
  ScrapFlavorFlags dontcare;
  
  err = GetCurrentScrap(&scrap);
  return ((err != noErr)||(GetScrapFlavorFlags(scrap,format,&dontcare) != noErr));
}

Bool wxSetClipboardData(int dataFormat, wxObject *obj, int width, int height)
{
  long format, length;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!wxGetClipboardFormatName(dataFormat, (char *)&format, 4))
    return FALSE;

  if (format == 'TEXT') {
    length = strlen((char *)obj);
  } else {
    length = (long)width * height;
  }
  
  ScrapRef scrap;
  OSErr err;
  
  err = GetCurrentScrap(&scrap);
  if (err != noErr) {
    return FALSE;
  }
  err = PutScrapFlavor(scrap,format,kScrapFlavorMaskNone,length,(const void *)obj);
  return (err == noErr);
}

wxObject *wxGetClipboardData(int dataFormat, long *size)
{
  char *result;
  long format, length, got_length;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!wxGetClipboardFormatName(dataFormat, (char *)&format, 4))
    return NULL;

  ScrapRef scrap;
  OSErr err;
  
  err = GetCurrentScrap(&scrap);
  if (err != noErr) {
    return NULL;
  }    
  err = GetScrapFlavorSize(scrap,format,&length);
  if (err != noErr) {
    return NULL;
  }
  result = new WXGC_ATOMIC char[length + 1];
  got_length = length;
  err = GetScrapFlavorData(scrap,format,&got_length,result);
  if (err != noErr) {
    return NULL;
  } else if (got_length < length)
    length = got_length;

  if (format == 'TEXT')
    result[length] = 0;

  if (size)
    *size = length;

  return (wxObject *)result;
}

int  wxEnumClipboardFormats(int dataFormat)
{
  long format;
  wxNode *node;
  ClipboardFormat *cf;

  if (!ClipboardFormats)
    InitFormats();   

  if (!dataFormat)
    node = ClipboardFormats->First();
  else {
    for (node = ClipboardFormats->First(); node; node = node->Next()) {
      cf = (ClipboardFormat *)node->Data();
      if (cf->format == dataFormat) {
	node = node->Next();
	break;
      }
    }
  }

  for (; node; node = node->Next()) {
    cf = (ClipboardFormat *)node->Data();
    memcpy(&format, cf->name, 4);
#ifdef WX_CARBON
    ScrapRef scrap;
    OSErr err;
    ScrapFlavorFlags dontcare;
    
    err = GetCurrentScrap(&scrap);
    if ((err != noErr)||(GetScrapFlavorFlags(scrap,format,&dontcare) != noErr))
      return cf->format;
#else
    long offset;      
    if (GetScrap(NULL, format, &offset) > 0)
      return cf->format;
#endif
  }

  return 0;
}

int  wxRegisterClipboardFormat(char *formatName)
{
  wxNode *node;
  ClipboardFormat *cf;

  if (!ClipboardFormats)
    InitFormats();
  
  for (node = ClipboardFormats->First(); node; node = node->Next()) {
    cf = (ClipboardFormat *)node->Data();
    if (!strcmp(cf->name, formatName))
      return cf->format;
  }

  cf = new ClipboardFormat;

  cf->format = ClipboardFormats->Number() + CUSTOM_ID_START;
  cf->name = new char[strlen(formatName) + 1];
  strcpy(cf->name, formatName);

  ClipboardFormats->Append(cf);
  
  return cf->format;
}

Bool wxGetClipboardFormatName(int dataFormat, char *formatName, int maxCount)
{
  wxNode *node;
  ClipboardFormat *cf;

  if (dataFormat == wxCF_OEMTEXT)
    dataFormat = wxCF_TEXT;

  if (!ClipboardFormats)
    InitFormats();
  
  for (node = ClipboardFormats->First(); node; node = node->Next()) {
    cf = (ClipboardFormat *)node->Data();
    if (cf->format == dataFormat) {
      formatName[0] = cf->name[0];
      formatName[1] = cf->name[1];
      formatName[2] = cf->name[2];
      formatName[3] = cf->name[3];
      return TRUE;
    }
  }

  return FALSE;
}

/********************************************************************************/
/*                             Clipboard Classes                                */
/********************************************************************************/

wxClipboard *wxTheClipboard;

void wxInitClipboard(void)
{
  if (!wxTheClipboard)
    wxREGGLOB(wxTheClipboard);
  wxTheClipboard = new wxClipboard;
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
    cbString = NULL;
  }

  if (wxOpenClipboard()) {
    char **formats, *data;
    int i;
    int ftype;
    long size;

    wxEmptyClipboard();

    formats = clipOwner->formats->ListToArray(FALSE);
    for (i = clipOwner->formats->Number(); i--; ) {
      ftype = FormatStringToID(formats[i]);
      data = clipOwner->GetData(formats[i], &size);
      if (!wxSetClipboardData(ftype, (wxObject *)data, size, 1)) {
	got_selection = FALSE;
	break;
      }
    }

    if (i < 0)
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
  cbString = NULL;
}

wxBitmap *wxClipboard::GetClipboardBitmap(long time)
{
  return NULL;
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


#endif
