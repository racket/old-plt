
/*
 * File:	wb_mf.cc
 * Purpose:	Metafiles and metafile DCs
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_mf.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

/* static const char sccsid[] = "@(#)wb_mf.cc	1.2 5/9/94"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_setup.h"

#endif

#if USE_METAFILE
#include "wb_mf.h"
#include "wx_types.h"

/*
 * Metafiles - Windows 3.1 only
 * Currently, the only purpose for making a metafile is to put
 * it on the clipboard.
 */

wxbMetaFile::wxbMetaFile(void)
{
  __type = wxTYPE_METAFILE;
}

wxbMetaFile::~wxbMetaFile(void)
{
}

Bool wxbMetaFile::SetClipboard(int WXUNUSED(width), int WXUNUSED(height))
{
  return FALSE;
}

/*
 * Metafile device context
 *
 */

wxbMetaFileDC::wxbMetaFileDC(char *WXUNUSED(file))
{
  __type = wxTYPE_DC_METAFILE;
}

wxbMetaFileDC::~wxbMetaFileDC(void)
{
}

wxMetaFile *wxbMetaFileDC::Close(void)
{
  return NULL;
}

void wxbMetaFileDC::SetMapMode(int WXUNUSED(mode))
{
}

#endif // USE_METAFILE
