
/*
 * File:	wb_mf.cc
 * Purpose:	Metafiles and metafile DCs
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#include "wx.h"

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
