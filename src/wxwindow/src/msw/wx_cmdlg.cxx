/*
 * File:	wx_cmdlg.cc
 * Purpose:	Common dialogs: MS Windows implementation
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 */

#include "wx.h"

#include "wx_pdf.h"

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

/*
 * Common dialogs
 *
 */
 
// Pop up a message box
int wxMessageBox(char *message, char *caption, long type,
                 wxWindow *parent, int x, int y)
{
  return wxbMessageBox(message, caption, type, parent, x, y);
}


static BOOL DoGetSaveFileName(OPENFILENAME *of, HWND parent)
{
  if (!of->hwndOwner)
    of->hwndOwner = parent;
  return GetSaveFileName(of);
}

static BOOL DoGetOpenFileName(OPENFILENAME *of, HWND parent)
{
  if (!of->hwndOwner)
    of->hwndOwner = parent;
  return GetOpenFileName(of);
}

static int set_init_dir;
extern void MrEdSyncCurrentDir(void);

char *wxFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;

  wxWnd *wnd = NULL;
  HWND hwnd = NULL;
  if (parent)
  {
    wnd = (wxWnd *)parent->handle;
    hwnd = wnd->handle;
  }
  char *file_buffer;
#define FILEBUF_SIZE 4096

  file_buffer = new WXGC_ATOMIC char[FILEBUF_SIZE];

  if (default_filename)
    strcpy(file_buffer, default_filename);
  else file_buffer[0] = 0;

  char *title_buffer;
  title_buffer = new WXGC_ATOMIC char[50];
  title_buffer[0] = 0;

  char *filter_buffer;
  filter_buffer = new WXGC_ATOMIC char[200];

  if (!wildcard)
    wildcard = "*.*";
  
  /* Alejandro Sierra's wildcard modification

     In wxFileSelector you can put, instead of a single wild_card, pairs of
     strings separated by '|'. The first string is a description, and the
     second is the wild card. You can put any number of pairs.
     
     eg.  "description1 (*.ex1)|*.ex1|description2 (*.ex2)|*.ex2"
     
     If you put a single wild card, it works as before my modification.
  */
  
  // Here begin my changes (Alex)              ******************************
  if (wildcard)                               
    {
         if (!strchr(wildcard, '|'))         // No '|'s, I leave it as it was
                sprintf(filter_buffer, "Files (%s)|%s",wildcard, wildcard);
         else
                strcpy(filter_buffer, wildcard);

         int len = strlen(filter_buffer);

         int i;
         for (i = 0; i < len; i++)
                if (filter_buffer[i]=='|')
                  filter_buffer[i] = '\0';

         filter_buffer[len+1] = '\0';

  }

  if (!set_init_dir) {
    set_init_dir = 1;
    MrEdSyncCurrentDir();
  }

  OPENFILENAME *of;
  of = new OPENFILENAME;

  memset(of, 0, sizeof(OPENFILENAME));

  of->lStructSize = sizeof(OPENFILENAME);
  of->hwndOwner = hwnd;

  // Picky, picky. Need backslashes.
  if (default_path) {
    int i;
    default_path = copystring(default_path);
    for (i = 0; default_path[i]; i++) {
      if (default_path[i] == '/')
	default_path[i] = '\\';
    }
  }

  if (wildcard) {
    of->lpstrFilter = (LPSTR)filter_buffer;
    of->nFilterIndex = 1L;
  } else {
    of->lpstrFilter = NULL;
    of->nFilterIndex = 0L;
  }
  of->lpstrCustomFilter = NULL;
  of->nMaxCustFilter = 0L;
  of->lpstrFile = file_buffer;
  of->nMaxFile = FILEBUF_SIZE;
  of->lpstrFileTitle = title_buffer;
  of->nMaxFileTitle = 50;
  of->lpstrInitialDir = default_path;
  of->lpstrTitle = message;
  of->nFileOffset = 0;
  of->nFileExtension = 0;
  of->lpstrDefExt = default_extension;

  long msw_flags = 0;

  if (flags & wxSAVE)
    msw_flags |= OFN_OVERWRITEPROMPT;
  if (flags & wxMULTIOPEN)
    msw_flags |= OFN_ALLOWMULTISELECT | OFN_EXPLORER;
  if (flags & wxHIDE_READONLY)
    msw_flags |= OFN_HIDEREADONLY;
  if (default_path)
    msw_flags |= OFN_NOCHANGEDIR;
  of->Flags = msw_flags;

  Bool success;

  if (flags & wxSAVE)
    success = wxPrimitiveDialog((wxPDF)DoGetSaveFileName, of, 1);
  else
    success = wxPrimitiveDialog((wxPDF)DoGetOpenFileName, of, 1);

  if (success && !*file_buffer)
    success = 0;

  if (success && (flags & wxMULTIOPEN)) {
    if (of->nFileOffset && !file_buffer[of->nFileOffset - 1]) {
      /* Calculate size... */
      char *s, *result;
      int len = 0, dirlen, filelen;

      s = file_buffer;
      dirlen = strlen(s);
      s += dirlen + 1;
      while (*s) {
	filelen = strlen(s);
	len += dirlen + filelen + 8;
	s += filelen + 1;
      }

      result = new WXGC_ATOMIC char[len];
      len = 0;
      s = file_buffer + dirlen + 1;
      while (*s) {
	filelen = strlen(s);
	sprintf(result + len, "%5d ", filelen + dirlen);
	memcpy(result + len + 6, file_buffer, dirlen);
	memcpy(result + len + 6 + dirlen, s, filelen);
	s += filelen + 1;
	len += filelen + dirlen + 6;
      }
      result[len] = 0;
      return result;
    } else {
      int len;
	  char *result;
      len = strlen(file_buffer);
      result = new WXGC_ATOMIC char[len + 7];
      sprintf(result, "%5d ", len);
      memcpy(result + 6, file_buffer, len + 1);
	  file_buffer = result;
    }
  }

  if (success)
    return file_buffer;
  else
    return NULL;
}
