/*
 * File:	wx_cmdlg.cc
 * Purpose:	Common dialogs: MS Windows implementation
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 2004 PLT Scheme, Inc.
 * Copyright:	(c) 1995, Julian Smart
 *
 * Renovated by Matthew for MrEd, 1995-2000
 * Corrected and improved by Noel Welsh.
 */

#include "wx.h"
#include "wx_pdf.h"
#include <objbase.h>
#include <objidl.h>
#include <shlobj.h>

#define wxDIALOG_DEFAULT_X 300
#define wxDIALOG_DEFAULT_Y 300

#define FILEBUF_SIZE 4096


/*
 * Utility functions
 */


/**
 * Gets the directory portion of an multiple file OPENFILENAME file buffer.  
 * Returns length of said directory portion
 */
static int GetDirectoryPart(char* fileBuffer, char* directory) 
{
  int length;
  
  /* The only confusion is that the directory portion may or may not
     end with a \.  If the file is, e.g., c:\file.dat, it will.  If
     the file is c:\somedir\file.dat, it won't so we must append the \ */

  length = strlen(fileBuffer);
  strcpy(directory, fileBuffer);

  if(! (directory[length - 1] == '\\') ) {
    directory[length] = '\\';
    directory[length + 1] = '\0';
    length = length + 1;
  }

  return length;
}


/**
 * Returns the length of the buffer than will be need for the
 * transformed multiple file OPENFILENAME file buffer 
 */
static int GetTotalLength(char* fileBuffer, int directoryLength)
{
  char* currentFile;
  int currentLength = 0;
  int totalLength = 0;
  
  currentFile = fileBuffer;
  
  /* Skip the directory part */      
  currentLength = strlen(currentFile);
  currentFile += currentLength + 1;

  while (*currentFile) {
    currentLength = strlen(currentFile);
    totalLength += directoryLength + currentLength + 8;
    currentFile += currentLength + 1;
  }

  return totalLength;
}


/**
 * If a dialog box allows multiple files to be selected, and the user
 * selects multiple files, the lpstrFile buffer contains the current
 * directory followed by the filenames of the selected files. For
 * Explorer-style dialog boxes, the directory and filename strings are
 * NULL separated, with an extra NULL character after the last
 * filename.
 *
 * The format we want is a simple string containing of the form:
 * "<length> <full-file-name>...", where
 *
 * <length> is the length of filename
 * <full-file-name> is concatenation of that path and the filename
 *
 * This function does the conversion from the Windows format to the
 * format we want
 */
static char* ExtractMultipleFileNames(OPENFILENAME* of, char* fileBuffer) 
{
  char* result;
  
  /* Check for multiple file names, indicated by a null character
     preceding nFileOffset */
  if (of->nFileOffset && !fileBuffer[of->nFileOffset - 1]) {
    char directory[FILEBUF_SIZE];
    int directoryLength = 0;
    int currentFile;
    int currentFileLength = 0;
    int currentTotal = 0;
    int totalLength = 0;
        
    directoryLength = GetDirectoryPart(fileBuffer, directory);
    totalLength = GetTotalLength(fileBuffer, directoryLength);
    
    result = new WXGC_ATOMIC char[totalLength];

    /* Skip the directory part */
    currentFileLength = strlen(fileBuffer);
    currentFile = currentFileLength + 1;

    /* Copy the concatentation of the directory and file */
    while (fileBuffer[currentFile]) {
      currentFileLength = strlen(fileBuffer XFORM_OK_PLUS currentFile);
      sprintf(result XFORM_OK_PLUS currentTotal, "%5d ",
	      currentFileLength + directoryLength);
      memcpy(result + currentTotal + 6, directory, directoryLength);
      memcpy(result + currentTotal + 6 + directoryLength,
	     fileBuffer + currentFile, currentFileLength);

      currentFile += currentFileLength + 1;
      currentTotal += currentFileLength + directoryLength + 6;
    }
    result[currentTotal] = 0;
  }
  /* Only a single file name so we can simply copy it */
  else {
    int length;
    length = strlen(fileBuffer);

    result = new WXGC_ATOMIC char[length + 7];
    sprintf(result, "%5d ", length);
    memcpy(result + 6, fileBuffer, length + 1);
  }

  return result;
}



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

static BOOL DoGetDir(BROWSEINFO *b, HWND parent)
{
  ITEMIDLIST *r;

  if (!b->hwndOwner)
    b->hwndOwner = parent;

  CoInitialize(NULL);

  r = SHBrowseForFolder(b);

  if (r) {
    IMalloc *mi;
    int ok;

    ok = SHGetPathFromIDList(r, b->pszDisplayName);

    SHGetMalloc(&mi);
    mi->Free(r);
    mi->Release();
    return ok;
  } else
    return 0;
}

static int set_init_dir;
extern void MrEdSyncCurrentDir(void);

char *wxFileSelector(char *message,
                     char *default_path, char *default_filename, 
                     char *default_extension, char *wildcard, int flags,
                     wxWindow *parent, int x, int y)
{
  wxWnd *wnd = NULL;
  HWND hwnd = NULL;
  char *file_buffer;
  char *filter_buffer;
  char *def_path, *def_ext;
  OPENFILENAME *of;
  long msw_flags = 0;
  Bool success;

  if (x < 0) x = wxDIALOG_DEFAULT_X;
  if (y < 0) y = wxDIALOG_DEFAULT_Y;

  if (parent) {
    wnd = (wxWnd *)parent->handle;
    hwnd = wnd->handle;
  }

  if (flags & wxGETDIR) {
    BROWSEINFO *b;
    char *result, *_result;
    int ok;

    _result = (char *)malloc(MAX_PATH + 1);
    
    b = (BROWSEINFO *)malloc(sizeof(BROWSEINFO));
    memset(b, 0, sizeof(BROWSEINFO));

#ifndef BIF_NEWDIALOGSTYLE
# define BIF_NEWDIALOGSTYLE	0x0040
#endif

    b->pidlRoot = NULL;
    b->pszDisplayName = _result;
    b->lpszTitle = message;
    b->ulFlags = (BIF_NEWDIALOGSTYLE | BIF_RETURNONLYFSDIRS);

    ok = wxPrimitiveDialog((wxPDF)DoGetDir, b, 1);
    free(b);

    if (ok) {
      result = new WXGC_ATOMIC char[MAX_PATH + 1];
      memcpy(result, _result, MAX_PATH + 1);
    } else
      result = NULL;

    free(_result);

    return result;
  }

  file_buffer = (char *)malloc(FILEBUF_SIZE);

  if (default_filename) {
    strncpy(file_buffer, default_filename, FILEBUF_SIZE);
    file_buffer[FILEBUF_SIZE-1] = 0;
  } else 
    file_buffer[0] = 0;

  if (!wildcard)
    wildcard = "*.*";
  else if (strlen(wildcard) > 50) {
    char *s;
    s = new WXGC_ATOMIC char[51];
    memcpy(s, wildcard, 50);
    s[50] = 0;
    wildcard = s;
  }

  /* In wxFileSelector you can put, instead of a single wild_card, pairs of
     strings separated by '|'. The first string is a description, and the
     second is the wild card. You can put any number of pairs.
     
     eg.  "description1 (*.ex1)|*.ex1|description2 (*.ex2)|*.ex2"
  */
  
  if (wildcard) {
    int i, len;

    filter_buffer = (char *)malloc(200);
  
    if (!strchr(wildcard, '|'))
      sprintf(filter_buffer, "Files (%s)|%s", wildcard, wildcard);
    else
      strcpy(filter_buffer, wildcard);

    len = strlen(filter_buffer);
	 
    for (i = 0; i < len; i++) {
      if (filter_buffer[i]=='|')
	filter_buffer[i] = '\0';
    }

    /* Extra terminator: */
    filter_buffer[len+1] = '\0';
  } else
    filter_buffer = NULL;

  if (!set_init_dir) {
    set_init_dir = 1;
    MrEdSyncCurrentDir();
  }

  of = (OPENFILENAME *)malloc(sizeof(OPENFILENAME));

  memset(of, 0, sizeof(OPENFILENAME));

  of->lStructSize = sizeof(OPENFILENAME);
  of->hwndOwner = hwnd;

  if (default_path) {
    int len, alen;
    len = strlen(default_path);
    alen = min(MAX_PATH, len);
    def_path = (char *)malloc(alen + 1);
    memcpy(def_path, default_path, alen + 1);
    def_path[alen] = 0;

    // Picky, picky. Need backslashes.
    {
      int i;
      for (i = 0; def_path[i]; i++) {
	if (def_path[i] == '/')
	  def_path[i] = '\\';
      }
    }
  } else
    def_path = NULL;

  if (default_extension) {
    int len, fl;
    len = strlen(default_extension);
	fl = min(50, len);
    def_ext = (char *)malloc(fl + 1);
    memcpy(def_ext, default_extension, fl);
    def_ext[fl] = 0;
  } else
    def_ext = NULL;
      
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
  of->lpstrFileTitle = NULL;
  of->nMaxFileTitle = 0;
  of->lpstrInitialDir = def_path;
  of->lpstrTitle = message;
  of->nFileOffset = 0;
  of->nFileExtension = 0;
  of->lpstrDefExt = def_ext;

  if (flags & wxSAVE)
    msw_flags |= OFN_OVERWRITEPROMPT;
  if (flags & wxMULTIOPEN)
    msw_flags |= OFN_ALLOWMULTISELECT | OFN_EXPLORER;
  if (flags & wxHIDE_READONLY)
    msw_flags |= OFN_HIDEREADONLY;
  if (default_path)
    msw_flags |= OFN_NOCHANGEDIR;
  of->Flags = msw_flags;

  if (flags & wxSAVE)
    success = wxPrimitiveDialog((wxPDF)DoGetSaveFileName, of, 1);
  else
    success = wxPrimitiveDialog((wxPDF)DoGetOpenFileName, of, 1);

  if (success && !*file_buffer)
    success = 0;

  if (def_path) free(def_path);
  if (def_ext) free(def_ext);
  if (filter_buffer) free(filter_buffer);

  {
    char *s;
    if (success && (flags & wxMULTIOPEN)) {
      s = ExtractMultipleFileNames(of, file_buffer);
    } else if (success) {
      s = copystring(file_buffer);
    } else
      s = NULL;
    
    free(of);
    free(file_buffer);

    return s;
  }
}
