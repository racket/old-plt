/*
 * File:	wx_cmdlg.cc
 * Purpose:	Common dialogs: MS Windows implementation
 * Author:	Julian Smart
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, Julian Smart
 *
 * Renovated by Matthew for MrEd, 1995-2000
 * Corrected and improved by Noel Welsh.
 */

#include "wx.h"

#include "wx_pdf.h"

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
    char* currentFile;
    int currentFileLength = 0;
    int currentTotal = 0;
    int totalLength = 0;
        
    directoryLength = GetDirectoryPart(fileBuffer, directory);
    totalLength = GetTotalLength(fileBuffer, directoryLength);
    
    result = new WXGC_ATOMIC char[totalLength];

    /* Skip the directory part */
    currentFileLength = strlen(fileBuffer);
    currentFile = fileBuffer + currentFileLength + 1;

    /* Copy the concatentation of the directory and file */
    while (*currentFile) {
      currentFileLength = strlen(currentFile);
      sprintf(result + currentTotal, "%5d ",
	      currentFileLength + directoryLength);
      memcpy(result + currentTotal + 6, directory, directoryLength);
      memcpy(result + currentTotal + 6 + directoryLength,
	     currentFile, currentFileLength);

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
    return ExtractMultipleFileNames(of, file_buffer);
  }

  if (success)
    return file_buffer;
  else
    return NULL;
}
