/*
 * File:        wb_utils.cc
 * Purpose:     Miscellaneous utilities
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_utils.cxx,v 1.3 1998/07/15 02:38:01 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_win.h"
#include "wx_menu.h"

#endif

#include <iostream.h>
#include <fstream.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifndef wx_mac
#include <sys/types.h>
#include <sys/stat.h>
#endif

#define _MAXPATHLEN 500

extern char *wxBuffer;

char *
copystring (const char *s)
{
  if (s == NULL) s = "";
  size_t len = strlen (s) + 1;

  char *news = new char[len];
  memcpy (news, s, len);	// Should be the fastest

  return news;
}

// Id generation
static long wxCurrentId = 100;

long 
wxNewId (void)
{
  return wxCurrentId++;
}

long
wxGetCurrentId(void) { return wxCurrentId; }

void 
wxRegisterId (long id)
{
  if (id >= wxCurrentId)
    wxCurrentId = id + 1;
}

void 
StringToFloat (char *s, float *number)
{
  if (s && *s && number)
    *number = (float) strtod (s, NULL);
}

void 
StringToDouble (char *s, double *number)
{
  if (s && *s && number)
    *number = strtod (s, NULL);
}

char *
FloatToString (float number)
{
  static char buf[20];

  sprintf (buf, "%.2f", number);
  return buf;
}

char *
DoubleToString (double number)
{
  static char buf[20];

  sprintf (buf, "%.2f", number);
  return buf;
}

void 
StringToInt (char *s, int *number)
{
  if (s && *s && number)
    *number = (int) strtol (s, NULL, 10);
}

void 
StringToLong (char *s, long *number)
{
  if (s && *s && number)
    *number = strtol (s, NULL, 10);
}

char *
IntToString (int number)
{
  static char buf[20];

  sprintf (buf, "%d", number);
  return buf;
}

char *
LongToString (long number)
{
  static char buf[20];

  sprintf (buf, "%ld", number);
  return buf;
}

Bool 
wxFileExists (const char *filename)
{
  struct stat stbuf;

  // (char *) cast necessary for VMS
  if (filename && stat ((char *)filename, &stbuf) == 0)
    return TRUE;
  return FALSE;
}

#ifdef wx_mac /* MATTHEW: [5] Mac (use for all platforms?) */
Bool 
wxDirExists (const char *filename)
{
  struct stat stbuf;

  if (filename && (stat ((char *)filename, &stbuf) == 0))
    return !!(stbuf.st_mode & S_IFDIR);
  return FALSE;
}
#endif

Bool 
wxIsAbsolutePath (const char *filename)
{
  if (filename)
    {
#ifdef wx_mac /* MATTHEW: [5] Mac */
      if (*filename == ':')
	return FALSE;
      for (; *filename; filename++)
	if (*filename == ':')
	  return TRUE;
      return FALSE;
#else
      if (*filename == '/'
#ifdef VMS
      || (*filename == '[' && *(filename+1) != '.')
#endif
#ifdef wx_msw
      /* MSDOS */
      || *filename == '\\' || (isalpha (*filename) && *(filename + 1) == ':')
#endif
	)
	return TRUE;
#endif
    }
  return FALSE;
}

/*
 * Strip off any extension (dot something) from end of file,
 * IF one exists. Inserts zero into buffer.
 *
 */
 
void wxStripExtension(char *buffer)
{
  int len = strlen(buffer);
  int i = len-1;
  while (i > 0)
  {
    if (buffer[i] == '.')
    {
      buffer[i] = 0;
      break;
    }
    i --;
  }
}

// Return just the filename, not the path
// (basename)
char *
wxFileNameFromPath (char *path)
{
  if (path)
    {
      register char *tcp;

      tcp = path + strlen (path);
      while (--tcp >= path)
	{
	  if (
#ifdef wx_mac /* MATTHEW: [5] Mac */
	      *tcp == ':'
#else
	      *tcp == '/' 
#ifdef wx_msw /* MATTHEW: [5] DOS only */
                    || *tcp == '\\'
#endif
#endif
#ifdef VMS
     || *tcp == ':' || *tcp == ']')
#else
     )
#endif
	    return tcp + 1;
	}			/* while */
#ifdef wx_msw
      if (isalpha (*path) && *(path + 1) == ':')
	return path + 2;
#endif
    }
  return path;
}

// Return just the directory, or NULL if no directory
char *
wxPathOnly (char *path)
{
  if (path && *path)
    {
      static char buf[_MAXPATHLEN];

      // Local copy
      strcpy (buf, path);

      int l = strlen(path);
      Bool done = FALSE;

      int i = l - 1;

      // Search backward for a backward or forward slash
      while (!done && i > -1)
      {
        if (
#ifdef wx_mac /* MATTHEW: [5] Mac */
	    path[i] == ':'
#else
	    path[i] == '/' 
#ifdef wx_msw /* MATTHEW: [5] DOS only */
	    || path[i] == '\\'
#endif
#endif
	    )
        {
          done = TRUE;
          buf[i] = 0;
          return buf;
        }
        else i --;
      }

/* there's a bug here somewhere, so replaced with my original code.
      char *tcp;
      // scan back
      for (tcp = &buf[strlen (buf) - 1]; tcp >= buf; tcp--)
	{
	  // Search for Unix or Dos path sep {'\\', '/'}
	  if (*tcp == '\\' || *tcp == '/')
	    {
	      *tcp = '\0';
	      return buf;
	    }
	}			// for()
*/
#ifdef wx_msw
      // Try Drive specifier
      if (isalpha (buf[0]) && buf[1] == ':')
	{
	  // A:junk --> A:. (since A:.\junk Not A:\junk)
	  buf[2] = '.';
	  buf[3] = '\0';
	  return buf;
	}
#endif
    }

  return NULL;
}

// Return the current date/time
// [volatile]
char *wxNow( void )
{
  time_t now = time(NULL);
  char *date = ctime(&now); 
  date[24] = '\0';
  return date;
}

/* Get Full RFC822 style email address */
Bool
wxGetEmailAddress (char *address, int maxSize)
{
  char host[65];
  char user[65];

  if (wxGetHostName(host, 64) == FALSE)
    return FALSE;
  if (wxGetUserId(user, 64) == FALSE)
    return FALSE;

  char tmp[130];
  strcpy(tmp, user);
  strcat(tmp, "@");
  strcat(tmp, host);

  strncpy(address, tmp, maxSize - 1);
  address[maxSize-1] = '\0';
  return TRUE;
}

// Concatenate two files to form third
Bool 
wxConcatFiles (const char *file1, const char *file2, const char *file3)
{
  char *outfile = wxGetTempFileName("cat");

  FILE *fp1 = NULL;
  FILE *fp2 = NULL;
  FILE *fp3 = NULL;
  // Open the inputs and outputs
  if ((fp1 = fopen (file1, "rb")) == NULL ||
      (fp2 = fopen (file2, "rb")) == NULL ||
      (fp3 = fopen (outfile, "wb")) == NULL)
    {
      if (fp1)
	fclose (fp1);
      if (fp2)
	fclose (fp2);
      if (fp3)
	fclose (fp3);
      return FALSE;
    }

  int ch;
  while ((ch = getc (fp1)) != EOF)
    (void) putc (ch, fp3);
  fclose (fp1);

  while ((ch = getc (fp2)) != EOF)
    (void) putc (ch, fp3);
  fclose (fp2);

  fclose (fp3);
  Bool result = wxRenameFile(outfile, file3);
  delete[] outfile;
  return result;
}

// Copy files
Bool 
wxCopyFile (const char *file1, const char *file2)
{
  FILE *fd1;
  FILE *fd2;
  int ch;

  if ((fd1 = fopen (file1, "rb")) == NULL)
    return FALSE;
  if ((fd2 = fopen (file2, "wb")) == NULL)
    {
      fclose (fd1);
      return FALSE;
    }

  while ((ch = getc (fd1)) != EOF)
    (void) putc (ch, fd2);

  fclose (fd1);
  fclose (fd2);
  return TRUE;
}

Bool 
wxRenameFile (const char *file1, const char *file2)
{
  // Normal system call
  if (0 == rename (file1, file2))
    return TRUE;
  // Try to copy
  if (wxCopyFile(file1, file2)) {
    wxRemoveFile(file1);
    return TRUE;
  }
  // Give up
  return FALSE;
}

/*
 * Strip out any menu codes
 */

char *wxStripMenuCodes (char *in, char *out)
{
  if (!in)
    return NULL;
    
  if (!out)
    out = copystring(in);

  char *tmpOut = out;
  
  while (*in)
    {
      if (*in == '&')
	{
	  // Check && -> &, &x -> x
	  if (*++in == '&')
	    *out++ = *in++;
	}
      else if (*in == '\t')
	{
          // Remove all stuff after \t in X mode, and let the stuff as is
          // in Windows mode.
          // Accelerators are handled in wx_item.cc for Motif, and are not
          // YET supported in XView
	  break;
	}
      else
	*out++ = *in++;
    }				// while

  *out = '\0';

  return tmpOut;
}


/*
 * Window search functions
 *
 */

/*
 * If parent is non-NULL, look through children for a label or title
 * matching the specified string. If NULL, look through all top-level windows.
 *
 */

static wxWindow *wxFindWindowByLabel1 (char *title, wxWindow * parent);

wxWindow *
wxFindWindowByLabel (char *title, wxWindow * parent)
{
  if (parent)
    {
      return wxFindWindowByLabel1 (title, parent);
    }
  else
    {
      for (wxChildNode * node = wxTopLevelWindows(NULL)->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  if (win && node->IsShown()) {
	    wxWindow *retwin = wxFindWindowByLabel1 (title, win);
	    if (retwin)
	      return retwin;
	  }
	}			// for()

    }
  return NULL;
}

// Recursive
static wxWindow *
wxFindWindowByLabel1 (char *title, wxWindow * parent)
{
  if (parent)
    {
      char *lab = parent->GetLabel ();
      if (lab && !strcmp(title, lab))
	return parent;
    }

  if (parent)
    {
      for (wxChildNode * node = parent->GetChildren()->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  wxWindow *retwin = wxFindWindowByLabel1 (title, win);
	  if (retwin)
	    return retwin;
	}			// for()

    }

  return NULL;			// Not found

}

/*
 * If parent is non-NULL, look through children for a name
 * matching the specified string. If NULL, look through all top-level windows.
 *
 */

static wxWindow *wxFindWindowByName1 (char *title, wxWindow * parent);

wxWindow *
wxFindWindowByName (char *title, wxWindow * parent)
{
  if (parent)
    {
      return wxFindWindowByName1 (title, parent);
    }
  else
    {
      for (wxChildNode * node = wxTopLevelWindows(NULL)->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  if (win && node->IsShown()) {
	    wxWindow *retwin = wxFindWindowByName1 (title, win);
	    if (retwin)
	      return retwin;
	  }
	}			// for()

    }
  // Failed? Try by label instead.
  return wxFindWindowByLabel(title, parent);
}

// Recursive
static wxWindow *
wxFindWindowByName1 (char *title, wxWindow * parent)
{
  if (parent)
    {
      char *lab = parent->GetName ();
      if (lab && !strcmp(title, lab))
	return parent;
    }

  if (parent)
    {
      for (wxChildNode * node = parent->GetChildren()->First (); node; node = node->Next ())
	{
	  wxWindow *win = (wxWindow *) node->Data ();
	  wxWindow *retwin = wxFindWindowByName1 (title, win);
	  if (retwin)
	    return retwin;
	}			// for()

    }

  return NULL;			// Not found

}

// Returns menu item id or -1 if none.
int 
wxFindMenuItemId (wxFrame * frame, char *menuString, char *itemString)
{
  wxMenuBar *menuBar = frame->GetMenuBar ();
  if (!menuBar)
    return -1;
  return menuBar->FindMenuItem (menuString, itemString);
}
