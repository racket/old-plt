/*
 * File:        wb_utils.cc
 * Purpose:     Miscellaneous utilities
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#include "wx.h"

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#define _MAXPATHLEN 500

char *
copystring (const char *s)
{
  if (s == NULL) s = "";
  size_t len = strlen (s) + 1;

  char *news = new char[len];
  memcpy (news, s, len);	// Should be the fastest

  return news;
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
	  if (*tcp == '/' 
	      || *tcp == '\\')
	    return tcp + 1;
	}			/* while */
      if (isalpha (*path) && *(path + 1) == ':')
	return path + 2;
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
        if (path[i] == '/' 
	    || path[i] == '\\')
        {
          done = TRUE;
          buf[i] = 0;
          return buf;
        }
        else i --;
      }

      // Try Drive specifier
      if (isalpha (buf[0]) && buf[1] == ':')
	{
	  // A:junk --> A:. (since A:.\junk Not A:\junk)
	  buf[2] = '.';
	  buf[3] = '\0';
	  return buf;
	}
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
