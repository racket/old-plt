/*
 * File:        wb_utils.cc
 * Purpose:     Miscellaneous utilities
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

#ifdef __GNUG__
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_win.h"
#include "wx_menu.h"

# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>

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

/****** FILE UTILITIES ******/

// Return just the filename, not the path
// (basename)
char *
wxFileNameFromPath (char *path)
{
  if (path) {
    register char *tcp;
    
    tcp = path + strlen (path);
    while (--tcp >= path) {
      if (
#ifdef OS_X
	  (*tcp == '/')
#else
	  (*tcp == ':')
#endif
	  )
	return tcp + 1;
    }
  }
  return path;
}

// Return just the directory, or NULL if no directory
char *
wxPathOnly (char *path)
{
  if (path && *path)
    {
      static char *buf = NULL;

      if (!buf) {
	wxREGGLOB(buf);
	buf = new char[_MAXPATHLEN];
      }

      // Local copy
      strcpy (buf, path);

      int l = strlen(path);
      Bool done = FALSE;

      int i = l - 1;

      // Search backward for a backward or forward slash
      while (!done && i > -1)
	{
	  if (
#ifdef OS_X
	      path[i] == '/'
#else
	      path[i] == ':'
#endif
	      )
	    {
	      done = TRUE;
	      buf[i] = 0;
	      return buf;
	    }
	  else i --;
	}
    }

  return NULL;
}

// Return the current date/time
// [volatile]
char *wxNow( void )
{
  return "";
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


// Returns menu item id or -1 if none.
int 
wxFindMenuItemId (wxFrame * frame, char *menuString, char *itemString)
{
  wxMenuBar *menuBar = frame->GetMenuBar ();
  if (!menuBar)
    return -1;
  return menuBar->FindMenuItem (menuString, itemString);
}

int strcasecmp(char *s, char *t);
int strcasecmp(char *s, char *t)
{
  int r;
  while (*s && *t) {
    r = tolower(*s++) - tolower(*t++);
    if (r != 0) return r;
  }
  return (tolower(*s) - tolower(*t));		// CJC is this correct
}

int strncasecmp(char *s, char *t, int w);
int strncasecmp(char *s, char *t, int w)
{
  int r,i = 0;
  while (i < w) {
    r = tolower(s[i]) - tolower(t[i]);
    if (r != 0) return r;
    i += 1;	
  }
  return 0;
}
