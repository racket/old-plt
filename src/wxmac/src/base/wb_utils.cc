/*
 * File:        wb_utils.cc
 * Purpose:     Miscellaneous utilities
 * Author:      Julian Smart
 * Created:     1993
 * Updated:     August 1994
 * RCS_ID:      $Id: wb_utils.cc,v 1.2 1998/01/14 03:04:10 mflatt Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "%W% %G%"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wx_utils.h"
#include "wx_win.h"
#include "wx_menu.h"

// If not MS C++, don't include wx.h: we'll just include
// the minimum set of files.
// If MS C++, we'll use a precompiled header instead.
#if !defined(_MSC_VER) && !defined(wx_wxh)
#define wx_wxh
#endif

#include "wx.h"

# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>
# if !defined(wx_mac) || defined(_powerc)
#  include <errno.h>
# endif

// Pattern matching code.
// Yes, this path is deliberate (for Borland compilation)
#ifdef wx_mac /* MATTHEW: [5] Mac doesn't like paths with "/" */
#include "glob.inc"
#else
#include "../base/glob.inc"
#endif

#define _MAXPATHLEN 500

extern char *wxBuffer;

/* MATTHEW: [5] Mac */
#if defined(VMS) 
// we have no strI functions under VMS, therefore I have implemented
// an inefficient but portable version: convert copies of strings to lowercase
// and then use the normal comparison
static void myLowerString(char *s)
{
  while(*s){
    if(isalpha(*s)) *s = (char)tolower(*s);
    s++;
  }
}

int strcasecmp(const char *str_1, const char *str_2)
{
  char *temp1 = new char[strlen(str_1)+1];
  char *temp2 = new char[strlen(str_2)+1];
  strcpy(temp1,str_1);
  strcpy(temp2,str_2);
  myLowerString(temp1);
  myLowerString(temp2);

  int result = strcmp(temp1,temp2);
  delete[] temp1;
  delete[] temp2;

  return(result);
}

int strncasecmp(const char *str_1, const char *str_2, size_t maxchar)
{
  char *temp1 = new char[strlen(str_1)+1];
  char *temp2 = new char[strlen(str_2)+1];
  strcpy(temp1,str_1);
  strcpy(temp2,str_2);
  myLowerString(temp1);
  myLowerString(temp2);

  int result = strncmp(temp1,temp2,maxchar);
  delete[] temp1;
  delete[] temp2;

  return(result);
}
#endif

#ifdef wx_msw

#define   _wxToLower(_c)     (char)lcharmap[(unsigned char)(_c)]

#define strcasecmp stricmp
#define strncasecmp strnicmp

// Lower case filename map
static unsigned char lcharmap[] =
{
  '\000', '\001', '\002', '\003', '\004', '\005', '\006', '\007',
  '\010', '\011', '\012', '\013', '\014', '\015', '\016', '\017',
  '\020', '\021', '\022', '\023', '\024', '\025', '\026', '\027',
  '\030', '\031', '\032', '\033', '\034', '\035', '\036', '\037',
  '\040', '\041', '\042', '\043', '\044', '\045', '\046', '\047',
  '\050', '\051', '\052', '\053', '\054', '\055', '\056', '\057',
  '\060', '\061', '\062', '\063', '\064', '\065', '\066', '\067',
  '\070', '\071', '\072', '\073', '\074', '\075', '\076', '\077',
  '\100', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
  '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
  '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
  '\170', '\171', '\172', '\133', '\134', '\135', '\136', '\137',
  '\140', '\141', '\142', '\143', '\144', '\145', '\146', '\147',
  '\150', '\151', '\152', '\153', '\154', '\155', '\156', '\157',
  '\160', '\161', '\162', '\163', '\164', '\165', '\166', '\167',
  '\170', '\171', '\172', '\173', '\174', '\175', '\176', '\177',
  '\200', '\201', '\202', '\203', '\204', '\205', '\206', '\207',
  '\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
  '\220', '\221', '\222', '\223', '\224', '\225', '\226', '\227',
  '\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
  '\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
  '\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
  '\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
  '\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
  '\300', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
  '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
  '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
  '\370', '\371', '\372', '\333', '\334', '\335', '\336', '\337',
  '\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
  '\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
  '\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
  '\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377',
};

#else
// This declaration is missing in SunOS!
// (Yes, I know it is NOT ANSI-C but its in BSD libc)
#if defined(__xlC) || defined(_AIX) || defined(__GNUG__)
extern "C"
{
  int strcasecmp (const char *, const char *);
  int strncasecmp (const char *, const char *, size_t);
}
#endif
#endif				/* wx_msw */
#ifdef wx_mac
int strncasecmp(char *, char *, int);	//these are in src:mac:wx_comparestrings.c
int strcasecmp(char *, char *);
#endif

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

#if 0
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
#endif

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

#if 0
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
#endif

// Match a string INDEPENDENT OF CASE
Bool 
StringMatch (char *str1, char *str2, Bool subString, Bool exact)
{
  if (str1 == NULL || str2 == NULL)
    return FALSE;
  if (str1 == str2)
    return TRUE;

  if (subString)
    {
      int len1 = strlen (str1);
      int len2 = strlen (str2);
      int i;

      // Search for str1 in str2
      // Slow .... but acceptable for short strings
      for (i = 0; i <= len2 - len1; i++)
	{
	  if (strncasecmp (str1, str2 + i, len1) == 0)
	    return TRUE;
	}
    }
  else if (exact)
    {
      if (strcasecmp (str1, str2) == 0)
	return TRUE;
    }
  else
    {
      int len1 = strlen (str1);
      int len2 = strlen (str2);

      if (strncasecmp (str1, str2, min (len1, len2)) == 0)
	return TRUE;
    }

  return FALSE;
}


/****** FILE UTILITIES ******/

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
      static char *buf = NULL;

	  if (!buf) buf = new char[_MAXPATHLEN];

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

// Utility for converting delimiters in DOS filenames to UNIX style
// and back again - or we get nasty problems with delimiters.
// Also, convert to lower case, since case is significant in UNIX.

void 
wxDos2UnixFilename (char *s)
{
  if (s)
    while (*s)
      {
	if (*s == '\\')
	  *s = '/';
#ifdef wx_msw
	else
	  *s = _wxToLower (*s);	// Case INDEPENDENT
#endif
	s++;
      }
}

void 
wxUnix2DosFilename (char *s)
{
// Yes, I really mean this to happen under DOS only! JACS
#ifdef wx_msw
  if (s)
    while (*s)
      {
	if (*s == '/')
	  *s = '\\';
	s++;
      }
#endif
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
      if (lab && StringMatch (title, lab))
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
      if (lab && StringMatch (title, lab))
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

/*
 * wxDebugStreamBuf
 */
#ifndef wx_mac /* MATTHEW: [5] Not in Mac header? */

wxDebugStreamBuf::wxDebugStreamBuf(void)
{
  if (allocate()) setp(base(),ebuf());
}

int wxDebugStreamBuf::overflow(int WXUNUSED(i))
{
  int len = pptr() - pbase();
  char *txt = new char[len+1];
  strncpy(txt, pbase(), len);
  txt[len] = '\0';
#ifdef wx_msw
  OutputDebugString((LPCSTR)txt);
#else
  fprintf(stderr, txt);
#endif
  setp(pbase(), epptr());
  delete[] txt;
  return EOF;
}

int wxDebugStreamBuf::sync(void)
{
  int len = pptr() - pbase();
  char *txt = new char[len+1];
  strncpy(txt, pbase(), len);
  txt[len] = '\0';
#ifdef wx_msw
  OutputDebugString((LPCSTR)txt);
#else
  fprintf(stderr, txt);
#endif
  setp(pbase(), epptr());
  delete[] txt;
  return 0;
}


#endif
