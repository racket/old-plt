/*
 *	Functions for drawing String's with tab characters in them
 */

#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include "xwTabString.h"

/*
 *	Like DrawImageString, except it takes an additional  "tabs"
 *	argument, used to specify what horizontal pixel position to
 *	move to when tab characters are present in the string.  If
 *	the "tabs" argument is NULL, works exactly like its
 *	counterpart.
 */
static void
doDrawImageString(display, drawable, gc, x, y, string, length, tabs, font, line, draw)
     Display *display;
     Drawable drawable;
     GC gc;
     int x;
     int y;
     String string;
     int length;
     int *tabs;
     XFontStruct *font;
     int line;
     void (*draw)();
{
  register char	*p, *ep, *ap;
  register int	tx, tab;
  
  if (!length)
    return;
 
  tab = tx = 0;
  for (p = string; length; )
    {
      if (tabs)
	ep = strnchr(p, '\t', length);
      else
	ep = NULL;
      if (font)
	ap = strnchr(p, '&', length);
      else
	ap = NULL;

      if (ep && ap) {
	if ((long)ep < (long)ap)
	  ap = NULL;
	else
	  ep = NULL;
      }

      if (ep) {
	draw(display, drawable, gc, x+tx, y, p, ep - p);
	tx = tabs[tab++];
	length -= ep - p + 1;
	p = ep + 1;
      } else if (ap) {
	draw(display, drawable, gc, x+tx, y, p, ap - p);
	tx += XTextWidth(font, p, ap - p);
	length -= ap - p + 1;
	p = ap + 1;
	if (length) {
	  /* Underline next */
	  XCharStruct overall;
	  int dir, ascent, descent;
	  draw(display, drawable, gc, x+tx, y, p, 1);
	  XTextExtents(font, p, 1, &dir, &ascent, &descent, &overall);
	  if (line && (*p != '&'))
	    XDrawLine(display, drawable, gc, x+tx, y+1, x+tx+overall.width, y+1);
	  length -= 1;
	  p += 1;
	  tx += overall.width;
	}
      } else {
	draw(display, drawable, gc, x+tx, y, p, length);
	break;
      }
    }
}

void
XfwfDrawImageString(display, drawable, gc, x, y, string, length, tabs, fnt)
     Display *display;
     Drawable drawable;
     GC gc;
     int x;
     int y;
     String string;
     int length;
     int *tabs;
     XFontStruct *fnt;
{
  doDrawImageString(display, drawable, gc, x, y, string, length, tabs, fnt, 1, XDrawImageString);
}

void
XfwfDrawString(display, drawable, gc, x, y, string, length, tabs, fnt, line)
     Display *display;
     Drawable drawable;
     GC gc;
     int x;
     int y;
     String string;
     int length;
     int *tabs;
     XFontStruct *fnt;
     int line;
{
  doDrawImageString(display, drawable, gc, x, y, string, length, tabs, fnt, line, XDrawString);
}

/*
 *	Converts a string list of tabs to an array of tabs
 */
int *
XfwfTablist2Tabs(tablist)
char *tablist;
{
	register int	*tabs = NULL;
	register int	ntabs = 0;

	if (!tablist)
		return NULL;
	for (;;)
	{
		/* Skip leading blanks */
		while (*tablist && *tablist == ' ') ++tablist;
		if (!*tablist) break;

		/* Allocate space for the new tab */
		if (ntabs)
			tabs = (int *) XtRealloc( (char *) tabs,
						(ntabs+1) * sizeof(int));
		else
			tabs = (int *) XtMalloc( (ntabs + 1) * sizeof(int));
		/* Add it to the list */
		tabs[ntabs++] = atoi(tablist);
		/* Skip to the next blank */
		while (*tablist && *tablist != ' ') ++tablist;
	}
	return (tabs);
}

/*
 *	Like TextWidth, except it takes an additional  "tabs"
 *	argument, used to specify what horizontal pixel position to
 *	move to when tab characters are present in the string.  If
 *	the "tabs" argument is NULL, works exactly like its
 *	counterpart.
 */
int
XfwfTextWidth(font, str, length, tabs)
     XFontStruct *font;
     String str;
     int length;
     int *tabs;
{
  register char	*p, *ep, *c = NULL, *pp;
  register int	tx, tab, rc, ll;

  if (!length)
    return 0;
 
  p = pp = str;
  ll = length;

  while (1) {
    ep = strnchr(pp, '&', ll);
    if (ep) {
      int l = ep - p;
      if (!c)
	c = XtMalloc(length + 1);
      memmove(c, p, l);
      memmove(c + l, p + l + 1, length - l); /* gets nul char */
      length -= 1;
      p = c;
      if (length > l) {
	pp = c + l + 1; /* Skip next char */
	ll = length - (l + 1);
      } else {
	pp = p;
	ll = length;
      }
    } else
      break;
  }

  tab = tx = 0;
  if (length == 0) {
    if (c)
      XtFree(c);
    return 0;
  }
  for (; length; ) {
    ep = strnchr(p, '\t', length);
    if (ep && tabs) {
      tx = tabs[tab++];
      length -= ep - p + 1;
      p = ep + 1;
    } else {
      rc = XTextWidth(font, p, length);
      if (c)
	XtFree(c);
      if (rc < 0) return rc; else return rc + tx;
    }
  }

  if (c)
    XtFree(c);

  return -1;
}

/*
 *	Like strchr, except has a length limit.
 */
char *
strnchr(s, c, n)
     char *s;
     int c;
     int n;
{
	while (n--)
		if (*s == c) return s; else ++s;
	return NULL;
}
