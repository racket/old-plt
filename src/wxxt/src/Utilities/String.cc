/*								-*- C++ -*-
 * $Id: String.cc,v 1.1 1996/01/10 14:56:56 markus Exp $
 *
 * Purpose: string copy and conversion
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "wx.h"

#include <string.h>

char *copystring(const char *s)
{
    if (s == NULL) s = "";
    size_t len = strlen (s) + 1;
    char *news = new char[len];
    memcpy(news, s, len);        // Should be the fastest
    return news;
}

void wxStringToDouble(char *s, double *number)
{
    if (s && *s && number)
	*number = strtod(s, NULL);
}

void wxStringToFloat(char *s, float *number)
{
    if (s && *s && number)
	*number = (float)strtod(s, NULL);
}

void wxStringToInt(char *s, int *number)
{
    if (s && *s && number)
	*number = (int)strtol(s, NULL, 10);
}

void wxStringToLong(char *s, long *number)
{
    if (s && *s && number)
	*number = strtol(s, NULL, 10);
}

char *wxDoubleToString(double number)
{
    sprintf(wxBuffer, "%.2f", number);
    return wxBuffer;
}

char *wxFloatToString(float number)
{
    sprintf(wxBuffer, "%.2f", number);
    return wxBuffer;
}

char *wxIntToString(int number)
{
    sprintf(wxBuffer, "%d", number);
    return wxBuffer;
}

char *wxLongToString(long number)
{
    sprintf(wxBuffer, "%ld", number);
    return wxBuffer;
}

void wxGetLabelAndKey(char *label, char **clean_label, char **clean_key)
{
    char *amp, *key;

    *clean_label = copystring(label); // make private copy
    if ((amp = strchr(*clean_label, '&'))) { // is there an ampersand? -> erase
	memmove(amp, amp+1, strlen(amp+1) + 1);
    }
    if ((key=strchr(*clean_label, '\t'))) // is there a key binding? -> split
	*key++ ='\0';
    if (clean_key)
      *clean_key = key; // point to key binding in private copy
}

Bool wxStringMatch(char *str1, char *str2, Bool subString, Bool exact)
{
    if (str1 == NULL || str2 == NULL)
	return FALSE;
    if (str1 == str2)
	return TRUE;

    if (subString) {
	int len1 = strlen (str1);
	int len2 = strlen (str2);
	int i;
	for (i = 0; i <= len2 - len1; i++) {
	    if (strncasecmp (str1, str2 + i, len1) == 0)
		return TRUE;
	}
    } else if (exact) {
	if (strcasecmp (str1, str2) == 0)
	    return TRUE;
    } else {
	int len1 = strlen (str1);
	int len2 = strlen (str2);
	if (strncasecmp (str1, str2, min (len1, len2)) == 0)
	    return TRUE;
    }
    return FALSE;
}

int wxStringEq(char *s1, char *s2)
{
    return (int)(s1 && s2 && (strcmp(s1, s2) == 0));
}

char *wxStripMenuCodes(char *in, char *out)
{
    if (!in)
	return NULL;
    if (!out)
	out = copystring(in);
    char *tmpOut = out;
  
    while (*in)  {
	if (*in == '&') {
	    // Check && -> &, &x -> x
	    if (*++in == '&')
		*out++ = *in++;
	} else if (*in == '\t') {
	    // Remove all stuff after \t in X mode, and let the stuff as is
	    // in Windows mode.
	    // Accelerators are handled in wx_item.cc for Motif, and are not
	    // YET supported in XView
	    break;
	} else
	    *out++ = *in++;
    }
    *out = '\0';
    return tmpOut;
}
