/*								-*- C++ -*-
 * $Id: WildCard.cc,v 1.1 1996/01/10 14:56:58 markus Exp $
 *
 * Purpose: wildcard matching
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

Bool wxIsWild(char *pattern)
{
    while (*pattern) {
	switch (*pattern++) {
	case '?': case '*': case '[': case '{':
	    return TRUE;
	case '\\':
	    if (!*pattern++)
		return FALSE;
	}
    }
    return FALSE;
}

Bool wxMatchWild(char *pattern, char *str, Bool dot_special)
{
    char c;
    char *cp;
    Bool done = FALSE, ret_code, ok;
    // Below is for vi fans
    const char OB = '{', CB = '}';

    // dot_special means '.' only matches '.'
    if (dot_special && *str == '.' && *pattern != *str)
	return FALSE;

    while ((*pattern != '\0') && (!done)
    && (((*str=='\0')&&((*pattern==OB)||(*pattern=='*')))||(*str!='\0'))) {
	switch (*pattern) {
	case '\\':
	    pattern++;
	    if (*pattern != '\0')
		pattern++;
	    break;
	case '*':
	    pattern++;
	    ret_code = FALSE;
	    while ((*str!='\0')
	    && (!(ret_code=wxMatchWild(pattern, str++, FALSE))))
		/*loop*/;
	    if (ret_code) {
		while (*str != '\0')
		    str++;
		while (*pattern != '\0')
		    pattern++;
	    }
	    break;
	case '[':
	    pattern++;
	  repeat:
	    if ((*pattern == '\0') || (*pattern == ']')) {
		done = TRUE;
		break;
	    }
	    if (*pattern == '\\') {
		pattern++;
		if (*pattern == '\0') {
		    done = TRUE;
		    break;
		}
	    }
	    if (*(pattern + 1) == '-') {
		c = *pattern;
		pattern += 2;
		if (*pattern == ']') {
		    done = TRUE;
		    break;
		}
		if (*pattern == '\\') {
		    pattern++;
		    if (*pattern == '\0') {
			done = TRUE;
			break;
		    }
		}
		if ((*str < c) || (*str > *pattern)) {
		    pattern++;
		    goto repeat;
		}
	    } else if (*pattern != *str) {
		pattern++;
		goto repeat;
	    }
	    pattern++;
	    while ((*pattern != ']') && (*pattern != '\0')) {
		if ((*pattern == '\\') && (*(pattern + 1) != '\0'))
		    pattern++;
		pattern++;
	    }
	    if (*pattern != '\0') {
		pattern++, str++;
	    }
	    break;
	case '?':
	    pattern++;
	    str++;
	    break;
	case OB:
	    pattern++;
	    while ((*pattern != CB) && (*pattern != '\0')) {
		cp = str;
		ok = TRUE;
		while (ok && (*cp != '\0') && (*pattern != '\0')
		&&  (*pattern != ',') && (*pattern != CB)) {
		    if (*pattern == '\\')
			pattern++;
		    ok = (*pattern++ == *cp++);
		}
		if (*pattern == '\0') {
		    ok = FALSE;
		    done = TRUE;
		    break;
		} else if (ok) {
		    str = cp;
		    while ((*pattern != CB) && (*pattern != '\0')) {
			if (*++pattern == '\\') {
			    if (*++pattern == CB)
				pattern++;
			}
		    }
		} else {
		    while (*pattern!=CB && *pattern!=',' && *pattern!='\0') {
			if (*++pattern == '\\') {
                            if (*++pattern == CB || *pattern == ',')
				pattern++;
			}
		    }
		}
		if (*pattern != '\0')
		    pattern++;
	    }
	    break;
	default:
	    if (*str == *pattern) {
		str++, pattern++;
	    } else {
		done = TRUE;
	    }
	}
    }
    while (*pattern == '*')
	pattern++;
    return ((*str == '\0') && (*pattern == '\0'));
}
