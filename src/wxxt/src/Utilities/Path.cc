/*								-*- C++ -*-
 * $Id: Path.cc,v 1.1.1.1 1997/12/22 17:28:56 mflatt Exp $
 *
 * Purpose: path- and filename manipulations
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

#include <ctype.h>
#include <limits.h>
#include <string.h>

char *wxContractPath(const char *fname, const char *envname, const char *user)
{
    static char *dest = wxBuffer;

    if (fname == NULL)
	return NULL;

    strcpy (dest, fname);

    // Handle environment
    char *val=NULL, *tcp=NULL;
    if (envname != NULL && (val = getenv(envname)) != NULL
    && (tcp = strstr(dest, val)) != NULL) {
        strcpy(wxBuffer, tcp + strlen (val));
        *tcp++ = '$';
        *tcp++ = '{';
        strcpy(tcp, envname);
        strcat(tcp, "}");
        strcat(tcp, wxBuffer);
    }
    // Handle User's home (ignore root homes!)
    size_t len=0;
    if ((val = wxGetUserHome(user)) != NULL
    && (len = strlen(val)) > 2
    && strncmp(dest, val, len) == 0) {
	strcpy(wxBuffer, "~");
	if (user && *user)
	    strcat(wxBuffer, user);
	strcat(wxBuffer, dest + len);
	strcpy (dest, wxBuffer);
    }
    return dest;
}

char *wxCopyAbsolutePath(const char *filename)
{
    if (filename == NULL)
	return NULL;

    if (!IsAbsolutePath(wxExpandPath(wxBuffer, filename))) {
	char buf[PATH_MAX];
	buf[0] = '\0';
	wxGetWorkingDirectory(buf, sizeof(buf)/sizeof(char));
	char ch = buf[strlen(buf) - 1];
	if (ch != '/')
	    strcat(buf, "/");
	strcat(buf, wxBuffer);
	return copystring( wxRealPath(buf) );
    }
    return copystring(wxBuffer);
}

char *wxExpandPath(char *buf, const char *name)
{
    register char  *d, *s, *nm;
    char           lnm[PATH_MAX];
    Bool           q;
    const char     trimchars[] = "\n \t";
    const char     SEP = '/';

    buf[0] = '\0';
    if (name == NULL || *name == '\0')
	return buf;
    // Make a scratch copy
    nm = copystring(name);
    char *nm_tmp = nm;
    /* Skip leading whitespace and cr */
    while (strchr(trimchars, *nm) != NULL)
	nm++;
    /* And strip off trailing whitespace and cr */
    s = nm + (q = strlen(nm)) - 1;
    while (q-- && strchr(trimchars, *s) != NULL)
	*s = '\0';
    s = nm;
    d = lnm;
    q = nm[0] == '\\' && nm[1] == '~';

    /* Expand inline environment variables */
    while ((*d++ = *s)) {
	if (*s == '\\') {
	    if ((*(d - 1) = *++s)) {
		s++;
		continue;
	    } else
		break;
	} else {
	    if (*s++ == '$') {
		register char  *start = d;
		register int   braces = (*s == '{' || *s == '(');
		register char  *value;
		while ((*d++ = *s))
		    if (braces ? (*s == '}' || *s == ')') :
                                 !(isalnum(*s) || *s == '_'))
			break;
		    else
			s++;
		*--d = 0;
		value = getenv(braces ? start + 1 : start);
		if (value) {
		    for (d = start - 1; (*d++ = *value++); )
			;
		    d--;
		    if (braces && *s)
			s++;
		}
	    }
	}
    }
    // Expand ~ and ~user
    nm = lnm;
    s = "";
    if (nm[0] == '~' && !q) {
	/* prefix ~ */
	if (nm[1] == SEP || nm[1] == 0) {	/* ~/filename */
	    if ((s = wxGetUserHome(NULL)) != NULL) {
		if (*++nm)
		    nm++;
	    }
	} else {		/* ~user/filename */
	    register char  *nnm;
	    register char  *home;
	    for (s = nm; *s && *s != SEP; s++);
	    nnm = *s ? s + 1 : s;
	    *s = 0;
	    if ((home = wxGetUserHome(nm + 1)) == NULL) {
		*s = SEP;
		s = "";
	    } else {
		nm = nnm;
		s = home;
	    }
	}
    }
    d = buf;
    if (*s) {
	/* Copy home dir */
	while ('\0' != (*d++ = *s++))
	    /* loop */;
	// Handle root home
	if (d - 1 > buf && *(d - 2) != SEP)
	    *(d - 1) = SEP;
    }
    s = nm;
    while ((*d++ = *s++));

    delete[] nm_tmp; // clean up alloc
    /* Now clean up the buffer */
    return wxRealPath(buf);
}

char *wxFileNameFromPath(char *path)
{
    if (path) {
	register char *tcp;

	tcp = path+strlen(path);
	while (--tcp >= path) {
	    if (*tcp == '/' || *tcp == '\\')
		return tcp + 1;
	}
    }
    return path;
}

Bool wxIsAbsolutePath(char *filename)
{
    return (filename && *filename == '/');
}

char *wxPathOnly(char *path)
{
    if (path) {
	char *p;
	char *last_slash = NULL;

	// copy path and keep the last slash or baskslash in mind
	for (p = wxBuffer; *path; ++path, ++p) {
	    *p = *path;
	    if (*p == '/' || *p == '\\')
		last_slash = p;
	}
	if (last_slash) {
	    *last_slash = '\0';
	    return wxBuffer;
	}
    }
    return NULL;
}

char *wxRealPath(char *path)
{
    static const char SEP = '/';

    if (path[0] && path[1]) {
	for (char *p = &path[2]; *p; p++) {
	    if (*p == SEP) {
		if (p[1]=='.' && p[2]=='.' && (p[3]==SEP || p[3]=='\0')) {
		    char *q;
		    for (q = p - 1; q >= path && *q != SEP; q--)
			/*loop*/;
		    if (q[0]==SEP && (q[1]!='.' || q[2]!='.' || q[3]!=SEP)
		    && (q-1<=path || q[-1]!=SEP)) {
			strcpy (q, p + 3);
			if (path[0] == '\0') {
			    path[0] = SEP;
			    path[1] = '\0';
			}
			p = q - 1;
		    }
		} else if (p[1]=='.' && (p[2]==SEP || p[2]=='\0'))
		    strcpy (p, p + 2);
	    }
	}
    }
    return path;
}

void wxStripExtension(char *buffer)
{
    int len = strlen(buffer);
    int i = len-1;
    while (i > 0) {
	if (buffer[i] == '.') {
	    buffer[i] = 0;
	    break;
	}
	i--;
    }
}
