/*								-*- C++ -*-
 * $Id: Net.cc,v 1.3 2000/05/22 15:21:21 mflatt Exp $
 *
 * Purpose: host and user net info
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

#include <pwd.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#if defined(__digital__) && defined(__unix__)
extern "C" {
extern int gethostname(char *, int);
};
#endif

Bool wxGetHostName(char *buf, int sz)
{
#if defined(SVR4) && !defined(__sgi)
    return (sysinfo(SI_HOSTNAME, buf, maxSize) != -1);
#else /* BSD Sockets */
    char name[255];
    // Get hostname
    if ((gethostname(name, sizeof(name)/sizeof(char)-1)) == -1)
	return FALSE;
    strncpy(buf, name, sz-1);
    return TRUE;
#endif
}

Bool wxGetEmailAddress(char *address, int maxSize)
{
    char host[65];
    char user[65];
    char tmp[130];

    if (wxGetHostName(host, 64) == FALSE)
	return FALSE;
    if (wxGetUserId(user, 64) == FALSE)
	return FALSE;

    strcpy(tmp, user);
    strcat(tmp, "@");
    strcat(tmp, host);
    strncpy(address, tmp, maxSize - 1);
    address[maxSize-1] = '\0';
    return TRUE;
}

Bool wxGetUserId(char *buf, int sz)
{
    struct passwd *who;

    if ((who = getpwuid(getuid ())) != NULL) {
	strncpy (buf, who->pw_name, sz-1);
	return TRUE;
    }
    return FALSE;
}

Bool wxGetUserName(char *buf, int sz)
{
    struct passwd *who;

    if ((who = getpwuid (getuid ())) != NULL) {
	strncpy (buf, who->pw_gecos, sz - 1);
	return TRUE;
    }
    return FALSE;
}
