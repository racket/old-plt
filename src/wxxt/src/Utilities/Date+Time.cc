/*								-*- C++ -*-
 * $Id: Date+Time.cc,v 1.1 1996/01/10 14:56:49 markus Exp $
 *
 * Purpose: time and date related functions
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

#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

void wxSleep(int nSecs)
{
#if defined(__sgi)
    sleep(nSecs);
#else
#if defined(SVR4) || (defined(sun) && defined(__svr4__))
    struct sigset_t oldmask, mask;
    struct timeval tv;

    tv.tv_sec = nSecs;
    tv.tv_usec = 0;

    sigemptyset (&mask);
    sigaddset (&mask, SIGIO);
    sigaddset (&mask, SIGALRM);
    sigprocmask (SIG_BLOCK, &mask, &oldmask);
    if ((select (0, 0, 0, 0, &tv)) == -1) {
	perror ("select in wxSleep");
    }
    sigprocmask (SIG_SETMASK, &oldmask, (sigset_t *) NULL);
#else
    int oldmask, mask;
    struct timeval tv;

    tv.tv_sec = nSecs;
    tv.tv_usec = 0;

    mask = sigmask (SIGIO);
    mask |= sigmask (SIGALRM);
    oldmask = sigblock (mask);
    if ((select (0, 0, 0, 0, &tv)) == -1) {
	perror ("select in wxSleep");
    }
    sigsetmask (oldmask);
#endif
#endif // __sgi
}

char *wxNow(void)
{
    time_t now = time(NULL);
    char *date = ctime(&now); 
    date[24] = '\0';
    return date;
}
