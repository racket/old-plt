/*								-*- C++ -*-
 * $Id: Subprocess.cc,v 1.1 1996/01/10 14:56:57 markus Exp $
 *
 * Purpose: Subprocess control
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
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>

Bool wxExecute(char **argv, Bool Async)
{
    if (*argv == NULL)
	return FALSE;

    /* fork the process */
#if defined(sun) || defined(__ultrix) || defined(__bsdi__)
    pid_t pid = vfork();
#else
    pid_t pid = fork();
#endif
    if (pid == -1) {
	perror ("fork failed");
	return FALSE;
    } else if (pid == 0) {
	/* child */
#ifdef _AIX
	execvp ((const char *)*argv, (const char **)argv);
#else
	execvp (*argv, argv);
#endif
	if (errno == ENOENT)
	    wxError("command not found", *argv);
	else
	    perror (*argv);
	wxError("could not execute", *argv);
	_exit (-1);
    }

    // Code below is NOT really acceptable!
    // One should NEVER use wait under X
    // Ideas? A Sleep idle callback?
    // WARNING: WARNING: WARNING: WARNING:
    // The CODE BELOW IS BAD BAD BAD BAD!
    if (Async) {
	int status;
	wxSleep(2);		// Give a little time
    /* MATTHEW: add FreeBSD */
#if !defined(DG) && !defined(_AIX) && !defined(__xlC__) && !defined(SVR4) && !defined(sun) && !defined(__alpha) && !defined(__sgi) && !defined(__hpux) && !defined(__SUNPRO_CC) &&!defined(__FreeBSD__)
	while (wait((union wait*)&status) != pid)
#else
	while (wait(&status) != pid)
#endif
	    wxSleep(3);	// 3 sec?
    }
    return TRUE;
}

Bool wxExecute (const char *command, Bool Async)
{
    if (command == NULL || *command == '\0')
	return FALSE; // Nothing to do

    int argc = 0;
    char *argv[127];
    char tmp[1024];
    const char *IFS = " \t\n";

    strncpy (tmp, command, sizeof(tmp) / sizeof(char) - 1);
    tmp[sizeof (tmp) / sizeof (char) - 1] = '\0';
    argv[argc++] = strtok (tmp, IFS);
    while ((argv[argc++] = strtok(NULL, IFS)) != NULL)
	/* loop */ ;
    return wxExecute(argv, Async);
}

Bool wxShell(const char *command)
{
#if defined(sun) || defined(__ultrix) || defined(__bsdi__)
    pid_t pid = vfork();
#else
    pid_t pid = fork();
#endif
    switch( pid ) {
    case -1:			/* error */
	return(FALSE);
    case 0:			/* child */
	// Generic X windows terminal window
	if (command && *command)
	    execlp("xterm", "-e", (char*)command, NULL);
	else
	    execlp("xterm", NULL);
	_exit(127);
    }
    return TRUE;
}
