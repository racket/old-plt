/*								-*- C++ -*-
 * $Id: Help.cc,v 1.1 1996/01/10 14:56:25 markus Exp $
 *
 * Purpose: connection to wxHelp server
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

#ifdef __GNUG__
#pragma implementation "Help.h"
#endif

#include <time.h>
#include <limits.h>

#define  Uses_wxHelp
#include "wx.h"

// Timeout in seconds
#define WX_HELP_TIMEOUT 15 /* was 30 */

//-----------------------------------------------------------------------------
// wxHelpInstance create and destroy
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxHelpInstance, wxClient)

wxHelpInstance::wxHelpInstance(Bool WXUNUSED(native))
{
    __type = wxTYPE_HELP_INSTANCE;

    useNative = FALSE;  // Vetoed under X -- no native help system

    helpFile = NULL;
    helpServer = -1;
    helpHost = NULL;
    helpRunning = FALSE;
    helpConnection = NULL;
}

wxHelpInstance::~wxHelpInstance(void)
{
    if (helpFile)
	delete[] helpFile; 
    if (helpHost)
	delete[] helpHost;
}

//-----------------------------------------------------------------------------
// wxHelpInstance methods
//-----------------------------------------------------------------------------

Bool wxHelpInstance::Initialize(char *filename, int server)
{
    char host_buf[255];
    if (wxGetHostName(host_buf, sizeof(host_buf)))
	helpHost = copystring(host_buf);
    else helpHost = NULL;

    helpFile = copystring(filename);
    if (!useNative) {
	helpServer = server;
	wxIPCInitialize();
    }
    return TRUE;
}

Bool wxHelpInstance::LoadFile(char *file)
{
    if (!file)
	file = helpFile;
    else {
	if (helpFile)
	    delete[] helpFile;
	helpFile = copystring(file);
    }

    if (!useNative) {
	if (!helpRunning && !Run())
	    return FALSE;
	char buf[PATH_MAX];
	sprintf(buf, "f %s", file);
	if (helpConnection)
	    return helpConnection->Execute(buf);
	else
	    return FALSE;
    } else {
	// use native
    }
    return FALSE;
}

Bool wxHelpInstance::DisplayContents(void)
{
    if (!useNative) {
	if (!helpRunning && !Run())
	    return FALSE;
	if (helpConnection)
	    return helpConnection->Execute("s -1");
	else return FALSE;
    } else {
	// use native
    }
    return FALSE;
}

Bool wxHelpInstance::DisplaySection(int section)
{
    if (!useNative) {
	if (!helpRunning && !Run())
	    return FALSE;
	char buf[PATH_MAX];
	sprintf(buf, "s %d", section);
	if (helpConnection)
	    return helpConnection->Execute(buf);
	else return FALSE;
    } else {
	// use native
    }
    return FALSE;
}

Bool wxHelpInstance::DisplayBlock(long block)
{
    if (!useNative) {
	if (!helpRunning && !Run())
	    return FALSE;
	char buf[PATH_MAX];
	sprintf(buf, "b %ld", block);
	if (helpConnection)
	    return helpConnection->Execute(buf);
	else return FALSE;
    } else {
	// use native
    }
    return FALSE;
}

Bool wxHelpInstance::KeywordSearch(char *k)
{
    if (!useNative) {
	if (!helpRunning && !Run())
	    return FALSE;
	char buf[500];
	sprintf(buf, "k %s", k);
	if (helpConnection)
	    return helpConnection->Execute(buf);
	else return FALSE;
    } else {
	// use native
    }
    return FALSE;
}

Bool wxHelpInstance::Quit(void)
{
    if (helpConnection)
	return helpConnection->Disconnect(); // Calls OnQuit via OnDisconnect
    else return TRUE;
}

Bool wxHelpInstance::Run(void)
{
    if (!helpFile || !helpHost || helpRunning)
	return FALSE;

    time_t current_time;
    // Invent a server name that's likely to be unique but different from
    // last time
    (void)time(&current_time);
    if (helpServer == -1)
	helpServer = (int)(4000 + (current_time % 4000));

    char server[32];
    sprintf(server, "%d", helpServer);
    // Start help process in server modus
    // char *argv[] = {"wxhelp", "-server", server, NULL};
    char *argv[4];
    argv[0] = "wxhelp";
    argv[1] = "-server";
    argv[2] = server;
    argv[3] = NULL;

    if (wxExecute(argv) == FALSE)
	return FALSE; // Maybe we should print a message?

    time_t start_time;
    (void)time(&start_time);
    // Give it some time to respond
    do {
	wxSleep(1);
	helpConnection
	    = (wxHelpConnection*)MakeConnection(helpHost, server, "WXHELP");
	(void)time(&current_time);
    } while (!helpConnection && ((current_time-start_time) < WX_HELP_TIMEOUT));

    if (helpConnection == NULL) {
	char buf[100];
	sprintf(buf, "Connection to wxHelp timed ot in %d seconds.",
		WX_HELP_TIMEOUT);
	(void)wxMessageBox(buf, "Error");
	return FALSE;
    }
    helpRunning = TRUE;
    return TRUE;
}

//-----------------------------------------------------------------------------
// wxHelpConnection
//-----------------------------------------------------------------------------

IMPLEMENT_CLASS(wxHelpConnection, wxConnection)

wxHelpConnection::wxHelpConnection(wxHelpInstance *instance)
{
    helpInstance = instance;
}

Bool wxHelpConnection::OnDisconnect(void)
{
    helpInstance->OnQuit();
    helpInstance->helpRunning = FALSE;
    helpInstance->helpConnection = NULL;
    helpInstance->helpServer = -1;
    delete this;
    return TRUE;
}
