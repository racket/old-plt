/*								-*- C++ -*-
 * $Id: Server.cc,v 1.1 1996/01/10 14:56:27 markus Exp $
 *
 * Purpose: server part of an IPC connection
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
#pragma implementation "Server.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxServer
#include "wx.h"
#include "Sock.h"

// includes needed for IPC

#include <math.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#ifdef UNIX_ADDRESSING
#   include <sys/un.h>
#else
#   include <netdb.h>
#endif

/* MATTHEW: I don't want accept/bind errors on stderr */
#define NO_IPC_ERRORS

// static functions
static int  wx_socket_create(int port);

//-----------------------------------------------------------------------------
// Server create and destroy
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxServer, wxIPCObject)

wxServer::wxServer(void)
{
    __type = wxTYPE_DDE_SERVER;

    topLevelConnection = NULL;
}

Bool wxServer::Create(char *server_name)
{
    service_name = copystring(server_name);

    // Under UNIX, server should be an integer inside a string!
    int the_port = 0;
    sscanf(server_name, "%d", &the_port);

    /* Create a socket listening on specified port */
    int server_socket = wx_socket_create(the_port);
    if (server_socket < 0)
	return FALSE;

    Bool notify_toplevel = FALSE;
    wxConnection *toplevel = this->OnAcceptConnection ("STDIO");
    if (toplevel) {
	toplevel->input_fd = 0;
	toplevel->output_fd = 1;
	notify_toplevel = TRUE;
	toplevel->topic_name = copystring ("STDIO");
    } else
	toplevel = DEBUG_NEW wxConnection (NULL, 0); // Create a dummy connection

    topLevelConnection = toplevel; // TEST CODE

    toplevel->server = this;
    toplevel->input_fd = server_socket;

    connections.Append(toplevel);

    /* Register stdin (IF APP ALLOWS IT) and the socket with the notifier */

    toplevel->xtInputId
	= XtAppAddInput(wxAPP_CONTEXT,
			server_socket,
			(XtPointer*)XtInputReadMask,
			(XtInputCallbackProc)wxServer::AcceptConnection,
			(XtPointer) toplevel);
    return TRUE;
}

wxServer::~wxServer(void)
{
}

//-----------------------------------------------------------------------------
// XtInputCallbackProc to trace activity on a filedescriptor
//-----------------------------------------------------------------------------

Bool wxServer::AcceptConnection(XtPointer client, int *fid, XtInputId *WXUNUSED(id))
{
    int fd = *fid;

    struct sockaddr_in addr;
    int newsock, addrlen = sizeof (addr);
    wxConnection *top_connection = (wxConnection*)client;

    /* Accept the connection, getting a new socket */
    SockMgr & mgr = SockMgr::create();

    newsock = mgr.accept(fd, (struct sockaddr*)&addr, &addrlen);
    if (newsock == -1) {
#ifndef NO_IPC_ERRORS
      wxError("Error in accept", "wxServer");
#endif
      return FALSE;
    }
    char buf[300];

    SafeSock * sock = mgr.getSock(newsock);  // accept() OK, so must be != 0.

    Bool fWhole = FALSE, fMore = FALSE;
    do {
      sock->read(buf, 300, fWhole, fMore);
    } while (!fWhole);

    if (buf[0] == wxCONNECT) {
	char *topic_name = copystring(buf + 1);
	/* Register new socket with the notifier */
	wxConnection *new_connection
	    = ((wxServer*)(top_connection->server))->OnAcceptConnection(topic_name);
	if (new_connection) {
	    // Acknowledge success
	    buf[0] = wxCONNECT;
	    buf[1] = 0;
	    sock->write(buf, 2);

	    new_connection->input_fd = newsock;
	    new_connection->output_fd = newsock;
	    new_connection->server = top_connection->server;
	    new_connection->topic_name = topic_name;
	    top_connection->server->connections.Append(new_connection);

	    new_connection->xtInputId
		= XtAppAddInput(wxAPP_CONTEXT, newsock,
				(XtPointer*)XtInputReadMask,
				(XtInputCallbackProc)wxConnection::InputReady,
				(XtPointer)new_connection);
	} else {
	    // Send failure message
	    buf[0] = wxFAIL;
	    buf[1] = 0;
	    sock->write(buf, 2);
	}
    }
    return FALSE;
}

//-----------------------------------------------------------------------------
// Create an internet socket listening on the specified port.
//-----------------------------------------------------------------------------

static int wx_socket_create(int port)
{
    struct sockaddr_in addr;
    int sock;

    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_ANY);
    addr.sin_port = htons(port);

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (bind(sock, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
#ifndef NO_IPC_ERRORS
	wxError("Error in bind", "wxServer");
#endif
	return -1;
    }
    listen(sock, 5);
    return sock;
}
