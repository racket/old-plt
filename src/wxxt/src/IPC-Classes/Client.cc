/*								-*- C++ -*-
 * $Id: Client.cc,v 1.1 1996/01/10 14:56:24 markus Exp $
 *
 * Purpose: client part of an IPC connection
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
#pragma implementation "Client.h"
#endif

#define  Uses_wxClient
#include "wx.h"
#include "Sock.h"

// includes needed for IPC

#include <math.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#ifdef UNIX_ADDRESSING
#   include <sys/un.h>
#else
#  if defined(__alpha)
extern "C" {
#  endif
#   include <netdb.h>
#  if defined(__alpha)
};
#  endif
#endif
#ifdef __sgi
#include <bstring.h>
#endif

#ifdef UNIX_ADDRESSING
    typedef union {
	struct sockaddr addr;
	struct sockaddr_un unaddr;
	struct sockaddr_in inaddr;
    } sockaddr_union;
#   define wxNAMELEN(s)\
	(wxFAMILY(s) == AF_INET ? \
	 sizeof(s.inaddr) : \
	 strlen(s.unaddr.sun_path)+sizeof(s.unaddr)-sizeof(s.unaddr.sun_path))
#else
    typedef union {
	struct sockaddr addr;
	struct sockaddr_in inaddr;
    } sockaddr_union;
#   define wxNAMELEN(s) sizeof(s.inaddr)
#endif
#define wxFAMILY(s) s.addr.sa_family

//-----------------------------------------------------------------------------
// Client create and destroy
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxClient, wxIPCObject)

wxClient::wxClient(void)
{
    __type = wxTYPE_DDE_CLIENT;
}

wxClient::~wxClient(void)
{
    wxNode *node = connections.First();
    while (node) {
	wxConnection *connection = (wxConnection *)node->Data();
	delete connection;  // Deletes the node implicitly (see ~wxConnection)
	node = connections.First();
    }
}


Bool wxClient::ValidHost(char *host)
{
    if (gethostbyname(host))
	return TRUE;
    else
	return FALSE;
}

wxConnection *wxClient::MakeConnection(char *host, char *server_name,
				       char *topic)
{
    int port;
    sscanf(server_name, "%d", &port);

    sockaddr_union s;
    struct hostent *server;
    int fd;

    bzero((caddr_t) & s.inaddr, sizeof (s.inaddr));
    server = gethostbyname(host);
    if (server == (struct hostent *) 0) {
	perror ("sockio: gethostbyname failed");
	return NULL;
    }
    bcopy((caddr_t)server->h_addr, (caddr_t)&s.inaddr.sin_addr,
	  server->h_length);
    s.inaddr.sin_family = AF_INET;
    s.inaddr.sin_port = htons(port);

    fd = socket(wxFAMILY(s), SOCK_STREAM, 0);
    if (fd < 0) {
	perror ("sockio: socket failed");
	return NULL;
    }

    SockMgr & mgr = SockMgr::create();
    
    if (mgr.connect (fd, &s.addr, wxNAMELEN(s)) == 0) {
	// Send topic name, and enquire whether this has succeeded
	char buf[200];

	buf[0] = wxCONNECT;
	strcpy(buf + 1, topic);

	SafeSock * sock = mgr.getSock(fd);  // connect() OK, so sock must be != 0.
	sock->write(buf, strlen(topic) + 2);

	// Make sure we get a whole message.  Protocol ensures there will be
	// only one message at this point, so fMore is unused.
	Bool fWhole = FALSE, fMore = FALSE;
	while (!fWhole)
	  sock->read(buf, sizeof(buf) / sizeof(char) - 1, fWhole, fMore);

	// OK! Confirmation.
	if (buf[0] == wxCONNECT) {
	    wxConnection *connection = OnMakeConnection();
	    if (connection) {
		connections.Append(connection);
		connection->input_fd = fd;
		connection->output_fd = fd;
		connection->client = this;
		// Register with the notifier
		connection->Notify(TRUE);
		return connection;
	    } else {
		mgr.close (fd);
		return NULL;
	    }
	} else {
	    mgr.close (fd);
	    return NULL;
	}
    } else {
	mgr.close (fd);
	return NULL;
    }
}
