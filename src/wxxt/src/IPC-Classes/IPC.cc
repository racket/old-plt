/*								-*- C++ -*-
 * $Id: IPC.cc,v 1.1 1996/01/10 14:56:26 markus Exp $
 *
 * Purpose: basic IPC classes
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
#pragma implementation "IPC.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxIPC
#include "wx.h"
#include "Sock.h"

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

// static functions
static int  wxAddString(int start, char *info, char *buffer, int size = -1);
static char *wxGetNextString(char *buffer);

//-----------------------------------------------------------------------------
// Initialization
//-----------------------------------------------------------------------------

void wxIPCInitialize (void)
{
    if (wxIPCInitialized)
	return;
    wxIPCInitialized = TRUE;
}

void wxIPCCleanUp()
{
    if (wxDefaultIPCBuffer)
	delete [] wxDefaultIPCBuffer ;
}

//-----------------------------------------------------------------------------
// wxIPCObject: base class for server and client
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxIPCObject, wxObject)

wxIPCObject::wxIPCObject(void)
{
    service_name = NULL;
}

wxIPCObject::~wxIPCObject(void)
{
    if (service_name)
	delete[] service_name;

    wxNode *node = connections.First();
    while (node) {
	wxConnection *connection = (wxConnection*)node->Data();
	wxNode *next = node->Next();
	connection->OnDisconnect(); // May delete the node implicitly
	node = next;
    }

    // If any left after this, delete them
    node = connections.First();
    while (node) {
	wxConnection *connection = (wxConnection*)node->Data();
	delete connection;
	node = connections.First();
    }
}

//-----------------------------------------------------------------------------
// Connection create and destroy
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxConnection, wxObject)

wxConnection::wxConnection(char *buffer, int size)
{
    if (buffer == NULL) {
	if (wxDefaultIPCBuffer == NULL)
	    wxDefaultIPCBuffer = new char[wxDefaultIPCBufferSize];
	buf_ptr = wxDefaultIPCBuffer;
	buf_size = wxDefaultIPCBufferSize;
    } else {
	buf_ptr = buffer;
	buf_size = size;
    }
    topic_name = NULL;

    client = NULL;
    server = NULL;

    input_fd = 0;
    output_fd = 0;
    xtInputId = 0;
}

wxConnection::wxConnection(void)
{
    if (wxDefaultIPCBuffer == NULL)
	wxDefaultIPCBuffer = new char[wxDefaultIPCBufferSize];

    buf_ptr = wxDefaultIPCBuffer;
    buf_size = wxDefaultIPCBufferSize;
    topic_name = NULL;

    client = NULL;
    server = NULL;

    input_fd = 0;
    output_fd = 0;
    xtInputId = 0;
}

wxConnection::~wxConnection(void)
{
    if (xtInputId > 0)
	XtRemoveInput(xtInputId);
    if (input_fd != 0)
	SockMgr::create().close(input_fd);
    if (output_fd != 0)
	SockMgr::create().close(output_fd);

    if (server)
	server->connections.DeleteObject (this);
    if (client)
	client->connections.DeleteObject (this);
    if (topic_name)
	delete[]topic_name;
}

//-----------------------------------------------------------------------------
// Calls that CLIENT can make
//-----------------------------------------------------------------------------

Bool wxConnection::Execute(char *data, int size, int format)
{
    if (size < 0)
	size = strlen(data);

    char format_buf[10];
    sprintf(format_buf, "%d", format);

    buf_ptr[0] = wxEXECUTE;
    int pos = wxAddString(1, format_buf, buf_ptr);
    pos = wxAddString(pos, data, buf_ptr, size);

    SockMgr::create().write(output_fd, buf_ptr, pos);
    return TRUE;
}

char *wxConnection::Request(char *item, int *size, int format)
{
    char format_buf[10];
    sprintf(format_buf, "%d", format);

    buf_ptr[0] = wxREQUEST;
    int pos = wxAddString (1, item, buf_ptr);
    pos = wxAddString (pos, format_buf, buf_ptr);

    SockMgr & mgr = SockMgr::create();

    Bool fWhole = FALSE, fMore = FALSE;
    mgr.write(output_fd, buf_ptr, pos);
    mgr.read(input_fd, buf_ptr, buf_size, fWhole, fMore);

    if (buf_ptr[0] == wxFAIL)
	return NULL;
    else {
	char *new_item = buf_ptr + 1;
	char *data = wxGetNextString (new_item);
	if (size)
	    *size = data - new_item;
	return data;
    }
}

Bool wxConnection::Poke(char *item, char *data, int size, int format)
{
    char format_buf[10];
    sprintf(format_buf, "%d", format);

    if (size < 0)
	size = strlen(data);

    buf_ptr[0] = wxPOKE;
    int pos = wxAddString(1, item, buf_ptr);
    pos = wxAddString(pos, format_buf, buf_ptr);
    pos = wxAddString(pos, data, buf_ptr, size);

    SockMgr::create().write(output_fd, buf_ptr, pos);
    return TRUE;
}

Bool wxConnection::StartAdvise(char *item)
{
    buf_ptr[0] = wxADVISE_START;
    int pos = wxAddString(1, item, buf_ptr);

    SockMgr & mgr = SockMgr::create();

    Bool fWhole = FALSE, fMore = FALSE;
    mgr.write(output_fd, buf_ptr, pos);
    mgr.read(input_fd, buf_ptr, buf_size, fWhole, fMore);

    if (buf_ptr[0] != wxFAIL)
	return TRUE;
    else
	return FALSE;
}

Bool wxConnection::StopAdvise(char *item)
{
    buf_ptr[0] = wxADVISE_STOP;
    int pos = wxAddString(1, item, buf_ptr);

    SockMgr & mgr = SockMgr::create();

    Bool fWhole = FALSE, fMore = FALSE;
    mgr.write(output_fd, buf_ptr, pos);
    mgr.read(input_fd, buf_ptr, buf_size, fWhole, fMore);

    if (buf_ptr[0] != wxFAIL)
	return TRUE;
    else
	return FALSE;
}

//-----------------------------------------------------------------------------
// Calls that SERVER can make
//-----------------------------------------------------------------------------

Bool wxConnection::Advise(char *item, char *data, int size, int format)
{
    char format_buf[10];
    sprintf(format_buf, "%d", format);

    buf_ptr[0] = wxADVISE;
    int pos = wxAddString(1, item, buf_ptr);
    pos = wxAddString(pos, format_buf, buf_ptr);
    pos = wxAddString(pos, data, buf_ptr, size);

    SockMgr::create().write(output_fd, buf_ptr, pos);
    return TRUE;
}

//-----------------------------------------------------------------------------
// Calls that BOTH can make
//-----------------------------------------------------------------------------

Bool wxConnection::Disconnect(void)
{
    buf_ptr[0] = wxDISCONNECT;
    write(output_fd, buf_ptr, 1);
    if (xtInputId)
	XtRemoveInput(xtInputId);
    xtInputId = 0;
    if (input_fd != 0)
	SockMgr::create().close(input_fd);
    if (output_fd != 0)
	SockMgr::create().close(output_fd);
    input_fd = 0;
    output_fd = 0;

    return TRUE;
}

void wxConnection::Notify (Bool notify)
{
    if (!notify) {
	if (xtInputId > 0) {
	    XtRemoveInput (xtInputId);
	    xtInputId = 0;
	}
    } else
	xtInputId = XtAppAddInput(wxAPP_CONTEXT,
				  input_fd,
				  (XtPointer *)XtInputReadMask,
				  (XtInputCallbackProc)(wxConnection::InputReady),
				  this);
}

//-----------------------------------------------------------------------------
// XtInputCallbackProc to trace activity on a filedescriptor
//-----------------------------------------------------------------------------

Bool wxConnection::InputReady(XtPointer client, int *WXUNUSED(fid),
			      XtInputId *WXUNUSED(id))
{
    wxConnection *connection = (wxConnection*)client;

    if (connection->xtInputId == 0)
	return FALSE;

    Bool isMore = FALSE;
    SockMgr & mgr = SockMgr::create();
    SafeSock * insock = mgr.getSock(connection->input_fd);
    do
    {
	int nread = 0;
	Bool isWhole = FALSE;
	do
	{
	    nread += insock->read(connection->buf_ptr, connection->buf_size,
		isWhole, isMore);
	} while ((nread > 0) && !isWhole);

	if ((nread >= 0) && (nread < connection->buf_size))
	    connection->buf_ptr[nread] = 0;

	switch (nread)
	{
	case -1:			/* Error - give up */
	    connection->Notify (FALSE);
	    connection->OnDisconnect ();
	    return FALSE;
	    break;

	case 0:			/* EOF - de-register descriptor */
	{
	    connection->Notify (FALSE);
	    connection->OnDisconnect ();
	    return FALSE;
	    break;
	}
	default:
	    break;
	}

	switch (connection->buf_ptr[0])
	{
	case wxEXECUTE:
	{
	    char *format_buf = connection->buf_ptr + 1;
	    char *data = wxGetNextString (format_buf);

	    int format = wxCF_TEXT;
	    format = atoi(format_buf);

	    int size = nread - (data - connection->buf_ptr);
	    connection->OnExecute (connection->topic_name, data, size, format);
	    break;
	}
	case wxADVISE:
        {
	    char *item = connection->buf_ptr + 1;
	    char *format_buf = wxGetNextString (item);;
	    char *data = wxGetNextString (format_buf);

	    int format = wxCF_TEXT;
	    format = atoi(format_buf);

	    int size = nread - (data - connection->buf_ptr);
	    connection->OnAdvise (connection->topic_name, item, data, size, format);
	    break;
	}
	case wxADVISE_START:
        {
	    char *item = connection->buf_ptr + 1;
	    Bool ok = connection->OnStartAdvise (connection->topic_name, item);
	    if (ok)
	    {
		connection->buf_ptr[0] = wxADVISE_START;
		connection->buf_ptr[1] = 0;
		mgr.write(connection->output_fd, connection->buf_ptr, 2);
	    }
	    else
	    {
		connection->buf_ptr[0] = wxFAIL;
		connection->buf_ptr[1] = 0;
		mgr.write(connection->output_fd, connection->buf_ptr, 2);
	    }

	    break;
	}
	case wxADVISE_STOP:
        {
	    char *item = connection->buf_ptr + 1;
	    Bool ok = connection->OnStopAdvise (connection->topic_name, item);
	    if (ok)
	    {
		connection->buf_ptr[0] = wxADVISE_STOP;
		connection->buf_ptr[1] = 0;
		mgr.write(connection->output_fd, connection->buf_ptr, 2);
	    }
	    else
	    {
		connection->buf_ptr[0] = wxFAIL;
		connection->buf_ptr[1] = 0;
		mgr.write(connection->output_fd, connection->buf_ptr, 2);
	    }

	    break;
	}
	case wxPOKE:
        {
	    char *item = connection->buf_ptr + 1;
	    char *format_buf = wxGetNextString (item);;
	    char *data = wxGetNextString (format_buf);

	    int format = wxCF_TEXT;
	    format = atoi(format_buf);

	    int size = nread - (data - connection->buf_ptr);
	    connection->OnPoke (connection->topic_name, item, data, size, format);
	    break;
	}
	case wxREQUEST:
        {
	    char *item = connection->buf_ptr + 1;
	    char *format_buf = wxGetNextString (item);;

	    int format = wxCF_TEXT;
	    format = atoi(format_buf);

	    int user_size = -1;
	    char *user_data = connection->OnRequest (connection->topic_name, item, &user_size, format);
	    if (user_data)
	    {
		connection->buf_ptr[0] = wxREQUEST_REPLY;
		int pos = wxAddString (1, item, connection->buf_ptr);
		pos = wxAddString (pos, user_data, connection->buf_ptr, user_size);

		mgr.write(connection->output_fd, connection->buf_ptr, pos);
	    }
	    else
	    {
		connection->buf_ptr[0] = wxFAIL;
		connection->buf_ptr[1] = 0;
		mgr.write(connection->output_fd, connection->buf_ptr, 2);
	    }
	    break;
	}
        default:
	    break;
        }
    } while(isMore);

    return FALSE;
}

//-----------------------------------------------------------------------------
// add and get string from buffer
//-----------------------------------------------------------------------------

static int wxAddString(int start, char *info, char *buffer, int size)
{
    if (size < 0)
	size = strlen(info);

    int i;
    for (i = start; i < (start + size); i++)
	buffer[i] = info[i-start];
    buffer[i] = 0;
    return i+1;
}

static char *wxGetNextString(char *buffer)
{
    int i = 0;
    int ch = -1;
    Bool flag = FALSE;
    while (!flag) {
	ch = (int)buffer[i];
	if (ch == 0)
	    flag = TRUE;
	else
	    i++;
    }
    return buffer+i+1;
}
