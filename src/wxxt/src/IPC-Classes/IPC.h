/*								-*- C++ -*-
 * $Id: IPC.h,v 1.1 1996/01/10 14:56:27 markus Exp $
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

#ifndef IPC_h
#define IPC_h

#ifdef __GNUG__
#pragma interface
#endif

class wxIPCObject : public wxObject {
DECLARE_DYNAMIC_CLASS(wxIPCObject)
public:
    wxIPCObject(void);
    ~wxIPCObject(void);
protected:
    friend class wxConnection;
    friend class wxServer;

    int lastError;
    char *service_name;
    wxList connections;
};

class wxConnection : public wxObject {
DECLARE_DYNAMIC_CLASS(wxConnection)
public:
    wxConnection(char *buffer, int size);
    wxConnection(void);
    ~wxConnection(void);

    // Callbacks to SERVER - override at will
    virtual Bool OnExecute(char *WXUNUSED(topic), char *WXUNUSED(data),
			   int WXUNUSED(size), int WXUNUSED(format))
	{ return FALSE; }
    virtual char *OnRequest(char *WXUNUSED(topic), char *WXUNUSED(item),
			    int *WXUNUSED(size), int WXUNUSED(format))
	{ return NULL; }
    virtual Bool OnPoke(char *WXUNUSED(topic), char *WXUNUSED(item),
			char *WXUNUSED(data), int WXUNUSED(size),
			int WXUNUSED(format))
	{ return FALSE; }
    virtual Bool OnStartAdvise(char *WXUNUSED(topic), char *WXUNUSED(item))
	{ return TRUE; }
    virtual Bool OnStopAdvise(char *WXUNUSED(topic), char *WXUNUSED(item))
	{ return TRUE; }

    // Callbacks to CLIENT - override at will
    virtual Bool OnAdvise(char *WXUNUSED(topic), char *WXUNUSED(item),
			  char *WXUNUSED(data), int WXUNUSED(size),
			  int WXUNUSED(format))
	{ return FALSE; }

    // Callbacks to BOTH

    // Default behaviour is to delete connection and return TRUE
    virtual Bool OnDisconnect(void)
	{ delete this; return TRUE; }

    // Calls that CLIENT can make
    virtual Bool Execute(char *data, int size = -1,
			 int format = wxCF_TEXT);
    virtual char *Request(char *item, int *size = NULL,
			  int format = wxCF_TEXT);
    virtual Bool Poke(char *item, char *data, int size = -1,
		      int format = wxCF_TEXT);
    virtual Bool StartAdvise(char *item);
    virtual Bool StopAdvise(char *item);

    // Calls that SERVER can make
    virtual Bool Advise(char *item, char *data, int size = -1,
			int format = wxCF_TEXT);

    // Calls that both can make
    virtual Bool Disconnect(void);
    virtual void Notify(Bool notify);
private:
#   ifdef Have_Xt_Types
    static Bool InputReady(XtPointer, int*, XtInputId*);
#   endif
    friend class wxClient;
    friend class wxServer;

    char          *buf_ptr;
    char          *topic_name;
    int           buf_size;
    unsigned long xtInputId;
    int           input_fd;
    int           output_fd;
    wxIPCObject*  server;
    wxIPCObject*  client;
};

void wxIPCInitialize(void);
void wxIPCCleanUp(void);

#endif // IPC_h
