/*
 * File:      wx_ipc.cc
 * Purpose:     Interprocess communication implementation (X version)
 * Author:      Julian Smart
 * Created:     1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_ipc.cc,v 1.3 1994/08/14 21:28:43 edz Exp $
 * Copyright:   (c) 1993, AIAI, University of Edinburgh
 */

static const char sccsid[] = "@(#)wx_ipc.cc	1.2 5/9/94";

#ifdef __GNUG__
#pragma implementation
#pragma implementation "wx_ipcob.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#ifndef VMS
#include <memory.h>
#endif

#ifdef SVR4
//#ifndef wx_motif
#include <X11/Xfuncs.h> /* bzero, bcopy */
//#endif
#endif

#ifdef __sgi
#include <bstring.h>
#endif
#ifdef __hpux
#include <strings.h>
#endif

#include "common.h"

#if USE_IPC

#include "wx_utils.h"
#include "wx_ipc.h"

#ifdef wx_motif
#include "wx_main.h"
#endif

// added by steve, 05.09.94, VMS doesn't know about these includes
#ifdef VMS
#include <socket.h>
#include <in.h>
#include <signal.h>
#include <netdb.h>
#include <string.h>
#define bcopy(s1,s2,n) memcpy(s1,s2,n)
#define bzero(adr,siz) memset(adr,0,siz) 

#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>
#include <sys/wait.h>
#ifdef        UNIX_ADDRESSING
#include <sys/un.h>
#endif
#include <netdb.h>

#endif

#define wxFAMILY(s) s.addr.sa_family
#ifdef        UNIX_ADDRESSING
#define wxNAMELEN(s) (wxFAMILY(s) == AF_INET \
		    ? sizeof(s.inaddr) \
		    : strlen(s.unaddr.sun_path) + sizeof(s.unaddr) - sizeof(s.unaddr.sun_path))
#else
#define wxNAMELEN(s) sizeof(s.inaddr)
#endif

#ifdef VMS
int errno=0; // there is no defined global variable
#else
extern errno;
#endif

int wx_socket_create (int port);
// Add NULL-terminated string to buffer, returning next start point
int wxAddString (int start, char *info, char *buffer, int size = -1);

// Get next NULL-terminated string from buffer
char *wxGetNextString (char *buffer);

extern Bool wxIPCInitialized;

#ifdef wx_xview
#include <xview/xview.h>
//#include <xview/panel.h>
Notify_value wx_input_ready (Notify_client client, int fd);
Notify_value wx_accept_connection (Notify_client client, int fd);
#endif

#ifdef wx_motif
Bool wx_input_ready (XtPointer client, int *fid, XtInputId * id);
Bool wx_accept_connection (XtPointer client, int *fid, XtInputId * id);
#endif

/**************************************************************** _sm_ begin */
////////////////////////////////////////////////////////////////
//
// Description:
//   A first cut at a fix for the socket-related problems in wxWindows.
//   The basic problem is that TCP does not preserve message boundaries --
//   only the beginning of a message might arrive, all of it might, or
//   several messages may arrive at once -- but the wxWindows code does
//   not know this.
//
// Strategy:
//   Wrap all information passed between the client and server with header
//   and trailer information.  In particular, this wrapper info contains
//   the length of the message, so we know when the whole thing has arrived.
//   This implementation can also signal that more than one full message
//   is waiting.  Looks like this:
//   0000089a|...2202 (== hex 89a) bytes of data...ROGR
//
// Pluses:
//   -- You can write (more) robust, (more) reliable IPC code under wxWindows.
//      A Good Thing!
//   -- Not a single line of code outside of wx_ipc.cc is affected.
//      One of my strongest goals was to minimize changes to existing code.
//
// Minuses/Caveats:
//   -- Small writes are rendered somewhat inefficient (though not as
//      inefficient has having your whole program hang!).
//   -- Tested for Motif only.
//   -- The code is not "safe" enough for my tastes -- you could read/write
//      sockets unsafely, for instance.
//

#include <ctype.h>  // Need isxdigit().

// Manage a file descriptor.  Wrap data on writes and unwrap it on reads.
class SafeSock
{
    const int fd;                     // The fd that this SafeSock manages.
    Bool ownFd;                       // If true, close fd in dtor.
    char * inBox;                     // Accumulates incoming messages.
    size_t inBoxLen;                  // strlen of preceding.

    static const char * const MAGIC;  // Magic sequence must appear at msg end.
    static const int MAGIC_LEN;       // strlen of preceding.
    static const int COUNT_LEN;       // Exact # digits in byte-count wrapper.
    static const int OVRHD_LEN;       // Total bytes of overhead info per write.
    static const char SEP_CHAR;       // Magic char to separate count from data.

public:
    // Try to find a message in the byte sequence beginning at start and
    // having the past-the-end value end.  If found, set start and end
    // to the start and past-the-end positions of the message and return
    // TRUE; else return FALSE with no guarantees about the contents of
    // start and end.
    Bool findMsg(char * & start, char * & end);

    friend class SockMgr;

    // PRIVATE!  May only be created/destroyed by friend class SockMgr.
    SafeSock(int fd_, Bool ownFd_ = FALSE):
        fd(fd_), ownFd(ownFd_), inBox(new char [1]), inBoxLen(0)
    {
	*inBox = 0;
    }

    ~SafeSock(void)
    {
	delete [] inBox;  inBox = 0;
	if (ownFd)
	    close(fd);
    }


    // Write buf to the fd.  Return value is the same as write(2), except
    // that we add some wrapper info that will be included in the count of
    // the number of bytes written.  A complicating factor is that buf
    // need not be simply NUL-terminated text -- it may contain embedded
    // NULs, or it may have none at all.
    int write(char * buf, int nBytes)
    {
	// Prepare a real buf with our wrapper info.
	int realLen = nBytes + OVRHD_LEN;
	char * realBuf = new char [realLen+1];

	// Begin with byte count + separation char ....
	sprintf(realBuf, "%08lx%c", (unsigned long) nBytes, SEP_CHAR);

	// Now copy the "real" data.
	char * p = realBuf + COUNT_LEN + 1;
	bcopy(buf, p, nBytes);

	// Finish off with magic sequence.
	strcpy(p + nBytes, MAGIC);

	// Write the real buf and free its memory.
	int nWritten = ::write(fd, realBuf, realLen);
	delete [] realBuf;  realBuf = 0;

	return nWritten;
    }


    // Read buf from the fd.  I use isWhole and isMore to propagate
    // info to the caller:
    //   isWhole tells you whether you have a complete and good incoming
    //     message (if FALSE, call this fn again), and
    //   isMore is TRUE if the same read yielded more than one complete
    //     message (if TRUE, you should process this message and then read
    //     again).
    // This imposes a certain protocol on the caller, but nobody uses it
    // outside of wx_ipc.cc, so its ugliness is perhaps bearable.
    int read(char * buf, int nBytes, Bool & isWhole, Bool & isMore)
    {
	int nRead = 0;

	// If isMore == TRUE, caller is getting second (or later) message from
	// earlier read.  Otherwise, read from fd.
	if (!isMore)
	{
	    nRead = ::read(fd, buf, nBytes);
	    if (nRead <= 0)
	    {
		isWhole = TRUE;
		isMore = FALSE;
		return nRead;
	    }
	    // Append buf to inBox.
	    int newLen = inBoxLen + nRead;
	    char * newBox = new char [newLen];
/* Causes a strange error message for Solaris compiler
	    bcopy(inBox, newBox, inBoxLen);
	    bcopy(buf, newBox + inBoxLen, nRead);
*/
            int i;
            for (i = 0; i < inBoxLen; i++)
               newBox[i] = inBox[i];
            for (i = 0; i < nRead; i++)
               newBox[inBoxLen + i] = buf[i];

	    delete [] inBox;  inBox = newBox;  newBox = 0;
	    inBoxLen = newLen;
	}

	isWhole = isMore = FALSE;  // Assumptions.

	// Is a complete message in inBox?  If not, just return 0.
	char * start = inBox, * end = start + inBoxLen;
	if (!findMsg(start, end))
	    return 0;

	// Good, we have at least one message.  Place it in buf.
	int msgLen = end - start;  // Should really be an unsigned long.
	isWhole = TRUE;
	bcopy(start, buf, msgLen);
	buf[msgLen] = 0;

	// Remove this message (and any preceding crud) from inBox.
	end += MAGIC_LEN;  // Point past MAGIC sequence.

	char * oldBox = inBox;
	inBoxLen -= end - inBox;  // NOT (end - start)!
	inBox = new char[inBoxLen];
	bcopy(end, inBox, inBoxLen);
	delete [] oldBox;  oldBox  = 0;

	// Is there another complete message in inBox?
	start = inBox;  end = start + inBoxLen;
	isMore = findMsg(start, end);

	return msgLen;
    }

    int getFd(void)  {  return fd;  }
};

Bool SafeSock::findMsg(char * & start, char * & end)
    {
	// Try to find beginning of count (first digit in pointed-to range).
	while ((start < end) && !isxdigit(*start))
	    ++start;

	if (start >= end)
	    return FALSE;

	// start currently points at first digit.  Make sure we have
	// COUNT_LEN digits followed by SEP_CHAR.
	char * p = start + 1;

	while ((p != end) && isxdigit(*p) && ((p - start) < COUNT_LEN))
	    ++p;
	if (p == end)
	    return FALSE;

	// Now p is pointing one past the count, which should contain
	// the sep char.  If not, try again with a later start.
	if ((*p != SEP_CHAR) || ((p - start) != COUNT_LEN))
	    return findMsg(++start, end);

	*p = 0;
	unsigned long msgLen;
	sscanf(start, "%08lx", &msgLen);
	*p = SEP_CHAR;

	// Point start at first char of what we tentatively believe is a
	// message; try to find message end.
	start = p + 1;

	// See if there is enough room for this message.  If not, it may
	// not be all here or we may have been fooled by some crud; try again.
	if ((end - start) < (msgLen + MAGIC_LEN))
	    return findMsg(start, end);

	// Insist on magic sequence to help ensure that we have a real message.
	if (strncmp(start + msgLen, MAGIC, MAGIC_LEN))
	    return findMsg(start, end);

	end = start + msgLen;
	return TRUE;
    }

const char * const SafeSock::MAGIC = "ROGR";  // No digits allowed (dec or hex).
const int SafeSock::COUNT_LEN = 8;            // Exact # of hex digits in count.
const char SafeSock::SEP_CHAR = '|';          // No digits allowed (dec or hex).

// Some compilers don't like this kind of initialisation, so
// replace with actual values.
// const int SafeSock::MAGIC_LEN = strlen(SafeSock::MAGIC);
const int SafeSock::MAGIC_LEN = 4;
//const int SafeSock::OVRHD_LEN = COUNT_LEN + 1 + MAGIC_LEN;
const int SafeSock::OVRHD_LEN = 13;


class SockMgr
{
    static SockMgr * master;  // A ptr to the sole SockMgr (or 0 if none).
    wxList socks;             // A list of ptrs to SafeSock objects.

    // PRIVATE!  Instances are created/destroyed only via the static mfns.
    // This is so we can force uniqueness: only one SockMgr may exist at once.
    // This triggers a g++ warning ("only a private ctor/dtor and no friends");
    // ignore it.
    SockMgr(const SockMgr &);                // No fn body: prevent copying.
    SockMgr & operator = (const SockMgr &);  // No fn body: prevent assignment.
    SockMgr(void)  {  if (!master)  master = this;  }
    ~SockMgr(void)
    {
	// Close any sockets remaining in the list, deleting the objects.
	for (wxNode * n = socks.First(); n; n = n->Next())
	{
	    SafeSock * sock = (SafeSock *) (n->Data());
	    close(sock->getFd());
	    delete sock;  sock = 0;
	}

	master = 0;
    }

    // Add a SafeSock object managing the given fd to the list, unless such
    // an object already exists (which it should not!).
    void addSock(int fd)
    {
	SafeSock * sock = getSock(fd);
	if (!sock)
	    socks.Append((wxObject *) (new SafeSock(fd)));
    }

public:
    // Return a reference to the master socket manager, creating if necessary.
    static SockMgr & create(void)
    {
	return *(master  ?  master  :  new SockMgr);
    }

//  Should be called when app terminates.  Since there is as yet no good way
//  to determine when the app is terminating, the mfn is commented out for now.
//  It would be nice to have a wxApp::AtExit(), which you could use to place
//  a fn on a list of fns to be called back during ~wxApp().
    // Destroy master SockMgr.  Note that this invalidates all existing refs!
//    static void destroy(void)  {  delete master;  master = 0;  }

    // Return ptr to SafeSock object corresponding to fd, or 0 if not found.
    SafeSock * getSock(int fd);

    // Close the given fd; if successful, delete the corresponding list node.
    // Args and return value are the same as for close(2).
    int close(int fd);

    // Attempt to create a socket connection on the given fd.
    // Args and return value are the same as connect(2).
    int connect(int fd, struct sockaddr * name, int nameLen)
    {
	int rval = ::connect(fd, name, nameLen);
	if (!rval)
	    addSock(fd);
	return rval;
    }

    // Attempt to accept a socket connection on the given fd.
    // Args and return value are the same as connect(2).
    int accept(int fd, struct sockaddr * addr, int * addrLen)
    {
	int rval = ::accept(fd, addr, addrLen);
	if (rval > 0)
	    addSock(rval);
	return rval;
    }

    // Read/write through the SockMgr.  For single reads/writes, this is
    // convenient.  But if you have multiple reads/writes to do on the same
    // fd, it is more efficient to call getSock() and then use the SafeSock
    // directly (that way avoids a list search per operation).  On failure,
    // these return -1 without setting errno!
    int read(int fd, char * buf, int nBytes, Bool & isWhole, Bool & isMore);
    int write(int fd, char * buf, int nBytes);
};

SafeSock * SockMgr::getSock(int fd)
    {
	for (wxNode * n = socks.First(); n; n = n->Next())
	{
	    SafeSock * sock = (SafeSock *) (n->Data());
	    if (sock->getFd() == fd)
		return sock;
	}

	return 0;
    }

int SockMgr::close(int fd)
    {
	int rval = ::close(fd);

	// If close succeeded, delete corresponding list node.
	// May be no corresponding node, if fd is STDI/O rather than socket.
	if (!rval)
	{
	    for (wxNode * n = socks.First(); n; n = n->Next())
	    {
		SafeSock * sock = (SafeSock *) (n->Data());
		if (sock->getFd() == fd)
		{
		    delete sock;  sock = 0;
		    socks.DeleteNode(n);
		    break;
		}
	    }
	}

	return rval;
    }

int SockMgr::read(int fd, char * buf, int nBytes, Bool & isWhole, Bool & isMore)
    {
	SafeSock * sock = getSock(fd);
	if (!sock)
	    return -1;

	return sock->read(buf, nBytes, isWhole, isMore);
    }

int SockMgr::write(int fd, char * buf, int nBytes)
    {
	SafeSock * sock = getSock(fd);
	if (!sock)
	    return -1;

	return sock->write(buf, nBytes);
    }

SockMgr * SockMgr::master = 0;

/**************************************************************** _sm_ end   */

/*
 * Initialization
 *
 */

void 
wxIPCInitialize (void)
{
  if (wxIPCInitialized)
    return;
  wxIPCInitialized = TRUE;
}

IMPLEMENT_ABSTRACT_CLASS(wxIPCObject, wxObject)

wxIPCObject::wxIPCObject (void)
{
  service_name = NULL;
}

wxIPCObject::~wxIPCObject (void)
{
}

/*
 * Server
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxServer, wxIPCObject)

wxServer::wxServer (void)
{
  topLevelConnection = NULL;
//  server_socket = -1;
}

Bool wxServer::Create (char *server_name)
{
  service_name = copystring (server_name);

  // Under UNIX, server should be an integer inside a string!
  int the_port = 0;
  the_port = atoi(server_name);

  /* Create a socket listening on specified port */
  int server_socket = wx_socket_create (the_port);
  if (server_socket < 0)
    return FALSE;

  Bool notify_toplevel = FALSE;
  wxConnection *toplevel = this->OnAcceptConnection ("STDIO");
  if (toplevel)
    {
      toplevel->input_fd = 0;
      toplevel->output_fd = 1;
      notify_toplevel = TRUE;
      toplevel->topic_name = copystring ("STDIO");
    }
  else
    toplevel = new wxConnection (NULL, 0);	// Create a dummy connection

  topLevelConnection = toplevel;	// TEST CODE

  toplevel->server = this;
  toplevel->input_fd = server_socket;

  connections.Append (toplevel);

  /* Register stdin (IF APP ALLOWS IT) and the socket with the notifier */

#ifdef wx_xview
  // stdin
  if (notify_toplevel)
    notify_set_input_func ((Notify_client) toplevel, (Notify_func) & wx_input_ready, 0);

  // Top level port
  notify_set_input_func ((Notify_client) toplevel, (Notify_func) & wx_accept_connection, server_socket);
#endif
#ifdef wx_motif
  toplevel->xtInputId = XtAppAddInput (wxTheApp->appContext, server_socket, (XtPointer *) XtInputReadMask, (XtInputCallbackProc) wx_accept_connection, (XtPointer) toplevel);
#endif
  return TRUE;
}

wxServer::~wxServer (void)
{
/*
   if (server_socket > -1)
   {
#ifdef wx_xview
   notify_set_input_func((Notify_client)topLevelConnection, NOTIFY_FUNC_NULL, server_socket);
#endif
#ifdef wx_motif
   if (xtInputId > -1) XtRemoveInput(xtInputId);
#endif
   SockMgr::create().close(server_socket);
   }
 */
}

/*
 * Client
 *
 */


IMPLEMENT_DYNAMIC_CLASS(wxClient, wxIPCObject)

wxClient::wxClient (void)
{
}

wxClient::~wxClient (void)
{
}

Bool wxClient::ValidHost (char *host)
{
  if (gethostbyname (host))
    return TRUE;
  else
    return FALSE;
}

wxConnection *wxClient::MakeConnection (char *host, char *server_name, char *topic)
{
  int port;
  port = atoi(server_name);

  union
    {
      struct sockaddr addr;
#ifdef        UNIX_ADDRESSING
      struct sockaddr_un unaddr;
#endif
      struct sockaddr_in inaddr;
    }
  s;

  struct hostent *server;

  int fd;

  bzero ((caddr_t) & s.inaddr, sizeof (s.inaddr));
  server = gethostbyname (host);
  if (server == (struct hostent *) 0)
    {
      perror ("sockio: gethostbyname failed");
      return NULL;
    }
  bcopy ((caddr_t) server->h_addr, (caddr_t) & s.inaddr.sin_addr, server->h_length);
  s.inaddr.sin_family = AF_INET;
  s.inaddr.sin_port = htons (port);

  fd = socket (wxFAMILY (s), SOCK_STREAM, 0);
  if (fd < 0)
    {
      perror ("sockio: socket failed");
      return NULL;
    }

  SockMgr & mgr = SockMgr::create();
  if (mgr.connect (fd, &s.addr, wxNAMELEN (s)) == 0)
    {
      // Send topic name, and enquire whether this has succeeded
      char buf[200];

      buf[0] = wxCONNECT;
      strcpy (buf + 1, topic);

      SafeSock * sock = mgr.getSock(fd);  // connect() OK, so sock must be != 0.
      sock->write(buf, strlen (topic) + 2);

      // Make sure we get a whole message.  Protocol ensures there will be
      // only one message at this point, so fMore is unused.
      Bool fWhole = FALSE, fMore = FALSE;
      while (!fWhole)
	  sock->read(buf, sizeof (buf) / sizeof (char) - 1, fWhole, fMore);

      // OK! Confirmation.
      if (buf[0] == wxCONNECT)
	{
	  wxConnection *connection = OnMakeConnection ();
	  if (connection)
	    {
	      connections.Append (connection);
	      connection->input_fd = fd;
	      connection->output_fd = fd;
	      connection->client = this;

	      // Register with the notifier
	      connection->Notify (TRUE);
	      return connection;
	    }
	  else
	    {
	      mgr.close(fd);
	      return NULL;
	    }
	}
      else
	{
	  mgr.close(fd);
	  return NULL;
	}
    }
  else
    {
      mgr.close(fd);
      return NULL;
    }
/*
   if(errno == ENOENT) {
   fprintf(stderr, "socket doesn't exist, retrying after 5 secs\n");
   sleep(5);
   } else {
   perror("sockio: connect failed");
   exit(1);
   }
 */
}

/*
 * Connection
 */

IMPLEMENT_CLASS(wxConnection, wxObject)

wxConnection::wxConnection (char *buffer, int size):wxbConnection (buffer, size)
{
#ifdef wx_x
  input_fd = 0;
  output_fd = 0;
#endif
}

wxConnection::wxConnection (void)
{
#ifdef wx_x
  input_fd = 0;
  output_fd = 0;
#endif
#ifdef wx_motif
  xtInputId = 0;
#endif
}

wxConnection::~wxConnection (void)
{
#ifdef wx_motif
  if (xtInputId > 0)
    XtRemoveInput (xtInputId);
#endif
#ifdef wx_xview
  if (input_fd != 0)
    notify_set_input_func ((Notify_client) this, NOTIFY_FUNC_NULL, input_fd);
#endif
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

// Calls that CLIENT can make
Bool wxConnection::Disconnect (void)
{
  buf_ptr[0] = wxDISCONNECT;

  SockMgr::create().write(output_fd, buf_ptr, 1);
#ifdef wx_motif
  if (xtInputId)
    XtRemoveInput (xtInputId);
  xtInputId = 0;
#endif
#ifdef wx_xview
  notify_set_input_func ((Notify_client) this, NOTIFY_FUNC_NULL, input_fd);
#endif
  if (input_fd != 0)
      SockMgr::create().close(input_fd);
  if (output_fd != 0)
      SockMgr::create().close(output_fd);
  input_fd = 0;
  output_fd = 0;

  return TRUE;
}

Bool wxConnection::Execute (char *data, int size, int format)
{
  if (size < 0)
    size = strlen (data);

  char format_buf[10];
  sprintf (format_buf, "%d", format);

  buf_ptr[0] = wxEXECUTE;
  int pos = wxAddString (1, format_buf, buf_ptr);
  pos = wxAddString (pos, data, buf_ptr, size);

  SockMgr::create().write(output_fd, buf_ptr, pos);
  return TRUE;
}

char *wxConnection::Request (char *item, int *size, int format)
{
  char format_buf[10];
  sprintf (format_buf, "%d", format);

  buf_ptr[0] = wxREQUEST;
  int pos = wxAddString (1, item, buf_ptr);
  pos = wxAddString (pos, format_buf, buf_ptr);

#ifdef wx_xview
  Notify (FALSE);
#endif
  SockMgr & mgr = SockMgr::create();
  mgr.write(output_fd, buf_ptr, pos);

  Bool fWhole = FALSE, fMore = FALSE;
  while (!fWhole)
      mgr.read(input_fd, buf_ptr, buf_size, fWhole, fMore);
#ifdef wx_xview
  Notify (TRUE);
#endif

  if (buf_ptr[0] == wxFAIL)
    return NULL;
  else
    {
      char *new_item = buf_ptr + 1;
      char *data = wxGetNextString (new_item);
      if (size)
	*size = data - new_item;
      return data;
    }
}

Bool wxConnection::Poke (char *item, char *data, int size, int format)
{
  char format_buf[10];
  sprintf (format_buf, "%d", format);

  if (size < 0)
    size = strlen (data);

  buf_ptr[0] = wxPOKE;
  int pos = wxAddString (1, item, buf_ptr);
  pos = wxAddString (pos, format_buf, buf_ptr);
  pos = wxAddString (pos, data, buf_ptr, size);

  SockMgr::create().write(output_fd, buf_ptr, pos);
  return TRUE;
}

Bool wxConnection::StartAdvise (char *item)
{
  buf_ptr[0] = wxADVISE_START;
  int pos = wxAddString (1, item, buf_ptr);

#ifdef wx_xview
  Notify (FALSE);
#endif

  SockMgr & mgr = SockMgr::create();
  mgr.write(output_fd, buf_ptr, pos);

  Bool fWhole = FALSE, fMore = FALSE;
  while (!fWhole)
      mgr.read(input_fd, buf_ptr, buf_size, fWhole, fMore);

#ifdef wx_xview
  Notify (TRUE);
#endif

  if (buf_ptr[0] != wxFAIL)
    return TRUE;
  else
    return FALSE;
}

Bool wxConnection::StopAdvise (char *item)
{
  buf_ptr[0] = wxADVISE_STOP;
  int pos = wxAddString (1, item, buf_ptr);

#ifdef wx_xview
  Notify (FALSE);
#endif

  SockMgr & mgr = SockMgr::create();
  mgr.write(output_fd, buf_ptr, pos);

  Bool fWhole = FALSE, fMore = FALSE;
  while (!fWhole)
      mgr.read(input_fd, buf_ptr, buf_size, fWhole, fMore);

#ifdef wx_xview
  Notify (TRUE);
#endif

  if (buf_ptr[0] != wxFAIL)
    return TRUE;
  else
    return FALSE;
}

// Calls that SERVER can make
Bool wxConnection::Advise (char *item, char *data, int size, int format)
{
  char format_buf[10];
  sprintf (format_buf, "%d", format);

  buf_ptr[0] = wxADVISE;
  int pos = wxAddString (1, item, buf_ptr);
  pos = wxAddString (pos, format_buf, buf_ptr);
  pos = wxAddString (pos, data, buf_ptr, size);

  SockMgr::create().write(output_fd, buf_ptr, pos);
  return TRUE;
}

void wxConnection::Notify (Bool notify)
{
#ifdef wx_motif

  if (!notify)
    {
      if (xtInputId > 0)
	{
	  XtRemoveInput (xtInputId);
	  xtInputId = 0;
	}
    }
  else
    xtInputId = XtAppAddInput (wxTheApp->appContext, input_fd, (XtPointer *) XtInputReadMask, (XtInputCallbackProc) wx_input_ready, this);

#endif
#ifdef wx_xview
  if (notify)
    notify_set_input_func ((Notify_client) this, (Notify_func) wx_input_ready, input_fd);
  else
    notify_set_input_func ((Notify_client) this, NOTIFY_FUNC_NULL, input_fd);
#endif
}

#ifdef wx_xview
#define NOTIFY_RETURN_TYPE Notify_value
Notify_value 
wx_input_ready (Notify_client client, int fd)
#endif
#ifdef wx_motif
#define NOTIFY_RETURN_TYPE Bool
     Bool wx_input_ready (XtPointer client, int *fid, XtInputId * id)
#endif
{
#ifdef wx_motif
//  int fd = *fid;
#endif
  wxConnection *connection = (wxConnection *) client;

#ifdef wx_motif
  if (connection->xtInputId == 0)
    return (NOTIFY_RETURN_TYPE) FALSE;
#endif

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
	    return (NOTIFY_RETURN_TYPE) FALSE;
	    break;

	case 0:			/* EOF - de-register descriptor */
	{
	    connection->Notify (FALSE);
	    connection->OnDisconnect ();
	    return (NOTIFY_RETURN_TYPE) FALSE;
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
    return (NOTIFY_RETURN_TYPE) FALSE;
}

#ifdef wx_xview
Notify_value 
wx_accept_connection (Notify_client client, int fd)
#endif
#ifdef wx_motif
     Bool wx_accept_connection (XtPointer client, int *fid, XtInputId * id)
#endif
{
#ifdef wx_motif
  int fd = *fid;
#endif

  struct sockaddr_in addr;
  int newsock, addrlen = sizeof (addr);
  wxConnection *top_connection = (wxConnection *) client;

  /* Accept the connection, getting a new socket */

  SockMgr & mgr = SockMgr::create();
  newsock = mgr.accept(fd, (struct sockaddr *) &addr, &addrlen);
  if (newsock == -1)
    printf ("Error in accept\n");
  char buf[300];

  SafeSock * sock = mgr.getSock(newsock);  // accept() OK, so must be != 0.

  // Make sure we get the whole connect message.
  // fMore is unused here, since protocol ensures only one pending msg.
  Bool fWhole = FALSE, fMore = FALSE;
  do
  {
      sock->read(buf, 300, fWhole, fMore);
  } while (!fWhole);

  if (buf[0] == wxCONNECT)
    {
      char *topic_name = copystring (buf + 1);

      /* Register new socket with the notifier */
      wxConnection *new_connection = top_connection->server->OnAcceptConnection (topic_name);
      if (new_connection)
	{
	  // Acknowledge success
	  buf[0] = wxCONNECT;
	  buf[1] = 0;
	  sock->write(buf, 2);

	  new_connection->input_fd = newsock;
	  new_connection->output_fd = newsock;
	  new_connection->server = top_connection->server;
	  new_connection->topic_name = topic_name;
	  top_connection->server->connections.Append (new_connection);

#ifdef wx_xview
	  notify_set_input_func ((Notify_client) new_connection, (Notify_func) & wx_input_ready, newsock);
#endif
#ifdef wx_motif
	  new_connection->xtInputId = XtAppAddInput (wxTheApp->appContext, newsock, (XtPointer *) XtInputReadMask, (XtInputCallbackProc) wx_input_ready, (XtPointer) new_connection);
#endif
	}
      else
	{
	  // Send failure message
	  buf[0] = wxFAIL;
	  buf[1] = 0;
	  sock->write(buf, 2);
	}
    }
  return (NOTIFY_RETURN_TYPE) FALSE;
}

/*
 * Create an internet socket listening on the specified port.
 */

int 
wx_socket_create (int port)
{
  struct sockaddr_in addr;
  int sock;

  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl (INADDR_ANY);
  addr.sin_port = htons (port);

  sock = socket (AF_INET, SOCK_STREAM, 0);
  if (bind (sock, (struct sockaddr *) &addr, sizeof (addr)) == -1)
    {
      /* MATTHEW: [2] Don't always print an error; return value is enough */
      // printf ("Error in bind\n");
      return -1;
    }

  listen (sock, 5);

  return sock;
}


/*

#ifdef wx_xview
static Notify_value
wx_child_died (Notify_client me, int pid, union wait *status, struct rusage *rusage)
{
  if (WIFEXITED (*status))
    {
      wxChild *child = (wxChild *) me;
      child->OnDeath ();
      delete child;
      return NOTIFY_DONE;
    }
  else
    return NOTIFY_IGNORED;
}
#endif

// Pipes
wxChild::wxChild (void)
{
  the_pid = -1;
}

Bool wxChild::Create (char *command, char *argv[])
{
#ifdef wx_motif
  return FALSE;
#endif
#ifdef wx_xview
  int to_subprocess[2], from_subprocess[2];

  pipe (to_subprocess);
  pipe (from_subprocess);

  int pid = fork ();

  the_pid = pid;
  switch (pid)
    {
    case -1:
      return FALSE;

    case 0:			// child

      // Copy pipe descriptors to stdin and stdout
      dup2 (to_subprocess[0], 0);
      dup2 (from_subprocess[1], 1);

      // Close unwanted descriptors

      close (to_subprocess[0]);
      close (to_subprocess[1]);
      close (from_subprocess[0]);
      close (from_subprocess[1]);

      // Exec new process

      //   execlp("prolog", "prolog", (char *)0); execvp(command, argv);

      // If we get here it failed; give up

      perror ("exec");
      _exit (1);		// Use _exit() not exit() after failed exec

    default:			// parent

      break;
    }

  // Close unneeded descriptors

  close (to_subprocess[0]);
  close (from_subprocess[1]);

  (void) notify_set_wait3_func ((Notify_client) this, (Notify_func) wx_child_died, pid);

  wxConnection *connection = this->OnSpawn (pid);
  connection->input_fd = from_subprocess[0];
  connection->output_fd = to_subprocess[1];

  if (connection)
    {
      connection->Notify (TRUE);
      return TRUE;
    }
  else
    {
      close (to_subprocess[1]);
      close (from_subprocess[0]);
      return FALSE;
    }
#endif
#ifdef wx_msw
  return FALSE;
#endif
}

void wxChild::OnDeath (void)
{
}

   // Default behaviour
wxConnection *wxChild::OnSpawn (int pid)
{
  return new wxConnection;
}


*/

#endif // USE_IPC
