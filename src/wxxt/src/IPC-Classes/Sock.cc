/*								-*- C++ -*-
 * $Id: Sock.cc,v 1.1 1996/01/10 14:56:27 markus Exp $
 *
 * Purpose: server part of an IPC connection
 *
 * Authors: Markus Holzem and Julian Smart and _sm_
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
#pragma implementation "Sock.h"
#endif

#include <math.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <signal.h>
#include <string.h>
#include <sys/wait.h>
#ifdef __sgi
#include <bstring.h>
#endif
#ifdef UNIX_ADDRESSING
#   include <sys/un.h>
#else
#   include <netdb.h>
#endif
#define Uses_wxList
#include "wx.h"
#include "Sock.h"

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

// PRIVATE!  May only be created/destroyed by friend class SockMgr.
SafeSock::SafeSock(int fd_, Bool ownFd_)
: fd(fd_), ownFd(ownFd_), inBox(new char [1]), inBoxLen(0)
{
  *inBox = 0;
}

SafeSock::~SafeSock(void)
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
int SafeSock::write(char * buf, int nBytes)
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
int SafeSock::read(char * buf, int nBytes, Bool & isWhole, Bool & isMore)
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

// Try to find a message in the byte sequence beginning at start and
// having the past-the-end value end.  If found, set start and end
// to the start and past-the-end positions of the message and return
// TRUE; else return FALSE with no guarantees about the contents of
// start and end.
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


SockMgr::~SockMgr(void)
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
void SockMgr::addSock(int fd)
{
  SafeSock * sock = getSock(fd);
  if (!sock)
    socks.Append((wxObject *) (new SafeSock(fd)));
}

// Return a reference to the master socket manager, creating if necessary.
SockMgr & SockMgr::create(void)
{
  return *(master  ?  master  :  new SockMgr);
}

//  Should be called when app terminates.  Since there is as yet no good way
//  to determine when the app is terminating, the mfn is commented out for now.
//  It would be nice to have a wxApp::AtExit(), which you could use to place
//  a fn on a list of fns to be called back during ~wxApp().
// Destroy master SockMgr.  Note that this invalidates all existing refs!
//    static void destroy(void)  {  delete master;  master = 0;  }

// Attempt to create a socket connection on the given fd.
// Args and return value are the same as connect(2).
int SockMgr::connect(int fd, struct sockaddr * name, int nameLen)
{
  int rval = ::connect(fd, name, nameLen);
  if (!rval)
    addSock(fd);
  return rval;
}

// Attempt to accept a socket connection on the given fd.
// Args and return value are the same as connect(2).
int SockMgr::accept(int fd, struct sockaddr * addr, int * addrLen)
{
  int rval = ::accept(fd, addr, addrLen);
  if (rval > 0)
    addSock(rval);
  return rval;
}

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

// Read/write through the SockMgr.  For single reads/writes, this is
// convenient.  But if you have multiple reads/writes to do on the same
// fd, it is more efficient to call getSock() and then use the SafeSock
// directly (that way avoids a list search per operation).  On failure,
// these return -1 without setting errno!

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
