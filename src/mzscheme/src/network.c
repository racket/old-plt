/*
  MzScheme
  Copyright (c) 2000-2001 Matthew Flatt

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

  libscheme
  Copyright (c) 1994 Brent Benson
  All rights reserved.
*/

/* This file implements the TCP interface. */

#include "schpriv.h"
#include <ctype.h>

#ifndef NO_TCP_SUPPORT
#ifdef USE_TCP

#ifdef UNISTD_INCLUDE
# include <unistd.h>
#endif
#ifdef USE_ULIMIT
# include <ulimit.h>
#endif
#ifdef FILES_HAVE_FDS
# include <sys/types.h>
# include <sys/time.h>
# ifdef BSTRING_INCLUDE
#  include <bstring.h>
# endif
# ifdef SELECT_INCLUDE
#  include <sys/select.h>
# endif
#endif
#ifdef IO_INCLUDE
# include <io.h>
#endif
#ifdef INCLUDE_OSKIT_SOCKET
# include <oskit/net/socket.h>
#endif
#ifdef NO_ERRNO_GLOBAL
static int mzerrno = 0;
# define errno mzerrno
#else
# include <errno.h>
#endif
#include "schfd.h"

#ifdef USE_MAC_TCP
# define USESROUTINEDESCRIPTORS 1
# include <MacTCP.h>
# include "macdnr.inc"
#endif

#ifdef USE_UNIX_SOCKETS_TCP
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# include <fcntl.h>
# define TCP_SOCKSENDBUF_SIZE 32768
# define NOT_WINSOCK(x) x
# define SOCK_ERRNO() errno
#endif

#ifdef USE_WINSOCK_TCP
# include <process.h>
# include <winsock.h>
struct SOCKADDR_IN {
  short sin_family;
  unsigned short sin_port;
  struct in_addr sin_addr;
  char sin_zero[8];
};
# define NOT_WINSOCK(x) 0
# define SOCK_ERRNO() WSAGetLastError()
#endif

#ifdef USE_MAC_TCP
# define TCP_BUFFER_SIZE 16384
#else
# define TCP_BUFFER_SIZE 512
#endif

#ifdef USE_UNIX_SOCKETS_TCP
typedef long tcp_t;
# define INVALID_SOCKET (-1)
static void closesocket(long s) {
  int cr;
  do { 
    cr = close(s);
  } while ((cr == -1) && (errno == EINTR));
}
#endif

#ifdef USE_WINSOCK_TCP
typedef SOCKET tcp_t;
#endif

#ifdef USE_SOCKETS_TCP
typedef struct {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  tcp_t s;
  Scheme_Custodian_Reference *mref;
} listener_t;
#endif

#ifdef USE_MAC_TCP
typedef struct tcp_t {
  void *create_pb;
  void *current_pb; /* prevents GC during async call */
  StreamPtr stream;
  int state;
  int async_errid;
  Scheme_Object *lock; /* read lock */
} tcp_t;

typedef struct {
  Scheme_Type type; 
  MZ_HASH_KEY_EX
  int hostid;
  int portid;
  int count;
  struct Scheme_Tcp **datas;
  Scheme_Custodian_Reference *mref;
} listener_t;
# define htons(x) x
# define htonl(x) x
#endif

typedef struct Scheme_Tcp {
  Scheme_Tcp_Buf b;
  tcp_t tcp;
#ifdef USE_MAC_TCP
  struct TCPiopb *activeRcv;
#endif
  int flags;
} Scheme_Tcp;

# define MZ_TCP_ABANDON_OUTPUT 0x1
# define MZ_TCP_ABANDON_INPUT  0x2

#ifdef USE_MAC_TCP
static int num_tcp_send_buffers = 0;
static void **tcp_send_buffers;
#endif

#endif /* USE_TCP */

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_connect_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listen(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_stop(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept_ready(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_accept_break(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_addresses(int argc, Scheme_Object *argv[]);
static Scheme_Object *tcp_abandon_port(int argc, Scheme_Object *argv[]);

static void register_tcp_listener_wait();

#ifdef MZ_PRECISE_GC
static void register_traversers(void);
#endif

void scheme_init_network(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  register_traversers();
#endif

#ifdef USE_MAC_TCP
  REGISTER_SO(tcp_send_buffers);
#endif    

  scheme_add_global_constant("tcp-connect", 
			     scheme_make_prim_w_arity2(tcp_connect,
						       "tcp-connect", 
						       2, 2,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-connect/enable-break", 
			     scheme_make_prim_w_arity2(tcp_connect_break,
						       "tcp-connect/enable-break", 
						       2, 2,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listen", 
			     scheme_make_prim_w_arity(tcp_listen,
						      "tcp-listen", 
						      1, 4),
			     env);
  scheme_add_global_constant("tcp-close", 
			     scheme_make_prim_w_arity(tcp_stop,
						      "tcp-close", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept-ready?", 
			     scheme_make_prim_w_arity(tcp_accept_ready,
						      "tcp-accept-ready?", 
						      1, 1), 
			     env);
  scheme_add_global_constant("tcp-accept", 
			     scheme_make_prim_w_arity2(tcp_accept,
						       "tcp-accept", 
						       1, 1,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-accept/enable-break", 
			     scheme_make_prim_w_arity2(tcp_accept_break,
						       "tcp-accept/enable-break", 
						       1, 1,
						       2, 2), 
			     env);
  scheme_add_global_constant("tcp-listener?", 
			     scheme_make_folding_prim(tcp_listener_p,
						      "tcp-listener?", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("tcp-addresses", 
			     scheme_make_folding_prim(tcp_addresses,
						      "tcp-addresses", 
						      1, 1, 1), 
			     env);
  scheme_add_global_constant("tcp-abandon-port", 
			     scheme_make_prim_w_arity(tcp_abandon_port,
						      "tcp-abandon_port", 
						      1, 1), 
			     env);

  register_tcp_listener_wait();
}


/*========================================================================*/
/*                             TCP glue                                   */
/*========================================================================*/


/* These two need o be outside of USE_TCP */
#define PORT_ID_TYPE "exact integer in [1, 65535]"
#define CHECK_PORT_ID(obj) (SCHEME_INTP(obj) && (SCHEME_INT_VAL(obj) >= 1) && (SCHEME_INT_VAL(obj) <= 65535))


#ifdef USE_TCP

#ifdef USE_SOCKETS_TCP
#define MAKE_TCP_ARG tcp_t tcp, 
#else
#define MAKE_TCP_ARG
#endif

#define REGISTER_SOCKET(s) /**/
#define UNREGISTER_SOCKET(s) /**/

#ifdef USE_UNIX_SOCKETS_TCP
typedef struct sockaddr_in tcp_address;
#endif
#ifdef USE_WINSOCK_TCP
typedef struct SOCKADDR_IN tcp_address;
# undef REGISTER_SOCKET
# undef UNREGISTER_SOCKET
# define REGISTER_SOCKET(s) winsock_remember(s)
# define UNREGISTER_SOCKET(s) winsock_forget(s)
#endif

#ifdef USE_WINSOCK_TCP

/******************************* WinSock ***********************************/

static int wsr_size = 0;
static tcp_t *wsr_array;

static void winsock_remember(tcp_t s)
{
  int i, new_size;
  tcp_t *naya;

  for (i = 0; i < wsr_size; i++) {
    if (!wsr_array[i]) {
      wsr_array[i] = s;
      return;
    }
  }

  if (!wsr_size) {
    REGISTER_SO(wsr_array);
    new_size = 32;
  } else
    new_size = 2 * wsr_size;

  naya = MALLOC_N_ATOMIC(tcp_t, new_size);
  for (i = 0; i < wsr_size; i++) {
    naya[i] = wsr_array[i];
  }

  naya[wsr_size] = s;

  wsr_array = naya;
  wsr_size = new_size;  
}

static void winsock_forget(tcp_t s)
{
  int i;

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i] == s) {
      wsr_array[i] = (tcp_t)NULL;
      return;
    }
  }
}

static int winsock_done(void)
{
  int i;

  for (i = 0; i < wsr_size; i++) {
    if (wsr_array[i]) {
      closesocket(wsr_array[i]);
      wsr_array[i] = (tcp_t)NULL;
    }
  }

  return WSACleanup();
}

static void TCP_INIT(char *name)
{
  static int started = 0;
  
  if (!started) {
    WSADATA data;
    if (!WSAStartup(MAKEWORD(1, 1), &data)) {
      started = 1;
#ifdef __BORLANDC__
      atexit((void(*)())winsock_done);
#else      
      _onexit(winsock_done);
#endif
      return;
    }
  } else
    return;
  
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "%s: not supported on this machine"
		   " (no winsock driver)",
		   name);
}
#else
#ifdef USE_MAC_TCP

/***************************** Mac *******************************************/

/* Much of this is derived from (or at least influenced by) GUSI's TCP
   socket implementation, by Matthias Neeracher, which was derived from
   a library by Charlie Reiman Tom Milligan */

static short tcpDriverId;

static ProcessSerialNumber tcp_psn;

#define	SOCK_STATE_NO_STREAM 0 /* Socket doesn't have a MacTCP stream yet */
#define	SOCK_STATE_UNCONNECTED 1 /* Socket is unconnected. */
#define	SOCK_STATE_LISTENING 2 /* Socket is listening for connection. */
#define	SOCK_STATE_CONNECTING 4 /* Socket is initiating a connection. */
#define	SOCK_STATE_CONNECTED 5 /* Socket is connected. */
#define	SOCK_STATE_EOF_FROM_OTHER 6 /* Socket is half-closed */
#define	SOCK_STATE_CLOSED 8 /* Socket closed nicely */

typedef struct TCPiopbX {
  TCPiopb pb;
  Scheme_Tcp *data;
  struct TCPiopbX *next;
} TCPiopbX;

typedef struct {
  MZTAG_IF_REQUIRED
  wdsEntry e[2];
  TCPiopbX *xpb;
} WriteData;

static TCPiopbX *active_pbs;

static pascal void dnr_done(struct hostInfo *hi, int * done)
{
  *done = true;
  WakeUpProcess(&tcp_psn);
}

static ResultUPP u_dnr_done;

static pascal void tcp_notify(StreamPtr stream, unsigned short eventCode,
			      Ptr userDataPtr, unsigned short something,
			      struct ICMPReport *reportPtr)
{
  tcp_t *t = (tcp_t *)userDataPtr;

  switch (eventCode) {
  case TCPClosing:
    t->state = SOCK_STATE_EOF_FROM_OTHER;
    break;
    
  case TCPTerminate:
    if (t->state == SOCK_STATE_LISTENING)
      t->state = SOCK_STATE_CLOSED;
    else if (t->state == SOCK_STATE_EOF_FROM_OTHER)
      t->state = SOCK_STATE_CLOSED;
    else
      t->state = SOCK_STATE_UNCONNECTED;
    break;
  }

  WakeUpProcess(&tcp_psn);
}

static TCPNotifyUPP u_tcp_notify;

static void tcp_connect_done(TCPiopbX *pbx)
{
  if (!pbx->pb.ioResult)
    pbx->data->tcp.state = SOCK_STATE_CONNECTED;

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_connect_done;

static void tcp_listen_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  switch(pb->ioResult) {
  case noErr:
    data->tcp.state = SOCK_STATE_CONNECTED;
    break;
    
  case openFailed:
  case invalidStreamPtr:
  case connectionExists:
  case duplicateSocket:
  case commandTimeout:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_listen_done;

static void tcp_recv_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;
  
  if (!pb->ioResult)
    data->b.bufmax = pb->csParam.receive.rcvBuffLen;

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_recv_done;

static void tcp_send_done(TCPiopbX *pbx)
{
  TCPiopb *pb = &pbx->pb;
  Scheme_Tcp *data = pbx->data;

  /* mark it free: */
  ((WriteData *)(pb->csParam.send.wdsPtr))->xpb = NULL;

  switch (pb->ioResult) {
  case noErr:
    break;
  case ipNoFragMemErr:
  case connectionClosing:
  case connectionTerminated:
  case connectionDoesntExist:
  case ipDontFragErr:
  case invalidStreamPtr:
  case invalidLength:
  case invalidWDS:
  default:
    data->tcp.state = SOCK_STATE_UNCONNECTED;
    data->tcp.async_errid = -pb->ioResult;
    break;
  }

  WakeUpProcess(&tcp_psn);
}

static TCPIOCompletionUPP u_tcp_send_done;

/************** Mac Set-up *****************/

static void tcp_cleanup(void);

static pascal OSErr (*mzPBOpenSync)(ParmBlkPtr paramBlock);
static pascal OSErr (*mzPBControlSync)(ParmBlkPtr paramBlock);
static pascal OSErr (*mzPBControlAsync)(ParmBlkPtr paramBlock);

static void TCP_INIT(char *name)
{
  ParamBlockRec pb;
  short errNo;
  FSSpec spec;
  CFragConnectionID connID;
  OSErr err;
  void (*f)(...);
  char *netglue;

  GetCurrentProcess(&tcp_psn);

  netglue = scheme_get_exec_path();
  if (netglue) {
    char *file;
    int i;
    i = strlen(netglue);
    file = scheme_malloc_atomic(i + 20);
    memcpy(file, netglue, i);
    for (i--; (i >= 0) && (file[i] != ':'); i--) {
    }
    memcpy(file + i + 1, "netglue", 8);
    netglue = file;
  } else
    netglue = "netglue";

  if (!scheme_mac_path_to_spec(netglue, &spec))
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "%s: TCP initialization error (can't find %q)",
		     name, netglue);

  err = GetDiskFragment(&spec, 0, 0, 0, kPrivateCFragCopy, &connID, 0, NULL);
  if (err != noErr)
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "%s: TCP initialization error (can't load %q: %e)",
		     name, netglue, err);
  err = FindSymbol(connID, "\pFillInNetPointers", (Ptr *)&f, 0);
  if (err != noErr)
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "%s: TCP initialization error (can't get netglue function: %e)",
		     name, err);

  f(&mzPBOpenSync, &mzPBControlSync, &mzPBControlAsync, 
    &mzSysEnvirons, &mzGetWDInfo, &mzNewRoutineDescriptor,
    &mzCallUniversalProc);

  pb.ioParam.ioCompletion = 0L; 
  pb.ioParam.ioNamePtr = (StringPtr) "\p.IPP"; 
  pb.ioParam.ioPermssn = fsCurPerm;
  
  if ((errNo = mzPBOpenSync(&pb))) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "%s: TCP initialization error (at .IPP; %e)",
		     name, (int)errNo);
  }

  if ((errNo = OpenResolver(NULL))) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "%s: TCP initialization error (at resolver; %e)",
		     name, (int)errNo);
  }
		
  tcpDriverId = pb.ioParam.ioRefNum; 
  
  u_dnr_done = NewResultProc(dnr_done);
  u_tcp_notify = NewTCPNotifyProc(tcp_notify);
  u_tcp_connect_done = NewTCPIOCompletionProc(tcp_connect_done);
  u_tcp_listen_done = NewTCPIOCompletionProc(tcp_listen_done);
  u_tcp_recv_done = NewTCPIOCompletionProc(tcp_recv_done);
  u_tcp_send_done = NewTCPIOCompletionProc(tcp_send_done);
  
  REGISTER_SO(active_pbs);
  
  atexit(tcp_cleanup);
}

static int tcp_addr(const char *address, struct hostInfo *info)
{
  int tries = 3;
  long *done = MALLOC_ONE_ATOMIC(long);
  
  /* Check for numerical address: */
  {
     unsigned char *s = (unsigned char *)address, n[4];
     int p = 0, v = 0;
     while (*s) {
       if (isdigit(*s)) {
         if (v < 256)
           v = (v * 10) + (*s - '0');
       } else if (*s == '.') {
         if (p < 4) {
           n[p] = v;
           p++;
         }
         v = 0;
       } else
         break;
       s++;
     }
     
     if (p == 3) {
       n[p] = v;
       p++;
     }
     
     if (!*s && (p == 4)
         && (s[0] < 256) && (s[1] < 256)
         && (s[2] < 256) && (s[3] < 256)) {
       /* Numerical address */
       info->addr[0] = *(unsigned long *)n;
       return 0;
     }
  }
  
 try_again:
  *done = 0;
  info->rtnCode = 0;
  if (StrToAddr((char *)address, info, u_dnr_done, (char *)done) == cacheFault) {
    /* FIXME: If we get a break, it's possible that `info' and `done' will be
              GCed before the async call completes. */
    while (!*done) { scheme_thread_block(0.25); }
    scheme_current_thread->ran_some = 1;
  }
  if (info->rtnCode == cacheFault) {
    if (--tries)
      goto try_again;
  }
  if (info->rtnCode)
    return info->rtnCode;
  if (info->cname[0] == 0)
    return -42;
  
  return 0;
}

/* Forward prototype: */
static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount);

#define STREAM_BUFFER_SIZE 131072

static TCPiopbX *mac_make_xpb(Scheme_Tcp *data)
{
  TCPiopbX *xpb;

  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  
  memcpy(xpb, data->tcp.create_pb, sizeof(TCPiopb));

  xpb->data = data;
  
  data->tcp.current_pb = xpb;

  return xpb;
}

static int mac_tcp_make(TCPiopbX **_xpb, TCPiopb **_pb, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;

  data = make_tcp_port_data(2);
  
  /* FIXME, precise GC: no GC tag... */
  xpb = (TCPiopbX *)scheme_malloc(sizeof(TCPiopbX));
  xpb->next = active_pbs;
  active_pbs = xpb;
  
  pb = (TCPiopb *)xpb;

  pb->ioCRefNum = tcpDriverId;
  pb->csCode = TCPCreate;
  pb->csParam.create.rcvBuff = (char *)scheme_malloc_atomic(STREAM_BUFFER_SIZE);
  pb->csParam.create.rcvBuffLen = STREAM_BUFFER_SIZE;
  pb->csParam.create.notifyProc = u_tcp_notify;
  pb->csParam.create.userDataPtr = (char *)&data->tcp;
  
  xpb->data = data;
  
  if ((errid = mzPBControlSync((ParamBlockRec*)pb)))
    return errid;
	
  data->tcp.create_pb = (void *)pb;
  data->tcp.stream = pb->tcpStream;
  data->tcp.async_errid = -1;

  *_xpb = xpb;
  *_pb = pb;
  *_data = data;

  return 0;
}

static void mac_tcp_close(Scheme_Tcp *data, int cls, int rel)
{
  TCPiopb *pb;
  
  pb = (TCPiopb *)mac_make_xpb(data);
  
  if (cls) {
    pb->ioCompletion = NULL;
    pb->csCode = TCPClose;
    pb->csParam.close.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.close.ulpTimeoutValue = 60 /* seconds */;
    pb->csParam.close.ulpTimeoutAction = 1 /* 1:abort 0:report */;
    mzPBControlSync((ParamBlockRec*)pb);
  }

  if (rel) {
    pb->csCode = TCPRelease;
    mzPBControlSync((ParamBlockRec*)pb);

    {
      TCPiopbX *x, *prev = NULL;
      x = active_pbs;
      while (x) {
	if (x->data->tcp.stream == data->tcp.stream) {
	  if (!prev)
	    active_pbs = x->next;
	  else
	    prev->next = x->next;
	  break;
	} else {
	  prev = x;
	  x = x->next;
	}
      }
    }
  }
}

static void mac_tcp_close_all(Scheme_Tcp *data)
{
  mac_tcp_close(data, 1, 1);
}

static int mac_tcp_listen(int id, long host_id, Scheme_Tcp **_data)
{
  TCPiopbX *xpb;
  TCPiopb *pb;
  Scheme_Tcp *data;
  int errid;
  
  if (!(errid = mac_tcp_make(&xpb, &pb, &data))) {
    data->tcp.state = SOCK_STATE_LISTENING;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;

    pb->ioCompletion = u_tcp_listen_done;
    pb->csCode = TCPPassiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.ulpTimeoutAction = 0 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0 /* seconds; 0 = infinity */;
    pb->csParam.open.remoteHost = host_id;
    pb->csParam.open.remotePort = 0;
    pb->csParam.open.localHost = 0;
    pb->csParam.open.localPort = id;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if ((errid = mzPBControlAsync((ParmBlkPtr)pb))) {
      data->tcp.state = SOCK_STATE_UNCONNECTED;
      mac_tcp_close(data, 1, 1);
      return errid;
    } else {
      *_data = data;
      return 0;
    }
  } else
    return errid;
}

static void tcp_cleanup(void)
{
  while (active_pbs) {
    TCPiopbX *pb = active_pbs;
    active_pbs = active_pbs->next;
    mac_tcp_close(pb->data, 1, 1);
  }
}

#else
#define TCP_INIT(x) /* nothing */
#endif
#endif

/*========================================================================*/
/*                       TCP ports and listeners                          */
/*========================================================================*/

#ifdef USE_SOCKETS_TCP
#define LISTENER_WAS_CLOSED(x) (((listener_t *)(x))->s == INVALID_SOCKET)
#endif
#ifdef USE_MAC_TCP
#define LISTENER_WAS_CLOSED(x) !((listener_t *)(x))->datas
#endif
#ifndef LISTENER_WAS_CLOSED
#define LISTENER_WAS_CLOSED(x) 0
#endif

/* Forward declaration */
static int stop_listener(Scheme_Object *o);

static int tcp_check_accept(Scheme_Object *listener)
{
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};
  int sr;

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  s = ((listener_t *)listener)->s;

  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, readfds);
  MZ_FD_SET(s, exnfds);
  
  do {
    sr = select(s + 1, readfds, NULL, exnfds, &time);
  } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));

  return sr;
#endif
#ifdef USE_MAC_TCP
  int i, count;
  Scheme_Tcp **datas;

  if (LISTENER_WAS_CLOSED(listener))
    return 1;

  count = ((listener_t *)listener)->count;
  datas = ((listener_t *)listener)->datas;

  for (i = 0; i < count; i++)
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING))
      return 1;

  return 0;
#endif
}

static void tcp_accept_needs_wakeup(Scheme_Object *listener, void *fds)
{
#ifdef USE_SOCKETS_TCP
  if (!LISTENER_WAS_CLOSED(listener)) {
    tcp_t s = ((listener_t *)listener)->s;
    void *fds2;

    fds2 = MZ_GET_FDSET(fds, 2);
    
    MZ_FD_SET(s, (fd_set *)fds);
    MZ_FD_SET(s, (fd_set *)fds2);
  }
#endif
}

static int tcp_check_connect(Scheme_Object *connector)
{
#ifdef USE_MAC_TCP
  return ((TCPiopb *)connector)->ioResult != inProgress;
#else
#ifdef USE_SOCKETS_TCP
  tcp_t s;
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};
  int sr;

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  s = (tcp_t)connector;

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);
    
  do {
    sr = select(s + 1, NULL, writefds, exnfds, &time);
  } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (!sr)
    return 0;
  if (FD_ISSET(s, exnfds))
    return -1;
  else
    return 1;
#else
  return 0;
#endif
#endif
}

static void tcp_connect_needs_wakeup(Scheme_Object *connector, void *fds)
{
#ifdef USE_SOCKETS_TCP
  void *fds1, *fds2;
  tcp_t s = (tcp_t)connector;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);

  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}

#ifdef USE_MAC_TCP
static void tcp_read_needs_wakeup(Scheme_Object *connector, void *fds)
{
}

static int tcp_check_read(Scheme_Object *pb)
{
  return (((TCPiopb *)pb)->ioResult != inProgress);
}
#endif

static int tcp_check_write(Scheme_Object *port)
{
  Scheme_Tcp *data = (Scheme_Tcp *)((Scheme_Output_Port *)port)->port_data;

  if (((Scheme_Output_Port *)port)->closed)
    return 1;

#ifdef USE_SOCKETS_TCP
  {
    tcp_t s;
    DECL_FDSET(writefds, 1);
    DECL_FDSET(exnfds, 1);
    struct timeval time = {0, 0};
    int sr;
    
    INIT_DECL_FDSET(writefds, 1);
    INIT_DECL_FDSET(exnfds, 1);
    
    s = data->tcp;
    
    MZ_FD_ZERO(writefds);
    MZ_FD_SET(s, writefds);
    MZ_FD_ZERO(exnfds);
    MZ_FD_SET(s, exnfds);
    
    do {
      sr = select(s + 1, NULL, writefds, exnfds, &time);
    } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));
    
    return sr;
  }
#else
  {
    TCPiopbX *xpb;
    TCPiopb *pb;
    int bytes;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    if (mzPBControlSync((ParamBlockRec*)pb))
      bytes = -1;
    else {
      bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
      if (bytes < 0)
	bytes = 0;
    }
    
    return !!bytes;
  }
#endif
}

static void tcp_write_needs_wakeup(Scheme_Object *port, void *fds)
{
#ifdef USE_SOCKETS_TCP
  Scheme_Object *conn = ((Scheme_Output_Port *)port)->port_data;
  void *fds1, *fds2;
  tcp_t s = ((Scheme_Tcp *)conn)->tcp;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);
  
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
#endif
}


static Scheme_Tcp *make_tcp_port_data(MAKE_TCP_ARG int refcount)
{
  Scheme_Tcp *data;
  char *bfr;
  
  data = MALLOC_ONE_RT(Scheme_Tcp);
#ifdef MZTAG_REQUIRED
  data->b.type = scheme_rt_tcp;
#endif
#ifdef USE_SOCKETS_TCP
  data->tcp = tcp;
#endif

  bfr = (char *)scheme_malloc_atomic(TCP_BUFFER_SIZE);
  data->b.buffer = bfr;

  data->b.bufpos = 0;
  data->b.bufmax = 0;
  data->b.hiteof = 0;
  data->b.refcount = refcount;

#ifndef USE_MAC_TCP
# ifdef USE_WINSOCK_TCP
  {
    unsigned long ioarg = 1;
    ioctlsocket(tcp, FIONBIO, &ioarg);
  }
# else
  fcntl(tcp, F_SETFL, MZ_NONBLOCKING);
# endif
#endif

  return data;
}

static int tcp_char_ready (Scheme_Input_Port *port)
{
  Scheme_Tcp *data;
#ifdef USE_SOCKETS_TCP
  int sr;
  DECL_FDSET(readfds, 1);
  DECL_FDSET(exfds, 1);
  struct timeval time = {0, 0};

  INIT_DECL_FDSET(readfds, 1);
  INIT_DECL_FDSET(exfds, 1);
#endif

  data = (Scheme_Tcp *)port->port_data;

  if (data->b.hiteof)
    return 1;
  if (data->b.bufpos < data->b.bufmax)
    return 1;

#ifdef USE_SOCKETS_TCP
  MZ_FD_ZERO(readfds);
  MZ_FD_ZERO(exfds);
  MZ_FD_SET(data->tcp, readfds);
  MZ_FD_SET(data->tcp, exfds);
    
  do {
    sr = select(data->tcp + 1, readfds, NULL, exfds, &time);
  } while ((sr == -1) && (NOT_WINSOCK(errno) == EINTR));

  return sr;
#endif

#ifdef USE_MAC_TCP
  if ((data->tcp.state == SOCK_STATE_CONNECTED)) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    
    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    pb->ioCompletion = NULL;

    if (mzPBControlSync((ParamBlockRec*)pb))
      return 1;
      
    if (pb->csParam.status.amtUnreadData)
      return 1;
 } else
   return 1;
#endif

  return 0;
}

static long tcp_get_string(Scheme_Input_Port *port, 
			   char *buffer, long offset, long size,
			   int nonblock)
{
  int errid;
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_MAC_TCP
 top:
#endif

  if (data->b.hiteof)
    return EOF;

#ifdef USE_MAC_TCP
  if (!data->activeRcv)
#endif
    if (data->b.bufpos < data->b.bufmax) {
      int n;
      n = data->b.bufmax - data->b.bufpos;
      n = ((size <= n)
	   ? size
	   : n);
      
      memcpy(buffer + offset, data->b.buffer + data->b.bufpos, n);
      data->b.bufpos += n;

      return n;
    }

  if (!tcp_char_ready(port)) {
    if (nonblock)
      return 0;

#ifdef USE_SOCKETS_TCP
    scheme_current_thread->block_descriptor = PORT_BLOCKED;
    scheme_current_thread->blocker = (Scheme_Object *)port;
#endif
    do {
      scheme_thread_block((float)0.0);
    } while (!tcp_char_ready(port));
#ifdef USE_SOCKETS_TCP
    scheme_current_thread->block_descriptor = NOT_BLOCKED;
    scheme_current_thread->blocker = NULL;
#endif
    scheme_current_thread->ran_some = 1;
  }

  /* We assume that no other process has access to our sockets, so
     when we unblock, there's definitely something to read. */

#ifdef USE_SOCKETS_TCP
  {
    int rn;
    do {
      rn = recv(data->tcp, data->b.buffer, TCP_BUFFER_SIZE, 0);
    } while ((rn == -1) && (NOT_WINSOCK(errno) == EINTR));
    data->b.bufmax = rn;
  }
  errid = SOCK_ERRNO();
#endif
#ifdef USE_MAC_TCP
  /* Allow only one read at a time: */
  if (!data->tcp.lock)
    data->tcp.lock = scheme_make_sema(0);
  else {
    if (!scheme_wait_sema(data->tcp.lock, 1)) {
      /* Do it the hard way: */
      scheme_wait_sema(data->tcp.lock, 0);
      scheme_post_sema(data->tcp.lock);
      goto top;
    }
  }
  
  if (data->activeRcv 
      || (data->tcp.state == SOCK_STATE_CONNECTED)) {
    /* socket is connected or an old recv is unfinished */
    TCPiopb *pb;    

    if (data->activeRcv) {
      pb = data->activeRcv;
    } else {
      pb = (TCPiopb *)mac_make_xpb(data);
    
      pb->csCode = TCPRcv;
      pb->ioCompletion = u_tcp_recv_done;
      pb->csParam.receive.commandTimeoutValue = 0; /* seconds, 0 = blocking */
      pb->csParam.receive.rcvBuff = data->b.buffer;
      pb->csParam.receive.rcvBuffLen = TCP_BUFFER_SIZE;
    
      data->activeRcv = pb;

      mzPBControlAsync((ParamBlockRec*)pb);
    }

    BEGIN_ESCAPEABLE(scheme_post_sema, data->tcp.lock);
    scheme_block_until(tcp_check_read, tcp_read_needs_wakeup, (Scheme_Object *)pb, 0);
    END_ESCAPEABLE();

    data->activeRcv = NULL;
    
    switch((errid = pb->ioResult)) {
    case noErr:
    case connectionClosing:
    case connectionTerminated:
      errid = 0;
      break;
    case commandTimeout:
    case connectionDoesntExist:
    case invalidStreamPtr:
    case invalidLength:
    case invalidBufPtr:
    default:
      break;
    }
  } else if (data->tcp.state == SOCK_STATE_EOF_FROM_OTHER 
             || data->tcp.state == SOCK_STATE_CLOSED) {
    data->b.bufmax = 0;
    errid = 0;
  } else
    errid = data->tcp.async_errid;
  
  if (errid)
    data->b.bufmax = -1;
    
  scheme_post_sema(data->tcp.lock);
#endif
  
  if (data->b.bufmax == -1) {
    scheme_raise_exn(MZEXN_I_O_PORT_READ,
		     port,
		     "tcp-read: error reading (%e)",
		     errid);
    return 0;
  } else if (!data->b.bufmax) {
    data->b.hiteof = 1;
    return EOF;
  }

  {
    int n;
    n = data->b.bufmax;
    if (size < n)
      n = size;
    memcpy(buffer + offset, data->b.buffer, n);
    data->b.bufpos = n;

    return n;
  }
}

static void tcp_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_SOCKETS_TCP
  {
    void *fds2;
  
    fds2 = MZ_GET_FDSET(fds, 2);
    
    MZ_FD_SET(data->tcp, (fd_set *)fds);
    MZ_FD_SET(data->tcp, (fd_set *)fds2);
  }
#endif
}

static void tcp_close_input(Scheme_Input_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_SOCKETS_TCP
  if (!(data->flags & MZ_TCP_ABANDON_INPUT))
    shutdown(data->tcp, 0);
#endif

  if (--data->b.refcount)
    return;

#ifdef USE_SOCKETS_TCP
  UNREGISTER_SOCKET(data->tcp);
  closesocket(data->tcp);
#endif
#ifdef USE_MAC_TCP
  mac_tcp_close(data, 0, 1);
#endif

  --scheme_file_open_count;
}

static long tcp_write_string(Scheme_Output_Port *port, 
			     const char *s, long offset, long len, 
			     int rarely_block)
{
  /* TCP writes aren't buffered at all right now. */
  /* If rarely_block is 1, it means only write as much as
     can be flushed immediately, blocking only if nothing
     can be written. */
  /* If rarely_block is 2, it means only write as much as
     can be flushed immediately, never ever blocking. */

  Scheme_Tcp *data;
  int errid, would_block = 0;
  long sent;

  if (!len) /* a flush request */
    return 0; 

  data = (Scheme_Tcp *)port->port_data;

 top:

#ifdef USE_SOCKETS_TCP
  do {
    sent = send(data->tcp, s + offset, len, 0);
  } while ((sent == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (sent != len) {
#ifdef USE_WINSOCK_TCP
    errid = WSAGetLastError();
# define SEND_BAD_MSG_SIZE(e) (e == WSAEMSGSIZE)
# define SEND_WOULD_BLOCK(e) \
    ((e == WSAEWOULDBLOCK) || (e == WSAEINPROGRESS))
#else
    errid = errno;
# ifdef SEND_IS_NEVER_TOO_BIG
#  define SEND_BAD_MSG_SIZE(errid) 0
# else
#  define SEND_BAD_MSG_SIZE(errid) (errid == EMSGSIZE)
# endif
# define SEND_WOULD_BLOCK(errid) \
   ((errid == EWOULDBLOCK) || (errid == EAGAIN)\
    || (errid == EINPROGRESS) || (errid == EALREADY))
#endif
    if (sent > 0) {
      /* Some data was sent. Return, or recur to handle the rest. */
      if (rarely_block)
	return sent;
      else
	sent += tcp_write_string(port, s, offset + sent, len - sent, 0);
      errid = 0;
    } else if ((len > 1) && SEND_BAD_MSG_SIZE(errid)) {
      /* split the message and try again: */
      int half = (len / 2);
      sent = tcp_write_string(port, s, offset, half, rarely_block);
      if (rarely_block)
	return sent;
      sent += tcp_write_string(port, s, offset + half, len - half, 0);
      errid = 0;
    } else if (SEND_WOULD_BLOCK(errid)) {
      errid = 0;
      would_block = 1;
    }
  } else
    errid = 0;
#endif
#ifdef USE_MAC_TCP
  errid = 0;
  if ((data->tcp.state == SOCK_STATE_CONNECTED) ||
      (data->tcp.state == SOCK_STATE_EOF_FROM_OTHER)) {
    /* socket is connected */
    TCPiopbX *xpb;
    TCPiopb *pb;
    int bytes;

    xpb = mac_make_xpb(data);
    pb = (TCPiopb *)xpb;
    
    pb->csCode = TCPStatus;
    if ((errid = mzPBControlSync((ParamBlockRec*)pb)))
      bytes = 0;
    else {
      bytes = pb->csParam.status.sendWindow - pb->csParam.status.amtUnackedData;
      if (bytes < 0)
	bytes = 0;
    }
    
    if (bytes >= len) {
      WriteData *wd;
      wdsEntry *e;
      int i;
      
      wd = NULL;
      for (i = 0; i < num_tcp_send_buffers; i++) {
	if (!((WriteData *)(tcp_send_buffers[i]))->xpb) {
	  wd = (WriteData *)(tcp_send_buffers[i]);
	  break;
	}
      }
      
      if (!wd) {
	void **naya;
	int nayac;
	
	nayac = (2 * num_tcp_send_buffers) + 1;
	naya = MALLOC_N(void *, nayac);
	memcpy(naya, tcp_send_buffers, sizeof(void *) * num_tcp_send_buffers);
	for (i = num_tcp_send_buffers; i < nayac; i++) {
	  wd = MALLOC_ONE_RT(WriteData);
#ifdef MZTAG_REQUIRED
	  wd->type = scheme_rt_write_data;
#endif
	  wd->xpb = NULL;
	  e = wd->e;
	  e[0].ptr = NULL;
	  e[1].ptr = NULL;
	  e[1].length = 0;
	  naya[i] = (void *)e;
	}

	wd = (WriteData *)naya[num_tcp_send_buffers];
	
	tcp_send_buffers = naya;
	num_tcp_send_buffers = nayac;
      }

      wd->xpb = xpb;
      e = wd->e;

      e[0].ptr = (Ptr)scheme_malloc_atomic(len);
      memcpy(e[0].ptr, s + offset, len);
      e[0].length = len;
      e[1].ptr = NULL;
      e[1].length = 0;

      pb->csCode = TCPSend;
      pb->ioCompletion = u_tcp_send_done;
      pb->csParam.send.validityFlags = timeoutValue | timeoutAction;
      pb->csParam.send.ulpTimeoutValue = 60 /* seconds */;
      pb->csParam.send.ulpTimeoutAction = 1 /* 0:abort 1:report */;
      pb->csParam.send.pushFlag = 1;
      pb->csParam.send.urgentFlag = 0;
      pb->csParam.send.wdsPtr = (Ptr)e;
      pb->csParam.send.sendFree = 0;
      pb->csParam.send.sendLength = 0;
      
      errid = mzPBControlAsync((ParamBlockRec*)pb);
    } else if (!errid) {
      if (bytes) {
      	/* Do partial write: */
        sent = tcp_write_string(port, s, offset, bytes, rarely_block);
	if (rarely_block)
	  return sent;
        sent = tcp_write_string(port, s, offset + bytes, len - bytes, 0);
	sent += bytes;
      } else
        would_block = 1;
    }
  } else
    errid = data->tcp.async_errid;
#endif

  if (would_block) {
    if (rarely_block == 2)
      return 0;

    /* Block for writing: */
    scheme_block_until(tcp_check_write, tcp_write_needs_wakeup, (Scheme_Object *)port, (float)0.0);

    /* Closed while blocking? */
    if (((Scheme_Output_Port *)port)->closed) {
      /* Call write again to signal the error: */
      scheme_put_string("tcp-write-string", (Scheme_Object *)port, s, offset, len, 0);
      return sent + len; /* shouldn't get here */
    }

    /* Ok - try again! */
    would_block = 0;
    goto top;
  }

  if (errid)
    scheme_raise_exn(MZEXN_I_O_PORT_WRITE,
		     port,
		     "tcp-write: error writing (%e)",
		     errid);

  return sent;
}

static void tcp_close_output(Scheme_Output_Port *port)
{
  Scheme_Tcp *data;

  data = (Scheme_Tcp *)port->port_data;

#ifdef USE_SOCKETS_TCP
  if (!(data->flags & MZ_TCP_ABANDON_OUTPUT))
    shutdown(data->tcp, 1);
#endif

#ifdef USE_MAC_TCP
  if (!(data->flags & MZ_TCP_ABANDON_OUTPUT)
      || (data->b.refcount == 1))
    mac_tcp_close(data, 1, data->b.refcount == 1);
#endif

  if (--data->b.refcount)
    return;

#ifdef USE_SOCKETS_TCP
  UNREGISTER_SOCKET(data->tcp);
  closesocket(data->tcp);
#endif

  --scheme_file_open_count;
}

static Scheme_Object *
make_named_tcp_input_port(void *data, const char *name)
{
  Scheme_Input_Port *ip;

  ip = _scheme_make_input_port(scheme_tcp_input_port_type,
			       data,
			       tcp_get_string,
			       NULL,
			       tcp_char_ready,
			       tcp_close_input,
			       tcp_need_wakeup,
			       1);

  ip->name = (char *)name;

  return (Scheme_Object *)ip;
}

static Scheme_Object *
make_tcp_output_port(void *data)
{
  return (Scheme_Object *)scheme_make_output_port(scheme_tcp_output_port_type,
						  data,
						  tcp_write_string,
						  (Scheme_Out_Ready_Fun)tcp_check_write,
						  tcp_close_output,
						  (Scheme_Need_Wakeup_Output_Fun)tcp_write_needs_wakeup,
						  1);
}

#endif /* USE_TCP */

/*========================================================================*/
/*                         TCP Scheme interface                           */
/*========================================================================*/

# ifdef PROTOENT_IS_INT
#  define PROTO_P_PROTO PROTOENT_IS_INT
# else
#  define PROTO_P_PROTO proto->p_proto
# endif

# ifndef MZ_PF_INET
#  define MZ_PF_INET PF_INET
# endif

#ifdef USE_SOCKETS_TCP
static void closesocket_w_decrement(tcp_t s)
{
  closesocket(s);
  --scheme_file_open_count;
}
#endif

#ifdef USE_WINSOCK_TCP
# ifdef __BORLANDC__
#  define MZ_LPTHREAD_START_ROUTINE unsigned int (__stdcall*)(void*)
# else
#  define MZ_LPTHREAD_START_ROUTINE LPTHREAD_START_ROUTINE
# endif

static int ghbn_lock;

typedef struct {
  HANDLE th;
  long result;
  int done;
} GHBN_Rec;

static char ghbn_hostname[256];

static long gethostbyname_in_thread(void *data)
{
  struct hostent *host;
  host = gethostbyname(ghbn_hostname);
  if (host)
    return *(long *)host->h_addr_list[0];
  else
    return 0;
}

static void release_ghbn_lock(GHBN_Rec *rec)
{
  ghbn_lock = 0;
  CloseHandle(rec->th);
}
static int ghbn_lock_avail(Scheme_Object *_ignored)
{
  return !ghbn_lock;
}

static int ghbn_thread_done(Scheme_Object *_rec)
{
  GHBN_Rec *rec = (GHBN_Rec *)_rec;

  if (rec->done)
    return 1;

  if (WaitForSingleObject(rec->th, 0) == WAIT_OBJECT_0) {
    DWORD code;

    GetExitCodeThread(rec->th, &code);
    rec->result = code;
    rec->done = 1;

    return 1;
  }

  return 0;
}

static void ghbn_thread_need_wakeup(Scheme_Object *_rec, void *fds)
{
  GHBN_Rec *rec = (GHBN_Rec *)_rec;

  scheme_add_fd_handle((void *)rec->th, fds, 0);
}

#define HOST_RESULT_IS_ADDR
static struct hostent *MZ_GETHOSTBYNAME(const char *name)
{
  GHBN_Rec *rec;
  long th;
  DWORD id;

  if (strlen(name) < 256)
    strcpy(ghbn_hostname, name);
  else
    return NULL;

  rec = MALLOC_ONE_ATOMIC(GHBN_Rec);
  rec->done = 0;

  scheme_block_until(ghbn_lock_avail, NULL, NULL, 0);

  ghbn_lock = 1;

  th = _beginthreadex(NULL, 5000, 
		      (MZ_LPTHREAD_START_ROUTINE)gethostbyname_in_thread,
		      NULL, 0, &id);

  rec->th = (HANDLE)th;
  
  BEGIN_ESCAPEABLE(release_ghbn_lock, rec);
  scheme_block_until(ghbn_thread_done, ghbn_thread_need_wakeup, (Scheme_Object *)rec, 0);
  END_ESCAPEABLE();

  CloseHandle(rec->th);

  ghbn_lock = 0;

  return (struct hostent *)rec->result;
}
#else
# define MZ_GETHOSTBYNAME gethostbyname 
#endif

static Scheme_Object *tcp_connect(int argc, Scheme_Object *argv[])
{
  char * volatile address = "", * volatile errmsg = "";
  unsigned short origid, id;
  int errpart = 0, errid = 0;
#ifdef USE_SOCKETS_TCP
  struct hostent *host;
  tcp_address tcp_connect_dest_addr; /* Use a long name for precise GC's xform.ss */
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("tcp-connect", "string", 0, argc, argv);
  if (!CHECK_PORT_ID(argv[1]))
    scheme_wrong_type("tcp-connect", PORT_ID_TYPE, 1, argc, argv);

#ifdef USE_TCP
  TCP_INIT("tcp-connect");
#endif

  address = SCHEME_STR_VAL(argv[0]);
  origid = (unsigned short)SCHEME_INT_VAL(argv[1]);

  scheme_security_check_network("tcp-connect", address, origid, 1);

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
#endif

#ifdef USE_MAC_TCP
  {
    TCPiopbX *xpb;
    TCPiopb *pb;
    Scheme_Tcp *data;
    int errNo;
    struct hostInfo *dest_host;
    Scheme_Object *v[2];
    
    dest_host = MALLOC_ONE_ATOMIC(struct hostInfo);
    if ((errNo = tcp_addr(address, dest_host))) {
      errpart = 1;
      errmsg = "; host not found";
      goto tcp_error;
    }

    if ((errNo = mac_tcp_make(&xpb, &pb, &data))) {
      errpart = 2;
      goto tcp_error;
    }

    data->tcp.state = SOCK_STATE_CONNECTING;
    
    pb->ioCompletion = u_tcp_connect_done;
    pb->csCode = TCPActiveOpen;
    pb->csParam.open.validityFlags = timeoutValue | timeoutAction;
    pb->csParam.open.ulpTimeoutValue = 60 /* seconds */;
    pb->csParam.open.ulpTimeoutAction = 1 /* 1:abort 0:report */;
    pb->csParam.open.commandTimeoutValue = 0;
    pb->csParam.open.remoteHost = dest_host->addr[0];
    pb->csParam.open.remotePort = id;
    pb->csParam.open.localHost = 0;
    pb->csParam.open.localPort = 0;
    pb->csParam.open.dontFrag = 0;
    pb->csParam.open.timeToLive = 0;
    pb->csParam.open.security = 0;
    pb->csParam.open.optionCnt = 0;

    if ((errNo = mzPBControlAsync((ParamBlockRec*)pb))) {
      errpart = 3;
      goto tcp_close_and_error;
    }
    
    BEGIN_ESCAPEABLE(mac_tcp_close_all, data);
    scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, (Scheme_Object *)pb, 0);
    END_ESCAPEABLE();
    
    if (data->tcp.state != SOCK_STATE_CONNECTED) {
      errpart = 4;
      errNo = pb->ioResult;
      goto tcp_close_and_error;
    }
    
    v[0] = make_named_tcp_input_port(data, address);
    v[1] = make_tcp_output_port(data);
    
    return scheme_values(2, v);
    
  tcp_close_and_error:
    
    mac_tcp_close(data, 1, 1);
    
  tcp_error:
    
    errid = errNo;
  }
#endif

#ifdef USE_SOCKETS_TCP
  host = MZ_GETHOSTBYNAME(address);
  if (host) {
    tcp_connect_dest_addr.sin_family = AF_INET;
    tcp_connect_dest_addr.sin_port = id;
    memset(&tcp_connect_dest_addr.sin_addr, 0, sizeof(tcp_connect_dest_addr.sin_addr));
    memset(&tcp_connect_dest_addr.sin_zero, 0, sizeof(tcp_connect_dest_addr.sin_zero));
#ifdef HOST_RESULT_IS_ADDR
    memcpy(&tcp_connect_dest_addr.sin_addr, &host, sizeof(long)); 
#else
    memcpy(&tcp_connect_dest_addr.sin_addr, host->h_addr_list[0], host->h_length); 
#endif

#ifndef PROTOENT_IS_INT
    proto = getprotobyname("tcp");
    if (proto)
#endif
    {
      tcp_t s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
      if (s != INVALID_SOCKET) {
	int status, inprogress;
#ifdef USE_WINSOCK_TCP
	unsigned long ioarg = 1;
	ioctlsocket(s, FIONBIO, &ioarg);
#else
	int size = TCP_SOCKSENDBUF_SIZE;
	fcntl(s, F_SETFL, MZ_NONBLOCKING);
# ifndef CANT_SET_SOCKET_BUFSIZE
	setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
# endif
#endif
	status = connect(s, (struct sockaddr *)&tcp_connect_dest_addr, sizeof(tcp_connect_dest_addr));
#ifdef USE_UNIX_SOCKETS_TCP
	if (status)
	  status = errno;
	if (status == EINTR)
	  status = EINPROGRESS;
	
	inprogress = (status == EINPROGRESS);
#endif
#ifdef USE_WINSOCK_TCP
	if (status)
	  status = WSAGetLastError();

	inprogress = (status == WSAEWOULDBLOCK);
	errno = status;
#endif

	scheme_file_open_count++;
	
	if (inprogress) {
          BEGIN_ESCAPEABLE(closesocket_w_decrement, s);
	  scheme_block_until(tcp_check_connect, tcp_connect_needs_wakeup, (void *)s, (float)0.0);
	  END_ESCAPEABLE();

	  /* Check whether connect succeeded, or get error: */
	  {
	    int so_len = sizeof(status);
	    if (getsockopt(s, SOL_SOCKET, SO_ERROR, (void *)&status, &so_len) != 0) {
	      status = SOCK_ERRNO();
	    }
	    errno = status; /* for error reporting, below */
	  }

	  /* Old way to test for success.
	   * This seems to cause a problem on later Linux kernels.
	   * Thanks to John R. Hall for tracking down the problem.
	   *
	   *  do {
	   *   status = recv(s, NULL, 0, 0); // test input
	   * } while ((status == -1) && (errno == EINTR));
	   * if (!status) {
	   *	 do {
	   *	   status = send(s, NULL, 0, 0); // test output
	   *   } while ((status == -1) && (errno == EINTR));
	   * }
	   */
	  /* Old way for Windows
	   * status = send(s, "", 0, 0); // test output
	   * if (status)
	   *  errno = WSAGetLastError();
	   */
	}
	
	if (!status) {
	  Scheme_Object *v[2];
	  Scheme_Tcp *tcp;

	  tcp = make_tcp_port_data(s, 2);
	  
	  v[0] = make_named_tcp_input_port(tcp, address);
	  v[1] = make_tcp_output_port(tcp);
	  
	  REGISTER_SOCKET(s);

	  return scheme_values(2, v);
	} else {
	  errid = errno;
	  closesocket(s);
	  --scheme_file_open_count;
	  errpart = 4;
	}
      } else {
	errpart = 3;
	errid = SOCK_ERRNO();
      }
    }
#ifndef PROTOENT_IS_INT
    else
      (errid = 1, errpart = 2);
#endif
  } else {
    errpart = 1;
    errid = 0;
    errmsg = "; host not found";
  }
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-connect: connection to %s, port %d failed%s (at step %d: %E)",
		   address, origid, errmsg, errpart, errid);
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "tcp-connect: not supported on this platform");
#endif

  return NULL;
}

static Scheme_Object *
tcp_connect_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(tcp_connect, argc, argv);
}

static Scheme_Object *
tcp_listen(int argc, Scheme_Object *argv[])
{
  unsigned short id, origid;
  int backlog, errid;
  int reuse = 0;
  const char *address;
#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  struct protoent *proto;
# endif
#endif

  if (!CHECK_PORT_ID(argv[0]))
    scheme_wrong_type("tcp-listen", PORT_ID_TYPE, 0, argc, argv);
  if (argc > 1) {
    if (!SCHEME_INTP(argv[1]) || (SCHEME_INT_VAL(argv[1]) < 1))
      scheme_wrong_type("tcp-listen", "small positive integer", 1, argc, argv);
  }
  if (argc > 2)
    reuse = SCHEME_TRUEP(argv[2]);
  if (argc > 3) {
    if (!SCHEME_STRINGP(argv[3]) && !SCHEME_FALSEP(argv[3]))
      scheme_wrong_type("tcp-connect", "string or #f", 3, argc, argv);
  }
    
#ifdef USE_TCP
  TCP_INIT("tcp-listen");
#endif

  origid = (unsigned short)SCHEME_INT_VAL(argv[0]);
  if (argc > 1)
    backlog = SCHEME_INT_VAL(argv[1]);
  else
    backlog = 4;
  if (argc > 3)
    address = (SCHEME_STRINGP(argv[3]) ? SCHEME_STR_VAL(argv[3]) : NULL);
  else
    address = NULL;

  scheme_security_check_network("tcp-listen", address, origid, 0);

#ifdef USE_TCP
  /* Set id in network order: */
  id = htons(origid);
#endif

#ifdef USE_MAC_TCP
  {
    int i;
    long hostid;
    Scheme_Tcp **datas, *data;
    struct hostInfo *local_host;

    if (address) {
      local_host = MALLOC_ONE_ATOMIC(struct hostInfo);
      if ((errid = tcp_addr(address, local_host))) {
	scheme_raise_exn(MZEXN_I_O_TCP,
			 "tcp-listen: host not found: %s (%E)",
			 address, errid);
	return NULL;
      }
      hostid = local_host->addr[0];
    } else
      hostid = 0;


    datas = MALLOC_N(Scheme_Tcp *, backlog);

    for (i = 0; i < backlog; i++) {
      if ((errid = mac_tcp_listen(id, hostid, &data))) {
        /* Close listeners that had succeeded: */
        int j;
        for (j = 0; j < i; j++)
          mac_tcp_close(datas[i], 1, 1);
	break;
      }
      datas[i] = data;
    }

    if (!errid) {
      listener_t *l = MALLOC_ONE_TAGGED(listener_t);

      l->type = scheme_listener_type;
      l->portid = id;
      l->hostid = hostid;
      l->count = backlog;
      l->datas = datas;
      l->mref = scheme_add_managed(NULL,
				   (Scheme_Object *)l,
				   (Scheme_Close_Custodian_Client *)stop_listener,
				   NULL,
				   1);
      
      return (Scheme_Object *)l;
    }
  }
#endif

#ifdef USE_SOCKETS_TCP
# ifndef PROTOENT_IS_INT
  proto = getprotobyname("tcp");
  if (proto)
# endif
  {
    struct hostent *host;

    if (address)
      host = MZ_GETHOSTBYNAME(address);
    else
      host = NULL;

    if (!address || host) {
      tcp_address tcp_listen_addr; /* Use a long name for precise GC's xform.ss */
      tcp_t s;

      tcp_listen_addr.sin_family = AF_INET;
      tcp_listen_addr.sin_port = id;
      memset(&tcp_listen_addr.sin_addr, 0, sizeof(tcp_listen_addr.sin_addr));
      memset(&tcp_listen_addr.sin_zero, 0, sizeof(tcp_listen_addr.sin_zero));
      if (host) {
#ifdef HOST_RESULT_IS_ADDR
	memcpy(&tcp_listen_addr.sin_addr, &host, sizeof(long)); 
#else
	memcpy(&tcp_listen_addr.sin_addr, host->h_addr_list[0], host->h_length); 
#endif
      }

      s = socket(MZ_PF_INET, SOCK_STREAM, PROTO_P_PROTO);
      if (s != INVALID_SOCKET) {
#ifdef USE_WINSOCK_TCP
	unsigned long ioarg = 1;
	ioctlsocket(s, FIONBIO, &ioarg);
#else
	fcntl(s, F_SETFL, MZ_NONBLOCKING);
#endif

	if (reuse) {
	  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &reuse, sizeof(int));
	}
      
	if (!bind(s, (struct sockaddr *)&tcp_listen_addr, sizeof(tcp_listen_addr))) {
	  if (!listen(s, backlog)) {
	    listener_t *l;

	    l = MALLOC_ONE_TAGGED(listener_t);
	    l->type = scheme_listener_type;
	    l->s = s;
	    {
	      Scheme_Custodian_Reference *mref;
	      mref = scheme_add_managed(NULL,
					(Scheme_Object *)l,
					(Scheme_Close_Custodian_Client *)stop_listener,
					NULL,
					1);
	      l->mref = mref;
	    }

	    scheme_file_open_count++;
	    REGISTER_SOCKET(s);

	    return (Scheme_Object *)l;
	  }
	}

	errid = SOCK_ERRNO();

	closesocket(s);
      } else
	errid = SOCK_ERRNO();
    } else {
      scheme_raise_exn(MZEXN_I_O_TCP,
		       "tcp-listen: host not found: %s",
		       address);
      return NULL;
    }
  }
# ifndef PROTOENT_IS_INT
  else {
    errid = SOCK_ERRNO();
  }
# endif
#endif

#ifdef USE_TCP
  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-listen: listen on %d failed (%E)",
		   origid, errid);
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "tcp-listen: not supported on this platform");
#endif

  return NULL;
}

#ifdef USE_TCP
static int stop_listener(Scheme_Object *o)
{
  int was_closed = 0;

#ifdef USE_MAC_TCP
  { 
    listener_t *l = (listener_t *)o;
    int i, count = l->count;
    Scheme_Tcp **datas = l->datas;
    if (!datas || !l->count)
      was_closed = 1;
    else {
      l->count = 0;
      for (i = 0; i < count; i++)
	if (datas[i])
	  mac_tcp_close(datas[i], 1, 1);
    }
 }
#endif

#ifdef USE_SOCKETS_TCP
  {
    tcp_t s = ((listener_t *)o)->s;
    if (s == INVALID_SOCKET)
      was_closed = 1;
    else {
      UNREGISTER_SOCKET(s);
      closesocket(s);
      ((listener_t *)o)->s = INVALID_SOCKET;
      --scheme_file_open_count;
    }
  }
#endif

  return was_closed;
}
#endif

static Scheme_Object *
tcp_stop(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-close");

  was_closed = stop_listener(argv[0]);

  if (was_closed) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-close: listener was already closed");
    return NULL;
  }

  return scheme_void;
#else
  scheme_wrong_type("tcp-close", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept_ready(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int ready;

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept-ready?", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept-ready?");

  if (LISTENER_WAS_CLOSED(argv[0])) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-accept-ready?: listener is closed");
    return NULL;
  }

  ready = tcp_check_accept(argv[0]);

  return (ready ? scheme_true : scheme_false);
#else
  scheme_wrong_type("tcp-accept-ready?", "tcp-listener", 0, argc, argv);
  return NULL;
#endif
}

static Scheme_Object *
tcp_accept(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  int was_closed = 0, errid;
  Scheme_Object *listener;
# ifdef USE_SOCKETS_TCP
  tcp_t s;
  int l;
  tcp_address tcp_accept_addr; /* Use a long name for precise GC's xform.ss */
# endif
# ifdef USE_MAC_TCP
  listener_t *l;
  int i, count;
  Scheme_Tcp **datas;
# endif

  if (!SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type))
    scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);

  TCP_INIT("tcp-accept?");

  listener = argv[0];

  was_closed = LISTENER_WAS_CLOSED(listener);

  if (!was_closed) {
    if (!tcp_check_accept(listener)) {
      scheme_current_thread->block_descriptor = -1;
      scheme_current_thread->blocker = listener;
      scheme_current_thread->block_check = tcp_check_accept;
      scheme_current_thread->block_needs_wakeup = tcp_accept_needs_wakeup;
      do {
	scheme_thread_block((float)0.0);
      } while (!tcp_check_accept(listener));
      scheme_current_thread->block_descriptor = NOT_BLOCKED;
      scheme_current_thread->blocker = NULL;
      scheme_current_thread->ran_some = 1;
    }
    was_closed = LISTENER_WAS_CLOSED(listener);
  }

  if (was_closed) {
    scheme_raise_exn(MZEXN_I_O_TCP,
		     "tcp-accept: listener is closed");
    return NULL;
  }
  
# ifdef USE_SOCKETS_TCP
  s = ((listener_t *)listener)->s;

  l = sizeof(tcp_accept_addr);

  do {
    s = accept(s, (struct sockaddr *)&tcp_accept_addr, &l);
  } while ((s == -1) && (NOT_WINSOCK(errno) == EINTR));

  if (s != INVALID_SOCKET) {
    Scheme_Object *v[2];
    Scheme_Tcp *tcp;
    
#  ifdef USE_UNIX_SOCKETS_TCP
    int size = TCP_SOCKSENDBUF_SIZE;
#   ifndef CANT_SET_SOCKET_BUFSIZE
    setsockopt(s, SOL_SOCKET, SO_SNDBUF, (char *)&size, sizeof(int));
#   endif
#  endif

    tcp = make_tcp_port_data(s, 2);

    v[0] = make_named_tcp_input_port(tcp, "TCP");
    v[1] = make_tcp_output_port(tcp);

    scheme_file_open_count++;
    REGISTER_SOCKET(s);
    
    return scheme_values(2, v);
  }
  errid = SOCK_ERRNO();
# endif

# ifdef USE_MAC_TCP
  l = (listener_t *)listener;
  count = l->count;
  datas = l->datas;

  errid = 0;
  for (i = 0; i < count; i++) {
    if (datas[i] && (datas[i]->tcp.state != SOCK_STATE_LISTENING)) {
      Scheme_Object *v[2];
      Scheme_Tcp *data;
      
      v[0] = make_named_tcp_input_port(datas[i], "TCP");
      v[1] = make_tcp_output_port(datas[i]);
      
      if (!(errid = mac_tcp_listen(l->portid, l->hostid, &data))) {
        /* new listener at the end of the queue: */
	memcpy(datas + i, datas + i + 1, sizeof(Scheme_Tcp *) * (count - i - 1));
	datas[count - 1] = data;

	scheme_file_open_count++;
      } else {
      	/* catastophic error; we permanently decrement the listener count */
        datas[i] = NULL;
      }
      return scheme_values(2, v);
    }
  }
# endif

  scheme_raise_exn(MZEXN_I_O_TCP,
		   "tcp-accept: accept from listener failed (%E)", errid);
#else
  scheme_wrong_type("tcp-accept", "tcp-listener", 0, argc, argv);
#endif

  return NULL;
}

static Scheme_Object *
tcp_accept_break(int argc, Scheme_Object *argv[])
{
  return scheme_call_enable_break(tcp_accept, argc, argv);
}

static void register_tcp_listener_wait()
{
#ifdef USE_TCP
  scheme_add_waitable(scheme_listener_type, tcp_check_accept, tcp_accept_needs_wakeup, NULL);
#endif
}

static Scheme_Object *tcp_listener_p(int argc, Scheme_Object *argv[])
{
   return (SAME_TYPE(SCHEME_TYPE(argv[0]), scheme_listener_type)
	   ? scheme_true
	   : scheme_false);
}

static Scheme_Object *tcp_addresses(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  Scheme_Tcp *tcp = NULL;
  int closed = 0;
  unsigned long here_a, there_a;
  unsigned char *b;
  Scheme_Object *result[2];
  char sa[20];

  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = (Scheme_Output_Port *)argv[0];
    if (op->sub_type == scheme_tcp_output_port_type)
      tcp = op->port_data;
    closed = op->closed;
  } else if (SCHEME_INPORTP(argv[0])) {
    Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)argv[0];
    if (ip->sub_type == scheme_tcp_input_port_type)
      tcp = ip->port_data;
    closed = ip->closed;
  }

  if (!tcp)
    scheme_wrong_type("tcp-addresses", "tcp-port", 0, argc, argv);

  if (closed)
    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
		     argv[0],
		     "tcp-addresses: port is closed");

# ifdef USE_SOCKETS_TCP
  {
    /* Use a long name for precise GC's xform.ss: */
    tcp_address tcp_here_addr, tcp_there_addr;
    int l;
    
    l = sizeof(tcp_here_addr);
    if (getsockname(tcp->tcp, (struct sockaddr *)&tcp_here_addr, &l)) {
      scheme_raise_exn(MZEXN_I_O_TCP,
		       "tcp-addresses: could not get local address (%e)",
		       SOCK_ERRNO());
    }
    l = sizeof(tcp_there_addr);
    if (getpeername(tcp->tcp, (struct sockaddr *)&tcp_there_addr, &l)) {
      scheme_raise_exn(MZEXN_I_O_TCP,
		       "tcp-addresses: could not get peer address (%e)",
		       SOCK_ERRNO());
    }

    here_a = *(unsigned long *)&tcp_here_addr.sin_addr;
    there_a = *(unsigned long *)&tcp_there_addr.sin_addr;
  }
# endif
# ifdef USE_MAC_TCP
  {
    here_a = ((TCPOpenPB *)tcp->tcp.create_pb)->localHost;
    here_a = ((TCPOpenPB *)tcp->tcp.create_pb)->remoteHost;
  }
# endif

  b = (unsigned char *)&here_a;
  sprintf(sa, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  result[0] = scheme_make_string(sa);

  b = (unsigned char *)&there_a;
  sprintf(sa, "%d.%d.%d.%d", b[0], b[1], b[2], b[3]);
  result[1] = scheme_make_string(sa);

  return scheme_values(2, result);
#else
  /* First arg can't possible be right! */
  scheme_wrong_type("tcp-addresses", "tcp-port", 0, argc, argv);
#endif
}

static Scheme_Object *tcp_abandon_port(int argc, Scheme_Object *argv[])
{
#ifdef USE_TCP
  if (SCHEME_OUTPORTP(argv[0])) {
    Scheme_Output_Port *op;
    op = (Scheme_Output_Port *)argv[0];
    if (op->sub_type == scheme_tcp_output_port_type) {
      if (!op->closed) {
	((Scheme_Tcp *)op->port_data)->flags |= MZ_TCP_ABANDON_OUTPUT;
	scheme_close_output_port(argv[0]);
      }
      return scheme_void;
    }
  } else if (SCHEME_INPORTP(argv[0])) {
    /* Abandon is not really useful on input ports from the Schemer's
       perspective, but it's here for completeness. */
    Scheme_Input_Port *ip;
    ip = (Scheme_Input_Port *)argv[0];
    if (ip->sub_type == scheme_tcp_input_port_type) {
      if (!ip->closed) {
	((Scheme_Tcp *)ip->port_data)->flags |= MZ_TCP_ABANDON_INPUT;
	scheme_close_input_port(argv[0]);
      }
      return scheme_void;
    }
  }
#endif

  scheme_wrong_type("tcp-abandon-port", "tcp-port", 0, argc, argv);

  return NULL;
}

/*========================================================================*/
/*                       precise GC traversers                            */
/*========================================================================*/

#ifdef MZ_PRECISE_GC

START_XFORM_SKIP;

#define MARKS_FOR_NETWORK_C
#include "mzmark.c"

static void register_traversers(void)
{
#ifdef USE_TCP
  GC_REG_TRAV(scheme_rt_tcp, mark_tcp);
# ifdef USE_MAC_TCP
  GC_REG_TRAV(scheme_rt_write_data, mark_write_data);
# endif
#endif
  GC_REG_TRAV(scheme_listener_type, mark_listener);  
}

END_XFORM_SKIP;

#endif

#endif /* !NO_TCP_SUPPORT */

