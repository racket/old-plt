/*-
 * File: dde_ipc.c
 * Purpose: Implement DDE with socket.
 * Author: Giordano Pezzoli, gio@crcc.it.
 * Modified by: E. Zimmermann <edz@crg.net>
 */

#include	<stdio.h>
#include	<ctype.h>
#include	<signal.h>
// #include	<unistd.h>
#include	<stdlib.h>
#include	<stdarg.h>
#include        <string.h>
#include	<windows.h>
#include	<ddeml.h>

#define HAVE_SOCKET 1

#include	"tcp1632.h"
#include	"dde_ipc.h"

static char *local_copystring(char *s)
{
  char *tmp = (char *)malloc(sizeof(char)*(strlen(s) + 1));
  strcpy(tmp, s);
  tmp[strlen(s)] = 0;
  return tmp;
}

/* Might need to play with these */
static unsigned long ipTimeout      =120000; /* 120 seconds timeout */
static unsigned  long ipReadTimeout = 15000; /* 15 seconds timeout */

#define MY_TIMER 10

#define	Malloc(TYPE,N)		((TYPE*)malloc(sizeof(TYPE)*(N)))
#define	Calloc(TYPE,N)		((TYPE*)calloc(sizeof(TYPE),(N)))
#define	Free(PTR)		free((void*)(PTR))
#define	Realloc(TYPE,PTR,N)	((PTR)=Xrealloc(TYPE,(void*)(PTR),sizeof(TYPE)*(N)))
#define	Xrealloc(TYPE,PTR,N)	((PTR)==NULL ? (TYPE*)malloc(N):(TYPE*)realloc(PTR,N))
#define	Check_Table(TYPE,T,PT,CS)	{\
		int	s_=(PT)-(T); \
		if ( s_%(CS)==0 && ((PT)=Realloc(TYPE,T,s_+(CS)))!=NULL ) \
			(PT) += s_;}


#define	Sizeofsockaddr(ADDR)		sizeof(struct sockaddr_in)

#define	writesocket(FD,BUF,SIZE)	send(FD,BUF,SIZE,0)
#define	readsocket(FD,BUF,SIZE)		recv(FD,BUF,SIZE,0)

#define	StringHSZ(APP,STRING)	\
	((STRING)==NULL ? (HSZ)NULL: \
		DdeCreateStringHandle((APP)->handle.windde.idInst,STRING,CP_WINANSI))

static void *HDDEDATAdata (HDDEDATA hdata, int *size);
static char *HSZstring (DDEApp * app, HSZ stringhsz);

#ifdef	WIN32
#define	_EXPORT /**/
#else
#define	_EXPORT	 _export
#endif

extern HDDEDATA EXPENTRY _EXPORT 
windows_call(
	     WORD type, WORD fmt,
	     HCONV hconv, HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
	     DWORD ldata1, DWORD ldata2);

/*
 * It's incredible how stupid are MS engineers! A callback mechanism without
 * a closure argument is like a message without a reply address: who care of.
 */
static DDEApp *The_Only_Application_Of_Windows = NULL;

static int ddesock_fdcmp (DDESock * s, void *arg);



typedef struct
  {
    int (*select) (DDEApp * app);
    int (*enable) (DDEApp * app, DDESock * s);
    int (*disable) (DDEApp * app, DDESock * s);
  }
DDEAfun;

#define	DDESelect(A)		(*(A)->members->select)(A)
#define	DDEEnable(A,S)		(*(A)->members->enable)(A,S)
#define	DDEDisable(A,S)		(*(A)->members->disable)(A,S)

struct ddeapp
  {
    union
      {
	struct
	  {
	    HANDLE hInstance;
	    HWND hWnd;
	    DWORD idInst;
	  }
	windde;

#if	HAVE_SOCKET
	struct
	  {
	    fd_set set;		/* select descriptors */
	    int max;		/* max descriptor set */
	  }
	select;
#endif
      }
    handle;
    DDESock **ss;		/* Socket to monitor */
    int nss;
    DDEAfun *members;
  };

static int ddeadd_sock (DDEApp * app, DDESock * s);
static int dderem_sock (DDEApp * app, DDESock * s);
static DDESock *ddeapp_sock (DDEApp * app, int (*cmp) (DDESock * s, void *arg), void *arg);
static int ddesock_addrcmp (DDESock * s, void *arg);

struct ddecall
  {
    OnMessage *callback;	/* send the message to this function */
    void *closure;		/* function private data */
    char deleted;		/* Has been deleted? */
    char enabled;		/* Is this call enabled? */
  };

#define	DDECall_fill(CALL,CALLBACK,CLOSURE)	(\
	(CALL)->callback=(CALLBACK), \
	(CALL)->closure=(CLOSURE), \
	(CALL)->deleted=0, \
	(CALL)->enabled=1,(CALL))

typedef struct
  {
    int (*new_server) (DDESock * s, void *address);
    int (*new_client) (DDESock * s, void *address, char *topic);
    int (*delete) (DDESock * s);
    int (*put) (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt);
    int (*putget) (DDESock * s, DDEmsg type, char *item, void **data, int *size, int *fmt);
  }
DDESfun;

#define	DDENew_server(S,A)	(*(S)->members->new_server)(S,A)
#define	DDENew_client(S,A,T)	(*(S)->members->new_client)(S,A,T)
#define	DDEDelete(S)		(*(S)->members->delete)(S)
#define	DDEPut(S,M,N,D,SZ,F)	(*(S)->members->put)(S,M,N,D,SZ,F)
#define	DDEPutget(S,M,N,D,SZ,F)	(*(S)->members->putget)(S,M,N,D,SZ,F)

struct ddesock
  {
    union
      {
	struct
	  {
	    HCONV hconv;	/* Windows conversation
				 * handle */
	    char *service;	/* Service name */
	    HDDEDATA data;	/* Sending data */
	    char rc;		/* Return code */
	  }
	windde;

#if	HAVE_SOCKET
	struct
	  {
	    SOCKET fd;		/* Socket file descriptor */
	    char readable;	/* Is the socket readable? */
	    char *server;	/* Socket file for Unix Domain */
	  }
	socket;
#endif
      }
    handle;
    DDECall *callbacks;		/* Called when sock become active */
    int ncallbacks;
    char calling;		/* Callback loop is on */
    DDEApp *app;		/* Application context */
    char *topic;		/* Topic */

    DDESfun *members;		/* DDESock member functions */
  };

static void ddesock_call (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt);

#if	HAVE_SOCKET
static void socket_getandcall (DDESock * s);

static int select_select (DDEApp * app);
static int select_enable (DDEApp * app, DDESock * s);
static int select_disable (DDEApp * app, DDESock * s);

static
DDEAfun select_members =
{
  select_select,
  select_enable,
  select_disable
};
#endif

static int windows_select (DDEApp * app);
static int windows_enable (DDEApp * app, DDESock * s);
static int windows_disable (DDEApp * app, DDESock * s);

static DDEAfun windows_amembers =
{
  windows_select,
  windows_enable,
  windows_disable
};


#if	HAVE_SOCKET
static int socket_new_server (DDESock * s, void *address);
static int socket_new_client (DDESock * s, void *address, char *topic);
static int socket_delete (DDESock * s);
static int socket_put (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt);
static int socket_putget (DDESock * s, DDEmsg type, char *item, void **data, int *size, int *fmt);

static
DDESfun socket_members =
{
  socket_new_server,
  socket_new_client,
  socket_delete,
  socket_put,
  socket_putget
};
#endif


static int windows_new_server (DDESock * s, void *address);
static int windows_new_client P_ ((DDESock * s, void *address, char *topic));
static int windows_delete (DDESock * s);
static int windows_put (DDESock * s, DDEmsg type, char *item, void *data, int size);
static int windows_putget (DDESock * s, DDEmsg type, char *item, void **data, int *size);

static DDESfun windows_smembers =
{
  windows_new_server,
  windows_new_client,
  windows_delete,
  windows_put,
  windows_putget
};


/*
 * Create an application context, given an Xtoolkit, Xview or Windows handle.
 * If no handle is passed, a select application context is created.
 */
DDEApp *
ddeApplication (DDEtype type,...)
{
  DDEApp *app = Malloc (DDEApp, 1);
  va_list ap;

  va_start (ap, type);
  if (app == NULL)
    return NULL;

  switch (type)
    {
#if	HAVE_SOCKET
    case appSELECT:
      FD_ZERO (&app->handle.select.set);
      app->handle.select.max = 0;
      app->members = &select_members;
      break;
#endif

    case appWINDOWS:
      app->handle.windde.hInstance = va_arg (ap, HANDLE);
      app->handle.windde.hWnd = va_arg (ap, HWND);
      app->handle.windde.idInst = 0L;
      app->members = &windows_amembers;
      The_Only_Application_Of_Windows = app;
      break;

    default:
      Free (app);
      return NULL;
      break;
    }
  va_end (ap);

  if (app == NULL)
    return NULL;


  {
    static Bool first_time = TRUE;

    if (first_time)
      {
	DdeInitialize (&app->handle.windde.idInst,
		(PFNCALLBACK) MakeProcInstance ((FARPROC) windows_call,
		app->handle.windde.hInstance),
		APPCLASS_STANDARD,
		0L);
	first_time = FALSE;
      }
  }


  app->ss = NULL;
  app->nss = 0;

  return app;
}

static int
dde_addsock (DDEApp * app, DDESock * s)
{
  DDESock **ss;

  for (ss = app->ss; ss - app->ss < app->nss && *ss != s; ss++);
  if (ss - app->ss < app->nss)
    return 1;

  /* Add socket to application list */
  Check_Table (DDESock *, app->ss, ss, 4);
  if (ss == NULL)
    return 0;

  *ss = s;
  app->nss++;

  return 1;
}

static int
dde_remsock (DDEApp * app, DDESock * s)
{
  DDESock **ss;

  for (ss = app->ss; ss - app->ss < app->nss && *ss != s; ss++);
  if (ss - app->ss >= app->nss)
    return 0;

  /* Remove socket from application list */
  if (ss - app->ss < app->nss - 1)
    (void) memcpy ((void *) ss, (void *) &ss[1],
		   (app->nss - (ss - app->ss) - 1) * sizeof *ss);
  app->nss--;

  return 1;
}

static DDESock *
ddeapp_sock (DDEApp * app, int (*cmp) (DDESock * s, void *arg), void *arg)
{
  DDESock **ss = app->ss;

  while (ss - app->ss < app->nss && !cmp (*ss, arg))
    ss++;

  return ss - app->ss < app->nss ? *ss : NULL;
}

static int
ddesock_addrcmp (DDESock * s, void *arg)
{
  return s == (DDESock *) arg;
}

int
ddeSelect (DDEApp * app)
{
  return DDESelect (app);
}

static void
ddesock_call (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt)
{
  DDEApp *app = s->app;
  int call = 0;

  /* Signal that we are calling callbacks */
  s->calling = 1;

  for (; call < s->ncallbacks; call++)
    {
      if (!s->callbacks[call].enabled)
	continue;

      (*s->callbacks[call].callback) (s, type, item, data, size, fmt, s->callbacks[call].closure);

      /* Socket(s) has been destroyed! */
      if (ddeapp_sock (app, ddesock_addrcmp, (void *) s) == NULL)
	return;
    }

  s->calling = 0;

  /* Delete the callbacks here */
  for (call = 0; call < s->ncallbacks;)
    if (s->callbacks[call].deleted)
      ddeRemcallback (s, &s->callbacks[call]);
    else
      call++;
}

static DDESock *ddesock_new (DDEApp * app, DDESfun * members);
static DDESock *ddesock_addr (DDEApp * app, char *host, char *server, int local, void **address);

DDESock *
ddeClient (DDEApp * app, char *host, char *server, char *topic, int local)
{
  void *address = NULL;
  DDESock *s = ddesock_addr (app, host, server, local, &address);

  if (s == NULL)
    return NULL;

  if (address != NULL
      && DDENew_client (s, address, topic)
      && dde_addsock (app, s)
      && DDEEnable (app, s))
    return s;

  ddeDelete (s);

  return NULL;
}

DDESock *
ddeServer (DDEApp * app, char *server, int local)
{
  void *address = NULL;
  DDESock *s = ddesock_addr (app, (char *) NULL, server, local, &address);

  if (s == NULL)
    return NULL;

  if (address != NULL
      && DDENew_server (s, address)
      && dde_addsock (app, s)
      && DDEEnable (app, s))
    return s;

  ddeDelete (s);

  return NULL;
}

void
ddeDelete (DDESock * s)
{
  if (s->app != NULL)
    {
      DDEDisable (s->app, s);
      (void) dde_remsock (s->app, s);
    }
  DDEDelete (s);

  if (s->topic != NULL)
    Free (s->topic);
  if (s->callbacks != NULL)
    Free (s->callbacks);

  Free (s);
}

int
ddeExecute (DDESock * s, void *data, int size, int fmt)
{
  return DDEPut (s, msgEXECUTE, (char *) NULL, data, size, fmt);
}

void *
ddeRequest (DDESock * s, char *item, int *size, int *fmt)
{
  void *data = NULL;

  return DDEPutget (s, msgREQUEST, item, &data, size, fmt) ? data : NULL;
}

int
ddePoke (DDESock * s, char *item, void *data, int size, int fmt)
{
  return DDEPut (s, msgPOKE, item, data, size, fmt);
}

int
ddeAdviseStart (DDESock * s, char *item)
{
  return DDEPutget (s, msgADVISE_START, item, (void **) NULL, (int *) NULL, (int *) NULL);
}

int
ddeAdviseStop (DDESock * s, char *item)
{
  return DDEPutget (s, msgADVISE_STOP, item, (void **) NULL, (int *) NULL, (int *) NULL);
}

int
ddeAdvise (DDESock * s, char *item, void *data, int size, int fmt)
{
  return DDEPut (s, msgADVISE, item, data, size, fmt);
}

int
ddePut (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt)
{
  return DDEPut (s, type, item, data, size, fmt);
}

DDECall *
ddeAddcallback (DDESock * s, OnMessage * callback, void *closure)
{
  DDECall *last = s->callbacks + s->ncallbacks;

  Check_Table (DDECall, s->callbacks, last, 4);
  if (last != NULL)
    {
      s->ncallbacks++;
      DDECall_fill (last, callback, closure);
    }
  return last;
}

void
ddeResetcallback (DDESock * s)
{
  if (s->callbacks != NULL)
    Free (s->callbacks);
  s->callbacks = NULL;
  s->ncallbacks = 0;
}

int
ddeRemcallback (DDESock * s, DDECall * call)
{
  DDECall *pcall = s->callbacks;

  if (pcall < s->callbacks || pcall - s->callbacks >= s->ncallbacks)
    return 0;

  if (s->calling)
    {
      pcall->deleted = 1;
      return 1;
    }
  if (pcall - s->callbacks < s->ncallbacks - 1)
    (void) memcpy ((void *) pcall,
		   (void *) &pcall[1],
	      (s->ncallbacks - (&pcall[1] - s->callbacks)) * sizeof *pcall);
  s->ncallbacks--;

  return 1;
}

static DDESock *
ddesock_new (DDEApp * app, DDESfun * members)
{
  DDESock *s = Malloc (DDESock, 1);

  if (s == NULL)
    return NULL;

  s->callbacks = NULL;
  s->ncallbacks = 0;
  s->calling = 0;
  s->app = app;
  s->topic = NULL;

  s->members = members;

  return s;
}

#if HAVE_SOCKET
static Bool winsock_running = FALSE;
#endif

static DDESock *
ddesock_addr (DDEApp * app, char *host, char *server, int local, void **address)
{
  DDESock *s = NULL;

#if HAVE_SOCKET
  if (local)
    {
#endif
      /* Use DDE transport */
      if ((s = ddesock_new (app, &windows_smembers)) == NULL)
	return NULL;

      s->handle.windde.hconv = 0;
      s->handle.windde.service = NULL;
      s->handle.windde.data = (HDDEDATA) NULL;
      s->handle.windde.rc = 0;

      if (server != NULL && address != NULL)
	*address = (void *) StringHSZ (app, server);
#if HAVE_SOCKET
    }
  else
    {
      int port;
      struct servent *se;
      struct hostent *he = NULL;
      static struct sockaddr_in inaddr;
      long addr;

      if (winsock_running == FALSE)
	{
	  WSADATA info;
	  (void) WSAStartup (0x0101, &info);
	  winsock_running = TRUE;
	}

      /* Port/Service (Stream type) */
      if (isdigit (*server))
	port = htons (atoi (server));
      else if ((se = getservbyname (server, "tcp")) != NULL)
	port = se->s_port;	/* Network byte order */
      else
	{
	  return NULL;		/* Unknown service */
	}

      /* Remote Host address */
      (void) memset (&inaddr, 0, sizeof inaddr);
      if (host == NULL || *host == '\0')
	inaddr.sin_addr.s_addr = htonl (INADDR_ANY);
      else if ((addr = inet_addr (host)) != -1 || strcmp (host, "255.255.255.255") == 0)
	(void) memcpy (&inaddr.sin_addr, &addr, sizeof addr);
      else if ((he = gethostbyname (host)) != 0)
	(void) memcpy (&inaddr.sin_addr, he->h_addr, he->h_length);
      else
	{
	  return NULL;		/* Unknown Host */
	}

      inaddr.sin_family = AF_INET;
      inaddr.sin_port = port;

      if ((s = ddesock_new (app, &socket_members)) == NULL)
	return NULL;

      s->handle.socket.fd = SOCKET_ERROR;
      s->handle.socket.readable = 0;
      s->handle.socket.server = NULL;

      if (address != NULL)
	*address = (void *) &inaddr;
    }
#endif /* HAVE_SOCKET */
  return s;
}

#if	HAVE_SOCKET
static int
select_select (DDEApp * app)
{
  int max = app->handle.select.max;
  fd_set set = app->handle.select.set;
  SOCKET fd = 0;
  int na;

  if (max <= 0)
    return 0;

  if ((na = select (max, &set, (fd_set *) NULL, (fd_set *) NULL, (struct timeval *) NULL)) <= 0)
    return 0;

  /* Some descriptor are ready for input! */
  for (; na > 0 && fd < max; fd++)
    if (FD_ISSET (fd, &set))
      {
	DDESock **ss = app->ss;
	na--;
	while (ss - app->ss < app->nss && (*ss)->handle.socket.fd != fd)
	  ss++;

	/* Call the callback associated to this stream */
	if (ss - app->ss < app->nss)
	  socket_getandcall (*ss);
      }
  return 1;
}

static int
select_enable (DDEApp * app, DDESock * s)
{
  SOCKET fd = s->handle.socket.fd;

  FD_SET (fd, &app->handle.select.set);
  if (fd >= app->handle.select.max)
    app->handle.select.max = fd + 1;

  return 1;
}

static int
select_disable (DDEApp * app, DDESock * s)
{
  SOCKET fd = s->handle.socket.fd;

  FD_CLR (fd, &app->handle.select.set);
  if (app->handle.select.max == fd + 1)
    {
      while (--fd >= 0 && !FD_ISSET (fd, &app->handle.select.set));
      app->handle.select.max = fd + 1;
    }
  return 1;
}
#endif


static int
windows_select (DDEApp * app)
{
  return 1;
}

static int
windows_enable (DDEApp * app, DDESock * s)
{
#if	HAVE_SOCKET
  if (s->members == &socket_members)
    /* Implicitely set non-blocking */
    (void) WSAAsyncSelect (s->handle.socket.fd, app->handle.windde.hWnd, (UINT) 18465,
	      (s->handle.socket.readable ? FD_READ : FD_ACCEPT) | FD_CLOSE);
#endif
  return 1;
}

static int
windows_disable (DDEApp * app, DDESock * s)
{
#if	HAVE_SOCKET
  if (s->members == &socket_members)
    {
      unsigned long nonblocking = 0;

      (void) WSAAsyncSelect (s->handle.socket.fd, app->handle.windde.hWnd, (UINT) 18465, 0L);

      /* Restore blocking mode */
      (void) ioctlsocket (s->handle.socket.fd, FIONBIO, &nonblocking);
    }
#endif
  return 1;
}

void __ddeUnblock(HWND hWnd, WPARAM wParam)
{
#if HAVE_SOCKET
  if(wParam==MY_TIMER){
    KillTimer(hWnd, MY_TIMER);
    if(WSAIsBlocking()) {
	// Cancel blocking call
	WSACancelBlockingCall();
    }
  }
#endif
}

long
ddeWindowProc (HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam)
{
  SOCKET fd = (SOCKET) wparam;
  DDEApp *app = The_Only_Application_Of_Windows;
  DDESock *s;

  if (WSAGETSELECTERROR (lparam) || app == NULL)
    return 1L;

  if ((s = ddeapp_sock (app, ddesock_fdcmp, (void *) fd)) != NULL)
    {
      /* Disable message posting for this socket */
      windows_disable (app, s);
      socket_getandcall (s);
      /* Reenable message posting */
      if (ddeapp_sock (app, ddesock_fdcmp, (void *) fd) != NULL)
	windows_enable (app, s);
    }
  return 1L;
}

static int
ddesock_fdcmp (DDESock * s, void *arg)
{
  return s->handle.socket.fd == (SOCKET) arg;
}


#if	HAVE_SOCKET
static DDESock *socket_new_connection (DDESock * s);
static int socket_get (SOCKET fd, DDEmsg * typeret, char **itemret,
		       void **dataret, int *sizeret, int *fmtret);
static int socket_read (SOCKET fd, char *buf, int size);

SOCKET
ddefileno (DDESock * s)
{
  return (s->handle.socket.fd);
}

DDESock *
fdSock (DDEApp * app, SOCKET fd, Bool listen)
{
  DDESock *s;

  if (fd != SOCKET_ERROR)
    return NULL;

  if ((s = ddesock_new (app, &socket_members)) == NULL)
    return NULL;

  s->handle.socket.fd = fd;
  s->handle.socket.readable = (char)!listen;
  s->handle.socket.server = NULL;

  if (dde_addsock (app, s) && DDEEnable (app, s))
    return s;

  ddeDelete (s);

  return NULL;
}

static int
socket_new_client (DDESock * s, void *address, char *topic)
{
  struct sockaddr *addr = (struct sockaddr *) address;

  if ((s->handle.socket.fd = socket (addr->sa_family, SOCK_STREAM, 0)) == SOCKET_ERROR)
    return 0;

  if (connect (s->handle.socket.fd, addr, Sizeofsockaddr (addr)) < 0)
    return 0;

  s->handle.socket.readable = 1;
  s->topic = local_copystring(topic);

  return DDEPutget (s, msgCONNECT, topic, (void **) NULL, (int *) NULL, (int *) NULL);
}

static int
socket_new_server (DDESock * s, void *address)
{
  struct sockaddr *addr = (struct sockaddr *) address;
  int iFlag = 1;

  if ((s->handle.socket.fd = socket (addr->sa_family, SOCK_STREAM, 0)) == SOCKET_ERROR)
    return 0;

  if (setsockopt(s->handle.socket.fd, SOL_SOCKET,SO_REUSEADDR,
         (char *)&iFlag,sizeof(iFlag))==SOCKET_ERROR)
    {
error:
	closesocket(s->handle.socket.fd);
	s->handle.socket.fd = SOCKET_ERROR;
        return 0;
    }

  if (bind (s->handle.socket.fd, addr, Sizeofsockaddr (addr)) < 0)
    goto error;

  if (listen (s->handle.socket.fd, 5) < 0)
    goto error;

  s->handle.socket.readable = 0;

  return 1;
}

static int
socket_delete (DDESock * s)
{
  if (s->handle.socket.fd != SOCKET_ERROR)
    (void) closesocket (s->handle.socket.fd);
  if (s->handle.socket.server != NULL)
    {
      (void) unlink (s->handle.socket.server);
      Free (s->handle.socket.server);
    }
  return 1;
}

static DDESock *
socket_new_connection (DDESock * s)
{
  union
  {
    struct sockaddr addr;
    struct sockaddr_in in;
  }
  addr;
  int addrlen = sizeof addr;
  SOCKET fd;
  HWND hWndMain = s->app->handle.windde.hWnd;

  SetTimer(hWndMain, MY_TIMER, ipTimeout, NULL); /* Set timeouts */

  fd  = accept (s->handle.socket.fd, &addr.addr, &addrlen);

  KillTimer(hWndMain, MY_TIMER); /* turn off the timeout timer */

  if (fd == SOCKET_ERROR)
    return NULL;

  if ((s = ddesock_new (s->app, &socket_members)) == NULL)
    {
      (void) closesocket (fd);
      return NULL;
    }
  s->handle.socket.fd = fd;
  s->handle.socket.readable = 1;
  s->handle.socket.server = NULL;

  return s;
}

static int
socket_putget (DDESock * s, DDEmsg type, char *item, void **data, int *size, int *fmt)
{
  DDEmsg reply;
  void *jdata = NULL;
  int jsize = 0;
  int jfmt = 0;
  int rc;

  if (data == NULL)
    data = &jdata;
  if (size == NULL)
    size = &jsize;
  if (fmt == NULL)
    fmt = &jfmt;

  DDEDisable (s->app, s);

  if (rc = socket_put (s, type, item, *data, *size, *fmt))
    {
      *data = NULL;
      *size = 0;
      rc = socket_get (s->handle.socket.fd, &reply, (char **) NULL, data, size, fmt);
    }
  DDEEnable (s->app, s);

  if (!rc)
    return 0;

  if ((reply != msgSUCCEED || data == &jdata) && *data != NULL)
    Free (*data);

  return reply == msgSUCCEED;
}

static void
socket_getandcall (DDESock * s)
{
  DDEApp *app = s->app;
  DDEmsg type = msgSUCCEED;
  char *item = NULL;
  void *data = NULL;
  int size = 0;
  int fmt = 0;
  int newconn = 0;

  if (!s->handle.socket.readable)
    {
      DDESock *c = socket_new_connection (s);

      if (c == NULL || !dde_addsock (app, c))
	{
	  if (c != NULL)
	    ddeDelete (c);
	  return;
	}
      /* Copia le callback di `s' in `c' */
      {
	DDECall *call = s->callbacks;

	for (; call - s->callbacks < s->ncallbacks; call++)
	  ddeAddcallback (c, call->callback, call->closure);
      }

      s = c;
      newconn = 1;
    }
  DDEDisable (app, s);

  if (!socket_get (s->handle.socket.fd, &type, &item, &data, &size, &fmt))
    {
      if (newconn)
	{
	  ddeDelete (s);
	  return;
	}
      type = msgDISCONNECT;

    }
  else if (type == msgCONNECT)
    s->topic = local_copystring (item);

  ddesock_call (s, type, item, data, size, fmt);

  /* Free data */
  if (item != NULL)
    Free (item);
  else if (data != NULL)
    Free (data);

  if (ddeapp_sock (app, ddesock_addrcmp, (void *) s) == NULL)
    return;

  if (type == msgDISCONNECT)
    ddeDelete (s);
  else
    DDEEnable (app, s);
}

typedef struct
{
  char type;			/* (DDEmsg) message type */
  char item;			/* Is there the item part */
  char fmt;			/* data format */
  char pad;			/* padding */
  short size;			/* message body size */
}
DDEhead;

/* Actually TCP Window */
#define MAX_WRITE_BLOCK_SIZ 1024

// Route to send a block of bytes
static int socket_write(SOCKET sSocket,LPSTR pBuf,int nBytesToWrite)
{
  int nBytesLeft,nWritten;

  nBytesLeft=nBytesToWrite;
  while(nBytesLeft > 0) {                   // while we haven't written enough
    nWritten=writesocket(sSocket,pBuf,
	 ((nBytesLeft>MAX_WRITE_BLOCK_SIZ)?MAX_WRITE_BLOCK_SIZ:nBytesLeft)); // write what we can
    if(nWritten <= 0) {
      return(nWritten);  // error occured
    }
    nBytesLeft -= nWritten;                 // count what we wrote
    pBuf   += nWritten;                     // adjust buffer pointer
  }
  return(nBytesToWrite - nBytesLeft);       // return count of bytes written
}

static int
socket_put (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt)
{
  DDEhead head;

  head.type = type;
  head.item = 0;
  head.fmt = fmt;
  head.pad = 0;

  if (size < 0 && data != NULL)
    size = strlen (data) + 1;
  head.size = size < 0 ? 0 : size;

  if (item != NULL)
    {
      int itemlen = strlen (item) + 1;

#define	Align(TYPE)	\
	((char*)&((struct {char _;TYPE __;}*)NULL)->__ - \
	 (char*)&((struct {char _;TYPE __;}*)NULL)->_)

      head.size += itemlen;
      if (itemlen % Align (double))
	{
	  head.pad = Align (double) - itemlen % Align (double);
	  head.size += head.pad;
	}
      head.item = 1;


    }

  /*
   * Actually, the writes should be only one, since if the
   * socket is shared between two writing process, then the
   * records may be intermixed!
   */
  head.size = htons (head.size);
  (void) socket_write (s->handle.socket.fd, (char *) &head, sizeof head);
  if (item != NULL)
    {
      static char zeros[sizeof (double)];
      (void) socket_write (s->handle.socket.fd, item, strlen (item) + 1);
      if (head.pad)
	(void) socket_write (s->handle.socket.fd, zeros, head.pad);
    }
  if (size)
    (void) socket_write (s->handle.socket.fd, data, size);
  return 1;
}

static int
socket_get (SOCKET fd, DDEmsg * typeret, char **itemret, void **dataret, int *sizeret, int *fmtret)
{
  DDEhead head;

  if (!socket_read (fd, (char *) &head, sizeof head))
    return 0;

  head.size = ntohs (head.size);

  if (typeret != NULL)
    *typeret = head.type;
  if (itemret != NULL)
    *itemret = NULL;
  if (fmtret != NULL)
    *fmtret = head.fmt;
  if (sizeret != NULL)
    *sizeret = 0;

  if (head.size <= 0)
    return 1;

  {
    char *data = Malloc (char, head.size);
    char *pdata = data;
    char *item = NULL;

    if (data == NULL)
      return 0;

    if (!socket_read (fd, data, head.size))
      {
	Free (pdata);
	return 0;
      }
    if (head.item)
      {
	int itemlen = strlen (item = data) + 1;

	data += itemlen + head.pad;
	head.size -= itemlen + head.pad;
      }
    if (itemret != NULL)
      *itemret = item;
    if (dataret != NULL && head.size > 0)
      *dataret = data;
    if (sizeret != NULL)
      *sizeret = head.size;

    if ((dataret == NULL || head.size <= 0) && itemret == NULL)
      Free (pdata);

    return 1;
  }
}

/* Non-blocking read from a socket (for slow bad links) */
static int socket_read(SOCKET sock, char * buffer, int length)
{
  int          result;
  char        *buf = buffer;
#ifndef NO_SELECT_CALL
  FD_SET       readSockSet;
  FD_SET       writeSockSet;
  LPFD_SET     readSet;
  LPFD_SET     writeSet;

  TIMEVAL      timeOutStruct;
  LPTIMEVAL    timeOut;

  timeOutStruct.tv_sec = (ipReadTimeout/1000);	/* seconds     */
  timeOutStruct.tv_usec= (ipReadTimeout%1000);	/* microseconds*/
  timeOut=&timeOutStruct;
  readSet=&readSockSet;
  writeSet=&writeSockSet;
#endif		/* ndef NO_SELECT_CALL */

  while(length > 0) {
#ifndef NO_SELECT_CALL
    /* Is there anything ready to receive? */
    FD_ZERO(readSet);
    FD_ZERO(writeSet);
    FD_SET(sock,readSet);

    result=select((int)0,(LPFD_SET)readSet,(LPFD_SET)writeSet,(LPFD_SET)NULL,(LPTIMEVAL)timeOut);
    switch(result) {
    case 0:
      /* "Connection timed out" */
      return(0);
    case SOCKET_ERROR:
      return(SOCKET_ERROR);
    case 1:
      break;
    default:
      /* "Select() returned %d", result */
      return(0);
    }
#endif		/* ndef NO_SELECT_CALL */

/*    result=readsocket(sock, buf, length, 0); */
    result=readsocket(sock, buf, length);

    if (result < 0) 
       return(SOCKET_ERROR);
    else if (result == 0) {
/*    connection closed */
      break;
    }
    length -= result;
    buf += result;
  }	/* for() */
  return buf - buffer;
}

#endif


static int
windows_new_client (DDESock * s, void *address, char *topic)
{
  s->topic = local_copystring (topic);
  s->handle.windde.hconv = DdeConnect (s->app->handle.windde.idInst,
				   (HSZ) address, StringHSZ (s->app, topic),
				       (PCONVCONTEXT) NULL);

  return s->handle.windde.hconv != (HCONV) NULL;
}

static int
windows_new_server (DDESock * s, void *address)
{
  s->handle.windde.service = HSZstring (s->app, (HSZ) address);
  return DdeNameService (s->app->handle.windde.idInst,
			 (HSZ) address, (HSZ) NULL, DNS_REGISTER) != 0L;
}

static int
windows_delete (DDESock * s)
{
  if (s->handle.windde.service != NULL)
    Free (s->handle.windde.service);

  if (s->handle.windde.hconv != (HCONV) NULL)
    DdeDisconnect (s->handle.windde.hconv);

  return 1;
}

static int
windows_putget (DDESock * s, DDEmsg type, char *item, void **data, int *size)
{
  void *jdata = NULL;
  int jsize = 0;
  HDDEDATA rdata;
  DWORD rc;

  if (data == NULL)
    data = &jdata;
  if (size == NULL)
    size = &jsize;

  switch (type)
    {
    default:
      return 0;
    case msgADVISE_START:
      type = XTYP_ADVSTART;
      break;
    case msgADVISE_STOP:
      type = XTYP_ADVSTOP;
      break;
    case msgREQUEST:
      type = XTYP_REQUEST;
      break;
    }

  rdata = DdeClientTransaction ((LPBYTE) NULL, 0, s->handle.windde.hconv,
			StringHSZ (s->app, item), CF_TEXT, type, 5000, &rc);

  *data = NULL;
  *size = 0;

  if (type != XTYP_REQUEST)
    return rc != 0L;

  if (rdata == (HDDEDATA) NULL)
    return 0;

  /* Returns data only if requested */
  if (data != &jdata)
    *data = HDDEDATAdata (rdata, size);

  DdeFreeDataHandle (rdata);

  return 1;
}

static int
windows_put (DDESock * s, DDEmsg type, char *item, void *data, int size)
{
  if (size < 0 && data != NULL)
    size = strlen (data) + 1;
  if (size < 0)
    size = 0;

  switch (type)
    {
    default:
      return 0;

    case msgADVISE:
    case msgSUCCEED:
      {
	/* Reply for msgREQUEST */
	HDDEDATA handle;

	if ((handle = DdeCreateDataHandle (s->app->handle.windde.idInst,
					   (LPBYTE) data, size, 0, StringHSZ (s->app, item), CF_TEXT, 0)) == (HDDEDATA) NULL)
	  return 0;

	s->handle.windde.data = handle;
	s->handle.windde.rc = 1;

	if (type != msgADVISE)
	  return 1;

	return DdePostAdvise (s->app->handle.windde.idInst,
			      StringHSZ (s->app, s->topic),
			      StringHSZ (s->app, item));
      }
    case msgFAIL:
      s->handle.windde.rc = 0;
      return 1;

    case msgPOKE:
      type = XTYP_POKE;
      break;
    }

  return DdeClientTransaction ((LPBYTE) data, size, s->handle.windde.hconv,
			       StringHSZ (s->app, item), CF_TEXT, type,
			       5000, NULL) != (HDDEDATA) NULL;
}

static DDESock *ddewin_sock (DDEApp * app, HCONV hconv,
			     HSZ itemsz, HDDEDATA hdata,
			     char **item, void **data, int *size);

static int ddesock_servcmp (DDESock * s, void *arg);
static int ddesock_hconvcmp (DDESock * s, void *arg);

HDDEDATA EXPENTRY _EXPORT
windows_call (WORD type, WORD fmt,
	      HCONV hconv, HSZ hsz1, HSZ hsz2, HDDEDATA hdata,
	      DWORD ldata1, DWORD ldata2)
{
  static DDESock *connecting = NULL;
  DDEApp *app = The_Only_Application_Of_Windows;

  if (app == NULL)
    return 0;

  switch (type)
    {
    case XTYP_CONNECT:
      {
	char *service = HSZstring (app, hsz2);
	char *item;
	DDESock *s, *c;

	if (service == NULL)
	  return 0;

	s = ddeapp_sock (app, ddesock_servcmp, (void *) service);
	Free (service);
	if (s == NULL)
	  return 0;

	if ((c = ddesock_new (app, &windows_smembers)) == NULL
	    || !dde_addsock (app, c))
	  return 0;

	c->handle.windde.hconv = 0;
	c->handle.windde.service = NULL;
	c->handle.windde.data = (HDDEDATA) NULL;
	c->handle.windde.rc = 0;

	if ((item = HSZstring (app, hsz1)) == NULL)
	  return 0;
	c->topic = local_copystring (item);

	/* Copia le callback di `s' in `c' */
	{
	  DDECall *call = s->callbacks;

	  for (; call - s->callbacks < s->ncallbacks; call++)
	    ddeAddcallback (c, call->callback, call->closure);
	}

	ddesock_call (c, msgCONNECT, item, (void *) NULL, 0, 0);
	Free (item);

	if (ddeapp_sock (app, ddesock_addrcmp, (void *) c) == NULL)
	  return 0;

	connecting = c;
	return 1;
      }
    case XTYP_CONNECT_CONFIRM:	/* hconv is valid here! */
      if (connecting == NULL
	  || ddeapp_sock (app, ddesock_addrcmp, (void *) connecting) == NULL)
	return 0;

      DDEEnable (app, connecting);
      connecting->handle.windde.hconv = hconv;
      connecting = NULL;

      return 1;

    case XTYP_DISCONNECT:
      {
	DDESock *s = ddeapp_sock (app, ddesock_hconvcmp, (void *)hconv);

	if (s == NULL)
	  return 0;

	ddesock_call (s, msgDISCONNECT, (char *) NULL, (void *) NULL, 0, 0);

	return ddeapp_sock (app, ddesock_hconvcmp, (void *)hconv) == NULL;
      }
    case XTYP_EXECUTE:
      {
	int size = 0;
	void *data = NULL;
	DDESock *s = ddewin_sock (app, hconv, (HSZ) NULL, hdata, (char **) NULL, &data, &size);

	if (s == NULL || data == NULL)
	  return DDE_FNOTPROCESSED;

	ddesock_call (s, msgEXECUTE, (char *) NULL, data, size, fmt);

	return DDE_FACK;
      }
    case XTYP_REQUEST:
      {
	char *item = NULL;
	DDESock *s = ddewin_sock (app, hconv, hsz2, (HDDEDATA) NULL, (char **) &item, (void **) NULL, (int *) NULL);

	if (s == NULL || item == NULL)
	  return (HDDEDATA) NULL;

	ddesock_call (s, msgREQUEST, item, (void *) NULL, 0, 0);

	return ddeapp_sock (app, ddesock_hconvcmp, (void *)hconv) == NULL ?
	  (HDDEDATA) NULL : s->handle.windde.data;
      }
    case XTYP_POKE:
      {
	char *item = NULL;
	int size = 0;
	void *data = NULL;
	DDESock *s = ddewin_sock (app, hconv, hsz2, hdata, &item, &data, &size);

	if (s == NULL || item == NULL || data == NULL)
	  return DDE_FNOTPROCESSED;

	ddesock_call (s, msgPOKE, item, data, size, fmt);

	return DDE_FACK;
      }
    case XTYP_ADVSTART:
      {
	char *item = NULL;
	DDESock *s = ddewin_sock (app, hconv, hsz2, (HDDEDATA) NULL, (char **) &item, (void **) NULL, (int *) NULL);

	if (s == NULL || item == NULL)
	  return DDE_FNOTPROCESSED;

	ddesock_call (s, msgADVISE_START, item, (void *) NULL, 0, 0);

	return ddeapp_sock (app, ddesock_hconvcmp, (void *) hconv) == NULL ?
	  0 : s->handle.windde.rc;
      }
    case XTYP_ADVSTOP:
      {
	char *item = NULL;
	DDESock *s = ddewin_sock (app, hconv, hsz2, (HDDEDATA) NULL, (char **) &item, (void **) NULL, (int *) NULL);

	if (s == NULL || item == NULL)
	  return DDE_FNOTPROCESSED;

	ddesock_call (s, msgADVISE_STOP, item, (void *) NULL, 0, 0);

	return ddeapp_sock (app, ddesock_hconvcmp, (void *) hconv) == NULL ?
	  0 : s->handle.windde.rc;
      }
    case XTYP_ADVREQ:
      {
	DDESock *s = ddeapp_sock (app, ddesock_hconvcmp, (void *) hconv);
	HDDEDATA hdata;

	if (s == NULL || s->handle.windde.data == (HDDEDATA) NULL)
	  return (HDDEDATA) NULL;

	hdata = s->handle.windde.data;
	s->handle.windde.data = (HDDEDATA) NULL;

	return hdata;
      }
    case XTYP_ADVDATA:
      {
	char *item = NULL;
	int size = 0;
	void *data = NULL;
	DDESock *s = ddewin_sock (app, hconv, hsz2, hdata, &item, &data, &size);

	if (s == NULL || item == NULL || data == NULL)
	  return DDE_FNOTPROCESSED;

	ddesock_call (s, msgADVISE, item, data, size, fmt);

	return DDE_FACK;
      }
    default:
      return 0;
    }
  /* NOTREACHED */
}

static int
ddesock_servcmp (DDESock * s, void *arg)
{
  return s->handle.windde.service != NULL
    && strcmp (s->handle.windde.service, (char *) arg) == 0;
}

static int
ddesock_hconvcmp (DDESock * s, void *arg)
{
  return s->handle.windde.hconv == (HCONV) arg;
}

static DDESock *
ddewin_sock (DDEApp * app,
HCONV hconv, HSZ itemsz, HDDEDATA hdata, char **item, void **data, int *size)
{
  DDESock *s = ddeapp_sock (app, ddesock_hconvcmp, (void *) hconv);

  if (s == NULL)
    return NULL;

  if (data != NULL)
    *data = HDDEDATAdata (hdata, size);
  if (hdata != (HDDEDATA) NULL)
    DdeFreeDataHandle (hdata);
  if (item != NULL)
    *item = HSZstring (app, itemsz);

  return s;
}

static void *
HDDEDATAdata (HDDEDATA hdata, int *size)
{
  void *rdata = NULL;
  int jsize;

  if (hdata == (HDDEDATA) NULL)
    return NULL;

  if (size == NULL)
    size = &jsize;
  *size = (int) DdeGetData (hdata, (void *) NULL, 0, 0);
  if (*size > 0 && (rdata = Calloc (char, *size)) != NULL)
      (void) DdeGetData (hdata, rdata, *size, 0);

  return rdata;
}

static
char *
HSZstring (DDEApp * app, HSZ stringhsz)
{
  int length;
  char *string;

  if (stringhsz == (HSZ) NULL)
    return NULL;

  if ((length = (int) DdeQueryString (app->handle.windde.idInst, stringhsz, (LPSTR) NULL, 0, CP_WINANSI)) < 0)
    return NULL;

  if ((string = Malloc (char, length + 1)) == NULL)
      return NULL;

  (void) DdeQueryString (app->handle.windde.idInst, stringhsz, (LPSTR) string, length + 1, CP_WINANSI);

  return string;
}

void
CleanUpSockets(void)
{
#if HAVE_SOCKET
  if (winsock_running)
    WSACleanup();
  winsock_running = FALSE;
#endif
}
