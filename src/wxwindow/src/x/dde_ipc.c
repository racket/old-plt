/*-
 * File: dde_ipc.c
 * Purpose: Implement DDE with sockets (X-Windows version).
 * Original Author: Giordano Pezzoli <gio@crcc.it>
 * Modified by: E. Zimmermann <edz@BSn.com>
 */
static char RCS_id[] = "$Id: dde_ipc.c,v 1.4 1995/03/14 16:29:19 edz Exp edz $";

/*
 * TODO:
 * - Re-implement as C++ class
 * - Use ACE Socket abstraction
 * This would allow for a common dde_ipc.cc
 * and have ACE handle the machine/OS dependencies
 */

/*
 * History
 * =======
 * $Log: dde_ipc.c,v $
 * Revision 1.4  1995/03/14  16:29:19  edz
 * Sync for distribution.
 *
 *
 */

#define UNIX_ADDRESSING 1	/* Unix System "Internal" protocols */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#include <stdarg.h>
#include <sys/types.h>
#include <pwd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#ifdef	UNIX_ADDRESSING
#include <sys/stat.h>
#include <sys/un.h>
#endif
#include <netdb.h>
#ifdef wx_motif
# include  <X11/Intrinsic.h>
# ifndef	XtPointer
#  define	XtPointer	caddr_t*	/* X11r3 style */
# endif
#endif /* wx_motif */
#ifdef wx_xview
#include  <xview/xview.h>
#endif /* wx_xview */
#include "dde_ipc.h"

#ifdef	UNIX_ADDRESSING
#define	Sizeofsockaddr(ADDR)	(\
		(ADDR)->sa_family==AF_INET ? sizeof(struct sockaddr_in): \
			(char*)&((struct sockaddr_un*)(ADDR))->sun_path- \
			(char*)((struct sockaddr_un*)(ADDR)) + \
			xstrnlen(((struct sockaddr_un*)(ADDR))->sun_path, \
				sizeof((struct sockaddr_un*)(ADDR))->sun_path) \
		)

#else
#define	Sizeofsockaddr(ADDR)	sizeof(struct sockaddr_in)
#endif /* UNIX_ADDRESSING */

#define	writesocket(FD,BUF,SIZE)	write(FD,BUF,SIZE)
#define	readsocket(FD,BUF,SIZE)		read(FD,BUF,SIZE)
#define	closesocket(FD)			close(FD)

#define INVALID_SOCKET	-1


#if	defined(__cplusplus)
extern "C" {
#endif
  int select (int, struct fd_set *, struct fd_set *, struct fd_set *, struct timeval *);
  int socket (int, int, int);
  int connect (int, const void *, int);
  int bind (int, struct sockaddr *, int);
  int accept (int, struct sockaddr *, int *);
  int listen (int, int);
  long inet_addr (char *);
#if	defined(__cplusplus)
};
#endif

/* Convienience Macros */
#define	Malloc(TYPE,N)		((TYPE*)malloc(sizeof(TYPE)*(N)))
#define	Free(PTR)		free((void *)(PTR))
#define	Realloc(TYPE,PTR,N)	((PTR)=Xrealloc(TYPE,(void*)(PTR),sizeof(TYPE)*(N)))
#define	Xrealloc(TYPE,PTR,N)	((PTR)==NULL ? (TYPE*)malloc(N):(TYPE*)realloc(PTR,N))
#define	Check_Table(TYPE,T,PT,CS)	{\
		int	s_=(PT)-(T); \
		if ( s_%(CS)==0 && ((PT)=Realloc(TYPE,T,s_+(CS)))!=NULL ) \
			(PT) += s_;}
#define strdup(_x) xstrcat(_x, NULL, NULL, NULL)

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
#ifdef wx_motif
	XtAppContext appContext;	/* X toolkit */
#endif
#ifdef wx_xview
	Frame frame;		/* Xview */
#endif
	struct
	  {
	    fd_set set;		/* select descriptors */
	    int max;		/* max descriptor set */
	  }
	select;
      }
    handle;
    DDESock **ss;		/* Socket to monitor */
    int nss;
    DDEAfun *members;
  };

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
	    int fd;		/* Socket file descriptor */
	    char readable;	/* Is the socket readable? */
	    char *server;	/* Socket file for Unix Domain */
	  }
	socket;
      }
    handle;
    DDECall *callbacks;		/* Called when sock become active */
    int ncallbacks;
    char calling;		/* Callback loop is on */
    DDEApp *app;		/* Application context */
    char *topic;		/* Topic */
#ifdef wx_motif
    unsigned long xtinputid;	/* handle for xtoolkit */
#endif
    DDESfun *members;		/* DDESock member functions */
  };

static void ddesock_call (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt);

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

#ifdef wx_motif
static int xtoolkit_select (DDEApp * app);
static int xtoolkit_enable (DDEApp * app, DDESock * s);
static int xtoolkit_disable (DDEApp * app, DDESock * s);

static DDEAfun xtoolkit_members =
{
  xtoolkit_select,
  xtoolkit_enable,
  xtoolkit_disable
};
#endif

#ifdef wx_xview
static int xview_select (DDEApp * app);
static int xview_enable (DDEApp * app, DDESock * s);
static int xview_disable (DDEApp * app, DDESock * s);

static DDEAfun xview_members =
{
  xview_select,
  xview_enable,
  xview_disable
};
#endif

static int socket_new_server (DDESock *, void *);
static int socket_new_client (DDESock *, void *, char *);
static int socket_delete (DDESock * s);
static int socket_put (DDESock *, DDEmsg, char *, void *, int, int);
static int socket_putget (DDESock *, DDEmsg, char *, void **, int *, int *);

static DDESfun socket_members =
{
  socket_new_server,
  socket_new_client,
  socket_delete,
  socket_put,
  socket_putget
};

static char *xstrcat (const char *, const char *, const char *, const char *);
static int xstrnlen (const char *, int);

/*
 * Create an application context, given an Xtoolkit or Xview handle.
 * If no handle is passed, a select application context is created.
 */
/* VARARGS PUBLIC */
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
    case appSELECT:
      FD_ZERO (&app->handle.select.set);
      app->handle.select.max = 0;
      app->members = &select_members;
      break;
#ifdef wx_motif
    case appXTOOLKIT:
      app->handle.appContext = va_arg (ap, XtAppContext);
      app->members = &xtoolkit_members;
      break;
#endif
#ifdef wx_xview
    case appXVIEW:
      app->handle.frame = va_arg (ap, Frame);
      app->members = &xview_members;
      break;
#endif
    default:
      Free (app);
      return NULL;
      break;
    }
  va_end (ap);

  if (app == NULL)
    return NULL;
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
    (void) memcpy ((void *) ss, (void *) &ss[1], (app->nss - (ss - app->ss) - 1) * sizeof *ss);
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

/* PUBLIC */
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
static DDESock *ddesock_addr (DDEApp * app, char *host, char *server, Bool local, void **address);

/* PUBLIC */
DDESock *
ddeClient (DDEApp * app, char *host, char *server, char *topic, Bool local)
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

/* PUBLIC */
DDESock *
ddeServer (DDEApp * app, char *server, Bool local)
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

/* PUBLIC */
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

/* PUBLIC */
int
ddeExecute (DDESock * s, void *data, int size, int fmt)
{
  return DDEPut (s, msgEXECUTE, (char *) NULL, data, size, fmt);
}

/* PUBLIC */
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

/* PUBLIC */
void
ddeResetcallback (DDESock * s)
{
  if (s->callbacks != NULL)
    Free (s->callbacks);
  s->callbacks = NULL;
  s->ncallbacks = 0;
}

/* PUBLIC */
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
#ifdef wx_motif
  s->xtinputid = 0;
#endif
  s->members = members;

  return s;
}

static DDESock *
ddesock_addr (DDEApp * app, char *host, char *server, Bool local, void **address)
{
  DDESock *s = NULL;

  if (server == NULL)
    server = "wxwin-ipc";
#ifdef	UNIX_ADDRESSING
  /* Local (UNIX) Transport */
  if (*server == '.' || *server == '/' || local)
    {
      static struct sockaddr_un unaddr;

      if (*server == '/')
	{
	  /* Absolute path */
	  server = strdup (server);
	}
      else
	{
	  uid_t uid = getuid ();
	  struct passwd *pw = getpwuid (uid);

	  /* Root and some other ids are special */
	  char *user = (pw ? pw->pw_name : "nobody");
	  char *home = (pw && uid >= 10 ? pw->pw_dir : NULL);

	  if (*server == '.' && home)
	    {
	      /* Put in "~/.ipc" */
	      home = xstrcat(home, "/.ipc", NULL, NULL);
	      mkdir (home, S_IRUSR | S_IWUSR | S_IXUSR);
	      server = xstrcat (home, "/", server, NULL);
	      Free(home);
	    }
	  else
	    {
	      /* Put in "/tmp" */
	      server = xstrcat ("/tmp/", user, ":", server);
	    }

	}


      (void) memset (&unaddr, 0, sizeof unaddr);
      unaddr.sun_family = AF_UNIX;
      (void) strncpy (unaddr.sun_path, server, sizeof unaddr.sun_path);

      if ((s = ddesock_new (app, &socket_members)) == NULL)
	{
	  Free (server);
	  return NULL;
	}
      s->handle.socket.fd = INVALID_SOCKET;
      s->handle.socket.readable = 0;

      if (host == NULL)
	s->handle.socket.server = server;
      else
	{
	  s->handle.socket.server = NULL;
	  Free (server);
	}

      if (address != NULL)
	*address = (void *) &unaddr;
    }
  else
#endif
    {
      int port;
      struct servent *se;
      struct hostent *he = NULL;
      static struct sockaddr_in inaddr;
      long addr;
      const char err[] = "\
wxWindows IPC: Unknown %s \"%s\"\n";

      /* Port/Service (Stream type)*/
      if (isdigit (*server))
	port = htons (atoi (server));
      else if ((se = getservbyname (server, "tcp")) != NULL)
	port = se->s_port;	/* Network byte order */
      else
	{
	  fprintf(stderr, err, "service", server);
	  return NULL; /* Unknown service */
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
	  fprintf(stderr, err, "host", host); 
	  return NULL; /* Unknown Host */
	}

      inaddr.sin_family = AF_INET;
      inaddr.sin_port = port;

      if ((s = ddesock_new (app, &socket_members)) == NULL)
	return NULL;

      s->handle.socket.fd = INVALID_SOCKET;
      s->handle.socket.readable = 0;
      s->handle.socket.server = NULL;

      if (address != NULL)
	*address = (void *) &inaddr;
    }
  return s;
}

static int
select_select (DDEApp * app)
{
  int max = app->handle.select.max;
  fd_set set = app->handle.select.set;
  int fd = 0;
  int na;
#if 1
  /* Non-Blocking Select */
  struct timeval  timeout;

  if (max <= 0)
    return 0;

  timeout.tv_sec = 20;
  timeout.tv_usec = 0;
  if ((na = select(max, &set, NULL, NULL, &timeout)) <= 0)
    return 0;
#else
  /* Blocking select */
  if (max <= 0)
    return 0;

  if ((na = select (max, &set, NULL, NULL, NULL)) <= 0)
    return 0;
#endif

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
  int fd = s->handle.socket.fd;

  FD_SET (fd, &app->handle.select.set);
  if (fd >= app->handle.select.max)
    app->handle.select.max = fd + 1;

  return 1;
}

static int
select_disable (DDEApp * app, DDESock * s)
{
  int fd = s->handle.socket.fd;

  FD_CLR (fd, &app->handle.select.set);
  if (app->handle.select.max == fd + 1)
    {
      while (--fd >= 0 && !FD_ISSET (fd, &app->handle.select.set));
      app->handle.select.max = fd + 1;
    }
  return 1;
}

#ifdef wx_motif
static int
xtoolkit_select (DDEApp * app)
{
  XtAppMainLoop (app->handle.appContext);
  /* Actually, never returns! */
  return 1;
}

static
Bool xtoolkit_call (XtPointer closure, int *fid, XtInputId * id);

static int
xtoolkit_enable (DDEApp * app, DDESock * s)
{
  if (!s->xtinputid)
    s->xtinputid = XtAppAddInput (app->handle.appContext,
				  s->handle.socket.fd,
				  (XtPointer) XtInputReadMask,
				  (XtInputCallbackProc) xtoolkit_call,
				  (XtPointer) s);

  return s->xtinputid != 0;
}

static int
xtoolkit_disable (DDEApp * app, DDESock * s)
{
  if (!s->xtinputid)
    return 0;

  XtRemoveInput (s->xtinputid);
  s->xtinputid = 0;

  return 1;
}

static Bool
xtoolkit_call (XtPointer closure, int *fid, XtInputId * id)
{
  socket_getandcall ((DDESock *) closure);

  return 0;
}
#endif

#ifdef wx_xview
static int
xview_select (DDEApp * app)
{
  Frame frame = app->handle.frame;
  xv_main_loop (frame);
  /* Actually, never returns */
  return 1;
}

static Notify_value xview_call (Notify_client closure, int fd);

static int
xview_disable (DDEApp * app, DDESock * s)
{
  return NOTIFY_OK != notify_set_input_func ((Notify_client) s, NOTIFY_FUNC_NULL, s->handle.socket.fd);
}

static int
xview_enable (DDEApp * app, DDESock * s)
{
  return NOTIFY_OK != notify_set_input_func ((Notify_client) s, (Notify_func) xview_call, s->handle.socket.fd);
}

static Notify_value
xview_call (Notify_client closure, int fd)
{
  socket_getandcall ((DDESock *) closure);

  return (Notify_value) 0;
}
#endif

static DDESock *socket_new_connection (DDESock * s);
static int socket_get (int fd, DDEmsg * typeret, char **itemret, void **dataret, int *sizeret, int *fmtret);
static int socket_read (int fd, char *buf, int size);

int
ddefileno (DDESock * s)
{
  return s->handle.socket.fd;
}

DDESock *
fdSock (DDEApp * app, int fd, Bool listen)
{
  DDESock *s;

  if (fd == INVALID_SOCKET)
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

  if ((s->handle.socket.fd = socket (addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
    return 0;

  if (connect (s->handle.socket.fd, addr, Sizeofsockaddr (addr)) < 0)
    return 0;

  s->handle.socket.readable = 1;
  s->topic = strdup (topic);

  return DDEPutget (s, msgCONNECT, topic, (void **) NULL, (int *) NULL, (int *) NULL);
}

static int
socket_new_server (DDESock * s, void *address)
{
  struct sockaddr *addr = (struct sockaddr *) address;

  if ((s->handle.socket.fd = socket (addr->sa_family, SOCK_STREAM, 0)) == INVALID_SOCKET)
    return 0;

  if (bind (s->handle.socket.fd, addr, Sizeofsockaddr (addr)) < 0) {
error:
    closesocket(s->handle.socket.fd);
    s->handle.socket.fd = INVALID_SOCKET;
    return 0;
  }

  if (listen (s->handle.socket.fd, 5) < 0) 
    goto error;

  s->handle.socket.readable = 0;

  return 1;
}

static int
socket_delete (DDESock * s)
{
  if (s->handle.socket.fd != INVALID_SOCKET)
    {
      (void) closesocket (s->handle.socket.fd);
      s->handle.socket.fd = INVALID_SOCKET;
    }
  if (s->handle.socket.server != NULL)
    {
      (void) unlink (s->handle.socket.server);
      Free (s->handle.socket.server);
      s->handle.socket.server = NULL;
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
#ifdef	UNIX_ADDRESSING
    struct sockaddr_un un;
#endif
  }
  addr;

  int addrlen = sizeof addr;
  int fd = accept (s->handle.socket.fd, (struct sockaddr *) &addr.addr, &addrlen);

  if (fd == INVALID_SOCKET)
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

/* TODO: Clean up this indirection without a reason! */
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

  if ((rc = socket_put (s, type, item, *data, *size, *fmt)) != 0)
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
    s->topic = strdup (item);

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

static int
socket_put (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt)
{
  DDEhead head;

  head.type = type;
  head.item = 0;
  head.fmt = fmt;
  head.pad = 0;

  if (size < 0 && data != NULL)
    size = strlen ((char *) data) + 1;
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

#undef		Align
    }
#ifdef	SIGPIPE
  {
    void (*osig) (int,...) = signal (SIGPIPE, SIG_IGN);
#endif

    /*
     * Actually, the writes should be only one, since if the
     * socket is shared between two writing process, then the
     * records may be intermixed!
     */
    head.size = htons (head.size);
    (void) writesocket (s->handle.socket.fd, (char *) &head, sizeof head);
    if (item != NULL)
      {
	static char zeros[sizeof (double)];
	(void) writesocket (s->handle.socket.fd, item, strlen (item) + 1);
	if (head.pad)
	  (void) writesocket (s->handle.socket.fd, zeros, head.pad);
      }
    if (size)
      (void) writesocket (s->handle.socket.fd, data, size);

#ifdef	SIGPIPE
    (void) signal (SIGPIPE, osig);
  }
#endif

  return 1;
}

static int
socket_get (int fd, DDEmsg * typeret, char **itemret, void **dataret, int *sizeret, int *fmtret)
{
  DDEhead head;

  if (!socket_read (fd, (char *) &head, sizeof head))
    return 0;

  head.size = ntohs (head.size);

  if (typeret != NULL)
    *typeret = (enum DDEmsg) head.type;
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

static int
socket_read (int fd, char *buf, int size)
{
  char *pbuf = buf;
  int nc;

  while (size > 0 && (nc = readsocket (fd, buf, size)) > 0)
    {
      size -= nc;
      buf += nc;
    }
  return buf - pbuf;
}


static char *
xstrcat (const char *s1, const char *s2, const char *s3, const char *s4)
{
  size_t len_s1 = s1 ? strlen (s1) : 0;
  size_t len_s2 = s2 ? strlen (s2) : 0;
  size_t len_s3 = s3 ? strlen (s3) : 0;
  size_t len_s4 = s4 ? strlen (s4) : 0;
  char *mem;

  if ((mem = Malloc (char, len_s1 + len_s2 + len_s3 + len_s4 + 1)) != NULL)
    {
      mem[0] = '\0';
      if (len_s1)
	memcpy (mem, s1, len_s1 + 1);
      if (len_s2)
	memcpy (mem + len_s1, s2, len_s2 + 1);
      if (len_s3)
	memcpy (mem + len_s1 + len_s2, s3, len_s3 + 1);
      if (len_s4)
	memcpy (mem + len_s1 + len_s2 + len_s3, s4, len_s4 + 1);
    }

  return mem;
}

static int
xstrnlen (const char *str, register int nc)
{
  register const char *pstr = str;

  while (nc-- > 0 && *str)
    str++;

  return str - pstr;
}
