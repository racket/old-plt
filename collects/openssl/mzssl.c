/* ssl.c: an extension to PLT MzScheme to allow SSL connections */

#include <openssl/ssl.h>
#include <openssl/err.h>
#include "escheme.h"

#ifdef USE_UNIX_SOCKETS_TCP
# include <sys/types.h>
# include <netinet/in.h>
# include <netdb.h>
# include <sys/socket.h>
# include <sys/time.h>
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>
# define SOCK_ERRNO() errno
# define NOT_WINSOCK(x) (x)
# define INVALID_SOCKET (-1)
# define WAS_EINPROGRESS(e) ((e == EINPROGRESS))
# define mz_hstrerror(x) dup_errstr(hstrerror(x))
#endif

#ifdef USE_WINSOCK_TCP
# include <winsock.h>
# define SOCK_ERRNO() WSAGetLastError()
# define NOT_WINSOCK(x) 0
# define WAS_EINPROGRESS(e) ((e == WSAEINPROGRESS))
# define mz_hstrerror(x) "Unknown error"
#endif

/* stolen from $(PLTHOME}/src/mzscheme/src/schpriv.h */
#ifdef USE_FCNTL_O_NONBLOCK
# define MZ_NONBLOCKING O_NONBLOCK
#else
# define MZ_NONBLOCKING FNDELAY
#endif

/* stolen from $(PLTHOME)/src/mzscheme/src/network.c */
# ifdef PROTOENT_IS_INT
#  define PROTO_P_PROTO PROTOENT_IS_INT
# else
#  define PROTO_P_PROTO proto->p_proto
# endif

/* stolen from $(PLTHOME)/src/mzscheme/src/schfd.h */
#ifdef USE_FAR_MZ_FDCALLS
# define DECL_FDSET(n, c) static fd_set *n
# define INIT_DECL_FDSET(n, c) (n = (n ? (fd_set *)scheme_init_fdset_array(n,c)\
                                       : (fd_set*)scheme_alloc_fdset_array(c,1)))
#else
# define DECL_FDSET(n, c) fd_set n[c]
# define INIT_DECL_FDSET(n, c) /* empty */
#endif

struct sslplt {
  SSL *ssl;
  char ibuffer, obuffer;
  char ib_used, ob_used;
  char close_in, close_out;
  char write_blocked_reason; /* 0 => might not be blocked, 1 => for read, 2 => for write */
  struct sslplt *next;
};

/*****************************************************************************
 * TOP-LEVEL THREAD: This is the routine and data involved with running the  *
 * top level thread (the one that helps us fake a couple guarantees).        *
 *****************************************************************************/
Scheme_Object *daemon_lock = NULL;
struct sslplt *ssls = NULL;

/* create_ register_sslplt: called when a new sslplt structure needs to be 
   created and registered with out flusing/closing thread. */
struct sslplt *create_register_sslplt(SSL *ssl)
{
  struct sslplt *sslplt = scheme_malloc(sizeof(struct sslplt));

  sslplt->ssl = ssl;
  sslplt->ib_used = 0; sslplt->ob_used = 0; 
  sslplt->close_in = 0; sslplt->close_out = 0;
  return sslplt;
}

int check_socket_ready(int s, int for_write)
{
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};
  int res;

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  MZ_FD_ZERO(writefds);
  MZ_FD_SET(s, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET(s, exnfds);

  do {
    res = select(s + 1, 
		 for_write ? NULL : writefds, 
		 for_write ? writefds : NULL, 
		 exnfds, &time);
  } while((res == 0) && NOT_WINSOCK(errno == EINTR));

  return res;
}

void socket_add_fds(int s, void *fds, int for_write)
{
  void *fds1, *fds2;
  
  fds1 = MZ_GET_FDSET(fds, (for_write ? 1 : 0));
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(s, (fd_set *)fds1);
  MZ_FD_SET(s, (fd_set *)fds2);
}

int daemon_ready(Scheme_Object *ignored)
{
  struct sslplt *cur;

  for (cur = ssls; cur; cur = cur->next) {
    if (!cur->write_blocked_reason)
      return 1;
    else
      check_socket_ready(BIO_get_fd(SSL_get_wbio(cur->ssl), NULL),
			 (cur->write_blocked_reason == 2));
  }
}

void deamon_needs_wakeup(Scheme_Object *ignored, void *fds)
{
  struct sslplt *cur;

  for (cur = ssls; cur; cur = cur->next) {
    if (!cur->write_blocked_reason)
      scheme_cancel_sleep();
    else
      socket_add_fds(BIO_get_fd(SSL_get_wbio(cur->ssl), NULL),
		     fds,
		     (cur->write_blocked_reason == 2));
  }
}


/* write_close_thread: this is the thread that flushes out our buffers
   automatically and/or closes items which need closing */
Scheme_Object *write_close_thread(int argc, Scheme_Object *argv[])
{
  struct sslplt *cur, *prev;
  int empty;

  /* this thread should not terminate unless killed externally */
  while (1) {
    scheme_wait_sema(daemon_lock, 0);

    while (1) {
      cur = ssls; prev = NULL;
      while (cur) {
	int status, drop = 1;

	if (cur->ob_used) {
	  cur->write_blocked_reason = 0;
	  status = SSL_write(cur->ssl, &(cur->obuffer), 1);
	  if (status > 1) {
	    cur->ob_used = 0;
	  } else {
	    int err;
	    drop = 0;
	    err = SSL_get_error(cur->ssl, status);
	    if (err == SSL_ERROR_WANT_READ)
	      cur->write_blocked_reason = 1;
	    else if (err == SSL_ERROR_WANT_WRITE)
	      cur->write_blocked_reason = 2;
	    else {
	      /* Some error. We drop the char, and assume
		 that it's not a transient error, so the
		 next action will find the same error. */
	      drop = 1;
	    }
	  }
	} else if (cur->close_in && cur->close_out) {
	  /* Apparently a force close */
          SSL_free(cur->ssl);
        }
	/* there shouldn't be a 3rd possibility */
	
	if (drop) {
	  prev = cur; 
	} else{
	  if (prev)
	    prev->next = cur->next;
	  else
	    ssls = cur->next;
	}
	cur = cur->next;
      }
      empty = !ssls;
   
      if (empty)
	break;

      /* wait until something becomes unblocked, or something new is queued */
      scheme_block_until(daemon_ready, deamon_needs_wakeup, NULL, (float)0.0);
    }
  }
}

/*****************************************************************************
 * ERROR FUNCTION: 
 *****************************************************************************/

/* Copy error strings in case a thread swap happens
   between the time the string is obtained and
   the string is put into an error message. */
static const char *dup_errstr(const char *s) {
  char *t;
  long len;
  if (s){
    len = strlen(s);
    t = scheme_malloc_atomic(len);
    memcpy(t, s, len);
    return t;
  } else
    return s;
}

static int get_ssl_error_msg(int errid, const char **msg, int status, int has_status)
{
  if ((errid == SSL_ERROR_SYSCALL) && has_status) {
    if (status == 0) {
      *msg = "unexpected EOF";
    } else {
      *msg = NULL;
      errid = SOCK_ERRNO();
    }
  } else {
    const char *c;

    c = dup_errstr(ERR_reason_error_string(errid));
    if (c)
      *msg = c;
    else
      *msg = "Unknown error";
  }

  return errid;
}

/*****************************************************************************
 * INPORT PORT FUNCTIONS: This is the stuff that works on input ports. This  *
 * is a little complicated because we have to get char_ready to work on top  *
 * of a system that doesn't have such a function. So we buffer one character *
 * as necessary.                                                             *
 *****************************************************************************/

/* this is the new subtype we're creating */
Scheme_Object *ssl_input_port_type = NULL; 

/* forward decls: */
static void sslin_need_wakeup(Scheme_Input_Port *port, void *fds);
static int sslin_char_ready(Scheme_Input_Port *port);

/* ssl_get_string: read a sequence of bytes into a buffer given to us. This is 
   made severely annoying by the nonblocking nature of the socket stream and 
   the possibly blocking nature that mzscheme might want from us. */
long ssl_do_get_string(Scheme_Input_Port *port, char *buffer, long offset,
		       long size, int nonblocking, 
		       int *stuck_why, int err_ok) 
{
  const char *errstr = "Unknown error";
  int err = 0;
  long status = 0;
  long bytes_read = 0;
  struct sslplt *ssl = (struct sslplt *)SCHEME_INPORT_VAL(port);

  /* check the buffer */
  if(ssl->ib_used) {
    buffer[offset++] = ssl->ibuffer;
    bytes_read++; 
    ssl->ib_used = 0;
  }

  while (!bytes_read) {
    /* make sure people aren't being sneaky */
    if(ssl->close_in) {
      errstr = "read from closed port!";
      goto read_error;
    }
    
    /* re-check writes,if any are blocked, since we're touching the
       ssl channel */
    ssl->write_blocked_reason = 0;
  
    /* read the data. maybe. hopefully. please. */
    status = SSL_read(ssl->ssl, buffer+offset+bytes_read, size-bytes_read);

    if(status < 1) {
      /* see what kind of error this was */
      err = SSL_get_error(ssl->ssl, status);

      /* see if we've hit the end of file */
      if(err == SSL_ERROR_ZERO_RETURN) {
	/* do stuff */
	if(bytes_read == 0) 
	  return EOF;
	else
	  return bytes_read;
      } else if ((err != SSL_ERROR_WANT_READ) && (err != SSL_ERROR_WANT_WRITE)) {
        /* critical error */
	if (!err_ok) return 0;

	err = get_ssl_error_msg(err, &errstr, status, 1);
        goto read_error;
      }

      *stuck_why = ((err == SSL_ERROR_WANT_READ) ? 1 : 2);
    } else
      bytes_read += status;

    if (nonblocking)
      break;

    /* It might be tempting at this point to block on the fd
       forreading if SSL_ERROR_WANT_READ. That would be a bad
       idea, because another thread might be using the port,
       and might shift it into SSL_ERROR_WANT_WRITE mode.
       Use the general sll input blocking functions. */

    scheme_block_until((Scheme_Ready_Fun)sslin_char_ready, 
		       (Scheme_Needs_Wakeup_Fun)sslin_need_wakeup,
		       (void *)port, (float)0.0);
  }
  
  return bytes_read;

 read_error:
  scheme_raise_exn(MZEXN_I_O_PORT_READ, port, "ssl-read: error reading (%Z)",
		   err, errstr);
  return 0; /* needless, but it makes GCC happy */
}

long ssl_get_string(Scheme_Input_Port *port, char *buffer, long offset,
		    long size, int nonblocking) 
{
  int stuck_why;

  return ssl_do_get_string(port, buffer, offset, size, nonblocking, &stuck_why, 1);
}

/* sslin_char_ready: return 1 (true) iff a nonblocking call to get_string 
   can read at least one character (that is, it won't return 0). This 
   function is the cause of a bit of suffering, actually. */
static int sslin_do_char_ready(Scheme_Input_Port *port, int *stuck_why)
{
  struct sslplt *ssl = SCHEME_INPORT_VAL(port);
  char buf[1];
  
  *stuck_why = 0;

  if (ssl->close_in) return 1;

  /* see if the buffer has something in it, and if so, return true */
  if(ssl->ib_used) return 1;

  /* otherwise, try to read a character in */
  if (ssl_do_get_string(port, buf, 0, 1, 1, stuck_why, 0)) {
    ssl->ib_used = 1;
    ssl->ibuffer = ((unsigned char *)buf)[0];
    return 1;
  }

  if (!*stuck_why) {
    /* not-yet-reported error */
    return 1;
  }

  /* nothing buffered and we can't read, so the answer is no */
  return 0;
}

static int sslin_char_ready(Scheme_Input_Port *port)
{
  int stuck_why;

  return sslin_do_char_ready(port, &stuck_why);
}

/* sslin_close: close down a buffer, freeing the temporary structures we
   created. */
void sslin_close(Scheme_Input_Port *port)
{
  struct sslplt *ssl;
  ssl= (struct sslplt *)SCHEME_INPORT_VAL(port);

  ssl->close_in = 1;
  ssl->write_blocked_reason = 0;

  if (ssl->close_out)
    SSL_free(ssl->ssl);
}

/* sslin_need_wakeup: called when the input port is blocked to determine 
   what exactly it's blocked on. We have to try a read to find out
   why it's blocked. */
static void sslin_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  struct sslplt *ssl = SCHEME_INPORT_VAL(port);
  long rfd = BIO_get_fd(SSL_get_rbio(ssl->ssl), NULL);
  void *fds2;
  int stuck_why;

  if (sslin_do_char_ready(port, &stuck_why)) {
    /* Need wakeup now! */
    scheme_cancel_sleep();
  } else {
    if (stuck_why == 1) {
      /* waiting for read */
      fds2 = MZ_GET_FDSET(fds, 0);
      MZ_FD_SET(rfd, (fd_set *)fds2);
    } else {
      /* waiting for write */
      fds2 = MZ_GET_FDSET(fds, 1);
      MZ_FD_SET(rfd, (fd_set *)fds2);
    }
  }
}

/* make_sslin_port: called to create a scheme input port to return to the
   caller, eventually */
Scheme_Input_Port *make_sslin_port(SSL *ssl, struct sslplt *wrapper)
{
  return scheme_make_input_port(ssl_input_port_type, wrapper, ssl_get_string, 
				NULL, sslin_char_ready, sslin_close, 
				sslin_need_wakeup, 1);
}

Scheme_Input_Port *make_named_sslin_port(SSL *ssl, struct sslplt *d, char *n)
{
  Scheme_Input_Port *retval = make_sslin_port(ssl, d);
  retval->name = n;
  return retval;
}

/*****************************************************************************
 * OUTPUT PORT FUNCTIONS: This is the stuff that works on output ports. This *
 * is very complicated because we have to get char_ready to work on top of a *
 * system that doesn't have such a function. So we buffer one character on   *
 * output as necessary.                                                      *
 *****************************************************************************/

/* this is the new subtype we're creating */
Scheme_Object *ssl_output_port_type = NULL;

/* forward decls: */
static int sslout_char_ready(Scheme_Output_Port *port);
static void sslout_need_wakeup(Scheme_Output_Port *port, void *fds);

/* write_string: write some bits of data out to the wire, if possible. This
   is made complicated by a host of problems. */
long write_string(Scheme_Output_Port *port, const char *buffer, long offset, 
		  long size, int rarely_block) 
{
  struct sslplt *ssl = (struct sslplt *)SCHEME_OUTPORT_VAL(port);
  const char *errstr = "Unknown error";
  int err = 0;
  int status = 0;
  long wrote_bytes = 0;

  /* make sure people aren't trying to do something sneaky */
  if(ssl->close_out) {
    errstr = "write to closed port!"; 
    goto write_error;
  }

  /* this is all very complicated due to rarely_block being three valued */
  if (ssl->ob_used) {
    if (rarely_block == 2)
      return 0;
    /* Wait until it's writable */
    scheme_block_until((Scheme_Ready_Fun)sslout_char_ready, 
		       (Scheme_Needs_Wakeup_Fun)sslout_need_wakeup,
		       (void *)port, (float)0.0);      
  }

  /* We get here only where !ssl->ob_used. */

  /* Try to write a decent sized chunk: */
  status = SSL_write(ssl->ssl, buffer+offset+wrote_bytes, size-wrote_bytes);

  if (status > 0)
    return status; /* success */

  err = SSL_get_error(ssl->ssl, status);
  if((err != SSL_ERROR_WANT_READ) && (err != SSL_ERROR_WANT_WRITE)) {
    err = get_ssl_error_msg(err, &errstr, status, 1);
    goto write_error;
  }
  if (rarely_block == 2) 
    return 0;

  /* Can't write a decent-sized chunk. Put one char in the outgoing
     buffer, and block asnecessary until the char is flushed */
   {
    int was_empty;

    ssl->ob_used = 1;
    ssl->obuffer= buffer[offset];

    /* Put this SLL into the list of things that the deamon must
       process. */
    was_empty = !ssls;
    ssl->next = ssls;
    ssls = ssl;

    /* Wake up the daemon thread if the list usedto be empty: */
    if (was_empty)
      scheme_post_sema(daemon_lock);

    /* We "wrote" one byte. The daemon will ensure that the byte
       actually goes out. */

    return 1;
  }

 write_error:
  scheme_raise_exn(MZEXN_I_O_PORT_WRITE, port, "ssl-read: error writing (%Z)",
		   err, errstr);
  return 0; /* needless, but it makes GCC happy */
}

/* sslout_char_ready: return 1 (true) iff a nonblocking (version 1, not 
   version 2) call to write_string will write at least one character. */
static int sslout_char_ready(Scheme_Output_Port *port)
{
  struct sslplt *ssl = SCHEME_OUTPORT_VAL(port);

  return !ssl->ob_used;
}

/* sslout_close: close down a buffer, freeing the temporary structures we
   created. */
void sslout_close(Scheme_Output_Port *port)
{
  struct sslplt *ssl = (struct sslplt *)SCHEME_OUTPORT_VAL(port);
  int forced = 0;

  if (!ssl->ob_used)
    SSL_shutdown(ssl->ssl);
  else if (scheme_close_should_force_port_closed()) {
    /* Tell daemon to give up: */
    ssl->ob_used = 0;
    ssl->write_blocked_reason = 0;
    forced = 1;
  }

  ssl->close_out = 1;
  if (!forced && ssl->close_in) {
    SSL_free(ssl->ssl);
  }
}

/* sslout_need_wakeup: we don't do anything, because low-level
   blocking is handled by the daemon thread */
static void sslout_need_wakeup(Scheme_Output_Port *port, void *fds)
{
}

/* make_sslout_port: called to create a scheme output port to return to the
   caller, eventually. */
static Scheme_Output_Port *make_sslout_port(SSL *ssl, struct sslplt *data)
{
  return scheme_make_output_port(ssl_output_port_type, data, write_string, 
				 sslout_char_ready, sslout_close, 
				 sslout_need_wakeup, 1);
}

/*****************************************************************************
 * CLEANING AND NETWORK FUNCTIONS: These are the functions which convert the *
 * things we get in to things that are useful, plus the routines for doing   *
 * various network operations                                                *
 *****************************************************************************/

/* check_host_and_convert: Make absolutely sure the first argument was a 
   string, and then convert it into a character string we can actually use.
   Or scream bloody murder if it wasn't a string. */
char *check_host_and_convert(int argc, Scheme_Object *argv[])
{
  if(SCHEME_STRINGP(argv[0]) )
    return SCHEME_STR_VAL(argv[0]); 
  
  scheme_wrong_type("ssl-connect", "string", 0, argc, argv);
  return NULL; /* unnecessary, but it makes GCC happy */
}

/* check_port_and_convert: Make absolutely sure the second argument was a
   potential port number, and if it is, convert it to a network-order value
   we can actually use. Or scream if it wasn't kosher. */
unsigned short check_port_and_convert(int argc, Scheme_Object *argv[])
{
  if(SCHEME_INTP(argv[1]))
    if(SCHEME_INT_VAL(argv[1]) >= 1)
      if(SCHEME_INT_VAL(argv[1]) <= 65535)
	return htons(SCHEME_INT_VAL(argv[1]));
  scheme_wrong_type("ssl_connect", "exact integer in [1, 65535]", 1,argc,argv);
  return 0; /* unnessary and wrong, but it makes GCC happy */
}

/* check_encrypt_and_convert: Check the third argument is a valid symbol here,
   and convert it to the SSL method function we'll be using if they gave us
   a good argument. Otherwise scream. The third argument tells us if we want
   client or server method functions. */
SSL_METHOD *check_encrypt_and_convert(int argc, Scheme_Object *argv[], int c)
{
  char *sym_val;

  if(argc == 2)
    return (c ? SSLv23_client_method() : SSLv23_server_method());
    
  if(!SCHEME_SYMBOLP(argv[2]))
    scheme_wrong_type("ssl-connect", "one of 'sslv23, 'sslv2, 'sslv3, or 'tls",
		      2, argc, argv);

  sym_val = SCHEME_SYM_VAL(argv[2]);

  if(!strcmp(sym_val, "sslv2-or-v3")) {
    return (c ? SSLv23_client_method() : SSLv23_server_method());
  } else if(!strcmp(sym_val, "sslv2")) {
    return (c ? SSLv2_client_method() : SSLv2_server_method());
  } else if(!strcmp(sym_val, "sslv3")) {
    return (c ? SSLv3_client_method() : SSLv3_server_method());
  } else if(!strcmp(sym_val, "tls")) {
    return (c ? TLSv1_client_method() : TLSv1_server_method());
  } else scheme_wrong_type("ssl-connect", 
			   "one of 'sslv23, 'sslv2, 'sslv3, or 'tls", 
			   2, argc, argv);
  return NULL; /* unnecessary, but it makes GCC happy */
}

/* parse_numerical: parse out a numerical address. this is lifted in its 
   entirely from ${PLTHOME}/src/mzscheme/src/network.c */
static int parse_numerical(const char *address, unsigned long *addr)
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
    *addr = *(unsigned long *)n;
    return 1;
  }

  return 0;
}

/* get_host_by_number: get the hostent structure for the system by parsing
   in the given numbers. Largely stolen from 
   $(PLTHOME)/src/mzscheme/src/network.c */
struct hostent *get_host_by_number(const char *address)
{
  static unsigned long by_number_id;
  static unsigned long *by_number_array[2];
  static struct hostent by_number_host;

  if(parse_numerical(address, &by_number_id)) {
    by_number_array[0] = &by_number_id;
    by_number_host.h_addr_list = (char**)by_number_array;
    by_number_host.h_length = sizeof(long);
    return &by_number_host;
  }

  return NULL;
}

/* ssl_check_sock: determine if a socket is ready for reading or
   writing; conector_p is an array of integers: socket and 
   0=>read/1=>write. */
int ssl_check_sock(Scheme_Object *connector_p)
{
  return check_socket_ready(((int *)connector_p)[0], ((int *)connector_p)[1]);
}

/* ssl_check_sock: block on socket for reading or
   writing; conector_p is an array of integers: socket and 
   0=>read/1=>write. */
void ssl_sock_needs_wakeup(Scheme_Object *connector_p, void *fds)
{
  socket_add_fds(((int *)connector_p)[0], fds, ((int *)connector_p)[1]);
}

#ifdef USE_UNIX_SOCKETS_TCP

/* closesocket: close a socket, and try real hard to do it. This is lifted 
   entirely from ${PLTHOME}/src/mzscheme/src/network.c */
void closesocket(long s)
{
  int cr;
  do { cr = close(s); } while((cr == -1) && NOT_WINSOCK(errno == EINTR));
}

#endif

/* close_socket_and_dec: called when we're broken out of our attempt to
   connect a socket */
void close_socket_and_dec(unsigned short sock)
{
  closesocket(sock);
}

/*****************************************************************************
 * SOCKET->SSL connection completion                                         *
 *****************************************************************************/

static Scheme_Object *finish_ssl(const char *name, int sock, SSL_METHOD *meth,
				 char *address, int port)
{
  SSL_CTX *ctx = NULL;
  BIO *bio = NULL;
  SSL *ssl = NULL;
  struct sslplt *sslplt = NULL;
  const char *errstr = "Unknown error";
  Scheme_Object *retval[2]; 
  int status;
  int err = 0;
  int *sptr = NULL;

  /* set up the BIO pipe */
  bio = BIO_new_socket(sock, BIO_CLOSE);
  if(!bio) { errstr = "couldn't create BIO stream"; goto clean_up_and_die; }

  /* set up the SSL context object */
  ctx = SSL_CTX_new(meth);
  if(!ctx) { 
    err = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
    goto clean_up_and_die; 
  }

  /* set up the full SSL object */
  ssl = SSL_new(ctx);
  if(!ssl) {
    err = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
    goto clean_up_and_die; 
  }
  SSL_set_bio(ssl, bio, bio);

  /* see if we can connect via SSL */
  status = SSL_connect(ssl);
  while(status < 1) {
    status = SSL_get_error(ssl, status);
    if ((status == SSL_ERROR_WANT_READ) 
	|| (status == SSL_ERROR_WANT_WRITE)) {
      if (!sptr) {
	sptr = (int *)scheme_malloc_atomic(2 * sizeof(int));
	sptr[0] = sock;
      }
      sptr[1] = (status == SSL_ERROR_WANT_WRITE);

      BEGIN_ESCAPEABLE(close_socket_and_dec, sock);
      scheme_block_until(ssl_check_sock, ssl_sock_needs_wakeup, 
			 (void *)sptr, (float)0.0);
      END_ESCAPEABLE();
    } else {
      err = get_ssl_error_msg(ERR_get_error(), &errstr, 0, 0);
      goto clean_up_and_die;
    }
    status = SSL_connect(ssl);
  }
  scheme_making_progress();

  sslplt = create_register_sslplt(ssl);
  retval[0] = (Scheme_Object*)make_named_sslin_port(ssl, sslplt, address);
  retval[1] = (Scheme_Object*)make_sslout_port(ssl, sslplt);
  return scheme_values(2, retval);

 clean_up_and_die:
  if(sock) closesocket(sock);
  if(ctx) SSL_CTX_free(ctx);
  if(ssl) SSL_free(ssl);
  scheme_raise_exn(MZEXN_I_O_TCP, 
		   "%s: connection to %s, port %d failed (%Z)",
		   name,
		   address, port, err, errstr);
  
  /* not strictly necessary, but it makes our C compiler happy */
  return NULL;
}

/*****************************************************************************
 * SCHEME EXTERNAL FUNCTION IMPLEMENTATIONS: These are the implemenations of *
 * the functions which are actually going to be exported to MzScheme userland*
 *****************************************************************************/

static Scheme_Object *ssl_connect(int argc, Scheme_Object *argv[])
{
  char *address = check_host_and_convert(argc, argv);
  unsigned short nport = check_port_and_convert(argc, argv);
  unsigned long port = SCHEME_INT_VAL(argv[1]);
  SSL_METHOD *meth = check_encrypt_and_convert(argc, argv, 1);
  struct hostent *host = NULL;
  int status;
  const char *errstr = "Unknown error";
  int err = 0;
  struct sockaddr_in addr;
  int sock;
#ifndef PROTOENT_IS_INT
  struct protoent *proto;
#endif

  /* check we have the security clearance to actually do this */
  scheme_security_check_network("ssl-connect", address, port, 1);

  /* try to create the socket */
#ifndef PROTOENT_IS_INT
  proto = getprotobyname("tcp");
  if(!proto) { 
    errstr = "couldn't find tcp protocol id"; goto clean_up_and_die; 
  }
#endif
  sock = socket(PF_INET, SOCK_STREAM, PROTO_P_PROTO);
  if (sock == INVALID_SOCKET)  { errstr = NULL; err = errno; goto clean_up_and_die; }
#ifdef USE_WINSOCK_TCP
  {
    unsigned long ioarg = 1;
    ioctlsocket(sock, FIONBIO, &ioarg);
  }
#else
  fcntl(sock, F_SETFL, MZ_NONBLOCKING);
#endif
  
  /* lookup hostname and get a reasonable structure */
  host = get_host_by_number(address);
  if(!host) host = gethostbyname(address);
  if(!host) { err = h_errno; errstr = mz_hstrerror(err); goto clean_up_and_die; }
    
  /* make the network connection */
  addr.sin_family = AF_INET;
  addr.sin_port = nport;
  memset(&addr.sin_addr, 0, sizeof(addr.sin_addr));
  memset(&addr.sin_zero, 0, sizeof(addr.sin_zero));
  memcpy(&addr.sin_addr, host->h_addr_list[0], host->h_length);
  status = connect(sock, (struct sockaddr *)&addr, sizeof(addr));
  /* here's the complicated bit */
  if(status == -1) {
    if(!WAS_EINPROGRESS(errno)) { 
      errstr = NULL; err= errno; goto clean_up_and_die; 
    }
    
    {
      int *sptr;

      sptr = (int *)scheme_malloc_atomic(2 * sizeof(int));
      sptr[1] = sock;
      sptr[0] = 1;

      BEGIN_ESCAPEABLE(close_socket_and_dec, sock);
      scheme_block_until(ssl_check_sock, ssl_sock_needs_wakeup, 
			 (void *)sptr, (float)0.0);
      END_ESCAPEABLE();
    }

    /* see if the connection succeeded, or die if it didn't */
    {
      int so_len = sizeof(status);
      if(getsockopt(sock, SOL_SOCKET,SO_ERROR, (void*)&status, &so_len) != 0) {
	errstr = NULL;
	err = status; 
	goto clean_up_and_die;
      }
    }
  }

  return finish_ssl("ssl-connect", sock, meth, address, port);

 clean_up_and_die:
  if (sock != INVALID_SOCKET) closesocket(sock);
  scheme_raise_exn(MZEXN_I_O_TCP, 
		   "ssl-connect: connection to %s, port %d failed (%Z)",
		   SCHEME_STR_VAL(argv[0]), SCHEME_INT_VAL(argv[1]), 
		   err, errstr);
  
  /* not strictly necessary, but it makes our C compiler happy */
  return NULL;
}

static Scheme_Object *ssl_connect_break(int argc, Scheme_Object *argv[]) {
  return scheme_call_enable_break(ssl_connect, argc, argv);
}

/*****************************************************************************
 * REGISTRATION FUNCTIONS: The functions that register the above externals so*
 * everybody else can use them.
 *****************************************************************************/

/* scheme_initialize: called when the extension is first loaded */
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  Scheme_Object *thread = scheme_make_prim_w_arity(write_close_thread,
                                                   "SSL Flushing Thread",
                                                   0, 0);
  Scheme_Custodian *newcust = scheme_make_custodian(NULL);
  
  SSL_library_init();
  daemon_lock = scheme_make_sema(0);
  ssl_input_port_type = scheme_make_port_type("<ssl-input-port>");
  ssl_output_port_type = scheme_make_port_type("<ssl-output-port>");
  scheme_register_extension_global(&daemon_lock, 4);
  scheme_register_extension_global(&ssls, 4);
  scheme_register_extension_global(&ssl_input_port_type, 4);
  scheme_register_extension_global(&ssl_output_port_type, 4);
  
  scheme_thread_w_custodian(thread, scheme_config, newcust);
  return scheme_reload(env);
}

/* scheme_reload: called when an extension is loaded a second+ time */
Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Object *v;

  v = scheme_intern_symbol("mzssl");
  env = scheme_primitive_module(v, env);
  
  /* add ssl-connect */
  v = scheme_make_prim_w_arity(ssl_connect, "ssl-connect", 2, 3);
  scheme_add_global("ssl-connect", v, env);
  v = scheme_make_prim_w_arity(ssl_connect_break,"ssl-connect/enable-break",2,3);
  scheme_add_global("ssl-connect/enable-break", v, env);
  scheme_add_global("ssl-available?", scheme_true, env);
  scheme_finish_primitive_module(env);

  return scheme_void;
}

/* scheme_module_name: called to get the name of this module */
Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("mzssl");
}
