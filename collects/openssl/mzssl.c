/* ssl.c: an extension to PLT MzScheme to allow SSL connections */
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include "escheme.h"

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

/* stolen from $(PLTHOME)/src/mzscheme/src/schpriv.h */
typedef void (*Scheme_Kill_Action_Func)(void *);
void scheme_push_kill_action(Scheme_Kill_Action_Func f, void *d);
void scheme_pop_kill_action();
# define BEGIN_ESCAPEABLE(func, data) \
    { mz_jmp_buf savebuf; \
      scheme_push_kill_action((Scheme_Kill_Action_Func)func, (void *)data); \
      memcpy(&savebuf, &scheme_error_buf, sizeof(mz_jmp_buf)); \
      if (scheme_setjmp(scheme_error_buf)) { \
        func(data); \
        scheme_longjmp(savebuf, 1); \
      } else {
# define END_ESCAPEABLE() \
      scheme_pop_kill_action(); \
      memcpy(&scheme_error_buf, &savebuf, sizeof(mz_jmp_buf)); } }

struct sslplt {
  SSL *ssl;
  char ibuffer, obuffer;
  char ib_used, ob_used;
  Scheme_Object *lock;
  char close_in, close_out;
  struct sslplt *next;
};

/*****************************************************************************
 * TOP-LEVEL THREAD: This is the routine and data involved with running the  *
 * top level thread (the one that helps us fake a couple guarantees).        *
 *****************************************************************************/
Scheme_Object *list_lock = NULL;
struct sslplt *ssls = NULL;

/* create_ register_sslplt: called when a new sslplt structure needs to be 
   created and registered with out flusing/closing thread. */
struct sslplt *create_register_sslplt(SSL *ssl)
{
  struct sslplt *sslplt = scheme_malloc(sizeof(struct sslplt));

  sslplt->ssl = ssl;
  sslplt->ib_used = 0; sslplt->ob_used = 0; 
  sslplt->close_in = 0; sslplt->close_out = 0;
  sslplt->lock = scheme_make_sema(1);
  scheme_wait_sema(list_lock, 0);
  sslplt->next = ssls;
  ssls = sslplt;
  scheme_post_sema(list_lock);
  return sslplt;
}

/* write_close_thread: this is the thread that flushes out our buffers
   automatically and/or closes items which need closing */
Scheme_Object *write_close_thread(int argc, Scheme_Object *argv[])
{
  struct sslplt *cur, *prev;
  int did_something = 0;

  /* this thread should not terminate unless killed externally */
  while(1) {
    scheme_wait_sema(list_lock, 0);
    cur = ssls; prev = NULL; did_something = 0;
    while(cur) {
      if(scheme_wait_sema(cur->lock, 1)) {
        if(cur->ob_used) {
          int status = SSL_write(cur->ssl, &(cur->obuffer), 1);
          if(status > 1) cur->ob_used = 0;
          scheme_post_sema(cur->lock);
          prev = cur; cur = cur->next;
          did_something = 1;
        } else if(cur->close_in && cur->close_out) {
          struct sslplt *killed = cur;
          cur = cur->next;
          SSL_free(killed->ssl);
          did_something = 1;
          /* no need to post anything, since this is dead */
        } else {
          scheme_post_sema(cur->lock);
	  prev = cur; cur = cur->next;
        }
      } else { prev = cur; cur = cur->next; }
    }
    scheme_post_sema(list_lock);
    if(did_something) scheme_making_progress();
    scheme_thread_block((float)0.0);
  }
}

/*****************************************************************************
 * INPORT PORT FUNCTIONS: This is the stuff that works on input ports. This  *
 * is a little complicated because we have to get char_ready to work on top  *
 * of a system that doesn't have such a function. So we buffer one character *
 * as necessary.                                                             *
 *****************************************************************************/

/* this is the new subtype we're creating */
Scheme_Object *ssl_input_port_type = NULL; 

/* ssl_get_string: read a sequence of bytes into a buffer given to us. This is 
   made severely annoying by the nonblocking nature of the socket stream and 
   the possibly blocking nature that mzscheme might want from us. */
long ssl_get_string(Scheme_Input_Port *port, char *buffer, long offset,
		    long size, int nonblocking) 
{
  const char *errstr = "Unknown error";
  long status = 0;
  long bytes_read = 0;
  struct sslplt *ssl = (struct sslplt *)SCHEME_INPORT_VAL(port);

  /* make sure people aren't being sneaky */
  if(ssl->close_in) {
    errstr = "read from closed port!"; goto read_error;
  }
  
  /* check the buffer */
  if(ssl->ib_used) {
    buffer[offset++] = ssl->ibuffer;
    bytes_read++; 
  }

  while(bytes_read != size) {
    /* read the data. maybe. hopefully. please. */
    status = SSL_read(ssl->ssl, buffer+offset+bytes_read, size-bytes_read);

    if(status < 1) {
      /* see what kind of error this was */
      int err = SSL_get_error(ssl->ssl, status);

      /* see if we've hit the end of file */
      if(err == SSL_ERROR_ZERO_RETURN) {
	/* do stuff */
	if(bytes_read == 0) 
	  return EOF;
	else
	  return bytes_read;
      } else if((err != SSL_ERROR_WANT_READ) && (err != SSL_ERROR_WANT_WRITE)) {
        /* critical error */
	const char *temperr = ERR_reason_error_string(err);
	errstr = temperr ? temperr : errstr;
        goto read_error;
      }
    } else bytes_read += status;

    if(nonblocking) break;

    scheme_making_progress();
    scheme_thread_block((float)0.0);
  }
  
  return bytes_read;

 read_error:
  scheme_raise_exn(MZEXN_I_O_PORT_READ, port, "ssl-read: error reading (%s)",
		   errstr);
  return 0; /* needless, but it makes GCC happy */
}

/* sslin_char_ready: return 1 (true) iff a nonblocking call to get_string 
   can read at least one character (that is, it won't return 0). This 
   function is the cause of a bit of suffering, actually. */
int sslin_char_ready(Scheme_Input_Port *port)
{
  struct sslplt *ssl = SCHEME_INPORT_VAL(port);
  int status;

  /* see if the buffer has something in it, and if so, return true */
  if(ssl->ib_used) return 1;

  /* otherwise, try to read a character in */
  status = SSL_read(ssl->ssl, &ssl->ibuffer, 1);
  if(status == 1) ssl->ib_used = 1; 

  /* nothing buffered and we can't read, so the answer is no */
  return ssl->ib_used;
}

/* sslin_close: close down a buffer, freeing the temporary structures we
   created. */
void sslin_close(Scheme_Input_Port *port)
{
  ((struct sslplt *)SCHEME_INPORT_VAL(port))->close_in = 1;
}

/* sslin_need_wakeup: called when the input port is blocked to determine 
   what exactly it's blocked on. */
void sslin_need_wakeup(Scheme_Input_Port *port, void *fds)
{
  struct sslplt *ssl = SCHEME_INPORT_VAL(port);
  long wfd = BIO_get_fd(SSL_get_wbio(ssl->ssl), NULL);
  long rfd = BIO_get_fd(SSL_get_rbio(ssl->ssl), NULL);
  void *fds2;
  
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(wfd, (fd_set *)fds2);
  MZ_FD_SET(rfd, (fd_set *)fds2);
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

/* write_string: write some bits of data out to the wire, if possible. This
   is made complicated by a host of problems. */
long write_string(Scheme_Output_Port *port, const char *buffer, long offset, 
		  long size, int rarely_block) 
{
  struct sslplt *ssl = (struct sslplt *)SCHEME_OUTPORT_VAL(port);
  const char *errstr = "Unknown error";
  int status = 0;
  long wrote_bytes = 0;

  /* make sure people aren't trying to do something sneaky */
  if(ssl->close_out) {
    errstr = "write to closed port!"; goto write_error;
  }

  /* this is all very complicated due to rarely_block being three valued */
  if(scheme_wait_sema(ssl->lock, rarely_block == 2)) {
    while(ssl->ob_used) {
      status = SSL_write(ssl->ssl, &(ssl->obuffer), 1);
      if(status < 1) {
        int err = SSL_get_error(ssl->ssl, status);
        if((err != SSL_ERROR_WANT_READ) && (err != SSL_ERROR_WANT_WRITE)) {
          const char *temperr = ERR_reason_error_string(err);
          scheme_post_sema(ssl->lock);
	  errstr = temperr ? temperr : errstr;
	  goto write_error;
        }
        if(rarely_block == 2) break;
        scheme_thread_block((float)0.0);
      } else ssl->ob_used = 0;
    }
    scheme_post_sema(ssl->lock);
    if(ssl->ob_used) return 0;
  } else return 0;

  while(wrote_bytes != size) {
    status = SSL_write(ssl->ssl, buffer+offset+wrote_bytes, size-wrote_bytes);
    if(status < 1) {
      int err = SSL_get_error(ssl->ssl, status);
      if((status != SSL_ERROR_WANT_READ) && (status != SSL_ERROR_WANT_WRITE)) {
        const char *temperr = ERR_reason_error_string(status);
	errstr = temperr ? temperr : errstr;
	goto write_error;
      }
    } else wrote_bytes += status;

    if(rarely_block) break;

    scheme_making_progress();
    scheme_thread_block((float)0.0);
  }

  return wrote_bytes;
  
 write_error:
  scheme_raise_exn(MZEXN_I_O_PORT_WRITE, port, "ssl-read: error writing (%s)",
		   errstr);
  return 0; /* needless, but it makes GCC happy */
}

/* sslout_char_ready: return 1 (true) iff a nonblocking (version 1, not 
   version 2) call to write_string will write at least one character. */
int sslout_char_ready(Scheme_Output_Port *port)
{
  struct sslplt *ssl = SCHEME_OUTPORT_VAL(port);
  int val = 1;

  if(scheme_wait_sema(ssl->lock, 1)) {
    val = ssl->ob_used;
    scheme_post_sema(ssl->lock);
  } 

  return !val;
}

/* sslout_close: close down a buffer, freeing the temporary structures we
   created. */
void sslout_close(Scheme_Output_Port *port)
{
  struct sslplt *ssl = (struct sslplt *)SCHEME_OUTPORT_VAL(port);
  
  SSL_shutdown(ssl->ssl);
  ssl->close_out = 1;
}

/* sslout_need_wakeup: called when the output port is blocked to determine
   what exactly it's blocked on. */
void sslout_need_wakeup(Scheme_Output_Port *port, void *fds)
{
  struct sslplt *ssl = SCHEME_OUTPORT_VAL(port);
  long wfd = BIO_get_fd(SSL_get_wbio(ssl->ssl), NULL);
  long rfd = BIO_get_fd(SSL_get_rbio(ssl->ssl), NULL);
  void *fds2;
  
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET(wfd, (fd_set *)fds2);
  MZ_FD_SET(rfd, (fd_set *)fds2);
}

/* make_sslout_port: called to create a scheme output port to return to the
   caller, eventually. */
Scheme_Output_Port *make_sslout_port(SSL *ssl, struct sslplt *data)
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

  if(!strcasecmp(sym_val, "sslv2-or-v3")) {
    return (c ? SSLv23_client_method() : SSLv23_server_method());
  } else if(!strcasecmp(sym_val, "sslv2")) {
    return (c ? SSLv2_client_method() : SSLv2_server_method());
  } else if(!strcasecmp(sym_val, "sslv3")) {
    return (c ? SSLv3_client_method() : SSLv3_server_method());
  } else if(!strcasecmp(sym_val, "tls")) {
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

/* ssl_check_connect: determine if a connection's been established, and if
   so, return 1. Stolen slmost entirely from tcp_check_connect in 
   ${PLTHOME}/src/mzscheme/src/network.c */
int ssl_check_connect(Scheme_Object *connector)
{
  DECL_FDSET(writefds, 1);
  DECL_FDSET(exnfds, 1);
  struct timeval time = {0, 0};
  int res;

  INIT_DECL_FDSET(writefds, 1);
  INIT_DECL_FDSET(exnfds, 1);

  MZ_FD_ZERO(writefds);
  MZ_FD_SET((int)connector, writefds);
  MZ_FD_ZERO(exnfds);
  MZ_FD_SET((int)connector, exnfds);

  do {
    res = select((int)connector + 1, NULL, writefds, exnfds, &time);
  } while((res == 0) && (errno == EINTR));

  return res;
}

/* ssl_connect_needs_wakeup: set the file descriptors we're blocking on
   for a connection. stolen entirely from tcp_connect_needs_wakeup in
   ${PLTHOME}/src/mzscheme/src/network.c */
void ssl_connect_needs_wakeup(Scheme_Object *connector, void *fds)
{
  void *fds1, *fds2;
  
  fds1 = MZ_GET_FDSET(fds, 1);
  fds2 = MZ_GET_FDSET(fds, 2);
  MZ_FD_SET((int)connector, (fd_set *)fds1);
  MZ_FD_SET((int)connector, (fd_set *)fds2);
}

/* closesocket: close a socket, and try real hard to do it. This is lifted 
   entirely from ${PLTHOME}/src/mzscheme/src/network.c */
void closesocket(long s)
{
  int cr;
  do { cr = close(s); } while((cr == -1) && (errno == EINTR));
}

/* close_socket_and_dec: called when we're broken out of our attempt to
   connect a socket */
void close_socket_and_dec(unsigned short sock)
{
  closesocket(sock);
/*   --scheme_file_open_count; */
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
  SSL_CTX *ctx = NULL;
  BIO *bio = NULL;
  SSL *ssl = NULL;
  struct hostent *host = NULL;
  struct sslplt *sslplt = NULL;
  Scheme_Object *retval[2]; 
  int status;
  const char *errstr = "Unknown error";
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
  if(sock == -1)  { errstr = strerror(errno); goto clean_up_and_die; }
  fcntl(sock, F_SETFL, MZ_NONBLOCKING);
  
  /* lookup hostname and get a reasonable structure */
  host = get_host_by_number(address);
  if(!host) host = gethostbyname(address);
  if(!host) { errstr = hstrerror(h_errno); goto clean_up_and_die; }
    
  /* make the network connection */
  addr.sin_family = AF_INET;
  addr.sin_port = nport;
  memset(&addr.sin_addr, 0, sizeof(addr.sin_addr));
  memset(&addr.sin_zero, 0, sizeof(addr.sin_zero));
  memcpy(&addr.sin_addr, host->h_addr_list[0], host->h_length);
  status = connect(sock, (struct sockaddr *)&addr, sizeof(addr));
  /* here's the complicated bit */
  if(status == -1) {
    if(errno != EINPROGRESS) { 
      errstr = strerror(errno); goto clean_up_and_die; 
    }
    
/*     scheme_file_open_count++; */
    BEGIN_ESCAPEABLE(close_socket_and_dec, sock);
    scheme_block_until(ssl_check_connect, ssl_connect_needs_wakeup, 
		       (void *)sock, (float)0.0);
    END_ESCAPEABLE();

    /* see if the connection succeeded, or die if it didn't */
    {
      int so_len = sizeof(status);
      if(getsockopt(sock, SOL_SOCKET,SO_ERROR, (void*)&status, &so_len) != 0) {
/* 	scheme_file_open_count--; */ errstr = strerror(status); 
	goto clean_up_and_die;
      }
    }
  }

  /* set up the BIO pipe */
  bio = BIO_new_socket(sock, BIO_CLOSE);
  if(!bio) { errstr = "couldn't create BIO stream"; goto clean_up_and_die; }

  /* set up the SSL context object */
  ctx = SSL_CTX_new(meth);
  if(!ctx) { 
    const char *temperr = ERR_reason_error_string(ERR_get_error()); 
    errstr = temperr ? temperr : "Error setting up SSL context";
    goto clean_up_and_die; 
  }

  /* set up the full SSL object */
  ssl = SSL_new(ctx);
  if(!ssl) {
    const char *temperr = ERR_reason_error_string(ERR_get_error()); 
    errstr = temperr ? temperr : "Error in creation of SSL structure";
    goto clean_up_and_die; 
  }
  SSL_set_bio(ssl, bio, bio);

  /* see if we can connect via SSL */
  status = SSL_connect(ssl);
  while(status < 1) {
    status = SSL_get_error(ssl, status);
    if((status != SSL_ERROR_WANT_READ) && (status != SSL_ERROR_WANT_WRITE)) {
      const char *temp = ERR_reason_error_string(ERR_get_error());
      if(temp) errstr = temp;
      goto clean_up_and_die;
    }
    scheme_thread_block((float)0.0);
    status = SSL_connect(ssl);
  }
  scheme_making_progress();

  sslplt = create_register_sslplt(ssl);
  retval[0] = (Scheme_Object*)make_named_sslin_port(ssl, sslplt, address);
  retval[1] = (Scheme_Object*)make_sslout_port(ssl, sslplt);
  return scheme_values(2, retval);

 clean_up_and_die:
  if(sock) closesocket(sock);
  if(host) free(host);
  if(ctx) SSL_CTX_free(ctx);
  if(ssl) SSL_free(ssl);
  scheme_raise_exn(MZEXN_I_O_TCP, 
		   "ssl-connect: connection to %s, port %d failed: %s",
		   SCHEME_STR_VAL(argv[0]), SCHEME_INT_VAL(argv[1]), errstr);
  
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
  list_lock = scheme_make_sema(1);
  ssl_input_port_type = scheme_make_port_type("<ssl-input-port>");
  ssl_output_port_type = scheme_make_port_type("<ssl-output-port>");
  scheme_register_extension_global(&list_lock, 4);
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
