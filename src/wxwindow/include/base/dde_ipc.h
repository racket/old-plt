/*
 * File: dde_ipc.h
 * Purpose: dde interface.
 */

#ifndef	DDE_IPC_H_
#define	DDE_IPC_H_

#if	defined(__cplusplus)
extern "C" {
#endif

# ifndef	P_
#	if	defined(__STDC__) || defined(__cplusplus)
#	define	P_(args)	args
# else
#	define	P_(args)	()
#	endif
# endif

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#define Bool int
#endif

#define	ASYNC_SELECT_MESSAGE	18465

  /* DDE messages */
  typedef enum
    {
      msgCONNECT = 1,
      msgEXECUTE,
      msgREQUEST,
      msgPOKE,
      msgADVISE_START,
      msgADVISE_STOP,
      msgADVISE,
      msgDISCONNECT,

      msgSUCCEED,
      msgFAIL
    }
  DDEmsg;

  /* DDE handle */
  typedef struct ddesock DDESock;

  /* DDE Callback handle */
  typedef struct ddecall DDECall;

  /* DDE Message callback */
  typedef void (OnMessage)
    (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt, void *closure);

  /* DDE Application context */
  typedef struct ddeapp DDEApp;

  typedef enum
    {
      appSELECT,
      appXTOOLKIT,
      appXVIEW,
      appWINDOWS
    }
  DDEtype;

  extern DDEApp *ddeApplication (DDEtype type,...);
  extern int ddeSelect (DDEApp * app);

  /* DDE messages */
  extern DDESock *ddeClient (DDEApp * app, char *host, char *server, char *topic, int local);
  extern DDESock *ddeServer (DDEApp * app, char *server, int local);
  extern int ddeExecute (DDESock * s, void *data, int size, int fmt);
  extern void *ddeRequest (DDESock * s, char *item, int *size, int *fmt);
  extern int ddePoke (DDESock * s, char *item, void *data, int size, int fmt);
  extern int ddeAdviseStart (DDESock * s, char *item);
  extern int ddeAdviseStop (DDESock * s, char *item);
  extern int ddeAdvise (DDESock * s, char *item, void *data, int size, int fmt);
  extern void ddeDelete (DDESock * s);
  extern int ddePut (DDESock * s, DDEmsg type, char *item, void *data, int size, int fmt);
  extern DDECall *ddeAddcallback (DDESock * s, OnMessage * callback, void *closure);
  extern int ddeRemcallback (DDESock * s, DDECall * call);
  extern void ddeResetcallback (DDESock * s);

#define	ddeOptCallback(call,op)		((call)->enabled=(op))
#define	ddeSucceed(s)			ddePut(s,msgSUCCEED,NULL,NULL,0,0)
#define	ddeFail(s)			ddePut(s,msgFAIL,NULL,NULL,0,0)

#ifndef wx_msw
# define SOCKET int
#endif
  /* Socket only */
  extern SOCKET ddefileno (DDESock * s);
  extern DDESock *fdSock (DDEApp * app, SOCKET fd, int listen);

#if	defined(__cplusplus)
}
#endif

#endif
