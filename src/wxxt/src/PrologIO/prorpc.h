/*								-*- C++ -*-
 * File:		prorpc.h
 * Purpose:	RPC implementation on top of PROLOGIO and DDE
 * Author:		Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef pro_rpch
#define pro_rpch

#ifndef wx_xt

    // wxWindows standard include mechanism
    /* sccsid[] = "%W% %G%" */
#   include <wx_ipc.h>
#   include "read.h"

#else // wx_xt

    // The Xt port uses another include mechanism
#   ifdef __GNUG__
#   pragma interface
#   endif

#endif // #ifndef wx_xt

class rpcCallTable;
class rpcConnection: public wxConnection
{
  DECLARE_CLASS(rpcConnection)
 public:
  PrologExpr *temp_expr;
  char *ReturnValue;
  rpcCallTable *call_table;

  rpcConnection(char *buffer, int size);
  rpcConnection(void);
  ~rpcConnection(void);

  // Callbacks to SERVER
  Bool OnExecute(char *topic, char *data, int size, int format);
  char *OnRequest(char *topic, char *item, int *size, int format);
  Bool OnPoke(char *topic, char *item, char *data, int size, int format);
  Bool OnStartAdvise(char *topic, char *item);
  Bool OnEndAdvise(char *topic, char *item);

  // Callbacks to CLIENT
  Bool OnAdvise(char *topic, char *item, char *data, int size, int format);

  // Deletes expr having called it, will delete return value
  // on its own so don't do it yourself! If you want to save
  // it from one call to the next, make a copy with the PrologExpr Copy member.
  virtual PrologExpr *Call(PrologExpr *expr);


  // User-overridable code here (can override above but may not work!)
  // NEW members to override for RPC in server
  virtual PrologExpr *OnCall(PrologExpr *expr);

  // Implementing a way of registering function calls with type checking...
  virtual void RegisterCallTable(rpcCallTable *table);
};


class rpcServer: public wxServer
{
  DECLARE_CLASS(rpcServer)
 public:
  rpcServer::rpcServer(void);
  wxConnection *OnAcceptConnection(char *topic);
};

class rpcClient: public wxClient
{
  DECLARE_CLASS(rpcClient)
 public:
  rpcClient::rpcClient(void);
  wxConnection *OnMakeConnection(void);
};

/*
 * Implementation of a table of calls and type info, to make it easy
 * to implement a command language
 */

#define rpcERROR_NODEF      1
#define rpcERROR_BADARGTYPE 2
#define rpcERROR_TOOFEWARGS 3
#define rpcERROR_ILLFORMED  4

typedef PrologExpr * (*rpcCall) (wxConnection *connection, PrologExpr *clause);

class rpcCallTableEntry: public wxObject
{
  DECLARE_CLASS(rpcCallTableEntry)
 public:
  char *functor;
  rpcCall fun;
  wxList *types;
  rpcCallTableEntry(char *the_functor, rpcCall the_fun, wxList *types);
  ~rpcCallTableEntry(void);
};

class rpcCallTable: public wxList
{
  DECLARE_CLASS(rpcCallTable)
 public:
  rpcCallTable(void);
  ~rpcCallTable(void);

  // Give functor, function to call, types
  void AddCall(char *functor, rpcCall the_fun, ...);
  PrologExpr *FindAndExecuteCall(wxConnection *connection, PrologExpr *clause);
};

#endif // pro_rpch
