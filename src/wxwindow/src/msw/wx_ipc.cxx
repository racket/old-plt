/*
 * File:	wx_ipc.cc
 * Purpose:	Interprocess communication implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wx_ipc.cc,v 1.1 1994/08/14 21:59:17 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wx_ipc.cc	1.2 5/9/94"; */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_setup.h"

#endif

#if USE_IPC
#include "wx_frame.h"
#include "wx_utils.h"
#include "wx_ipc.h"
#include "wx_main.h"

#include <ddeml.h>

#ifdef WIN32
#define _EXPORT /**/
#else
#define _EXPORT _export
#endif

wxConnection *wxFindConnection(HCONV hConv);
void wxDeleteConnection(HCONV hConv);
wxServer *wxFindServer(char *s);

extern "C" HDDEDATA EXPENTRY _EXPORT wxDdeCallback(
WORD wType,
WORD wFmt,
HCONV hConv,
HSZ hsz1,
HSZ hsz2,
HDDEDATA hData,
DWORD lData1,
DWORD lData2);

// Add topic name to atom table before using in conversations
void wxAddAtom(char *string);
HSZ wxGetAtom(char *string);
void wxPrintDDEError(void);

#include <ddeml.h>
DWORD wxIdInst = 0L;
wxConnection *wxCurrentlyConnecting = NULL;

wxList wxAtomTable(wxKEY_STRING);
wxList wxIPCObjects;

extern char *wxDefaultIPCBuffer;
extern int wxDefaultIPCBufferSize;

/*
 * Initialization
 *
 */

extern Bool wxIPCInitialized;

void wxIPCInitialize(void)
{
  if (wxIPCInitialized)
    return;
  wxIPCInitialized = TRUE;

  // Should insert filter flags
  DdeInitialize(&wxIdInst, (PFNCALLBACK)MakeProcInstance(
               (FARPROC)wxDdeCallback, wxTheApp->hInstance),
               APPCLASS_STANDARD,
               0L);
}

IMPLEMENT_ABSTRACT_CLASS(wxIPCObject, wxObject)

wxIPCObject::wxIPCObject(void)
{
  service_name = NULL;
  wxIPCObjects.Append(this);
}

wxIPCObject::~wxIPCObject(void)
{
  wxIPCObjects.DeleteObject(this);
}

// Global find connection
wxConnection *wxFindConnection(HCONV hConv)
{
  wxNode *node = wxIPCObjects.First();
  wxConnection *found = NULL;
  while (node && !found)
  {
    wxIPCObject *object = (wxIPCObject *)node->Data();
    found = object->FindConnection(hConv);
    node = node->Next();
  }
  return found;
}

// Global delete connection
void wxDeleteConnection(HCONV hConv)
{
  wxNode *node = wxIPCObjects.First();
  Bool found = FALSE;
  while (node && !found)
  {
    wxIPCObject *object = (wxIPCObject *)node->Data();
    found = object->DeleteConnection(hConv);
    node = node->Next();
  }
}

wxConnection *wxIPCObject::FindConnection(HCONV conv)
{
  wxNode *node = connections.First();
  wxConnection *found = NULL;
  while (node && !found)
  {
    wxConnection *connection = (wxConnection *)node->Data();
    if (connection->hConv == conv)
      found = connection;
    else node = node->Next();
  }
  return found;
}

Bool wxIPCObject::DeleteConnection(HCONV conv)
{
  wxNode *node = connections.First();
  Bool found = FALSE;
  while (node && !found)
  {
    wxConnection *connection = (wxConnection *)node->Data();
    if (connection->hConv == conv)
    {
      found = TRUE;
      delete node;
    }
    else node = node->Next();
  }
  return found;
}

// Find a server from a service name
wxServer *wxFindServer(char *s)
{
  wxNode *node = wxIPCObjects.First();
  wxServer *found = NULL;
  while (node && !found)
  {
    wxIPCObject *object = (wxIPCObject *)node->Data();
    
    if (object->service_name && strcmp(object->service_name, s) == 0)
      found = (wxServer *)object;
    else node = node->Next();
  }
  return found;
}

/*
 * Server
 *
 */

IMPLEMENT_DYNAMIC_CLASS(wxServer, wxIPCObject)

wxServer::wxServer(void)
{
}

Bool wxServer::Create(char *server_name)
{
  service_name = copystring(server_name);
  HSZ wxServiceName = DdeCreateStringHandle(wxIdInst, server_name, CP_WINANSI);

  if (DdeNameService(wxIdInst, wxServiceName, NULL, DNS_REGISTER) == 0)
  {
    wxPrintDDEError();
    return FALSE;
  }
  return TRUE;
}

wxServer::~wxServer(void)
{
  if (service_name)
  {
    HSZ wxServiceName = DdeCreateStringHandle(wxIdInst, service_name, CP_WINANSI);
    if (DdeNameService(wxIdInst, wxServiceName, NULL, DNS_UNREGISTER) == 0)
    {
      wxPrintDDEError();
    }
  }
}

/*
 * Client
 *
 */


IMPLEMENT_DYNAMIC_CLASS(wxClient, wxIPCObject)

wxClient::wxClient(void)
{
}

wxClient::~wxClient(void)
{
}

Bool wxClient::ValidHost(char *WXUNUSED(host))
{
  return TRUE;
}

wxConnection *wxClient::MakeConnection(char *WXUNUSED(host), char *server_name, char *topic)
{
  HSZ wxServiceName = DdeCreateStringHandle(wxIdInst, server_name, CP_WINANSI);
  HSZ topic_atom = DdeCreateStringHandle(wxIdInst, topic, CP_WINANSI);

  HCONV hConv = DdeConnect(wxIdInst, wxServiceName, topic_atom, (PCONVCONTEXT)NULL);
  if (hConv == NULL)
    return NULL;
  else
  {
    wxConnection *connection = OnMakeConnection();
    if (connection)
    {
      connection->hConv = hConv;
      connection->topic_name = copystring(topic);
      connections.Append(connection);
      return connection;
    }
    else return NULL;
  }
}

/*
 * Connection
 */

IMPLEMENT_DYNAMIC_CLASS(wxConnection, wxObject)

wxConnection::wxConnection(char *buffer, int size)
{
  if (buffer == NULL)
  {
    if (wxDefaultIPCBuffer == NULL)
      wxDefaultIPCBuffer = new char[wxDefaultIPCBufferSize];
    buf_ptr = wxDefaultIPCBuffer;
    buf_size = wxDefaultIPCBufferSize;
  }
  else
  {
    buf_ptr = buffer;
    buf_size = size;
  }

  topic_name = NULL;

  client = NULL;
  server = NULL;

  hConv = NULL;
  sending_data = NULL;
}

wxConnection::wxConnection(void)
{
  hConv = NULL;
  sending_data = NULL;
}

wxConnection::~wxConnection(void)
{
  if (topic_name)
    delete [] topic_name ;
  topic_name = NULL ;
}

// Calls that CLIENT can make
Bool wxConnection::Disconnect(void)
{
  wxDeleteConnection(hConv);
  return DdeDisconnect(hConv);
}

Bool wxConnection::Execute(char *data, int size, int format)
{
  DWORD result;
  if (size < 0)
    size = strlen(data);

  size ++;

  DdeClientTransaction((LPBYTE)data, size, hConv,
    NULL, format, XTYP_EXECUTE, 5000, &result);

  return TRUE;
}

char *wxConnection::Request(char *item, int *size, int format)
{
  DWORD result;
  HSZ atom = wxGetAtom(item);

  HDDEDATA returned_data = DdeClientTransaction(NULL, 0, hConv,
    atom, format, XTYP_REQUEST, 5000, &result);

  DWORD len = DdeGetData(returned_data, (LPBYTE)(buf_ptr), buf_size, 0);

  DdeFreeDataHandle(returned_data);

  if (size) *size = (int)len;
  if (len > 0)
  {
    return buf_ptr;
  }
  else return NULL;
}

Bool wxConnection::Poke(char *item, char *data, int size, int format)
{
  DWORD result;
  if (size < 0)
    size = strlen(data);

  size ++;

  HSZ item_atom = wxGetAtom(item);
  DdeClientTransaction((LPBYTE)data, size, hConv,
    item_atom, format, XTYP_POKE, 5000, &result);

  return TRUE;
}

Bool wxConnection::StartAdvise(char *item)
{
  DWORD result;
  HSZ atom = wxGetAtom(item);

  DdeClientTransaction(NULL, 0, hConv,
    atom, CF_TEXT, XTYP_ADVSTART, 5000, &result);

  return (Bool)result;
}

Bool wxConnection::StopAdvise(char *item)
{
  DWORD result;
  HSZ atom = wxGetAtom(item);

  DdeClientTransaction(NULL, 0, hConv,
    atom, CF_TEXT, XTYP_ADVSTOP, 5000, &result);

  return (Bool)result;
}

// Calls that SERVER can make
Bool wxConnection::Advise(char *item, char *data, int size, int format)
{
  if (size < 0)
    size = strlen(data);

  size ++;

  HSZ item_atom = wxGetAtom(item);
  HSZ topic_atom = wxGetAtom(topic_name);
  sending_data = data;
  data_size = size;
  data_type = format;
  return DdePostAdvise(wxIdInst, topic_atom, item_atom);
}

void wxConnection::Notify(Bool WXUNUSED(notify))
{
}


/*

// Pipes
wxChild::wxChild(void)
{
  the_pid = -1;
}

Bool wxChild::Create(char *command, char *argv[])
{
  return FALSE;
}

void wxChild::OnDeath(void)
{
}

*/

HDDEDATA EXPENTRY _EXPORT wxDdeCallback(
WORD wType,
WORD wFmt,
HCONV hConv,
HSZ hsz1,
HSZ hsz2,
HDDEDATA hData,
DWORD WXUNUSED(lData1),
DWORD WXUNUSED(lData2))
{
  switch (wType)
  {
    case XTYP_CONNECT:
    {
      char topic_buf[100];
      char server_buf[100];
      DdeQueryString(wxIdInst, hsz1, (LPSTR)topic_buf, sizeof(topic_buf),
                     CP_WINANSI);
      DdeQueryString(wxIdInst, hsz2, (LPSTR)server_buf, sizeof(topic_buf),
                     CP_WINANSI);
      wxServer *server = wxFindServer(server_buf);
      if (server)
      {
        wxConnection *connection =
          server->OnAcceptConnection(topic_buf);
        if (connection)
        {
          connection->server = server;
          server->connections.Append(connection);
          connection->hConv = 0;
          connection->topic_name = copystring(topic_buf);
          wxCurrentlyConnecting = connection;
          return (HDDEDATA)TRUE;
        }
      }
      else return 0;
      break;
    }

    case XTYP_CONNECT_CONFIRM:
    {
      if (wxCurrentlyConnecting)
      {
        wxCurrentlyConnecting->hConv = hConv;
        wxCurrentlyConnecting = NULL;
        return (HDDEDATA)TRUE;
      }
      else return 0;
      break;
    }

    case XTYP_DISCONNECT:
    {
      wxConnection *connection = wxFindConnection(hConv);
/*
      if (connection)
        wxDebugMsg("Disconnect: found connection.\n");
      else
        wxDebugMsg("Disconnect: did not find connection.\n");
*/
      if (connection && connection->OnDisconnect())
      {
//        wxDebugMsg("Disconnect: OnDisconnect returned TRUE so should be deleted.\n");
        wxDeleteConnection(hConv);  // Delete mapping: hConv => connection
        return (HDDEDATA)TRUE;
      }
      else return 0;
      break;
    }

    case XTYP_EXECUTE:
    {
      wxConnection *connection = wxFindConnection(hConv);

      if (connection)
      {
        DWORD len = DdeGetData(hData, (LPBYTE)(connection->buf_ptr), connection->buf_size, 0);
        DdeFreeDataHandle(hData);
        if (connection->OnExecute(connection->topic_name, connection->buf_ptr, (int)len, wFmt))
          return (HDDEDATA)DDE_FACK;
        else
          return (HDDEDATA)DDE_FNOTPROCESSED;
      } else return (HDDEDATA)DDE_FNOTPROCESSED;
      break;
    }

    case XTYP_REQUEST:
    {
      wxConnection *connection = wxFindConnection(hConv);

      if (connection)
      {
        char item_name[200];
        DdeQueryString(wxIdInst, hsz2, (LPSTR)item_name, sizeof(item_name),
                     CP_WINANSI);

        int user_size = -1;
        char *data = connection->OnRequest(connection->topic_name, item_name, &user_size, wFmt);
        if (data)
        {
          if (user_size < 0) user_size = strlen(data);

          HDDEDATA handle = DdeCreateDataHandle(wxIdInst,
                 (LPBYTE)data, user_size + 1, 0, hsz2, wFmt, 0);
          return handle;
        } else return 0;
      } else return 0;
      break;
    }

    case XTYP_POKE:
    {
      wxConnection *connection = wxFindConnection(hConv);

      if (connection)
      {
        char item_name[200];
        DdeQueryString(wxIdInst, hsz2, (LPSTR)item_name, sizeof(item_name),
                     CP_WINANSI);
        DWORD len = DdeGetData(hData, (LPBYTE)(connection->buf_ptr), connection->buf_size, 0);
        DdeFreeDataHandle(hData);
        connection->OnPoke(connection->topic_name, item_name, connection->buf_ptr, (int)len, wFmt);
        return (HDDEDATA)DDE_FACK;
      } else return (HDDEDATA)DDE_FNOTPROCESSED;
      break;
    }

    case XTYP_ADVSTART:
    {
      wxConnection *connection = wxFindConnection(hConv);

      if (connection)
      {
        char item_name[200];
        DdeQueryString(wxIdInst, hsz2, (LPSTR)item_name, sizeof(item_name),
                     CP_WINANSI);

        return (HDDEDATA)connection->OnStartAdvise(connection->topic_name, item_name);
      } else return 0;
      break;
    }

    case XTYP_ADVSTOP:
    {
      wxConnection *connection = wxFindConnection(hConv);

      if (connection)
      {
        char item_name[200];
        DdeQueryString(wxIdInst, hsz2, (LPSTR)item_name, sizeof(item_name),
                     CP_WINANSI);
        return (HDDEDATA)connection->OnStopAdvise(connection->topic_name, item_name);
      } else return 0;
      break;
    }

    case XTYP_ADVREQ:
    {
      wxConnection *connection = wxFindConnection(hConv);

      if (connection && connection->sending_data)
      {
        HDDEDATA data = DdeCreateDataHandle(wxIdInst,
                          (LPBYTE)connection->sending_data,
                          connection->data_size, 0, hsz2, connection->data_type, 0);
        connection->sending_data = NULL;
        return data;
      } else return NULL;
      break;
    }

    case XTYP_ADVDATA:
    {
      wxConnection *connection = wxFindConnection(hConv);

      if (connection)
      {
        char item_name[200];
        DdeQueryString(wxIdInst, hsz2, (LPSTR)item_name, sizeof(item_name),
                     CP_WINANSI);

        DWORD len = DdeGetData(hData, (LPBYTE)(connection->buf_ptr), connection->buf_size, 0);
        DdeFreeDataHandle(hData);
        if (connection->OnAdvise(connection->topic_name, item_name, connection->buf_ptr, (int)len, wFmt))
          return (HDDEDATA)DDE_FACK;
        else
          return (HDDEDATA)DDE_FNOTPROCESSED;
      } else return (HDDEDATA)DDE_FNOTPROCESSED;
      break;
    }
  }
  return 0;
}

// Atom table stuff
void wxAddAtom(char *string)
{
  HSZ atom = DdeCreateStringHandle(wxIdInst, string, CP_WINANSI);
  wxAtomTable.Append(string, (wxObject *)atom);
}

HSZ wxGetAtom(char *string)
{
  wxNode *node = wxAtomTable.Find(string);
  if (node)
    return (HSZ)node->Data();
  else
  {
    wxAddAtom(string);
    return (HSZ)(wxAtomTable.Find(string)->Data());
  }
}

void wxPrintDDEError(void)
{
  char *err = NULL;
  switch (DdeGetLastError(wxIdInst))
  {
    case DMLERR_ADVACKTIMEOUT:
      err = "A request for a synchronous advise transaction has timed out.";
      break;
    case DMLERR_BUSY:
      err = "The response to the transaction caused the DDE_FBUSY bit to be set.";
      break;
    case DMLERR_DATAACKTIMEOUT:
      err = "A request for a synchronous data transaction has timed out.";
      break;
    case DMLERR_DLL_NOT_INITIALIZED:
      err = "A DDEML function was called without first calling the DdeInitialize function,\n\ror an invalid instance identifier\n\rwas passed to a DDEML function.";
      break;
    case DMLERR_DLL_USAGE:
      err = "An application initialized as APPCLASS_MONITOR has\n\rattempted to perform a DDE transaction,\n\ror an application initialized as APPCMD_CLIENTONLY has \n\rattempted to perform server transactions.";
      break;
    case DMLERR_EXECACKTIMEOUT:
      err = "A request for a synchronous execute transaction has timed out.";
      break;
    case DMLERR_INVALIDPARAMETER:
      err = "A parameter failed to be validated by the DDEML.";
      break;
    case DMLERR_LOW_MEMORY:
      err = "A DDEML application has created a prolonged race condition.";
      break;
    case DMLERR_MEMORY_ERROR:
      err = "A memory allocation failed.";
      break;
    case DMLERR_NO_CONV_ESTABLISHED:
      err = "A client's attempt to establish a conversation has failed.";
      break;
    case DMLERR_NOTPROCESSED:
      err = "A transaction failed.";
      break;
    case DMLERR_POKEACKTIMEOUT:
      err = "A request for a synchronous poke transaction has timed out.";
      break;
    case DMLERR_POSTMSG_FAILED:
      err = "An internal call to the PostMessage function has failed. ";
      break;
    case DMLERR_REENTRANCY:
      err = "Reentrancy problem.";
      break;
    case DMLERR_SERVER_DIED:
      err = "A server-side transaction was attempted on a conversation\n\rthat was terminated by the client, or the server\n\rterminated before completing a transaction.";
      break;
    case DMLERR_SYS_ERROR:
      err = "An internal error has occurred in the DDEML.";
      break;
    case DMLERR_UNADVACKTIMEOUT:
      err = "A request to end an advise transaction has timed out.";
      break;
    case DMLERR_UNFOUND_QUEUE_ID:
      err = "An invalid transaction identifier was passed to a DDEML function.\n\rOnce the application has returned from an XTYP_XACT_COMPLETE callback,\n\rthe transaction identifier for that callback is no longer valid.";
      break;
    default:
      err = "Unrecognised error type.";
      break;
  }
  MessageBox(NULL, (LPCSTR)err, wxTheApp->GetAppName(), MB_OK | MB_ICONINFORMATION);
}

#endif // USE_IPC
