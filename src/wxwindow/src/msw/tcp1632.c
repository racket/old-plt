/* This module is used to initialise the handles that are used to access the 16-bit DLL from
   the 32-bit code. I have used names that are specific to the DLL so that a program could use
   this technique several times without name conflicts. */

#include "tcp1632.h"

#ifdef __WINDOWS_386__

/* Here we hold the handles to allow access to the 16-bit DLL from the 32-bit app. */

HINDIR acceptHandle;
HINDIR bindHandle;
HINDIR connectHandle;
HINDIR GetErrnoHandle;
HINDIR gethostnameHandle;
HINDIR gethostbynameHandle;
HINDIR getprotobynameHandle;
HINDIR getservbynameHandle;
HINDIR htonlHandle;
HINDIR htonsHandle;
HINDIR ntohlHandle;
HINDIR ntohsHandle;
HINDIR inet_addrHandle;
HINDIR inet_ntoaHandle;
HINDIR ioctlHandle;
HINDIR listenHandle;
HINDIR selectHandle;
HINDIR setsockoptHandle;
HINDIR socketHandle;
HINDIR socloseHandle;
HINDIR soreadHandle;
HINDIR sowriteHandle;
HINDIR shutdownHandle;
HINDIR __WSAFDIsSetHandle;
HINDIR WSAStartupHandle;
HINDIR WSACleanupHandle;
HINDIR WSAIsBlockingHandle;
HINDIR WSACancelBlockingCallHandle;
HINDIR WSAAsyncSelectHandle;

#define Startup(s, set) \
 (short)InvokeIndirectFunction(WSAStartupHandle, s, set)
#define Cleanup() \
 (short)InvokeIndirectFunction(WSACleanupHandle)


static HANDLE dll;

LPCSTR dll_name = "winsock.dll";

BOOL LoadTCPDLL(int version, WSADATA *WSAData)
{
  FARPROC addr;

  if (dll >= 32) return(TRUE); /* Already done! */

  dll = LoadLibrary(dll_name);
  if (dll < 32) {
#ifdef wx_msw
    MessageBox(GetFocus(), "Couldn't load WinSock DynaLink Library", "Error", MB_OK );
#else
    panic("Can't load network Dyna Link (%s)", dll_name);
#endif
    return (FALSE);
  }

  addr = GetProcAddress(dll, "WSAStartup");
  WSAStartupHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "WSACleanup");
  WSACleanupHandle = GetIndirectFunctionHandle(addr, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "__WSAFDIsSet");
  __WSAFDIsSetHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "accept");
  acceptHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "bind");
  bindHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "connect");
  connectHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_WORD,INDIR_ENDLIST);

  addr = GetProcAddress(dll, "WSAGetLastError");
  GetErrnoHandle = GetIndirectFunctionHandle(addr, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "gethostname");
  gethostnameHandle = GetIndirectFunctionHandle(addr, INDIR_PTR, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "getprotobyname");
  getprotobynameHandle = GetIndirectFunctionHandle(addr, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "gethostbyname");
  gethostbynameHandle = GetIndirectFunctionHandle(addr, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "getservbyname");
  getservbynameHandle = GetIndirectFunctionHandle(addr, INDIR_PTR, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "htonl");
  htonlHandle = GetIndirectFunctionHandle(addr, INDIR_DWORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "htons");
  htonsHandle = GetIndirectFunctionHandle(addr, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "ntohl");
  ntohlHandle = GetIndirectFunctionHandle(addr, INDIR_DWORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "ntohs");
  ntohsHandle = GetIndirectFunctionHandle(addr, INDIR_WORD, INDIR_ENDLIST);


  addr = GetProcAddress(dll, "inet_addr");
  inet_addrHandle = GetIndirectFunctionHandle(addr, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "inet_ntoa");
  inet_ntoaHandle = GetIndirectFunctionHandle(addr,
	INDIR_DWORD, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "ioctlsocket");
  ioctlHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_WORD, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "listen");
  listenHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "select");
  selectHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_PTR, INDIR_PTR, INDIR_PTR, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "WSAAsyncSelect");
  WSAAsyncSelectHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_WORD, INDIR_WORD, INDIR_DWORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "setsockopt");
  setsockoptHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_WORD, INDIR_WORD, INDIR_PTR, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "socket");
  socketHandle = GetIndirectFunctionHandle(addr,
        INDIR_WORD, INDIR_WORD, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "closesocket");
  socloseHandle = GetIndirectFunctionHandle(addr, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "recv");
  soreadHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_WORD, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "send");
  sowriteHandle = GetIndirectFunctionHandle(addr,
	INDIR_WORD, INDIR_PTR, INDIR_WORD, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "shutdown");
  shutdownHandle = GetIndirectFunctionHandle(addr, INDIR_WORD, INDIR_WORD, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "WSACancelBlockingCall");
  WSACancelBlockingCallHandle = GetIndirectFunctionHandle(addr, INDIR_ENDLIST);

  addr = GetProcAddress(dll, "WSAIsBlocking");
  WSAIsBlockingHandle = GetIndirectFunctionHandle(addr, INDIR_ENDLIST);

  onexit(FreeTCPDLL);

  if (Startup((WORD)version, WSAData) != 0)
    return FALSE;

  return(TRUE);
}


int FreeTCPDLL(void)
{
  int result = 0;

  if (dll >= 32) {
    result= Cleanup();
    FreeLibrary(dll);
    dll = 0;
  }
  return result;
}

#else

/* Compat. hooks for 16 bit lib */
BOOL LoadTCPDLL(int version, WSADATA *WSAData)
{
 if (WSAStartup(version,WSAData) == 0) return TRUE;
 return FALSE;
}

int FreeTCPDLL(void)
{
  return WSACleanup();  
}

#endif
