/* This header has been created to hide the linkage between the 32-bit
   Windows code and the 16-bit DLL that provides the TCP/IP protocol stack.
   See the Windows 32-bit programming guide for the WATCOM C compiler
*/

#ifndef _H_TCP1632
#define _H_TCP1632

#undef TRUE
#undef FALSE
#include <winsock.h> 

#ifdef __WINDOWS_386__

extern WSADATA WSAData;

extern HINDIR acceptHandle;
extern HINDIR bindHandle;
extern HINDIR connectHandle;
extern HINDIR GetErrnoHandle;
extern HINDIR gethostnameHandle;
extern HINDIR gethostbynameHandle;
extern HINDIR getprotobynameHandle;
extern HINDIR ntohsHandle;
extern HINDIR ntohlHandle;
extern HINDIR htonlHandle;
extern HINDIR htonsHandle;
extern HINDIR inet_addrHandle;
extern HINDIR inet_ntoaHandle;
extern HINDIR ioctlHandle;
extern HINDIR listenHandle;
extern HINDIR selectHandle;
extern HINDIR setsockoptHandle;
extern HINDIR socketHandle;
extern HINDIR socloseHandle;
extern HINDIR soreadHandle;
extern HINDIR sowriteHandle;
extern HINDIR getservbynameHandle;
extern HINDIR shutdownHandle;
extern HINDIR WSACancelBlockingCallHandle;
extern HINDIR WSAIsBlockingHandle;
extern HINDIR WSAAsyncSelectHandle;

extern HINDIR __WSAFDIsSetHandle;

#define __WSAFDIsSet(s, set) \
 (short)InvokeIndirectFunction(__WSAFDIsSetHandle, s, set)

#define accept(s, addr, addrlen) \
 (short)InvokeIndirectFunction(acceptHandle, s, addr, addrlen)

#define bind(s, name, name_len) \
 (short)InvokeIndirectFunction(bindHandle, s, name, name_len)

#define connect(s, name, name_len) \
 (short)InvokeIndirectFunction(connectHandle, s, name, name_len)

#define WSAIsBlocking() \
 (BOOL)InvokeIndirectFunction(WSAIsBlockingHandle)

#define WSACancelBlockingCall() \
 (short)InvokeIndirectFunction(WSACancelBlockingCallHandle)

#define WSAGetLastError() \
 (short)InvokeIndirectFunction(GetErrnoHandle)
#define GetErrno WSAGetLastError

#define gethostname(name, name_len) \
 (short)InvokeIndirectFunction(gethostnameHandle, name, name_len)

#define gethostbyname(name) \
 (struct hostent *)InvokeIndirectFunction(gethostbynameHandle, name)

#define getservbyname(name, prot) \
 (struct servent *)InvokeIndirectFunction(getservbynameHandle, name, prot)

#define getprotobyname(name) \
 (struct protoent *)InvokeIndirectFunction(getprotobynameHandle, name)

#define htonl(hostlong) \
 (u_long)InvokeIndirectFunction(htonlHandle, hostlong)

#define ntohl(netlong) \
 (u_long)InvokeIndirectFunction(ntohlHandle, netlong)

#define htons(hostshort) \
 (u_short)InvokeIndirectFunction(htonsHandle, hostshort)

#define ntohs(netshort) \
 (u_short)InvokeIndirectFunction(ntohsHandle, netshort)

#define inet_addr(cp) \
 (unsigned long)InvokeIndirectFunction(inet_addrHandle, cp)

#define inet_ntoa(in, cp) \
 (char *)InvokeIndirectFunction(inet_ntoaHandle, in, cp)

#define ioctlsocket(s, request, argp) \
 (short)InvokeIndirectFunction(ioctlHandle, s, request, argp)

#define listen(s, backlog) \
 (short)InvokeIndirectFunction(listenHandle, s, backlog)

#define select(nfds, readfds, writefds, exceptfds, timeout) \
 (short)InvokeIndirectFunction(selectHandle, nfds, readfds, writefds, exceptfds, timeout)

#define WSAAsyncSelect(s, hWnd, wMsg, lEvent ) \
 (short)InvokeIndirectFunction(WSAAsyncSelectHandle, s, hWnd, wMsg, lEvent)

#define setsockopt(s, level, optname, optval, optlen) \
 (short)InvokeIndirectFunction(setsockoptHandle, s, level, optname, optval, optlen)

#define socket(domain, type, protocol) \
 (short)InvokeIndirectFunction(socketHandle, domain, type, protocol)

#define closesocket(s) \
 (short)InvokeIndirectFunction(socloseHandle, s)

#define recv(s, buf, nbytes, flags) \
 (short)InvokeIndirectFunction(soreadHandle, s, buf, nbytes, flags)

#define send(s, buf, nbytes, flags) \
 (short)InvokeIndirectFunction(sowriteHandle, s, buf, nbytes, flags)

#define shutdown(s, how) \
 (short)InvokeIndirectFunction(shutdownHandle, s, how)

/* After the macros that are used to hide the indirection we need to
   define the two functions that load and free the DLL library. */

BOOL LoadTCPDLL(int, WSADATA *);
#define WSAStartup(X,Y) (LoadTCPDLL((int)(X),Y) ? 0 : -1);

int FreeTCPDLL(void);
#define WSACleanup FreeTCPDLL

/* #endif JACS */

/* For those LWP programers */
#define soclose(s) closesocket(s)
#define soread(s, buf, bytes) recv(s,buf,nbytes,0)
#define sowrite(s, buf, nbytes) send(s,buf,nbytes,0)

#endif /* __WINDOWS_386__ */
#endif /* _H_TCP1632 */

