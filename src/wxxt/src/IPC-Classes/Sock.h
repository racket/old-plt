
/**************************************************************** _sm_ begin */
////////////////////////////////////////////////////////////////
//
// Description:
//   A first cut at a fix for the socket-related problems in wxWindows.
//   The basic problem is that TCP does not preserve message boundaries --
//   only the beginning of a message might arrive, all of it might, or
//   several messages may arrive at once -- but the wxWindows code does
//   not know this.
//
// Strategy:
//   Wrap all information passed between the client and server with header
//   and trailer information.  In particular, this wrapper info contains
//   the length of the message, so we know when the whole thing has arrived.
//   This implementation can also signal that more than one full message
//   is waiting.  Looks like this:
//   0000089a|...2202 (== hex 89a) bytes of data...ROGR
//
// Pluses:
//   -- You can write (more) robust, (more) reliable IPC code under wxWindows.
//      A Good Thing!
//   -- Not a single line of code outside of wx_ipc.cc is affected.
//      One of my strongest goals was to minimize changes to existing code.
//
// Minuses/Caveats:
//   -- Small writes are rendered somewhat inefficient (though not as
//      inefficient has having your whole program hang!).
//   -- Tested for Motif only.
//   -- The code is not "safe" enough for my tastes -- you could read/write
//      sockets unsafely, for instance.
//

class SafeSock
{
    const int fd;                     // The fd that this SafeSock manages.
    Bool ownFd;                       // If true, close fd in dtor.
    char * inBox;                     // Accumulates incoming messages.
    size_t inBoxLen;                  // strlen of preceding.

    static const char * const MAGIC;  // Magic sequence must appear at msg end.
    static const int MAGIC_LEN;       // strlen of preceding.
    static const int COUNT_LEN;       // Exact # digits in byte-count wrapper.
    static const int OVRHD_LEN;       // Total bytes of overhead info per write.
    static const char SEP_CHAR;       // Magic char to separate count from data.

    friend class SockMgr;

public:
    // PRIVATE!  May only be created/destroyed by friend class SockMgr.
    SafeSock(int fd_, Bool ownFd_ = FALSE);
    ~SafeSock(void);

    int write(char * buf, int nBytes);
    int read(char * buf, int nBytes, Bool & isWhole, Bool & isMore);
    int getFd(void)  {  return fd;  }
    Bool findMsg(char * & start, char * & end);
};

class SockMgr
{
    static SockMgr * master;  // A ptr to the sole SockMgr (or 0 if none).
    wxList socks;             // A list of ptrs to SafeSock objects.

    // PRIVATE!  Instances are created/destroyed only via the static mfns.
    // This is so we can force uniqueness: only one SockMgr may exist at once.
    // This triggers a g++ warning ("only a private ctor/dtor and no friends");
    // ignore it.
    SockMgr(const SockMgr &);                // No fn body: prevent copying.
    SockMgr & operator = (const SockMgr &);  // No fn body: prevent assignment.

    void addSock(int fd);

  public:
    SockMgr(void)  {  if (!master)  master = this;  }
    ~SockMgr(void);

    static SockMgr & create(void);
    SafeSock * getSock(int fd);
    int close(int fd);
    int connect(int fd, struct sockaddr * name, int nameLen);
    int accept(int fd, struct sockaddr * addr, int * addrLen);
    int read(int fd, char * buf, int nBytes, Bool & isWhole, Bool & isMore);
    int write(int fd, char * buf, int nBytes);
};
