
#if defined(WINDOWS_PROCESSES) || defined(DETECT_WIN32_CONSOLE_STDIN)
# ifndef NO_STDIO_THREADS
typedef struct {
  fd_set set;

  int num_handles;
  HANDLE *handles;

  int *repost_sema;

} win_extended_fd_set;
# endif
#endif
