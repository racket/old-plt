
extern wxWindow *wxDefaultParent;

/* MATTHEW: [11] */
/* Interface for a window manager in a separate thread */

#ifdef USE_SEP_WIN_MANAGER

extern DWORD wxwmTheManagerThreadId;

void wxwmMainThreadSuspend(void);
void wxwmMainThreadResume(void);

void wxwmMessage(int, LPARAM);

#define WXM_START (WM_USER)
#define WXM_CREATE_DC (WM_USER + 1)
#define WXM_CREATE_DIALOG (WM_USER + 2)
#define WXM_CREATE_WINDOW (WM_USER + 3)
#define WXM_CREATE_TIMER (WM_USER + 4)
#define WXM_GET_MESSAGE (WM_USER + 5)
#define WXM_PEEK_MESSAGE (WM_USER + 6)
#define WXM_TRANSLATE (WM_USER + 7)
#define WXM_MESSAGE_BOX (WM_USER + 8)
#define WXM_PUT_FILE (WM_USER + 9)
#define WXM_GET_FILE (WM_USER + 10)
#define WXM_CHECK_SHIFT_CTL_C (WM_USER + 11)
#define WXM_DESTROY_WINDOW (WM_USER + 12)
#define WXM_CREATE_MENU (WM_USER + 13)
#define WXM_DESTROY_MENU (WM_USER + 14)
#define WXM_CREATE_POPUP_MENU (WM_USER + 15)
#define WXM_TRACK_POPUP (WM_USER + 16)
#define WXM_SET_FOCUS (WM_USER + 17)
#define WXM_BRING_TOP (WM_USER + 18)
#define WXM_GET_DC (WM_USER + 19)
#define WXM_RELEASE_DC (WM_USER + 20)
#define WXM_REPLY (WM_USER + 39)
#define WXM_END (WM_USER + 40)

#define WXM_RELEASE WM_USER

typedef struct {
  HWND hparent;
  DLGPROC proc;
  char *dialog_template;
  HWND result;
} wxwmCreateDialog;

typedef struct {
  DWORD extendedStyle;
  char *wclass;
  char *title;
  int x, y, w, h;
  DWORD style;
  HMENU windows_id;
  HWND hparent;
  HWND result;
  HINSTANCE inst;
  LPVOID data;
} wxwmCreateWindow;

typedef struct {
  UINT id;
  UINT milliseconds;
  TIMERPROC proc;
  UINT result;
} wxwmCreateTimer;

typedef struct {
  HWND owner;
  LPCTSTR text;
  LPCTSTR title;
  UINT style;
  int result;
} wxwmMessageBox;

typedef struct {
  LPOPENFILENAME name;
  BOOL result;
} wxwmGetPutFile;

typedef struct {
  HMENU menu;
  UINT flags;
  int x, y, rsvd;
  HWND win;
  LPRECT lprc;
} wxwmTrackPopup;

typedef struct {
  HWND w;
  HDC dc;
} wxwmRelease;

HWND wxwmCreateWindowEx(
  DWORD extendedStyle,
  char *wclass,
  char *title,
  DWORD style,
  int x1, int x2, int w, int h,
  HWND hparent,
  HMENU windows_id,
  HINSTANCE inst,
  LPVOID data
  );
void wxwmDestroyWindow(HWND);

BOOL wxwmGetMessage(MSG *msg);
void wxwmTranslateMessage(MSG *msg);
void wxwmDispatchMessage(MSG *msg);

HMENU wxwmCreateMenu();
HMENU wxwmCreatePopupMenu();
#define wxwmDestroyMenu(x)  wxwmMessage(WXM_DESTROY_MENU, (LPARAM)(x))
#define wxwmBringWindowToTop(x)  wxwmMessage(WXM_BRING_TOP, (LPARAM)(x))

void wxwmSetFocus(HWND x);

void wxwmTrackPopupMenu(HMENU menu, UINT flags, int x, int y, int rsvd,
								HWND win, LPRECT lprc);

void wxwmNotify(char *, int);

HDC wxwmGetDC(HWND w);
void wxwmReleaseDC(HWND w, HDC dc);

Bool wxwmCheckInMain();
void wxwmCheckOutMain(Bool c);

HDC wxwmCreateCompatibleDC(HDC dc);

#else

#define wxwmCreateWindowEx ::CreateWindowEx
#define wxwmDestroyWindow ::DestroyWindow
#define wxwmCreateMenu ::CreateMenu
#define wxwmCreatePopupMenu ::CreatePopupMenu
#define wxwmDestroyMenu ::DestroyMenu
#define wxwmGetMessage(m) ::GetMessage(m, NULL, 0, 0)
#define wxwmTranslateMessage ::TranslateMessage
#define wxwmDispatchMessage ::DispatchMessage
#define wxwmTrackPopupMenu ::TrackPopupMenu
#define wxwmSetFocus ::SetFocus
#define wxwmBringWindowToTop ::BringWindowToTop
#define wxwmGetDC ::GetDC
#define wxwmReleaseDC ::ReleaseDC
#define wxwmCreateCompatibleDC ::CreateCompatibleDC

#define wxwmCheckInMain() 1
#define wxwmCheckOutMain(v)

#define wxwmNotify(x, y)


#endif

