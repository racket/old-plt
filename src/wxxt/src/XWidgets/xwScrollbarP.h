/* Generated by wbuild
 * (generator version 3.2)
 */
#ifndef ___XWSCROLLBARP_H
#define ___XWSCROLLBARP_H
#include <./xwBoardP.h>
#include <./xwScrollbar.h>
_XFUNCPROTOBEGIN
typedef void (*scroll_response_Proc)(
#if NeedFunctionPrototypes
Widget ,XtPointer ,XtPointer 
#endif
);
#define XtInherit_scroll_response ((scroll_response_Proc) _XtInherit)

typedef struct {
/* methods */
scroll_response_Proc scroll_response;
/* class variables */
} XfwfScrollbarClassPart;

typedef struct _XfwfScrollbarClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfScrollbarClassPart xfwfScrollbar_class;
} XfwfScrollbarClassRec;

typedef struct {
/* resources */
Boolean  vertical;
XtCallbackList  scrollCallback;
XtCallbackProc  scrollResponse;
Cardinal  initialDelay;
Cardinal  repeatDelay;
float  increment;
Pixel  scrollbarForeground;
Dimension  shadow;
Dimension  minsize;
Boolean  drawgrayScrollbar;
Boolean  egdeBar;
/* private state */
Widget  arrow1;
Widget  arrow2;
Widget  slider;
Boolean  initializing;
XtCallbackProc  slider_scroll;
} XfwfScrollbarPart;

typedef struct _XfwfScrollbarRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfScrollbarPart xfwfScrollbar;
} XfwfScrollbarRec;

externalref XfwfScrollbarClassRec xfwfScrollbarClassRec;

_XFUNCPROTOEND
#endif /* ___XWSCROLLBARP_H */
