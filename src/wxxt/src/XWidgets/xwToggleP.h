/* Generated by wbuild
 * (generator version 3.2)
 */
#ifndef ___XWTOGGLEP_H
#define ___XWTOGGLEP_H
#include <./xwButtonP.h>
#include <./xwToggle.h>
_XFUNCPROTOBEGIN

typedef struct {
/* methods */
/* class variables */
int dummy;
} XfwfToggleClassPart;

typedef struct _XfwfToggleClassRec {
CoreClassPart core_class;
CompositeClassPart composite_class;
XfwfCommonClassPart xfwfCommon_class;
XfwfFrameClassPart xfwfFrame_class;
XfwfBoardClassPart xfwfBoard_class;
XfwfLabelClassPart xfwfLabel_class;
XfwfButtonClassPart xfwfButton_class;
XfwfToggleClassPart xfwfToggle_class;
} XfwfToggleClassRec;

typedef struct {
/* resources */
XtCallbackList  onCallback;
XtCallbackList  offCallback;
Boolean  on;
Dimension  indicatorSize;
int  indicatorType;
Pixel  indicatorColor;
/* private state */
GC  indicator_gc;
GC  center_gc;
GC  ex_gc;
Dimension  saveLeftMargin;
} XfwfTogglePart;

typedef struct _XfwfToggleRec {
CorePart core;
CompositePart composite;
XfwfCommonPart xfwfCommon;
XfwfFramePart xfwfFrame;
XfwfBoardPart xfwfBoard;
XfwfLabelPart xfwfLabel;
XfwfButtonPart xfwfButton;
XfwfTogglePart xfwfToggle;
} XfwfToggleRec;

externalref XfwfToggleClassRec xfwfToggleClassRec;

_XFUNCPROTOEND
#endif /* ___XWTOGGLEP_H */
