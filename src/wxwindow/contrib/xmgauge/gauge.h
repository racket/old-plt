/* Copyright 1994 GROUPE BULL -- See license conditions in file COPYRIGHT */
/* $Id: */
#ifndef __GAUGE_H
#define __GAUGE_H

#ifdef __cplusplus
extern "C" {
#endif

/****************************************************************
 *
 * Gauge widget public header
 *
 ****************************************************************/


typedef struct _XmGaugeClassRec*	XmGaugeWidgetClass;
typedef struct _XmGaugeRec*	        XmGaugeWidget;

extern WidgetClass xmGaugeWidgetClass;

typedef struct _XmGaugeCallbackStruct{
    int reason;
    XEvent *event;
    int value;
} XmGaugeCallbackStruct;


void
XmGaugeSetValue(Widget w, int value);

void
XmGaugeSetMax(Widget w, int value);

int
XmGaugeGetValue(Widget w);

#ifdef __cplusplus
};
#endif

#endif /* __GAUGE_H */
