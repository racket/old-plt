/* Copyright 1994 GROUPE BULL -- See license conditions in file COPYRIGHT */
/* $Id: */
#ifndef __GAUGEP_H
#define __GAUGEP_H


#include <Xm/PrimitiveP.h>
#include <Xm/DrawP.h>

#include "gauge.h"


typedef struct {
    int empty;
} XmGaugeClassPart;

typedef struct _XmGaugeClassRec {    
    CoreClassPart	        core_class;
    XmPrimitiveClassPart	primitive_class;
    XmGaugeClassPart	        gauge_class;
} XmGaugeClassRec;


typedef struct _XmGaugePart{
    int value;
    int minimum;
    int maximum;
    unsigned char orientation;
    unsigned char processingDirection;

    XtCallbackList dragCallback;
    XtCallbackList valueChangedCallback;
    
    /* private fields */
    Boolean dragging;		/* drag in progress ? */
    int oldx, oldy;
    GC gc;
} XmGaugePart;


typedef struct _XmGaugeRec {
    CorePart		core;
    XmPrimitivePart	primitive;
    XmGaugePart	        gauge;
} XmGaugeRec;

extern XmGaugeClassRec xmGaugeClassRec;






#endif /* __GAUGEP_H */
