/* Copyright 1994 GROUPE BULL -- See license conditions in file COPYRIGHT */
/* $Id: */

#include "gaugep.h"


void 
GaugePick(Widget w, XEvent *e, String *args, Cardinal  *num_args);
void 
GaugeDrag(Widget w, XEvent *e, String *args, Cardinal  *num_args);
void 
GaugeDrop(Widget w, XEvent *e, String *args, Cardinal  *num_args);



static char translations[] =
"<Btn1Down>: GaugePick()\n\
 <Btn1Motion>: GaugeDrag()\n\
 <Btn1Up>: GaugeDrop()\n\
";



static XtActionsRec actions[] = {
    {"GaugePick", GaugePick},
    {"GaugeDrag", GaugeDrag},
    {"GaugeDrop", GaugeDrop},
};

static void
DrawSlider(XmGaugeWidget gw, Boolean clear)
{
#define THIS gw->gauge
    int size, sht, full, empty;
    float ratio;
/***chubraev
    char string[20];
    int len;
    unsigned long backgr,foregr;
    XRectangle rects[1];
***/
    
    sht = gw->primitive.shadow_thickness;

    ratio =  (float)THIS.value/
             (float)(THIS.maximum - THIS.minimum);
    full = (ratio == 1.0);
    empty = !ratio;

/***chubraev
    sprintf(string,"%-d%%",(int)(ratio*100));
    len=strlen(string);
    XtVaGetValues(gw,XmNbackground,&backgr,XmNforeground,&foregr,NULL);
***/

    if (clear && 0) {
	XClearArea(XtDisplay(gw), XtWindow(gw), sht, sht,
		   gw->core.width - 2 * sht, gw->core.height - 2 * sht, False);
    }
    switch(THIS.orientation) {
    case XmHORIZONTAL:
	size = (gw->core.width - 2 * sht)*ratio;
	/***chubraev
	XDrawString(XtDisplay(gw), XtWindow(gw), THIS.gc, sht+gw->core.width/2, 
gw->core.height - 2 * sht, string, len);
	***/
	switch(THIS.processingDirection) {
	case XmMAX_ON_RIGHT:
	case XmMAX_ON_BOTTOM:
	  if (!empty)
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   sht, sht, size, gw->core.height - 2 * sht);

	  if (!full)
	    XClearArea(XtDisplay(gw), XtWindow(gw), 
		       sht + size, sht, 
		       gw->core.width - 2 * sht - size, 
		       gw->core.height - 2 * sht, False);

            /***chubraev
            rects[0].x = sht; rects[0].y = sht;
            rects[0].width = size; rects[0].height = gw->core.height - 2 * sht;
	    ***/
	    break;
	case XmMAX_ON_LEFT:
	case XmMAX_ON_TOP:
	  if (!empty)
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   gw->core.width - size - sht, sht,
			   size, gw->core.height - 2 * sht);
            
	    if (!full)
	      XClearArea(XtDisplay(gw), XtWindow(gw), 
			 sht, sht, 
			 gw->core.width - 2 * sht - size, 
			 gw->core.height - 2 * sht, False);

            /***chubraev
            rects[0].x = gw->core.width - size - sht; rects[0].y = sht;
            rects[0].width = size; rects[0].height = gw->core.height - 2 * sht;
	    ***/
	    break;
	}
        /***chubraev
        XSetClipRectangles(XtDisplay(gw), THIS.gc, 0, 0, rects, 1, Unsorted);
	XSetForeground(XtDisplay(gw), THIS.gc, backgr);
	XDrawString(XtDisplay(gw), XtWindow(gw), THIS.gc, sht+gw->core.width/2, 
gw->core.height - 2 * sht, string, len);
	***/

	break;
    case XmVERTICAL:
	size = (gw->core.height - 2 * sht)*ratio;
	/***chubraev
	XDrawString(XtDisplay(gw), XtWindow(gw), THIS.gc, sht, 
sht+gw->core.height/2, string,len);
	***/
	switch(THIS.processingDirection) {
	case XmMAX_ON_RIGHT:
	case XmMAX_ON_BOTTOM:
	  if (!empty)
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   sht, sht, gw->core.width - 2 * sht, size);

	  if (!full)
	    XClearArea(XtDisplay(gw), XtWindow(gw), 
		       sht, sht + size, 
		       gw->core.width - 2 * sht, 
		       gw->core.height - size - 2 * sht, False);

            /***chubraev
            rects[0].x = sht; rects[0].y = sht;
            rects[0].width = gw->core.width - 2 * sht; rects[0].height = size;
	    ***/
	    break;
	case XmMAX_ON_LEFT:
	case XmMAX_ON_TOP:
	  if (!empty)
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   sht, gw->core.height - size - sht,
			   gw->core.width - 2 * sht, size);

	  if (!full)
	    XClearArea(XtDisplay(gw), XtWindow(gw), 
		       sht, sht, 
		       gw->core.width - 2 * sht, 
		       gw->core.height - size - 2 * sht, False);

            /***chubraev
            rects[0].x = sht; rects[0].y = gw->core.height - size - sht;
            rects[0].width = gw->core.width - 2 * sht; rects[0].height = size;
	    ***/
	}
        /***chubraev
        XSetClipRectangles(XtDisplay(gw), THIS.gc, 0, 0, rects, 1, Unsorted);
	XSetForeground(XtDisplay(gw), THIS.gc, backgr);
	XDrawString(XtDisplay(gw), XtWindow(gw), THIS.gc, sht, 
sht+gw->core.height/2, string,len);
	***/
	break;
    }
    /***chubraev
    XSetClipMask(XtDisplay(gw), THIS.gc, None);
    XSetForeground(XtDisplay(gw), THIS.gc, foregr);
    ***/
#undef THIS	
}

/* Old code
 */
#if 0
static void
DrawSlider(XmGaugeWidget gw, Boolean clear)
{
#define THIS gw->gauge
    int size, sht;
/*    float ratio; */

    sht = gw->primitive.shadow_thickness;
/* See fix comment below: can cause divide by zero error.
    ratio = (float)((float)THIS.maximum -
		    (float)THIS.minimum) / (float)THIS.value;
*/
    if(clear) {
	XClearArea(XtDisplay(gw), XtWindow(gw), sht, sht,
		   gw->core.width - 2 * sht, gw->core.height - 2 * sht, False);
    }
    switch(THIS.orientation) {
    case XmHORIZONTAL:
/*	size = (gw->core.width - 2 * sht) / ratio; */
/* A fix suggested by Dmitri Chubraev */
        size = (gw->core.width - 2 * sht) /((float)THIS.maximum-(float)THIS.minimum)*(float)THIS.value;
	switch(THIS.processingDirection) {
	case XmMAX_ON_RIGHT:
	case XmMAX_ON_BOTTOM:
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   sht, sht, size, gw->core.height - 2 * sht);
	    break;
	case XmMAX_ON_LEFT:
	case XmMAX_ON_TOP:
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   gw->core.width - size - sht, sht,
			   size, gw->core.height - 2 * sht);
	    break;
	}
	break;
    case XmVERTICAL:
        size = (gw->core.height - 2 * sht) /((float)THIS.maximum-(float)THIS.minimum)*(float)THIS.value;
/*	size = (gw->core.height - 2 * sht)/ ratio; */
	switch(THIS.processingDirection) {
	case XmMAX_ON_RIGHT:
	case XmMAX_ON_BOTTOM:
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   sht, sht, gw->core.width - 2 * sht, size);
	    break;
	case XmMAX_ON_LEFT:
	case XmMAX_ON_TOP:
	    XFillRectangle(XtDisplay(gw), XtWindow(gw), THIS.gc,
			   sht, gw->core.height - size - sht,
			   gw->core.width - 2 * sht, size);
	}
	break;
    }
#undef THIS	
}
#endif

static void
Initialize(Widget req, Widget new_w, ArgList args, Cardinal *num_args )
{
    XmGaugeWidget gw = (XmGaugeWidget)new_w;
#define THIS gw->gauge
    XGCValues values;

    values.foreground = gw->primitive.foreground;
    THIS.gc = XtGetGC(new_w, GCForeground, &values);
    
#undef THIS	
    
}



static void
Destroy(Widget w)
{
    XmGaugeWidget gw = (XmGaugeWidget)w;
#define THIS gw->gauge
    XtReleaseGC(w, THIS.gc);
#undef THIS	
}



    
static Boolean
SetValues(
        Widget cw,
        Widget rw,
        Widget nw,
        ArgList args,
        Cardinal *num_args )
{
    XmGaugeWidget cgw = (XmGaugeWidget)cw;
    XmGaugeWidget ngw = (XmGaugeWidget)nw;

    Boolean redraw = False;
    if(cgw->primitive.foreground != ngw->primitive.foreground) {
	XGCValues values;

	redraw = True;
	XtReleaseGC(nw, ngw->gauge.gc);
	values.foreground = ngw->primitive.foreground;
	ngw->gauge.gc = XtGetGC(nw, GCForeground, &values);
    }
    if(cgw->gauge.value != ngw->gauge.value) {
       redraw = True;
    }
    return redraw;
}




static void
ExposeProc(Widget w, XEvent *event, Region r)
{
    XmGaugeWidget gw = (XmGaugeWidget)w;
#define THIS gw->gauge
    int sht;

    sht = gw->primitive.shadow_thickness;
    _XmDrawShadows(XtDisplay(w), XtWindow(w),
		   gw->primitive.top_shadow_GC,
		   gw->primitive.bottom_shadow_GC,
		   0, 0, w->core.width, w->core.height,
		   sht, XmSHADOW_IN);
    DrawSlider(gw, False);
#undef THIS	
}





static XtResource 
resources[] = {
#define offset(field) XtOffset(XmGaugeWidget, gauge.field)
  {XmNvalue, XmCValue, XtRInt, sizeof(int),
     offset(value), XtRImmediate, (caddr_t)10},
  
  {XmNminimum, XmCValue, XtRInt, sizeof(int),
     offset(minimum), XtRImmediate, (caddr_t)0},
  
  {XmNmaximum, XmCValue, XtRInt, sizeof(int),
     offset(maximum), XtRImmediate, (caddr_t)100},
  
  {XmNorientation, XmCOrientation, XmROrientation, sizeof(unsigned char),
     offset(orientation), XtRImmediate, (caddr_t)XmVERTICAL},
  
  {XmNprocessingDirection, XmCProcessingDirection,
   XmRProcessingDirection, sizeof(unsigned char),
   offset(processingDirection), XtRImmediate, (caddr_t)XmMAX_ON_RIGHT},
  
  {XmNdragCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
     offset(dragCallback), XtRImmediate, (caddr_t)NULL},
  
  {XmNvalueChangedCallback, XmCCallback, XmRCallback, sizeof(XtCallbackList),
     offset(valueChangedCallback), XtRImmediate, (caddr_t)NULL},

  
#undef offset
};


XmGaugeClassRec xmGaugeClassRec = {
    {				/* core fields */
	(WidgetClass) &xmPrimitiveClassRec, /* superclass		*/
	"XmGauge",		/* class_name		*/
	sizeof(XmGaugeRec),	/* widget_size		*/
	NULL,			/* class_initialize		*/
	NULL,			/* class_part_initialize	*/
	FALSE,			/* class_inited		*/
	Initialize,		/* initialize		*/
	NULL,			/* initialize_hook		*/
	XtInheritRealize,	/* realize			*/
	actions,		/* actions			*/
	XtNumber(actions),	/* num_actions		*/
	resources,		/* resources		*/
	XtNumber(resources),	/* num_resources		*/
	NULLQUARK,		/* xrm_class		*/
	TRUE,			/* compress_motion		*/
	TRUE,			/* compress_exposure	*/
	TRUE,			/* compress_enterleave	*/
	FALSE,			/* visible_interest		*/
	Destroy,		/* destroy			*/
	NULL,			/* resize			*/
	ExposeProc,		/* expose			*/
	SetValues,		/* set_values		*/
	NULL,			/* set_values_hook		*/
	XtInheritSetValuesAlmost, /* set_values_almost	*/
	NULL,			/* get_values_hook		*/
	NULL,			/* accept_focus		*/
	XtVersion,		/* version			*/
	NULL,			/* callback_private		*/
	translations,		/* tm_table			*/
	NULL,			/* query_geometry		*/
	NULL,			/* display_accelerator	*/
	NULL			/* extension		*/
    },
				/* primitive_class fields */
    {
	NULL,			/* border_highlight	*/
	NULL,			/* border_unhighlight	*/
	NULL,			/* translations		*/
	NULL,			/* arm_and_activate	*/
	NULL,			/* syn_resources	*/
	0,			/* num_syn_resources	*/
	NULL			/* extension		*/
    },
    { /* gauge fields */
	0			/* empty		*/
    }
};

WidgetClass xmGaugeWidgetClass = (WidgetClass)&xmGaugeClassRec;




void 
GaugePick(Widget w, XEvent *e, String *args, Cardinal  *num_args)
{
    XmGaugeWidget gw = (XmGaugeWidget)w;
#define THIS gw->gauge
    int size, sht;
    float ratio;
    Boolean dragging = False;
    XButtonEvent *event = (XButtonEvent *)e;
    int x, y;

    x = event->x;
    y = event->y;
    sht = gw->primitive.shadow_thickness;
    _XmDrawShadows(XtDisplay(w), XtWindow(w),
		   gw->primitive.top_shadow_GC,
		   gw->primitive.bottom_shadow_GC,
		   0, 0, w->core.width, w->core.height,
		   sht, XmSHADOW_IN);


    ratio = (float)((float)THIS.maximum -
		    (float)THIS.minimum) / (float)THIS.value;		   
    switch(THIS.orientation) {
    case XmHORIZONTAL:
	size = (w->core.width - 2 * sht) / ratio;
	switch(THIS.processingDirection) {
	case XmMAX_ON_RIGHT:
	case XmMAX_ON_BOTTOM:
	    dragging = (x > sht) && (y > sht) &&
		(x < sht + size) && (y < w->core.height - sht);
	    break;
	case XmMAX_ON_LEFT:
	case XmMAX_ON_TOP:
	    dragging = (x > w->core.width - size - sht) && (y > sht) &&
		(x < w->core.width - sht) && (y < w->core.height + sht);
	    break;
	}
	break;
    case XmVERTICAL:
	size = (w->core.height - 2 * sht) / ratio;
	switch(THIS.processingDirection) {
	case XmMAX_ON_RIGHT:
	case XmMAX_ON_BOTTOM:
	    dragging = (x > sht) && (y > sht) &&
		(x < w->core.width - sht) &&
		(y < w->core.width - 2 * sht + size);
	    break;
	case XmMAX_ON_LEFT:
	case XmMAX_ON_TOP:
	    dragging = (x > sht) && (y > w->core.height - size - sht) &&
		(x < w->core.width - sht) && (y < w->core.height - sht);
	}
	break;
    }
    THIS.dragging = dragging;
    THIS.oldx = x;
    THIS.oldy = y;
#undef THIS    
}

#define round(x) ( (x) > 0 ? ((x) + 0.5) : -(-(x) + 0.5) )

void 
GaugeDrag(Widget w, XEvent *e, String *args, Cardinal  *num_args)
{
    XmGaugeWidget gw = (XmGaugeWidget)w;
#define THIS gw->gauge
    int sht, x, y, max, value;
    float ratio, nratio, size, nsize, fvalue, delta;
    XMotionEvent *event = (XMotionEvent *)e;
    
    if( ! THIS.dragging) return;

    x = event->x;
    y = event->y;
    sht = gw->primitive.shadow_thickness;

    ratio = (float)THIS.value / (float)((float)THIS.maximum -
					(float)THIS.minimum);
    switch(THIS.orientation) {
    case XmHORIZONTAL:
	max = (w->core.width - 2 * sht);
	size = (float)max * ratio;
	delta =  (float)x - (float)THIS.oldx;
	break;
    case XmVERTICAL:
	max = (w->core.height - 2 * sht);
	size = (float) max * ratio;
	delta =  (float)y - (float)THIS.oldy;
	break;
    }
    switch(THIS.processingDirection) {
    case XmMAX_ON_RIGHT:
    case XmMAX_ON_BOTTOM:
	nsize = size + delta;
	break;
    default:
	nsize = size - delta;
    }
    if(nsize > (float)max) nsize = (float)max;
    if(nsize < (float)0 ) nsize = (float)0;
    nratio =  nsize / (float)max;
    
    fvalue = (int)((float)THIS.maximum -
			 (float)THIS.minimum) * (float)nsize / (float)max;
    value = round(fvalue);

    THIS.value = value;
    THIS.oldx = x;
    THIS.oldy = y;

    /* clear old slider only if it was larger */
    DrawSlider(gw, (nsize < size));
    
    {
	XmGaugeCallbackStruct call;

	if(NULL  != THIS.dragCallback) {
	    call.reason = XmCR_DRAG;
	    call.event = e;
	    call.value = THIS.value;
	    XtCallCallbacks(w, XmNdragCallback, &call);
	}
    }
#undef THIS    
}


void 
GaugeDrop(Widget w, XEvent *e, String *args, Cardinal  *num_args)
{
    XmGaugeWidget gw = (XmGaugeWidget)w;
#define THIS gw->gauge
    if( ! THIS.dragging) return;

    if(NULL  != THIS.valueChangedCallback) {
	XmGaugeCallbackStruct call;
	call.reason = XmCR_VALUE_CHANGED;
	call.event = e;
	call.value = THIS.value;
	XtCallCallbacks(w, XmNvalueChangedCallback, &call);
    }
    THIS.dragging = False;
#undef THIS    
}


void
XmGaugeSetValue(Widget w, int value)
{
    XmGaugeWidget gw = (XmGaugeWidget)w;
    int clear = (gw->gauge.value > value);

    gw->gauge.value = value;
    DrawSlider(gw, clear);
    /* XFlush(XtDisplay(w)); */
}

void
XmGaugeSetMax(Widget w, int value)
{
    XmGaugeWidget gw = (XmGaugeWidget)w;
    int clear;

    if (value < 1) value = 1;

    clear = (gw->gauge.maximum < value);

    gw->gauge.maximum = value;
    if (gw->gauge.value > value)
     gw->gauge.value = value;
    DrawSlider(gw, clear);
    /* XFlush(XtDisplay(w)); */
}

int
XmGaugeGetValue(Widget w)
{    
    XmGaugeWidget gw = (XmGaugeWidget)w;

    return gw->gauge.value;
}
