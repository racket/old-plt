

@CLASS XfwfArrow (XfwfBoard)  @file = xwArrow

@ The Arrow widget is usually part of a composite sceft or right, depending on the
|direction| resource. It has a single callback, that is repeatedly
called as long as a mouse button -- button 1 by default -- is pressed.

The triangle has a 3D shadow, the size of which can be controlled with
a resource. The shadow is either stippled or colored, depending on the
|shadowScheme| and associated resources (see the XfwfFrame widget).

@PUBLIC

@ The direction of the arrow (triangle) is given by the |direction|
resource, which is of type |Alignment|. Only |"top"| (|XfwfTop|),
|"bottom"| (|XfwfBottom|), |"left"| (|XfwfLeft|) and |"right"|
(|XfwfRight|) are valid directions. Other directions result in a
warning.

@var Alignment direction = XfwfTop

@ The color of the arrow also determines the color of the 3D shadow,
at least if |shadowScheme| is set to |XfwfAuto|, as it is by default.

@var Pixel foreground = <String> XtDefaultBackground

@ The width of the arrow's shadow is by default 2 pixels.

@var Dimension arrowShadow = 2

@ The action should be usually repeated.

@var Boolean repeat = TRUE

@ When the user presses and then holds the mouse button, the action
function waits some milliseconds before it starts repeating the
callbacks.

@var Cardinal initialDelay = 500

@ Between repeated calls to the callback routines, the arrow widget
will wait a few milliseconds.

@var Cardinal repeatDelay = 200

@ The |callback| function is called by the |activate| action. It is
called repeatedly until the mouse button that triggered the action is
released again.

@var <Callback> XtCallbackList callback = NULL

@PRIVATE

@ The three GC's are used for drawing the arrow and its shadows.

@var GC arrowgc
@var GC arrowlightgc
@var GC arrowdarkgc

@ The repeating callback is implemented with a time out routine. The
timer is a private variable of the widget.

@var long timer

@ The arrow is drawn as large as possible. The arrow is actually a triangle
with 3D shadows. |p1| is the triangle itself, |p2|, |p3| and |p4| are the
shadows.

@var  XPoint    p1[3]
@var  XPoint    p2[4]
@var  XPoint    p3[4]
@var  XPoint    p4[4]
@var  Dimension a2
@var  Dimension a3
  
@METHODS

@ The |initialize| method sets initial values for the three GC's and
checks the |direction| resource.

@proc initialize
{
    if ($direction != XfwfTop && $direction != XfwfLeft
	&& $direction != XfwfRight && $direction != XfwfBottom) {
	XtWarning("direction of Arrow widget incorrect; set to `top'");
	$direction = XfwfTop;
    }
    $arrowgc = NULL; create_arrowgc($);
    $arrowlightgc = NULL; create_arrowlightgc($);
    $arrowdarkgc = NULL; create_arrowdarkgc($);
    $timer = 0;
}

@proc destroy
{
   if ($timer) {
      if ($timer != 0x1)
        wxRemoveTimeOut($timer);
      $timer = 0;
   }
   if ($arrowgc) XtReleaseGC($, $arrowgc); $arrowgc = NULL;
   if ($arrowlightgc) XtReleaseGC($, $arrowlightgc); $arrowlightgc = NULL;
   if ($arrowdarkgc) XtReleaseGC($, $arrowdarkgc); $arrowdarkgc = NULL;   
}

@ When the |foreground|, |arrowShadow| or |direction| resource changes,
the widget has to be redrawn. Like in the |initialize| method, the
|direction| resource needs to be checked for valid values.

If the inherited resource |shadowScheme| or one of its family changes, new
GC's need to be created.

@proc set_values
{
    Boolean need_redisplay = False;

    if ($direction != XfwfTop && $direction != XfwfLeft
	&& $direction != XfwfRight && $direction != XfwfBottom) {
	XtWarning("direction of Arrow widget incorrect; set to `top'");
	$direction = XfwfTop;
    }
    if ($old$direction != $direction)
	need_redisplay = True;
    if ($old$foreground != $foreground) {
	create_arrowgc($);
	need_redisplay = True;
    }
    if ($old$arrowShadow != $arrowShadow)
	need_redisplay = True;
    if ($shadowScheme != $old$shadowScheme) {
	create_arrowdarkgc($);
	create_arrowlightgc($);
	need_redisplay = True;
    } else if ($shadowScheme == XfwfColor) {
	if ($topShadowColor != $old$topShadowColor) {
	    create_arrowlightgc($);
	    need_redisplay = True;
	}
	if ($bottomShadowColor != $old$bottomShadowColor) {
	    create_arrowdarkgc($);
	    need_redisplay = True;
	}
    } else if ($shadowScheme == XfwfStipple) {
	if ($topShadowStipple != $old$topShadowStipple) {
	    create_arrowlightgc($);
	    need_redisplay = True;
	}
	if ($bottomShadowStipple != $old$bottomShadowStipple) {
	    create_arrowdarkgc($);
	    need_redisplay = True;
	}
    }
    return need_redisplay;
}

@proc _expose
{
    Position x, y;
    int  width, height;
    Dimension a, a2, a3;

    assert($direction == XfwfTop || $direction == XfwfLeft
	   || $direction == XfwfRight || $direction == XfwfBottom);

    if (! XtIsRealized($)) return;
    if (region != NULL) {
	XSetRegion(XtDisplay($), $arrowgc, region);
	XSetRegion(XtDisplay($), $arrowlightgc, region);
	XSetRegion(XtDisplay($), $arrowdarkgc, region);
    }
    $compute_inside($, &x, &y, &width, &height);
    width = max(1, width);
    height = max(1, height);
    a = $arrowShadow;
    switch ($direction) {
    case XfwfTop:
	$a2 = a2 = (1.0 + 0.71*(float)width/(float)height) * a;
	$a3 = a3 = (1.0 + 0.83*(float)height/(float)width) * a;

	$p1[0].x = x + width/2;           $p1[0].y = y + a3;
	$p1[1].x = x + $a2;	          $p1[1].y = y + height - a;
	$p1[2].x = x + width - a2;	  $p1[2].y = y + height - a;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);
	if (a == 0) break;
	$p2[0].x = x + width/2;	          $p2[0].y = y;
	$p2[1].x = x + width/2;	          $p2[1].y = y + a3;
	$p2[2].x = x + width - a2;	  $p2[2].y = y + height - a;
	$p2[3].x = x + width;		  $p2[3].y = y + height;

	$p3[0].x = x + a2;		  $p3[0].y = y + height - a;
	$p3[1].x = x;			  $p3[1].y = y + height;
	$p3[2].x = x + width;		  $p3[2].y = y + height;
	$p3[3].x = x + width - a2;	  $p3[3].y = y + height - a;

	$p4[0].x = x + width/2;	          $p4[0].y = y;
	$p4[1].x = x;			  $p4[1].y = y + height;
	$p4[2].x = x + a2;		  $p4[2].y = y + height - a;
	$p4[3].x = x + width/2;	          $p4[3].y = y + a3;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfLeft:
	a2 = (1.0 + 0.83*width/height) * a;
	a3 = (1.0 + 0.71*height/width) * a;
	$p1[0].x = x + a2;		  $p1[0].y = y + height/2;
	$p1[1].x = x + width - a;	  $p1[1].y = y + a3;
	$p1[2].x = x + width - a;	  $p1[2].y = y + height - a3;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);
	if ($arrowShadow == 0) break;
	$p2[0].x = x + width;		  $p2[0].y = y;
	$p2[1].x = x;			  $p2[1].y = y + height/2;
	$p2[2].x = x + a2;		  $p2[2].y = y + height/2;
	$p2[3].x = x + width - a;	  $p2[3].y = y + a3;

	$p3[0].x = x;			  $p3[0].y = y + height/2;
	$p3[1].x = x + width;		  $p3[1].y = y + height;
	$p3[2].x = x + width - a;	  $p3[2].y = y + height - a3;
	$p3[3].x = x + a2;		  $p3[3].y = y + height/2;

	$p4[0].x = x + width;		  $p4[0].y = y;
	$p4[1].x = x + width - a;	  $p4[1].y = y + a3;
	$p4[2].x = x + width - a;	  $p4[2].y = y + height - a3;
	$p4[3].x = x + width;		  $p4[3].y = y + height;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfBottom:
	a2 = (1.0 + 0.71*width/height) * a;
	a3 = (1.0 + 0.83*height/width) * a;
	$p1[0].x = x + width/2;	          $p1[0].y = y + height - a3;
	$p1[1].x = x + a2;		  $p1[1].y = y + a;
	$p1[2].x = x + width - a2;	  $p1[2].y = y + a;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);
	if ($arrowShadow == 0) break;
	$p2[0].x = x;			  $p2[0].y = y;
	$p2[1].x = x + width/2;	          $p2[1].y = y + height;
	$p2[2].x = x + width/2;	          $p2[2].y = y + height - a3;
	$p2[3].x = x + a2;		  $p2[3].y = y + a;

	$p3[0].x = x + width;		  $p3[0].y = y;
	$p3[1].x = x + width - a2;	  $p3[1].y = y + a;
	$p3[2].x = x + width/2;	          $p3[2].y = y + height - a3;
	$p3[3].x = x + width/2;	          $p3[3].y = y + height;

	$p4[0].x = x;			  $p4[0].y = y;
	$p4[1].x = x + a2;		  $p4[1].y = y + a;
	$p4[2].x = x + width - a2;	  $p4[2].y = y + a;
	$p4[3].x = x + width;		  $p4[3].y = y;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfRight:
	a2 = (1.0 + 0.83*width/height) * a;
	a3 = (1.0 + 0.71*height/width) * a;
	$p1[0].x = x + width - a;	  $p1[0].y = y + height/2;
	$p1[1].x = x + a;		  $p1[1].y = y + a;
	$p1[2].x = x + a;		  $p1[2].y = y + height - a;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);
	if ($arrowShadow == 0) break;
	$p2[0].x = x;			  $p2[0].y = y + height;
	$p2[1].x = x + width;		  $p2[1].y = y + height/2;
	$p2[2].x = x + width - a2;	  $p2[2].y = y + height/2;
	$p2[3].x = x + a;		  $p2[3].y = y + height - a3;

	$p3[0].x = x;			  $p3[0].y = y;
	$p3[1].x = x + a;		  $p3[1].y = y + a3;
	$p3[2].x = x + width - a2;	  $p3[2].y = y + height/2;
	$p3[3].x = x + width;		  $p3[3].y = y + height/2;

	$p4[0].x = x;			  $p4[0].y = y;
	$p4[1].x = x;			  $p4[1].y = y + height;
	$p4[2].x = x + a;		  $p4[2].y = y + height - a3;
	$p4[3].x = x + a;		  $p4[3].y = y + a3;
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    }
    if (region != NULL) {
	XSetClipMask(XtDisplay($), $arrowgc, None);
	XSetClipMask(XtDisplay($), $arrowlightgc, None);
	XSetClipMask(XtDisplay($), $arrowdarkgc, None);
    }
}

@TRANSLATIONS

@trans <Btn1Down>: push_down() activate_and_start_timer()
@trans <Btn1Up>: push_up() stop_timer()

@ACTIONS

@ The |activate| action calls the |callback| routine once and installs
a timeout routine.

@proc activate_and_start_timer
{
    if (event->type != ButtonPress) {
        XtWarning("The Arrow activate action isn't bound to a BtnDown event");
	return;
    }
    stop_timer($, event, params, num_params);
    $timer = 0x1; /* During callback, a stop_timer may be evaluated */
    XtCallCallbackList($, $callback, NULL);
    if ($repeat) {
      if ($timer) {
        stop_timer($, event, params, num_params);
	$timer = wxAppAddTimeOut(XtWidgetToApplicationContext($),
				 $initialDelay, timer_callback, $);
      }
    } else
	push_up($, event, params, num_params);
}

@proc stop_timer
{
    if ($timer && $timer != 0x1)
	wxRemoveTimeOut($timer);
    $timer = 0;
}


@proc push_up
{

  switch ($direction) {
    case XfwfTop:
	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfLeft:

	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfBottom:

	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfRight:

	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
      }
}

@proc push_down
{

  switch ($direction) {
    case XfwfTop:
	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfLeft:

	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfBottom:

	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
    case XfwfRight:

	XFillPolygon(XtDisplay($), XtWindow($), $arrowgc, $p1, 3, Convex,
		     CoordModeOrigin);

	if ($arrowShadow == 0) break;

	XFillPolygon(XtDisplay($), XtWindow($), $arrowlightgc, $p2, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p3, 4, Convex,
		     CoordModeOrigin);
	XFillPolygon(XtDisplay($), XtWindow($), $arrowdarkgc, $p4, 4, Convex,
		     CoordModeOrigin);
	break;
      }
}

@UTILITIES

@ The time-out calls the |timer_callback| routine.  The routine
re-installs the time-out and calls the |callback| function (but in the
reverse order, because we do not want time-outs to overtake each
other). The delay is now |repeatDelay| instead of |initialDelay|.

@proc timer_callback(XtPointer client_data, XtIntervalId *timer)
{
    Widget $ = (Widget) client_data;

    XtCallCallbackList($, $callback, NULL);
    if ($timer) { /* i.e., no stop issued by callback */
      if ($timer != 0x1)
        wxRemoveTimeOut($timer);
      $timer = wxAppAddTimeOut(XtWidgetToApplicationContext($),
				     $repeatDelay, timer_callback, $);
    }
}

@ The GC for the triangle is created by a utility function. It destroys the
old GC and then creates a new one, based on the |foreground| resource.

@proc create_arrowgc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($arrowgc != NULL) XtReleaseGC($, $arrowgc);
    mask = GCForeground;
    values.foreground = $foreground;
    $arrowgc = XtGetGC($, mask, &values);
}

@ The GC for the light shadow is dependent on the inherited |shadowScheme|
resource. It is the same routine as for the shadows in the XfwfFrame widget.

@proc create_arrowlightgc($)
{
    XtGCMask mask=0;
    XGCValues values;

    if ($arrowlightgc != NULL) XtReleaseGC($, $arrowlightgc);
    switch ($shadowScheme) {
    case XfwfColor:
	mask = GCForeground;
	values.foreground = $topShadowColor;
	break;
    case XfwfStipple:
	mask = GCFillStyle | GCStipple | GCForeground | GCBackground;
	values.fill_style = FillOpaqueStippled;
	values.background = $background_pixel;
	values.stipple = $topShadowStipple;
	values.foreground = WhitePixelOfScreen(XtScreen($));
	break;
    case XfwfBlack:
    case XfwfAuto:
	if (DefaultDepthOfScreen(XtScreen($)) > 4
	    && $lighter_color($, $foreground, &values.foreground)) {
	    mask = GCForeground;
	} else {
	    mask = GCFillStyle | GCBackground | GCForeground | GCStipple;
	    values.fill_style = FillOpaqueStippled;
	    /* values.background = $foreground; */
	    values.background = BlackPixelOfScreen(XtScreen($));
	    values.foreground = WhitePixelOfScreen(XtScreen($));
	    values.stipple =
		XCreateBitmapFromData(XtDisplay($),
				      RootWindowOfScreen(XtScreen($)),
				      gray_bits, gray_width, gray_height);
	}
	break;
    }
    $arrowlightgc = XtGetGC($, mask, &values);
}

@ The routine for the dark part of the shadow is analogous.

@proc create_arrowdarkgc($)
{
    XtGCMask mask=0;
    XGCValues values;

    if ($arrowdarkgc != NULL) XtReleaseGC($, $arrowdarkgc);
    switch ($shadowScheme) {
    case XfwfColor:
	mask = GCForeground;
	values.foreground = $bottomShadowColor;
	break;
    case XfwfStipple:
	mask = GCFillStyle | GCStipple | GCForeground | GCBackground;
	values.fill_style = FillOpaqueStippled;
	values.stipple = $bottomShadowStipple;
	values.foreground = BlackPixelOfScreen(XtScreen($));
	values.background = $background_pixel;
	break;
    case XfwfBlack:
    case XfwfAuto:
	if (DefaultDepthOfScreen(XtScreen($)) > 4
	    && $darker_color($, $foreground, &values.foreground)) {
	    mask = GCForeground;
	} else {
	    mask = GCFillStyle | GCBackground | GCForeground | GCStipple;
	    values.fill_style = FillOpaqueStippled;
	    /* values.background = $foreground; */
	    values.background = BlackPixelOfScreen(XtScreen($));
	    values.foreground = WhitePixelOfScreen(XtScreen($));
	    values.stipple =
		XCreateBitmapFromData(XtDisplay($),
				      RootWindowOfScreen(XtScreen($)),
				      gray_bits, gray_width, gray_height);
	}
	break;
    }
    $arrowdarkgc = XtGetGC($, mask, &values);
}

@IMPORTS

@ The stipple for the shadows are loaded from a bitmap file.

@incl <X11/bitmaps/gray>
@incl <stdio.h>
@incl <assert.h>
@incl <wxtimeout.h>
