#
# 1.0 (Feb 1995)
#

@class XfwfEnforcer (XfwfBoard) @file=xwEnforcer

@ The Enforcer widget can be used to apply location resources to a
widget that is not a subclass of XfwfBoard. The Widget accepts a
single child and forces that child to the same size as itself (minus
the frame).

It can also be used to put a frame around some widget and to add a label.

@PUBLIC

@ There are two ways the geometry of the child may be handled. First
the geometry may be forced to fit to the Enforcer, or second the Enforcer
may shrink to fit around the child.

	@var Boolean shrinkToFit = FALSE

@ The default width and height are changed to 10 x 10.

	@var float rel_width = <String> "0.0"
	@var float rel_height = <String> "0.0"
	@var Position abs_width = 10
	@var Position abs_height = 10

@ The label must be a single line. It is displayed superimposed on the
frame, in the upper lefthand corner. Currently, it must be simple
string in a single font.

	@var String label = NULL

@ The font for the label is set with |font|.

	@var <FontStruct> XFontStruct *font = <String> XtDefaultFont

@ The foreground color is the color used to draw the text.

	@var Pixel foreground = <String> XtDefaultForeground

@ The label can be aligned in the widget the following ways:
|XfwfTop|, |XfwfTopLeft| (i.e. left label aligned at top), and
|XfwfLeft| (i.e. left label aligned at center).

	@var Alignment alignment = XfwfTop

@ The |propagateTarget| will receive the key events that occure by
traversal.

	@var <PropagateTarget> Widget propagateTarget = 0

        @var Boolean drawgray = FALSE

@ACTIONS

@ The |propagateKey| sends the key event to a |propagateTarget|.

@proc propagateKey
{
#if 0
    if ($propagateTarget && $travMode)
      if (event->xkey.state & ControlMask)
	$travMode = 0;

    if (!$propagateTarget || $travMode)
      XtCallActionProc($, "checkTraverse", event, NULL, 0);
#endif
    
    if ($propagateTarget /* && !$travMode */) {
	event->xkey.display	= XtDisplay($propagateTarget);
	event->xkey.send_event	= True;
	event->xkey.window	= XtWindow($propagateTarget);
	XSendEvent(XtDisplay($propagateTarget), XtWindow($propagateTarget),
		   FALSE, KeyPressMask | KeyReleaseMask, event);
    }
}

@PRIVATE

@ |labelWidth| and |labelHeight| are stored for faster access.

	@var Dimension labelWidth

	@var Dimension labelHeight

@ The GC is used for the text.

	@var GC textgc
	@var GC graygc

@METHODS

@ The |initialize| method initializes the private variables.

@proc initialize
{
    if (propagate_trans == NULL)
        propagate_trans = XtParseTranslationTable(propagateTranslation);

    XtAugmentTranslations($, propagate_trans);

    if ($label)
      $label = XtNewString($label);
    $textgc = NULL;
    $graygc = NULL;
    /* make_textgc($); - On demand */
    compute_label_size($);
}

@proc destroy
{
  if ($textgc) XtReleaseGC($, $textgc); $textgc = NULL;
  if ($graygc) XtReleaseGC($, $graygc); $graygc = NULL;
}

@ The |set_values| method has to deal with changes in |label|, |font|
or |foreground|.

@proc set_values
{
    Boolean need_redraw = False;

    if (($background_pixel != $old$background_pixel) && $graygc)
	make_graygc($);

    if ($old$label != $label) {
	if ($old$label)
	  XtFree($old$label);
	if ($label)
	  $label = XtNewString($label);
	need_redraw = True;
    }
    if ($font != $old$font || $foreground != $old$foreground) {
	if ($textgc) make_textgc($);
	if ($label != NULL)
	    need_redraw = True;
    }
    if ($label != $old$label || $font != $old$font)
	compute_label_size($);

    /* adjust board abs variables */
    if ($width != $old$width)
	$abs_width = $width;
    if ($height != $old$height)
	$abs_height = $height;

    if ($label && ($drawgray != $old$drawgray))
      need_redraw = True;

    return need_redraw;
}

@ The |expose| method first calls the |expose| method of its
superclass -- which basically just draws the frame -- and then adds
the label to it.

@proc _expose
{
    int w, h;
    Position x, y;

    if (! XtIsRealized($)) return;
    #_expose($, event, region);
    if ($label) {
	if (!$textgc) make_textgc($);
	$compute_inside($, &x, &y, &w, &h);

	w = max(0, w);
	h = max(0, h);
	
	switch ($alignment) {
	case XfwfTop:
	  XfwfDrawImageString(XtDisplay($), XtWindow($), $textgc,
			      x, $font->ascent,
			      $label, strlen($label), NULL, $font);
	  break;
	case XfwfTopLeft:
	  XfwfDrawImageString(XtDisplay($), XtWindow($), $textgc,
			      0, y+$font->ascent,
			      $label, strlen($label), NULL, $font);
	  break;
	case XfwfLeft:
	  XfwfDrawImageString(XtDisplay($), XtWindow($), $textgc,
			      0, y+(h-$labelHeight)/2+$font->ascent,
			      $label, strlen($label), NULL, $font);
	  break;
	}

	if ($drawgray) {
	  if (!$graygc) make_graygc($);
	  XFillRectangle(XtDisplay($), XtWindow($), $graygc, 
			 0, y, w + x, h);
	}
    }
}

@ The |resize| method passes on the resize message to its child, after
decreasing the area by the amount needed for the frame.

@proc resize
{
    Position x, y;
    int w, h;
    Widget child;

    if ($num_children == 0) return;
    $compute_inside($, &x, &y, &w, &h);
    child = $children[0];
    w -= 2 * $child$border_width;
    h -= 2 * $child$border_width;
    XtConfigureWidget(child, x, y, max(1, w), max(1, h), $child$border_width);
}

@ The |insert_child| method is called, when a child is inserted
in the |children| list. If |shrinkToFit| is true the enforcer
widget has to be resized to fit around the frame.

@proc insert_child
{
    #insert_child(child);

    if (child == $children[0] && $shrinkToFit) {
	Position x, y; int w, h, cw;

	$compute_inside($, &x, &y, &w, &h);
	if ($alignment == XfwfTop)
	  cw = max($child$width, $labelWidth);
	else
	  cw = max(1, $child$width);
	w = cw + 2*$child$border_width + $width - w;
	h = $height - h + $child$height + 2*$child$border_width;
	XtVaSetValues($, XtNwidth, max(1, w), XtNheight, max(1, h), NULL);
    }
}

@ The |change_managed| method is called when a child becomes managed
or unmanaged. The task of the routine is enforcing the layout policy,
which in this case consists of resizing the child to fit inside the
frame, or the frame around the child.

@proc change_managed
{
    Widget child;
    Position x, y; int w, h;

    if ($num_children == 0) return;
    $compute_inside($, &x, &y, &w, &h);
    child = $children[0];

    if ($shrinkToFit) {
	int selfw, selfh, cw;

	if ($alignment == XfwfTop)
	  cw = max($child$width, $labelWidth);
	else
	  cw = max(1, $child$width);

	selfw = $width  - w + cw  + 2*$child$border_width;
	selfh = $height - h + $child$height + 2*$child$border_width;

	XtVaSetValues($, XtNwidth, max(1, selfw), XtNheight, max(1, selfh), NULL);
	$compute_inside($, &x, &y, &w, &h);
    } else  {
	w -= 2 * $child$border_width;
	h -= 2 * $child$border_width;
    }
    
    XtConfigureWidget(child, x, y, max(1, w), max(1, h), $child$border_width);
}

@ If a child requests to be resized, the request is always ignored, or if 
|shrinkToFit| is TRUE, the enforcer resizes to fit.

@proc geometry_manager
{
    if ($shrinkToFit) {
	Position x, y; int w, h;

	/* ask parent to resize (granted because parent is a Board Widget) */
	$compute_inside($, &x, &y, &w, &h);
	if (request->request_mode & CWWidth) {
	    Dimension cw;

	    if ($alignment == XfwfTop)
	      cw = max(request->width, $labelWidth);
	    else
	      cw = max(1, request->width);

	    w = $width  - w + cw;
	    XtVaSetValues($, XtNwidth, max(1, w), NULL);
	}
	if (request->request_mode & CWHeight) {
	  h = $height - h + request->height;
	  XtVaSetValues($, XtNheight, max(1, h), NULL);
	}
	$compute_inside($, &x, &y, &w, &h);
	XtConfigureWidget(child, x, y, max(1, w), max(1, h), $child$border_width);

	return XtGeometryDone;
    }
    return XtGeometryNo;
}

@ The method |compute_inside| is re-defined. The method now leaves
place for the label.

@proc compute_inside
{
    #compute_inside($, x, y, w, h);
    /* change sizes to have enough space for the label */
    if ($label) {
	switch ($alignment) {
	case XfwfTop:
	    *y += $labelHeight + $highlightThickness;
	    *h -= $labelHeight + $highlightThickness;
	    break;
	case XfwfLeft:
	case XfwfTopLeft:
	    *x += $labelWidth + $highlightThickness;
	    *w -= $labelWidth + $highlightThickness;
	    break;
	}
    }
}

@ The highlight and unhighlight methods have to be overriden to skip the
label.

@proc highlight_border
{
    XRectangle  rect[4];
    Position    x, y;
    int   w, h, t;

    if ($highlightThickness == 0) return;

    $compute_inside($, &x, &y, &w, &h);
    x -= $total_frame_width($);
    y -= $total_frame_width($);
    w += 2 * $total_frame_width($);
    h += 2 * $total_frame_width($);

    w = max(0, w);
    h = max(0, h);
    t = 1 /*$highlightThickness */;

    rect[0].x = x;
    rect[0].y = y;
    rect[0].width = w;
    rect[0].height = t;

    rect[1].x = x;
    rect[1].y = y;
    rect[1].width = t;
    rect[1].height = h;

    rect[2].x = $width - t;
    rect[2].y = y;
    rect[2].width = t;
    rect[2].height = h;

    rect[3].x = x;
    rect[3].y = $height - t;
    rect[3].width = w;
    rect[3].height = t;

    if (!$bordergc) create_bordergc($);
    XFillRectangles(XtDisplay($), XtWindow($), $bordergc, &rect[0], 4);
}

@proc unhighlight_border
{
    Position   x, y;
    int  w, h;

    if ($highlightThickness == 0) return;

    $compute_inside($, &x, &y, &w, &h);
    x -= $total_frame_width($);
    y -= $total_frame_width($);
    w += 2 * $total_frame_width($);
    h += 2 * $total_frame_width($);

    w = max(w, 0);
    h = max(h, 0);

    XClearArea(XtDisplay($), XtWindow($), 
               x, y, w, $highlightThickness, False);
    XClearArea(XtDisplay($), XtWindow($),
               x, y, $highlightThickness, h, False);
    XClearArea(XtDisplay($), XtWindow($),
               $width - $highlightThickness, y, 
               $highlightThickness, h, False);
    XClearArea(XtDisplay($), XtWindow($),
               x, $height - $highlightThickness,
               w, $highlightThickness, False);
}

@UTILITIES

@var char propagateTranslation[] = "<KeyPress> : propagateKey() \n <KeyRelease> : propagateKey()";
@var XtTranslations propagate_trans = NULL;

@ The |compute_label_size| routine computes width and height of label.

@proc compute_label_size($)
{
    int direction, ascent, descent;
    XCharStruct overall;

    if ($label) {
	int len = strlen($label);
	XTextExtents($font, $label, len,
		     &direction, &ascent, &descent, &overall);
	$labelWidth  = XfwfTextWidth($font, $label, len, NULL);
	$labelHeight = ascent + descent;
    } else {
	$labelWidth = $labelHeight = 0;
    }
}

@ The |make_textgc| routine creates the GC for the text. 

@proc make_textgc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($textgc != NULL) XtReleaseGC($, $textgc);
    values.background = $background_pixel;
    values.foreground = $foreground;
    values.font = $font->fid;
    mask = GCFont | GCBackground | GCForeground;
    $textgc = XtGetGC($, mask, &values);
}

@proc make_graygc($)
{
    XtGCMask mask;
    XGCValues values;

    if ($graygc != NULL) XtReleaseGC($, $graygc);
    values.foreground = $background_pixel;
    values.stipple = GetGray($);
    values.fill_style = FillStippled;
    mask = GCForeground | GCStipple | GCFillStyle;
    $graygc = XtGetGC($, mask, &values);
}

@IMPORTS

@incl <stdio.h>
@incl "xwTabString.h"

