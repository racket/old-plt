#
# 1.0 (Feb 1995)
#
# $Id: xwCanvas.w,v 1.1.1.1 1997/12/22 17:29:05 mflatt Exp $

@class XfwfCanvas (XfwfBoard) @file=xwCanvas

@ The Canvas widget is used as like the |core| widget. It adds only a
handling for the |backing_store| window attribute.

@PUBLIC

@ |backingStore| handles, if the server shall do backing store for this
widget.

	@var int backingStore = NotUseful

@METHODS

@ The |realize| changes the |backing_store| attribute of the realized window.

@proc realize
{
    if (($backingStore == Always)
    ||  ($backingStore == NotUseful)
    ||  ($backingStore == WhenMapped)) {
	*mask |= CWBackingStore;
	attributes->backing_store = $backingStore;
    } else {
	*mask &= ~CWBackingStore;
    }
    /* chain to parent method */
    #realize($, mask, attributes);
}

@ The |set_values| method has to deal with changes in |backing_store|.

@proc set_values
{
    if ($old$backingStore != $backingStore) {
	if (($backingStore == Always)
	||  ($backingStore == NotUseful)
	||  ($backingStore == WhenMapped)) {
	    XSetWindowAttributes attributes;
	    unsigned long	 mask = CWBackingStore;

	    attributes.backing_store = $backingStore;
	    XChangeWindowAttributes(XtDisplay($), XtWindow($), mask, &attributes);
	}
    }

    return FALSE; /* there is no need to redraw */
}
