# HScrollbar.w -- Horizontal scrollbar widget
# Bert Bos <bert@let.rug.nl>
# 18 Nov '92
# Version 1.0.1 for FWF V4.0 (see README for history)
# 
# $Id: HScrollb.w,v 1.1 1996/01/10 14:57:41 markus Exp $

@class XfwfHScrollbar (XfwfScrollbar)  @file=xwHScrollb

@ The |XfwfHScrollbar| widget is exactly the same as its superclass,
|XfwfScrollbar|, except for the default value of the |vertical|
resource and the fact that it has default translations for keyboard
events. It would be a simple matter to add these translations in a
resource file or in an application's source, but having them by
default is quite convenient.



@public

@var vertical = False



@translations

@ The action is already defined by the superclass, but by
default not used.

@trans Ctrl<Key>Left: Scroll("pageLeft")
@trans Ctrl<Key>Right: Scroll("pageRight")

@trans <Key>Left: Scroll("left")
@trans <Key>Right: Scroll("right")

@ Are |PageUp| and |PageDown| always available? Or only in OSF?
@ trans Ctrl<Key>PageUp: Scroll("pageLeft")
@ trans Ctrl<Key>PageDown: Scroll("pageRight")

@trans Shift<Key>Home: Scroll("rightSide")
@trans <Key>Home: Scroll("leftSide")
