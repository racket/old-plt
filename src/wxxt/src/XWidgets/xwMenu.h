/***********************************************************
Copyright 1995 by Markus Holzem

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#ifndef _Menu_h
#define _Menu_h

#include <X11/Xmu/Converters.h>
#include <X11/Core.h>

/****************************************************************
 *
 * Menu widgets
 *
 ****************************************************************/

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------

 cursor		     Cursor		Cursor		right_ptr
 font                Font               XFontStruct*    XtDefaultFont
 foreground          Foreground         Pixel           XtDefaultForeground
 hMargin             HMargin            Dimension       1
 horizontal          Horizontal         Boolean         True
 menu                Menu               Pointer         NULL
 onNewItem           Callback           XtCallbackList  NULL
 onSelect            Callback           XtCallbackList  NULL
 spacing             Spacing            Dimension       4
 indicatorPixel      IndicatorPixel     Pixel           dynamic
 indicatorContrast   IndicatorContrast  Int             85%
 indicatorSize       IndicatorSize      Dimension       dynamic
 vMargin             VMargin            Dimension       2

 * Resources extracted from ThreeD

 shadowWidth	      ShadowWidth          Dimension    2
 beNiceToColormap     BeNiceToColormap     Boolean      False
 topShadowPixel       TopShadowPixel       Pixel        dynamic
 bottomShadowPixel    BottomShadowPixel    Pixel        dynamic
 topShadowContrast    TopShadowContrast    Int          120%
 bottomShadowContrast BottomShadowContrast Int          60%

*/

#define XtNhMargin              "hMargin"
#define XtCHMargin              "HMargin"
#define XtNhorizontal           "horizontal"
#define XtCHorizontal           "Horizontal"
#define XtNmenu                 "menu"
#define XtCMenu                 "Menu"
#define XtNrefresh              "refresh"
#define XtCRefresh              "Refresh"
#define XtNonNewItem            "onNewItem"
#define XtNonSelect             "onSelect"
#define XtNonNoSelect           "onNoSelect"
#define XtNspacing              "spacing"
#define XtCSpacing              "Spacing"
#define XtNvMargin              "vMargin"
#define XtCVMargin              "hMargin"

#define XtNindicatorPixel       "indicatorPixel"
#define XtCIndicatorPixel       "IndicatorPixel"
#define XtNindicatorContrast    "indicatorContrast"
#define XtCIndicatorContrast    "IndicatorContrast"
#define XtNindicatorSize        "indicatorSize"
#define XtCIndicatorSize        "IndicatorSize"

#define XtNcursor               "cursor"

#define XtNshadowWidth          "shadowWidth"
#define XtCShadowWidth          "ShadowWidth"
#define XtNbeNiceToColormap     "beNiceToColormap"
#define XtCBeNiceToColormap     "BeNiceToColormap"
#define XtNtopShadowPixel       "topShadowPixel"
#define XtCTopShadowPixel       "TopShadowPixel"
#define XtNtopShadowContrast    "topShadowContrast"
#define XtCTopShadowContrast    "TopShadowContrast"
#define XtNbottomShadowPixel    "bottomShadowPixel"
#define XtCBottomShadowPixel    "BottomShadowPixel"
#define XtNbottomShadowContrast "bottomShadowContrast"
#define XtCBottomShadowContrast "BottomShadowContrast"

typedef struct _MenuClassRec  *MenuWidgetClass;
typedef struct _MenuRec	      *MenuWidget;

extern WidgetClass menuWidgetClass;

/* Data Types for Menu Structure */

typedef enum _e_menu_item_type {
    MENU_TEXT      = 0,		/* Label item */
    MENU_BUTTON    = 1,		/* selectable item */
    MENU_RADIO     = 2,		/* radio item (diamond decoration) */
    MENU_TOGGLE    = 3,		/* toggle item (square decoration) */
    MENU_CASCADE   = 4,		/* submenu item */
    MENU_SEPARATOR = 5,		/* separator item */
    MENU_PUSHRIGHT = 6,		/* if in menubar, pushright follonwing items */
    MENU_HELP      = 7          /* CASCADE + PUSHRIGHT */
} menu_item_type;

typedef enum _e_Subresource {
    SUBRESOURCE_LABEL = 0,
    SUBRESOURCE_HELP = 1,
    SUBRESOURCE_KEY = 2
} Subresource;

/* one menu item */
typedef struct _menu_item {
    /* public data */
    char              *label;
    char              *key_binding;
    char              *help_text;
    long              ID;
    menu_item_type    type;
    Boolean           enabled;
    Boolean           set;	  /* used for toggles and radios */
    struct _menu_item *contents;  /* pointer to submenu */
    struct _menu_item *next;	  /* pointer to next menu item same level */
    struct _menu_item *prev;	  /* pointer to prev menu item same level */
    void              *user_data; /* data associated with menu item */
    /* private data */
    Position          start, end;       /* start and end of item in window */
} menu_item;

_XFUNCPROTOBEGIN

void Xaw3dPopupMenu(
#if NeedFunctionPrototypes
    MenuWidget,			/* Menu widget to pop up*/
    Widget			/* Widget calling Xaw3dPopupMenu */
#endif
);

void Xaw3dPopupMenuAtPos(
#if NeedFunctionPrototypes
    MenuWidget,			/* Menu widget to pop up*/
    int, int			/* root position of popup menu */
#endif
);

char *ResourcedText(
#if NeedFunctionPrototypes
    MenuWidget,			/* Menu Widget */
    menu_item*,			/* selected item */
    Subresource			/* wanted subresource */
#endif
);

_XFUNCPROTOEND

#endif /* _Menu_h */
