/*
 * File:	wx_itemp.h
 * Purpose:	Panel item private declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

#ifndef wx_itemph
#define wx_itemph

/* When implementing a new item, be sure to:
 *
 * - add the item to the parent panel
 * - set window_parent to the parent
 * - NULL any extra child window pointers not created for this item
 *   (e.g. label control that wasn't needed)
 * - delete any extra child windows in the destructor (e.g. label control)
 * - implement GetSize and SetSize
 * - to find panel position if coordinates are (-1, -1), use GetPosition
 * - call AdvanceCursor after creation, for panel layout mechanism.
 *
 */

#include "wx_item.h"
#include "wx_privt.h"
#include "wx_utils.h"

 /*
  * If we have Fafa lib, include header.
  * else, force Windows Look
  */

#if FAFA_LIB
#include "fafa.h"
#include "fafapriv.h" //added by Chubraev 
#endif

#if CTL3D
#include <ctl3d.h>
#endif

/*
 * Decide what window classes we're going to use
 * for this combination of CTl3D/FAFA settings
 */
 
#define STATIC_CLASS     "STATIC"
#define STATIC_FLAGS     (SS_LEFT|WS_CHILD|WS_VISIBLE)
#define CHECK_CLASS      "wxBUTTON"
#define CHECK_FLAGS      (BS_AUTOCHECKBOX|WS_TABSTOP|WS_CHILD)
#define RADIO_CLASS      "wxBUTTON"
#define RADIO_FLAGS      (BS_RADIOBUTTON|WS_CHILD|WS_VISIBLE)
#define RADIO_SIZE       20
#define PURE_WINDOWS
#define GROUP_CLASS      "wxBUTTON"
#define GROUP_FLAGS      (BS_GROUPBOX|WS_CHILD|WS_VISIBLE)

#define BITCHECK_FLAGS   (FB_BITMAP|FC_BUTTONDRAW|FC_DEFAULT|WS_VISIBLE)
#define BITRADIO_FLAGS   (FC_BUTTONDRAW|FB_BITMAP|FC_RADIO|WS_CHILD|WS_VISIBLE)

#define MEANING_CHARACTER '0'
#define EDIT_CONTROL_FACTOR (15.0/10.0)
                                        // Scale font to get edit control height
#if !defined(APIENTRY)	// NT defines APIENTRY, 3.x not
#define APIENTRY FAR PASCAL
#endif
 
#ifdef WIN32
#define _EXPORT /**/
#else
#define _EXPORT _export
typedef signed short int SHORT ;
#endif

// Generic subclass proc, for panel item moving/sizing and intercept
// EDIT control VK_RETURN messages
extern LONG APIENTRY _EXPORT
  wxSubclassedGenericControlProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

// Find maximum size of window/rectangle
extern void wxFindMaxSize(HWND hwnd, RECT *rect);

// List of controls
#if !WXGARBAGE_COLLECTION_ON /* MATTHEW: GC */
extern wxList *wxControlHandleList;
#else
extern wxNonlockingHashTable *wxControlHandleList;
#endif
// List of scrollbar controls
extern wxList wxScrollBarList;
// The MakeProcInstance version of the function wxSubclassedGenericControlProc
extern FARPROC wxGenericControlSubClassProc;
extern char *wxBuffer;
extern HINSTANCE wxhInstance;

wxItem *wxFindControlFromHandle(HWND hWnd);
void wxAddControlHandle(HWND hWnd, wxItem *item);
void wxRemoveControlHandle(HWND hWnd);

#endif
