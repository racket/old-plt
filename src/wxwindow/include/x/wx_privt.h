/*
 * File:	wx_privt.h
 * Purpose:	Private class declarations.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wx_privt.h	1.2 5/9/94" */

#ifndef wx_privth
#define wx_privth

#ifdef __GNUG__
#pragma interface
#endif

#define MEANING_CHARACTER	'0'
// See notes in wx_item.cc
char *fillCopy (const char *str);

#ifdef wx_motif
#include <Xm/Xm.h>
#include "wx_hash.h"
class wxColour;

/* MATTHEW: GC requires differe tabel type: */
#if WXGARBAGE_COLLECTION_ON
extern wxNonlockingHashTable *wxWidgetHashTable;
#else
extern wxHashTable *wxWidgetHashTable;
#endif

extern void wxWidgetResizeProc(Widget w, XConfigureEvent *event, String args[], int *num_args);
extern XmString wxFindAcceleratorText(char *s) ;
/* MATTHEW: [4] Need display parameter */
extern int wxComputeColors (Display *display, wxColour * backColour, wxColour * buttonColour);
extern char *wxFindAccelerator(char *s) ;
// Find the letter corresponding to the mnemonic, for Motif
extern char wxFindMnemonic(char *s);
// Translates from an X event to a wxWindows mouse event
Bool wxTranslateMouseEvent(wxMouseEvent& wxevent, wxWindow *win, XEvent *xevent);
#endif

#define	wxNO_COLORS   0x00
#define wxBACK_COLORS 0x01
#define wxFORE_COLORS 0x02

#ifdef wx_motif
extern XColor itemColors[5] ;

#define wxBACK_INDEX 0
#define wxFORE_INDEX 1
#define wxSELE_INDEX 2
#define wxTOPS_INDEX 3
#define wxBOTS_INDEX 4
#endif

int CharCodeXToWX(KeySym keySym);
KeySym CharCodeWXToX(int id);


#endif // wx_privth

