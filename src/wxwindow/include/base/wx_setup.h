/*
 * File:	wx_setup.h
 * Purpose:	Window library configuration file. Note: you may need to edit
 *              the main makefile after you have edited this.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	June 1995
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wx_setuph
#define wx_setuph

#ifdef __GNUG__
#pragma interface
#endif

/*
 * General features
 *
 */
 

#define USE_POSTSCRIPT  1
                        	// 0 for no PostScript device context
#define USE_AFM_FOR_POSTSCRIPT 1
                                // 1 to use font metric files in GetTextExtent
#define USE_METAFILE    0
                                // 0 for no Metafile and metafile device context
#define USE_FORM        1
                                // 0 for no wxForm
#define USE_IPC         0
                                // 0 for no interprocess comms
// Note: wxHELP uses IPC under X so these are interdependent!
#define USE_HELP        0
                                // 0 for no help facility
#define USE_ENHANCED_DIALOG 0
                        	// 0 for no enhanced dialog
#define USE_RESOURCES   1
                        	// 0 for no wxGetResource/wxWriteResource
#define USE_CONSTRAINTS 0
                                // 0 for no window layout constraint system
#define USE_CLIPBOARD   1
                        	// 0 for no clipboard functions
#define USE_SPLINES     1
                            	// 0 for no splines
#define USE_XFIG_SPLINE_CODE  1
                           	// 1 for XFIG spline code, 0 for AIAI spline code.
// AIAI spline code is slower, but freer of copyright issues.
#define USE_DRAG_AND_DROP 0
                        	// 0 for no drag and drop

#define USE_TOOLBAR       0
                                    // Define 1 to use toolbar classes
#define USE_BUTTONBAR     0
                                    // Define 1 to use buttonbar classes (enhanced toolbar
                                    // for MS Windows)
#define USE_GAUGE       1
                                    // Define 1 to use Microsoft's gauge (Windows)
                                    // or Bull's gauge (Motif) library (both in contrib).
#define USE_EXTENDED_STATICS 0
                                    // Define 1 to compile contributed wxStatic.. classes
#define USE_VLBOX       0
                                    // Define 1 to compile contributed wxVirtListBox class
#define USE_SCROLLBAR   0
                                    // Define 1 to compile contributed wxScrollBar class
#define USE_XPM_IN_X    1
#define USE_XPM_IN_MSW  1
                                    // Define 1 to support the XPM package in wxBitmap,
                                    // separated by platform. If 1, you must link in
                                    // the XPM library to your applications.
#define USE_IMAGE_LOADING_IN_X          1
                                  // Use dynamic icon/bitmap loading/saving code in utils/image under X.
                                  // If this is 1, you will need to link your applications
                                  // with image_X.lib. where X is motif, ol, or hp.

#define USE_IMAGE_LOADING_IN_MSW        1
                                  // Use dynamic DIB loading/saving code in utils/dib under MSW.
                                  // If this is 1, you will need to link your applications
                                  // with dib.lib.
#define USE_RESOURCE_LOADING_IN_MSW     0
                                  // Use dynamic icon/cursor loading/saving code in utils/rcparser
                                  // under MSW.
                                  // If this is 1, you will need to link your applications
                                  // with rcparser.lib.
#define USE_WX_RESOURCES        0
                                  // Use .wxr resource mechanism (requires PrologIO library)

#define USE_GNU_WXSTRING        0
                                  // Define 1 to use modified GNU wxString class
                                  // from (stefan.hammes@urz.uni-heidelberg.de) in contrib\wxstring
#define HAVE_SOCKET             1
                                  // Use WinSock if 1                                 
#define USE_DOC_VIEW_ARCHITECTURE 0
                                  // Set to 0 to disable document/view architecture
#define USE_PRINTING_ARCHITECTURE  1
                                    // Set to 0 to disable print/preview architecture code
#define USE_POSTSCRIPT_ARCHITECTURE_IN_MSW 1
                                    // Set to 0 to disable PostScript print/preview architecture code
                                    // under Windows (just use Windows printing).
#define USE_DYNAMIC_CLASSES     0
                                  // If 1, enables provision of run-time type information.
                                  // This will have a small space overhead.
#define WX_USE_MEMORY_TRACING      0
                                  // If 1, enables debugging versions of wxObject::new and
                                  // wxObject::delete *IF* DEBUG is also defined.
                                  // WARNING: this code may not work with all architectures, especially
                                  // if alignment is an issue.
#define USE_GLOBAL_MEMORY_OPERATORS 0
                                  // In debug mode, cause new and delete to be redefined globally.
                                  // If this causes problems (e.g. link errors), set this to 0.
#define WXGARBAGE_COLLECTION_ON 1
                                  // If 1, enables the experimental garbage collection system.

#define REMOVE_UNUSED_ARG 1
                                  // Set this to 0 if your compiler can't cope
                                  // with omission of prototype parameters.

/*
 * Finer detail
 *
 */

/*
 * Motif and XView
 *
 */

#define WX_STANDARD_GRAPHICS 1
                                  // If 1, normalizes X drawing code to behave exactly as
                                  // as MSW. If 0, is compatible with existing applications.
                                  // Some Xlib drawing primitives have non-intuitive behaviour!

#define USE_GADGETS                1
                                    // More efficient to use gadgets for some
                                    // widgets in Motif. 0 for no gadgets.
                                    // Please note: there is no reason to not
                                    // use it except if you intend to modify
                                    // color of individuals items OR
                                    // you need to move panel items interactively
#define USE_BUTTON_GADGET          0
                                    // On JACS's system, gadget buttons
                                    // interfere with default button setting.
#define PIXEL0_DISABLE             0
                                    // Define as 1 to disallow allocation
                                    // of pixel #0 (wxXOR problem).
                                    // JACS - I found this caused problems.

#define MOTIF_MENUBAR_DELETE_FIX   0
                                    // On some systems (Ultrix, OSF), deleting a frame
                                    // from within a menu callback causes a crash.
                                    // Set to 1 to avoid deleting the menubar handle directly,
                                    // which seems to cure it.
#define	DEFAULT_FILE_SELECTOR_SIZE 0
				    // Let Motif defines the size of File
				    // Selector Box (if 1), or fix it to
				    // wxFSB_WIDTH x wxFSB_HEIGHT (if 0)
#define	wxFSB_WIDTH                600
#define wxFSB_HEIGHT               500

#define	MOTIF_MANAGE               1
				    // Control default style of Dialogs
				    // 1: use wxMOTIF_RESIZE as default
				    // 0: do not use wxMOTIF_RESIZE as default

#define USE_NOTICES                1
                                    // Under XView, use Notice package
                                    // for small messages instead of
                                    // home-grown dialog boxes


/*
 * MS Windows/Windows NT
 *
 */

	/*
	 * Special Note about FAFA_LIB, WINDOWS_LOOK, CTL3D
	 */
/****************************************************************************

These 3 #define control how graphic controls are managed. Not all combinations
are valid.

FAFA_LIB=0

	WINDOWS_LOOK is ignored, and forced to 1.
	CTL3D controls the 3D-rendering using ctl3d.lib
	All controls are Microsoft

FAFA_LIB=1

	CTL3D=1

		WINDOWS_LOOK is ignored, and forced to 1.
		3D-rendering is done via ctl3d.lib
		Only necessary fafa items are created [wxBitmapped'buttons]

	CTL3D=0

		WINDOWS_LOOK=1

			Idem above, but no 3D-rendering.

		WINDOWS_LOOK=0

			3D-rendering is done via fafa.lib.
			All controls are Fafa.

Recommended configuration:

FAFA_LIB=1 , CTL3D=1 , WINDOWS_LOOK=1

****************************************************************************/

#define ALS_CHOICE_SIZE	           1
                                    // Lets you choose one of the 2 forms for
                                    // wxChoice::SetSize
                                    // EXPERIMENTAL

#define EDITABLE_TEXT_WINDOW       0
                                    // Editable text window ONLY (a value of 1 compiles
                                    // OUT the large file support).
                                    // The editable text window uses
                                    // an EDIT control, but files must be < 64K
                                    // If 0, both types of window are allowed.
#define FAFA_LIB                   1
                                    // Define 1 for Fafa lib. Mandatory for pixmapped
                                    // buttons under Windows. If FAFA_LIB is 1
                                    // and CTL3D is 1, all controls except wxButton
                                    // will be CTL3D controls: wxButton will
                                    // be a FAFA button with optional bitmap.
#define USE_COMMON_DIALOGS         1
                                    // On rare occasions (e.g. using DJGPP) may want
                                    // to omit common dialogs
                                    // (e.g. file selector, printer dialog).
                                    // Switching this off also switches off
                                    // the printing architecture and interactive
                                    // wxPrinterDC.
#define USE_KEYBOARD_HOOK          1
                                    // Set to 0 to disable Windows keyboard hook.
                                    // May need to disable for WIN32 since is not tested
                                    // under WIN32, and may code may need to be put
                                    // in a DLL according to the SDK documentation.
#define USE_GREY_BACKGROUND        1
                                    // If 1, uses grey (gray!) panels
                                    // in FAFA and non-FAFA, non-CTL3D modes.
                                    // I (JACS) think the controls look better
                                    // this way. CTL3D always uses grey panels.

#define WINDOWS_LOOK		   1
				    // Define 1 to use as many as possible
				    // Microsoft controls.
#define USE_ITSY_BITSY             0
                                    // Define 1 to use Microsoft's IstyBitsy
                                    // small title bar library (in contrib)
#define USE_BITMAP_MESSAGE         1
                                    // Define 1 to use bitmap messages.
#define USE_ODBC                   0
                                    // Define 1 to use ODBC classes

                                 
/*
 * Any platform
 *
 */
#define ENHANCED_FONTS             1
                                    // Define 1 to have predefined fonts in
                                    // wxEnhDialogBox.
#define USE_PANEL_IN_PANEL         0
                                    // 1 to use panel in panel in common dialogs
                                    // Advantage: neater dialogs. Disadvantage:
				    // messes up default button and
				    // tab traversal.
#define USE_TYPEDEFS               0
                                    // Use typedefs not classes for wxPoint
				    // and others, to reduce overhead and avoid
				    // MS C7 memory bug. Bounds checker
				    // complains about deallocating
                                    // arrays of wxPoints if wxPoint is a class.



/*
 * DO NOT MODIFY THESE LINES!!
 */
#if !FAFA_LIB
#undef WINDOWS_LOOK
#define WINDOWS_LOOK	1
// Can't use bitmap message if no FAFA
#undef USE_BITMAP_MESSAGE
#define USE_BITMAP_MESSAGE 0
#else
#if CTL3D
#undef WINDOWS_LOOK
#define WINDOWS_LOOK	1
#endif
#endif

#endif // wx_setuph

