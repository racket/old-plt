/*								-*- C++ -*-
 * $Id: TextWindow.cc,v 1.2 1998/04/11 13:57:32 mflatt Exp $
 *
 * Purpose: text window panel item
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "TextWindow.h"
#endif

#define  Uses_XtIntrinsic
#define  Uses_wxTextWindow
#define  Uses_wxTypeTree
#include "wx.h"
#define  Uses_AsciiTextWidget
#define  Uses_EnforcerWidget
#include "widgets.h"

#include <unistd.h> // for access

//-----------------------------------------------------------------------------
// create and destroy textWindow
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxTextWindow, wxText)

wxTextWindow::wxTextWindow(void) : wxText()
{
    __type = wxTYPE_TEXT_WINDOW;

    modified = FALSE;
}

wxTextWindow::wxTextWindow(wxPanel *panel, int x, int y, int width, int height,
			   long style, char *name) : wxText()
{
    __type = wxTYPE_TEXT_WINDOW;

    modified = FALSE;
    Create(panel, x, y, width, height, style, name);
}

Bool wxTextWindow::Create(wxPanel *panel, int x, int y, int width, int height,
			 long style, char *name)
{
    ChainToPanel(panel, style, name);

    // create frame
    X->frame = XtVaCreateManagedWidget
	(name, xfwfEnforcerWidgetClass, parent->GetHandle()->handle,
	 XtNbackground, bg->GetPixel(cmap),
	 XtNforeground, label_fg->GetPixel(cmap),
	 XtNfont,       label_font->GetInternalFont(),
	 XtNframeType,  XfwfSunken,
	 XtNframeWidth, 2,
	 NULL);
    // create textWindow widget
    X->handle = XtVaCreateManagedWidget
	("textwindow", asciiTextWidgetClass, X->frame,
	 XtNbackground,       bg->GetPixel(cmap),
	 XtNforeground,       fg->GetPixel(cmap),
	 XtNfont,             font->GetInternalFont(),
	 XtNtype,             /* XawAsciiFile */ XawAsciiString,
	 XtNeditType,         style & wxREADONLY ? XawtextRead : XawtextEdit,
	 XtNborderWidth,      0,
	 XtNresize,           XawtextResizeNever,
	 XtNscrollVertical,   XawtextScrollAlways,
	 XtNscrollHorizontal, XawtextScrollWhenNeeded,
	 NULL);
    // propagate key events from frame to text widget
    XtVaSetValues(X->frame, XtNpropagateTarget, X->handle, NULL);
    // callback
    XtAddCallback(XawTextGetSource(X->handle), XtNcallback,
		  wxTextWindow::EventCallback, (XtPointer)this);

    panel->PositionItem(this, x, y,
			(width  > -1 ? width  : wxTEXT_WINDOW_WIDTH),
			(height > -1 ? height : wxTEXT_WINDOW_HEIGHT));
    AddEventHandlers();

    cursor = wxIBEAM_CURSOR;

    return TRUE;
}

//-----------------------------------------------------------------------------
// methods to allow program to modify text
//-----------------------------------------------------------------------------

void wxTextWindow::Clear(void)
{
    XtVaSetValues(X->handle, XtNstring, NULL, NULL);
}

char *wxTextWindow::GetContents(void)
{
    return "";
}

long wxTextWindow::GetInsertionPoint(void)
{
    return long(XawTextGetInsertionPoint(X->handle));
}

long wxTextWindow::GetLastPosition(void)
{
    // set insertion point to end of text and disable redisplay
    XawTextPosition save = GetInsertionPoint();
    XawTextDisableRedisplay(X->handle);
    SetInsertionPointEnd();
    // restore insertion point and enable redisplay
    SetInsertionPoint(save);
    XawTextEnableRedisplay(X->handle);
    return long(save);
}

int wxTextWindow::GetLineLength(long WXUNUSED(lineNo))
{
    return 0;
}

int wxTextWindow::GetLineText(long WXUNUSED(lineNo), char *WXUNUSED(buf))
{
    return 0;
}

int wxTextWindow::GetNumberOfLines(void)
{
    return 0;
}

Bool wxTextWindow::LoadFile(char *file)
{
    Bool readable = (Bool)(access(file, R_OK) == 0);
    Bool writable = (Bool)(access(file, W_OK) == 0);

    XtVaSetValues
	(X->handle,
	 XtNeditType, (style & wxREADONLY || !writable) ? XawtextRead : XawtextEdit,
	 XtNstring,   (readable ? file : NULL),
	 NULL);
    modified = FALSE;
    return readable;
}

void wxTextWindow::PositionToXY(long WXUNUSED(pos), long *x, long *y)
{
    *x = *y = 0;
}

void wxTextWindow::Remove(long from, long to)
{
    XawTextBlock text_block;
    text_block.firstPos = 0;
    text_block.length = 0;
    text_block.ptr = NULL;
    text_block.format = FMT8BIT;
    XawTextReplace(X->handle, from, to, &text_block);
}

void wxTextWindow::Replace(long from, long to, char *value)
{
    XawTextBlock text_block;
    text_block.firstPos = 0;
    text_block.length = strlen(value);
    text_block.ptr = value;
    text_block.format = FMT8BIT;
    XawTextReplace(X->handle, from, to, &text_block);
}

Bool wxTextWindow::SaveFile(char *file)
{
    modified = FALSE;
    return Bool(XawAsciiSaveAsFile(XawTextGetSource(X->handle), file));
}

void wxTextWindow::SetEditable(Bool editable)
{
    XtVaSetValues(X->handle, XtNeditType, (editable? XawtextEdit : XawtextRead), NULL);
}

void wxTextWindow::SetInsertionPoint(long pos)
{
    XawTextSetInsertionPoint(X->handle, XawTextPosition(pos));
}

void wxTextWindow::SetInsertionPointEnd(void)
{
    XEvent event;
    XtCallActionProc(X->handle, "end-of-file", (XEvent*)&event, NULL, 0);
}

void wxTextWindow::SetSelection(long from, long to)
{
    XawTextSetSelection(X->handle, XawTextPosition(from), XawTextPosition(to));
}

void wxTextWindow::ShowPosition(long WXUNUSED(pos))
{
}

void wxTextWindow::WriteText(char *text)
{
    // set insertion point to end of text and disable redisplay
    XawTextPosition save = GetInsertionPoint();
    XawTextDisableRedisplay(X->handle);
    SetInsertionPointEnd();
    // insert text
    XawTextPosition last = GetInsertionPoint();
    XawTextBlock text_block;
    text_block.firstPos = 0;
    text_block.length = strlen(text);
    text_block.ptr = text;
    text_block.format = FMT8BIT;
    XawTextReplace(X->handle, last, last, &text_block);
    // restore insertion point and enable redisplay
    SetInsertionPoint(save);
    XawTextEnableRedisplay(X->handle);
}

long wxTextWindow::XYToPosition(long WXUNUSED(x), long WXUNUSED(y))
{
    return 0;
}


wxTextWindow& wxTextWindow::operator<<(char *s)
{
    WriteText(s);
    return *this;
}

wxTextWindow& wxTextWindow::operator<<(float f)
{
    char buf[100];
    sprintf(buf, "%.2f", f);
    WriteText(buf);
    return *this;
}

wxTextWindow& wxTextWindow::operator<<(double d)
{
    char buf[100];
    sprintf(buf, "%.2f", d);
    WriteText(buf);
    return *this;
}

wxTextWindow& wxTextWindow::operator<<(int i)
{
    char buf[100];
    sprintf(buf, "%i", i);
    WriteText(buf);
    return *this;
}

wxTextWindow& wxTextWindow::operator<<(long i)
{
    char buf[100];
    sprintf(buf, "%ld", i);
    WriteText(buf);
    return *this;
}

wxTextWindow& wxTextWindow::operator<<(char c)
{
    char buf[2];
    buf[0] = c;
    buf[1] = 0;
    WriteText(buf);
    return *this;
}

//-----------------------------------------------------------------------------
// callback for textWindowWidgetClass
//-----------------------------------------------------------------------------

void wxTextWindow::EventCallback(Widget WXUNUSED(w),
				 XtPointer clientData, XtPointer WXUNUSED(ptr))
{
    ((wxTextWindow*)clientData)->modified = TRUE;
}

//-----------------------------------------------------------------------------
// compatibility, I like the typesave constructor more
//-----------------------------------------------------------------------------

wxTextWindow::wxTextWindow(wxWindow *panel, int x, int y, int width, int height,
			   long style, char *name) : wxText()
{
    __type = wxTYPE_TEXT_WINDOW;

    modified = FALSE;
    Create(panel, x, y, width, height, style, name);
}

wxTextWindow::wxTextWindow(wxFrame *panel, int x, int y, int width, int height,
			   long style, char *name) : wxText()
{
    __type = wxTYPE_TEXT_WINDOW;

    modified = FALSE;
    Create((wxPanel *)panel, x, y, width, height, style, name);
}

Bool wxTextWindow::Create(wxWindow *panel, int x, int y, int width, int height,
			 long style, char *name)
{
    if (!wxSubType(__type, wxTYPE_PANEL))
	wxFatalError("parent has to be a wxFrame, wxPanel, or any subtype",
		     "wxTextWindow");
    return Create((wxPanel*)panel, x, y, width, height, style, name);
}
