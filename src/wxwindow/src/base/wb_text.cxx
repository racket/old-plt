/*
 * File:	wb_text.cc
 * Purpose:	wxTextWindow implementation
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	August 1994
 * RCS_ID:      $Id: wb_text.cc,v 1.3 1994/08/14 21:34:01 edz Exp $
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* static const char sccsid[] = "@(#)wb_text.cc	1.2 5/9/94"; */

// #include "wx.h" // Uncomment this line for Borland precomp. headers to work

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "common.h"
#include "wb_text.h"
#include "wx_main.h"
#include "wx_utils.h"
#include "wx_gdi.h"

#endif

wxbTextWindow::wxbTextWindow(void)
#ifndef __BORLANDC__
 :streambuf()
#endif
{
  __type = wxTYPE_TEXT_WINDOW;
  file_name = NULL;
  window_parent = NULL;
  font = wxSWISS_FONT;
#ifndef __BORLANDC__
  if( allocate() ) setp(base(),ebuf());
#endif
}

wxbTextWindow::wxbTextWindow(wxWindow *parent, int WXUNUSED(x), int WXUNUSED(y), int WXUNUSED(width), int WXUNUSED(height),
                           long style, char *WXUNUSED(name))
#ifndef __BORLANDC__
 :streambuf()
#endif
{
  __type = wxTYPE_TEXT_WINDOW;
  windowStyle = style;
  file_name = NULL;
  window_parent = parent;
  font = wxSWISS_FONT;

#ifndef __BORLANDC__
  if( allocate() ) setp(base(),ebuf());
#endif
}

wxbTextWindow::~wxbTextWindow(void)
{
}

//=========================================================================
// Called then the buffer is full (gcc 2.6.3) 
// or then "endl" is output (Borland 4.5)
//=========================================================================
// Class declaration using multiple inheritance doesn't work properly for
// Borland. See note in wb_text.h.
#ifndef __BORLANDC__
int wxbTextWindow::overflow(int WXUNUSED(i))
{
  int len = pptr() - pbase();
  char *txt = new char[len+1];
  strncpy(txt, pbase(), len);
  txt[len] = '\0';
  (*this) << txt;
  setp(pbase(), epptr());
  delete[] txt;
  return EOF;
}

//=========================================================================
// called then "endl" is output (gcc) or then explicit sync is done (Borland)
//=========================================================================
int wxbTextWindow::sync(void)
{
  int len = pptr() - pbase();
  char *txt = new char[len+1];
  strncpy(txt, pbase(), len);
  txt[len] = '\0';
  (*this) << txt;
  setp(pbase(), epptr());
  delete[] txt;
  return 0;
}

//=========================================================================
// Should not be called by a "ostream". Used by a "istream"
//=========================================================================
int wxbTextWindow::underflow(void)
{
//  cerr << "TxtwinbufT::underflow " <<endl;
  return EOF;
}
#endif

wxbTextWindow& wxbTextWindow::operator<<(char *s)
{
  WriteText(s);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(float f)
{
  static char buf[100];
  sprintf(buf, "%.2f", f);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(double d)
{
  static char buf[100];
  sprintf(buf, "%.2f", d);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(int i)
{
  static char buf[100];
  sprintf(buf, "%i", i);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(long i)
{
  static char buf[100];
  sprintf(buf, "%ld", i);
  WriteText(buf);
  return *this;
}

wxbTextWindow& wxbTextWindow::operator<<(char c)
{
  char buf[2];

  buf[0] = c;
  buf[1] = 0;
  WriteText(buf);
  return *this;
}

