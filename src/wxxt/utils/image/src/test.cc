/*
 * File:     test.cc
 * Purpose:  Demo for wxImage class library (X only)
 *
 * This is an almost undocumented release of X bitmap loading
 * routines for wxWindows. They're taken from the
 * pre-shareware version of XV; I'm pretty sure they can be used
 * by everyone without restriction but can't guarantee it.
 * Please see XV sources for these issues.
 *
 * To use:
 *
 * 1) Include wx_image.h, link with libimage_motif.a or libimage_ol.a.
 * 2) Use wxBitmap *wxLoadBitmap(char *filename, wxColourMap **cmap);
 *    or wxImage directly (see code in OnMenuCommand) setting the
 *    colourmap for the canvas.
 * 3) Splat wxImage or wxBitmap onto a canvas (see OnPaint).
 *
 * This example uses two methods for loading and drawing images:
 * using the wxImage class, or just loading into a wxBitmap.
 * Using wxImage allows scaling; using a wxBitmap is more consistent
 * with wxWindows.
 *
 * All due thanks to original authors of the bitmap loading code!!
 * (see XV sources)
 *
 * Julian Smart 22nd September 1994.
 */

#include "wx.h"
#ifdef wx_x
#include "wx_image.h"
#endif
#ifdef wx_msw
#include "dib.h"
#endif
#include "test.h"

// Declare two frames
MyFrame   *frame = NULL;
wxMenuBar *menu_bar = NULL;
wxIcon    *test_icon = NULL;

wxImage   *theImage = NULL;
wxBitmap  *theBitmap = NULL;
wxColourMap *theColourMap = NULL;

int imageMethod = TEST_LOAD_FILE;

// This statement initialises the whole application
MyApp     myApp;

// For drawing lines in a canvas
float     xpos = -1;
float     ypos = -1;

// Must initialise these in OnInit, not statically
wxPen     *red_pen;
wxFont    *small_font;

float     zoom_factor = 1.0;

// The `main program' equivalent, creating the windows and returning the
// main frame
wxFrame *MyApp::OnInit(void)
{
  // Create a red pen
  red_pen = new wxPen("RED", 3, wxSOLID);

  // Create a small font
  small_font = new wxFont(10, wxSWISS, wxNORMAL, wxNORMAL);

  // Create the main frame window
  frame = new MyFrame(NULL, "Hello wxWindows image demo", 0, 0, 400, 400);

  // Give it a status line
  frame->CreateStatusLine();

  // Give it an icon
  test_icon = new wxIcon("aiai_icn");
  frame->SetIcon(test_icon);

  // Make a menubar
  wxMenu *file_menu = new wxMenu;

  file_menu->Append(TEST_LOAD_FILE, "&Load file into wxImage");
  file_menu->Append(TEST_LOAD_FILE_WXBITMAP_METHOD, "Load file into &wxBitmap");

  file_menu->AppendSeparator();
  file_menu->Append(TEST_QUIT, "&Quit");

  wxMenu *help_menu = new wxMenu;
  help_menu->Append(TEST_ABOUT, "&About");

  menu_bar = new wxMenuBar;

  menu_bar->Append(file_menu, "&File");
  menu_bar->Append(help_menu, "&Help");

  // Associate the menu bar with the frame
  frame->SetMenuBar(menu_bar);

  MyCanvas *canvas = new MyCanvas(frame, 0, 0, -1, -1, wxRETAINED);

  wxCursor *cursor = new wxCursor(wxCURSOR_PENCIL);
  canvas->SetCursor(cursor);

  // Give it scrollbars: the virtual canvas is 20 * 50 = 1000 pixels in each direction
  canvas->SetScrollbars(20, 20, 50, 50, 4, 4);
  canvas->SetPen(red_pen);
  frame->canvas = canvas;

  frame->Show(TRUE);

  frame->SetStatusText("Hello, wxWindows image demo");

  // Return the main frame window
  return frame;
}

// Define my frame constructor
MyFrame::MyFrame(wxFrame *frame, char *title, int x, int y, int w, int h):
  wxFrame(frame, title, x, y, w, h)
{
}


// Intercept menu commands
void MyFrame::OnMenuCommand(int id)
{
  wxDC *dc = canvas->GetDC();
  switch (id)
  {
    // This loads a wxImage and uses it directly to draw to the screen.
    // It's more flexible (you can scale the image)
    // but inconsistent with wxBitmap usage. See below for
    // a wxBitmap method.
    case TEST_LOAD_FILE:
    {
      imageMethod = TEST_LOAD_FILE;
      char *s = wxFileSelector("Load image file", NULL, NULL, NULL, "*.*");
      if (s)
      {
        if (!theImage)
          theImage = new wxImage;
        canvas->SetColourMap(NULL);
        if (theImage->Load(s))
	{
          if (theColourMap)
            delete theColourMap;
          theColourMap = theImage->GetColourMap();
          if (theColourMap)
            canvas->SetColourMap(theColourMap);

          canvas->GetDC()->Clear();

          int w, h;
          canvas->GetSize(&w, &h);
          theImage->Resize(w, h);
          theImage->Draw(canvas);
	}
      }
      break;
    }
    // This uses wxLoadBitmap to return a wxBitmap and wxColourMap,
    // needing no reference to wxImage and therefore more consistent with
    // wxWindows code.
    case TEST_LOAD_FILE_WXBITMAP_METHOD:
    {
      imageMethod = TEST_LOAD_FILE_WXBITMAP_METHOD;
      char *s = wxFileSelector("Load image file", NULL, NULL, NULL, "*.*");
      if (s)
      {
        if (theColourMap)
	{
          canvas->SetColourMap(NULL);
          delete theColourMap;
          theColourMap = NULL;
	}
        if (theBitmap)
        {
          delete theBitmap;
          theBitmap = NULL;
        }
        theBitmap = wxLoadBitmap(s, &theColourMap);

        if (!theBitmap)
          return;

        // Draw the bitmap on the canvas
        if (theColourMap)
          canvas->SetColourMap(theColourMap);
        wxCanvasDC *canvdc = canvas->GetDC();
        canvas->GetDC()->Clear();
        wxMemoryDC temp_dc(canvdc);
        temp_dc.SelectObject(theBitmap);
        canvdc->Blit(0.0,0.0,(float)theBitmap->GetWidth(),(float)theBitmap->GetHeight(), &temp_dc, 0.0, 0.0, wxCOPY);

      }
      break;
    }
    case TEST_QUIT:
    {
      OnClose();
      delete this;
      break;
    }
    case TEST_ABOUT:
    {
      (void)wxMessageBox("wxWindows image demo Vsn 1.50\nAuthor: Julian Smart J.Smart@ed.ac.uk\nAIAI (c) 1994", "About wxImage Demo");
      break;
    }
  }
}

// Intercept menu item selection - only has an effect in Windows
void MyFrame::OnMenuSelect(int id)
{
  char *msg = NULL;
  switch (id)
  {
    case TEST_LOAD_FILE:
      msg = "Load a bitmap into a wxImage";
      break;
    case TEST_LOAD_FILE_WXBITMAP_METHOD:
      msg = "Load a bitmap into a wxBitmap";
      break;
    case TEST_QUIT:
      msg = "Quit program";
      break;
    case -1:
      msg = "";
      break;
  }
  if (msg)
    frame->SetStatusText(msg);
}

// Define a constructor for my canvas
MyCanvas::MyCanvas(wxFrame *frame, int x, int y, int w, int h, long style):
 wxCanvas(frame, x, y, w, h, style)
{
}

// Define the repainting behaviour
void MyCanvas::OnPaint(void)
{
  switch (imageMethod)
  {
    case TEST_LOAD_FILE:
    {
//      if (theImage)
//        theImage->Draw(this);
      break;
    }
    case TEST_LOAD_FILE_WXBITMAP_METHOD:
    {
      if (theBitmap)
      {
        // Draw the bitmap on the canvas
        wxCanvasDC *canvdc = GetDC();
        wxMemoryDC temp_dc(canvdc);
        temp_dc.SelectObject(theBitmap);
        canvdc->Blit(0.0,0.0,(float)theBitmap->GetWidth(),(float)theBitmap->GetHeight(), &temp_dc, 0.0, 0.0, wxCOPY);
      }
    }
    default:
      break;
  }
}

void MyCanvas::OnSize(int w, int h)
{
  if (theImage)
  {
    theImage->Resize(w, h);
    GetDC()->Clear();
    theImage->Draw(this);
  }
}

// This implements a tiny doodling program! Drag the mouse using
// the left button.
void MyCanvas::OnEvent(wxMouseEvent& event)
{
  SetPen(wxBLACK_PEN);
  float x, y;
  event.Position(&x, &y);
  if (xpos > -1 && ypos > -1 && event.Dragging())
  {
    DrawLine(xpos, ypos, x, y);
  }
  xpos = x;
  ypos = y;
}

// Define the behaviour for the frame closing
// - must delete all frames except for the main one.
Bool MyFrame::OnClose(void)
{
  return TRUE;
}

