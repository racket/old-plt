/*
 * File:     test.h
 * Purpose:  Demo for wxWindows image class library
 *
 *
 */

// Define a new application
class MyApp: public wxApp
{
  public:
    wxFrame *OnInit(void);
};

// Define a new canvas which can receive some events
class MyCanvas: public wxCanvas
{
  public:
    MyCanvas(wxFrame *frame, int x, int y, int w, int h, long style = wxRETAINED);
    void OnPaint(void);
    void OnEvent(wxMouseEvent& event);
    void OnSize(int w, int h);
};

// Define a new frame
class MyFrame: public wxFrame
{
  public:
    MyCanvas *canvas;
    MyFrame(wxFrame *frame, char *title, int x, int y, int w, int h);
    Bool OnClose(void);
    void OnMenuCommand(int id);
    void OnMenuSelect(int id);
};


#define TEST_LOAD_FILE  100
// Use wxBitmap method (copy wxImage to wxBitmap)
#define TEST_LOAD_FILE_WXBITMAP_METHOD  101
#define TEST_QUIT       102
#define TEST_ABOUT      103
