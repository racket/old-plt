
#ifndef wxb_stath
#define wxb_stath

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_item.h"

#ifdef IN_CPROTO
typedef void *wxbStaticItem
#else

class wxObject;
class wxPanel;
class wxPanelDC;
class wxbStaticItem : public wxItem
   {
     protected :
          int xpos, ypos;           // The common coordinates
          int width, height;        // The common coordinates
          int x_draw, y_draw;       // The coordinates for redrawing
          int w_draw, h_draw;       // The coordinates for redrawing
          wxPanel *panel;
          wxPanelDC *dc;
     public :
          wxbStaticItem(void);
          wxbStaticItem(wxPanel *the_panel,int x,int y,int w,int h);
          ~wxbStaticItem(void);
          virtual Bool Show(Bool WXUNUSED(show)) { return TRUE; } ;
          virtual Bool IsShow(void) const { return isShow; } ;
          virtual void Draw(void) {} ;

          virtual void GetSize(int *x, int *y, int *w, int *h)
                          {
                            *x = xpos; *y = ypos;
                            *w = width; *h = height;
                          };
          // Avoid compiler warning
          void GetSize(int *w, int *h) { wxWindow::GetSize(w, h); }

          virtual void GetDrawingSize(int *x, int *y, int *w, int *h)
                          {
                            *x = x_draw; *y = y_draw;
                            *w = w_draw; *h = h_draw;
                          };

          virtual void GetClientSize(int *w, int *h)
                          {
                            *w = width; *h = height;
                          };
   };

#endif //IN_CPROTO
#endif
