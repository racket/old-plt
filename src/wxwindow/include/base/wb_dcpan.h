
#ifndef wxb_dcpanh
#define wxb_dcpanh

#ifdef __GNUG__
// #pragma interface
#endif

#include <wx_canvs.h>
#include <wx_dccan.h>
#include <wx_panel.h>

#ifdef IN_CPROTO
typedef void *wxbPanelDC;
#else

class wxPanel;
class wxbPanelDC: public wxCanvasDC
{
 public:
  inline wxbPanelDC(void) { };
  inline wxbPanelDC(wxPanel *the_panel):wxCanvasDC((wxCanvas *)the_panel) { };
  inline ~wxbPanelDC(void) { };
};

#endif // IN_CPROTO
#endif // wxb_dcpanh


