
#ifndef wx_dcpanh
#define wx_dcpanh

#ifdef __GNUG__
#pragma interface
#endif

#include <wb_dcpan.h>
#include <wx_dccan.h>

#define wxDEVICE_PANEL 6

#ifdef IN_CPROTO
define void *wxPanelDC;
#else

class wxPanel;
class wxbPanelDC;
class wxCanvasDC;
class wxPanelDC: public wxbPanelDC
{
  DECLARE_DYNAMIC_CLASS(wxPanelDC)

 public:
  wxPanel *panel;

  wxPanelDC(void);
  wxPanelDC(wxPanel *panel);
  ~wxPanelDC(void);
};

#endif // IN_CPROTO
#endif // wx_dcpanh


