
#ifndef wx_stath
#define wx_stath

#include <wb_stat.h>

#define wxIS_BORDER (1 << 0)
#define wxIS_BOX    (1 << 1)

#define wxDefRadius   9

#define wxDefColor COLORREF(-1)

typedef enum {
               wxRIZED_SOLID = 0,
               wxRECESSED_SOLID = 1,
               wxRIZED_ROUNDED = 2,
               wxRECESSED_ROUNDED = 3,
               wxBUMPED = 4,
               wxDIPPED = 5,
               wxWHITE_SOLID = 6,
               wxBLACK_SOLID = 7,
               wxWHITE_ROUNDED = 8,
               wxBLACK_ROUNDED = 9 } wx_box_style;

#define wxVERT_SEPARATOR  0
#define wxHORIZ_SEPARATOR 1

#ifdef IN_CPROTO
typedef void *wxBasicColors;
typedef void *wxStaticItem;
typedef void *wxStaticBox;
typedef void *wxStaticSeparator;
typedef void *wxStaticBitmap;
#else

class wxObject;
class wxPanel;
class wxPanelDC;
class wxImage;
class wxColour;
class wxPen;
class wxBrush;
class wxBitmap;
class wxCanvas;

class wxBasicColors : public wxObject
{
  DECLARE_CLASS(wxBasicColors)
 private:
   wxPanel *panel;
 public :
   COLORREF fore, back1, back2, back3;
   wxBasicColors(wxPanel *the_panel,COLORREF base_color);
   ~wxBasicColors(void);
};

class wxStaticItem : public wxbStaticItem
{
  DECLARE_ABSTRACT_CLASS(wxStaticItem)

 public :
  wxStaticItem(void);
  wxStaticItem(wxPanel *the_panel,int x,int y,int w,int h);
  ~wxStaticItem(void);
  virtual Bool Show(Bool show);
  virtual void Draw(PAINTSTRUCT *ps) {} ;
};

class wxStaticBox : public wxStaticItem
{
  DECLARE_CLASS(wxStaticBox)

 protected :
  int shadow;
  wx_box_style style;
  COLORREF box_ref;
  COLORREF border_ref;
  wxColour *box_color;
  wxColour *border_color;
  wxBasicColors *box_base;
  wxBasicColors *border_base;
  wxPen *pen;
  wxBrush *brush;
  unsigned int mask;
 public :
  wxStaticBox(wxPanel *the_panel,int x, int y, int w, int h,
                int the_shadow = 2,
                COLORREF box = wxDefColor,
                COLORREF border = wxDefColor,
                unsigned int the_mask = wxIS_BOX | wxIS_BORDER,
                wx_box_style the_style = wxRECESSED_SOLID);
  ~wxStaticBox(void);
  virtual void Draw(PAINTSTRUCT *ps);
  void DrawBWSolidBox(void);
  void DrawBWRoundedBox(void);
  void DrawRecessedSolidBox(void);
  void DrawRecessedRoundedBox(void);
  void DrawSimpleBox(void);
};

class wxStaticSeparator : public wxStaticItem
{
  DECLARE_CLASS(wxStaticSeparator)

 protected :
  int shadow;
  wx_box_style style;
  COLORREF border_ref;
  wxColour *border_color;
  wxBasicColors *border_base;
  wxPen *pen;
  int layout;
 public :
  wxStaticSeparator(wxPanel *the_panel,int x, int y, int w,
                      int the_layout = wxHORIZ_SEPARATOR,
                      int the_shadow = 2,
                      COLORREF border = wxDefColor,
                      wx_box_style the_style = wxBUMPED);
  ~wxStaticSeparator(void);
  virtual void Draw(PAINTSTRUCT *ps);
};

class wxStaticBitmap :  public wxStaticItem
{
  DECLARE_CLASS(wxStaticBitmap)

 private:
  wxBitmap *bitmap;
  COLORREF fore_ref;
  COLORREF back_ref;
  wxBasicColors *fore;
  wxBasicColors *back;
  int depth;
 public :
  wxStaticBitmap(wxPanel *the_panel,int x, int y,
                 wxBitmap *the_bitmap,
                 COLORREF fg = wxDefColor,
                 COLORREF bg = wxDefColor);
  wxStaticBitmap(wxPanel *the_panel, int x, int y,
                 char *fname);
  ~wxStaticBitmap(void);
  virtual void Draw(PAINTSTRUCT *ps);
};

#endif //IN_CPROTO
#endif
