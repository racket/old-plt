
#ifndef wx_stath
#define wx_stath

#ifdef __GNUG__
#pragma interface
#endif

#include <wb_stat.h>

#ifdef wx_xview
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/cms.h>
#endif

#ifdef wx_x

#include <X11/Xlib.h>

typedef unsigned int   DWORD;
typedef unsigned char  BYTE;
typedef unsigned short WORD;
typedef DWORD COLORREF;

#define RGB(r,g,b) ((COLORREF)(((BYTE)(r) |\
                   ((WORD)(g) << 8)) |\
                   (((DWORD)(BYTE)(b)) << 16)))

#define GetRValue(rgb) ((BYTE)(rgb))
#define GetGValue(rgb) ((BYTE)(((WORD)(rgb)) >> 8))
#define GetBValue(rgb) ((BYTE)((rgb) >> 16))

#endif

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
          unsigned long fore, back1, back2, back3;
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
          virtual void Draw(XRectangle *r,int c) {} ;
   };

class wxStaticBox : public wxStaticItem
   {
  DECLARE_DYNAMIC_CLASS(wxStaticBox)

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
          virtual void Draw(XRectangle *r,int c);
          void DrawBWSolidBox(void);
          void DrawBWRoundedBox(void);
          void DrawRecessedSolidBox(void);
          void DrawRecessedRoundedBox(void);
          void DrawSimpleBox(void);
   };

class wxStaticSeparator : public wxStaticItem
   {
  DECLARE_DYNAMIC_CLASS(wxStaticSeparator)

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
          virtual void Draw(XRectangle *r,int c);
   };

class wxStaticBitmap :  public wxStaticItem 
   {
  DECLARE_DYNAMIC_CLASS(wxStaticBitmap)
 private:
          Pixmap pixmap;
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
          wxStaticBitmap(wxPanel *the_panel, int x, int y,
                         char **data, COLORREF bg = wxDefColor);
          ~wxStaticBitmap(void);
          virtual void Draw(XRectangle *r, int c);
   };

#endif //IN_CPROTO
#endif
