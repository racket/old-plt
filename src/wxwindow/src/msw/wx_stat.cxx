/*
 * File:	wx_stat.cc
 * Purpose:	Static items
 * Author:	
 * Created:	
 * Updated:	
 * Copyright:
 */

#if defined(_MSC_VER)
# include "wx.h"
#else

#ifdef __GNUG__
#pragma implementation
#endif

#include "wx_setup.h"

#endif

#if USE_EXTENDED_STATICS

#include "wx_stat.h"
#include "wx_panel.h"
#include "wx_obj.h"
#include "wx_utils.h"
#include "wx_wmgr.h"

#include "wx_dcpan.h"

#include <math.h>
#include <stdlib.h>

IMPLEMENT_CLASS(wxBasicColors, wxObject)

wxBasicColors::wxBasicColors(wxPanel *the_panel,COLORREF base_color)
   {
     panel = the_panel;
     if (!panel)
        {
          //sprintf(buff,"The NULL pointer passed to wxBasicColors!\n");
          //OutputDebugString((LPCSTR)buff);
          exit(1);
        }
     if (base_color != wxDefColor)
        {
          back1 = PALETTERGB(GetRValue(base_color),
                             GetGValue(base_color),
                             GetBValue(base_color));
          back2 = PALETTERGB((GetRValue(back1) * 90) / 100,
                             (GetGValue(back1) * 90) / 100,
                             (GetBValue(back1) * 90) / 100);
          back3 = PALETTERGB((GetRValue(back1) * 50) / 100,
                             (GetGValue(back1) * 50) / 100,
                             (GetBValue(back1) * 50) / 100);
          fore  = PALETTERGB((GetRValue(back1) * 150) / 100,
                             (GetGValue(back1) * 150) / 100,
                             (GetBValue(back1) * 150) / 100);
        }
     else
        {
          back1 = GetSysColor(COLOR_BTNFACE);
          back2 = GetSysColor(COLOR_BTNSHADOW);
          back3 = GetSysColor(COLOR_BTNTEXT);
          fore = GetSysColor(COLOR_BTNHIGHLIGHT);
        }
   }

wxBasicColors::~wxBasicColors(void)
   {
   }

IMPLEMENT_ABSTRACT_CLASS(wxStaticItem, wxItem)

wxStaticItem::wxStaticItem(void)
   {
   }

wxStaticItem::wxStaticItem(wxPanel *the_panel,int x, int y, int w, int h):
              wxbStaticItem(the_panel,x,y,w,h)
   {
     panel -> AddStaticItem(this);
   }

Bool wxStaticItem::Show(Bool show)
   {
     if (IsShown() != show)
        {
	  SetShown(show);
          RECT rect;
          rect.left   = x_draw;
          rect.top    = y_draw;
          rect.right  = x_draw + w_draw;
          rect.bottom = y_draw + h_draw;
          InvalidateRect(panel -> GetHWND(),&rect,TRUE);
        }
     return TRUE;
   }

wxStaticItem::~wxStaticItem(void)
   {
     panel -> RemoveStaticItem(this);
   }

IMPLEMENT_CLASS(wxStaticBox, wxStaticItem)

wxStaticBox::wxStaticBox(wxPanel *the_panel,int x,int y,int w,int h,
                          int the_shadow,
                          COLORREF box,
                          COLORREF border,
                          unsigned int the_mask,
                          wx_box_style the_style):
              wxStaticItem(the_panel,x,y,w,h),
              shadow(the_shadow),
              box_ref(box),
              border_ref(border),
              style(the_style),
              box_color(NULL),
              border_color(NULL),
              box_base(NULL),
              border_base(NULL),
              mask(the_mask),
              pen(NULL),
              brush(NULL)
   {
     if (shadow < 1) shadow = 1;
     if (shadow > 8) shadow = 8;

     x_draw -= (shadow /2 + 1);
     y_draw -= (shadow /2 + 1);
     w_draw += (shadow + 2);
     h_draw += (shadow + 2);

     box_color = new wxColour(GetRValue(box_ref),
                              GetGValue(box_ref),
                              GetBValue(box_ref));
     border_color = new wxColour(GetRValue(border_ref),
                                 GetGValue(border_ref),
                                 GetBValue(border_ref));
     box_base = new wxBasicColors(panel,box_ref);
     border_base = new wxBasicColors(panel,border_ref);

     box_color -> pixel = box_base -> back1;
     border_color -> pixel = border_base -> fore;

     pen = new wxPen(*border_color,0,wxSOLID);
     brush = new wxBrush(*box_color,wxSOLID);

     if ((mask & wxIS_BOX) == wxIS_BOX) brush -> SetStyle(wxSOLID);
     else brush -> SetStyle(wxTRANSPARENT);

     if ((mask & wxIS_BORDER) == wxIS_BORDER) pen -> SetStyle(wxSOLID);
     else pen -> SetStyle(wxTRANSPARENT);

     pen -> SetJoin(wxJOIN_MITER);
   }

wxStaticBox::~wxStaticBox(void)
   {

     if (brush) delete brush;
     if (pen) delete  pen;
     if (box_color) delete box_color;
     if (border_color) delete border_color;
     if (box_base) delete box_base;
     if (border_base) delete border_base;
     pen = NULL;
     brush = NULL;
     box_color = NULL;
     border_color = NULL;
     box_base = NULL;
     border_base = NULL;
   }

void wxStaticBox::Draw(PAINTSTRUCT *ps)
   {
     dc -> SetBrush(brush);
     dc -> SetPen(pen);
     switch(style)
        {

          case wxBUMPED           :
          case wxDIPPED           : DrawSimpleBox();
                                    break;
          case wxRIZED_SOLID      :
          case wxRECESSED_SOLID   : DrawRecessedSolidBox();
                                    break;
          case wxRIZED_ROUNDED    :
          case wxRECESSED_ROUNDED : DrawRecessedRoundedBox();
                                    break;
          case wxBLACK_SOLID      :
          case wxWHITE_SOLID      : DrawBWSolidBox();
                                    break;
          case wxBLACK_ROUNDED    :
          case wxWHITE_ROUNDED    : DrawBWRoundedBox();
                                    break;
          default :
               border_color -> pixel = border_base -> fore;
               box_color -> pixel = box_base -> back1;
               pen -> SetColour(*border_color);
               pen -> SetWidth(shadow);
               brush -> SetColour(*box_color);

               dc -> DrawRectangle(xpos,ypos,width,height);

               break;
        }
   }

void wxStaticBox::DrawBWSolidBox(void)
   {
     if (style == wxBLACK_SOLID)
          border_color -> pixel = border_base -> back3;
     else border_color -> pixel = border_base -> fore;
     box_color -> pixel = box_base -> back1;
     pen -> SetColour(*border_color);
     pen -> SetWidth(shadow);
     brush -> SetColour(*box_color);

     dc -> DrawRectangle(xpos,ypos,width,height);
   }

void wxStaticBox::DrawBWRoundedBox(void)
   {
     if (style == wxBLACK_SOLID)
          border_color -> pixel = border_base -> back3;
     else border_color -> pixel = border_base -> fore;
     box_color -> pixel = box_base -> back1;
     pen -> SetColour(*border_color);
     pen -> SetWidth(shadow);
     brush -> SetColour(*box_color);

     if ((mask & wxIS_BOX) == wxIS_BOX)
        {
          pen -> SetStyle(wxTRANSPARENT);
          dc -> DrawRectangle(xpos + wxDefRadius - 1,ypos,
                              width - 2 * wxDefRadius + 2,height + 1);
          dc -> DrawRectangle(xpos,ypos + wxDefRadius,
                              wxDefRadius + 1,height - 2 * wxDefRadius + 1);
          dc -> DrawRectangle(xpos + width - wxDefRadius,ypos + wxDefRadius,
                              wxDefRadius + 1,height - 2 * wxDefRadius + 1);
          Pie(dc -> cdc, xpos, ypos,
              xpos + 2 * wxDefRadius, ypos + 2 * wxDefRadius,
              xpos + wxDefRadius, ypos,
              xpos, ypos + wxDefRadius);
          Pie(dc -> cdc, xpos, ypos + height - 2 * wxDefRadius,
              xpos + 2 * wxDefRadius, ypos + height,
              xpos, ypos + height - wxDefRadius,
              xpos + wxDefRadius, ypos + height);
          Pie(dc -> cdc, xpos + width - 2 * wxDefRadius, ypos,
              xpos + width, ypos + 2 * wxDefRadius,
              xpos + width, ypos + wxDefRadius,
              xpos + width - wxDefRadius, ypos);
          Pie(dc -> cdc, xpos + width - 2 * wxDefRadius,
              ypos + height - 2 * wxDefRadius,
              xpos + width, ypos + height,
              xpos + width - wxDefRadius, ypos + height,
              xpos + width, ypos + height - wxDefRadius);
        }

     if ((mask & wxIS_BORDER) == wxIS_BORDER)
        {
          pen -> SetStyle(wxSOLID);
          dc -> IntDrawLine(xpos + wxDefRadius, ypos,
                            xpos + width - wxDefRadius + 1, ypos);
          dc -> IntDrawLine(xpos + wxDefRadius, ypos + height - 1,
                            xpos + width - wxDefRadius + 1, ypos + height - 1);
          dc -> IntDrawLine(xpos, ypos + wxDefRadius,
                            xpos, ypos + height - wxDefRadius + 1);
          dc -> IntDrawLine(xpos + width - 1, ypos + wxDefRadius,
                            xpos + width - 1, ypos + height - wxDefRadius + 1);
          Arc(dc -> cdc, xpos, ypos,
              xpos + 2 * wxDefRadius, ypos + 2 * wxDefRadius,
              xpos + wxDefRadius, ypos,
              xpos, ypos + wxDefRadius);
          Arc(dc -> cdc, xpos, ypos + height - 2 * wxDefRadius,
              xpos + 2 * wxDefRadius, ypos + height,
              xpos, ypos + height - wxDefRadius,
              xpos + wxDefRadius, ypos + height);
          Arc(dc -> cdc, xpos + width - 2 * wxDefRadius, ypos,
              xpos + width, ypos + 2 * wxDefRadius,
              xpos + width, ypos + wxDefRadius,
              xpos + width - wxDefRadius, ypos);
          Arc(dc -> cdc, xpos + width - 2 * wxDefRadius,
              ypos + height - 2 * wxDefRadius,
              xpos + width, ypos + height,
              xpos + width - wxDefRadius, ypos + height,
              xpos + width, ypos + height - wxDefRadius);
        }
   }

void wxStaticBox::DrawRecessedSolidBox(void)
   {
     int md = shadow / 2;
     int pd;
     if (shadow < 3) pd = - shadow / 2 + (shadow / 2) * 1;
     else pd = shadow / 2 - ((shadow % 2) ? 0 : 1);

     if (style == wxRECESSED_SOLID)
          border_color -> pixel = border_base -> back3;
     else border_color -> pixel = border_base -> fore;
     box_color -> pixel = box_base -> back1;
     pen -> SetColour(*border_color);
     pen -> SetWidth(shadow);
     brush -> SetColour(*box_color);
     pen -> SetWidth(0);

     dc -> DrawRectangle(xpos,ypos,width,height);

     int llp;
     for(llp = 0;llp < shadow;llp++)
        {
          dc -> IntDrawLine(xpos + 1 + llp - md, ypos + height - llp + pd - 1,
                            xpos + width - md, ypos + height - llp + pd - 1);
          dc -> IntDrawLine(xpos + width - llp + pd - 1, ypos + llp - md,
                            xpos + width - llp + pd - 1, ypos + height + pd);
        }

     if (style == wxRECESSED_SOLID)
          border_color -> pixel = border_base -> fore;
     else border_color -> pixel = border_base -> back3;
     pen -> SetColour(*border_color);
     for(llp = 0;llp < shadow;llp++)
        {
          dc -> IntDrawLine(xpos - md, ypos + llp - md,
                            xpos + width - llp + pd - 1, ypos + llp - md);
          dc -> IntDrawLine(xpos + llp - md, ypos - md,
                            xpos + llp - md, ypos + height - llp + pd - 1);
        }
     border_color -> pixel = border_base -> back2;
     pen -> SetColour(*border_color);
     dc -> IntDrawLine(xpos - md,ypos + height + pd - 1,
                       xpos + shadow - md, ypos + height - shadow + pd - 1);
     dc -> IntDrawLine(xpos + width + pd - 1,ypos - md,
                       xpos + width - shadow + pd - 1, ypos + shadow - md);

   }

void wxStaticBox::DrawSimpleBox(void)
   {
     if (style == wxDIPPED)
          border_color -> pixel = border_base -> back3;
     else border_color -> pixel = border_base -> fore;
     box_color -> pixel = box_base -> back1;
     pen -> SetColour( *border_color);
     pen -> SetWidth(0);
     brush -> SetColour(*box_color);

     dc -> DrawRectangle(xpos,ypos,width,height);

     dc -> IntDrawLine(xpos + 1,ypos + height - 1,
                       xpos + 1,ypos + 1);
     dc -> IntDrawLine(xpos + 1,ypos + 1,
                       xpos + width - 2,ypos + 1);
     dc -> IntDrawLine(xpos + width - 1,ypos,
                       xpos + width - 1,ypos + height - 1);
     dc -> IntDrawLine(xpos + 1,ypos + height - 1,
                       xpos + width - 1,ypos  + height - 1);

     if (style == wxDIPPED)
          border_color -> pixel = border_base -> fore;
     else border_color -> pixel = border_base -> back3;
     pen -> SetColour(*border_color);
     dc -> IntDrawLine(xpos,ypos + height - 1,
                       xpos,ypos );
     dc -> IntDrawLine(xpos,ypos,
                       xpos + width - 2,ypos);
     dc -> IntDrawLine(xpos + width - 2,ypos,
                       xpos + width - 2,ypos + height - 1);
     dc -> IntDrawLine(xpos + 2,ypos + height - 2,
                       xpos + width - 2,ypos  + height - 2);

     border_color -> pixel = border_base -> back2;
     pen -> SetColour(*border_color);

     dc -> DrawPoint(xpos,ypos + height - 1);
     dc -> DrawPoint(xpos + width - 1,ypos);
   }

void wxStaticBox::DrawRecessedRoundedBox(void)
   {

     if (style == wxRECESSED_ROUNDED)
          border_color -> pixel = border_base -> back3;
     else border_color -> pixel = border_base -> fore;
     box_color -> pixel = box_base -> back1;
     pen -> SetColour(*border_color);
     pen -> SetWidth(shadow);
     brush -> SetColour(*box_color);

     if ((mask & wxIS_BOX) == wxIS_BOX)
        {
          pen -> SetStyle(wxTRANSPARENT);
          dc -> DrawRectangle(xpos + wxDefRadius - 1,ypos,
                              width - 2 * wxDefRadius + 2,height + 1);
          dc -> DrawRectangle(xpos,ypos + wxDefRadius,
                              wxDefRadius + 1,height - 2 * wxDefRadius + 1);
          dc -> DrawRectangle(xpos + width - wxDefRadius,ypos + wxDefRadius,
                              wxDefRadius + 1,height - 2 * wxDefRadius + 1);
          Pie(dc -> cdc, xpos, ypos,
              xpos + 2 * wxDefRadius, ypos + 2 * wxDefRadius,
              xpos + wxDefRadius, ypos,
              xpos, ypos + wxDefRadius);
          Pie(dc -> cdc, xpos, ypos + height - 2 * wxDefRadius,
              xpos + 2 * wxDefRadius, ypos + height,
              xpos, ypos + height - wxDefRadius,
              xpos + wxDefRadius, ypos + height);
          Pie(dc -> cdc, xpos + width - 2 * wxDefRadius, ypos,
              xpos + width, ypos + 2 * wxDefRadius,
              xpos + width, ypos + wxDefRadius,
              xpos + width - wxDefRadius, ypos);
          Pie(dc -> cdc, xpos + width - 2 * wxDefRadius,
              ypos + height - 2 * wxDefRadius,
              xpos + width, ypos + height,
              xpos + width - wxDefRadius, ypos + height,
              xpos + width, ypos + height - wxDefRadius);
        }

     if ((mask & wxIS_BORDER) == wxIS_BORDER)
        {
          pen -> SetStyle(wxSOLID);
          dc -> IntDrawLine(xpos + wxDefRadius, ypos,
                            xpos + width - wxDefRadius + 1, ypos);
          dc -> IntDrawLine(xpos, ypos + wxDefRadius,
                            xpos, ypos + height - wxDefRadius + 1);
          Arc(dc -> cdc, xpos, ypos,
              xpos + 2 * wxDefRadius, ypos + 2 * wxDefRadius,
              xpos + wxDefRadius, ypos,
              xpos, ypos + wxDefRadius);
          Arc(dc -> cdc, xpos + width - 2 * wxDefRadius, ypos,
              xpos + width, ypos + 2 * wxDefRadius,
              xpos + width, ypos,
              xpos + width - wxDefRadius, ypos);
          Arc(dc -> cdc, xpos, ypos + height - 2 * wxDefRadius,
              xpos + 2 * wxDefRadius, ypos + height,
              xpos, ypos + height - wxDefRadius,
              xpos, ypos + height);

          if (style == wxRECESSED_ROUNDED)
               border_color -> pixel = border_base -> fore;
          else border_color -> pixel = border_base -> back3;
          pen -> SetColour(*border_color);

          dc -> IntDrawLine(xpos + wxDefRadius, ypos + height - 1,
                            xpos + width - wxDefRadius + 1, ypos + height - 1);
          dc -> IntDrawLine(xpos + width - 1, ypos + wxDefRadius,
                            xpos + width - 1, ypos + height - wxDefRadius + 1);
          Arc(dc -> cdc, xpos, ypos + height - 2 * wxDefRadius,
              xpos + 2 * wxDefRadius, ypos + height,
              xpos, ypos + height,
              xpos + wxDefRadius, ypos + height);
          Arc(dc -> cdc, xpos + width - 2 * wxDefRadius, ypos,
              xpos + width, ypos + 2 * wxDefRadius,
              xpos + width, ypos + wxDefRadius,
              xpos + width, ypos);
          Arc(dc -> cdc, xpos + width - 2 * wxDefRadius,
              ypos + height - 2 * wxDefRadius,
              xpos + width, ypos + height,
              xpos + width - wxDefRadius, ypos + height,
              xpos + width, ypos + height - wxDefRadius);
        }
   }

IMPLEMENT_CLASS(wxStaticSeparator, wxStaticItem)

wxStaticSeparator::wxStaticSeparator(wxPanel *the_panel,int x,int y,int w,
                                     int the_layout,
                                     int the_shadow,
                                     COLORREF border,
                                     wx_box_style the_style):
              wxStaticItem(the_panel,x,y,w,the_shadow),
              layout(the_layout),
              shadow(the_shadow),
              border_ref(border),
              style(the_style),
              border_color(NULL),
              border_base(NULL),
              pen(NULL)
   {
     if (shadow < 1) shadow = 1;
     if (shadow > 8) shadow = 8;

     if ((layout != wxHORIZ_SEPARATOR) && (layout != wxVERT_SEPARATOR))
        layout = wxHORIZ_SEPARATOR;

     if ((style != wxDIPPED) && (style != wxBUMPED) &&
         (style != wxBLACK_SOLID) && (style != wxWHITE_SOLID))
          style = wxDIPPED;

     if ((style == wxBUMPED) || (style == wxDIPPED))
          shadow = 2;

     if (layout == wxHORIZ_SEPARATOR)
        {
          height = shadow;
          x_draw -= (shadow /2 + 1);
          y_draw -= (shadow /2 + 1);
          w_draw = width + (shadow + 2);
          h_draw = height + (shadow + 2);
        }
     else
        {
          height = width;
          width = shadow;
          x_draw -= (shadow /2 + 1);
          y_draw -= (shadow /2 + 1);
          w_draw = width + (shadow + 2);
          h_draw = height + (shadow + 2);
        }

     border_color = new wxColour(GetRValue(border_ref),
                                 GetGValue(border_ref),
                                 GetBValue(border_ref));
     border_base = new wxBasicColors(panel,border_ref);
     border_color -> pixel = border_base -> back1;
     pen = new wxPen(*border_color,0,wxSOLID);
     pen -> SetStyle(wxSOLID);
   }

wxStaticSeparator::~wxStaticSeparator(void)
   {
     if (pen) delete pen;
     if (border_color) delete border_color;
     if (border_base) delete border_base;
     pen = NULL;
     border_color = NULL;
     border_base = NULL;
   }

void wxStaticSeparator::Draw(PAINTSTRUCT *ps)
   {
     pen -> SetJoin(wxJOIN_MITER);
     dc -> SetPen(pen);
     switch(style)
        {

          case wxBUMPED :
          case wxDIPPED :
               if (style == wxBUMPED)
                   border_color -> pixel = border_base -> fore;
               else border_color -> pixel = border_base -> back3;
               pen -> SetColour(*border_color);
               pen -> SetWidth(0);
               if (layout == wxHORIZ_SEPARATOR)
                  {
                    dc -> IntDrawLine(xpos,ypos,xpos + width,ypos);
                    dc -> DrawPoint(xpos, ypos + 1);
                  }
               else
                  {
                    dc -> IntDrawLine(xpos,ypos,xpos,ypos + height);
                    dc -> DrawPoint(xpos + 1, ypos);
                  }
               if (style == wxBUMPED)
                   border_color -> pixel = border_base -> back3;
               else border_color -> pixel = border_base -> fore;
               pen -> SetColour(*border_color);
               if (layout == wxHORIZ_SEPARATOR)
                  {
                    dc -> IntDrawLine(xpos + 1,ypos + 1,xpos + width,ypos + 1);
                    dc -> DrawPoint(xpos + width, ypos);
                  }
               else
                  {
                    dc -> IntDrawLine(xpos + 1,ypos + 1,xpos + 1,ypos + height);
                    dc -> DrawPoint(xpos, ypos + height);
                  }
               break;
          case wxWHITE_SOLID  :
          case wxBLACK_SOLID  :
               if (style == wxWHITE_SOLID)
                   border_color -> pixel = border_base -> fore;
               else border_color -> pixel = border_base -> back3;
               pen -> SetColour(*border_color);
               pen -> SetWidth(shadow);
               pen -> SetJoin(wxJOIN_MITER);
               if (layout == wxHORIZ_SEPARATOR)
                         dc -> IntDrawLine(xpos,ypos,xpos + width,ypos);
               else dc -> IntDrawLine(xpos,ypos,xpos,ypos + height);
               break;
        }
   }

IMPLEMENT_CLASS(wxStaticBitmap, wxStaticItem)

wxStaticBitmap::wxStaticBitmap(wxPanel *the_panel,int x,int y,
                          wxBitmap *the_bitmap,
                          COLORREF fg, COLORREF bg):
              wxStaticItem(the_panel,x,y,0,0),
              bitmap(the_bitmap),
              fore_ref(fg),
              back_ref(bg),
              fore(NULL),
              back(NULL)
   {
     if (!bitmap)
        {
//          cout << "Null bitmap passed!\n";
          exit(1);
        }
     fore = new wxBasicColors(panel,fore_ref);
     back = new wxBasicColors(panel,back_ref);
     height = bitmap -> GetHeight();
     width = bitmap -> GetWidth();
     depth = bitmap -> GetDepth();
     w_draw = width;
     h_draw = height;
   }

wxStaticBitmap::wxStaticBitmap(wxPanel *the_panel,int x,int y,
                               char *fname):
              wxStaticItem(the_panel,x,y,0,0),
              bitmap(NULL),
              fore_ref(wxDefColor),
              back_ref(wxDefColor),
              fore(NULL),
              back(NULL)
   {
     if (!fname)
        {
//          cout << "Null fname passed!\n";
          exit(1);
        }
     bitmap = new wxBitmap(fname);
     if (!bitmap)
        {
//          cout << "Bitmap Creation Failed!\n";
          exit(1);
        }
     height = bitmap -> GetHeight();
     width = bitmap -> GetWidth();
     depth = bitmap -> GetDepth();
     w_draw = width;
     h_draw = height;
   }

wxStaticBitmap::~wxStaticBitmap(void)
   {
     long llp,ppl;

     if (bitmap) delete bitmap;
     bitmap = NULL;
     if (fore) delete fore;
     fore = NULL;
     if (back) delete back;
     back = NULL;
   }

void wxStaticBitmap::Draw(PAINTSTRUCT *ps)
   {
     HDC memo;
     HBITMAP hbmp;
	  memo = wxwmCreateCompatibleDC(dc -> cdc);
     hbmp = SelectObject(memo,bitmap -> ms_bitmap);
     BitBlt(dc -> cdc, xpos, ypos, width, height, memo,
            0, 0, SRCCOPY);
     SelectObject(memo,hbmp);
     DeleteDC(memo);
   }

#endif
  // End USE_EXTENDED_STATICS
