/*
 * File:	wb_gdi.h
 * Purpose:	Declaration of various graphics objects - fonts, pens, icons etc.
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "@(#)wb_gdi.h	1.2 5/9/94" */

#ifndef wxb_gdih
#define wxb_gdih

#include "wx_obj.h"
#include "wx_list.h"
#include "wx_setup.h"

#ifdef __GNUG__
#pragma interface
#endif

// Standard cursors
typedef enum {
 wxCURSOR_ARROW =  1,
 wxCURSOR_BULLSEYE,
 wxCURSOR_CHAR,
 wxCURSOR_CROSS,
 wxCURSOR_HAND,
 wxCURSOR_IBEAM,
 wxCURSOR_LEFT_BUTTON,
 wxCURSOR_MAGNIFIER,
 wxCURSOR_MIDDLE_BUTTON,
 wxCURSOR_NO_ENTRY,
 wxCURSOR_PAINT_BRUSH,
 wxCURSOR_PENCIL,
 wxCURSOR_POINT_LEFT,
 wxCURSOR_POINT_RIGHT,
 wxCURSOR_QUESTION_ARROW,
 wxCURSOR_RIGHT_BUTTON,
 wxCURSOR_SIZENESW,
 wxCURSOR_SIZENS,
 wxCURSOR_SIZENWSE,
 wxCURSOR_SIZEWE,
 wxCURSOR_SIZING,
 wxCURSOR_SPRAYCAN,
 wxCURSOR_WAIT,
 wxCURSOR_WATCH,
 wxCURSOR_BLANK
#ifdef wx_x
  /* Not yet implemented for Windows */
  , wxCURSOR_CROSS_REVERSE,
  wxCURSOR_DOUBLE_ARROW,
  wxCURSOR_BASED_ARROW_UP,
  wxCURSOR_BASED_ARROW_DOWN
#endif
} _standard_cursors_t;

#ifdef IN_CPROTO
typedef       void *wxbFont;
typedef       void *wxColour;
typedef       void *wxPoint;
typedef       void *wxbPen;
typedef       void *wxbBrush;
typedef       void *wxPenList;
typedef       void *wxBrushList;
typedef       void *wxColourDatabase;
typedef       void *wxGDIList;
typedef       void *wxDash ;
#else

#ifdef wx_msw
typedef    DWORD  wxDash ;
#endif

#ifdef wx_x
typedef    char wxDash ;
#endif

// Font
class wxFont;
class wxbFont: public wxObject
{
 protected:
  Bool temporary;   // If TRUE, the pointer to the actual font
                    // is temporary and SHOULD NOT BE DELETED by
                    // destructor
  int point_size;
  int family;
  int fontid; /* MATTHEW: [4] New font system */
  int style;
  int weight;
  Bool underlined;
 public:
  wxbFont(void);
  /* MATTHEW: [4] New font system */
  wxbFont(int PointSize, int FamilyOrFontId, int Style, int Weight, 
	  Bool underline = FALSE);
  ~wxbFont();

  inline int GetPointSize(void) { return point_size; }
  inline int GetFamily(void) { return family; }
  inline int GetFontId(void) { return fontid; } /* MATTHEW: [4] New font system */
  inline int GetStyle(void) { return style; }
  inline int GetWeight(void) { return weight; }
  char *GetFamilyString(void);
  char *GetFaceString(void); /* MATTHEW: [4] New font system */
  char *GetStyleString(void);
  char *GetWeightString(void);
  inline Bool GetUnderlined(void) { return underlined; }
};

#include "../../../wxcommon/FontDirectory.h"

// Colour
class wxColour: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxColour)
 private:
  short isInit;
  short locked;
  unsigned char red;
  unsigned char blue;
  unsigned char green;
 public:
#ifdef wx_x
  int pixel;
#endif
#ifdef wx_msw
  COLORREF pixel ;
#endif

  wxColour(void);
  wxColour(const unsigned char r, const unsigned char b, const unsigned char g);
  wxColour(const wxColour* col);
  wxColour(const char *col);
  ~wxColour(void) ;
  wxColour* CopyFrom(const wxColour* src) ;
  wxColour* CopyFrom(const char *src) ;
#ifndef MZ_PRECISE_GC
  wxColour& operator=(const wxColour & src) ;
#endif
  inline int Ok(void) { return (isInit) ; }

  void Set(unsigned char r, unsigned char b, unsigned char g);
  void Get(unsigned char *r, unsigned char *b, unsigned char *g);

  inline unsigned char Red(void) { return red; }
  inline unsigned char Green(void) { return green; }
  inline unsigned char Blue(void) { return blue; }

  inline Bool IsMutable(void) { return !locked; }
  inline void Lock(int d) { locked += d; }
};

#define wxColor wxColour

class wxColourMap;


// Point
class wxPoint: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxPoint)
 public:
  float x;
  float y;
  wxPoint(void);
  wxPoint(float the_x, float the_y);
  ~wxPoint(void);
};

class wxIntPoint: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxIntPoint)
 public:
  int x;
  int y;
  wxIntPoint(void);
  wxIntPoint(int the_x, int the_y);
  ~wxIntPoint(void);
};

// Pen
class wxPen;
class wxBitmap;
class wxbPen: public wxObject
{
 protected:
  int width;
  short locked;
  short style;
  int join ;
  int cap ;
  wxBitmap *stipple ;
 public:
  int nb_dash ;
  wxDash *dash ;

  wxColour colour;

  wxbPen(void);
  wxbPen(wxColour *col, int width, int style);
  wxbPen(const char *col, int width, int style);
  ~wxbPen(void);

  virtual void SetColour(wxColour *col) ;
  virtual void SetColour(const char *col)  ;
  virtual void SetColour(char r, char g, char b)  ;

  virtual void SetWidth(int width)  ;
  virtual void SetStyle(int style)  ;
  virtual void SetStipple(wxBitmap *stipple)  ;
  virtual void SetDashes(int nb_dashes, wxDash *dash)  ;
  virtual void SetJoin(int join)  ;
  virtual void SetCap(int cap)  ;

  virtual wxColour* GetColour(void);
  virtual int GetWidth(void);
  virtual int GetStyle(void);
  virtual int GetJoin(void);
  virtual int GetCap(void);
  virtual int GetDashes(wxDash **dash);
  virtual wxBitmap *GetStipple(void);

  inline Bool IsMutable(void) { return !locked; }
  inline void Lock(int d) { locked += d; colour.Lock(d); }
};

// Brush
class wxBrush;
class wxbBrush: public wxObject
{
 protected:
  short locked;
  short style;
  wxBitmap *stipple ;
 public:
  wxColour colour;
  wxbBrush(void);
  wxbBrush(wxColour *col, int style);
  wxbBrush(char *col, int style);
  ~wxbBrush(void);

  virtual void SetColour(wxColour *col)  ;
  virtual void SetColour(const char *col)  ;
  virtual void SetColour(char r, char g, char b)  ;
  virtual void SetStyle(int style)  ;
  virtual void SetStipple(wxBitmap* stipple=NULL)  ;

  virtual wxColour *GetColour(void);
  virtual int GetStyle(void);
  virtual wxBitmap *GetStipple(void);

  inline Bool IsMutable(void) { return !locked; }
  inline void Lock(int d) { locked += d; colour.Lock(d); }
};

/*
 * Bitmap flags
 */

// Hint to discard colourmap if one is loaded
#define wxBITMAP_DISCARD_COLOURMAP      1

// Hint to indicate filetype
#define wxBITMAP_TYPE_BMP               2
#define wxBITMAP_TYPE_BMP_RESOURCE      4
#define wxBITMAP_TYPE_ICO               8
#define wxBITMAP_TYPE_ICO_RESOURCE      16
#define wxBITMAP_TYPE_CUR               32
#define wxBITMAP_TYPE_CUR_RESOURCE      64
#define wxBITMAP_TYPE_XBM               128
#define wxBITMAP_TYPE_XBM_DATA          256
#define wxBITMAP_TYPE_XPM               1024
#define wxBITMAP_TYPE_XPM_DATA          2048
#define wxBITMAP_TYPE_TIF               4096
#define wxBITMAP_TYPE_GIF               8192
#define wxBITMAP_TYPE_ANY               16384

#define wxBITMAP_TYPE_RESOURCE wxBITMAP_TYPE_BMP_RESOURCE

class wxBitmap;
class wxCursor;

// Management of pens, brushes and fonts
class wxPenList: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxPenList)
  wxChildList *list;
 public:
  wxPenList(void);
  ~wxPenList(void);
  void AddPen(wxPen *pen);
  wxPen *FindOrCreatePen(wxColour *colour, int width, int style);
  wxPen *FindOrCreatePen(char *colour, int width, int style);
};

class wxBrushList: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxBrushList)
  wxChildList *list;
 public:
  wxBrushList(void);
  ~wxBrushList(void);
  void AddBrush(wxBrush *brush);
  wxBrush *FindOrCreateBrush(wxColour *colour, int style);
  wxBrush *FindOrCreateBrush(char *colour, int style);
};

class wxFontList: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxFontList)
  wxChildList *list;
 public:
  wxFontList(void);
  ~wxFontList(void);
  void AddFont(wxFont *font);
  /* MATTHEW: [4] New font system */
  wxFont *FindOrCreateFont(int PointSize, int FamilyOrFontId, int Style, int Weight, 
			   Bool underline = FALSE);
  wxFont *FindOrCreateFont(int PointSize, const char *Face, int Family, int Style, 
			   int Weight, Bool underline = FALSE); 
};

class wxColourDatabase: public wxList
{
  DECLARE_CLASS(wxColourDatabase)
 public:
  wxColourDatabase(int type);
  ~wxColourDatabase(void) ;
  wxColour *FindColour(const char *colour);
  char *FindName(wxColour *colour);
  void Initialize(void);
};

class wxGDIList: public wxList
{
  DECLARE_DYNAMIC_CLASS(wxGDIList)
 public:
   wxGDIList(void);
  ~wxGDIList(void);
};

// Lists of GDI objects
extern wxPenList   *wxThePenList;
extern wxBrushList *wxTheBrushList;
extern wxFontList   *wxTheFontList;
extern wxGDIList   *wxTheBitmapList;

// Stock objects
extern wxFont *wxNORMAL_FONT;
extern wxFont *wxSMALL_FONT;
extern wxFont *wxITALIC_FONT;
extern wxFont *wxSWISS_FONT;

extern wxPen *wxRED_PEN;
extern wxPen *wxCYAN_PEN;
extern wxPen *wxGREEN_PEN;
extern wxPen *wxBLACK_PEN;
extern wxPen *wxWHITE_PEN;
extern wxPen *wxTRANSPARENT_PEN;
extern wxPen *wxBLACK_DASHED_PEN;
extern wxPen *wxGREY_PEN;
extern wxPen *wxMEDIUM_GREY_PEN;
extern wxPen *wxLIGHT_GREY_PEN;

extern wxBrush *wxBLUE_BRUSH;
extern wxBrush *wxGREEN_BRUSH;
extern wxBrush *wxWHITE_BRUSH;
extern wxBrush *wxBLACK_BRUSH;
extern wxBrush *wxGREY_BRUSH;
extern wxBrush *wxMEDIUM_GREY_BRUSH;
extern wxBrush *wxLIGHT_GREY_BRUSH;
extern wxBrush *wxTRANSPARENT_BRUSH;
extern wxBrush *wxCYAN_BRUSH;
extern wxBrush *wxRED_BRUSH;

extern wxColour *wxBLACK;
extern wxColour *wxWHITE;
extern wxColour *wxRED;
extern wxColour *wxBLUE;
extern wxColour *wxGREEN;
extern wxColour *wxCYAN;
extern wxColour *wxLIGHT_GREY;

// Stock cursors types
extern wxCursor *wxSTANDARD_CURSOR;
extern wxCursor *wxHOURGLASS_CURSOR;
extern wxCursor *wxCROSS_CURSOR;
extern wxCursor *wxIBEAM_CURSOR;

extern wxColourDatabase *wxTheColourDatabase;
extern void wxInitializeStockObjects(void);
extern void wxDeleteStockObjects(void);

extern Bool wxColourDisplay(void);

// Returns depth of screen
extern int wxDisplayDepth(void);

extern void wxDisplaySize(int *width, int *height);

extern void wxSetCursor(wxCursor *cursor);

#endif // IN_CPROTO
#endif // wxb_gdih
