/*
 * File:	wb_res.h
 * Purpose:	Resource processor
 * Author:	Julian Smart
 * Created:	1994
 * Updated:	
 * Copyright:	(c) 1994, Julian Smart
 */

/* sccsid[] = "%W% %G%" */

#ifndef wxb_resh
#define wxb_resh

#ifdef __GNUG__
#pragma interface
#endif

#include "wx_setup.h"

#if USE_WX_RESOURCES
#include <stdio.h>

// A few further types not in wx_types.h
#define wxRESOURCE_TYPE_SEPARATOR   1000
#define wxRESOURCE_TYPE_XBM_DATA    1001
#define wxRESOURCE_TYPE_XPM_DATA    1002

/*
 * Internal format for control/panel item
 */
 
class wxItemResource: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxItemResource)

 protected:
  wxList children;
  WXTYPE itemType;
  int x, y, width, height;
  char *title;
  char *name;
  long windowStyle;
  long value1, value2, value3;
  char *value4;
  wxStringList *stringValues; // Optional string values
  wxBitmap *bitmap;
 public:
 
  wxItemResource(void);
  ~wxItemResource(void);

  inline void SetType(WXTYPE typ) { itemType = typ; }
  inline void SetStyle(long styl) { windowStyle = styl; }
  inline void SetBitmap(wxBitmap *bm) { bitmap = bm; }
  inline wxBitmap *GetBitmap(void) { return bitmap; }
  inline void SetSize(int xx, int yy, int ww, int hh)
  {  x = xx; y = yy; width = ww; height = hh; }
  void SetTitle(char *t);
  void SetName(char *n);
  inline void SetValue1(long v) { value1 = v; }
  inline void SetValue2(long v) { value2 = v; }
  inline void SetValue3(long v) { value3 = v; }
  void SetValue4(char *v);
  inline void SetStringValues(wxStringList *svalues) { stringValues = svalues; }

  inline WXTYPE GetType(void) { return itemType; }
  inline int GetX(void) { return x; }
  inline int GetY(void) { return y; }
  inline int GetWidth(void) { return width; }
  inline int GetHeight(void) { return height; }

  inline char *GetTitle(void) { return title; }
  inline char *GetName(void) { return name; }
  inline long GetStyle(void) { return windowStyle; }

  inline long GetValue1(void) { return value1; }
  inline long GetValue2(void) { return value2; }
  inline long GetValue3(void) { return value3; }
  inline char *GetValue4(void) { return value4; }
  inline wxList& GetChildren(void) { return children; }
  inline wxStringList *GetStringValues(void) { return stringValues; }
};

/*
 * Resource table (normally only one of these)
 */
 
class wxResourceTable: public wxHashTable
{
  DECLARE_DYNAMIC_CLASS(wxResourceTable)

  protected:
    
  public:
    wxHashTable identifiers;
    
    wxResourceTable(void);
    ~wxResourceTable(void);
    
    wxItemResource *FindResource(char *name);
    void AddResource(wxItemResource *item);
    Bool DeleteResource(char *name);

    Bool ParseResourceFile(char *filename);
    Bool ParseResourceData(char *data);
    Bool SaveResource(char *filename);

    // Register XBM/XPM data
    Bool RegisterResourceBitmapData(char *name, char bits[], int width, int height);
    Bool RegisterResourceBitmapData(char *name, char **data);

    void ClearTable(void);
};

extern wxResourceTable wxDefaultResourceTable;
extern long wxParseWindowStyle(char *style);

class wxMenuBar;
class wxMenu;
class wxBitmap;
class wxIcon;
extern wxBitmap *wxResourceCreateBitmap(char *resource, wxResourceTable *table = NULL);
extern wxIcon *wxResourceCreateIcon(char *resource, wxResourceTable *table = NULL);
extern wxMenuBar *wxResourceCreateMenuBar(char *resource, wxResourceTable *table = NULL);
extern wxMenu *wxResourceCreateMenu(char *resource, wxResourceTable *table = NULL);
extern Bool wxResourceParseData(char *resource, wxResourceTable *table = NULL);
extern Bool wxResourceParseFile(char *filename, wxResourceTable *table = NULL);
extern void wxResourceClear(wxResourceTable *table = NULL);
// Register XBM/XPM data
extern Bool wxResourceRegisterBitmapData(char *name, char bits[], int width, int height, wxResourceTable *table = NULL);
extern Bool wxResourceRegisterBitmapData(char *name, char **data, wxResourceTable *table = NULL);
#define wxResourceRegisterIconData wxResourceRegisterBitmapData

/*
 * Resource identifer code: #define storage
 */

extern Bool wxResourceAddIdentifier(char *name, int value, wxResourceTable *table = NULL);
extern int wxResourceGetIdentifier(char *name, wxResourceTable *table = NULL);

#endif
#endif
