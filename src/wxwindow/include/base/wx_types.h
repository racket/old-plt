/*
 * File:	wx_types.h
 * Purpose:	Explicit type system
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

/* sccsid[] = "%W% %G%" */

#ifndef wxb_typesh
#define wxb_typesh

#ifdef __GNUG__
#pragma interface
#endif

#include "common.h"
#include "wx_obj.h"
#include "wx_hash.h"

// Types of objects
#define wxTYPE_ANY             0
#define wxTYPE_OBJECT          wxTYPE_ANY
#define wxTYPE_WINDOW          1
#define wxTYPE_DIALOG_BOX      2
#define wxTYPE_ITEM            3
#define wxTYPE_PANEL           4
#define wxTYPE_CANVAS          5
#define wxTYPE_TEXT_WINDOW     6
#define wxTYPE_FRAME           7

#define wxTYPE_BUTTON          8
#define wxTYPE_TEXT            9
#define wxTYPE_MESSAGE         10
#define wxTYPE_CHOICE          11
#define wxTYPE_LIST_BOX        12
#define wxTYPE_SLIDER          13
#define wxTYPE_CHECK_BOX       14
#define wxTYPE_MENU            15
#define wxTYPE_MENU_BAR        16
#define wxTYPE_MULTI_TEXT      17
#define wxTYPE_RADIO_BOX       18
#define wxTYPE_GROUP_BOX       19
#define wxTYPE_GAUGE           20
#define wxTYPE_SCROLL_BAR      21
#define wxTYPE_VIRT_LIST_BOX   22

#define wxTYPE_EVENT           25
#define wxTYPE_DC              26
#define wxTYPE_DC_CANVAS       27
#define wxTYPE_DC_POSTSCRIPT   28
#define wxTYPE_DC_PRINTER      29
#define wxTYPE_DC_METAFILE     30
#define wxTYPE_DC_MEMORY       31
#define wxTYPE_MOUSE_EVENT     32
#define wxTYPE_KEY_EVENT       33
#define wxTYPE_COMMAND_EVENT   34
#define wxTYPE_DC_PANEL        35

#define wxTYPE_PEN             40
#define wxTYPE_BRUSH           41
#define wxTYPE_FONT            42
#define wxTYPE_ICON            42
#define wxTYPE_BITMAP          43
#define wxTYPE_METAFILE        44
#define wxTYPE_TIMER           45
#define wxTYPE_COLOUR          46
#define wxTYPE_COLOURMAP       47
#define wxTYPE_CURSOR          48

#define wxTYPE_DDE_CLIENT      60
#define wxTYPE_DDE_SERVER      61
#define wxTYPE_DDE_CONNECTION  62

#define wxTYPE_HELP_INSTANCE   63

#define wxTYPE_LIST            70
#define wxTYPE_STRING_LIST     71
#define wxTYPE_HASH_TABLE      72
#define wxTYPE_NODE            73
#define wxTYPE_APP             74
#define wxTYPE_DATE            75

#define wxTYPE_ENHANCED_DIALOG 80
#define wxTYPE_TOOLBAR         81
#define wxTYPE_BUTTONBAR       82

#define wxTYPE_DATABASE        90
#define wxTYPE_QUERY_FIELD     91
#define wxTYPE_QUERY_COL       92
#define wxTYPE_RECORDSET       93

#define wxTYPE_USER            500

#ifdef IN_CPROTO
typedef       void    *wxTypeDef ;
typedef       void    *wxTypeTree ;
#else

class wxTypeDef: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxTypeDef)
 public:
  char *name;
  WXTYPE type;
  WXTYPE parent;
  inline wxTypeDef(void) { name = NULL ; }
  inline ~wxTypeDef(void) { if (name) delete name ; }
};

class wxTypeTree: public wxHashTable
{
  DECLARE_DYNAMIC_CLASS(wxTypeTree)
 public:
  wxTypeTree(void);
  ~wxTypeTree(void);
  void AddType(WXTYPE t, WXTYPE parent, char *name);
  char *GetName(WXTYPE t);
};

extern wxTypeTree wxAllTypes;

Bool wxSubType(WXTYPE type1, WXTYPE type2);

char *wxGetTypeName(WXTYPE type);

void wxInitStandardTypes(void);

#endif // IN_CPROTO
#endif // wxb_typesh

