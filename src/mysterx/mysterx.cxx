// mysterx.cpp : COM/ActiveX/DHTML extension for MzScheme
// Author: Paul Steckler

#include "stdafx.h"

#include <assert.h>

#include <stdio.h>
#include <malloc.h>
#include <float.h>

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>
#include <shellapi.h>

#include "resource.h"

#include "escheme.h"

// ATL support

#include <atlbase.h>
extern CComModule _Module;
#include <atlcom.h>
#include <atlhost.h>
CComModule _Module;

// end ATL support

#include "myspage.h"
#include "myspage_i.c"
#include "mysterx.h"

static HINSTANCE hInstance;
static HICON hIcon;
static HWND documentHwnd;
static HANDLE documentHwndMutex;
static HANDLE createHwndSem;

static MX_TYPE_TBL_ENTRY *typeTable[TYPE_TBL_SIZE];

static char *objectAttributes[] = { "InprocServer", "InprocServer32",
				    "LocalServer", "LocalServer32", NULL };
static char *controlAttributes[] = { "Control", NULL };

static MX_PRIM mxPrims[] = {
  { mx_com_invoke,"invoke",2,-1}, 
  { mx_com_set_property,"set-property!",2,-1 },
  { mx_com_get_property,"get-property",2, -1 },
  { mx_com_methods,"methods",1,1 },
  { mx_com_get_properties,"get-properties",1,1 },
  { mx_com_set_properties,"set-properties", 1, 1 },
  { mx_com_method_type,"method-type",2,2 },
  { mx_com_get_property_type,"get-property-type",2,2 },
  { mx_com_set_property_type,"set-property-type",2,2 },
  { mx_all_com_classes,"all-com-classes",0,0 },
  { mx_find_element,"document-find-element",3,3 },
  { mx_document_objects,"document-objects",1,1 },
  { mx_coclass_to_html,"coclass->html",1,3 },
  { mx_insert_html,"document-insert-html",2,2 },
  { mx_append_html,"document-append-html",2,2 },
  { mx_replace_html,"document-replace-html",2,2 },
  { mx_document_pred,"document?",1,1 },

  // elements

  { mx_element_insert_html,"element-insert-html",2,2 },
  { mx_element_append_html,"element-append-html",2,2 },
  { mx_element_insert_text,"element-insert-text",2,2 },
  { mx_element_append_text,"element-append-text",2,2 },
  { mx_element_replace_html,"element-replace-html",2,2 },
  { mx_element_attribute,"element-attribute",2,2 },
  { mx_element_set_attribute,"element-set-attribute!",3,3 },
  { mx_element_click,"element-click",1,1 },
  { mx_element_tag,"element-tag",1,1 },
  { mx_element_font_family,"element-font-family",1,1 },
  { mx_element_set_font_family,"element-set-font-family!",2,2 },
  { mx_element_font_style,"element-font-style",1,1 },
  { mx_element_set_font_style,"element-set-font-style!",2,2 },
  { mx_element_font_variant,"element-font-variant",1,1 },
  { mx_element_set_font_variant,"element-set-font-variant!",2,2 },
  { mx_element_font_weight,"element-font-weight",1,1 },
  { mx_element_set_font_weight,"element-set-font-weight!",2,2 },
  { mx_element_font,"element-font",1,1 },
  { mx_element_set_font,"element-set-font!",2,2 }, 
  { mx_element_background,"element-background",1,1 },
  { mx_element_set_background,"element-set-background!",2,2 }, 
  { mx_element_background_image,"element-background-image",1,1 },
  { mx_element_set_background_image,"element-set-background-image!",1,1 },
  { mx_element_background_repeat,"element-background-repeat",1,1 },
  { mx_element_set_background_repeat,"element-set-background-repeat!",1,1 },
  { mx_element_background_position,"element-background-position",1,1 },
  { mx_element_set_background_position,"element-set-background-position!",2,2 }, 
  { mx_element_text_decoration,"element-text-decoration",1,1 },
  { mx_element_set_text_decoration,"element-set-text-decoration!",2,2 }, 
  { mx_element_text_transform,"element-text-transform",1,1 },
  { mx_element_set_text_transform,"element-set-text-transform!",2,2 }, 
  { mx_element_text_align,"element-text-align",1,1 },
  { mx_element_set_text_align,"element-set-text-align!",2,2 }, 
  { mx_element_margin,"element-margin",1,1 },
  { mx_element_set_margin,"element-set-margin!",2,2 }, 
  { mx_element_padding,"element-padding",1,1 },
  { mx_element_set_padding,"element-set-padding!",2,2 }, 
  { mx_element_border,"element-border",1,1 },
  { mx_element_set_border,"element-set-border!",2,2 }, 
  { mx_element_border_top,"element-border-top",1,1 },
  { mx_element_set_border_top,"element-set-border-top!",2,2 }, 
  { mx_element_border_bottom,"element-border-bottom",1,1 },
  { mx_element_set_border_bottom,"element-set-border-bottom!",2,2 }, 
  { mx_element_border_left,"element-border-left",1,1 },
  { mx_element_set_border_left,"element-set-border-left!",2,2 }, 
  { mx_element_border_right,"element-border-right",1,1 },
  { mx_element_set_border_right,"element-set-border-right!",2,2 }, 
  { mx_element_border_color,"element-border-color",1,1 },
  { mx_element_set_border_color,"element-set-border-color!",2,2 }, 
  { mx_element_border_width,"element-border-width",1,1 },
  { mx_element_set_border_width,"element-set-border-width!",2,2 }, 
  { mx_element_border_style,"element-border-style",1,1 },
  { mx_element_set_border_style,"element-set-border-style!",2,2 }, 
  { mx_element_border_top_style,"element-border-top-style",1,1 },
  { mx_element_set_border_top_style,"element-set-border-top-style!",2,2 }, 
  { mx_element_border_bottom_style,"element-border-bottom-style",1,1 },
  { mx_element_set_border_bottom_style,"element-set-border-bottom-style!",2,2 }, 
  { mx_element_border_left_style,"element-border-left-style",1,1 },
  { mx_element_set_border_left_style,"element-set-border-left-style!",2,2 }, 
  { mx_element_border_right_style,"element-border-right-style",1,1 },
  { mx_element_set_border_right_style,"element-set-border-right-style!",2,2 }, 
  { mx_element_style_float,"element-style-float",1,1 },
  { mx_element_set_style_float,"element-set-style-float!",2,2 }, 
  { mx_element_clear,"element-clear",1,1 },
  { mx_element_set_clear,"element-set-clear!",2,2 }, 
  { mx_element_display,"element-display",1,1 },
  { mx_element_set_display,"element-set-display!",2,2 }, 
  { mx_element_visibility,"element-visibility",1,1 },
  { mx_element_set_visibility,"element-set-visibility!",2,2 }, 
  { mx_element_list_style_type,"element-list-style-type",1,1 },
  { mx_element_set_list_style_type,"element-set-list-style-type!",2,2 }, 
  { mx_element_list_style_position,"element-list-style-position",1,1 },
  { mx_element_set_list_style_position,"element-set-list-style-position!",2,2 }, 
  { mx_element_list_style_image,"element-list-style-image",1,1 },
  { mx_element_set_list_style_image,"element-set-list-style-image!",2,2 }, 
  { mx_element_list_style,"element-list-style",1,1 },
  { mx_element_set_list_style,"element-set-list-style!",2,2 }, 
  { mx_element_whitespace,"element-whitespace",1,1 },
  { mx_element_set_whitespace,"element-set-whitespace!",2,2 }, 
  { mx_element_position,"element-position",1,1 },
  { mx_element_overflow,"element-overflow",1,1 },
  { mx_element_set_overflow,"element-set-overflow!",2,2 }, 
  { mx_element_pagebreak_before,"element-pagebreak-before",1,1 },
  { mx_element_set_pagebreak_before,"element-set-pagebreak-before!",2,2 }, 
  { mx_element_pagebreak_after,"element-pagebreak-after",1,1 },
  { mx_element_set_pagebreak_after,"element-set-pagebreak-after!",2,2 }, 
  { mx_element_css_text,"element-css-text",1,1 },
  { mx_element_set_css_text,"element-set-css-text!",2,2 }, 
  { mx_element_cursor,"element-cursor",1,1 },
  { mx_element_set_cursor,"element-set-cursor!",2,2 }, 
  { mx_element_clip,"element-clip",1,1 },
  { mx_element_set_clip,"element-set-clip!",2,2 }, 
  { mx_element_filter,"element-filter",1,1 },
  { mx_element_set_filter,"element-set-filter!",2,2 }, 
  { mx_element_style_string,"element-style-string",1,1 },
  { mx_element_text_decoration_none,"element-text-decoration-none",1,1 },
  { mx_element_set_text_decoration_none,"element-set_text_decoration_none!",2,2 }, 
  { mx_element_text_decoration_underline,"element-text-decoration-underline",1,1 },
  { mx_element_set_text_decoration_underline,"element-set-text-decoration-underline!",2,2 }, 
  { mx_element_text_decoration_overline,"element-text-decoration-overline",1,1 },
  { mx_element_set_text_decoration_overline,"element-set-text-decoration-overline!",2,2 }, 
  { mx_element_text_decoration_linethrough,"element-text-decoration-linethrough",1,1 },
  { mx_element_set_text_decoration_linethrough,"element-set-text-decoration-linethrough!",2,2 }, 
  { mx_element_text_decoration_blink,"element-text-decoration-blink",1,1 },
  { mx_element_set_text_decoration_blink,"element-set-text-decoration-blink!",2,2 }, 
  { mx_element_pixel_top,"element-pixel-top",1,1 },
  { mx_element_set_pixel_top,"element-set-pixel-top!",2,2 }, 
  { mx_element_pixel_left,"element-pixel-left",1,1 },
  { mx_element_set_pixel_left,"element-set-pixel-left!",2,2 }, 
  { mx_element_pixel_width,"element-pixel-width",1,1 },
  { mx_element_set_pixel_width,"element-set-pixel-width!",2,2 }, 
  { mx_element_pixel_height,"element-pixel-height",1,1 },
  { mx_element_set_pixel_height,"element-set-pixel-height!",2,2 }, 
  { mx_element_pos_top,"element-pos-top",1,1 },
  { mx_element_set_pos_top,"element-set-pos-top!",2,2 }, 
  { mx_element_pos_left,"element-pos-left",1,1 },
  { mx_element_set_pos_left,"element-set-pos-left!",2,2 }, 
  { mx_element_pos_width,"element-pos-width",1,1 },
  { mx_element_set_pos_width,"element-set-pos-width!",2,2 }, 
  { mx_element_pos_height,"element-pos-height",1,1 },
  { mx_element_set_pos_height,"element-set-pos-height!",2,2 }, 
  { mx_element_font_size,"element-font-size",1,1 },
  { mx_element_set_font_size,"element-set-font-size!",2,2 }, 
  { mx_element_color,"element-color",1,1 },
  { mx_element_set_color,"element-set-color!",2,2 }, 
  { mx_element_background_color,"element-background-color",1,1 },
  { mx_element_set_background_color,"element-set-background-color!",2,2 }, 
  { mx_element_background_position_x,"element-background-position-x",1,1 },
  { mx_element_set_background_position_x,"element-set-background-position-x!",2,2 }, 
  { mx_element_background_position_y,"element-background-position-y",1,1 },
  { mx_element_set_background_position_y,"element-set-background-position-y!",2,2 }, 
  { mx_element_word_spacing,"element-word-spacing",1,1 },
  { mx_element_set_word_spacing,"element-set-word-spacing!",2,2 }, 
  { mx_element_letter_spacing,"element-letter-spacing",1,1 },
  { mx_element_set_letter_spacing,"element-set-letter-spacing!",2,2 }, 
  { mx_element_vertical_align,"element-vertical-align",1,1 },
  { mx_element_set_vertical_align,"element-set-vertical-align!",2,2 }, 
  { mx_element_text_indent,"element-text-indent",1,1 },
  { mx_element_set_text_indent,"element-set-text-indent!",2,2 }, 
  { mx_element_line_height,"element-line-height",1,1 },
  { mx_element_set_line_height,"element-set-line-height!",2,2 }, 
  { mx_element_margin_top,"element-margin-top",1,1 },
  { mx_element_set_margin_top,"element-set-margin-top!",2,2 }, 
  { mx_element_margin_bottom,"element-margin-bottom",1,1 },
  { mx_element_set_margin_bottom,"element-set-margin-bottom!",2,2 }, 
  { mx_element_margin_left,"element-margin-left",1,1 },
  { mx_element_set_margin_left,"element-set-margin-left!",2,2 }, 
  { mx_element_margin_right,"element-margin-right",1,1 },
  { mx_element_set_margin_right,"element-set-margin-right!",2,2 }, 
  { mx_element_padding_top,"element-padding-top",1,1 },
  { mx_element_set_padding_top,"element-set-padding-top!",2,2 }, 
  { mx_element_padding_bottom,"element-padding-bottom",1,1 },
  { mx_element_set_padding_bottom,"element-set-padding-bottom!",2,2 }, 
  { mx_element_padding_left,"element-padding-left",1,1 },
  { mx_element_set_padding_left,"element-set-padding-left!",2,2 }, 
  { mx_element_padding_right,"element-padding-right",1,1 },
  { mx_element_set_padding_right,"element-set-padding-right!",2,2 }, 
  { mx_element_border_top_color,"element-border-top-color",1,1 },
  { mx_element_set_border_top_color,"element-set-border-top-color!",2,2 }, 
  { mx_element_border_bottom_color,"element-border-bottom-color",1,1 },
  { mx_element_set_border_bottom_color,"element-set-border-bottom-color!",2,2 }, 
  { mx_element_border_left_color,"element-border-left-color",1,1 },
  { mx_element_set_border_left_color,"element-set-border-left-color!",2,2 }, 
  { mx_element_border_right_color,"element-border-right-color",1,1 },
  { mx_element_set_border_right_color,"element-set-border-right-color!",2,2 }, 
  { mx_element_border_top_width,"element-border-top-width",1,1 },
  { mx_element_set_border_top_width,"element-set-border-top-width!",2,2 }, 
  { mx_element_border_bottom_width,"element-border-bottom-width",1,1 },
  { mx_element_set_border_bottom_width,"element-set-border-bottom-width!",2,2 }, 
  { mx_element_border_left_width,"element-border-left-width",1,1 },
  { mx_element_set_border_left_width,"element-set-border-left-width!",2,2 }, 
  { mx_element_border_right_width,"element-border-right-width",1,1 },
  { mx_element_set_border_right_width,"element-set-border-right-width!",2,2 }, 
  { mx_element_width,"element-width",1,1 },
  { mx_element_set_width,"element-set-width!",2,2 }, 
  { mx_element_height,"element-height",1,1 },
  { mx_element_set_height,"element-set-height!",2,2 }, 
  { mx_element_top,"element-top",1,1 },
  { mx_element_set_top,"element-set-top!",2,2 }, 
  { mx_element_left,"element-left",1,1 },
  { mx_element_set_left,"element-set-left!",2,2 }, 
  { mx_element_z_index,"element-z-index",1,1 },
  { mx_element_set_z_index,"element-set-z-index!",2,2 }, 
  
  // events
  
  { mx_event_pred,"event?",1,1 },
  { mx_get_event,"get-event",1,1 },
  { mx_event_tag,"event-tag",1,1},
  { mx_event_id,"event-id",1,1},
  { mx_event_from_tag,"event-from-tag",1,1},
  { mx_event_from_id,"event-from-id",1,1},
  { mx_event_to_tag,"event-to-tag",1,1},
  { mx_event_to_id,"event-to-id",1,1},
  { mx_event_x,"event-x",1,1},
  { mx_event_y,"event-y",1,1},
  { mx_event_keypress_pred,"event-keypress?",1,1},
  { mx_event_keydown_pred,"event-keydown?",1,1},
  { mx_event_keyup_pred,"event-keyup?",1,1},
  { mx_event_mousedown_pred,"event-mousedown?",1,1},
  { mx_event_mousemove_pred,"event-mousemove?",1,1},
  { mx_event_mouseover_pred,"event-mouseover?",1,1},
  { mx_event_mouseout_pred,"event-mouseout?",1,1},
  { mx_event_mouseup_pred,"event-mouseup?",1,1},
  { mx_event_click_pred,"event-click?",1,1},
  { mx_event_dblclick_pred,"event-dblclick?",1,1},
  { mx_event_error_pred,"event-error?",1,1},
  { mx_event_available,"event-available?",1,1},
  { mx_make_document,"make-document",6,6},
  { mx_document_show,"document-show",2,2},
};

DOCUMENT_WINDOW_STYLE_OPTION styleOptions[] = {

  // keep alphabetic for bsearch()

  // { symbol,Win32 constant,add/remove } 

  { "iconize",WS_ICONIC,TRUE },
  { "maximize",WS_MAXIMIZE,TRUE },
  { "no-caption",WS_CAPTION,FALSE },
  { "no-system-menu",WS_CAPTION | WS_SYSMENU,FALSE },
  { "no-thick-border",WS_THICKFRAME,FALSE },
};

void scheme_release_com_object(void *comObject,void *pIUnknown) {

  // when COM object GC'd, release interface pointer

  ((IUnknown *)pIUnknown)->Release();
}

void mx_register_com_object(Scheme_Object *obj,IUnknown *pIUnknown) {
  scheme_register_finalizer(obj,scheme_release_com_object,pIUnknown,NULL,NULL);
}

char *inv_kind_string(INVOKEKIND invKind) {
  switch (invKind) {
  case INVOKE_FUNC :
    return "method";
  case INVOKE_PROPERTYGET :
  case INVOKE_PROPERTYPUT :
    return "property";
  }

  return NULL;
}

char *mx_fun_string(INVOKEKIND invKind) {
  switch (invKind) {
  case INVOKE_FUNC :
    return "com-invoke";
  case INVOKE_PROPERTYGET :
    return "com-get-property";
  case INVOKE_PROPERTYPUT :
    return "com-set-property!";
  }

  return NULL;
}

unsigned short getHashValue(IDispatch *pIDispatch,INVOKEKIND invKind,
			    char *name) {
  char *p;
  unsigned short hashVal;
  
  hashVal = (unsigned short)pIDispatch + invKind;

  p = name;
  while (*p) {
    hashVal += (unsigned short)(*p);
    p++;
  }

  return hashVal % TYPE_TBL_SIZE;
  
}

void addTypeToTable(IDispatch *pIDispatch,char *name,
			  INVOKEKIND invKind,
			  MX_TYPEDESC *pTypeDesc) {
  unsigned short hashVal;
  MX_TYPE_TBL_ENTRY *pEntry,*p;

  pEntry = (MX_TYPE_TBL_ENTRY *)scheme_malloc(sizeof(MX_TYPE_TBL_ENTRY));
  pEntry->pTypeDesc = pTypeDesc;
  pEntry->pIDispatch = pIDispatch;
  pEntry->invKind = invKind;
  pEntry->name = name;
  pEntry->next = NULL;
  
  hashVal = getHashValue(pIDispatch,invKind,name);
  
  p = typeTable[hashVal];
  
  if (p == NULL) {
    typeTable[hashVal] = pEntry;
  }
  else {
    while (p->next != NULL) {
      p = p->next;
    }
    p->next = pEntry; 
  }
}

MX_TYPEDESC *lookupTypeDesc(IDispatch *pIDispatch,char *name,
			    INVOKEKIND invKind) {
  unsigned short hashVal;
  MX_TYPE_TBL_ENTRY *p;

  hashVal = getHashValue(pIDispatch,invKind,name);
  
  p = typeTable[hashVal];
  
  while (p) {
    if (p->pIDispatch == pIDispatch && 
	p->invKind == invKind &&
	strcmp(p->name,name) == 0) {
      return p->pTypeDesc;
    }
    p = p->next;
  }

  return NULL;
}

void scheme_add_prim_to_env(Scheme_Env *env,
			    Scheme_Object *(*f)(int,Scheme_Object **),
			    char *name,
			    short minArgs,short maxArgs) {
  Scheme_Object *pobj;

  pobj = scheme_make_prim_w_arity(f,name,minArgs,maxArgs);
    
  scheme_add_global(name,pobj,env);
}

MX_TYPEDESC *getMethodType(IDispatch *pIDispatch,char *name,INVOKEKIND invKind) {
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  MX_TYPEDESC *pTypeDesc;
  MEMBERID memID;
  MX_DESCKIND descKind;
  BOOL foundDesc;
  ITypeInfo *pITypeInfo;
  UINT typeInfoCount;
  int i;
  int count;

  // we really want 
  // WCHAR unicodeName[256];
  // but VC++ complains 
	
  WCHAR *unicodeName = (WCHAR *)(alloca(UNICODE_BUFFER_SIZE * sizeof(WCHAR))); 

  // need Unicode version of name to please ITypeInfo::GetIDsOfNames
  // note that we need string length + 1

  // check in hash table to see if we already have the type information

  pTypeDesc = lookupTypeDesc(pIDispatch,name,invKind);

  if (pTypeDesc) {
    return pTypeDesc;
  }
  
  pIDispatch->GetTypeInfoCount(&typeInfoCount);

  if (typeInfoCount == 0) {
    scheme_signal_error("COM object does not expose type information");
  }

  hr = pIDispatch->GetTypeInfo(0,LOCALE_SYSTEM_DEFAULT,&pITypeInfo);

  if (hr != S_OK) {
    scheme_signal_error("Error getting COM type information");
  }

  count = MultiByteToWideChar(CP_ACP,(DWORD)0,name,strlen(name) + 1,
			      unicodeName,UNICODE_BUFFER_SIZE);

  if (count == 0) {
    scheme_signal_error("Error translating function name % to Unicode",name);
  }

  // try ITypeInfo version of GetIDsOfNames

  hr = pITypeInfo->GetIDsOfNames(&unicodeName,1,&memID);

  if (hr != S_OK) {
    
    // if that doesn't work, try IDispatch version

    hr = pIDispatch->GetIDsOfNames(IID_NULL,&unicodeName,1,LOCALE_SYSTEM_DEFAULT,&memID);
    
    if (hr != S_OK) {
      scheme_signal_error("Error looking up function \"%s\" in type library\n",name);
    }
  }

  // using memID of procedure, find FUNCDESC for that procedure

  hr = pITypeInfo->GetTypeAttr(&pTypeAttr);

  if (hr != S_OK) {
    scheme_signal_error("Error getting type attributes for function \"%s\"",name);
  }

  foundDesc = FALSE;

  for (i = 0; i < pTypeAttr->cFuncs; i++) {
    hr = pITypeInfo->GetFuncDesc(i,&pFuncDesc);		
		
    if (hr != S_OK) {
      scheme_signal_error("Error getting type description");
    }

    // see if this FUNCDESC is the one we want

    if (pFuncDesc->memid == memID && pFuncDesc->invkind == invKind) {
      foundDesc = TRUE;
      descKind = funcDesc;			
      break;
    }

    // if not, throw it back
    
    pITypeInfo->ReleaseFuncDesc(pFuncDesc);
  
  }

  if (invKind != INVOKE_FUNC) {
    for (i = 0; i < pTypeAttr->cVars; i++) {
      hr = pITypeInfo->GetVarDesc(i,&pVarDesc);		
      if (hr != S_OK) {
	scheme_signal_error("Error getting type description");
      }

      // see if this VARDESC is the one we want

      if (pVarDesc->memid == memID) {
	foundDesc = TRUE;
	descKind = varDesc;
	break;
      }

      // if not, throw it back
      
      pITypeInfo->ReleaseVarDesc(pVarDesc);
  
    }
  }

  pITypeInfo->ReleaseTypeAttr(pTypeAttr);

  if (foundDesc == FALSE) {
    scheme_signal_error("Error finding type description for \"%s\"",name);
  }

  pTypeDesc = (MX_TYPEDESC *)scheme_malloc(sizeof(MX_TYPEDESC));

  pTypeDesc->memID = memID;
  pTypeDesc->descKind = descKind;

  if (descKind == funcDesc) {
    pTypeDesc->pFuncDesc = pFuncDesc;
  }
  else {
    pTypeDesc->pVarDesc = pVarDesc;
  }

  addTypeToTable(pIDispatch,name,invKind,pTypeDesc);

  return pTypeDesc;
}

Scheme_Object *mx_do_get_methods(int argc,Scheme_Object **argv,INVOKEKIND invKind) {
  IDispatch *pIDispatch;
  ITypeInfo *pITypeInfo;
  BSTR bstr;
  HRESULT hr;
  TYPEATTR *pTypeAttr;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  Scheme_Object *retval;
  char buff[256];
  unsigned int count,typeInfoCount;
  int i;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("mx-methods","mx-object",0,argc,argv);
  }

  pIDispatch = MX_COM_OBJ_VAL(argv[0]);

  if (pIDispatch == NULL) {
    scheme_signal_error("NULL COM object");
  }
  
  pIDispatch->GetTypeInfoCount(&typeInfoCount);
  
  if (typeInfoCount == 0) {
    scheme_signal_error("COM object does not expose type information");
  }

  hr = pIDispatch->GetTypeInfo(0,LOCALE_SYSTEM_DEFAULT,&pITypeInfo);

  if (hr != S_OK || pITypeInfo == NULL) {
    scheme_signal_error("Error getting COM type information");
  }

  hr = pITypeInfo->GetTypeAttr(&pTypeAttr);

  if (hr != S_OK || pTypeAttr == NULL) {
    scheme_signal_error("Error getting type attributes");
  }

  retval = scheme_null;

  // properties can appear in list of functions
  // or in list of variables

  for (i = 0; i < pTypeAttr->cFuncs; i++) {
    pITypeInfo->GetFuncDesc(i,&pFuncDesc);		
    if (pFuncDesc->invkind == invKind) {
      pITypeInfo->GetNames(pFuncDesc->memid,&bstr,1,&count);
      WideCharToMultiByte(CP_ACP,(DWORD)0,bstr,SysStringLen(bstr) + 1,
			  buff,sizeof(buff) - 1,
			  NULL,NULL);
      retval = scheme_make_pair(scheme_make_string(buff),retval);
      SysFreeString(bstr);
    }
    pITypeInfo->ReleaseFuncDesc(pFuncDesc);
  }

  if (invKind == INVOKE_FUNC) { // done, if not a property
    return retval;
  }

  for (i = 0; i < pTypeAttr->cVars; i++) {
    pITypeInfo->GetVarDesc(i,&pVarDesc);		
    pITypeInfo->GetNames(pVarDesc->memid,&bstr,1,&count);
    WideCharToMultiByte(CP_ACP,(DWORD)0,bstr,SysStringLen(bstr) + 1,
			buff,sizeof(buff) - 1,
			NULL,NULL);
    retval = scheme_make_pair(scheme_make_string(buff),retval);
    SysFreeString(bstr);
    pITypeInfo->ReleaseVarDesc(pVarDesc);
  } 

  return retval;
}

Scheme_Object *mx_com_methods(int argc,Scheme_Object **argv) {
  return mx_do_get_methods(argc,argv,INVOKE_FUNC);
}

Scheme_Object *mx_com_get_properties(int argc,Scheme_Object **argv) {
  return mx_do_get_methods(argc,argv,INVOKE_PROPERTYGET);
}

Scheme_Object *mx_com_set_properties(int argc,Scheme_Object **argv) {
  return mx_do_get_methods(argc,argv,INVOKE_PROPERTYPUT);
}

VARTYPE getVarTypeFromElemDesc(ELEMDESC *pElemDesc) {
  unsigned short flags;
  
  flags = pElemDesc->paramdesc.wParamFlags;
  
  if ((flags & PARAMFLAG_FOPT) && (flags & PARAMFLAG_FHASDEFAULT))  {
  
    // use type of default value

    return pElemDesc->paramdesc.pparamdescex->varDefaultValue.vt;
  }

  if (pElemDesc->tdesc.vt == VT_PTR) {
    return pElemDesc->tdesc.lptdesc->vt | VT_BYREF;
  }

  return pElemDesc->tdesc.vt;
  
}

Scheme_Object *makeBoxType(Scheme_Object *theType) {
  static Scheme_Object *theBox;
  static BOOL init;

  if (init == FALSE) {
    theBox = scheme_intern_symbol("box");
    init = TRUE;
  }

  return scheme_make_pair(theBox,
			  scheme_make_pair(theType,scheme_null));
}

Scheme_Object *newTypeSymbol(char *s) {
  Scheme_Object *retval;
  retval = scheme_intern_symbol(s);
  scheme_register_extension_global(retval,sizeof(Scheme_Object));

  return retval;
}

Scheme_Object *elemDescToSchemeType(ELEMDESC *pElemDesc,BOOL ignoreByRef) {
  static char buff[256];
  VARTYPE vt;
  static Scheme_Object 
    *voidSymbol,*charSymbol,*shortIntSymbol,
    *intSymbol,*floatSymbol,*doubleSymbol,
    *stringSymbol,*currencySymbol,*dateSymbol,
    *booleanSymbol,*scodeSymbol,*unknownSymbol,
    *comObjectSymbol,*anySymbol;
  static BOOL init;

  if (init == FALSE) {
    voidSymbol = newTypeSymbol("void");
    charSymbol = newTypeSymbol("char");
    shortIntSymbol = newTypeSymbol("short-int");
    intSymbol = newTypeSymbol("int");
    floatSymbol = newTypeSymbol("float"); 
    doubleSymbol = newTypeSymbol("double");
    stringSymbol = newTypeSymbol("string");
    currencySymbol = newTypeSymbol("mx-currency");
    dateSymbol = newTypeSymbol("mx-date");
    booleanSymbol = newTypeSymbol("boolean");
    scodeSymbol = newTypeSymbol("mx-scode");
    unknownSymbol = newTypeSymbol("mx-unknown-com-object");
    comObjectSymbol = newTypeSymbol("mx-com-object");
    anySymbol = newTypeSymbol("mx-any");

    init = TRUE;
  }

  vt = getVarTypeFromElemDesc(pElemDesc);

  if (ignoreByRef) {
    vt &= ~VT_BYREF;
  }

  switch(vt) {

  case VT_HRESULT :
  case VT_NULL :

    return voidSymbol;

  case VT_UI1 :

    return charSymbol;
  
  case VT_UI1 | VT_BYREF :

    return makeBoxType(charSymbol);

  case VT_I2 :
    
    return shortIntSymbol;

  case VT_I2 + VT_BYREF :

    return makeBoxType(shortIntSymbol);
  
  case VT_I4 :

    return intSymbol;

  case VT_I4 | VT_BYREF:

    return makeBoxType(intSymbol);
             
  case VT_R4 :

    return floatSymbol;

  case VT_R4 | VT_BYREF :

    return makeBoxType(floatSymbol);

  case VT_R8 :

    return doubleSymbol;

  case VT_R8 | VT_BYREF :

    return makeBoxType(doubleSymbol);

  case VT_BSTR :
    
    return stringSymbol;
    
  case VT_BSTR | VT_BYREF :
    
    return makeBoxType(stringSymbol);
    
  case VT_CY :

    return currencySymbol;

  case VT_CY | VT_BYREF :

    return makeBoxType(currencySymbol);

  case VT_DATE :

    return dateSymbol;
      
  case VT_DATE | VT_BYREF :

    return makeBoxType(dateSymbol);
      
  case VT_BOOL :

    return booleanSymbol;

  case VT_BOOL | VT_BYREF :

    return makeBoxType(booleanSymbol);

  case VT_ERROR :
    
    return scodeSymbol;
     
  case VT_ERROR | VT_BYREF:
    
    return makeBoxType(scodeSymbol);
     
  case VT_UNKNOWN :
  
    return unknownSymbol;

  case VT_UNKNOWN | VT_BYREF :
  
    return makeBoxType(unknownSymbol);

  case VT_DISPATCH :

    return comObjectSymbol;

  case VT_DISPATCH | VT_BYREF :

    return makeBoxType(comObjectSymbol);
    
  case VT_VARIANT : 

    return anySymbol;

  case VT_VARIANT | VT_BYREF : 

    return makeBoxType(anySymbol);

  default :

    { char buff[32];
    sprintf(buff,"COM-0x%X",vt);
    return scheme_intern_symbol(buff);
    }
  }
}


Scheme_Object *mx_make_function_type(Scheme_Object *paramTypes,
				     Scheme_Object *returnType) {
  Scheme_Object *arrow;

  arrow = scheme_intern_symbol("->");

  return scheme_append(paramTypes,
		       scheme_make_pair(arrow,
					scheme_make_pair(returnType,
							 scheme_null)));
}

Scheme_Object *mx_do_get_method_type(int argc,Scheme_Object **argv,
				     INVOKEKIND invKind) {
  IDispatch *pIDispatch;
  MX_TYPEDESC *pTypeDesc;
  FUNCDESC *pFuncDesc;
  VARDESC *pVarDesc;
  Scheme_Object *s,*paramTypes,*returnType;
  char *name;
  short int numActualParams;
  int i;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type("mx-method-type","mx-object",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("mx-method-type","string",1,argc,argv);
  }

  pIDispatch = MX_COM_OBJ_VAL(argv[0]);

  if (pIDispatch == NULL) {
    scheme_signal_error("NULL COM object");
  }

  name = SCHEME_STR_VAL(argv[1]);

  pTypeDesc = getMethodType(pIDispatch,name,invKind);

  if (pTypeDesc->descKind == funcDesc) {

    pFuncDesc = pTypeDesc->pFuncDesc;
  
    paramTypes = scheme_null;

    numActualParams = pFuncDesc->cParams;

    if ((invKind == INVOKE_PROPERTYGET || invKind == INVOKE_FUNC) && 
	pFuncDesc->cParams > 0) {
      numActualParams--; 
    }

    for (i = 0; i < numActualParams; i++) {
      s = elemDescToSchemeType(&pFuncDesc->lprgelemdescParam[i],FALSE);
      paramTypes = scheme_make_pair(s,paramTypes);
    }
  }

  // if not a function type, distinguish varDesc's 
  // by invKind

  else if (invKind == INVOKE_PROPERTYGET) {
    pVarDesc = pTypeDesc->pVarDesc;
    paramTypes = scheme_null;
    numActualParams = 0;
  }
  else if (invKind == INVOKE_PROPERTYPUT) {
    pVarDesc = pTypeDesc->pVarDesc;
    paramTypes = 
      scheme_make_pair(elemDescToSchemeType(&pVarDesc->elemdescVar,FALSE),
		       scheme_null);
    numActualParams = 1;
  }

  switch(invKind) {
  
  case INVOKE_FUNC :

    // assume pTypeDesc->descKind is funcDesc

    if (pFuncDesc->cParams == 0) {
      returnType = scheme_intern_symbol("void");
    }
    else {
      returnType = elemDescToSchemeType(&pFuncDesc->lprgelemdescParam[numActualParams],TRUE);
    }
    break;

  case INVOKE_PROPERTYPUT :
    returnType = scheme_intern_symbol("void");
    break;
  
  case INVOKE_PROPERTYGET :

    // pTypeDesc->descKind may be either funcDesc or varDesc

    if (pTypeDesc->descKind == funcDesc) {

      if (pFuncDesc->cParams == 0) {
	returnType = scheme_intern_symbol("void");
      }
      else {
	returnType = elemDescToSchemeType(&pFuncDesc->lprgelemdescParam[numActualParams],TRUE);
      }

    }
    else { // pTypeDesc->descKind == varDesc
	returnType = elemDescToSchemeType(&pVarDesc->elemdescVar,TRUE);
    }

    break;
  }

  return mx_make_function_type(paramTypes,returnType);

}


Scheme_Object *mx_com_method_type(int argc,Scheme_Object **argv) {
  return mx_do_get_method_type(argc,argv,INVOKE_FUNC);
}

Scheme_Object *mx_com_get_property_type(int argc,Scheme_Object **argv) {
  return mx_do_get_method_type(argc,argv,INVOKE_PROPERTYGET);
}

Scheme_Object *mx_com_set_property_type(int argc,Scheme_Object **argv) {
  return mx_do_get_method_type(argc,argv,INVOKE_PROPERTYPUT);
}

BOOL schemeValueFitsVarType(Scheme_Object *val,VARTYPE vt) {
  long int longInt;

  switch (vt) {

  case VT_NULL :

    return SCHEME_VOIDP(val);

  case VT_UI1 :

    return SCHEME_CHARP(val);
  
  case VT_I2 :
    
    return SCHEME_INTP(val) && 
      scheme_get_int_val(val,&longInt) && 
      (longInt & 0xFFFF0000) == 0L;
  
  case VT_I4 :

    return SCHEME_EXACT_INTEGERP(val) && 
      scheme_get_int_val(val,&longInt);
             
  case VT_R4 :

    return SCHEME_FLTP(val) || 
      (SCHEME_DBLP(val) &&
       SCHEME_DBL_VAL(val) >= FLT_MIN &&
       SCHEME_DBL_VAL(val) <= FLT_MAX);

  case VT_R8 :

    return SCHEME_DBLP(val);

  case VT_BSTR :
    
    return SCHEME_STRINGP(val);
    
  case VT_CY :

    return MX_CYP(val);

  case VT_DATE :

    return MX_DATEP(val);
      
  case VT_BOOL :

    return TRUE; // ain't Scheme great

  case VT_ERROR :
    
    return MX_SCODEP(val);
     
  case VT_UNKNOWN :
  
    return MX_IUNKNOWNP(val);

  case VT_VARIANT : // we can package anything into a VARIANTARG

    return TRUE;

  default :

    return FALSE;
	
  }
}


BOOL subArrayFitsVarType(Scheme_Object *val,
                         unsigned short numDims,SAFEARRAYBOUND *bounds,
                         VARTYPE vt) {
  Scheme_Object **els;
  unsigned long len;
  
  if (SCHEME_VECTORP(val) == FALSE) {
    return FALSE;
  }
  
  len = SCHEME_VEC_SIZE(val);

  if (len != bounds->cElements) {
    return FALSE;
  }

  els = SCHEME_VEC_ELS(val);

  if (numDims == 1) { // innermost vector
    for (unsigned long i = 0; i < len; i++) {
      if (schemeValueFitsVarType(els[i],vt) == FALSE) {
        return FALSE;
      }
    }
  }
  else {
    for (unsigned long i = 0; i < len; i++) {

      // recursion, the programmer's best friend

      if (subArrayFitsVarType(els[i],numDims - 1,bounds + 1,vt) == FALSE) {
        return FALSE;
      }
    } 
  }

  return TRUE;
}

BOOL schemeValueFitsElemDesc(Scheme_Object *val,ELEMDESC *pElemDesc) {
  unsigned short flags;

  // if default available, check value has appropriate type

  flags = pElemDesc->paramdesc.wParamFlags;
  if ((flags & PARAMFLAG_FOPT) && (flags & PARAMFLAG_FHASDEFAULT))  {
    return schemeValueFitsVarType(val,pElemDesc->paramdesc.pparamdescex->varDefaultValue.vt);
  }

  // if array, check we have a vector of proper dimension and contained types 
  
  if (pElemDesc->tdesc.vt & VT_ARRAY) {
    return subArrayFitsVarType(val,
                               pElemDesc->tdesc.lpadesc->cDims,
                               pElemDesc->tdesc.lpadesc->rgbounds,
                               pElemDesc->tdesc.lpadesc->tdescElem.vt);
                    
  }

  
  // if box, check the contained value

  if (pElemDesc->tdesc.vt == VT_PTR) {
    if (SCHEME_BOXP(val) == FALSE) {
      return FALSE;
    }

    return schemeValueFitsVarType(SCHEME_BOX_VAL(val),pElemDesc->tdesc.lptdesc->vt);
  }
  
  // not array or box or default value
  
  return schemeValueFitsVarType(val,pElemDesc->tdesc.vt);
}

VARIANT_BOOL schemeValToBool(Scheme_Object *val) {
  return SCHEME_FALSEP(val) ? 0 : 0xFFFF;
}

BSTR stringToBSTR(char *s,size_t len) {
  HRESULT hr;
  BSTR bstr;
  WCHAR *unicodeString;

  unicodeString = (WCHAR *)scheme_malloc((len + 1) * sizeof(WCHAR));
  scheme_dont_gc_ptr(unicodeString);

  hr = MultiByteToWideChar(CP_ACP,(DWORD)0,s,len + 1,
			   unicodeString,len + 1);

  scheme_gc_ptr_ok(unicodeString);

  if (hr == 0) {
    scheme_signal_error("Error translating string parameter to Unicode");
  }

  bstr = SysAllocString(unicodeString);

  if (bstr == NULL) {
    scheme_signal_error("Error allocating string parameter");
  }

  return bstr;
}

BSTR schemeStringToBSTR(Scheme_Object *o) {
  return stringToBSTR(SCHEME_STR_VAL(o),SCHEME_STRLEN_VAL(o));
}

VARTYPE schemeValueToVarType(Scheme_Object *obj) {

  // test for global constants

  if (SCHEME_FALSEP(obj)) {
    return VT_BOOL;
  }

  if (SCHEME_VOIDP(obj)) {
    return VT_NULL;
  }

  // otherwise, dispatch on value type

  switch(obj->type) {
  case scheme_char_type :
    return VT_UI1;
  case scheme_integer_type :
    return VT_I4;
  case scheme_float_type :
    return VT_R4;
  case scheme_double_type :
    return VT_R8;
  case scheme_string_type :
    return VT_BSTR;
  case scheme_vector_type : // may need to specify elt type
    return VT_ARRAY;
  }

  scheme_signal_error("Unable to coerce value to VARIANT");

  return 0; // keep compiler happy
}

Scheme_Object *BSTRToSchemeString(BSTR bstr) {
  char *buff;
  unsigned int len;

  len = SysStringLen(bstr);

  buff = (char *)scheme_malloc(len + 1);

  WideCharToMultiByte(CP_ACP,(DWORD)0,bstr,len + 1,
		      buff,len,
		      NULL,NULL);
 
  return scheme_make_string(buff);
}

void updateSchemeStringFromBSTR(Scheme_Object *val,BSTR bstr) {
  int len;

  len = SysStringLen(bstr);

  if (len > SCHEME_STRLEN_VAL(val)) {
    scheme_signal_error("String updated with longer string");
  }

  WideCharToMultiByte(CP_ACP,(DWORD)0,
		      bstr,len + 1,
		      SCHEME_STR_VAL(val),SCHEME_STRLEN_VAL(val),
		      NULL,NULL);

  SCHEME_STRLEN_VAL(val) = len;

}

void *allocParamMemory(size_t n) {
  void *retval;

  // do we need a semaphore here?

  retval = scheme_malloc(n);
  scheme_dont_gc_ptr(retval);

  return retval;
}

void marshallSchemeValueToVariant(Scheme_Object *val,VARIANTARG *pVariantArg) {

  // called when COM type spec allows any VARIANT

  if (SCHEME_CHARP(val)) {
    pVariantArg->vt = VT_UI1;
    pVariantArg->bVal = SCHEME_CHAR_VAL(val);
    return;
  }

  if (SCHEME_INTP(val)) {
    pVariantArg->vt = VT_I4;
    pVariantArg->lVal = SCHEME_INT_VAL(val);
    return;
  }

#ifdef MZ_USE_SINGLE_FLOATS    
  if (SCHEME_FLTP(val)) {
    pVariantArg->vt = VT_R4;
    pVariantArg->fltVal = SCHEME_FLT_VAL(val);
    return;
  }
#endif
    
  if (SCHEME_DBLP(val)) {
    pVariantArg->vt = VT_R8;
    pVariantArg->dblVal = SCHEME_DBL_VAL(val);
    return;
  }

  if (SCHEME_STRINGP(val)) {
    pVariantArg->vt = VT_BSTR;
    pVariantArg->bstrVal = schemeStringToBSTR(val);
    return;
  }

  if (MX_CYP(val)) {
    pVariantArg->vt = VT_CY;
    pVariantArg->cyVal = MX_CY_VAL(val);
    return;
  }

  if (MX_DATEP(val)) {
    pVariantArg->vt = VT_DATE;
    pVariantArg->date = MX_DATE_VAL(val);
    return;
  }

  if (val == scheme_false) {
    pVariantArg->vt = VT_BOOL;
    pVariantArg->boolVal = 0;
    return;
  }

  if (val == scheme_true) {
    pVariantArg->vt = VT_BOOL;
    pVariantArg->boolVal = -1;
    return;
  }

  if (MX_SCODEP(val)) {
    pVariantArg->vt = VT_ERROR;
    pVariantArg->scode = MX_SCODE_VAL(val);
    return;
  }

  if (MX_IUNKNOWNP(val)) {
    pVariantArg->vt = VT_UNKNOWN;
    pVariantArg->punkVal = MX_IUNKNOWN_VAL(val);
    return;
  }

  scheme_signal_error("Unable to inject Scheme value into VARIANT");

}

void marshallSchemeValue(Scheme_Object *val,VARIANTARG *pVariantArg) {

  if (pVariantArg->vt & VT_ARRAY) {
    scheme_signal_error("Array marshalling not implemented yet");
  }

  switch (pVariantArg->vt) {

  case VT_NULL :

    break;
     
  case VT_UI1 :
    
    pVariantArg->bVal = SCHEME_CHAR_VAL(val);
    break;
      
  case VT_UI1 | VT_BYREF :

    pVariantArg->pbVal = (unsigned char *)allocParamMemory(sizeof(unsigned char));
    *pVariantArg->pbVal = (unsigned char)SCHEME_CHAR_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_I2 :
  
    pVariantArg->iVal = (short)SCHEME_INT_VAL(val);
    break;

  case VT_I2 | VT_BYREF :
  
    pVariantArg->piVal = (short *)allocParamMemory(sizeof(short));
    *pVariantArg->piVal = (short)SCHEME_INT_VAL(SCHEME_BOX_VAL(val));

    break;

  case VT_I4 :

    pVariantArg->lVal = SCHEME_INT_VAL(val);
    break;

  case VT_I4 | VT_BYREF :

    pVariantArg->plVal = (long *)allocParamMemory(sizeof(long));
    *pVariantArg->plVal = (long)SCHEME_INT_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_R4 :

    pVariantArg->fltVal = (float)SCHEME_DBL_VAL(val);
    break;

  case VT_R4 | VT_BYREF :

    pVariantArg->pfltVal = (float *)allocParamMemory(sizeof(float));
    *pVariantArg->pfltVal = (float)SCHEME_DBL_VAL(SCHEME_BOX_VAL(val));
    break;

  case VT_R8 :

    pVariantArg->dblVal = SCHEME_DBL_VAL(val);
    break;

  case VT_R8 | VT_BYREF :

    pVariantArg->pdblVal = (double *)allocParamMemory(sizeof(double));
    *pVariantArg->pdblVal = (double)SCHEME_DBL_VAL(SCHEME_BOX_VAL(val));
    break;

  case VT_BSTR :

    pVariantArg->bstrVal = schemeStringToBSTR(val);
    break;

  case VT_BSTR | VT_BYREF :
    
    pVariantArg->pbstrVal = (BSTR *)allocParamMemory(sizeof(BSTR));
    *pVariantArg->pbstrVal = schemeStringToBSTR(val);
    break;
      
  case VT_CY :

    pVariantArg->cyVal = MX_CY_VAL(val);
    break;

  case VT_CY | VT_BYREF :

    pVariantArg->pcyVal = (CY *)allocParamMemory(sizeof(CY));
    *pVariantArg->pcyVal = (CY)MX_CY_VAL(val);
    break;

  case VT_DATE :

    pVariantArg->date = MX_DATE_VAL(val);
    break;

  case VT_DATE | VT_BYREF :

    pVariantArg->pdate = (DATE *)allocParamMemory(sizeof(DATE));
    *pVariantArg->pdate = (DATE)MX_DATE_VAL(val);
    break;

  case VT_BOOL :

    pVariantArg->boolVal = schemeValToBool(val);
    break;

  case VT_BOOL | VT_BYREF :

    pVariantArg->pboolVal = (VARIANT_BOOL *)allocParamMemory(sizeof(VARIANT_BOOL));
    *pVariantArg->pboolVal = schemeValToBool(val);
    break;

  case VT_ERROR :
    
    pVariantArg->scode = MX_SCODE_VAL(val);
    break;

  case VT_ERROR | VT_BYREF :
    
    pVariantArg->pscode = (SCODE *)allocParamMemory(sizeof(SCODE));
    *pVariantArg->pscode = MX_SCODE_VAL(SCHEME_BOX_VAL(val));
    break;

  case VT_DISPATCH :

    pVariantArg->pdispVal = MX_COM_OBJ_VAL(val);
    break;

  case VT_DISPATCH | VT_BYREF :

    pVariantArg->ppdispVal = (IDispatch **)allocParamMemory(sizeof(IDispatch *));
    *pVariantArg->ppdispVal = MX_COM_OBJ_VAL(SCHEME_BOX_VAL(val));
    break;
    
  case VT_VARIANT | VT_BYREF :
  
    // pass boxed value of almost-arbitrary type

    pVariantArg->pvarVal = (VARIANTARG *)allocParamMemory(sizeof(VARIANTARG));
    pVariantArg->pvarVal->vt = schemeValueToVarType(val);
    marshallSchemeValue(SCHEME_BOX_VAL(val),pVariantArg->pvarVal);
    break;

  case VT_UNKNOWN :
    pVariantArg->punkVal = MX_IUNKNOWN_VAL(val);
    break;

  case VT_UNKNOWN | VT_BYREF :
    pVariantArg->ppunkVal = (IUnknown **)allocParamMemory(sizeof(IUnknown *));
    *pVariantArg->ppunkVal = MX_IUNKNOWN_VAL(SCHEME_BOX_VAL(val));
    break;

  case VT_VARIANT :
    marshallSchemeValueToVariant(val,pVariantArg);
    break;

  default :
    scheme_signal_error("Unable to marshall Scheme value into VARIANT: 0x%X",pVariantArg->vt);

  }
}

Scheme_Object *variantToSchemeObject(VARIANTARG *pVariantArg) {

  switch(pVariantArg->vt) {

  case VT_EMPTY :
  case VT_NULL :

    return scheme_void;

  case VT_UI1 :

    return scheme_make_character((char)(pVariantArg->bVal));

  case VT_I2 :

    return scheme_make_integer(pVariantArg->iVal);

  case VT_I4 :
  
    return scheme_make_integer(pVariantArg->lVal);

  case VT_R4 :

#ifdef MZ_USE_SINGLE_FLOATS
    return scheme_make_float(pVariantArg->fltVal);
#else
    return scheme_make_double((double)(pVariantArg->fltVal));
#endif

  case VT_R8 :

    return scheme_make_double(pVariantArg->dblVal);

  case VT_BSTR :

    return BSTRToSchemeString(pVariantArg->bstrVal);

  case VT_CY :

    return mx_make_cy(&pVariantArg->cyVal);

  case VT_DATE :

    return mx_make_date(&pVariantArg->date);

  case VT_BOOL :

    return mx_make_bool(pVariantArg->boolVal);

  case VT_ERROR :
    
    return mx_make_scode(pVariantArg->scode);

  case VT_DISPATCH :

    return mx_make_idispatch(pVariantArg->pdispVal);
    
  case VT_UNKNOWN :

    return mx_make_iunknown(pVariantArg->punkVal);

  default :
    
    scheme_signal_error("Can't make Scheme value from VARIANT %X",
			pVariantArg->vt);

  }

  return NULL;
}

void unmarshallVariant(Scheme_Object *val,VARIANTARG *pVariantArg) {

  switch(pVariantArg->vt) {

  case VT_UI1 | VT_BYREF :

    SCHEME_BOX_VAL(val) = scheme_make_character((char)(*pVariantArg->pbVal));
    scheme_gc_ptr_ok(pVariantArg->pbVal);
    break;

  case VT_I2 | VT_BYREF :

    SCHEME_BOX_VAL(val) = scheme_make_integer(*pVariantArg->piVal);
    scheme_gc_ptr_ok(pVariantArg->piVal);
    break;

  case VT_I4 | VT_BYREF :

    SCHEME_BOX_VAL(val) = scheme_make_integer(*pVariantArg->plVal);
    scheme_gc_ptr_ok(pVariantArg->plVal);
    break;

  case VT_R4 | VT_BYREF :

#ifdef MZ_USE_SINGLE_FLOATS
    SCHEME_BOX_VAL(val) = scheme_make_float(*pVariantArg->pfltVal);
#else
    SCHEME_BOX_VAL(val) = scheme_make_double((double)(*pVariantArg->pfltVal));
#endif
    scheme_gc_ptr_ok(pVariantArg->pfltVal);
    break;

  case VT_R8 | VT_BYREF :

    SCHEME_BOX_VAL(val) = scheme_make_double(*pVariantArg->pdblVal);
    scheme_gc_ptr_ok(pVariantArg->pdblVal);
    break;

  case VT_CY | VT_BYREF :

    SCHEME_BOX_VAL(val) = mx_make_cy(pVariantArg->pcyVal);
    scheme_gc_ptr_ok(pVariantArg->pcyVal);
    break;

  case VT_DATE | VT_BYREF :

    SCHEME_BOX_VAL(val) = mx_make_date(pVariantArg->pdate);
    scheme_gc_ptr_ok(pVariantArg->pdate);
    break;

  case VT_BOOL | VT_BYREF :
 
    SCHEME_BOX_VAL(val) = mx_make_bool(*pVariantArg->pboolVal);
    scheme_gc_ptr_ok(pVariantArg->pboolVal);
    break;

  case VT_ERROR | VT_BYREF :

    SCHEME_BOX_VAL(val) = mx_make_scode(*pVariantArg->pscode);
    scheme_gc_ptr_ok(pVariantArg->pscode);
    break;

  case VT_DISPATCH | VT_BYREF :

    SCHEME_BOX_VAL(val) = mx_make_idispatch(*pVariantArg->ppdispVal);
    scheme_gc_ptr_ok(pVariantArg->ppdispVal);
    break;

  case VT_UNKNOWN | VT_BYREF :

    SCHEME_BOX_VAL(val) = mx_make_iunknown(*pVariantArg->ppunkVal);
    scheme_gc_ptr_ok(pVariantArg->ppunkVal);
    break;

  case VT_VARIANT | VT_BYREF :

    SCHEME_BOX_VAL(val) = variantToSchemeObject(pVariantArg->pvarVal);
    scheme_gc_ptr_ok(pVariantArg->pvarVal);
    break;

  case VT_BSTR :
    
    updateSchemeStringFromBSTR(val,pVariantArg->bstrVal);
    SysFreeString(pVariantArg->bstrVal);
    break;

  case VT_BSTR | VT_BYREF :

    SCHEME_BOX_VAL(val) = BSTRToSchemeString(*pVariantArg->pbstrVal);
    SysFreeString(*pVariantArg->pbstrVal);
    scheme_gc_ptr_ok(pVariantArg->pbstrVal);
    break;

  default :

   ;   
    
    // no unmarshalling or cleanup needed

  }
}

short int buildMethodArgumentsUsingFuncDesc(FUNCDESC *pFuncDesc,
					    INVOKEKIND invKind,
					    int argc,Scheme_Object **argv,
					    DISPPARAMS *methodArguments) {
  char errBuff[256];
  short int numNeededParams;
  int i,j,k;
  static DISPID dispidPropPut = DISPID_PROPERTYPUT;

  numNeededParams = pFuncDesc->cParams;

  if ((invKind == INVOKE_PROPERTYGET || invKind == INVOKE_FUNC) 
      && pFuncDesc->cParams > 0) { 
    // last parameter is retval
    numNeededParams--;
  }

  if (argc != numNeededParams + 2) {
    sprintf(errBuff,"%s (%s \"%s\")",
	    mx_fun_string(invKind),
	    inv_kind_string(invKind),
	    SCHEME_STR_VAL(argv[1]));
    scheme_wrong_count(errBuff,numNeededParams+2,numNeededParams+2,argc,argv);
  }

  for (i = 0,j = numNeededParams - 1,k = 2; i < numNeededParams; i++,j--,k++) {

    // i = index of ELEMDESC's
    // j = index of VARIANTARG's
    // k = index of actual args in argv
    
    if (schemeValueFitsElemDesc(argv[k],&pFuncDesc->lprgelemdescParam[i]) == FALSE) {
      sprintf(errBuff,"%s (%s \"%s\")",mx_fun_string(invKind),
	      inv_kind_string(invKind),SCHEME_STR_VAL(argv[1]));
      scheme_wrong_type(errBuff,
			SCHEME_SYM_VAL(elemDescToSchemeType(&(pFuncDesc->lprgelemdescParam[i]),FALSE)),
			k,argc,argv);
    }
  }
  
  switch(invKind) {

  case INVOKE_PROPERTYPUT :

    // Named argument represents the assigned value

    methodArguments->rgdispidNamedArgs = &dispidPropPut;
    methodArguments->cNamedArgs = methodArguments->cArgs = 1;
    break;

  case INVOKE_PROPERTYGET :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numNeededParams;
    break;

  default :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numNeededParams;
    break;

  }

  if (numNeededParams > 0) {
    methodArguments->rgvarg = 
      (VARIANTARG *)scheme_malloc(numNeededParams * sizeof(VARIANTARG));
    scheme_dont_gc_ptr(methodArguments->rgvarg);
  }

  // marshall Scheme argument list into COM argument list
  // arguments are in reverse order in rgvarg

  for (i = 0,j = numNeededParams - 1,k = 2; i < numNeededParams; i++,j--,k++) {

    // i = index of ELEMDESC's
    // j = index of VARIANTARG's

    VariantInit(&methodArguments->rgvarg[j]);
    methodArguments->rgvarg[j].vt = 
      getVarTypeFromElemDesc(&pFuncDesc->lprgelemdescParam[i]);
    marshallSchemeValue(argv[k],&methodArguments->rgvarg[j]);
  }

  return numNeededParams;
}

short int buildMethodArgumentsUsingVarDesc(VARDESC *pVarDesc,
					   INVOKEKIND invKind,
					   int argc,Scheme_Object **argv,
					   DISPPARAMS *methodArguments) {
  char errBuff[256];
  short int numNeededParams;
  int i,j,k;
  static DISPID dispidPropPut = DISPID_PROPERTYPUT;

  if (invKind == INVOKE_PROPERTYGET) {
    numNeededParams = 0;
  }
  else if (invKind == INVOKE_PROPERTYPUT) {
    numNeededParams = 1;
  }

  if (argc != numNeededParams + 2) {
    sprintf(errBuff,"%s (%s \"%s\")",
	    mx_fun_string(invKind),
	    inv_kind_string(invKind),
	    SCHEME_STR_VAL(argv[1]));
    scheme_wrong_count(errBuff,
		       numNeededParams + 2,numNeededParams + 2,
		       argc,argv);
  }

  switch(invKind) {

  case INVOKE_PROPERTYPUT :

    methodArguments->rgdispidNamedArgs = &dispidPropPut;
    methodArguments->cNamedArgs = methodArguments->cArgs = 1;
    break;

  case INVOKE_PROPERTYGET :

    methodArguments->rgdispidNamedArgs = NULL;
    methodArguments->cNamedArgs = 0;
    methodArguments->cArgs = numNeededParams;
    break;

  }

  if (numNeededParams > 0) {
    methodArguments->rgvarg = 
      (VARIANTARG *)scheme_malloc(numNeededParams * sizeof(VARIANTARG));
    scheme_dont_gc_ptr(methodArguments->rgvarg);
  }

  // marshall Scheme argument list into COM argument list

  for (i = 0,j = numNeededParams - 1,k = 2; i < numNeededParams; i++,j--,k++) {

    // i = index of ELEMDESC's
    // j = index of VARIANTARG's

    VariantInit(&methodArguments->rgvarg[j]);
    methodArguments->rgvarg[j].vt = 
      getVarTypeFromElemDesc(&pVarDesc->elemdescVar);
    marshallSchemeValue(argv[k],&methodArguments->rgvarg[j]);
  }

  return numNeededParams;
}

short int buildMethodArguments(MX_TYPEDESC *pTypeDesc,
			       INVOKEKIND invKind,
			       int argc,Scheme_Object **argv,
			       DISPPARAMS *methodArguments) {
  if (pTypeDesc->descKind == funcDesc) {
    return buildMethodArgumentsUsingFuncDesc(pTypeDesc->pFuncDesc,
					     invKind,argc,argv,
					     methodArguments);
  }

  return buildMethodArgumentsUsingVarDesc(pTypeDesc->pVarDesc,
					  invKind,argc,argv,
					  methodArguments);
			       
}

static Scheme_Object *mx_make_call(int argc,Scheme_Object **argv,
				   INVOKEKIND invKind) {
  MX_TYPEDESC *pTypeDesc;
  DISPPARAMS methodArguments; 
  VARIANT methodResult;
  EXCEPINFO exnInfo;
  unsigned int errorIndex;
  IDispatch *pIDispatch;
  char *name;
  short numNeededParams;
  int i,k;
  HRESULT hr;

  if (MX_COM_OBJP(argv[0]) == FALSE) {
    scheme_wrong_type(mx_fun_string(invKind),"com-object",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type(mx_fun_string(invKind),"string",1,argc,argv);
  }
 
  pIDispatch = MX_COM_OBJ_VAL(argv[0]);

  if (pIDispatch == NULL) {
    scheme_signal_error("NULL COM object");
  }
  
  name = SCHEME_STR_VAL(argv[1]);

  // check arity, types of method arguments
  
  pTypeDesc = getMethodType(pIDispatch,name,invKind);

  numNeededParams = buildMethodArguments(pTypeDesc,
					 invKind,
					 argc,argv,
					 &methodArguments);
 
  if (invKind != INVOKE_PROPERTYPUT) {
    VariantInit(&methodResult);
  }

  // invoke requested method

  hr = pIDispatch->Invoke(pTypeDesc->memID,IID_NULL,LOCALE_SYSTEM_DEFAULT,
			  invKind,
			  &methodArguments,
			  (invKind == INVOKE_PROPERTYPUT) ? NULL : &methodResult,
			  &exnInfo,
			  &errorIndex);

  if (hr == DISP_E_EXCEPTION) {
    char errBuff[2048];
    char description[1024];
    BOOL hasErrorCode;
    BOOL hasDescription;

    hasErrorCode = (exnInfo.wCode > 0); 
    hasDescription = (exnInfo.bstrDescription != NULL);

    if (hasDescription) {
      WideCharToMultiByte(CP_ACP,(DWORD)0,
			  exnInfo.bstrSource,
			  SysStringLen(exnInfo.bstrDescription)+1,
			  description,sizeof(description)-1,
			  NULL,NULL);
    }

    sprintf(errBuff,
	    "COM object exception, %s 0x%X%s%s",
	    hasErrorCode ? "error code" : "SCODE",
	    hasErrorCode ? exnInfo.wCode : exnInfo.scode,
	    hasDescription ? "\nDescription: " : "" ,
	    hasDescription ? description : "");

    scheme_signal_error(errBuff);
  }

  if (hr != S_OK) {
    scheme_signal_error("\"%s\" (%s) failed with code 0x%lX",
			SCHEME_STR_VAL(argv[1]),inv_kind_string(invKind),
			hr);
  }

  // unmarshall data passed by reference, cleanup

  for (i = 0,k = 2; i < numNeededParams; i++,k++) {
    unmarshallVariant(argv[k],&methodArguments.rgvarg[i]);
  }
 
  if (numNeededParams > 0) {
    scheme_gc_ptr_ok(methodArguments.rgvarg);
  }  
  
  if (invKind == INVOKE_PROPERTYPUT) {
    return scheme_void;
  }

  // unmarshall return value

  return variantToSchemeObject(&methodResult);
}

BOOL _stdcall drawContinue(DWORD data) {
  return TRUE;
}

Scheme_Object *mx_com_invoke(int argc,Scheme_Object **argv) {
  return mx_make_call(argc,argv,INVOKE_FUNC);
}

Scheme_Object *mx_com_get_property(int argc,Scheme_Object **argv) {
  return mx_make_call(argc,argv,INVOKE_PROPERTYGET); 
}

Scheme_Object *mx_com_set_property(int argc,Scheme_Object **argv) {
  return mx_make_call(argc,argv,INVOKE_PROPERTYPUT); 
}

Scheme_Object *mx_all_clsid(int argc,Scheme_Object **argv,char **attributes) {
  LONG result;
  Scheme_Object *retval;
  HKEY hkey,hsubkey;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsidBuffer[256];
  DWORD clsidBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  BOOL loopFlag;
  char **p;

  retval = scheme_null;
	
  result = RegOpenKeyEx(HKEY_CLASSES_ROOT,
			"CLSID",
			(DWORD)0,
			KEY_READ,
			&hkey);
	
  if (result != ERROR_SUCCESS) {
    return retval;
  }	    

  // enumerate subkeys until we find the one we want

  keyIndex = 0;

  while (1) {

    // get next subkey
      
    clsidBufferSize = sizeray(clsidBuffer);

    result = RegEnumKeyEx(hkey,keyIndex++,
			  clsidBuffer,
			  &clsidBufferSize,
			  0,NULL,NULL,
			  &fileTime);
  	
    if (result == ERROR_NO_MORE_ITEMS) {
      break;
    }		

    if (strlen(clsidBuffer) != 38) { // not a CLSID -- bogus entry
      continue;
    }

    // open subkey

    result = RegOpenKeyEx(hkey,clsidBuffer,
			  (DWORD)0,
			  KEY_READ,&hsubkey);

    if (result != ERROR_SUCCESS) {
      scheme_signal_error("Error while searching Windows registry");
    }	    

    dataBufferSize = sizeof(dataBuffer);
       
    RegQueryValueEx(hsubkey,"",0,&dataType,dataBuffer,&dataBufferSize);

    if (dataType == REG_SZ) {
      int subkeyIndex;
      TCHAR subkeyBuffer[256];
      DWORD subkeyBufferSize;

      subkeyIndex = 0;

      loopFlag = TRUE;

      while (loopFlag) {

	subkeyBufferSize = sizeray(subkeyBuffer);

	result = RegEnumKeyEx(hsubkey,subkeyIndex++,
			      subkeyBuffer,
			      &subkeyBufferSize,
			      0,NULL,NULL,
			      &fileTime);

	if (result == ERROR_NO_MORE_ITEMS) {
	  break;
	}
  	
	p = attributes;

	while (*p) {
	  if (stricmp(subkeyBuffer,*p) == 0) {
	    retval = scheme_make_pair(scheme_make_string((char *)dataBuffer),
				      retval);
	    loopFlag = FALSE;
	    break; // *p loop
	  }
	  p++;
	}
      }
    }

    RegCloseKey(hsubkey);  
  } 

  RegCloseKey(hkey);  
    
  return retval;
}

Scheme_Object *mx_all_controls(int argc,Scheme_Object **argv) {
  return mx_all_clsid(argc,argv,controlAttributes);
}

Scheme_Object *mx_all_com_classes(int argc,Scheme_Object **argv) {
  return mx_all_clsid(argc,argv,objectAttributes);
}

Scheme_Object *mx_document_objects(int argc,Scheme_Object **argv) {
  IHTMLDocument2 *pDocument;
  IHTMLElement *pBody;
  IHTMLElementCollection *pObjectsCollection;
  long numObjects;
  Scheme_Object *retval;
  int i;
  IDispatch *pObjectDispatch;
  MX_COM_Object *com_object;

  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("document_objects","mx-document",0,argc,argv);
  }

  pDocument = MX_DOCUMENT_VAL(argv[0]); 

  pDocument->get_body(&pBody);

  if (pBody == NULL) {
    scheme_signal_error("Can't find document body");
  }

  pObjectsCollection = getBodyElementsWithTag(pBody,"OBJECT");
  
  pBody->Release();

  pObjectsCollection->get_length(&numObjects);

  retval = scheme_null;

  for (i = numObjects - 1; i >= 0; i--) {

    pObjectDispatch = getObjectInCollection(pObjectsCollection,i);

    com_object = (MX_COM_Object *)scheme_malloc(sizeof(MX_COM_Object));

    com_object->type = mx_com_object_type; 
    com_object->pIDispatch = pObjectDispatch;

    mx_register_com_object((Scheme_Object *)com_object,pObjectDispatch);

    retval = scheme_make_pair((Scheme_Object *)com_object,retval);
  }

  pObjectsCollection->Release();

  return retval;
}

CLSID getCLSIDFromString(char const *name) {  // linear search through Registry
  HKEY hkey,hsubkey;
  LONG result;
  FILETIME fileTime;
  unsigned long keyIndex;
  TCHAR clsidBuffer[256];
  OLECHAR oleClsidBuffer[256];
  DWORD clsidBufferSize;
  DWORD dataType;
  BYTE dataBuffer[256];
  DWORD dataBufferSize;
  CLSID clsid;
  BOOL loopFlag;
  int count;
  char **p;

  // dummy entry

  memset(&clsid,0,sizeof(clsid));

  // get HKEY to Interfaces listing in Registry
	
  result = RegOpenKeyEx(HKEY_CLASSES_ROOT,
			"CLSID",
			(DWORD)0,
			KEY_READ,
			&hkey);

	
  if (result != ERROR_SUCCESS) {
    scheme_signal_error("Error while searching Windows registry");
  }	    

  // enumerate subkeys until we find the one we want

  // really, should call RegQueryInfoKey to find size needed for buffers

  keyIndex = 0;

  while (1) {

    // get next subkey
      
    clsidBufferSize = sizeof(clsidBuffer);

    result = RegEnumKeyEx(hkey,keyIndex++,
			  clsidBuffer,
			  &clsidBufferSize,
			  0,NULL,NULL,
			  &fileTime);
  	
    if (result == ERROR_NO_MORE_ITEMS) {
      break;
    }		

    if (result != ERROR_SUCCESS) {
      scheme_signal_error("Error enumerating subkeys in Windows registry");
    }

    if (strlen(clsidBuffer) != 38) { // not a CLSID -- bogus entry
      continue;
    }

    // open subkey

    result = RegOpenKeyEx(hkey,clsidBuffer,
			  (DWORD)0,
			  KEY_READ,&hsubkey);

    if (result != ERROR_SUCCESS) {
      return clsid;
    }	    

    dataBufferSize = sizeof(dataBuffer);
       
    RegQueryValueEx(hsubkey,"",0,&dataType,dataBuffer,&dataBufferSize);

    if (dataType == REG_SZ && strcmp(name,(char *)dataBuffer) == 0) {
      int subkeyIndex;
      TCHAR subkeyBuffer[256];
      DWORD subkeyBufferSize;

      // confirm this is a COM object

      subkeyIndex = 0;

      loopFlag = TRUE;

      while (loopFlag) {

	subkeyBufferSize = sizeray(subkeyBuffer);

	result = RegEnumKeyEx(hsubkey,subkeyIndex++,
			      subkeyBuffer,
			      &subkeyBufferSize,
			      0,NULL,NULL,
			      &fileTime);

	if (result == ERROR_NO_MORE_ITEMS) {
	  break;
	}

	if (result != ERROR_SUCCESS) {
	  scheme_signal_error("Error enumerating subkeys in Windows registry");
	}
  	
	p = objectAttributes;

	while (*p) {
	  if (stricmp(subkeyBuffer,*p) == 0) {
	    count = MultiByteToWideChar(CP_ACP,(DWORD)0,
					clsidBuffer,strlen(clsidBuffer) + 1,
					oleClsidBuffer,sizeray(oleClsidBuffer));

	    if (count == 0) {
	      scheme_signal_error("Error translating CLSID to Unicode",name);
	    }

	    CLSIDFromString(oleClsidBuffer,&clsid);
	    loopFlag = FALSE;
	    break; // *p loop
	  }
	  p++;
	}
      }
    }

    RegCloseKey(hsubkey);

  }

  RegCloseKey(hkey);  
    
  return clsid;
}

Scheme_Object *mx_find_element(int argc,Scheme_Object **argv) {
  IHTMLElement *pIHTMLElement;
  MX_Element *retval;

  if (MX_DOCUMENTP(argv[0]) == FALSE) { 
    scheme_wrong_type("document-find-element","mx-document",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) { 
    scheme_wrong_type("document-find-element","string",1,argc,argv);
  }

  if (SCHEME_STRINGP(argv[2]) == FALSE) {
    scheme_wrong_type("document-find-element","string",2,argc,argv);
  }

  pIHTMLElement = findBodyElement(MX_DOCUMENT_VAL(argv[0]),
				  SCHEME_STR_VAL(argv[1]),
				  SCHEME_STR_VAL(argv[2]));

  if (pIHTMLElement == NULL) {
    scheme_signal_error("Element not found");
  }
  
  retval = (MX_Element *)scheme_malloc(sizeof(MX_Element));

  retval->type = mx_element_type;
  retval->valid = TRUE;
  retval->pIHTMLElement = pIHTMLElement;

  return (Scheme_Object *)retval;
}

Scheme_Object *mx_coclass_to_html(int argc,Scheme_Object **argv) {
  char *controlName;
  LPOLESTR clsidString;
  char buff[512];
  CLSID clsid;
  static CLSID emptyClsid;
  int width,height;

  if (SCHEME_STRINGP(argv[0]) == FALSE) {
    scheme_wrong_type("coclass->html","string",0,argc,argv);
  }

  if (argc == 2 || argc == 3) {
    if (SCHEME_INTP(argv[1]) == FALSE) {
      scheme_wrong_type("coclass->html","int",1,argc,argv);
    }
    width = SCHEME_INT_VAL(argv[1]);
  }
  else {
    width = MX_DEFAULT_WIDTH;
  }

  if (argc == 3) {
    if (SCHEME_INTP(argv[2]) == FALSE) {
      scheme_wrong_type("coclass->html","int",2,argc,argv);
    }
    height = SCHEME_INT_VAL(argv[2]);
  }
  else {
    height = MX_DEFAULT_HEIGHT;
  }

  controlName = SCHEME_STR_VAL(argv[0]);

  clsid = getCLSIDFromString(controlName);

  if (memcmp(&clsid,&emptyClsid,sizeof(CLSID)) == 0) {
    scheme_signal_error("Control not found");  
  }

  StringFromCLSID(clsid,&clsidString);

  *(clsidString + wcslen(clsidString) - 1) = L'\0'; 

  if (clsidString == NULL) {
    scheme_signal_error("Can't convert control CLSID to string");
  }

  sprintf(buff,
	  "<OBJECT ID=\"%s\" CLASSID=\"clsid:%S\">\n"
	  "</OBJECT>",
	  controlName,              
	  clsidString + 1,
	  width,height);

  return (Scheme_Object *)scheme_make_string(buff);
}

Scheme_Object *mx_stuff_html(int argc,Scheme_Object **argv,
			     WCHAR *oleWhere,char *scheme_name) {
  IHTMLDocument2 *pDocument; 
  IHTMLElement *pBody;
  BSTR where,html;

  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type(scheme_name,"mx-document",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type(scheme_name,"string",1,argc,argv);
  }

  pDocument = MX_DOCUMENT_VAL(argv[0]);
  html = schemeStringToBSTR(argv[1]);

  pDocument->get_body(&pBody);

  if (pBody == NULL) {
    scheme_signal_error("Can't find document body");
  }

  where = SysAllocString(oleWhere);

  pBody->insertAdjacentHTML(where,html);

  SysFreeString(where);			    
  SysFreeString(html);			    

  return scheme_void;

}

Scheme_Object *mx_insert_html(int argc,Scheme_Object **argv) {
  return mx_stuff_html(argc,argv,L"AfterBegin","doc-insert-html");
}

Scheme_Object *mx_append_html(int argc,Scheme_Object **argv) {
  return mx_stuff_html(argc,argv,L"BeforeEnd","doc-append-html");
}

Scheme_Object *mx_replace_html(int argc,Scheme_Object **argv) {
  IHTMLDocument2 *pDocument; 
  IHTMLElement *pBody;
  BSTR html;

  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("replace-html","mx-document",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("replace-html","string",1,argc,argv);
  }

  pDocument = MX_DOCUMENT_VAL(argv[0]);
  html = schemeStringToBSTR(argv[1]);

  pDocument->get_body(&pBody);

  if (pBody == NULL) {
    scheme_signal_error("Can't find document body");
  }

  pBody->put_innerHTML(html);

  SysFreeString(html);			    

  return scheme_void;
}

DWORD WINAPI docHwndMsgLoop(LPVOID p) {
  HRESULT hr;
  MSG msg;
  HWND hwnd;
  IUnknown *pIUnknown;
  DOCUMENT_WINDOW_INIT *pDocWindowInit;

  pDocWindowInit = (DOCUMENT_WINDOW_INIT *)p;

  hwnd = CreateWindow("AtlAxWin","myspage.DHTMLPage.1",
		      WS_VISIBLE|pDocWindowInit->docWindow.style,
		      pDocWindowInit->docWindow.x,pDocWindowInit->docWindow.y,
		      pDocWindowInit->docWindow.width,pDocWindowInit->docWindow.height,
		      NULL,NULL,hInstance,NULL);

  if (hwnd == NULL) {
    scheme_signal_error("Can't create document window");
  }

  documentHwnd = hwnd;

  SetClassLong(hwnd,GCL_HICON,(LONG)hIcon);
  
  SetWindowText(hwnd,pDocWindowInit->docWindow.label);
  ShowWindow(hwnd,SW_SHOW);
  SetForegroundWindow(hwnd);

  pIUnknown = NULL;

  while (IsWindow(hwnd)) {

    if (pIUnknown == NULL) {
      AtlAxGetControl(hwnd,&pIUnknown);
      if (pIUnknown) {

        hr = CoMarshalInterThreadInterfaceInStream(IID_IUnknown,pIUnknown,
						   pDocWindowInit->ppIStream);

        if (hr != S_OK) {
          ::MessageBox(NULL,"Can't marshal document interface","MysterX",MB_OK);
          DestroyWindow(hwnd);
          ReleaseSemaphore(createHwndSem,1,NULL);
          return 0;
        }

        ReleaseSemaphore(createHwndSem,1,NULL);
      }
    }

    while (PeekMessage(&msg,NULL,0,0,PM_REMOVE)) {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }

  return 0;
}

int cmpDwso(char *key,DOCUMENT_WINDOW_STYLE_OPTION *dwso) {
  return strcmp(key,dwso->name);
}

void assignIntOrDefault(int *pVal,Scheme_Object **argv,int argc,int ndx) {
  if (SCHEME_SYMBOLP(argv[ndx])) {
    *pVal = CW_USEDEFAULT;
    if (strcmpi(SCHEME_SYM_VAL(argv[ndx]),"default") == 0) {
      *pVal = CW_USEDEFAULT;
    }
    else {
      scheme_wrong_type("make-document","int",ndx+1,argc,argv);
    }
  }
  else if (SCHEME_INTP(argv[ndx]) == FALSE) {
    scheme_wrong_type("make-document","int",ndx+1,argc,argv);
  }
  else {
    *pVal = SCHEME_INT_VAL(argv[ndx]);
  }
}

Scheme_Object *mx_make_document(int argc,Scheme_Object **argv) {
  HRESULT hr;
  DWORD threadId;
  IUnknown *pIUnknown;
  IDispatch *pIDispatch;
  IDHTMLPage *pIDHTMLPage;
  IStream *pIStream,*pDocumentStream;
  IWebBrowser2 *pIWebBrowser2;
  IHTMLDocument2 *pIHTMLDocument2;
  IEventQueue *pIEventQueue;
  MX_Document_Object *doc;
  Scheme_Object *pSyms,*currSym;
  char *currStyleOption;
  DOCUMENT_WINDOW_INIT docWindowInit;
  DOCUMENT_WINDOW_STYLE_OPTION *pDwso;
  int i;

  // mutex to protect association between new window and pIUnknown pointer to DHTML control

  WaitForSingleObject(documentHwndMutex,INFINITE);

  if (SCHEME_STRINGP(argv[0]) == FALSE) {
    scheme_wrong_type("make-document","string",1,argc,argv);
  }

  docWindowInit.docWindow.label = SCHEME_STR_VAL(argv[0]);

  assignIntOrDefault(&docWindowInit.docWindow.width,argv,argc,1);
  assignIntOrDefault(&docWindowInit.docWindow.height,argv,argc,2);
  assignIntOrDefault(&docWindowInit.docWindow.x,argv,argc,3);
  assignIntOrDefault(&docWindowInit.docWindow.y,argv,argc,4);

  if (SCHEME_PAIRP(argv[5]) == FALSE && argv[5] != scheme_null) {
    scheme_wrong_type("make-document","list of symbols",5,argc,argv);
  }

  pSyms = argv[5];
  docWindowInit.docWindow.style = WS_OVERLAPPEDWINDOW;

  while (pSyms != scheme_null) {

    currSym = SCHEME_CAR(pSyms);

    if (SCHEME_SYMBOLP(currSym) == FALSE) {
      scheme_wrong_type("make-document","list of symbols",5,argc,argv);
    }
 
    currStyleOption = SCHEME_SYM_VAL(currSym);
    
    pDwso = (DOCUMENT_WINDOW_STYLE_OPTION *)bsearch(currStyleOption,
						    styleOptions,
						    sizeray(styleOptions),
						    sizeof(styleOptions[0]),
						    (int (*)(const void *,const void *))cmpDwso);
    if (pDwso == NULL) {
      scheme_signal_error("Invalid document window style option: %s",currStyleOption);
    }

    if (pDwso->enable) {
      docWindowInit.docWindow.style |= pDwso->bits;
    }
    else {
      docWindowInit.docWindow.style &= ~(pDwso->bits);
    }

    pSyms = SCHEME_CDR(pSyms);
  }

  docWindowInit.ppIStream = &pDocumentStream;

  CreateThread(NULL,0,docHwndMsgLoop,(void *)&docWindowInit,0L,&threadId);

  // wait until the window is created
  
  WaitForSingleObject(createHwndSem,INFINITE);

  hr = CoGetInterfaceAndReleaseStream(pDocumentStream,IID_IUnknown,(void **)&pIUnknown);

  doc = (MX_Document_Object *)scheme_malloc(sizeof(MX_Document_Object));
  doc->type = mx_document_type;
  doc->hwnd = documentHwnd;

  ReleaseSemaphore(documentHwndMutex,1,NULL);

  if (hr != S_OK || pIUnknown == NULL) {
    DestroyWindow(documentHwnd);
    scheme_signal_error("Can't get document unknown interface, code: %X",hr);
  }

  pIUnknown->QueryInterface(IID_IDHTMLPage,(void **)&pIDHTMLPage);

  if (pIDHTMLPage == NULL) {
    scheme_signal_error("Can't get document interface");
  }

  // workaround for inability to use exdisp.idl or mshtml.idl
  
  pIStream = NULL;
  pIDHTMLPage->marshalWebBrowserToStream(&pIStream);
  
  if (pIStream == NULL) {
    scheme_signal_error("Can't get stream for Web browser");
  }

  hr = CoGetInterfaceAndReleaseStream(pIStream,IID_IWebBrowser2,(void **)&pIWebBrowser2);

  if (hr != S_OK || pIWebBrowser2 == NULL) {
    scheme_signal_error("Can't get web browser interface, code %X",hr);
  }

  pIStream = NULL;
  pIDHTMLPage->marshalEventQueueToStream(&pIStream);
  
  if (pIStream == NULL) {
    scheme_signal_error("Can't get stream for event queue");
  }

  hr = CoGetInterfaceAndReleaseStream(pIStream,IID_IEventQueue,(void **)&pIEventQueue);

  if (hr != S_OK || pIEventQueue == NULL) {
    scheme_signal_error("Can't get event queue interface, code %X",hr);
  }

  // may have to wait for document to be created
  
  for (i = 0; i < DOCDISPATCH_TRIES; i++) {

    pIWebBrowser2->get_Document(&pIDispatch);

    if (pIDispatch) {
      break;
    }

    Sleep(500);
  }

  pIWebBrowser2->Release();

  if (pIDispatch == NULL) {
    scheme_signal_error("Error retrieving DHTML dispatch interface");
  }

  hr = pIDispatch->QueryInterface(IID_IHTMLDocument2,(void **)&pIHTMLDocument2);

  pIDispatch->Release();

  if (pIHTMLDocument2 == NULL) {
    scheme_signal_error("Error retrieving DHTML document interface, code %X",hr);
  }

  doc->pIHTMLDocument2 = pIHTMLDocument2;
  doc->pIEventQueue = pIEventQueue;

  return (Scheme_Object *)doc;
}

Scheme_Object *mx_document_show(int argc,Scheme_Object **argv) {
  MX_Document_Object *pDoc;

  if (MX_DOCUMENTP(argv[0]) == FALSE) {
    scheme_wrong_type("show-document","mx-document",0,argc,argv);
  }

  pDoc = (MX_Document_Object *)argv[0];

  ShowWindow(pDoc->hwnd,
	     argv[1] == scheme_false ? SW_HIDE : SW_SHOW);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  HRESULT hr;
  int i;

  mx_com_object_type = scheme_make_type("<com-object>");
  mx_document_type = scheme_make_type("<mx-document>");
  mx_element_type = scheme_make_type("<mx-element>");
  mx_event_type = scheme_make_type("<mx-event>");
  mx_com_cy_type = scheme_make_type("<com-currency>");
  mx_com_date_type = scheme_make_type("<com-date>");
  mx_com_boolean_type = scheme_make_type("<com-bool>");
  mx_com_scode_type = scheme_make_type("<com-scode>");
  mx_com_variant_type = scheme_make_type("<com-variant>");
  mx_com_iunknown_type = scheme_make_type("<com-iunknown>");
  mx_com_pointer_type = scheme_make_type("<com-pointer>");
  mx_com_array_type = scheme_make_type("<com-array>");

  hr = CoInitialize(NULL);

  // S_OK means success, S_FALSE means COM already loaded

  if (hr != S_OK && hr != S_FALSE) {
    return scheme_false;
  }		

  // make type hash table uncollectable

  scheme_register_extension_global(typeTable,TYPE_TBL_SIZE * sizeof(MX_TYPE_TBL_ENTRY *));

  for (i = 0; i < sizeray(mxPrims); i++) {
    scheme_add_prim_to_env(env,
			   mxPrims[i].c_fun,
			   mxPrims[i].name,
			   mxPrims[i].minargs,
			   mxPrims[i].maxargs);
  }

  initEventNames();

  puts("MysterX extension for MzScheme, "
       "Copyright (c) 1999 Rice PLT (Paul Steckler)");
  
  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *env) {
  return scheme_initialize(env); /* reloading COM is OK */
}

BOOL APIENTRY DllMain(HANDLE hModule,DWORD reason,LPVOID lpReserved) {

  if (reason == DLL_PROCESS_ATTACH) {

    hInstance = (HINSTANCE)hModule;

    documentHwndMutex = CreateSemaphore(NULL,1,1,NULL);
    createHwndSem = CreateSemaphore(NULL,0,1,NULL);

    hIcon = (HICON)LoadImage(hInstance,
			     MAKEINTRESOURCE(MYSTERX_ICON),
			     IMAGE_ICON,0,0,0);
    
    _Module.Init(NULL,hInstance);
    AtlAxWinInit();
  
  }
  else if (reason == DLL_PROCESS_DETACH) {
    _Module.Term();
  }

  return TRUE;
}

