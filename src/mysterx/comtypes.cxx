// comtypes.cpp

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

#include "escheme.h"

#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

Scheme_Type mx_com_object_type;     
Scheme_Type mx_document_type;
Scheme_Type mx_element_type;
Scheme_Type mx_event_type;

Scheme_Type mx_com_cy_type;
Scheme_Type mx_com_date_type;
Scheme_Type mx_com_boolean_type;
Scheme_Type mx_com_scode_type;
Scheme_Type mx_com_variant_type;
Scheme_Type mx_com_hresult_type;
Scheme_Type mx_com_iunknown_type;
Scheme_Type mx_com_pointer_type;
Scheme_Type mx_com_array_type;

Scheme_Object *mx_document_pred(int argc,Scheme_Object **argv) {
  return MX_DOCUMENTP(argv[0]) ? scheme_true : scheme_false;
} 

Scheme_Object *mx_make_cy(CY *pCy) {
  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc(sizeof(MX_COM_Data_Object));

  retval->type = mx_com_cy_type;
  retval->cy = *pCy;

  return (Scheme_Object *)retval;

}

Scheme_Object *mx_make_date(DATE *pDate) {
  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc(sizeof(MX_COM_Data_Object));

  retval->type = mx_com_date_type;
  retval->date = *pDate;

  return (Scheme_Object *)retval;

}

Scheme_Object *mx_make_bool(unsigned boolVal) {
  return (boolVal == 0) ? scheme_false : scheme_true;
}

Scheme_Object *mx_make_scode(SCODE scode) {
  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc(sizeof(MX_COM_Data_Object));

  retval->type = mx_com_scode_type;
  retval->scode = scode;

  return (Scheme_Object *)retval;
}


Scheme_Object *mx_make_idispatch(IDispatch *pIDispatch) {
  MX_COM_Object *retval;

  retval = (MX_COM_Object *)scheme_malloc(sizeof(MX_COM_Object));

  retval->type = mx_com_object_type;
  retval->pIDispatch = pIDispatch;
  retval->pEventTypeInfo = NULL;

  return (Scheme_Object *)retval;

}


Scheme_Object *mx_make_iunknown(IUnknown *pIUnknown) {
  MX_COM_Data_Object *retval;

  retval = (MX_COM_Data_Object *)scheme_malloc(sizeof(MX_COM_Data_Object));

  retval->type = mx_com_iunknown_type;
  retval->pIUnknown = pIUnknown;

  return (Scheme_Object *)retval;

}
