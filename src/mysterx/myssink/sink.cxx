// Sink.cpp : Implementation of CSink

#include "stdafx.h"
#include <stdio.h>
#include "myssink.h"
#include "sink.h"
#include "comtypes.h"

/////////////////////////////////////////////////////////////////////////////
// CSink

// private methods

int CSink::getHashValue(DISPID dispId) {

  // casting dispId guarantees positive result

  return (int)((ULONG)dispId % EVENT_HANDLER_TBL_SIZE);
}

EVENT_HANDLER_ENTRY *CSink::newEventHandlerEntry(DISPID dispId,Scheme_Object *handler) {
  EVENT_HANDLER_ENTRY *p;
  p = (EVENT_HANDLER_ENTRY *)scheme_malloc(sizeof(EVENT_HANDLER_ENTRY));
  p->dispId = dispId;
  p->handler = handler;
  p->next = NULL;

  return p;
}

EVENT_HANDLER_ENTRY *CSink::lookupHandler(DISPID dispId) {
  int hashVal;
  EVENT_HANDLER_ENTRY *p;  				

  hashVal = getHashValue(dispId);
  
  p = &eventHandlerTable[hashVal];
  
  while (p) {
    if (p->dispId == dispId) {
      return p;
    }
    p = p->next;
  }

  return NULL;
}

// import Scheme extension table

STDMETHODIMP CSink::set_extension_table(int p)
{
  scheme_extension_table = (Scheme_Extension_Table *)p;
  return S_OK;
}

STDMETHODIMP CSink::set_myssink_table(int p)
{
  myssink_table = (MYSSINK_TABLE *)p;
  return S_OK;
}

STDMETHODIMP CSink::register_handler(DISPID dispId,int handler) {
  unsigned short hashVal;
  EVENT_HANDLER_ENTRY *p;

  hashVal = getHashValue(dispId);
  
  p = &eventHandlerTable[hashVal];
  
  if (p->dispId == (DISPID)0) {
    p->dispId = dispId;
    p->handler = (Scheme_Object *)handler;
    p->next = NULL;
  }
  else {

    while (p->next != NULL) {

      if (p->dispId == dispId) { // update existing entry
	p->handler = (Scheme_Object *)handler;
	return S_OK;
      }

      p = p->next;
    }

    p->next = newEventHandlerEntry(dispId,(Scheme_Object *)handler);
  }

  return S_OK;
}

// different than the same-named function in mysterx.cxx
// *here* we're coercing VARIANT's to be arguments to
// Scheme procedures; *there*, we're coercing a VARIANTARG
// return value to be the value of a method call, and 
// VARIANTARG's, unlike VARIANT's, cannot have VT_BYREF bit

Scheme_Object *CSink::variantToSchemeObject(VARIANTARG *pVariantArg) {

  switch(pVariantArg->vt) {

  case VT_NULL :

    return scheme_void;

  case VT_UI1 :

    return scheme_make_character((char)(pVariantArg->bVal));

  case VT_UI1 | VT_BYREF :

    return scheme_box(scheme_make_character((char)(*pVariantArg->pbVal)));

  case VT_I2 :

    return scheme_make_integer(pVariantArg->iVal);

  case VT_I2 | VT_BYREF :

    return scheme_box(scheme_make_integer(*pVariantArg->piVal));

  case VT_I4 :
  
    return scheme_make_integer(pVariantArg->lVal);

  case VT_I4 | VT_BYREF :
  
    return scheme_box(scheme_make_integer(pVariantArg->lVal));

  case VT_R4 :

#ifdef MZ_USE_SINGLE_FLOATS
    return scheme_make_float(pVariantArg->fltVal);
#else
    return scheme_make_double((double)(pVariantArg->fltVal));
#endif

  case VT_R4 | VT_BYREF :

#ifdef MZ_USE_SINGLE_FLOATS
    return scheme_box(scheme_make_float(pVariantArg->fltVal));
#else
    return scheme_box(scheme_make_double((double)(pVariantArg->fltVal)));
#endif

  case VT_R8 :

    return scheme_make_double(pVariantArg->dblVal);

  case VT_R8 | VT_BYREF :

    return scheme_box(scheme_make_double(pVariantArg->dblVal));

  case VT_BSTR :

    return BSTRToSchemeString(pVariantArg->bstrVal);

  case VT_BSTR | VT_BYREF :

    return scheme_box(BSTRToSchemeString(*pVariantArg->pbstrVal));

  case VT_CY :

    return make_cy(&pVariantArg->cyVal);

  case VT_CY | VT_BYREF :

    return scheme_box(make_cy(&pVariantArg->cyVal));

  case VT_DATE :

    return make_date(&pVariantArg->date);

  case VT_DATE | VT_BYREF :

    return scheme_box(make_date(&pVariantArg->date));

  case VT_BOOL :

    return make_bool(pVariantArg->boolVal);

  case VT_BOOL | VT_BYREF :

    return scheme_box(make_bool(pVariantArg->boolVal));

  case VT_ERROR :
    
    return make_scode(pVariantArg->scode);

  case VT_ERROR | VT_BYREF :
    
    return scheme_box(make_scode(pVariantArg->scode));

  case VT_DISPATCH :

    return make_idispatch(pVariantArg->pdispVal);
    
  case VT_DISPATCH | VT_BYREF :

    return scheme_box(make_idispatch(pVariantArg->pdispVal));
    
  case VT_UNKNOWN :

    return make_iunknown(pVariantArg->punkVal);

  case VT_UNKNOWN | VT_BYREF:

    return scheme_box(make_iunknown(pVariantArg->punkVal));

  default :
    
    scheme_signal_error("Can't make Scheme value from VARIANT %X",
			pVariantArg->vt);

  }

  return NULL;
}

void CSink::handlerUpdateError(char *s) {
  scheme_signal_error("Handler updated box with value other than "
		      "expected type: %s",s);
}

void CSink::unmarshallSchemeObject(Scheme_Object *obj,VARIANTARG *pVariantArg) {
  Scheme_Object *val;

  if (pVariantArg->vt & VT_BYREF) {
    val = SCHEME_BOX_VAL(obj);
  }

  switch (pVariantArg->vt) {

  case VT_UI1 | VT_BYREF :

    if (SCHEME_CHARP(val) == FALSE) {
      handlerUpdateError("character");
    } 

    *(pVariantArg->pbVal) = SCHEME_CHAR_VAL(val);
    break;

  case VT_I2 | VT_BYREF :

    if (isShortInt(val) == FALSE) {
      handlerUpdateError("exact integer");
    } 

    *(pVariantArg->piVal) = (short)SCHEME_INT_VAL(val);
    break;

  case VT_I4 | VT_BYREF :
  
    long lVal;

    if (SCHEME_EXACT_INTEGERP(val) == FALSE) {
      handlerUpdateError("exact integer");
    }

    if (scheme_get_int_val(val,&lVal) == 0) {
      scheme_signal_error("Handler updated box with too large an exact integer");
    } 

    *(pVariantArg->plVal) = lVal;
    break;

  case VT_R4 | VT_BYREF :

#ifdef MZ_USE_SINGLE_FLOATS
    if (SCHEME_FLTP(val) == FALSE) {
      handlerUpdateError("float");
    } 

    *(pVariantArg->pfltVal) = SCHEME_FLT_VAL(val);
#else
    if (SCHEME_DBLP(val) == FALSE) {
      handlerUpdateError("double");
    } 

    *(pVariantArg->pfltVal) = (float)SCHEME_DBL_VAL(val);
#endif
    break;

  case VT_R8 | VT_BYREF :

    if (SCHEME_DBLP(val) == FALSE) {
      handlerUpdateError("double");
    } 

    *(pVariantArg->pdblVal) = SCHEME_DBL_VAL(val);

  case VT_BSTR :

    // string passed to Scheme can be updated in-place

    BSTR bstr;
    
    bstr = schemeStringToBSTR(obj);
    wcscpy(pVariantArg->bstrVal,bstr);
    SysFreeString(bstr);
    break;

  case VT_BSTR | VT_BYREF :

    BSTR bstr2;
    
    if (SCHEME_STRINGP(val) == FALSE) {
      handlerUpdateError("string");
    }

    bstr2 = schemeStringToBSTR(val);
    wcscpy(*(pVariantArg->pbstrVal),bstr2);
    SysFreeString(bstr2);
    break;

  case VT_CY | VT_BYREF :

    if (cy_pred(val) == FALSE) {
      handlerUpdateError("com-cy");
    }

    *(pVariantArg->pcyVal) = cy_val(val);
    break;

  case VT_DATE | VT_BYREF :

    if (date_pred(val) == FALSE) {
      handlerUpdateError("com-date");
    }

    *(pVariantArg->pdate) = date_val(val);
    break;

  case VT_BOOL | VT_BYREF :

    *(pVariantArg->pboolVal) = (val == scheme_false) ? 0 : -1;
    break;

  case VT_ERROR | VT_BYREF :
    
    if (scode_pred(val) == FALSE) {
      handlerUpdateError("com-scode");
    }

    *(pVariantArg->pscode) = scode_val(val);
    break;

  case VT_DISPATCH | VT_BYREF :

    if (comobj_pred(val) == FALSE) {
      handlerUpdateError("com-obj");
    }

    *(pVariantArg->ppdispVal) = comobj_val(val);
    break;

  case VT_UNKNOWN | VT_BYREF:

    if (iunknown_pred(val) == FALSE) {
      handlerUpdateError("com-iunknown");
    }

    *(pVariantArg->ppunkVal) = iunknown_val(val);
    break;

  default :

    ; // no update needed

  } 
}

// override default implementation of IDispatch::Invoke

HRESULT CSink::Invoke(DISPID dispId,REFIID refiid,LCID lcid,WORD flags,
                      DISPPARAMS* pDispParams,VARIANT* pvarResult,
		      EXCEPINFO* pexcepinfo,UINT* puArgErr) {

  Scheme_Object *handler;
  EVENT_HANDLER_ENTRY *p;  
  VARIANTARG *pCurrArg;
  short numParams,actualParams;
  Scheme_Object *argv[128];
  mz_jmp_buf jmpSave;
  int i;

  p = lookupHandler(dispId);

  if (p == NULL) { // nothing registered
    return S_OK;
  }

  handler = p->handler;
  
  numParams = pDispParams->cArgs;

  if (numParams > 128) {
    return DISP_E_TYPEMISMATCH;
  }

  // named arguments not supported

  if (pDispParams->cNamedArgs > 0) {
    return DISP_E_NONAMEDARGS;
  }

  // trap any local errors

  memcpy(&jmpSave, &scheme_error_buf, sizeof(mz_jmp_buf));

  if (scheme_setjmp(scheme_error_buf)) {
    scheme_clear_escape();
    memcpy(&scheme_error_buf, &jmpSave, sizeof(mz_jmp_buf));
    return S_OK;
  }

  /* memory layout of rgvargs:

      --------------------------------
     | opt params | required params   |
      --------------------------------

     these are in reverse order from the order
     given to Scheme

  */

  actualParams = numParams;

  i = 0;

  while (i < numParams) {
    pCurrArg = &pDispParams->rgvarg[i];
    if (pCurrArg->vt == VT_ERROR &&
	pCurrArg->scode == DISP_E_PARAMNOTFOUND) { 
      actualParams--;
    }
    else { // as soon as first required arg found
      break;
    }
    i++;
  }

  for (i = 0; i < actualParams; i++) {
    pCurrArg = &pDispParams->rgvarg[numParams - 1 - i];
    argv[i] = variantToSchemeObject(pCurrArg);
  }

  scheme_apply(handler,actualParams,argv);

  // updating of boxes needs to be reflected in BYREF parameters 

  for (i = 0; i < actualParams; i++) {
    pCurrArg = &pDispParams->rgvarg[numParams - 1 - i];
    unmarshallSchemeObject(argv[i],pCurrArg);
  }

  memcpy(&scheme_error_buf, &jmpSave, sizeof(mz_jmp_buf));
  return S_OK;
}

