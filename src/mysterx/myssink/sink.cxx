// Sink.cpp : Implementation of CSink

#include "stdafx.h"
#include <stdio.h>
#include "Myssink.h"
#include "Sink.h"
#include "comtypes.h"

/////////////////////////////////////////////////////////////////////////////
// CSink

// private methods

int CSink::getHashValue(DISPID dispId) {

  // casting dispId guarantees positive result

  return (int)((ULONG)dispId % EVENT_HANDLER_TBL_SIZE);
}

EVENT_HANDLER_ENTRY *CSink::newEventHandlerEntry(DISPID dispId,Scheme_Object *handler,FUNCDESC *pFuncDesc) {
  EVENT_HANDLER_ENTRY *p;
  p = (EVENT_HANDLER_ENTRY *)scheme_malloc(sizeof(EVENT_HANDLER_ENTRY));
  p->dispId = dispId;
  p->handler = handler;
  p->pFuncDesc = pFuncDesc;
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

STDMETHODIMP CSink::set_make_cy(int p)
{
  make_cy = (Scheme_Object *(*)(CY *))p;
  return S_OK;
}

STDMETHODIMP CSink::set_make_date(int p)
{
  make_date = (Scheme_Object *(*)(DATE *))p;
  return S_OK;
}

STDMETHODIMP CSink::set_make_bool(int p)
{
  make_bool = (Scheme_Object *(*)(unsigned))p;
  return S_OK;
}

STDMETHODIMP CSink::set_make_scode(int p)
{
  make_scode = (Scheme_Object *(*)(SCODE))p;
  return S_OK;
}

STDMETHODIMP CSink::set_make_idispatch(int p)
{
  make_idispatch = (Scheme_Object *(*)(IDispatch *))p;
  return S_OK;
}

STDMETHODIMP CSink::set_make_iunknown(int p)
{
  make_iunknown = (Scheme_Object *(*)(IUnknown *))p;
  return S_OK;
}

STDMETHODIMP CSink::register_handler(DISPID dispId,int handler,int pFuncDesc) {
  unsigned short hashVal;
  EVENT_HANDLER_ENTRY *p;

  hashVal = getHashValue(dispId);
  
  p = &eventHandlerTable[hashVal];
  
  if (p->dispId == (DISPID)0) {
    p->dispId = dispId;
    p->handler = (Scheme_Object *)handler;
    p->pFuncDesc = (FUNCDESC *)pFuncDesc;
    p->next = NULL;
  }
  else {

    while (p->next != NULL) {

      if (p->dispId == dispId) { // update existing entry
	p->handler = (Scheme_Object *)handler;
	p->pFuncDesc = (FUNCDESC *)pFuncDesc;

	return S_OK;
      }

      p = p->next;
    }

    p->next = newEventHandlerEntry(dispId,(Scheme_Object *)handler,(FUNCDESC *)pFuncDesc);
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

// override default implementation of IDispatch::Invoke

HRESULT CSink::Invoke(DISPID dispId,REFIID refiid,LCID lcid,WORD flags,
                      DISPPARAMS* pDispParams,VARIANT* pvarResult,
		      EXCEPINFO* pexcepinfo,UINT* puArgErr) {

  Scheme_Object *handler;
  EVENT_HANDLER_ENTRY *p;  
  FUNCDESC *pFuncDesc;
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
  pFuncDesc = p->pFuncDesc;
  
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

  if (actualParams == 0) { 
    scheme_apply(handler,0,NULL);
    memcpy(&scheme_error_buf, &jmpSave, sizeof(mz_jmp_buf));
    return S_OK;
  }

  for (i = 0; i < actualParams; i++) {
    pCurrArg = &pDispParams->rgvarg[numParams - 1 - i];
    argv[i] = variantToSchemeObject(pCurrArg);
  }

  scheme_apply(handler,actualParams,argv);

  // updating of boxes needs to be reflected in BYREF parameters 

  memcpy(&scheme_error_buf, &jmpSave, sizeof(mz_jmp_buf));
  return S_OK;
}
