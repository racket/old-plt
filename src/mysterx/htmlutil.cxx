// htmlutil.cxx

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "myspage.h"
#include "mysterx.h"

IHTMLElementCollection *getBodyObjects(IHTMLElement *pBody) {
  IDispatch *pBodyDispatch;
  IDispatch *pObjectsDispatch;
  IHTMLElementCollection *pBodyEltCollection;
  IHTMLElementCollection *pObjectsCollection;
  long numBodyItems;
  BSTR bstr;
  VARIANT variant;
  VARIANT emptyVariant;

  pBody->get_all(&pBodyDispatch);

  pBodyDispatch->QueryInterface(IID_IHTMLElementCollection,(void **)&pBodyEltCollection);

  pBodyDispatch->Release();

  if (pBodyEltCollection == NULL) {
    scheme_signal_error("Can't get body elements");
  }

  pBodyEltCollection->get_length(&numBodyItems);

  bstr = SysAllocString(L"OBJECT");
  
  VariantInit(&variant);
  VariantInit(&emptyVariant);

  variant.vt = VT_BSTR;
  variant.bstrVal = bstr;

  pBodyEltCollection->tags(variant,&pObjectsDispatch);

  pBodyEltCollection->Release();

  if (pObjectsDispatch == NULL) {
    scheme_signal_error("Can't get body objects");
  }

  pObjectsDispatch->QueryInterface(IID_IHTMLElementCollection,(void **)&pObjectsCollection);

  pObjectsDispatch->Release();

  if (pObjectsCollection == NULL) {
    scheme_signal_error("Can't get body elements");
  }

  return pObjectsCollection;  
}

IDispatch *getObjectInCollection(IHTMLElementCollection *pObjectCollection,int ndx) {
  VARIANT variant,emptyVariant;
  IDispatch *pIDispatch;
  IHTMLObjectElement *pIHTMLObjectElement;

  VariantInit(&variant);
  VariantInit(&emptyVariant);

  variant.vt = VT_I2;
  variant.iVal = ndx;

  pObjectCollection->item(variant,emptyVariant,&pIDispatch);

  if (pIDispatch == NULL) {
    scheme_signal_error("Unable to find object in body");
  }

  // sometimes pIDispatch is HTML element containing the control
  // sometimes it's the control itself
  // apparent bug in DHTML

  pIDispatch->QueryInterface(IID_IHTMLObjectElement,(void **)&pIHTMLObjectElement);
 
  if (pIHTMLObjectElement != NULL) {

    pIDispatch->Release();
      
    pIHTMLObjectElement->get_object(&pIDispatch);

    if (pIDispatch == NULL) {
      scheme_signal_error("Unable to get object element interface for object");
    }
  }

  return pIDispatch;
}
