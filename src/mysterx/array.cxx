// array.cxx

#include "stdafx.h"

#include <objbase.h>
#include <mshtml.h>
#include <initguid.h>
#include <winnls.h>
#include <exdisp.h>

#include "escheme.h"

#include "myspage.h"
#include "myssink.h"

#include "mysterx.h"

Scheme_Object * SafeArrayRowToVector(SAFEARRAY *arr,
				     int row,int dim,UINT type) {
  Scheme_Object *vec, *temp;
  long size, high, low;
  long lDimension[2];
  int i;
    
  if (dim == 1) {
    lDimension[1] = row;
  }
  else {
    lDimension[0] = row;
  }

  SafeArrayGetLBound(arr, dim, &low);
  SafeArrayGetUBound(arr, dim, &high);
  size = high - low + 1;
  
  vec = scheme_make_vector(size,scheme_void);

  for (i = low; i <= high; i++) {
    if (dim == 1) {
      lDimension[0] = i;
    }
    else {
      lDimension[1] = i;
    }
    temp = SafeArrayElement(arr, lDimension, type);
    SCHEME_VEC_ELS(vec)[i - low] = temp;
  }

  return vec;
}

Scheme_Object *safeArrayToVector(SAFEARRAY *arr,UINT type) {
  Scheme_Object *vec;
  int i;
  int size;
  long low, high;
  long dims;
  
  dims = SafeArrayGetDim(arr);
  if (dims == 2) {
    SafeArrayGetLBound(arr, 1, &low);
    SafeArrayGetUBound(arr, 1, &high);
    size = high - low + 1;
    
    vec = scheme_make_vector(size,scheme_void);
    for (i = low; i <= high; i++) {
      SCHEME_VEC_ELS(vec)[i - low] = SafeArrayRowToVector(arr,i,2,type);
    }
  }
  else if (dims == 1) {
    vec = SafeArrayRowToVector(arr,1,1,type);
  }

  return vec;
}

Scheme_Object *SafeArrayElement(SAFEARRAY *arr,long *lDimension, 
				UINT type) {
  
  switch(type) {
    
  case VT_EMPTY | VT_ARRAY :
  case VT_NULL	| VT_ARRAY:
    
    return scheme_void;
    
  case VT_UI1  | VT_ARRAY:
    char carg;
    SafeArrayGetElement(arr, lDimension, &carg);
    return scheme_make_character(carg);
    
  case VT_I2  | VT_ARRAY:
    int iarg;
    SafeArrayGetElement(arr, lDimension, &iarg);
    return scheme_make_integer(iarg);
    
  case VT_I4 | VT_ARRAY :
    long larg;
    SafeArrayGetElement(arr, lDimension, &larg);
    return scheme_make_integer_value(larg);
    
  case VT_R4 | VT_ARRAY :
    
#ifdef MZ_USE_SINGLE_FLOATS
    float farg;
    SafeArrayGetElement(arr, lDimension, &farg);
    return scheme_make_float(farg);
#else
    double darg;
    SafeArrayGetElement(arr, lDimension, &darg);
    return scheme_make_double((double)(darg));
#endif
    
  case VT_R8 | VT_ARRAY :
    SafeArrayGetElement(arr, lDimension, &darg);
    return scheme_make_double((double)(darg));
    
  case VT_BSTR | VT_ARRAY :
    unsigned short *barg;
    SafeArrayGetElement(arr, lDimension, &barg);
    return BSTRToSchemeString((unsigned short *)barg);
    
  case VT_VARIANT | VT_ARRAY :
    VARIANT var;
    SafeArrayGetElement(arr, lDimension, &var);
    return variantToSchemeObject(&var);
    
  default :
    
    scheme_signal_error("Can't make Scheme value from VARIANT %X",
			type);
    
  }
  
  return NULL;
}

SAFEARRAY * mx_vectorToSafeArray(Scheme_Object *vec) {
  VARIANT varTemp;
  Scheme_Object *schTemp;
  SAFEARRAY *arr;
  long lDimension[2];
  long size;
  int dims;

  if (SCHEME_VECTORP(vec) == FALSE) {
    scheme_signal_error("Invalid type for safe array");
  }

  dims = 1;
  size = SCHEME_VEC_SIZE(vec);

  if ((SCHEME_VECTORP(SCHEME_VEC_ELS(vec)[0]))) { //2D vector
    dims = size;
    size = SCHEME_VEC_SIZE(SCHEME_VEC_ELS(vec)[0]);
    SAFEARRAYBOUND aDim[2];
    aDim[0].lLbound = 0; 
    aDim[0].cElements = dims;
    aDim[1].lLbound = 0; 
    aDim[1].cElements = size;
    arr = SafeArrayCreate(VT_VARIANT,2,aDim);
    
    for (int i = 0; i < dims; i++) {
      for (int j = 0; j < size; j++) {
	schTemp = SCHEME_VEC_ELS(SCHEME_VEC_ELS(vec)[i])[j];
	marshallSchemeValueToVariant(schTemp,&varTemp);
	lDimension[0] = i;
	lDimension[1] = j;
	SafeArrayPutElement(arr, lDimension, &varTemp);
      }
    }
  }
  else {
    SAFEARRAYBOUND aDim[1];
    aDim[0].lLbound = 0; 
    aDim[0].cElements = size;
    arr = SafeArrayCreate(VT_VARIANT,1,aDim);
    
    SafeArrayLock(arr);

    for (int i = 0; i < size; i++) {
      schTemp = SCHEME_VEC_ELS(vec)[i];
      marshallSchemeValueToVariant(schTemp, &varTemp);
      ((VARIANT *)arr->pvData)[i] = varTemp;
    }

    SafeArrayUnlock(arr);
  }

  return arr;
  
}
