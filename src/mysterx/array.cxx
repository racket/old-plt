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

Scheme_Object *buildVectorFromArray(SAFEARRAY *theArray,
				    long numDims,long currDim,
				    long *allIndices,long *currNdx,UINT type) {
  Scheme_Object *vec;
  long low,high,vecSize;
  long i;

  SafeArrayGetLBound(theArray,currDim,&low);
  SafeArrayGetUBound(theArray,currDim,&high);
  vecSize = high - low + 1;

  vec = scheme_make_vector(vecSize,scheme_void);

  if (currDim < numDims - 1) {
    for (i = 0; i < vecSize; i++) {
      currNdx[0] = i;
      SCHEME_VEC_ELS(vec)[i] = 
	buildVectorFromArray(theArray,numDims,currDim + 1,
			     allIndices,currNdx + 1,type);
    }
  }
  else {
    for (i = 0; i < vecSize; i++) {
      SCHEME_VEC_ELS(vec)[i] = 
	safeArrayElementToSchemeObject(theArray,allIndices,type);
    }
  }

  return vec;
}

Scheme_Object *safeArrayToSchemeVector(SAFEARRAY *theArray,UINT type) {
  long numDims;
  long *indices;
  
  numDims = SafeArrayGetDim(theArray);

  indices = (long *)scheme_malloc(numDims * sizeof(long));

  return buildVectorFromArray(theArray,numDims,1,indices,indices,type);
  
}

Scheme_Object *safeArrayElementToSchemeObject(SAFEARRAY *arr,long *lDimension, 
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

int getSchemeVectorDims(Scheme_Object *vec) {
  Scheme_Object *currObj;
  int numDims;

  numDims = 0;
  currObj = vec;

  do {
    numDims++;
    currObj = SCHEME_VEC_ELS(currObj)[0];
  } while (SCHEME_VECTORP(currObj)); 

  return numDims;
}

void setArrayEltCounts(Scheme_Object *vec,SAFEARRAYBOUND *rayBounds) {
  Scheme_Object *currObj;
  int i;

  currObj = vec;

  i = 0;

  do {
    rayBounds[i++].cElements = SCHEME_VEC_SIZE(currObj);
    currObj = SCHEME_VEC_ELS(currObj)[0];
  } while (SCHEME_VECTORP(currObj)); 
}

BOOL isRegularVector(Scheme_Object *vec) {
  Scheme_Object **elts,*elt;
  BOOL isVec,zeroIsVec;
  int len,currLen,zeroLen;
  int i;

  if (SCHEME_VECTORP(vec) == FALSE) {
    return TRUE;
  }

  len = SCHEME_VEC_SIZE(vec);
  elts = SCHEME_VEC_ELS(vec);

  // use zeroth elt as standard

  elt = elts[0];

  zeroIsVec = SCHEME_VECTORP(elt);
  if (zeroIsVec) {
    zeroLen = SCHEME_VEC_SIZE(elt);
  }

  if (isRegularVector(elt) == FALSE) {
    return FALSE;
  }

  for (i = 1; i < len; i++) {
    elt = elts[i];

    isVec = SCHEME_VECTORP(elt);

    if (isVec != zeroIsVec) {
      return FALSE;
    }

    if (isVec) {
      currLen = SCHEME_VEC_SIZE(elt);

      if (currLen != zeroLen) {
	return FALSE;
      }

      if (isRegularVector(elt) == FALSE) {
	return FALSE;
      }
    }
  }

  return TRUE;
}

void doSetArrayElts(Scheme_Object *vec,SAFEARRAY *theArray,long *allIndices,long *currNdx) {
  VARIANT variant;
  BOOL hasSubVectors;
  Scheme_Object *elt;
  int len;
  int i;
  
  len = SCHEME_VEC_SIZE(vec);

  // we already know this vector is regular, so only test 0th elt

  hasSubVectors = SCHEME_VECTORP(SCHEME_VEC_ELS(vec)[0]);

  if (hasSubVectors) {
    for (i = 0; i < len; i++) {
      elt = SCHEME_VEC_ELS(vec)[i];
      currNdx[0] = i;
      doSetArrayElts(elt,theArray,allIndices,currNdx + 1);
    }
  }
  else {
    for (i = 0; i < len; i++) {
      elt = SCHEME_VEC_ELS(vec)[i];
      marshallSchemeValueToVariant(elt,&variant);
      SafeArrayPutElement(theArray,allIndices,&variant);
    }
  }
}

void setArrayElts(Scheme_Object *vec,SAFEARRAY *theArray) {
  long indices[MAXARRAYDIMS];

  memset(indices,0,sizeof(indices));

  doSetArrayElts(vec,theArray,indices,indices);
}

SAFEARRAY *schemeVectorToSafeArray(Scheme_Object *vec) {
  SAFEARRAY *theArray;
  SAFEARRAYBOUND *rayBounds;
  int numDims;
  int i;

  if (SCHEME_VECTORP(vec) == FALSE) {
    scheme_signal_error("Can't convert non-vector to SAFEARRAY");
  }

  if (isRegularVector(vec) == FALSE) {
    scheme_signal_error("Can't convert irregular vector to SAFEARRAY");
  }

  numDims = getSchemeVectorDims(vec);

  if (numDims > MAXARRAYDIMS) {
    scheme_signal_error("Too many array dimensions");
  }

  rayBounds = (SAFEARRAYBOUND *)scheme_malloc(numDims * sizeof(SAFEARRAYBOUND));

  for (i = 0; i < numDims; i++) {
    rayBounds[i].lLbound = 0L;
  }

  setArrayEltCounts(vec,rayBounds);

  theArray = SafeArrayCreate(VT_VARIANT,numDims,rayBounds);

  setArrayElts(vec,theArray);

  return theArray;
  
}

