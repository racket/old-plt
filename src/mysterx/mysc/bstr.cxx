// bstr.cxx -- BSTR utility functions

#include <windows.h>

#include "escheme.h"

static
Scheme_Object * BSTRToScheme (BSTR bstr, Scheme_Object * converter (LPSTR, UINT))
{
  UINT len = SysStringLen (bstr);
  char * buff = (char *) scheme_malloc_atomic (len + 1);

  WideCharToMultiByte (CP_ACP, (DWORD)0, bstr, len,
                       buff, len,
                       NULL, NULL);

  buff[len] = '\0';
  return converter (buff, len);
}

// This function is here so that I can ignore
// the length parameter when passing it to BSTRToScheme.

static
Scheme_Object * scheme_make_string_with_length (LPSTR chars, UINT)
{
  return scheme_make_string_without_copying (chars);
}

Scheme_Object * BSTRToSchemeString (BSTR bstr)
{
  return
      BSTRToScheme (bstr,
                    (Scheme_Object * (*)(LPSTR, UINT)) scheme_make_string_with_length);
}

Scheme_Object * BSTRToSchemeSymbol (BSTR bstr)
{
  return BSTRToScheme (bstr, (Scheme_Object * (*)(LPSTR, UINT)) scheme_intern_exact_symbol);
}


// This parameter controls whether strings returned by
// COM are converted to scheme symbols or to scheme strings.
int mx_unmarshal_strings_as_symbols;

Scheme_Object * unmarshalBSTR (BSTR bstr)
{
  return
      (scheme_get_param (scheme_config, mx_unmarshal_strings_as_symbols) == scheme_false)
      ? BSTRToSchemeString (bstr)
      : BSTRToSchemeSymbol (bstr);
}

void updateSchemeStringFromBSTR(Scheme_Object *val,BSTR bstr) {
  int len;

  len = SysStringLen(bstr);

  if (len > SCHEME_STRLEN_VAL(val)) {
    scheme_signal_error("String updated with longer string");
  }

  WideCharToMultiByte(CP_ACP,(DWORD)0,
		      bstr,len,
		      SCHEME_STR_VAL(val),SCHEME_STRLEN_VAL(val),
		      NULL,NULL);

  SCHEME_STRLEN_VAL(val) = len;

}

BSTR stringToBSTR (LPCSTR s, UINT len)
{
  BSTR bstr = SysAllocStringLen (NULL, len);
  if (bstr == NULL)
    scheme_signal_error ("Error allocating string parameter");

  if (MultiByteToWideChar (CP_ACP, (DWORD)0,
                           s, len,
                           bstr, len + 1) == 0
      && len > 0)
    scheme_signal_error ("Error translating string parameter to Unicode");
  bstr[len] = '\0';
  return bstr;
}

// This will take a scheme string or symbol
BSTR schemeStringToBSTR (Scheme_Object *o)
{
  return (SCHEME_STRINGP (o))
      ? stringToBSTR (SCHEME_STR_VAL(o), SCHEME_STRLEN_VAL (o))
      : (SCHEME_SYMBOLP (o))
      ? stringToBSTR (SCHEME_SYM_VAL(o), SCHEME_SYM_LEN (o))
      : (scheme_signal_error ("Cannot convert scheme value to BSTR."),
         (BSTR) 0);
}
