// bstr.h

extern int mx_unmarshal_strings_as_symbols;
Scheme_Object *BSTRToSchemeString(BSTR);
void updateSchemeStringFromBSTR(Scheme_Object *,BSTR);
BSTR stringToBSTR(const char *,size_t);
BSTR schemeStringToBSTR(Scheme_Object *);
Scheme_Object * unmarshalBSTR (BSTR bstr);

