#ifndef WXS_SETUP_ONLY
#endif
void objscheme_setup_wxBitmap(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxBitmap(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxBitmap(class wxBitmap *realobj);
class wxBitmap *objscheme_unbundle_wxBitmap(Scheme_Object *obj, const char *where, int nullOK);
#endif
void objscheme_setup_wxIcon(void *env);
#ifndef WXS_SETUP_ONLY
int objscheme_istype_wxIcon(Scheme_Object *obj, const char *stop, int nullOK);
Scheme_Object *objscheme_bundle_wxIcon(class wxIcon *realobj);
class wxIcon *objscheme_unbundle_wxIcon(Scheme_Object *obj, const char *where, int nullOK);
#endif
