
#ifdef __cplusplus
extern "C" 
{
#endif


extern Status wxAllocColor(Display *d, Colormap cm, XColor *c);
extern int wxQueryColor(Display *display, Colormap colormap, XColor *def_in_out);
extern Colormap wx_default_colormap;

#ifdef __cplusplus
}
#endif
