
#if defined(_MSC_VER)
# include "wx.h"
#endif
#if defined(wx_mac)
# include "common.h"
#endif

#define Uses_XLib // Xt
#include "common.h" // wxWindows
#include "wx_media.h"
#include "wx_win.h"
#include "wxscheme.h"
#include "wx_main.h"
#include "wx_dcps.h"
#include "wx_clipb.h"
#include "mrdispatch.h"
#include "wxsmred.h"

#include "wxs_obj.h"
#define WXS_SETUP_ONLY 1
#include "wxs_win.h"
#include "wxs_fram.h"
#include "wxs_item.h"
#include "wxs_butn.h"
#include "wxs_ckbx.h"
#include "wxs_chce.h"
#include "wxs_evnt.h"
#include "wxs_panl.h"
#include "wxs_madm.h"
#include "wxs_mio.h"
#include "wxs_styl.h"
#include "wxs_menu.h"
#include "wxs_bmap.h"
#include "wxs_misc.h"
#include "wxs_rado.h"
#include "wxs_slid.h"
#include "wxs_gage.h"
#include "wxs_lbox.h"
#include "wxs_tabc.h"

#include "wxs_glob.h"

#undef WXS_SETUP_ONLY
#include "wxs_gdi.h"
#include "wxs_dc.h"
#include "wxs_cnvs.h"
#include "wxs_misc.h"
#include "wxs_medi.h"
#include "wxs_mede.h"
#include "wxs_mpb.h"
#include "wxs_snip.h"

#ifdef wx_msw
# include "wx_pdf.h"
#endif

#include <stdlib.h>
#include <ctype.h>

#ifdef WX_USE_XFT
#include <X11/Xft/Xft.h>
#endif

#ifdef wx_mac
# ifdef WX_CARBON
#  ifdef OS_X
#   include <Quicktime/Movies.h>
#  else
#   include <Movies.h>
#  endif
# else
  #include <Gestalt.h>
  #include <Movies.h>
  #include <ColorPicker.h>
  #include <Folders.h>
 #endif
#endif

class GCBitmap {
public:
#ifdef MZ_PRECISE_GC
  Scheme_Object *canvasptr;
#else
  wxCanvas **canvasptr; /* weak reference */
#endif
  float x, y, w, h;
  float onx, ony, offx, offy;
  wxBitmap *on, *off;
  GCBitmap *next;
};

#ifdef MZ_PRECISE_GC
# define GET_CANVAS(gcbm) ((wxCanvas *)gcPTR_TO_OBJ(SCHEME_BOX_VAL(gcbm->canvasptr)))
#else
# define GET_CANVAS(gcbm) (*gcbm->canvasptr)
#endif


static GCBitmap *gc_bitmaps = NULL;
extern "C" {
  typedef void (*GC_START_END_PTR)(void);
  MZ_EXTERN GC_START_END_PTR GC_collect_start_callback;
  MZ_EXTERN GC_START_END_PTR GC_collect_end_callback;
};
static GC_START_END_PTR orig_collect_start_callback;
static GC_START_END_PTR orig_collect_end_callback;
static void collect_start_callback(void);
static void collect_end_callback(void);

static void wxScheme_Install(Scheme_Env *global_env);

static Scheme_Object *setup_file_symbol, *init_file_symbol, *x_display_symbol;

static Scheme_Object *get_file, *put_file, *get_ps_setup_from_user, *message_box;

static Scheme_Object *executer;

static Scheme_Object *make_media_edit, *make_media_pasteboard, *make_media_snip, *none_symbol;

static Scheme_Object *wait_symbol;

static Scheme_Object *mono_symbol, *all_symbol;

#define CONS scheme_make_pair

void wxsScheme_setup(Scheme_Env *env)
{
  wxREGGLOB(get_file);
  wxREGGLOB(put_file);
  wxREGGLOB(get_ps_setup_from_user);
  wxREGGLOB(message_box);

  env = scheme_primitive_module(scheme_intern_symbol("#%mred-kernel"), env);

  wxREGGLOB(gc_bitmaps);

  objscheme_init(env);

  wxREGGLOB(setup_file_symbol);
  wxREGGLOB(init_file_symbol);
  wxREGGLOB(x_display_symbol);
  setup_file_symbol = scheme_intern_symbol("setup-file");
  init_file_symbol = scheme_intern_symbol("init-file");
  x_display_symbol = scheme_intern_symbol("x-display");

  wxScheme_Install(env);

  scheme_finish_primitive_module(env);
  
  get_file = scheme_false;
  put_file = scheme_false;
  get_ps_setup_from_user = scheme_false;
  message_box = scheme_false;

  orig_collect_start_callback = GC_collect_start_callback;
  GC_collect_start_callback = (GC_START_END_PTR)collect_start_callback;
  orig_collect_end_callback = GC_collect_end_callback;
  GC_collect_end_callback = (GC_START_END_PTR)collect_end_callback;
}

extern "C" {

  void scheme_install_xc_global(char *name, Scheme_Object *val, Scheme_Env *env)
    {
      scheme_add_global(name, val, env);
    }
  
  Scheme_Object * scheme_lookup_xc_global(char *name, Scheme_Env *env)
    {
      return scheme_lookup_global(scheme_intern_symbol(name), env);
    }

};

/***********************************************************************/
/*                             gc bitmap                               */
/***********************************************************************/

#ifdef wx_x
extern Display *MrEdGetXDisplay(void);
#endif

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void draw_gc_bm(int on)
{
  GCBitmap *gcbm = gc_bitmaps;

#ifdef MZ_PRECISE_GC
  /* Too hard to make GCBlit et al. unconverted.
     We just save and restore the variable stack instead. */
  void **save_var_stack;
  save_var_stack = GC_variable_stack;
#endif

  while (gcbm) {
    wxCanvas *cnvs = GET_CANVAS(gcbm);
#ifdef MZ_PRECISE_GC
    if (!gcOBJ_TO_PTR(cnvs))
      cnvs = NULL;
#endif
    if (cnvs) {
      /* Due to custodian shutdowns and ordered finalization, it's
	 possible that a canvas will be deleted without yet being
	 collected: */
      if (cnvs->__type != -1) {
	wxCanvasDC *dc;
	dc = (wxCanvasDC *)cnvs->GetDC();
	dc->GCBlit(gcbm->x, gcbm->y,
		   gcbm->w, gcbm->h,
		   on ? gcbm->on : gcbm->off,
		   0, 0);
      }
    }
    gcbm = gcbm->next;
  }

#ifdef MZ_PRECISE_GC
  GC_variable_stack = save_var_stack;
#endif

#ifdef wx_x
  XFlush(MrEdGetXDisplay());
#endif
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

void wxsKeymapError(char *s)
{
  scheme_signal_error("%s", s);
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#endif

static void collect_start_callback(void)
{
  draw_gc_bm(1);
  orig_collect_start_callback();
}

static void collect_end_callback(void)
{
  orig_collect_end_callback();
  draw_gc_bm(0);
}

#ifdef MZ_PRECISE_GC
END_XFORM_SKIP;
#endif

static Scheme_Object *wxSchemeUnregisterCollectingBitmap(int, Scheme_Object **a)
{
  GCBitmap *gcbm, *prev = NULL;
  wxCanvas *c;

  if (a)
    c = objscheme_unbundle_wxCanvas(a[0], "unregister-collecting-blit", 0);
  else
    c = NULL;
  
  gcbm = gc_bitmaps;
  while (gcbm) {
    if (!gcbm->canvasptr || (GET_CANVAS(gcbm) == c)) {
      if (prev)
	prev->next = gcbm->next;
      else
	gc_bitmaps = gcbm->next;
      gcbm->on = gcbm->off = NULL;
      gcbm->canvasptr = NULL;
    } else
      prev = gcbm;
    gcbm = gcbm->next;
  }

  return scheme_void;
}

static Scheme_Object *wxSchemeRegisterCollectingBitmap(int n, Scheme_Object **a)
{
  GCBitmap *gcbm;
  wxCanvas *cvs;

  gcbm = new GCBitmap;

  cvs = objscheme_unbundle_wxCanvas(a[0], "register-collecting-blit", 0);

#ifdef MZ_PRECISE_GC
  {
    void *cp;
    cp = GC_malloc_weak_box(gcOBJ_TO_PTR(cvs), NULL, 0);
    gcbm->canvasptr = (Scheme_Object *)cp;
  }
#else
  gcbm->canvasptr = (wxCanvas **)scheme_malloc_atomic(sizeof(wxCanvas*));
  *gcbm->canvasptr = cvs;
#endif

  gcbm->x = objscheme_unbundle_float(a[1], "register-collecting-blit");
  gcbm->y = objscheme_unbundle_float(a[2], "register-collecting-blit");
  gcbm->w = objscheme_unbundle_nonnegative_float(a[3], "register-collecting-blit");
  gcbm->h = objscheme_unbundle_nonnegative_float(a[4], "register-collecting-blit");
  gcbm->on = objscheme_unbundle_wxBitmap(a[5], "register-collecting-blit", 0);
  gcbm->off = objscheme_unbundle_wxBitmap(a[6], "register-collecting-blit", 0);
  gcbm->onx = gcbm->ony = gcbm->offx = gcbm-> offy = 0;
  if (n > 7) {
    gcbm->onx = objscheme_unbundle_float(a[7], "register-collecting-blit");
    if (n > 8) {
      gcbm->ony = objscheme_unbundle_float(a[8], "register-collecting-blit");
      if (n > 9) {
	gcbm->offx = objscheme_unbundle_float(a[9], "register-collecting-blit");
	if (n > 10) {
	  gcbm->offy = objscheme_unbundle_float(a[10], "register-collecting-blit");
	}
      }
    }
  }

  gcbm->next = gc_bitmaps;
  gc_bitmaps = gcbm;

#ifndef MZ_PRECISE_GC
  GC_general_register_disappearing_link((void **)gcbm->canvasptr, 
					*gcbm->canvasptr);
#endif

  wxSchemeUnregisterCollectingBitmap(0, NULL);

  return scheme_void;
}

/***********************************************************************/
/*                             open gl                                 */
/***********************************************************************/

#ifdef wx_msw
# define USE_GL
#endif
#ifdef wx_mac
# define USE_GL
# ifdef OS_X
#  define PROTECT_GLS
# endif
#endif

#ifdef PROTECT_GLS
static int gl_param;
#endif

#ifdef MPW_CPLUS
extern "C" {
  typedef void (*DW_PRE_PTR)(void *);
  typedef Scheme_Object *(*DW_RUN_PTR)(void *);
  typedef void (*DW_POST_PTR)(void *);
}
# define CAST_DW_PRE (DW_PRE_PTR)
# define CAST_DW_RUN (DW_RUN_PTR)
# define CAST_DW_POST (DW_POST_PTR)
#else
# define CAST_DW_PRE /* empty */
# define CAST_DW_RUN /* empty */
# define CAST_DW_POST /* empty */
#endif

#ifdef USE_GL

extern void wxGLNoContext(void);
static Scheme_Object *context_sema;

#ifdef PROTECT_GLS
/* We can protect a GL context from other threads only if it's ok to
   switch the GL context at any time. It appears to be ok only under
   Mac OS. */
static Scheme_Object *on_thread_swap(Scheme_Object *)
{
  Scheme_Object *o;
  wxGL *c;

  o = scheme_get_param(scheme_config, gl_param);
  if (SCHEME_TRUEP(o))
    c = objscheme_unbundle_wxGL(o, NULL, 0);
  else
    c = NULL;
 
  if (c)
    c->ThisContextCurrent();
  else
    wxGLNoContext();

  return NULL;
}
#endif

static void init_gl_mgr(void)
{
#ifdef PROTECT_GLS
  scheme_set_param(scheme_config, gl_param, scheme_false);
  scheme_add_swap_callback(on_thread_swap, NULL);
#endif
}

static void swap_ctx(void *c)
     /* In the PROTECT_GLS case, We defeat this general
	parameterize-like swap below to keep it in sync with the lock,
	but maybe it will be useful some day. */
{
  Scheme_Object *n;
  wxGL *gl;

  n = ((Scheme_Object **)c)[1];
#ifdef PROTECT_GLS
  {
    Scheme_Object *o;
    o = scheme_get_param(scheme_config, gl_param);
    scheme_set_param(scheme_config, gl_param, n);
    ((Scheme_Object **)c)[1] = o;
  }
#else
  ((Scheme_Object **)c)[1] = scheme_false;
#endif

  if (SCHEME_TRUEP(n)) {
    gl = objscheme_unbundle_wxGL(n, NULL, 0);
    if (gl)
      gl->ThisContextCurrent();
    else
      wxGLNoContext();
  } else
    wxGLNoContext();
}

static void swap_ctx_in(void *c)
{
  if (*(Scheme_Object **)c)
    swap_ctx(c);
}

static Scheme_Object *do_call_ctx(void *c)
{
  return _scheme_apply_multi(((Scheme_Object **)c)[0], 0, NULL);
}

static void swap_ctx_out(void *c)
{
  if (*(Scheme_Object **)c) {
    swap_ctx(c);
    *(Scheme_Object **)c = NULL;
    scheme_post_sema(context_sema);
  }
}

static void release_context_lock(void *c)
{
  wxGLNoContext();
  scheme_post_sema(context_sema);
}

void *wxWithGLContext(wxGL *gl, void *thunk, void *alt_waitable, int eb)
{
  Scheme_Object **a, *wa[3], *glo, *v;
  int waitables;

  if (!context_sema) {
    wxREGGLOB(context_sema);
    context_sema = scheme_make_sema(1);
  }

  a = (Scheme_Object **)scheme_malloc(2 * sizeof(Scheme_Object *));
  glo = objscheme_bundle_wxGL(gl);

  a[0] = (Scheme_Object *)thunk;
  a[1] = (Scheme_Object *)alt_waitable;

  scheme_check_proc_arity("call-as-current in gl-context<%>", 
			  0, 0, 
			  (alt_waitable ? 2 : 1), a);
  if (alt_waitable) {
    if (!scheme_is_waitable((Scheme_Object *)alt_waitable)) {
      scheme_wrong_type("call-as-current in gl-context<%>", "waitable", 1, 2, a);
      return NULL;
    }
    waitables = 3;
    wa[2] = a[1];
  } else
    waitables = 2;

  wa[0] = scheme_false;
  wa[1] = context_sema;

  if (eb)
    v = scheme_object_wait_multiple_enable_break(waitables, wa);
  else
    v = scheme_object_wait_multiple(waitables, wa);

  if (v == context_sema) {
    a[0] = (Scheme_Object *)thunk;
    a[1] = glo;

    BEGIN_ESCAPEABLE(release_context_lock, a);
    v = scheme_dynamic_wind(CAST_DW_PRE swap_ctx_in, 
			    CAST_DW_RUN do_call_ctx, 
			    CAST_DW_POST swap_ctx_out,
			    NULL, a);
    END_ESCAPEABLE();
  }

  return v;
}

#endif


void wxscheme_early_gl_init(void)
{
#ifdef PROTECT_GLS
  gl_param = scheme_new_param();
#endif
}

/***********************************************************************/
/*                          color chooser                              */
/***********************************************************************/

#ifdef wx_msw
static BOOL do_choose_color(void *data, HWND parent)
{
  CHOOSECOLOR *c = (CHOOSECOLOR *)data;
  c->hwndOwner = parent;

  return ChooseColor(c);
}
#endif

#ifdef wx_mac
pascal Boolean NullEventFilter(EventRecord *evt) 
{
  // just dump them all on the color picker
  return false;
}

pascal void MyColorChangedCallback ( SInt32 userData, PMColor *newColor )
{
  // do nothing
  return;
}
#endif

static Scheme_Object *wxSchemeGetColourFromUser(int argc, Scheme_Object **argv)
{
  char *s;
#ifndef wx_x
  wxColour *c;
# ifdef wx_msw
  wxWindow *parent;
# endif
#endif

  if (!argc || SCHEME_FALSEP(argv[0]))
    s = "Choose a color";
  else
    s = objscheme_unbundle_string(argv[0], "get-color-from-user");

#ifndef wx_x
# ifdef wx_msw
  parent = ((argc > 1)
	    ? objscheme_unbundle_wxWindow(argv[1], "get-color-from-user", 1)
	    : NULL);
# endif
  c = ((argc > 2)
       ? objscheme_unbundle_wxColour(argv[2], "get-color-from-user", 1)
       : NULL);
#endif

#ifdef wx_x
  return scheme_false;
#endif
#ifdef wx_mac
# ifdef WX_CARBON
  {
    GC_CAN_IGNORE struct ColorPickerInfo cpInfo;
    
    cpInfo.theColor.profile = NULL; // use the default ColorSync profile
    if (c) {
      cpInfo.theColor.color.rgb.red = c->Red() << 8;
      cpInfo.theColor.color.rgb.green = c->Green() << 8;
      cpInfo.theColor.color.rgb.blue = c->Blue() << 8;
    } else {
      cpInfo.theColor.color.rgb.red = cpInfo.theColor.color.rgb.green = cpInfo.theColor.color.rgb.blue = 0;
    }

    cpInfo.dstProfile = NULL; // default Profile (again!)
    cpInfo.flags = NULL;
    cpInfo.placeWhere = kCenterOnMainScreen;  
    cpInfo.dialogOrigin.h = 0;
    cpInfo.dialogOrigin.v = 0;
    cpInfo.pickerType = 0; 
    cpInfo.eventProc = NewUserEventUPP(NullEventFilter);
    cpInfo.colorProc = NewColorChangedUPP(MyColorChangedCallback);
    cpInfo.colorProcData = 0;
    cpInfo.mInfo.editMenuID = 128; // Not sure this will work.
    CopyCStringToPascal(s,cpInfo.prompt);
    cpInfo.newColorChosen = FALSE;
    
    if (PickColor(&cpInfo) != noErr) {
      return scheme_false;
    }
    if (cpInfo.newColorChosen == FALSE) {
      return scheme_false;
    }
    
    c = new wxColour(cpInfo.theColor.color.rgb.red >> 8, cpInfo.theColor.color.rgb.green >> 8, cpInfo.theColor.color.rgb.blue >> 8);

    return objscheme_bundle_wxColour(c);
  }    
# else
  int l;
  Point pt = {0, 0};
  Str255 buf;
  RGBColor in, out;
  
  CopyCStringToPascal(s,buf);

  if (c) {
    in.red = c->Red() << 8;
    in.green = c->Green() << 8;
    in.blue = c->Blue() << 8;
  } else
    in.red = in.green = in.blue = 0;

  if (!GetColor(pt, buf, &in, &out))
    return scheme_false;

  c = new wxColour(out.red >> 8, out.green >> 8, out.blue >> 8);

  return objscheme_bundle_wxColour(c);
# endif
#endif
#ifdef wx_msw
  {
    CHOOSECOLOR *cc;
    static unsigned long userCustomColors[16];

    cc = (CHOOSECOLOR *)malloc(sizeof(CHOOSECOLOR));
    cc->lStructSize = sizeof(CHOOSECOLOR);
    cc->hwndOwner = NULL; // (parent ? parent->GetHWND() : (HWND)NULL)
    if (c) {
      int rr, gg, bb;
      rr = c->Red();
      gg = c->Green();
      bb = c->Blue();
      cc->rgbResult = RGB(rr, gg, bb);
    }
    cc->Flags = (c ? CC_RGBINIT : 0);
    cc->lpCustColors = userCustomColors;
    
    if (!wxPrimitiveDialog(do_choose_color, cc, 0)) {
      free(cc);
      return scheme_false;
    }
    
    c = new wxColour(GetRValue(cc->rgbResult), GetGValue(cc->rgbResult), GetBValue(cc->rgbResult));
    
    free(cc);

    return objscheme_bundle_wxColour(c);
  }
#endif
}

/***********************************************************************/
/*                           font chooser                              */
/***********************************************************************/

#ifdef wx_msw
static BOOL do_choose_font(void *data, HWND parent)
{
  CHOOSEFONT *c = (CHOOSEFONT *)data;
  c->hwndOwner = parent;

  return ChooseFont(c);
}
#endif

static Scheme_Object *wxSchemeGetFontFromUser(int argc, Scheme_Object **argv)
{
  char *prompt;

  if (!argc || SCHEME_FALSEP(argv[0]))
    prompt = "Choose a font";
  else
    prompt = objscheme_unbundle_string(argv[0], "get-font-from-user");

#ifdef wx_x
  return scheme_false;
#endif
#ifdef wx_mac
  return scheme_false;
#endif
#ifdef wx_msw
  {
    wxWindow *parent;
    wxFont *f;
    CHOOSEFONT *c;
    LOGFONT *lf;
    int len;
    char *s;
    int fontFamily = wxSWISS;
    int fontStyle = wxNORMAL;
    int fontWeight = wxNORMAL;
    int fontPoints = 10;
    Bool fontUnderline = FALSE;
    int lfFamily;

    parent = ((argc > 1)
		      ? objscheme_unbundle_wxWindow(argv[1], "get-font-from-user", 1)
		      : NULL);
    f = ((argc > 2)
	 ? objscheme_unbundle_wxFont(argv[2], "get-font-from-user", 1)
	 : NULL);

    lf = (LOGFONT *)malloc(sizeof(LOGFONT));
    c = (CHOOSEFONT *)malloc(sizeof(CHOOSEFONT));

    s = (f ? f->GetFaceString() : NULL);
    if (s) {
      len = strlen(s);
      if (len > 31)
	len = 31;
    } else
      len = 0;
  
    memcpy(lf->lfFaceName, s, len);
    lf->lfFaceName[len] = 0;
  
    lf->lfHeight = 0;
    lf->lfWidth = 0;
    lf->lfEscapement = 0;
    lf->lfOrientation = 0;
    if (f) {
      switch (f->GetWeight()) {
      case wxBOLD:
	lf->lfWeight = FW_BOLD;
	break;
      case wxLIGHT:
	lf->lfWeight = FW_LIGHT;
      default:
	lf->lfWeight = FW_NORMAL;
      } 
    } else
      lf->lfWeight = FW_NORMAL;
    if (f) {
      switch (f->GetStyle()) {
      case wxITALIC:
      case wxSLANT:
	lf->lfItalic = TRUE;
	break;
      default:
	lf->lfItalic = FALSE;
      } 
    } else
      lf->lfItalic = FALSE;
    if (f) {
      lf->lfUnderline = f->GetUnderlined();
    } else
      lf->lfUnderline = FALSE;
    lf->lfStrikeOut = FALSE;
    lf->lfCharSet = OEM_CHARSET;
    lf->lfOutPrecision = OUT_DEFAULT_PRECIS;
    lf->lfClipPrecision = CLIP_DEFAULT_PRECIS;
    lf->lfQuality = DEFAULT_QUALITY;
    lf->lfPitchAndFamily = DEFAULT_PITCH;
    if (f) {
      switch (f->GetFamily()) {
      case wxDECORATIVE:
	lf->lfPitchAndFamily |= FF_DECORATIVE;
	break;
      case wxMODERN:
	lf->lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
	break;
      case wxROMAN:
	lf->lfPitchAndFamily |= FF_ROMAN;
	break;
      case wxSCRIPT:
	lf->lfPitchAndFamily |= FF_SCRIPT;
	break;
      case wxSWISS:
	lf->lfPitchAndFamily |= FF_SWISS;
	break;
      default:
      case wxDEFAULT:
	lf->lfPitchAndFamily |= FF_DONTCARE;
	break;
      } 
    } else
      lf->lfPitchAndFamily |= FF_DONTCARE;

    c->lStructSize = sizeof(CHOOSEFONT);
    c->hwndOwner = NULL; /* (parent ? parent->GetHWND() : (HWND)NULL) */
    c->lpLogFont = lf;
    if (f) {
      c->iPointSize = 10 * f->GetPointSize();
    } else
      c->iPointSize = 100;
    c->Flags = CF_INITTOLOGFONTSTRUCT | CF_SCREENFONTS;

    if (!wxPrimitiveDialog(do_choose_font, c, 0)) {
      free(c);
      free(lf);
      return scheme_false;
    }
  
    if (!lf->lfFaceName[0])
      s = NULL;
    else
      s = lf->lfFaceName;
    
    lfFamily = lf->lfPitchAndFamily;
    if (lfFamily & FIXED_PITCH)
      lfFamily -= FIXED_PITCH;
    if (lfFamily & VARIABLE_PITCH)
      lfFamily -= VARIABLE_PITCH;
    
    switch (lfFamily)
      {
      case FF_ROMAN:
	fontFamily = wxROMAN;
	break;
      case FF_SWISS:
	fontFamily = wxSWISS;
	break;
      case FF_SCRIPT:
	fontFamily = wxSCRIPT;
	break;
      case FF_MODERN:
	fontFamily = wxMODERN;
	break;
      case FF_DECORATIVE:
	fontFamily = wxDECORATIVE;
	break;
      default:
	fontFamily = wxSWISS;
	break;
      }
    switch (lf->lfWeight)
      {
      case FW_LIGHT:
	fontWeight = wxLIGHT;
	break;
      case FW_NORMAL:
	fontWeight = wxNORMAL;
	break;
      case FW_BOLD:
	fontWeight = wxBOLD;
	break;
      default:
	fontWeight = wxNORMAL;
	break;
      }
    if (lf->lfItalic)
      fontStyle = wxITALIC;
    else
      fontStyle = wxNORMAL;

    if (lf->lfUnderline)
      fontUnderline = TRUE;

    if (s)
      f = new wxFont(c->iPointSize / 10, s, fontFamily, fontStyle, 
		     fontWeight, fontUnderline);
    else
      f = new wxFont(c->iPointSize / 10, fontFamily, fontStyle, 
		     fontWeight, fontUnderline);

    free(c);
    free(lf);

    return objscheme_bundle_wxFont(f);
  }
#endif
}

#ifdef wx_x
static int indirect_strcmp(const void *a, const void *b)
{
  return strcmp(*(char **)a, *(char **)b);
}
#endif


#ifdef wx_msw
typedef struct {
  int mono_only;
  int count, size;
  char **names;
} gfData;

static int CALLBACK get_font(ENUMLOGFONT FAR*  lpelf, 
			     NEWTEXTMETRIC FAR* lpntm, 
			     DWORD type, 
			     LPARAM _data)
{
  gfData *data = (gfData *)_data;
  int l;
  char *s;

  if (data->mono_only) {
    /* TMPF_FIXED_PITCH flag means not monospace */
    if (lpntm->tmPitchAndFamily & TMPF_FIXED_PITCH) {
      return 1;
    }
  }
  
  if (data->count == data->size) {
    char **naya;

    data->size += (2 * data->size) + 10;
    naya = new char*[data->size];
    memcpy(naya, data->names, data->count * sizeof(char *));
    data->names = naya;
  }
  
  l = strlen(lpelf->elfLogFont.lfFaceName);
  s = new char[l + 1];
  memcpy(s, lpelf->elfLogFont.lfFaceName, l);

  data->names[data->count++] = s;

#  ifdef MZ_PRECISE_GC
#   ifndef GC_STACK_CALLEE_RESTORE
  /* Restore variable stack. */
  GC_variable_stack = (void **)__gc_var_stack__[0];
#   endif
#  endif

  return 1;
}
#endif

typedef int (*Indirect_Cmp_Proc)(const void *, const void *);

static Scheme_Object *wxSchemeGetFontList(int argc, Scheme_Object **argv)
{
  Scheme_Object *first = scheme_null, *last = NULL;
  int mono_only = 0;
#ifdef wx_x
  int count, i = 0;
  char **xnames, **names;
  int last_pos = -1, last_len = 0;
#endif
#ifdef wx_mac
  FMFontFamilyIterator iterator;
  FMFontFamily fam;
  Str255 fname;
  char temp[256];
#endif
#ifdef wx_msw
  gfData data;
  HDC dc;
  int i = 0;
#endif

  if (argc > 0) {
    if (!mono_symbol) {
      wxREGGLOB(mono_symbol);
      wxREGGLOB(all_symbol);
      mono_symbol = scheme_intern_symbol("mono");
      all_symbol = scheme_intern_symbol("all");
    }
    if (SAME_OBJ(mono_symbol, argv[0]))
      mono_only = 1;
    else if (!SAME_OBJ(all_symbol, argv[0])) {
      scheme_wrong_type("get-face-list", "'mono or 'all symbol", 0, argc, argv);
      return NULL;
    }
  }
  
#ifdef wx_x
  xnames = XListFonts(wxAPP_DISPLAY, "*", 50000, &count);

  names = new char* [count];
  for (i = 0; i < count; i++) {
    names[i] = xnames[i];
  }

  qsort(names, count, sizeof(char *), 
	(Indirect_Cmp_Proc)indirect_strcmp);

  i = 0;
#endif
#ifdef wx_mac
# ifndef OS_X
#  define kFMDefaultIterationScope 0
# endif

  FMCreateFontFamilyIterator(NULL, NULL, kFMDefaultIterationScope, &iterator);
#endif
#ifdef wx_msw
  data.mono_only = mono_only;
  data.count = data.size = 0;
  data.names = NULL;

  dc = GetDC(NULL);

  EnumFontFamilies(dc, NULL, (FONTENUMPROC)get_font, (LPARAM)&data);
#endif

  while (1) {
    char *s;
    int l;
    Scheme_Object *pr;

#ifdef wx_x
    while ((i < count)
	   && ((last_pos >= 0) 
	       && !strncmp(names[i], names[last_pos], last_len))) {
      i++;
    }
    if (i >= count)
      break;

    last_pos = i;
    if (names[i][0] != '-') {
      l = strlen(names[i]);
    } else {
      int c = 0;
      for (l = 0; names[i][l]; l++) {
	if (names[i][l] == '-') {
	  c++;
	  if (c == 3) {
	    /* Special case: null weight, slant, non-normal */
	    if (names[i][l + 1] == '-') {
	      l++;
	      if (names[i][l + 1] == '-') {
		l++;
		if (names[i][l + 1] == '-')
		  l++;
	      }
	    }
	    break;
	  }
	}
      }
    }
    last_len = l;
    
    s = names[i++];
#endif
#ifdef wx_mac
    if (FMGetNextFontFamily(&iterator, &fam) != noErr)
      break;
    FMGetFontFamilyName(fam, fname);
    CopyPascalStringToC(fname,temp);
    l = strlen(temp);
    s = temp;
#endif
#ifdef wx_msw
    if (i >= data.count)
      break;
    s = data.names[i++];
    l = strlen(s);
#endif
    
    pr = scheme_make_pair(scheme_make_sized_string(s, l, 1), scheme_null);
    if (last)
      SCHEME_CDR(last) = pr;
    else
      first = pr;
    last = pr;
  }

#ifdef wx_x
   XFreeFontNames(xnames);
#endif
#ifdef wx_msw
   ReleaseDC(NULL, dc);
#endif
#ifdef wx_mac
  FMDisposeFontFamilyIterator(&iterator);
#endif

  /* But wait --- there's more! At least under X when Xft is enabled.
     In that case, we want the Xft names, too, and we put them on the
     front. */
#ifdef WX_USE_XFT
  {
    XftFontSet *fs;
    int i, len, ssize;
    char *s, *copy, buf[256];

    if (mono_only) {
      fs = XftListFonts(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY), 
			XFT_SPACING, XftTypeInteger, XFT_MONO,
			NULL, 
			XFT_FAMILY, NULL);
    } else
      fs = XftListFonts(wxAPP_DISPLAY, DefaultScreen(wxAPP_DISPLAY), NULL, XFT_FAMILY, NULL);

    for (i = 0; i < fs->nfont; i++) {
      s = buf;
      ssize = 256;
      do {
	if (XftNameUnparse(fs->fonts[i], s, ssize))
	  break;
	ssize *= 2;
	s = new WXGC_ATOMIC char[ssize];
      } while (1);
      /* Add a space at the fton to indicate "Xft" */
      len = strlen(s);
      copy = new WXGC_ATOMIC char[len + 2];
      memcpy(copy + 1, s, len + 1);
      copy[0] = ' ';
      first = scheme_make_pair(scheme_make_sized_string(copy, len + 1, 0), first);
    }

    first = scheme_make_pair(scheme_make_string(" Sans"), first);
    first = scheme_make_pair(scheme_make_string(" Serif"), first);
    first = scheme_make_pair(scheme_make_string(" Monospace"), first);

    XftFontSetDestroy(fs);
  }
#endif

  return first;
}

/***********************************************************************/
/*                           panel color                               */
/***********************************************************************/

static Scheme_Object *wxSchemeGetPanelBackground(int, Scheme_Object **)
{
  wxColour *c;

#ifdef wx_x
  c = new wxColour(wxGREY);
#endif
#ifdef wx_mac
  c = new wxColour(0xE8, 0xE8, 0xE8);
#endif
#ifdef wx_msw
  DWORD v;

  v = GetSysColor(COLOR_BTNFACE);

  c = new wxColour(GetRValue(v), GetGValue(v), GetBValue(v));
#endif

  return objscheme_bundle_wxColour(c);
}

/***********************************************************************/
/*                            play sound                               */
/***********************************************************************/

#ifndef wx_x

#ifdef wx_mac

# ifdef OS_X
/* In MzScheme in Classic, mredmac.cxx in OS X */
extern int scheme_mac_path_to_spec(const char *filename, FSSpec *spec);
# endif

class AsyncSoundRec {
public:
  Movie mov;
  short file;
  AsyncSoundRec *next;
};

static AsyncSoundRec *playing = NULL;

int IsFinished(void *movie)
{
  MoviesTask((Movie)movie,0);
  return IsMovieDone((Movie)movie);
}

void MyCloseMovie(Movie movie, short resRefNum) 
{
  short osErr;
  DisposeMovie(movie);

  osErr = CloseMovieFile(resRefNum);
  if (osErr != noErr)
    scheme_signal_error("cannot close movie file (errno = %d)", osErr);
}

int movieInitialized = FALSE;

void MovieInitialize(void)
{
  short osErr;
  long result;
  
  osErr = Gestalt(gestaltQuickTime, &result);
  if (osErr != noErr) {
    scheme_signal_error("Movie Toolbox not available");
  }
  
  if (result < 0x03000000) {
    scheme_signal_error("Quicktime 3.0 or later required to play sounds.");
  }
  
  osErr = EnterMovies();
  if (osErr != noErr) {
    scheme_signal_error("Unable to initialize Movie Toolbox (errno = %d)", osErr);
  }
  
  movieInitialized = TRUE;

  wxREGGLOB(playing);
} 
  
  
void wxCheckFinishedSounds(void)
{
  AsyncSoundRec *playptr = playing;
  AsyncSoundRec *last_playptr = NULL;
  
  while (playptr) {
    if (IsFinished((void *)playptr->mov)) {
      if (last_playptr) {
        last_playptr->next = playptr->next;
      } else {
        playing = playptr->next;
      }
      MyCloseMovie(playptr->mov, playptr->file);
    } else
      last_playptr = playptr;
    playptr = playptr->next;
  }      
}      
      
void my_signal_error(char *msg, char *filename, int err)
{
  scheme_signal_error("%s: \"%s\" (errno = %d)", msg, filename, err);
}

#endif

static Scheme_Object *wxPlaySound(int argc, Scheme_Object **argv)
{
  Bool async, ok;
  char *f;
  
  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("play-sound", "string", 0, argc, argv);
  
  async = SCHEME_TRUEP(argv[1]);
  
  f = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
			     SCHEME_STRTAG_VAL(argv[0]),
			     "play-sound",
			     NULL,
			     SCHEME_GUARD_FILE_READ);

#ifdef wx_msw  
  ok = PlaySound(f, NULL, async ? SND_ASYNC : SND_SYNC);
#endif
#ifdef wx_mac
  {
    FSSpec spec;
    short osErr;
    short resRefNum;
    Movie theMovie;
    
    if (! movieInitialized) {
      MovieInitialize();
    }
    
    osErr = scheme_mac_path_to_spec(f,&spec);

    if (! osErr) 
      scheme_signal_error("cannot find file: \"%s\"", SCHEME_STR_VAL(argv[0]));
    
    // load sound as "movie"
    
    osErr = OpenMovieFile(&spec,&resRefNum,fsRdPerm);
    if (osErr != noErr)
      my_signal_error("cannot open as movie file", SCHEME_STR_VAL(argv[0]), osErr);
    
    osErr = NewMovieFromFile(&theMovie, resRefNum, NULL, NULL, newMovieActive, NULL);
    if (osErr != noErr)
      my_signal_error("cannot create movie from file", SCHEME_STR_VAL(argv[0]), osErr);

    // play the movie once thru
    StartMovie(theMovie);
    
    if (!async) {
      wxDispatchEventsUntil(IsFinished,theMovie);
      MyCloseMovie(theMovie, resRefNum);
    } else {
      AsyncSoundRec *r;
      
      r = new AsyncSoundRec;

      r->mov = theMovie;
      r->file = resRefNum;
      r->next = playing;
      playing = r;
    }
    
    ok = TRUE;
  }
#endif  

  return (ok ? scheme_true : scheme_false);
}
#endif

/***********************************************************************/
/*                         constructor hooks                           */
/***********************************************************************/

wxMediaSnip *wxsMakeMediaSnip(wxMediaBuffer *useme,
			      Bool border,
			      int lm, int tm, int rm, int bm,
			      int li, int ti, int ri, int bi,
			      float w, float W, float h, float H)
{
  if (make_media_snip) {
    Scheme_Object *a[20], *r;

    a[0] = (useme ? objscheme_bundle_wxMediaBuffer(useme) : scheme_false);
    a[1] = (border ? scheme_true : scheme_false);
    a[2] = scheme_make_integer(lm);
    a[3] = scheme_make_integer(tm);
    a[4] = scheme_make_integer(rm);
    a[5] = scheme_make_integer(bm);
    a[6] = scheme_make_integer(li);
    a[7] = scheme_make_integer(ti);
    a[8] = scheme_make_integer(ri);
    a[9] = scheme_make_integer(bi);
    a[10] = ((w > 0) ? scheme_make_double(w) : none_symbol);
    a[11] = ((W > 0) ? scheme_make_double(W) : none_symbol);
    a[12] = ((h > 0) ? scheme_make_double(h) : none_symbol);
    a[13] = ((H > 0) ? scheme_make_double(H) : none_symbol);

    r = scheme_apply(make_media_snip, 14, a);

    return objscheme_unbundle_wxMediaSnip(r, NULL, 0);
  } else {
    return new wxMediaSnip(useme, border,
			   lm, tm, rm, bm,
			   li, ti, ri, bi,
			   w, W, h, H);
  }
}

wxMediaEdit *wxsMakeMediaEdit()
{
  if (make_media_edit) {
    return objscheme_unbundle_wxMediaEdit(scheme_apply(make_media_edit, 0, NULL), 
					  NULL, 0);
  } else
    return new wxMediaEdit();
}

wxMediaPasteboard *wxsMakeMediaPasteboard()
{
  if (make_media_pasteboard) {
    return objscheme_unbundle_wxMediaPasteboard(scheme_apply(make_media_pasteboard, 0, NULL), 
						NULL, 0);
  } else
    return new wxMediaPasteboard();
}

static Scheme_Object *SetMediaSnipMaker(int, Scheme_Object *a[])
{
  wxREGGLOB(make_media_snip);
  wxREGGLOB(none_symbol);
  make_media_snip = a[0];
  none_symbol = scheme_intern_symbol("none");
  return scheme_void;
}

static Scheme_Object *SetMediaEditMaker(int, Scheme_Object *a[])
{
  wxREGGLOB(make_media_edit);
  make_media_edit = a[0];
  return scheme_void;
}

static Scheme_Object *SetMediaPasteboardMaker(int, Scheme_Object *a[])
{
  wxREGGLOB(make_media_pasteboard);
  make_media_pasteboard = a[0];
  return scheme_void;
}

static Scheme_Object *is_menu;

Bool wxsCheckIsPopupMenu(void *m)
{
  Scheme_Object *v, *a[1];

  a[0] = (Scheme_Object *)m;
  v = _scheme_apply(is_menu, 1, a);
  return SCHEME_TRUEP(v);
}

static Scheme_Object *SetIsMenu(int, Scheme_Object *a[])
{
  wxREGGLOB(is_menu);
  is_menu = a[0];
  return scheme_void;
}

static Scheme_Object *SetDialogs(int, Scheme_Object *a[])
{
  get_file = a[0];
  put_file = a[1];
  get_ps_setup_from_user = a[2];
  message_box = a[3];
  return scheme_void;
}

static Scheme_Object *snip_class_getter, *editor_data_class_getter;

static Scheme_Object *SetSnipClassGetter(int, Scheme_Object *a[])
{
  wxREGGLOB(snip_class_getter);
  snip_class_getter = a[0];
  return scheme_void;
}

static Scheme_Object *SetEditorDataClassGetter(int, Scheme_Object *a[])
{
  wxREGGLOB(editor_data_class_getter);
  editor_data_class_getter = a[0];
  return scheme_void;
}

wxSnipClass *wxGetSnipClass(const char *name)
{
  if (!snip_class_getter)
    return NULL;
  else {
    Scheme_Object *a[1];
    a[0] = scheme_make_string(name);
    return objscheme_unbundle_wxSnipClass(_scheme_apply(snip_class_getter, 1, a), NULL, 1);
  }
}

wxBufferDataClass *wxGetEditorDataClass(const char *name)
{
  if (!editor_data_class_getter)
    return NULL;
  else {
    Scheme_Object *a[1];
    a[0] = scheme_make_string(name);
    return objscheme_unbundle_wxBufferDataClass(_scheme_apply(editor_data_class_getter, 1, a), NULL, 1);
  }
}

/***********************************************************************/
/*                          interapp hooks                             */
/***********************************************************************/

#ifdef wx_mac
extern short wxMacDisableMods;
#define SCK_ARG p
#else
#define SCK_ARG /**/
#endif

Scheme_Object *wxs_app_file_proc;
Scheme_Object *wxs_app_quit_proc;
Scheme_Object *wxs_app_about_proc;
Scheme_Object *wxs_app_pref_proc;

static Scheme_Object *SpecialCtlKey(int c, Scheme_Object *SCK_ARG[])
{
#ifdef wx_mac
  if (c) {
    if (SCHEME_FALSEP(p[0]))
      wxMacDisableMods -= (wxMacDisableMods & controlKey);
    else
      wxMacDisableMods |= controlKey;
    return scheme_void;
  } else {
    if (wxMacDisableMods & controlKey)
      return scheme_true;
    else
      return scheme_false;
  }
#else
  if (c)
    return scheme_void;
  else
    return scheme_false;
#endif
}

static Scheme_Object *SpecialOptionKey(int c, Scheme_Object *SCK_ARG[])
{
#ifdef wx_mac
  if (c) {
    if (SCHEME_FALSEP(p[0]))
      wxMacDisableMods -= (wxMacDisableMods & optionKey);
    else
      wxMacDisableMods |= optionKey;
    return scheme_void;
  } else {
    if (wxMacDisableMods & optionKey)
      return scheme_true;
    else
      return scheme_false;
  }
#else
  if (c)
    return scheme_void;
  else
    return scheme_false;
#endif
}


static Scheme_Object *DefaultAppFileProc(int n, Scheme_Object *p[])
{
  if (!SCHEME_STRINGP(p[0]))
    scheme_wrong_type("default-application-file-handler", "string",
		      0, n, p);

  return scheme_void;
}

static Scheme_Object *ApplicationFileProc(int n, Scheme_Object *p[])
{
  if (!n)
    return wxs_app_file_proc;
  else {
    scheme_check_proc_arity("application-file-handler", 1,
			    0, n, p);
    wxs_app_file_proc = p[0];
    return scheme_void;
  }
}

static Scheme_Object *DefaultAppQuitProc(int, Scheme_Object **)
{
  return scheme_void;
}

static Scheme_Object *ApplicationQuitProc(int n, Scheme_Object *p[])
{
  if (!n)
    return wxs_app_quit_proc;
  else {
    scheme_check_proc_arity("application-quit-handler", 0, 0, n, p);
    wxs_app_quit_proc = p[0];
    return scheme_void;
  }
}

static Scheme_Object *ApplicationPrefProc(int n, Scheme_Object *p[])
{
  if (!n)
    return wxs_app_pref_proc;
  else {
    wxs_app_pref_proc = p[0];
    return scheme_void;
  }
}

static Scheme_Object *DefaultAppAboutProc(int, Scheme_Object **)
{
#ifdef wx_mac
  wxTheApp->DoDefaultAboutItem();
#endif
  return scheme_void;
}

static Scheme_Object *ApplicationAboutProc(int n, Scheme_Object *p[])
{
  if (!n)
    return wxs_app_about_proc;
  else {
    scheme_check_proc_arity("application-about-handler", 0, 0, n, p);
    wxs_app_about_proc = p[0];
    return scheme_void;
  }
}

static Scheme_Object *SetExecuter(int, Scheme_Object *a[])
{
  wxREGGLOB(executer);
  executer = a[0];
  return scheme_void;
}

void wxsExecute(char **argv)
{
  int i, c;
  Scheme_Object **a, *aa;

  for (i = 0; argv[i]; i++) {
  }

  c = i;
  a = (Scheme_Object **)scheme_malloc(sizeof(Scheme_Object *) * c);

  for (i = 0; i < c; i++) {
    aa = scheme_make_string(argv[i]);
    a[i] = aa;
  }

  (void *)scheme_apply_multi(executer, c, a);
}

#ifdef wx_mac
extern int scheme_mac_send_event(char *name, int argc, Scheme_Object **argv, 
				 Scheme_Object **result, 
				 int *err, char **stage);
#endif

static Scheme_Object *wxSendEvent(int c, Scheme_Object *args[])
{
#ifdef wx_mac
  int err;
  char *stage = "";
  Scheme_Object *result;
  if (scheme_mac_send_event("send-event", c, args, &result, &err, &stage))
    return result;
  else {
    scheme_raise_exn(MZEXN_MISC, "send-event: failed (%s%e)", stage, err);
    return NULL;
  }
#else
  scheme_raise_exn(MZEXN_MISC_UNSUPPORTED,
		   "send-event: not supported on this platform");
  return NULL;
#endif
}

static Scheme_Object *file_type_and_creator(int argc, Scheme_Object **argv)
{
  char *filename;
  int was_dir = 0, write_failed = 0;
  int err;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("file-creator-and-type", "string", 0, argc, argv);

  if (argc > 1) {
    if (!SCHEME_STRINGP(argv[1]) || (SCHEME_STRTAG_VAL(argv[1]) != 4))
      scheme_wrong_type("file-creator-and-type", "4-character string", 1, argc, argv);
    if (!SCHEME_STRINGP(argv[2]) || (SCHEME_STRTAG_VAL(argv[2]) != 4))
      scheme_wrong_type("file-creator-and-type", "4-character string", 2, argc, argv);
  }

  filename = scheme_expand_filename(SCHEME_STR_VAL(argv[0]),
				    SCHEME_STRTAG_VAL(argv[0]),
				    "file-creator-and-type",
				    NULL,
				    (argc > 1) ? SCHEME_GUARD_FILE_WRITE : SCHEME_GUARD_FILE_READ);

#ifdef wx_mac
  {
    FSSpec spec;
    int spec_ok = 0;
    FInfo info;

#ifndef OS_X
    spec_ok = scheme_mac_path_to_spec(filename, &spec);
# else
    {
      FSRef ref;
      Boolean isd;
      
      err = FSPathMakeRef((UInt8*)filename, &ref, &isd);
      if (!err && isd)
	was_dir = 1;
      else if (!err) {
	err = FSGetCatalogInfo(&ref, kFSCatInfoNone, NULL, NULL, &spec, NULL);
	spec_ok = !err;
      }
    }
# endif

    if (spec_ok) {
      err = FSpGetFInfo(&spec, &info);
      if (!err) {
	if (argc > 1) {
	  info.fdCreator = *(unsigned long *)SCHEME_STR_VAL(argv[1]);
	  info.fdType = *(unsigned long *)SCHEME_STR_VAL(argv[2]);
	  err = FSpSetFInfo(&spec, &info);

	  if (!err)
	    return scheme_void;
	  write_failed = 1;
	} else {
	  Scheme_Object *a[2];

	  a[0] = scheme_make_sized_string((char *)&info.fdCreator, 4, 1);
	  a[1] = scheme_make_sized_string((char *)&info.fdType, 4, 1);
	  return scheme_values(2, a);
	}
      }
    }
  }
#else
  err = -1;
  if (scheme_file_exists(filename)) {
    if (argc > 1)
      return scheme_void;
    else {
      Scheme_Object *a[2];

      a[0] = scheme_make_sized_string("????", 4, 0);
      a[1] = a[0];
      return scheme_values(2, a);
    }
  } else if (scheme_directory_exists(filename))
    was_dir = 1;
#endif

  scheme_raise_exn(MZEXN_I_O_FILESYSTEM,
		   argv[0],
		   scheme_false,
		   "file-creator-and-type: %s: \"%q\" (%E)",
		   (was_dir 
		    ? "path is a directory" 
		    : (write_failed 
		       ? "error setting creator and type"
		       : "file not found")),
		   filename, err);
  return NULL;
}

/***********************************************************************/
/*                             ps-setup                                */
/***********************************************************************/

static Scheme_Object *PS_Setup_p(int, Scheme_Object **argv)
{
  return (objscheme_istype_wxPrintSetupData(argv[0], NULL, 0)
	  ? scheme_true
	  : scheme_false);
}

Scheme_Object *wxsBundlePSSetup(wxPrintSetupData *d)
{
  return objscheme_bundle_wxPrintSetupData(d);
}

wxPrintSetupData *wxsUnbundlePSSetup(Scheme_Object *o)
{
  return objscheme_unbundle_wxPrintSetupData(o, NULL, 0);
}

static Scheme_Object *wxSchemeCurrentPSSetup(int argc, Scheme_Object **argv)
{
  if (!argc) {
    wxPrintSetupData *ps;
    ps = wxGetThePrintSetupData();
    return wxsBundlePSSetup(ps);
  }

  return scheme_param_config("current-ps-setup", 
			     scheme_make_integer(mred_ps_setup_param),
			     argc, argv,
			     -1, CAST_SP PS_Setup_p, "ps-setup% instance", 0);
}

/***********************************************************************/
/*                            eventspaces                              */
/***********************************************************************/

static Scheme_Object *Eventspace_p(int, Scheme_Object **argv)
{
  return ((SCHEME_TYPE(argv[0]) == mred_eventspace_type)
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *wxSchemeCurrentEventspace(int argc, Scheme_Object **argv)
{
  return scheme_param_config("current-eventspace", 
			     scheme_make_integer(mred_eventspace_param),
			     argc, argv,
			     -1, CAST_SP Eventspace_p, "eventspace", 0);
}

static Scheme_Object *wxSchemeEventDispatchHandler(int argc, Scheme_Object **argv)
{
  return scheme_param_config("event-dispatch-handler", 
			     scheme_make_integer(mred_event_dispatch_param),
			     argc, argv,
			     1, NULL, NULL, 0);
}

static Scheme_Object *wxSchemeMakeEventspace(int, Scheme_Object **)
{
  return (Scheme_Object *)MrEdMakeEventspace((Scheme_Config *)NULL);
}

static Scheme_Object *wxEventspaceHandlerThread(int argc, Scheme_Object **argv)
{
  if (SCHEME_TYPE(argv[0]) == mred_eventspace_type) {
    Scheme_Object *v;
    v = MrEdEventspaceThread(argv[0]);
    if (!v)
      v = scheme_false;
    return v;
  }

  scheme_wrong_type("eventspace-handler-thread", "eventspace", 0, argc, argv);
  return NULL;
}

static Scheme_Object *queue_callback(int argc, Scheme_Object **argv)
{
  MrEd_add_q_callback("queue-callback", argc, argv);
  return scheme_void;
}

void *wxSchemeYield(void *sema)
{
  if (!wait_symbol) {
    wxREGGLOB(wait_symbol);
    wait_symbol = scheme_intern_symbol("wait");
  }

  if (sema == wait_symbol) {
    mred_wait_eventspace();
    return scheme_true;
  } else if (sema) {
    if (!scheme_is_waitable((Scheme_Object *)sema))
      scheme_wrong_type("yield", "waitable or 'wait", -1, 0, (Scheme_Object **)&sema);

    return wxDispatchEventsUntilWaitable((wxDispatch_Check_Fun)NULL, NULL, (Scheme_Object *)sema);
  } else {
    if (wxYield())
      return scheme_true;
    else
      return scheme_false;
  }
}

static Scheme_Object *wxSchemeCheckForBreak(int, Scheme_Object **)
{
  return (MrEdCheckForBreak()
	  ? scheme_true
	  : scheme_false);
}

static Scheme_Object *Shutdown_p(int argc, Scheme_Object **argv)
{
  Scheme_Type type = SCHEME_TYPE(argv[0]);

  if (type == mred_eventspace_type) {
    return wxsIsContextShutdown((void *)argv[0]) ? scheme_true : scheme_false;
  }

  scheme_wrong_type("eventspace-shutdown?", "eventspace", 0, argc, argv);
  return NULL;
}

static Scheme_Object *main_eventspace_p(int argc, Scheme_Object **argv)
{
  return wxIsUserMainEventspace(argv[0]) ? scheme_true : scheme_false;
}

extern "C" {
  MZ_EXTERN void scheme_start_atomic(void);
  MZ_EXTERN void scheme_end_atomic(void);
}

static Scheme_Object *wxInAtomicRegion(int, Scheme_Object **argv)
{
  if (SCHEME_SEMAP(argv[0])) {
    scheme_wait_sema(argv[0], 0);
    /* MzScheme promises that no break or kill will happen
       between receiving the semaphore post and returning to us. */
    scheme_start_atomic();
  } else
    scheme_end_atomic();

  return scheme_void;
}

/***********************************************************************/
/*                             clipboard                               */
/***********************************************************************/

class wxGetData {
public:
  char *result;
  wxClipboardClient *clipOwner;
  char *format;
  long length;
  Scheme_Object *sema;
};

extern "C" int objscheme_something_prepared;

Scheme_Object *get_data_from_client(void *_gd, int, Scheme_Object **)
{
  wxGetData *gd = (wxGetData *)_gd;
  char *result;
  long length;

  result = gd->clipOwner->GetData(gd->format, &length);

  gd->length = length;
  gd->result = result;
  scheme_post_sema(gd->sema);

  return scheme_void;
}

char *wxsGetDataInEventspace(wxClipboardClient *clipOwner, char *format, long *length)
{
  if (objscheme_something_prepared && clipOwner->context && (clipOwner->context != wxGetContextForFrame())) {
    Scheme_Object *cb, *sema;
    wxGetData *gd;
    
    sema = scheme_make_sema(0);

    gd = new wxGetData;
    gd->clipOwner = clipOwner;
    gd->format = format;
    gd->sema = sema;

    cb = scheme_make_closed_prim((Scheme_Closed_Prim *)get_data_from_client, gd);

    MrEdQueueInEventspace(clipOwner->context, cb);

    if (!scheme_wait_sema(sema, 1)) {
      scheme_thread_block(0);
      scheme_making_progress();
      if (!scheme_wait_sema(sema, 1)) {
	scheme_thread_block(0.001);
	scheme_making_progress();
	if (!scheme_wait_sema(sema, 1)) {
	  scheme_thread_block(0.1);
	  scheme_making_progress();
	  if (!scheme_wait_sema(sema, 1)) {
	    scheme_thread_block(0.5);
	    scheme_making_progress();
	    if (!scheme_wait_sema(sema, 1)) {
	      scheme_thread_block(0.5);
	      scheme_making_progress();
	      if (!scheme_wait_sema(sema, 1)) {
		/* timeout */
		return NULL;
	      }
	    }
	  }
	}
      }
    }

    *length = gd->length;
    return gd->result;
  } else
    return clipOwner->GetData(format, length);
}

/***********************************************************************/
/*                         miscellaneous gui                           */
/***********************************************************************/

Scheme_Object *wxsLocationToWindow(int, Scheme_Object **a)
{
  wxWindow *w;
  w = wxLocationToWindow(SCHEME_INT_VAL(a[0]), SCHEME_INT_VAL(a[1]));
  return objscheme_bundle_wxWindow(w);
}

static Scheme_Object *wxSchemeGetFrameList(int, Scheme_Object **)
{
  return MrEdGetFrameList();
}

static Scheme_Object *wLabelShortcutsVisible(int argc, Scheme_Object **argv)
{
  int menu_too;

  if (argc)
    menu_too = SCHEME_TRUEP(argv[0]);
  else
    menu_too = 0;

#ifdef wx_x
  return menu_too ? scheme_false : scheme_true;
#endif
#ifdef wx_msw
  return scheme_true;
#endif
#ifdef wx_mac
  return scheme_false;
#endif
}

/***********************************************************************/
/*                         files and directories                       */
/***********************************************************************/

#ifdef wx_mac
# ifdef OS_X
/* In MzScheme in Classic, mredmac.cxx in OS X */
extern char *scheme_mac_spec_to_path(FSSpec *spec);
# endif
# ifndef OS_X
#  define wxmac_startup_directory 0
# endif
#endif

enum {
  id_init_file,
  id_setup_file,
  id_x_display
};

#ifdef wx_msw
static char *win_find_home()
{
  char *d, *p;

  d = getenv("HOMEDRIVE");
  p = getenv("HOMEPATH");

  if (d && p) {
    char *s;
    s = new char[strlen(d) + strlen(p) + 1];
    strcpy(s, d);
    strcat(s, p);
    
    if (scheme_directory_exists(s))
      return s;
  }

  {
    int i;
    char *s;

    p = wxTheApp->argv[0];
    s = copystring(p);

    i = strlen(s) - 1;
    
    while (i && (s[i] != '\\')) {
      --i;
    }
    s[i] = 0;
    return s;
  }
} 
#endif

#ifdef wx_x
static char *x_display_str;
extern void wxsRememberDisplay(char *str)
{
  if (str)
    x_display_str = str;
  else
    x_display_str = getenv("DISPLAY");
}
#endif

Scheme_Object *wxSchemeFindDirectory(int argc, Scheme_Object **argv)
{
  int which;

  if (argv[0] == init_file_symbol)
    which = id_init_file;
  else if (argv[0] == setup_file_symbol)
    which = id_setup_file;
  else if (argv[0] == x_display_symbol)
    which = id_x_display;
  else {
    scheme_wrong_type("find-graphical-system-path", "graphical path symbol",
		      0, argc, argv);
    return NULL;
  }

#if defined(wx_x) || defined(OS_X)
  {
    Scheme_Object *home;
    int ends_in_slash;

# ifdef wx_mac
    home = scheme_make_string(scheme_expand_filename("~/Library/Preferences", 2, NULL, NULL, 0));
# else
    home = scheme_make_string(scheme_expand_filename("~/", 2, NULL, NULL, 0));
# endif
    
    ends_in_slash = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1] == '/';
    
    if (which == id_init_file)
      return scheme_append_string(home,
				  scheme_make_string("/.mredrc" + ends_in_slash));
    if (which == id_setup_file)
      return scheme_append_string(home,
				  scheme_make_string("/.mred.resources" + ends_in_slash));

    if (which == id_x_display) {
# if defined(wx_x)
      if (x_display_str)
	return scheme_make_string(x_display_str);
# endif
      return scheme_false;
    }
  }
#endif

#ifdef wx_msw
  {
    Scheme_Object *home;
    int ends_in_slash;
    
    home = scheme_make_string_without_copying(win_find_home());
    
    ends_in_slash = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1];
    ends_in_slash = ((ends_in_slash == '/') || (ends_in_slash == '\\'));
    
    if (which == id_init_file)
      return scheme_append_string(home,
				  scheme_make_string("\\mredrc.ss" + ends_in_slash));
    if (which == id_setup_file)
      return scheme_append_string(home,
				  scheme_make_string("\\mred.ini" + ends_in_slash));  
    
    if (which == id_x_display)
      return scheme_false;
  }
#endif

#if defined(wx_mac) && !defined(OS_X)
  OSType t;
  FSSpec spec;
  Scheme_Object *home;

  if (which == id_x_display)
    return scheme_false;

  switch (which) {
  case id_init_file:
  default:
    t = 'temp';
    break;
  }

  SInt16 vRefNum;
  SInt32 dirID;
  const Str255 fileName = "\p";

  if (!FindFolder(kOnSystemDisk, t, kCreateFolder, &vRefNum, &dirID) == noErr) {
    FSMakeFSSpec(vRefNum,dirID,fileName,&spec);
    home = scheme_make_string(scheme_mac_spec_to_path(&spec));
  } else if (wxmac_startup_directory) {
    home = scheme_make_string(wxmac_startup_directory);
  } else {
    home = scheme_make_string(scheme_os_getcwd(NULL, 0, NULL, 1));
  }
  
  int ends_in_colon;
  ends_in_colon = (SCHEME_STR_VAL(home))[SCHEME_STRTAG_VAL(home) - 1] == ':';

  if (which == id_init_file)
    return scheme_append_string(home,
				scheme_make_string(":mredrc.ss" + ends_in_colon));
  if (which == id_setup_file)
    return scheme_append_string(home,
				scheme_make_string(":mred.fnt" + ends_in_colon));  
#endif

  return scheme_void;
}

char *wxsFileDialog(char *message, char *default_path, 
		    char *default_filename, char *default_extension, 
		    int is_put, wxWindow *parent)
{
  Scheme_Object *a[6], *r;
  
  a[0] = !message ? scheme_false : scheme_make_string(message);
  a[1] = !parent ? scheme_false : objscheme_bundle_wxWindow(parent);
  a[2] = !default_path ? scheme_false : scheme_make_string(default_path);
  a[3] = !default_filename ? scheme_false : scheme_make_string(default_filename);
  a[4] = !default_extension ? scheme_false : scheme_make_string(default_extension);
  a[5] = scheme_null;

  r = scheme_apply(is_put ? put_file : get_file, 6, a);

  if (SCHEME_FALSEP(r))
    return NULL;
  else
    return SCHEME_STR_VAL(r);
}

/***********************************************************************/
/*                            dialog hooks                             */
/***********************************************************************/

extern wxPrintSetupData *wxGetThePrintSetupData();

Bool wxsPrinterDialog(wxWindow *parent)
{
  Scheme_Object *a[4], *r;
  
  a[0] = scheme_false;
  a[1] = !parent ? scheme_false : objscheme_bundle_wxWindow(parent);
  a[2] = scheme_false;
  a[3] = scheme_null;

  r = scheme_apply(get_ps_setup_from_user, 4, a);

  if (SCHEME_FALSEP(r)) {
    return 0;
  } else {
    wxPrintSetupData *p, *p2;
    p = objscheme_unbundle_wxPrintSetupData(r, NULL, 0);
    p2 = wxGetThePrintSetupData();
    p2->copy(p);
    return 1;
  }
}

int wxsMessageBox(char *message, char *caption, long style, wxWindow *parent)
{
  Scheme_Object *a[4], *r;
  
  a[0] = scheme_make_string(caption);
  a[1] = scheme_make_string(message);
  a[2] = !parent ? scheme_false : objscheme_bundle_wxWindow(parent);
  a[3] = ((style & wxYES_NO)
	  ? scheme_intern_symbol("yes-no")
	  : ((style & wxCANCEL)
	     ? scheme_intern_symbol("ok-cancel")
	     : scheme_intern_symbol("ok")));

  a[3] = scheme_make_pair(a[3], scheme_null);    

  r = scheme_apply(message_box, 4, a);

  if (SAME_OBJ(r, scheme_intern_symbol("ok"))) {
    return wxOK;
  }
  if (SAME_OBJ(r, scheme_intern_symbol("cancel"))) {
    return wxCANCEL;
  }
  if (SAME_OBJ(r, scheme_intern_symbol("yes"))) {
    return wxYES;
  }
  return wxNO;
}

/***********************************************************************/
/*                            image types                              */
/***********************************************************************/

int wxsGetImageType(char *fn)
{
  FILE *f;
  int type;
#ifndef MZ_PRECISE_GC
# define GC_CAN_IGNORE /**/
#endif
  GC_CAN_IGNORE unsigned char *expect = NULL;

  f = fopen(fn, "rb");
  if (f) {
    switch ((unsigned)fgetc(f)) {
    case 'B':
      expect = (unsigned char *)"M";
      type = wxBITMAP_TYPE_BMP;
      break;
    case '#':
      expect = (unsigned char *)"define";
      type = wxBITMAP_TYPE_XBM;
      break;
    case '/':
      expect = (unsigned char *)"* XPM */";
      type = wxBITMAP_TYPE_XPM;
      break;
    case 'G':
      expect = (unsigned char *)"IF8";
      type = wxBITMAP_TYPE_GIF;
      break;
    case 0xFF:
      expect = (unsigned char *)"\xD8\xFF";
      type = wxBITMAP_TYPE_JPEG;
      break;
    case 137:
      expect = (unsigned char *)"PNG\r\n";
      type = wxBITMAP_TYPE_PNG;
      break;
    default:
      type = 0;
      break;
    }

    if (expect) {
      while (*expect) {
	if (*expect != fgetc(f)) {
	  type = 0;
	  break;
	}
	expect++;
      }
    }

    fclose(f);
  } else
    type = 0;

  return type ? type : wxBITMAP_TYPE_XBM;
}

/***********************************************************************/
/*                            preferences                              */
/***********************************************************************/

static char *pref_file_cache;
static long pref_file_cache_size;
#define PREF_CACHE_SEG 4096

int wxGetPreference(const char *name, char *res, long len)
{
  int offset, depth, c;

  /* This function duplicates a lot of work that's elsewhere,
     unfornatunately, due to timing constraints (i.e., it's called
     especially early during startup). */

  /******************************************/
  /* Step 1: load the pref file into memory */
  /******************************************/

  if (!pref_file_cache) {
    FILE *fp;
    char *home, *s;
    int l, ends_in_slash;

    wxREGGLOB(pref_file_cache);

    /*************** Unix ***************/

#if defined(wx_xt) || defined(OS_X)
# ifdef wx_mac
    home = scheme_expand_filename("~/Library/Preferences/", -1, NULL, NULL, 0);
# else
    home = scheme_expand_filename("~/", 2, NULL, NULL, 0);
# endif 
    
    l = strlen(home);
    ends_in_slash = (home[l] == '/');
    
    s = new char[l + 20];
    memcpy(s, home, l);
    if (!ends_in_slash)
      s[l++] = '/';
    memcpy(s + l, ".plt-prefs.ss", 14);
#endif

    /*************** Windows ***************/

#ifdef wx_msw
    home = win_find_home();

    l = strlen(home);
    ends_in_slash = ((home[l] == '/') || (home[l] == '\\'));
  
    s = new char[l + 20];
    memcpy(s, home, l);
    if (!ends_in_slash)
      s[l++] = '\\';
    memcpy(s + l, "plt-prefs.ss", 13);
#endif

    /*************** Mac OS Classic ***************/

#if defined(wx_mac) && !defined(OS_X)
    {
      OSType t;
      FSSpec spec;
      SInt16 vRefNum;
      SInt32 dirID;
      const Str255 fileName = "\p";

      if (!FindFolder(kOnSystemDisk, 'pref', kCreateFolder, &vRefNum, &dirID) == noErr) {
	FSMakeFSSpec(vRefNum,dirID,fileName,&spec);
	home = scheme_mac_spec_to_path(&spec);
      } else if (wxmac_startup_directory) {
	home = wxmac_startup_directory;
      } else {
	home = scheme_os_getcwd(NULL, 0, NULL, 1);
      }
    
      l = strlen(home);
      ends_in_slash = (home[l] == ':');
  
      s = new char[l + 20];
      memcpy(s, home, l);
      if (!ends_in_slash)
	s[l++] = ':';
      memcpy(s + l, "plt-prefs.ss", 13);
    }
#endif

    /*************** Common ***************/

    fp = fopen(s, "rb");
    if (!fp)
      return 0;

    pref_file_cache_size = PREF_CACHE_SEG;
    pref_file_cache = new char[pref_file_cache_size];
    offset = 0;

    while (!feof(fp)) {
      long got;

      if (offset + PREF_CACHE_SEG > pref_file_cache_size) {
	s = new char[2 * pref_file_cache_size];
	memcpy(s, pref_file_cache, pref_file_cache_size);
	pref_file_cache_size *= 2;
	pref_file_cache = s;
      }

      got = fread(pref_file_cache + offset, 1, PREF_CACHE_SEG, fp);
      offset += got;
    }
    pref_file_cache_size = offset;

    fclose(fp);
  }

#define cgetc() ((offset < pref_file_cache_size) ? pref_file_cache[offset++] : -1)

  /*******************************************/
  /* Step 2: a lightweight `read'.           */
  /* Assume a real `read' would succeed, and */
  /* assume there are no comments.           */
  /*******************************************/

  offset = 0;
  depth = 0;
  while (offset < pref_file_cache_size) {
    do {
      c = cgetc();
    } while ((c > 0) && isspace(c));

  top:
    
    switch (c) {
    case '(':
      depth++;
      if (depth == 2) {
	/* Maybe the entry we're looking for: */
	do {
	  c = cgetc();
	} while ((c > 0) && isspace(c));
	
	if (c == '|') {
	  char *prefix = "MrEd:";
	  int i;

	  for (i = 0; prefix[i]; i++) {
	    c = cgetc();
	    if (c != prefix[i])
	      break;
	  }
	  if (!prefix[i]) {
	    for (i = 0; name[i]; i++) {
	      c = cgetc();
	      if (c != name[i])
		break;
	    }
	    if (!name[i]) {
	      c = cgetc();
	      if (c == '|') {
		c = cgetc();
		if ((c > 0) && isspace(c)) {
		  int closer = ')';
		  
		  do {
		    c = cgetc();
		  } while ((c > 0) && isspace(c));

		  if (c == '"') {
		    closer = '"';
		    i = 0;
		  } else {
		    res[0] = c;
		    if (c == '\\')
		      res[0] = cgetc();
		    i = 1;
		  }
		  
		  /* Read until closing parenthesis */
		  for (; i < len; i++) {
		    res[i] = cgetc();
		    if (res[i] == '\\') {
		      res[i] = cgetc();
		    } else {
		      if (res[i] == closer) {
			res[i] = 0;
			break;
		      }
		    }
		  }
		  res[len - 1] =0;
		  
		  return 1;
		}

		return 0;
	      }
	    }
	  }
	  /* Need closing | */
	  if (c != '|') {
	    do {
	      c = cgetc();
	    } while (c != '|');
	  }
	  c = cgetc();
	}
	goto top;
      }
      break;
    case ')':
      --depth;
      break;
    case '"':
      do {
	c = cgetc();
	if (c == '\\')
	  cgetc();
      } while ((c != '"') && (c != -1));
      break;
    case '\\':
      cgetc();
      break;
    case '|':
      do {
	c = cgetc();
      } while ((c != '|') && (c != -1));
      break;
    }
  }

  return 0;
}

int wxGetPreference(const char *name, int *res)
{
  char buf[20];

  if (wxGetPreference(name, buf, 20)) {    
    long v;
    char *p;
    v = strtol(buf, &p, 10);
    if (p == (buf + strlen(buf))) {
      *res = v;
      return 1;
    }
  }

  return 0;
}

/***********************************************************************/
/*                            initialization                           */
/***********************************************************************/

static void wxScheme_Install(Scheme_Env *global_env)
{
  wxREGGLOB(wxs_app_quit_proc);
  wxREGGLOB(wxs_app_file_proc);
  wxREGGLOB(wxs_app_about_proc);
  wxREGGLOB(wxs_app_pref_proc);

  wxs_app_file_proc = scheme_make_prim_w_arity(CAST_SP DefaultAppFileProc,
					       "default-application-file-handler",
					       1, 1);
  wxs_app_quit_proc = scheme_make_prim_w_arity(CAST_SP DefaultAppQuitProc,
					       "default-application-quit-handler",
					       0, 0);
  wxs_app_about_proc = scheme_make_prim_w_arity(CAST_SP DefaultAppAboutProc,
					       "default-application-about-handler",
					       0, 0);
  wxs_app_pref_proc = scheme_false;

  scheme_install_xc_global("special-control-key", 
			   scheme_make_prim_w_arity(CAST_SP SpecialCtlKey, 
						    "special-control-key", 
						    0, 1), 
			   global_env);
  scheme_install_xc_global("special-option-key", 
			   scheme_make_prim_w_arity(CAST_SP SpecialOptionKey, 
						    "special-option-key", 
						    0, 1), 
			   global_env);
  
  scheme_install_xc_global("application-file-handler",
			   scheme_make_prim_w_arity(CAST_SP ApplicationFileProc,
						    "application-file-handler",
						    0, 1),
			   global_env);
  scheme_install_xc_global("application-quit-handler",
			   scheme_make_prim_w_arity(CAST_SP ApplicationQuitProc,
						    "application-quit-handler",
						    0, 1),
			   global_env);
  scheme_install_xc_global("application-about-handler",
			   scheme_make_prim_w_arity(CAST_SP ApplicationAboutProc,
						    "application-about-handler",
						    0, 1),
			   global_env);
  scheme_install_xc_global("application-pref-handler",
			   scheme_make_prim_w_arity(CAST_SP ApplicationPrefProc,
						    "application-pref-handler",
						    0, 1),
			   global_env);
  
  scheme_install_xc_global("get-color-from-user",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeGetColourFromUser,
						    "get-color-from-user",
						    0, 3),
			   global_env);
  
  scheme_install_xc_global("get-font-from-user",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeGetFontFromUser,
						    "get-font-from-user",
						    0, 3),
			   global_env);
  
  scheme_install_xc_global("get-face-list",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeGetFontList,
						    "get-face-list",
						    0, 1),
			   global_env);
  
  scheme_install_xc_global("get-panel-background",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeGetPanelBackground,
						    "get-panel-background",
						    0, 0),
			   global_env);
  
#ifdef wx_x
  scheme_install_xc_global("play-sound", scheme_false, global_env);
#else
  scheme_install_xc_global("play-sound", 
			     scheme_make_prim_w_arity(CAST_SP wxPlaySound, 
						      "play-sound", 
						      2, 2), 
			     global_env);
#endif

  scheme_install_xc_global("make-eventspace",
			     scheme_make_prim_w_arity(CAST_SP wxSchemeMakeEventspace,
						      "make-eventspace",
						      0, 0),
			     global_env);
  scheme_install_xc_global("current-eventspace",
			   scheme_register_parameter(CAST_SP wxSchemeCurrentEventspace,
						     "current-eventspace",
						     mred_eventspace_param),
			   global_env);
  scheme_install_xc_global("event-dispatch-handler",
			   scheme_register_parameter(CAST_SP wxSchemeEventDispatchHandler,
						     "event-dispatch-handler",
						     mred_event_dispatch_param),
			   global_env);
  scheme_install_xc_global("eventspace?",
			   scheme_make_prim_w_arity(CAST_SP Eventspace_p,
						    "eventspace?",
						    1, 1),
			   global_env);

  scheme_install_xc_global("current-ps-setup",
			   scheme_register_parameter(CAST_SP wxSchemeCurrentPSSetup,
						     "current-ps-setup",
						     mred_ps_setup_param),
			   global_env);

  scheme_install_xc_global("queue-callback",
			   scheme_make_prim_w_arity(CAST_SP queue_callback,
						    "queue-callback",
						    1, 2),
			   global_env);
  MrEd_mid_queue_key = scheme_make_pair(scheme_false, scheme_false);
  scheme_install_xc_global("middle-queue-key", MrEd_mid_queue_key, global_env);


  scheme_install_xc_global("check-for-break",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeCheckForBreak,
						    "check-for-break",
						    0, 0),
			   global_env);


  scheme_install_xc_global("find-graphical-system-path",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeFindDirectory,
						    "find-graphical-system-path",
						    1, 1),
			   global_env);

  scheme_install_xc_global("get-top-level-windows",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeGetFrameList,
						    "get-top-level-windows",
						    0, 0),
			   global_env);

  scheme_install_xc_global("register-collecting-blit",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeRegisterCollectingBitmap,
						    "register-collecting-blit",
						    7, 11),
			   global_env);
  scheme_install_xc_global("unregister-collecting-blit",
			   scheme_make_prim_w_arity(CAST_SP wxSchemeUnregisterCollectingBitmap,
						    "unregister-collecting-blit",
						    1, 1),
			   global_env);

  scheme_install_xc_global("shortcut-visible-in-label?",
			   scheme_make_prim_w_arity(CAST_SP wLabelShortcutsVisible,
						    "shortcut-visible-in-label?",
						    0, 1),
			   global_env);


  scheme_install_xc_global("eventspace-shutdown?",
			   scheme_make_prim_w_arity(CAST_SP Shutdown_p,
						    "eventspace-shutdown?",
						    1, 1),
			   global_env);
  scheme_install_xc_global("main-eventspace?",
			   scheme_make_prim_w_arity(CAST_SP main_eventspace_p,
						    "main-eventspace?",
						    1, 1),
			   global_env);
  scheme_install_xc_global("eventspace-handler-thread",
			   scheme_make_prim_w_arity(CAST_SP wxEventspaceHandlerThread,
						    "eventspace-handler-thread",
						    1, 1),
			   global_env);

  scheme_install_xc_global("in-atomic-region",
			   scheme_make_prim_w_arity(CAST_SP wxInAtomicRegion,
						    "in-atomic-region",
						    1, 1),
			   global_env);

  scheme_install_xc_global("set-executer",
			   scheme_make_prim_w_arity(CAST_SP SetExecuter,
						    "set-executer",
						    1, 1),
			   global_env);

  scheme_install_xc_global("set-editor-snip-maker",
			   scheme_make_prim_w_arity(CAST_SP SetMediaSnipMaker,
						    "set-editor-snip-maker",
						    1, 1),
			   global_env);
  
  scheme_install_xc_global("set-text-editor-maker",
			   scheme_make_prim_w_arity(CAST_SP SetMediaEditMaker,
						    "set-text-editor-maker",
						    1, 1),
			   global_env);
  
  scheme_install_xc_global("set-pasteboard-editor-maker",
			   scheme_make_prim_w_arity(CAST_SP SetMediaPasteboardMaker,
						    "set-pasteboard-editor-maker",
						    1, 1),
			   global_env);
  scheme_install_xc_global("set-menu-tester",
			   scheme_make_prim_w_arity(CAST_SP SetIsMenu,
						    "set-menu-tester",
						    1, 1),
			   global_env);

  scheme_install_xc_global("set-snip-class-getter",
			   scheme_make_prim_w_arity(CAST_SP SetSnipClassGetter,
						    "set-snip-class-getter",
						    1, 1),
			   global_env);
  scheme_install_xc_global("set-editor-data-class-getter",
			   scheme_make_prim_w_arity(CAST_SP SetEditorDataClassGetter,
						    "set-editor-data-class-getter",
						    1, 1),
			   global_env);
  
  scheme_install_xc_global("location->window",
			   scheme_make_prim_w_arity(CAST_SP wxsLocationToWindow,
						    "location->window",
						    2, 2),
			   global_env);

  scheme_install_xc_global("set-dialogs",
			   scheme_make_prim_w_arity(CAST_SP SetDialogs,
						    "set-dialogs",
						    4, 4),
			   global_env);

  scheme_install_xc_global("send-event",
			   scheme_make_prim_w_arity(CAST_SP wxSendEvent,
						    "send-event",
						    3, 5),
			   global_env);

  scheme_install_xc_global("file-creator-and-type", 
			   scheme_make_prim_w_arity(CAST_SP file_type_and_creator,
						    "file-creator-and-type", 
						    1, 3), 
			   global_env);


#ifdef USE_GL
  init_gl_mgr();
#endif

  /* Order is important! Base class must be initialized before derived. */
  objscheme_setup_wxObject(global_env);
  objscheme_setup_wxWindow(global_env);
  objscheme_setup_wxFrame(global_env);
  objscheme_setup_wxColour(global_env);
  objscheme_setup_wxColourDatabase(global_env);
  objscheme_setup_wxPoint(global_env);
  objscheme_setup_wxBrush(global_env);
  objscheme_setup_wxBrushList(global_env);
  objscheme_setup_wxPen(global_env);
  objscheme_setup_wxPenList(global_env);
  objscheme_setup_wxBitmap(global_env);
  objscheme_setup_wxCursor(global_env);
  objscheme_setup_wxRegion(global_env);
  objscheme_setup_wxFont(global_env);
  objscheme_setup_wxFontList(global_env);
  objscheme_setup_wxFontNameDirectory(global_env);
  objscheme_setup_wxGDIGlobal(global_env);
  objscheme_setup_wxItem(global_env);
  objscheme_setup_wxMessage(global_env);
  objscheme_setup_wxButton(global_env);
  objscheme_setup_wxRadioBox(global_env);
  objscheme_setup_wxCheckBox(global_env);
  objscheme_setup_wxListBox(global_env);
  objscheme_setup_wxChoice(global_env);
  objscheme_setup_wxSlider(global_env);
  objscheme_setup_wxsGauge(global_env);
  objscheme_setup_wxTabChoice(global_env);
  objscheme_setup_wxGroupBox(global_env);
  objscheme_setup_wxMenu(global_env);
  objscheme_setup_wxMenuBar(global_env);
  objscheme_setup_wxsMenuItem(global_env);
  objscheme_setup_wxEvent(global_env);
  objscheme_setup_wxCommandEvent(global_env);
  objscheme_setup_wxPopupEvent(global_env);
  objscheme_setup_wxScrollEvent(global_env);
  objscheme_setup_wxKeyEvent(global_env);
  objscheme_setup_wxMouseEvent(global_env);
  objscheme_setup_wxDC(global_env);
  objscheme_setup_wxMemoryDC(global_env);
  objscheme_setup_wxPostScriptDC(global_env);
  objscheme_setup_basePrinterDC(global_env);
  objscheme_setup_wxGL(global_env);
  objscheme_setup_wxCanvas(global_env);
  objscheme_setup_wxPanel(global_env);
  objscheme_setup_wxDialogBox(global_env);
  objscheme_setup_wxMediaGlobal(global_env);
  objscheme_setup_wxMediaCanvas(global_env);
  objscheme_setup_wxMediaBuffer(global_env);
  objscheme_setup_wxMediaEdit(global_env);
  objscheme_setup_wxMediaPasteboard(global_env);
  objscheme_setup_wxSnipClass(global_env);
  objscheme_setup_wxSnipClassList(global_env);
  objscheme_setup_wxSnip(global_env);
  objscheme_setup_wxTextSnip(global_env);
  objscheme_setup_wxTabSnip(global_env);
  objscheme_setup_wxImageSnip(global_env);
  objscheme_setup_wxMediaSnip(global_env);
  objscheme_setup_wxSnipAdmin(global_env);
  objscheme_setup_wxMediaAdmin(global_env);
  // objscheme_setup_wxCanvasMediaAdmin(global_env);
  objscheme_setup_wxMediaSnipMediaAdmin(global_env);
  objscheme_setup_wxBufferData(global_env);
  objscheme_setup_wxBufferDataClass(global_env);
  objscheme_setup_wxBufferDataClassList(global_env);
  objscheme_setup_wxKeymap(global_env);
  objscheme_setup_wxMediaStreamInBase(global_env);
  objscheme_setup_wxMediaStreamOutBase(global_env);
  objscheme_setup_wxMediaStreamInStringBase(global_env);
  objscheme_setup_wxMediaStreamOutStringBase(global_env);
  objscheme_setup_wxMediaStreamIn(global_env);
  objscheme_setup_wxMediaStreamOut(global_env);
  objscheme_setup_wxMediaWordbreakMap(global_env);
  objscheme_setup_wxGlobalMediaWordbreakMap(global_env);
  objscheme_setup_wxAddColour(global_env);
  objscheme_setup_wxMultColour(global_env);
  objscheme_setup_wxStyleDelta(global_env);
  objscheme_setup_wxStyle(global_env);
  objscheme_setup_wxStyleList(global_env);
  objscheme_setup_wxGlobalStyleList(global_env);
#if 0
  objscheme_setup_baseMetaFile(global_env);
  objscheme_setup_baseMetaFileDC(global_env);
#endif
  objscheme_setup_wxTimer(global_env);
  objscheme_setup_wxClipboard(global_env);
  objscheme_setup_wxClipboardGlobal(global_env);
  objscheme_setup_wxClipboardClient(global_env);
  objscheme_setup_wxPrintSetupData(global_env);

  objscheme_setup_wxsGlobal(global_env);
  objscheme_setup_wxsMenuItemGlobal(global_env);
}


/****************************************************************************/
/*                            phantom tool                                  */
/****************************************************************************/

extern wxBitmap *wx_get_alternate_icon(int little);

static unsigned char check_phantom_module[] = {
40,109,111,100,117,108,101,32,35,37,99,104,
101,99,107,45,112,104,97,110,116,111,109,32,
109,122,115,99,104,101,109,101,10,32,32,40,
112,114,111,118,105,100,101,32,117,115,101,45,
112,104,97,110,116,111,109,63,41,10,32,32,40,
100,101,102,105,110,101,32,117,115,101,45,112,
104,97,110,116,111,109,63,10,32,32,32,32,40,
108,101,116,32,40,91,100,32,40,115,101,99,111,
110,100,115,45,62,100,97,116,101,32,40,99,117,
114,114,101,110,116,45,115,101,99,111,110,100,
115,41,41,93,41,10,32,32,32,32,32,32,40,97,
110,100,32,40,61,32,40,100,97,116,101,45,109,
111,110,116,104,32,100,41,32,55,41,10,9,32,
32,32,40,61,32,40,100,97,116,101,45,100,97,
121,32,100,41,32,50,41,41,41,41,41,10};
static unsigned char wrapper_module[] = {
40,109,111,100,117,108,101,32,35,37,105,110,102,111,45,119,114,97,112,112,101,
114,32,109,122,115,99,104,101,109,101,32,10,32,32,40,112,114,111,118,105,100,
101,32,97,100,100,105,116,105,111,110,41,10,32,32,40,100,101,102,105,110,101,
32,40,99,104,97,105,110,32,102,32,97,32,46,32,114,101,115,116,41,10,32,
32,32,32,40,99,97,115,101,32,97,10,32,32,32,32,32,32,91,40,116,111,
111,108,115,41,32,40,108,105,115,116,32,40,108,105,115,116,32,34,112,104,97,
110,116,111,109,45,116,111,111,108,46,115,115,34,41,41,93,10,32,32,32,32,
32,32,91,40,116,111,111,108,45,110,97,109,101,115,41,32,40,108,105,115,116,
32,34,66,105,114,116,104,100,97,121,34,41,93,10,32,32,32,32,32,32,91,
40,116,111,111,108,45,105,99,111,110,115,41,32,40,108,105,115,116,32,35,102,
41,93,10,32,32,32,32,32,32,91,101,108,115,101,32,40,97,112,112,108,121,
32,102,32,97,32,114,101,115,116,41,93,41,41,10,32,32,40,100,101,102,105,
110,101,32,40,97,100,100,105,116,105,111,110,32,102,41,10,32,32,32,32,40,
99,97,115,101,45,108,97,109,98,100,97,32,10,32,32,32,32,32,91,40,115,
41,32,40,99,104,97,105,110,32,102,32,115,41,93,10,32,32,32,32,32,91,
40,115,32,102,97,105,108,41,32,40,99,104,97,105,110,32,102,32,115,32,102,
97,105,108,41,93,41,41,41,10};
static unsigned char phantom_tool_module[] = {
10,40,109,111,100,117,108,101,32,35,37,109,107,45,116,111,111,108,32,109,122,
115,99,104,101,109,101,10,32,32,40,114,101,113,117,105,114,101,32,40,108,105,
98,32,34,109,114,101,100,46,115,115,34,32,34,109,114,101,100,34,41,10,9,
32,32,32,40,108,105,98,32,34,99,108,97,115,115,46,115,115,34,41,10,9,
32,32,32,40,108,105,98,32,34,117,110,105,116,115,105,103,46,115,115,34,41,
10,9,32,32,32,40,108,105,98,32,34,116,111,111,108,46,115,115,34,32,34,
100,114,115,99,104,101,109,101,34,41,41,10,10,32,32,40,112,114,111,118,105,
100,101,32,109,107,45,116,111,111,108,64,41,10,10,32,32,40,100,101,102,105,
110,101,32,40,109,107,45,116,111,111,108,64,32,98,105,103,45,98,109,32,108,
105,116,116,108,101,45,98,109,41,10,32,32,32,32,40,117,110,105,116,47,115,
105,103,32,100,114,115,99,104,101,109,101,58,116,111,111,108,45,101,120,112,111,
114,116,115,94,10,32,32,32,32,32,32,40,105,109,112,111,114,116,32,100,114,
115,99,104,101,109,101,58,116,111,111,108,94,41,10,32,32,32,32,32,32,10,
32,32,32,32,32,32,40,100,101,102,105,110,101,32,112,104,97,115,101,49,32,
118,111,105,100,41,10,32,32,32,32,32,32,40,100,101,102,105,110,101,32,112,
104,97,115,101,50,32,118,111,105,100,41,10,32,32,32,32,32,32,10,32,32,
32,32,32,32,40,100,101,102,105,110,101,32,40,109,97,107,101,45,110,101,119,
45,117,110,105,116,45,102,114,97,109,101,37,32,115,117,112,101,114,37,41,10,
32,32,32,32,32,32,32,32,40,99,108,97,115,115,32,115,117,112,101,114,37,
10,32,32,32,32,32,32,32,32,32,32,40,105,110,104,101,114,105,116,32,103,
101,116,45,98,117,116,116,111,110,45,112,97,110,101,108,41,10,32,32,32,32,
32,32,32,32,32,32,40,115,117,112,101,114,45,105,110,115,116,97,110,116,105,
97,116,101,32,40,41,41,10,32,32,32,32,32,32,32,32,32,32,10,32,32,
32,32,32,32,32,32,32,32,40,100,101,102,105,110,101,32,98,117,116,116,111,
110,10,32,32,32,32,32,32,32,32,32,32,32,32,40,105,110,115,116,97,110,
116,105,97,116,101,32,98,117,116,116,111,110,37,32,40,108,105,116,116,108,101,
45,98,109,10,9,9,9,9,32,32,40,103,101,116,45,98,117,116,116,111,110,
45,112,97,110,101,108,41,10,9,9,9,9,32,32,40,108,97,109,98,100,97,
32,40,98,117,116,116,111,110,32,101,118,116,41,10,9,9,9,9,32,32,32,
32,40,108,101,116,32,40,91,100,32,40,109,97,107,101,45,111,98,106,101,99,
116,32,100,105,97,108,111,103,37,32,10,9,9,9,9,9,9,9,32,32,34,
72,97,112,112,121,32,66,105,114,116,104,100,97,121,44,32,82,111,98,98,121,
33,34,10,9,9,9,9,9,9,9,32,32,35,102,41,93,41,10,9,9,9,
9,32,32,32,32,32,32,40,109,97,107,101,45,111,98,106,101,99,116,32,109,
101,115,115,97,103,101,37,32,98,105,103,45,98,109,32,100,41,10,9,9,9,
9,32,32,32,32,32,32,40,109,97,107,101,45,111,98,106,101,99,116,32,98,
117,116,116,111,110,37,32,34,72,97,112,112,121,32,66,105,114,116,104,100,97,
121,32,82,111,98,98,121,33,34,32,10,9,9,9,9,9,9,32,32,32,100,
10,9,9,9,9,9,9,32,32,32,40,108,97,109,98,100,97,32,40,98,32,
101,41,10,9,9,9,9,9,9,32,32,32,32,32,40,115,101,110,100,32,100,
32,115,104,111,119,32,35,102,41,41,10,9,9,9,9,9,9,32,32,32,39,
40,98,111,114,100,101,114,41,41,10,9,9,9,9,32,32,32,32,32,32,40,
115,101,110,100,32,100,32,115,104,111,119,32,35,116,41,41,41,41,10,9,9,
9,32,91,115,116,121,108,101,32,39,40,100,101,108,101,116,101,100,41,93,41,
41,10,9,32,32,40,115,101,110,100,32,40,103,101,116,45,98,117,116,116,111,
110,45,112,97,110,101,108,41,32,99,104,97,110,103,101,45,99,104,105,108,100,
114,101,110,10,9,9,40,108,97,109,98,100,97,32,40,108,41,32,40,99,111,
110,115,32,98,117,116,116,111,110,32,108,41,41,41,41,41,10,32,32,32,32,
32,32,10,32,32,32,32,32,32,40,100,114,115,99,104,101,109,101,58,103,101,
116,47,101,120,116,101,110,100,58,101,120,116,101,110,100,45,117,110,105,116,45,
102,114,97,109,101,32,109,97,107,101,45,110,101,119,45,117,110,105,116,45,102,
114,97,109,101,37,32,35,102,41,41,41,41,10,  };

static int checked = 0;
static const char interesting_coll[6] = { 109, 122, 108, 105, 98, 0 };

static Scheme_Object *phantom_tool_hook(int c, Scheme_Object **argv)
{
  Scheme_Object *dr, *a[2], *wrap, *v;

  if (!checked) {
    checked = 1;
    scheme_eval_string((char *)check_phantom_module, scheme_get_env(scheme_config));
    a[0] = scheme_intern_symbol("#%check-phantom");
    a[1] = scheme_intern_symbol("use-phantom?");
    dr = scheme_builtin_value("dynamic-require");
    v = _scheme_apply(dr, 2, a);
    if (SCHEME_FALSEP(v)) {
      scheme_module_demand_hook = NULL;
      return NULL;
    }
  }

  if (c == 1) {
    /* Mode 1 => interesting info file? */
    v = argv[0];
    if (SCHEME_SYM_VAL(v)[0] == ',') {
      GC_CAN_IGNORE char *s;
      int depth;

      depth = SCHEME_SYM_LEN(v);
      s = SCHEME_SYM_VAL(v);
      if ((depth > 11) && !strcmp("info", s + depth - 4)) {
	int skipped_one = 0;
	depth -= 4;
	while ((depth > 6)
	       && ((s[depth - 1] == '/')
		   || (s[depth - 1] == '\\')
		   || (s[depth - 1] == ':'))) {
	  depth--;
	  skipped_one = 1;
	}
	if (skipped_one 
	    && (depth > 6) 
	    && !memcmp(interesting_coll, s + depth - 5, 5)) {
	  return scheme_intern_symbol("#%info-lookup");
	}
      }
    }
    return NULL;
  } else if (c == 3) {
    /* Mode 3 => wrap info content */
    v = argv[0];
    scheme_eval_string((char *)wrapper_module, scheme_get_env(scheme_config));
    a[0] = scheme_intern_symbol("#%info-wrapper");
    a[1] = scheme_intern_symbol("addition");
    dr = scheme_builtin_value("dynamic-require");
    wrap = _scheme_apply(dr, 2, a);

    a[0] = v;
    return _scheme_apply(wrap, 1, a);
  } else {
    /* Mode 2 => dynamic require remap */
    if (SCHEME_SYMBOLP(argv[1]) 
	&& (SCHEME_SYM_LEN(argv[1]) == 5)
	&& !strcmp(SCHEME_SYM_VAL(argv[1]), "tool@")) {
      if (SCHEME_PAIRP(argv[0])) {
	Scheme_Object *ak, *s, *lib;
	lib = scheme_intern_symbol("lib");
	if (SAME_OBJ(lib, SCHEME_CAR(argv[0]))
	    && SCHEME_PAIRP(SCHEME_CDR(argv[0]))) {
	  ak = SCHEME_CDR(argv[0]);
	  s = SCHEME_CAR(ak);
	  if (SCHEME_STRINGP(s)
	      && (SCHEME_STRTAG_VAL(s) == 15)
	      && !strcmp(SCHEME_STR_VAL(s), "phantom-tool.ss")) {
	    ak = SCHEME_CDR(ak);
	    if (SCHEME_NULLP(ak)) {
	      s = scheme_true;
	    } else if (SCHEME_PAIRP(ak) 
		       && SCHEME_NULLP(SCHEME_CDR(ak))) {
	      s = SCHEME_CAR(ak);
	      if (SCHEME_STRINGP(s)
		  && (SCHEME_STRTAG_VAL(s) == 5)
		  && !strcmp(SCHEME_STR_VAL(s), interesting_coll)) {
		s = scheme_true;
	      } else
		s = NULL;
	    } else
	      s = NULL;

	    if (s) {
	      wxBitmap *big, *little;
	      big = wx_get_alternate_icon(0);
	      little = wx_get_alternate_icon(1);
	      scheme_eval_string((char *)phantom_tool_module, scheme_get_env(scheme_config));
	      dr = scheme_builtin_value("dynamic-require");
	      a[0] = scheme_intern_symbol("#%mk-tool");
	      a[1] = scheme_intern_symbol("mk-tool@");
	      v = scheme_apply(dr, 2, a);
	      a[0] = objscheme_bundle_wxBitmap(big);
	      a[1] = objscheme_bundle_wxBitmap(little);
	      return _scheme_apply(v, 2, a);
	    }
	  }
	}
      }
    }

    return NULL;
  }
}

void wxscheme_prepare_hooks(int argc, char **argv)
{
  /* Check whether we're running a certain popular PLT program.
     Install a MzScheme hook if so. */
  int i, delta, ender;
  char *s;

  for (i = 0; i < argc; i++) {
    s = argv[i];
    
    ender = 0;
    if (!i) {
      delta = strlen(s);
#ifndef wx_xt
# ifdef wx_msw
      if ((delta >= 12) && (s[delta-4] == '.')
	  && (s[delta-3] == 'e') && (s[delta-2] == 'x')
	  && (s[delta-1] == 'e')) {
	delta -= 4;
	ender = '.';
      } else
	delta = 0;
# endif
      if ((delta >= 10) && (s[delta-2] == '3')
	  && (s[delta-1] == 'm')) {
	delta -= 2;
	ender = '3';
      }

      if (delta >= 8)
	delta -= 8;
#endif
    } else
      delta = 0;

    if (((s[delta+0] == 100)  || (s[delta+0] == 68))
	&& (s[delta+1] == 114)
	&& ((s[delta+2] == 115)  || (s[delta+2] == 83))
	&& (s[delta+3] == 99)
	&& (s[delta+4] == 104) && (s[delta+5] == 101)
	&& (s[delta+6] == 109) && (s[delta+7] == 101)
	&& (s[delta+8] == ender)) {
      scheme_module_demand_hook = CAST_SP phantom_tool_hook;
      break;
    }
  }
}
