
@INCLUDE prefix.xci

#include "wx_utils.h"
#include "wx_dialg.h"
#include "wx_cmdlg.h"
#include "wx_timer.h"
#include "wx_dcps.h"
#include "wx_main.h"

#if USE_METAFILE
#include "wx_mf.h"
#endif

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS fileSelMode > ONE
@SYM "open" : wxOPEN
@SYM "save" : wxSAVE
@SYM "overwrite-prompt" : wxOVERWRITE_PROMPT
@SYM "hide-readonly" : wxHIDE_READONLY
@ENDSYMBOLS

#define USE_PRINTER 1

extern Bool wxSchemeYield(void *sema);

extern void wxFlushDisplay(void);

#ifdef wx_x
#define FILE_SEL_DEF_PATTERN "*"
#else
#define FILE_SEL_DEF_PATTERN "*.*"
#endif

#define BUF_SIZE 1000
static char buffer[BUF_SIZE];

static char *wxStripMenuCodes_Scheme(char *in)
{
  static char *buffer = NULL;
  static long buflen = 0;
  long len;

  len = strlen(in);
  if (buflen <= len) {
    if (buffer)
      delete[] buffer;
    buflen = 2 * len + 1;
    buffer = new char[buflen];
  }

  wxStripMenuCodes(in, buffer);
  return buffer;
}

#ifdef wx_xt
extern void wxBell(void);
#endif

@GLOBAL wxsGlobal

extern int objscheme_istype_wxFrame(Scheme_Object *obj, const char *stop, int nullOK);
extern class wxFrame *objscheme_unbundle_wxFrame(Scheme_Object *obj, const char *where, int nullOK);
extern int objscheme_istype_wxDialogBox(Scheme_Object *obj, const char *stop, int nullOK);
extern class wxDialogBox *objscheme_unbundle_wxDialogBox(Scheme_Object *obj, const char *where, int nullOK);

@MACRO ubFrameDialog[who] = (((n <= {s}) || XC_SCHEME_NULLP({x})) ? (wxWindow *)NULL : (objscheme_istype_wxFrame({x}, NULL, 1) ? (wxWindow *)objscheme_unbundle_wxFrame({x}, NULL, 0) : (objscheme_istype_wxDialogBox({x}, NULL, 1) ? (wxWindow *)objscheme_unbundle_wxDialogBox({x}, NULL, 0) : (scheme_wrong_type(<who>, "frame% or dialog-box%", -1, 0, &{x}), (wxWindow *)NULL))))
@MACRO cFrameDialog = (objscheme_istype_wxFrame({x}, NULL, 1) || objscheme_istype_wxDialogBox({x}, NULL, 1))

@ "file-selector" : nstring wxFileSelector(string,nstring=NULL,nstring=NULL,nstring=NULL,string=FILE_SEL_DEF_PATTERN,SYM[fileSelMode]=wxOPEN,wxWindow^//ubFrameDialog["file-selector"]/cFrameDialog=NULL,int=-1,int=-1);

@ "color-display?" : bool wxColourDisplay();
@ "display-depth" : int wxDisplayDepth();

#if !USE_METAFILE
#define wxMakeMetaFilePlaceable(a,b,c,d,e,f) TRUE
#endif
@ "make-meta-file-placeable" : bool wxMakeMetaFilePlaceable(string,float,float,float,float,float);


@ "begin-busy-cursor" : void wxBeginBusyCursor()
@ "is-busy?" : bool wxIsBusy();
@ "end-busy-cursor" : void wxEndBusyCursor();
@ "bell" : void wxBell();
@ "display-size" : void wxDisplaySize(int*,int*);

@ "label->plain-label" : string wxStripMenuCodes_Scheme(string);

@ "get-resource" : bool wxGetResource(string,string,string*,nstring=NULL); <> string
@ "get-resource" : bool wxGetResource(string,string,long*,nstring=NULL); <> number
@ "write-resource" : bool wxWriteResource(string,string,string,nstring=NULL); <> string
@ "write-resource" : bool wxWriteResource(string,string,long,nstring=NULL); <> number

@MACRO BundleVoidStar = (void *){x}
@MACRO spSema = semaphore

@ "yield" : bool wxSchemeYield(void[]=NULL//BundleVoidStar///spSema);
@ "flush-display" : void wxFlushDisplay();

@END
