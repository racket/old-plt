
@INCLUDE prefix.xci

#include "wx_obj.h"
#include "wx_win.h"
#include "wx_timer.h"
#include "wx_types.h"
#include "wx_stdev.h"
#include "wx_dc.h"
#include "wx_dcps.h"
#include "wx_clipb.h"

@INCLUDE wxs.xci

@HEADER

#if !defined(wx_mac)
#define NEWEST_TYPES 1
#else
#define NEWEST_TYPES 0
#endif

#ifdef wx_msw

#include "wx_mf.h"

class baseMetaFile : public wxMetaFile
{
};

#else

class baseMetaFile : public wxObject
{
public:
  Bool Ok() { return FALSE; }
  void Play(wxDC*) { }
  Bool SetClipboard(int, int) { return FALSE; }

};

#endif

@CLASSBASE baseMetaFile "meta-file" : "object"

// @CREATOR (string=NULL);

@ "ok?" : bool Ok();
@ "play" : void Play(wxDC!);
@ "set-clipboard" : bool SetClipboard(int=0,int=0);

@END

@MACRO rFALSE = return FALSE;

@CLASSBASE wxTimer "timer" : "object"

@CREATOR ();

@ "interval" : int Interval();
@ v "notify" : void Notify();
@ "start" : bool Start(int,bool=FALSE); : : : : rFALSE
@ "stop" : void Stop();

@END


void AddType(wxClipboardClient *c, char *s) 
{ 
  c->formats.Add(s); 
}

Scheme_Object *GetTypes(wxClipboardClient *c)
{
  wxNode *n = c->formats.First();
  Scheme_Object *first = scheme_null, *last = NULL;
  for (; n; n = n->Next()) {
    Scheme_Object *p;
    
    p = scheme_make_pair(scheme_make_string((char *)n->Data()), scheme_null);
    if (last)
      SCHEME_CDR(last) = p;
    else
      first = p;
    last = p;
  }
  
  return first;
}

@MACRO makeSizedString[i] = (r ? scheme_make_sized_string(r, _x<i>, 1) : XC_SCHEME_NULL)

@CLASSBASE wxClipboard "clipboard" : "object"
@INTERFACE "clipboard"

@ "set-clipboard-client" : void SetClipboardClient(wxClipboardClient!,long);
@ "set-clipboard-string" : void SetClipboardString(string,long);
@ "get-clipboard-client" : wxClipboardClient^ GetClipboardClient();
@ "get-clipboard-string" : nstring GetClipboardString(long);
@ "get-clipboard-data" : nstring/makeSizedString[1] GetClipboardData(string,-long*,long);

@CONSTANT "the-clipboard" : wxClipboard^ wxTheClipboard

@END


@MACRO setStringSize[cn] = if (SCHEME_STRINGP(v)) (*x<cn>) = SCHEME_STRTAG_VAL(v);
@MACRO identity = {x}
@MACRO XrNULL = return NULL;

@MACRO sbString = str

@CLASSBASE wxClipboardClient "clipboard-client" : "object"

@CREATOR ()

@ V "being-replaced" : void BeingReplaced();
@ V "get-data" : nstring GetData(string,-long*); : //setStringSize[1] : : : XrNULL

@ m "add-type" : void AddType(string);
@ m "get-types" : Scheme_Object*/identity//sbString GetTypes();

@END


@BEGINSYMBOLS psMode > ONE
@SYM "preview" : PS_PREVIEW
@SYM "file" : PS_FILE
@SYM "printer" : PS_PRINTER
@ENDSYMBOLS

@BEGINSYMBOLS psOrientation > ONE
@SYM "portrait" : PS_PORTRAIT
@SYM "landscape" : PS_LANDSCAPE
@ENDSYMBOLS


@CLASSBASE wxPrintSetupData "ps-setup" : "object"

@CREATOR ()

@ "get-command" : string GetPrinterCommand();
@ "get-file" : string GetPrinterFile();
@ "get-preview-command" : string GetPrintPreviewCommand();
@ "get-mode" : SYM[psMode] GetPrinterMode();
@ "get-orientation" : SYM[psOrientation] GetPrinterOrientation();
@ "get-options" : string GetPrinterOptions();
@ "get-scaling" : void GetPrinterScaling(float*,float*);
@ "get-translation" : void GetPrinterTranslation(float*,float*);
@ "get-paper-name" : nstring GetPaperName();
@ "get-afm-path" : nstring GetAFMPath();
@ "get-level-2" : bool GetLevel2();

@ "set-command" : void SetPrinterCommand(string);
@ "set-file" : void SetPrinterFile(pathname);
@ "set-preview-command" : void SetPrintPreviewCommand(string); 
@ "set-mode" : void SetPrinterMode(SYM[psMode]);
@ "set-orientation" : void SetPrinterOrientation(SYM[psOrientation]);
@ "set-options" : void SetPrinterOptions(string);
@ "set-scaling" : void SetPrinterScaling(nnfloat,nnfloat);
@ "set-translation" : void SetPrinterTranslation(float,float);
@ "set-paper-name" : void SetPaperName(nstring);
@ "set-afm-path" : void SetAFMPath(nstring);
@ "set-level-2" : void SetLevel2(bool);

@ "copy-from" : void copy(wxPrintSetupData%);

@END
