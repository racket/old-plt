
@INCLUDE prefix.xci

#include "wx_media.h"

@INCLUDE wxs.xci

@HEADER

@SET TYPE = float
@SET NOTEST = 1
@INCLUDE list.xci

@MACRO rNULL = return NULL;
@MACRO rFALSE = return FALSE;
@MACRO rZERO = return 0;

@BEGINSYMBOLS selType > ONE
@SYM "default" : wxDEFAULT_SELECT
@SYM "x" : wxX_SELECT
@SYM "local" : wxLOCAL_SELECT
@ENDSYMBOLS

@BEGINSYMBOLS move > ONE
@SYM "simple" : wxMOVE_SIMPLE
@SYM "line" : wxMOVE_LINE
@SYM "page" : wxMOVE_PAGE
@SYM "word" : wxMOVE_WORD
@ENDSYMBOLS

@BEGINSYMBOLS findKind > ONE
@SYM "before-or-none" : wxSNIP_BEFORE_OR_NULL
@SYM "before" : wxSNIP_BEFORE
@SYM "after" : wxSNIP_AFTER
@SYM "after-or-none" : wxSNIP_AFTER_OR_NULL
@ENDSYMBOLS

@BEGINSYMBOLS breakType > ONE
@SYM "caret" : wxBREAK_FOR_CARET
@SYM "line" : wxBREAK_FOR_LINE
@SYM "selection" : wxBREAK_FOR_SELECTION
@SYM "user1" : wxBREAK_FOR_USER_1
@SYM "user2" : wxBREAK_FOR_USER_2
@ENDSYMBOLS

@INCLUDE wxs_fcs.xci

@CLASSBASE wxMediaEdit "text" : "editor"

@CREATOR (float=1.0,float[]=NULL/bList/ubList/cList,-int=0); : : /glueListSet[float.1.1.2.METHODNAME("text%","initialization")]//

@CLASSID wxTYPE_MEDIA_EDIT

@VAR Scheme_Object *scroll_closure;

@SETMARK X = 
@SETMARK Y = d
@SETMARK Z = d
@INCLUDE wxs_mbuf.xci

@ "get-position" : void GetPosition(long?,long?=NULL);
@ "get-start-position" : long GetStartPosition();
@ "get-end-position" : long GetEndPosition();
@ "set-position" : void SetPosition(long,long=-1,bool=FALSE,bool=TRUE,SYM[selType]=wxDEFAULT_SELECT);
@ "set-position-bias-scroll" : void SetPositionBiasScroll(int,long,long=-1,bool=FALSE,bool=TRUE,SYM[selType]=wxDEFAULT_SELECT);
@ "move-position" :  void MovePosition(long,bool=FALSE,SYM[move]=wxMOVE_SIMPLE);
@ "scroll-to-position" : bool ScrollToPosition(long,bool=FALSE,long=-1,int=0);
@ "get-visible-position-range" : void GetVisiblePositionRange(long?,long?);
@ "get-visible-line-range" : void GetVisibleLineRange(long?,long?);

@ v "set-anchor" : void SetAnchor(bool);
@ "get-anchor" : bool GetAnchor();

@ "flash-on" : void FlashOn(long,long,bool=FALSE,bool=TRUE,long=500);
@ "flash-off" : void FlashOff();

@MACRO setStringLen[i.s] = x<i> = SCHEME_STRTAG_VAL(p[<s>]);
@MACRO checkStringLen[i.s] = if ((x<i> < 0) || (x<i> > SCHEME_STRTAG_VAL(p[<s>]))) scheme_signal_error("%s",METHODNAME("text%","insert")": bad string length");

@ "insert" : void Insert(-long,string,long,long=-1,bool=TRUE);  : : /setStringLen[0.0] <> string and position
@ "insert" : void Insert(-long,string);  : : /setStringLen[0.0] <> string without position
@ "insert" : void Insert(long,string,long,long=-1,bool=TRUE);  : : /checkStringLen[0.1] <> length and string without position
@ "insert" : void Insert(long,string);  : : /checkStringLen[0.1] <> length, string, and position
@ "insert" : void Insert(wxSnip!,long,long=-1,bool=TRUE); <> snip% and position
@ "insert" : void Insert(uchar); <> character without position
@ "insert" : void Insert(uchar,long,long=-1); <> character and position

@ "delete" : void Delete(long, long=-1,bool=TRUE); <> position
@ "delete" : void Delete(); <> no position
@ "erase" :  void Erase();

@ "cut" :  void Cut(bool,long,long,long=-1); <> position
@ "copy" : void Copy(bool,long,long,long=-1); <> position
@ "paste" : void Paste(long,long,long=-1); <> position
@ "paste-next" : void PasteNext();
@ "kill" : void Kill(long,long,long); <> position

@ v "do-copy" : void DoCopy(long,long,long,bool);
@ v "do-paste" : void DoPaste(long,long);

@ "change-style" : void ChangeStyle(wxStyleDelta^,long,long=-1); <> style-delta% and position
@ "change-style" : void ChangeStyle(wxStyle^,long=-1,long=-1); <> style%
			
@ "split-snip" : void SplitSnip(long);

@ "find-position" : long FindPosition(float,float,bool?=NULL,bool?=NULL,float?=NULL);
@ "find-line" : long FindLine(float,bool?=NULL);
@ "find-position-in-line" : long FindPositionInLine(long,float,bool?=NULL,bool?=NULL,float?=NULL);

@ "get-between-threshold" : float GetBetweenThreshold();
@ "set-between-threshold" : void SetBetweenThreshold(float);

@ "position-line" : long PositionLine(long,bool=FALSE);
@ "position-location" :  void PositionLocation(long,float?=NULL,float?=NULL,bool=TRUE,bool=FALSE,bool=FALSE);
@ "line-location" : float LineLocation(long,bool=TRUE);

@ "line-start-position" : long LineStartPosition(long,bool=TRUE);
@ "line-end-position" : long LineEndPosition(long,bool=TRUE);
@ "line-length" : long LineLength(long);
@ "last-position" : long LastPosition();
@ "last-line" : long LastLine();

@ "position-paragraph" : long PositionParagraph(long,bool=FALSE);
@ "paragraph-start-position" : long ParagraphStartPosition(long,bool=TRUE);
@ "paragraph-end-position" : long ParagraphEndPosition(long,bool=TRUE);
@ "line-paragraph" : long LineParagraph(long);
@ "paragraph-start-line" : long ParagraphStartLine(long);
@ "paragraph-end-line" : long ParagraphEndLine(long);
@ "last-paragraph" : long LastParagraph();

@MACRO CHECKDIR[p] = if ((x<p> != 1) && (x<p> != -1)) scheme_signal_error("%s%d", METHODNAME("text%","find-string")": direction must be 1 or -1, given ", x<p>);

@ "find-string" : long FindString(string,int=1,long=-1,long=-1,bool=TRUE,bool=TRUE);  : : /CHECKDIR[1]

@SET TYPE = long
@SET NOTEST = 1
@INCLUDE list.xci

@ "find-string-all" : long[]/bReturnList[long.1] FindStringAll(string,-long*,int=1,long=-1,long=-1,bool=TRUE,bool=TRUE);

@ "find-snip" : wxSnip^ FindSnip(long,SYM[findKind],long?=NULL)
@ "get-snip-position-and-location" : void GetSnipPositionAndLocation(wxSnip!,long?,float?,float?);
@ "get-snip-position" : long GetSnipPosition(wxSnip!);

@MACRO makeNoCopyString[len] = scheme_make_sized_string(r, <len>, 0)

@ "get-text" : string/makeNoCopyString[_x4] GetText(long=-1,long=-1,bool=FALSE,bool=FALSE,-long*=NULL);
@ "get-character" : uchar GetCharacter(long);

@ "insert-file" : bool InsertFile(string,SYM[fileType]=wxMEDIA_FF_GUESS);

@ "read-from-file" : bool ReadFromFile(wxMediaStreamIn%,long,bool=FALSE); <> with position
@ "write-to-file" : bool WriteToFile(wxMediaStreamOut%,long,long=-1); <> with position

@ "get-file-format" : SYM[fileType] GetFileFormat();
@ "set-file-format" : void SetFileFormat(SYM[fileType]);

@ "get-overwrite-mode" : bool GetOverwriteMode();
@ "set-overwrite-mode" : void SetOverwriteMode(bool);

@MACRO checkNull = if (!x0) x0 = &_x0;

@ "get-tabs" : float[]/bReturnList[float.0] GetTabs(int?=NULL,float?=NULL,bool?=NULL); : : /checkNull/
@ "set-tabs" : void SetTabs(float[]/bList/ubList/cList,-int,float=wxTAB_WIDTH,bool=TRUE); : : /glueListSet[float.0.0.1.METHODNAME("text%","set-tabs")]//

@ "add-editor-functions" : void AddEditorFunctions(wxKeymap!);

@ v "on-insert" : bool OnInsert(long,long);
@ v "after-insert" : void AfterInsert(long,long);
@ v "on-delete" : bool OnDelete(long,long);
@ v "after-delete" : void AfterDelete(long,long);
@ v "on-change-style" : bool OnChangeStyle(long,long);
@ v "after-change-style" : void AfterChangeStyle(long,long);
@ v "after-set-position" : void AfterSetPosition();
@ v "on-set-size-constraint" : bool OnSetSizeConstraint();
@ v "after-set-size-constraint" : void AfterSetSizeConstraint();

@ v "get-region-data" : wxBufferData^ GetRegionData(long,long);
@ v "set-region-data" : void SetRegionData(long, long, wxBufferData^);

@ "find-wordbreak" : void FindWordbreak(long?,long?,SYM[breakType]);

@ "set-wordbreak-map" : void SetWordbreakMap(wxMediaWordbreakMap^);
@ "get-wordbreak-map" : wxMediaWordbreakMap^ GetWordbreakMap();

@ "hide-caret" : void HideCaret(bool);
@ "caret-hidden?" : bool CaretHidden();

@ v "on-new-text-snip" : wxTextSnip! OnNewTextSnip();
@ v "on-new-tab-snip" : wxTabSnip! OnNewTabSnip();

@ "set-autowrap-bitmap" : wxBitmap^ SetAutowrapBitmap(wxBitmap^);

static void WordbreakCallbackToScheme(wxMediaEdit *,long*,long*,int,Scheme_Object *);

@MACRO ubCallback = (wxWordbreakFunc)WordbreakCallbackToScheme
@MACRO ubData = p[0]
@MACRO spCallback = (wxMediaEdit-object (box num) (box num) num -> void)

@MACRO ubCallback2 = (wxClickbackFunc)ClickbackToScheme
@MACRO ubData2 = p[2]
@MACRO cCallback2 = SCHEME_PROCP({x})
@MACRO spCallback2 = (wxMediaEdit-object num num -> void)

@ "set-wordbreak-func" : void SetWordbreakFunc(wxWordbreakFunc//ubCallback/cCallback//spCallback,-unknown#void*//ubData);

@ "set-clickback" : void SetClickback(long,long,wxClickbackFunc//ubCallback2/cCallback2//spCallback2,-unknown#void*//ubData2,wxStyleDelta^=NULL,bool=FALSE);
@ "remove-clickback" : void RemoveClickback(long,long);

static void WordbreakCallbackToScheme(wxMediaEdit *media,
				      long *start, long *end,
				      int reason,
				      Scheme_Object *f)
{
  Scheme_Object *p[4], *s, *e;
  jmp_buf savebuf;

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf)) {
    p[0] = objscheme_bundle_wxMediaEdit(media);
    if (start)
      s = scheme_box(objscheme_bundle_integer(*start));
    else
      s = XC_SCHEME_NULL;
    if (end)
      e = scheme_box(objscheme_bundle_integer(*end));
    else
      e = XC_SCHEME_NULL;
    p[1] = s;
    p[2] = e;
    p[3] = scheme_make_integer(reason);

    scheme_apply_multi(f, 4, p);
    if (start)
      *start = objscheme_unbundle_integer(scheme_unbox(s), "Scheme wordbreak callback");
    if (end)
      *end = objscheme_unbundle_integer(scheme_unbox(e), "Scheme wordbreak callback");
  }

  COPY_JMPBUF(scheme_error_buf, savebuf);
}

static void ClickbackToScheme(wxMediaEdit *media,
			      long start, long end,
			      Scheme_Object *f)
{
  Scheme_Object *p[3];
  jmp_buf savebuf;

  p[0] = objscheme_bundle_wxMediaEdit(media);
  p[1] = objscheme_bundle_integer(start);
  p[2] = objscheme_bundle_integer(end);

  COPY_JMPBUF(savebuf, scheme_error_buf);

  if (!scheme_setjmp(scheme_error_buf))
    scheme_apply_multi(f, 3, p);

  COPY_JMPBUF(scheme_error_buf, savebuf);
}

@END


