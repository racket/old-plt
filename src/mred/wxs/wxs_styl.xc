
@INCLUDE prefix.xci

#include "wx_style.h"
#include "wx_mtype.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxMultColour "wx:mult-colour" : "wx:object"

@IVAR "r" : float r
@IVAR "g" : float g
@IVAR "b" : float b

@ "get" : void Get(float*,float*,float*);
@ "set" : void Set(float,float,float);

@END

@CLASSBASE wxAddColour "wx:add-colour" : "wx:object"

@IVAR "r" : short r
@IVAR "g" : short g
@IVAR "b" : short b

@ "get" : void Get(short*,short*,short*);
@ "set" : void Set(short,short,short);

@END

@BEGINSYMBOLS family > ONE
@SYM "base" : wxBASE
@SYM "default" : wxDEFAULT
@SYM "decorative" : wxDECORATIVE
@SYM "roman" : wxROMAN
@SYM "script" : wxSCRIPT
@SYM "swiss" : wxSWISS
@SYM "modern" : wxMODERN
@SYM "teletype" : wxTELETYPE
@SYM "system" : wxSYSTEM
@ENDSYMBOLS

@BEGINSYMBOLS weight > ONE
@SYM "base" : wxBASE
@SYM "normal" : wxNORMAL
@SYM "light" : wxLIGHT
@SYM "bold" : wxBOLD
@ENDSYMBOLS

@BEGINSYMBOLS style > ONE
@SYM "base" : wxBASE
@SYM "normal" : wxNORMAL
@SYM "italic" : wxITALIC
@SYM "slant" : wxSLANT
@ENDSYMBOLS

@BEGINSYMBOLS align > ONE
@SYM "base" : wxBASE
@SYM "align-top" :  wxALIGN_TOP
@SYM "align-bottom" : wxALIGN_BOTTOM
@SYM "align-center" : wxALIGN_CENTER
@ENDSYMBOLS

@BEGINSYMBOLS changeNoArg > ONE
@SYM "change-nothing" : wxCHANGE_NOTHING
@SYM "change-normal" : wxCHANGE_NORMAL
@SYM "change-bold" : wxCHANGE_BOLD
@SYM "change-italic" : wxCHANGE_ITALIC
@SYM "change-toggle-underline" : wxCHANGE_TOGGLE_UNDERLINE
@SYM "change-normal-colour" : wxCHANGE_NORMAL_COLOUR
@ENDSYMBOLS
@BEGINSYMBOLS changeFam > ONE
@SYM "change-family" : wxCHANGE_FAMILY
@ENDSYMBOLS
@BEGINSYMBOLS changeStyle > ONE
@SYM "change-style" : wxCHANGE_STYLE
@SYM "change-toggle-style" : wxCHANGE_TOGGLE_STYLE
@ENDSYMBOLS
@BEGINSYMBOLS changeWeight > ONE
@SYM "change-weight" : wxCHANGE_WEIGHT
@SYM "change-toggle-weight" : wxCHANGE_TOGGLE_WEIGHT
@ENDSYMBOLS
@BEGINSYMBOLS changeUnderline > ONE
@SYM "change-underline" : wxCHANGE_UNDERLINE
@ENDSYMBOLS
@BEGINSYMBOLS changeSize > ONE
@SYM "change-size" : wxCHANGE_SIZE
@SYM "change-bigger" : wxCHANGE_BIGGER
@SYM "change-smaller" : wxCHANGE_SMALLER
@ENDSYMBOLS
@BEGINSYMBOLS changeAlign > ONE
@SYM "change-alignment" : wxCHANGE_ALIGNMENT
@ENDSYMBOLS

@CLASSBASE wxStyleDelta "wx:style-delta" : "wx:object"

@IVAR "family" : SYM[family] family
@IVAR "face" : nstring face
@IVAR "size-mult" : float sizeMult
@IVAR "size-add" : int sizeAdd
@IVAR "weight-on" : SYM[weight] weightOn
@IVAR "weight-off" : SYM[weight] weightOff
@IVAR "style-on" : SYM[style] styleOn
@IVAR "style-off" : SYM[style] styleOff
@IVAR "underlined-on" : bool underlinedOn
@IVAR "underlined-off" : bool underlinedOff
@IVAR "transparent-text-backing-on" : bool transparentTextBackingOn
@IVAR "transparent-text-backing-off" : bool transparentTextBackingOff
@IVAR r "foreground-mult" : wxMultColour% foregroundMult
@IVAR r "background-mult" : wxMultColour% backgroundMult
@IVAR r "foreground-add" : wxAddColour% foregroundAdd
@IVAR r "background-add" : wxAddColour% backgroundAdd
@IVAR "alignment-on" : SYM[align] alignmentOn
@IVAR "alignment-off" : SYM[align] alignmentOff
  
@CREATOR (SYM[changeNoArg]=wxCHANGE_NOTHING,-int=0); <> no change argument
@CREATORX (SYM[changeFam],SYM[family]); <> family
@CREATORX (SYM[changeStyle],SYM[style]); <> style
@CREATORX (SYM[changeWeight],SYM[weight]); <> weight
@CREATORX (SYM[changeUnderline],bool); <> underline
@CREATORX (SYM[changeSize],int); <> size

@CLASSID wxTYPE_STYLE_DELTA

@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeNoArg]=wxCHANGE_NOTHING,-int=0); <> no change argument
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeFam],SYM[family]); <> family
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeStyle],SYM[style]); <> style
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeWeight],SYM[weight]); <> weight
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeUnderline],bool); <> underline
@ "set-delta" : wxStyleDelta! SetDelta(SYM[changeSize],int); <> size

@ "set-delta-face" : wxStyleDelta! SetDeltaFace(string);
@ "set-delta-background" : wxStyleDelta! SetDeltaBackground(string); <> color name
@ "set-delta-background" : wxStyleDelta! SetDeltaBackground(wxColour%); <> wx:colour%
@ "set-delta-foreground" : wxStyleDelta! SetDeltaForeground(string); <> color name
@ "set-delta-foreground" : wxStyleDelta! SetDeltaForeground(wxColour%); <> wx:colour%

@ "equal?" : bool Equal(wxStyleDelta%);
@ "collapse" : bool Collapse(wxStyleDelta%);
@ "copy" : void Copy(wxStyleDelta!);

@END


@CLASSBASE wxStyle "wx:style" : "wx:object"

@CLASSID wxTYPE_STYLE

@ "get-name" : string GetName();
@ "get-family" : int GetFamily();
@ "get-face" : nstring GetFace();
@ "get-size" : int GetSize();
@ "get-weight" : int GetWeight();
@ "get-style" : int GetStyle();
@ "get-underlined" : bool GetUnderlined();
@ "get-font" : wxFont! GetFont();
@ "get-foreground" : wxColour% GetForeground();
@ "get-colour" : wxColour% GetBackground();
@ "get-alignment" : int GetAlignment();
@ "get-transparent-text-backing" : bool GetTransparentTextBacking();

@ "get-text-height" : float GetTextHeight(wxDC!);
@ "get-text-descent" : float GetTextDescent(wxDC!);
@ "get-text-space" : float GetTextSpace(wxDC!);
@ "get-text-width" : float GetTextWidth(wxDC!);

@ "get-base-style" : wxStyle! GetBaseStyle();
@ "set-base-style" : void SetBaseStyle(wxStyle!);

@ "get-delta" : void GetDelta(wxStyleDelta%);
@ "set-delta" : void SetDelta(wxStyleDelta%);

@ "is-join?" : bool IsJoin();

@ "get-shift-style" :  wxStyle! GetShiftStyle();
@ "set-shift-style" : void SetShiftStyle(wxStyle!);

@ "switch-to" : void SwitchTo(wxDC!, wxStyle!); : : /CHECKOK[0."wx:style%::switch-to"]

@END


@CLASSBASE wxStyleList "wx:style-list" : "wx:object"

@CREATOR ();

@CLASSID wxTYPE_STYLE_LIST

@ "clear" : void Clear();
@ "copy" : void Copy(wxStyleList!);

@ "basic-style" : wxStyle! BasicStyle();

@ "number" : int Number();

@ "find-or-create-style" : wxStyle! FindOrCreateStyle(wxStyle^,wxStyleDelta!);
@ "find-or-create-join-style" : wxStyle! FindOrCreateJoinStyle(wxStyle^,wxStyle!);
@ "find-named-style" : wxStyle! FindNamedStyle(string);
@ "new-named-style" : wxStyle! NewNamedStyle(string,wxStyle^);
@ "replace-named-style" : wxStyle! ReplaceNamedStyle(string,wxStyle^);

@ "convert" : wxStyle! Convert(wxStyle!);

@ "index-to-style" : wxStyle^ IndexToStyle(int);
@ "style-to-index" : int StyleToIndex(wxStyle!);

@ "adjust-usage" : void AdjustUsage(bool);
@ "is-used? " : bool IsUsed();

@CONSTANT "wx:the-style-list" : wxStyleList! wxTheStyleList

@END

