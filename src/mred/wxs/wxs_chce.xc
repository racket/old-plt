
@INCLUDE prefix.xci

#include "wx_choic.h"

@INCLUDE wxs.xci

@HEADER

@CLASSBASE wxChoice "wx:choice":"wx:item"

@SET CALLBACK_CLASS = wxChoice
@SET CALLBACK_CLASS_USER = "wx:choice%::initialization"
@INCLUDE cb_start.xci

@SET TYPE = string
@SET NOTEST = 1
@INCLUDE list.xci

#define RANGECLASS wxChoice
@INCLUDE range.xci

@CREATOR (wxPanel!,wxFunction/bCallback/ubCallback/cCallback//spCallback,nstring,int=-1,int=-1,int=-1,int=-1,-int=0,string[]=NULL/bList/ubList/cList,long=0,string="checkBox"); : : ubCallbackSetup/glueListSet[string.7.8.7."wx:choice%::initialization"]|NOZERO[5]|NOZERO[6]/glueCleanup[8]/ubCallbackCreatorFinish

@INCLUDE wxs_item.xci

@ "append" : void Append(string);
@ "clear" : void Clear();
@ "number" : int Number()
@ "find-string" : int FindString(string);
@ "get-selection" : int GetSelection();
@ "get-string-selection" : string GetStringSelection();
@ "set-selection" : void SetSelection(int); : : /RANGE[0]
@ "set-string-selection" : void SetStringSelection(string);
@ "get-string" : string GetString(int); : : /RANGERET[0.scheme_null]

@END

@INCLUDE cb_end.xci
