
@INCLUDE prefix.xci

#include "wx_stdev.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS eventClass > ONE
@SYM "key" : wxTYPE_KEY_EVENT
@SYM "command" : wxTYPE_COMMAND_EVENT
@SYM "mouse" : wxTYPE_MOUSE_EVENT
@ENDSYMBOLS

@CLASSBASE wxEvent "wx:event":"wx:object"

@IVAR "event-class" : SYM[eventClass] eventClass
@IVAR "event-object" : wxObject^ eventObject

@END

@BEGINSYMBOLS commandType > ONE
@SYM "button" : wxEVENT_TYPE_BUTTON_COMMAND
@SYM "check-box" : wxEVENT_TYPE_CHECKBOX_COMMAND
@SYM "choice" : wxEVENT_TYPE_CHOICE_COMMAND
@SYM "list-box" : wxEVENT_TYPE_LISTBOX_COMMAND
@SYM "text" : wxEVENT_TYPE_TEXT_COMMAND
@SYM "slider" : wxEVENT_TYPE_SLIDER_COMMAND
@SYM "radio-box" : wxEVENT_TYPE_RADIOBOX_COMMAND
@SYM "text-enter" : wxEVENT_TYPE_TEXT_ENTER_COMMAND
@SYM "set-focus" : wxEVENT_TYPE_SET_FOCUS
@SYM "kill-focus" : wxEVENT_TYPE_KILL_FOCUS
@SYM "scroll-top" : wxEVENT_TYPE_SCROLL_TOP
@SYM "scroll-bottom" : wxEVENT_TYPE_SCROLL_BOTTOM
@SYM "scroll-line-up" : wxEVENT_TYPE_SCROLL_LINEUP
@SYM "scroll-line-down" : wxEVENT_TYPE_SCROLL_LINEDOWN
@SYM "scroll-page-up" : wxEVENT_TYPE_SCROLL_PAGEUP
@SYM "scroll-page-down" : wxEVENT_TYPE_SCROLL_PAGEDOWN
@SYM "scroll-thumb" : wxEVENT_TYPE_SCROLL_THUMBTRACK
@ENDSYMBOLS

Bool CommandEventIsDoubleClick(wxCommandEvent *ce)
{
   return (ce->extraLong == 2);
}

@CLASSBASE wxCommandEvent "wx:command-event":"wx:event"

@CREATOR (int)

@ "checked?" : bool Checked();
@ "is-selection?" : bool IsSelection();
@ m "is-double-click?" : bool CommandEventIsDoubleClick();

@IVAR "event-type" : SYM[commandType] eventType
@IVAR "selection-type" : long extraLong
@IVAR "selection" : int commandInt
@IVAR "string" : nstring commandString

// These will be removed in the next version
#define __commandInt commandInt
#define __commandString commandString
#define __extraLong extraLong
@IVAR "command-int" : int __commandInt
@IVAR "command-string" : nstring __commandString
@IVAR "extra-long" : long __extraLong

@END

@BEGINSYMBOLS keyCode > ONE/CHAR
@SYM "escape" : WXK_ESCAPE
@SYM "start" : WXK_START
@SYM "lbutton" : WXK_LBUTTON
@SYM "rbutton" : WXK_RBUTTON
@SYM "cancel" : WXK_CANCEL
@SYM "mbutton" : WXK_MBUTTON
@SYM "clear" : WXK_CLEAR
@SYM "shift" : WXK_SHIFT
@SYM "control" : WXK_CONTROL
@SYM "menu" : WXK_MENU
@SYM "pause" : WXK_PAUSE
@SYM "capital" : WXK_CAPITAL
@SYM "prior" : WXK_PRIOR
@SYM "next" : WXK_NEXT
@SYM "end" : WXK_END
@SYM "home" : WXK_HOME
@SYM "left" : WXK_LEFT
@SYM "up" : WXK_UP
@SYM "right" : WXK_RIGHT
@SYM "down" : WXK_DOWN
@SYM "select" : WXK_SELECT
@SYM "print" : WXK_PRINT
@SYM "execute" : WXK_EXECUTE
@SYM "snapshot" : WXK_SNAPSHOT
@SYM "insert" : WXK_INSERT
@SYM "help" : WXK_HELP
@SYM "numpad0" : WXK_NUMPAD0
@SYM "numpad1" : WXK_NUMPAD1
@SYM "numpad2" : WXK_NUMPAD2
@SYM "numpad3" : WXK_NUMPAD3
@SYM "numpad4" : WXK_NUMPAD4
@SYM "numpad5" : WXK_NUMPAD5
@SYM "numpad6" : WXK_NUMPAD6
@SYM "numpad7" : WXK_NUMPAD7
@SYM "numpad8" : WXK_NUMPAD8
@SYM "numpad9" : WXK_NUMPAD9
@SYM "multiply" : WXK_MULTIPLY
@SYM "add" : WXK_ADD
@SYM "separator" : WXK_SEPARATOR
@SYM "subtract" : WXK_SUBTRACT
@SYM "decimal" : WXK_DECIMAL
@SYM "divide" : WXK_DIVIDE
@SYM "f1" : WXK_F1
@SYM "f2" : WXK_F2
@SYM "f3" : WXK_F3
@SYM "f4" : WXK_F4
@SYM "f5" : WXK_F5
@SYM "f6" : WXK_F6
@SYM "f7" : WXK_F7
@SYM "f8" : WXK_F8
@SYM "f9" : WXK_F9
@SYM "f10" : WXK_F10
@SYM "f11" : WXK_F11
@SYM "f12" : WXK_F12
@SYM "f13" : WXK_F13
@SYM "f14" : WXK_F14
@SYM "f15" : WXK_F15
@SYM "f16" : WXK_F16
@SYM "f17" : WXK_F17
@SYM "f18" : WXK_F18
@SYM "f19" : WXK_F19
@SYM "f20" : WXK_F20
@SYM "f21" : WXK_F21
@SYM "f22" : WXK_F22
@SYM "f23" : WXK_F23
@SYM "f24" : WXK_F24
@SYM "numlock" : WXK_NUMLOCK
@SYM "scroll" : WXK_SCROLL
@ENDSYMBOLS

@CLASSBASE wxKeyEvent "wx:key-event":"wx:event"

@MACRO SETX0 = x0=wxEVENT_TYPE_CHAR;

@CREATOR (-int=wxEVENT_TYPE_CHAR); : : /SETX0

// @ "key-code" : SYM[keyCode] KeyCode();

@IVAR "key-code" : SYM[keyCode] keyCode
@IVAR "shift-down" : bool shiftDown
@IVAR "control-down" : bool controlDown
@IVAR "meta-down" : bool metaDown
@IVAR "alt-down" : bool altDown
@IVAR "time-stamp" : long timeStamp

@IVAR "x" : float x
@IVAR "y" : float y

@END

@BEGINSYMBOLS mouseEventType > ONE
@SYM "left-down" : wxEVENT_TYPE_LEFT_DOWN
@SYM "left-up" : wxEVENT_TYPE_LEFT_UP
@SYM "middle-down" : wxEVENT_TYPE_MIDDLE_DOWN
@SYM "middle-up" : wxEVENT_TYPE_MIDDLE_UP
@SYM "right-down" : wxEVENT_TYPE_RIGHT_DOWN
@SYM "right-up" : wxEVENT_TYPE_RIGHT_UP
@SYM "motion" : wxEVENT_TYPE_MOTION
@SYM "enter-window" : wxEVENT_TYPE_ENTER_WINDOW
@SYM "leave-window" : wxEVENT_TYPE_LEAVE_WINDOW
@SYM "left-dclick" : wxEVENT_TYPE_LEFT_DCLICK
@SYM "middle-dclick" : wxEVENT_TYPE_MIDDLE_DCLICK
@SYM "right-dclick" : wxEVENT_TYPE_RIGHT_DCLICK
@ENDSYMBOLS

@CLASSBASE wxMouseEvent "wx:mouse-event":"wx:event"

@CREATOR (int);

@ "button?" : bool Button(int);
@ "button-d-click?" : bool ButtonDClick(int=-1);
@ "button-down?" : bool ButtonDown(int=-1);
@ "button-up?" : bool ButtonUp(int=-1);
@ "dragging?" : bool Dragging();
@ "entering?" : bool Entering();
@ "leaving?" : bool Leaving();
@ "is-button?" : bool IsButton();
@ "moving?" : bool Moving();

@IVAR "event-type" : SYM[mouseEventType] eventType
@IVAR "left-down" : bool leftDown
@IVAR "middle-down" : bool middleDown
@IVAR "right-down" : bool rightDown
@IVAR "shift-down" : bool shiftDown
@IVAR "control-down" : bool controlDown
@IVAR "meta-down" : bool metaDown
@IVAR "alt-down" : bool altDown
@IVAR "x" : float x
@IVAR "y" : float y
@IVAR "time-stamp" : long timeStamp

@END
