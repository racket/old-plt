
@INCLUDE prefix.xci

#include "wx_stdev.h"

@INCLUDE wxs.xci

@HEADER

@BEGINSYMBOLS eventClass > ONE
@SYM "type-key-event" : wxTYPE_KEY_EVENT
@SYM "type-command-event" : wxTYPE_COMMAND_EVENT
@SYM "type-mouse-event" : wxTYPE_MOUSE_EVENT
@ENDSYMBOLS

@CLASSBASE wxEvent "wx:event":"wx:object"

@IVAR "event-class" : SYM[eventClass] eventClass
@IVAR "event-object" : wxObject! eventObject

@END

@BEGINSYMBOLS commandType > ONE
@SYM "event-type-button-command" : wxEVENT_TYPE_BUTTON_COMMAND
@SYM "event-type-checkbox-command" : wxEVENT_TYPE_CHECKBOX_COMMAND
@SYM "event-type-choice-command" : wxEVENT_TYPE_CHOICE_COMMAND
@SYM "event-type-listbox-command" : wxEVENT_TYPE_LISTBOX_COMMAND
@SYM "event-type-text-command" : wxEVENT_TYPE_TEXT_COMMAND
@SYM "event-type-multitext-command" : wxEVENT_TYPE_MULTITEXT_COMMAND
@SYM "event-type-menu-command" : wxEVENT_TYPE_MENU_COMMAND
@SYM "event-type-slider-command" : wxEVENT_TYPE_SLIDER_COMMAND
@SYM "event-type-radiobox-command" : wxEVENT_TYPE_RADIOBOX_COMMAND
@SYM "event-type-text-enter-command" : wxEVENT_TYPE_TEXT_ENTER_COMMAND
@SYM "event-type-set-focus" : wxEVENT_TYPE_SET_FOCUS
@SYM "event-type-kill-focus" : wxEVENT_TYPE_KILL_FOCUS
@SYM "event-type-scrollbar-command" : wxEVENT_TYPE_SCROLLBAR_COMMAND  
@SYM "event-type-virt-listbox-command" : wxEVENT_TYPE_VIRT_LISTBOX_COMMAND
@SYM "event-type-scroll-top" : wxEVENT_TYPE_SCROLL_TOP
@SYM "event-type-scroll-bottom" : wxEVENT_TYPE_SCROLL_BOTTOM
@SYM "event-type-scroll-lineup" : wxEVENT_TYPE_SCROLL_LINEUP
@SYM "event-type-scroll-linedown" : wxEVENT_TYPE_SCROLL_LINEDOWN
@SYM "event-type-scroll-pageup" : wxEVENT_TYPE_SCROLL_PAGEUP
@SYM "event-type-scroll-pagedown" : wxEVENT_TYPE_SCROLL_PAGEDOWN
@SYM "event-type-scroll-thumbtrack" : wxEVENT_TYPE_SCROLL_THUMBTRACK
@ENDSYMBOLS

@CLASSBASE wxCommandEvent "wx:command-event":"wx:event"

@CREATOR (int)

@ "get-selection" : int GetSelection();
@ "get-string" : string GetString();
@ "checked?" : bool Checked();
@ "is-selection?" : bool IsSelection();

@IVAR "event-type" : SYM[commandType] eventType
@IVAR "extra-long" : long extraLong
@IVAR "command-int" : int commandInt
@IVAR "command-string" : string commandString

@END

@BEGINSYMBOLS keyCode > ONE/CHAR
@SYM "k-back" : WXK_BACK
@SYM "k-tab" : WXK_TAB
@SYM "k-return" : WXK_RETURN
@SYM "k-escape" : WXK_ESCAPE
@SYM "k-space" : WXK_SPACE
@SYM "k-delete" : WXK_DELETE
@SYM "k-start" : WXK_START
@SYM "k-lbutton" : WXK_LBUTTON
@SYM "k-rbutton" : WXK_RBUTTON
@SYM "k-cancel" : WXK_CANCEL
@SYM "k-mbutton" : WXK_MBUTTON
@SYM "k-clear" : WXK_CLEAR
@SYM "k-shift" : WXK_SHIFT
@SYM "k-control" : WXK_CONTROL
@SYM "k-menu" : WXK_MENU
@SYM "k-pause" : WXK_PAUSE
@SYM "k-capital" : WXK_CAPITAL
@SYM "k-prior" : WXK_PRIOR
@SYM "k-next" : WXK_NEXT
@SYM "k-end" : WXK_END
@SYM "k-home" : WXK_HOME
@SYM "k-left" : WXK_LEFT
@SYM "k-up" : WXK_UP
@SYM "k-right" : WXK_RIGHT
@SYM "k-down" : WXK_DOWN
@SYM "k-select" : WXK_SELECT
@SYM "k-print" : WXK_PRINT
@SYM "k-execute" : WXK_EXECUTE
@SYM "k-snapshot" : WXK_SNAPSHOT
@SYM "k-insert" : WXK_INSERT
@SYM "k-help" : WXK_HELP
@SYM "k-numpad0" : WXK_NUMPAD0
@SYM "k-numpad1" : WXK_NUMPAD1
@SYM "k-numpad2" : WXK_NUMPAD2
@SYM "k-numpad3" : WXK_NUMPAD3
@SYM "k-numpad4" : WXK_NUMPAD4
@SYM "k-numpad5" : WXK_NUMPAD5
@SYM "k-numpad6" : WXK_NUMPAD6
@SYM "k-numpad7" : WXK_NUMPAD7
@SYM "k-numpad8" : WXK_NUMPAD8
@SYM "k-numpad9" : WXK_NUMPAD9
@SYM "k-multiply" : WXK_MULTIPLY
@SYM "k-add" : WXK_ADD
@SYM "k-separator" : WXK_SEPARATOR
@SYM "k-subtract" : WXK_SUBTRACT
@SYM "k-decimal" : WXK_DECIMAL
@SYM "k-divide" : WXK_DIVIDE
@SYM "k-f1" : WXK_F1
@SYM "k-f2" : WXK_F2
@SYM "k-f3" : WXK_F3
@SYM "k-f4" : WXK_F4
@SYM "k-f5" : WXK_F5
@SYM "k-f6" : WXK_F6
@SYM "k-f7" : WXK_F7
@SYM "k-f8" : WXK_F8
@SYM "k-f9" : WXK_F9
@SYM "k-f10" : WXK_F10
@SYM "k-f11" : WXK_F11
@SYM "k-f12" : WXK_F12
@SYM "k-f13" : WXK_F13
@SYM "k-f14" : WXK_F14
@SYM "k-f15" : WXK_F15
@SYM "k-f16" : WXK_F16
@SYM "k-f17" : WXK_F17
@SYM "k-f18" : WXK_F18
@SYM "k-f19" : WXK_F19
@SYM "k-f20" : WXK_F20
@SYM "k-f21" : WXK_F21
@SYM "k-f22" : WXK_F22
@SYM "k-f23" : WXK_F23
@SYM "k-f24" : WXK_F24
@SYM "k-numlock" : WXK_NUMLOCK
@SYM "k-scroll" : WXK_SCROLL
@ENDSYMBOLS

@CLASSBASE wxKeyEvent "wx:key-event":"wx:event"

@CREATOR (-int=wxEVENT_TYPE_CHAR);

@ "key-code" : long KeyCode();

// @IVAR "key-code" : SYM[keyCode] keyCode
@IVAR "key-code" : int keyCode
@IVAR "shift-down" : bool shiftDown
@IVAR "control-down" : bool controlDown
@IVAR "meta-down" : bool metaDown
@IVAR "alt-down" : bool altDown
@IVAR "time-stamp" : long timeStamp

@IVAR "x" : float x
@IVAR "y" : float y

@END

@BEGINSYMBOLS mouseEventType > ONE
@SYM "event-type-left-down" : wxEVENT_TYPE_LEFT_DOWN
@SYM "event-type-left-up" : wxEVENT_TYPE_LEFT_UP
@SYM "event-type-middle-down" : wxEVENT_TYPE_MIDDLE_DOWN
@SYM "event-type-middle-up" : wxEVENT_TYPE_MIDDLE_UP
@SYM "event-type-right-down" : wxEVENT_TYPE_RIGHT_DOWN
@SYM "event-type-right-up" : wxEVENT_TYPE_RIGHT_UP
@SYM "event-type-motion" : wxEVENT_TYPE_MOTION
@SYM "event-type-enter-window" : wxEVENT_TYPE_ENTER_WINDOW
@SYM "event-type-leave-window" : wxEVENT_TYPE_LEAVE_WINDOW
@SYM "event-type-left-dclick" : wxEVENT_TYPE_LEFT_DCLICK
@SYM "event-type-middle-dclick" : wxEVENT_TYPE_MIDDLE_DCLICK
@SYM "event-type-right-dclick" : wxEVENT_TYPE_RIGHT_DCLICK
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
