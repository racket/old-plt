/*								-*- C++ -*-
 * $Id: Event.h,v 1.2 1996/01/10 23:47:00 markus Exp $
 *
 * Purpose: standard wxWindows event classes
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef Event_h
#define Event_h

#ifdef __GNUG__
#pragma interface
#endif

class wxEvent : public wxObject {
DECLARE_DYNAMIC_CLASS(wxEvent)
public:
    wxEvent(WXTYPE type=0);

    inline void*    GetEventHandle(void)  { return eventHandle; }
    inline WXTYPE   GetEventType(void)    { return eventType; };
    inline WXTYPE   GetEventClass(void)   { return eventClass; };
    inline WXTYPE   GetObjectType(void)   { return objectType; };
    inline wxObject *GetEventObject(void) { return eventObject; };
    inline long     GetTimestamp(void)    { return timeStamp; }
    inline void     SetTimestamp(long ts = 0) { timeStamp = ts; }

    void*     eventHandle;
    WXTYPE    eventType;
    WXTYPE    eventClass;
    WXTYPE    objectType;
    wxObject* eventObject;
    long      timeStamp;
};

// command event class used by items
class wxCommandEvent : public wxEvent {
DECLARE_DYNAMIC_CLASS(wxCommandEvent)
public:
    wxCommandEvent(WXTYPE commandType = 0);

    inline char *GetClientData() { return clientData; }
    inline int  GetSelection() { return commandInt; }
    inline char *GetString() { return commandString; }
    inline Bool Checked() { return (Bool)commandInt; }
    inline Bool IsSelection() { return (Bool)extraLong; }

    char    *commandString; // String event argument
    int     commandInt;     // Integer event argument
    long    extraLong;      // Additional information (e.g. select/deselect)
    char    *labelString;   // The label of the item
    char    *clientData;    // Arbitrary client data
};

// mouse event class
class wxMouseEvent : public wxEvent {
DECLARE_DYNAMIC_CLASS(wxMouseEvent)
public:
    wxMouseEvent(WXTYPE mouseType = 0);

    Bool ButtonDown(int but = -1);
    Bool ButtonDClick(int but = -1);
    Bool ButtonUp(int but = -1);
    Bool Button(int but);
    Bool ButtonIsDown(int but);

    Bool IsButton(void)
	{ return Button(-1); }
    Bool ControlDown(void)
	{ return controlDown; }
    Bool MetaDown(void)
	{ return metaDown; }
    Bool AltDown(void)
	{ return altDown; }
    Bool ShiftDown(void)
	{ return shiftDown; }
    Bool LeftDown(void)
        { return (eventType==wxEVENT_TYPE_LEFT_DOWN); }
    Bool MiddleDown(void)
        { return (eventType==wxEVENT_TYPE_MIDDLE_DOWN); }
    Bool RightDown(void)
        { return (eventType==wxEVENT_TYPE_RIGHT_DOWN); }
    Bool LeftUp(void)
        { return (eventType==wxEVENT_TYPE_LEFT_UP); }
    Bool MiddleUp(void)
        { return (eventType==wxEVENT_TYPE_MIDDLE_UP); }
    Bool RightUp(void)
        { return (eventType==wxEVENT_TYPE_RIGHT_UP); }
    Bool LeftDClick(void)
        { return (eventType==wxEVENT_TYPE_LEFT_DCLICK); }
    Bool MiddleDClick(void)
        { return (eventType==wxEVENT_TYPE_MIDDLE_DCLICK); }
    Bool RightDClick(void)
        { return (eventType==wxEVENT_TYPE_RIGHT_DCLICK); }
    Bool LeftIsDown(void)
	{ return leftDown; }
    Bool MiddleIsDown(void)
	{ return middleDown; }
    Bool RightIsDown(void)
	{ return rightDown; }
    Bool Dragging(void)
	{ return (Moving()&&(LeftIsDown()||MiddleIsDown()||RightIsDown())); }
    Bool Moving(void)
        { return (eventType==wxEVENT_TYPE_MOTION); }
    Bool Entering(void)
        { return (eventType==wxEVENT_TYPE_ENTER_WINDOW); }
    Bool Leaving(void)
        { return (eventType==wxEVENT_TYPE_LEAVE_WINDOW); }
    void Position(float *xpos, float *ypos)
        { *xpos = x; *ypos = y; }

    float x;
    float y;
    Bool leftDown;
    Bool middleDown;
    Bool rightDown;
    Bool controlDown;
    Bool shiftDown;
    Bool altDown;
    Bool metaDown;
};

// keyboard input event class
class wxKeyEvent : public wxEvent {
DECLARE_DYNAMIC_CLASS(wxKeyEvent)
public:
    wxKeyEvent(WXTYPE keyType = 0);

    Bool ControlDown(void)
        { return controlDown; }
    Bool AltDown(void)
        { return altDown; }
    Bool MetaDown(void)
        { return metaDown; }
    Bool ShiftDown(void)
        { return shiftDown; }
    long KeyCode(void)
        { return keyCode; }
    void Position(float *xpos, float *ypos)
        { *xpos = x; *ypos = y; }

    float x;
    float y;
    long keyCode;
    Bool controlDown;
    Bool shiftDown;
    Bool altDown;
    Bool metaDown;
};

#endif // Event_h
