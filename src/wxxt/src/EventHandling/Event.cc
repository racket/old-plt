/*								-*- C++ -*-
 * $Id: Event.cc,v 1.2 1996/01/10 23:47:00 markus Exp $
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

#ifdef __GNUG__
#pragma implementation "Event.h"
#endif

#define  Uses_XLib
#define  Uses_wxEvent
#include "wx.h"

//-----------------------------------------------------------------------------
// wxEvent
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxEvent, wxObject)

wxEvent::wxEvent(WXTYPE commandType)
{
    __type = eventClass = wxTYPE_EVENT;

    eventHandle = NULL;
    eventType   = commandType;
    objectType  = 0;
    eventObject = NULL;
    timeStamp   = 0;
}

//-----------------------------------------------------------------------------
// wxCommandEvent
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxCommandEvent, wxEvent)

wxCommandEvent::wxCommandEvent(WXTYPE commandType) : wxEvent (commandType)
{
    __type = eventClass = wxTYPE_COMMAND_EVENT;
    commandInt    = 0;
    commandString = NULL;
    extraLong     = 0;
    labelString   = NULL;
    clientData    = 0;
}

//-----------------------------------------------------------------------------
// wxMouseEvent
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxMouseEvent, wxEvent)

wxMouseEvent::wxMouseEvent(WXTYPE commandType) : wxEvent(commandType)
{
    __type = eventClass = wxTYPE_MOUSE_EVENT;
    metaDown = altDown = controlDown = shiftDown 
	= leftDown = middleDown = rightDown = FALSE;
    x = y = 0.0;
}

Bool wxMouseEvent::ButtonDClick(int but)
{
    switch (but) {
    case -1:
	return (LeftDClick() || MiddleDClick() || RightDClick());
    case 1:
	return LeftDClick();
    case 2:
	return MiddleDClick();
    case 3:
	return RightDClick();
  }
  return FALSE;
}

Bool wxMouseEvent::ButtonDown(int but)
{
    switch (but) {
    case -1:
	return (LeftDown() || MiddleDown() || RightDown());
    case 1:
	return LeftDown();
    case 2:
	return MiddleDown();
    case 3:
	return RightDown();
    }
    return FALSE;
}

Bool wxMouseEvent::ButtonUp(int but)
{
    switch (but) {
    case -1:
	return (LeftUp() || MiddleUp() || RightUp());
    case 1:
	return LeftUp();
    case 2:
      return MiddleUp();
    case 3:
	return RightUp();
    }
    return FALSE;
}

Bool wxMouseEvent::Button(int but)
{
    switch (but) {
    case -1:
	return (ButtonUp(-1) || ButtonDown(-1) || ButtonDClick(-1)) ;
    case 1:
	return (LeftDown() || LeftUp() || LeftDClick());
    case 2:
	return (MiddleDown() || MiddleUp() || MiddleDClick());
    case 3:
	return (RightDown() || RightUp() || RightDClick());
    }
    return FALSE;
}

Bool wxMouseEvent::ButtonIsDown(int but)
{
    switch (but) {
    case -1:
	return (LeftIsDown() || MiddleIsDown() || RightIsDown());
    case 1:
	return LeftIsDown();
    case 2:
	return MiddleIsDown();
    case 3:
	return RightIsDown();
    }
    return FALSE;
}

//-----------------------------------------------------------------------------
// wxKeyEvent
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxKeyEvent, wxEvent)

wxKeyEvent::wxKeyEvent(WXTYPE type) : wxEvent(type)
{
    __type = eventClass = wxTYPE_KEY_EVENT;
    shiftDown = controlDown = metaDown = altDown = FALSE;
    keyCode = 0;
    x = y = 0.0;
}
