/*								-*- C++ -*-
 * $Id: Help.h,v 1.1 1996/01/10 14:56:25 markus Exp $
 *
 * Purpose: connection to wxHelp server
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

#ifndef Help_h
#define Help_h

#ifdef __GNUG__
#pragma interface
#endif

class wxHelpInstance;

class wxHelpConnection : public wxConnection {
DECLARE_DYNAMIC_CLASS(wxHelpConnection)
public:
    wxHelpConnection(wxHelpInstance *instance);
    Bool OnDisconnect(void);
private:
    wxHelpInstance *helpInstance;
};

class wxHelpInstance : public wxClient {
DECLARE_DYNAMIC_CLASS(wxHelpInstance)
public:
    wxHelpInstance(Bool native = FALSE);
    ~wxHelpInstance(void);

    Bool DisplayBlock(long blockNo);
    Bool DisplayContents(void);
    Bool DisplaySection(int sectionNo);
    Bool Initialize(char *file, int server = -1);
    Bool KeywordSearch(char *k);
    Bool LoadFile(char *file = NULL);
    Bool Quit(void);

    virtual void OnQuit(void)
	{}

    Bool Run(void);
    inline wxConnection *OnMakeConnection(void)
	{ return new wxHelpConnection(this); }
private:
    friend class wxHelpConnection;

    char *helpFile;
    int  helpServer;
    char *helpHost;
    Bool helpRunning;
    Bool useNative;
    wxHelpConnection *helpConnection;
};

#endif // Help_h
