/*								-*- C++ -*-
 * $Id: FileDialog.cc,v 1.2 1996/01/11 10:26:56 markus Exp $
 *
 * Purpose: file load and save dialogs
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

#include <limits.h> // for PATH_MAX
#include <sys/types.h>
#include <dirent.h> // for opendir, readdir, etc.
#include <unistd.h> // for getcwd
#include <string.h>

#define  Uses_wxDialogBox
#define  Uses_wxStringList
#define  Uses_wxButton
#define  Uses_wxLayout
#define  Uses_wxListBox
#define  Uses_wxMessage
#define  Uses_wxText
#include "wx.h"

//-----------------------------------------------------------------------------
// wxFileDialogBox, Dialog for file selection
//-----------------------------------------------------------------------------

class wxFileDialogBox : public wxDialogBox {
public:
    wxFileDialogBox(char *message, char *caption, wxWindow *parent,
		    char *default_path, char *default_filename,
		    char *wildcard, int flags);
    ~wxFileDialogBox(void)
	{ delete wildcard; }

    Bool OnClose(void);
    void OnCommand(wxWindow &win, wxCommandEvent &event);
    void OnDefaultAction(wxItem *item);
    void ReadDir(void);
    char *filename;
private:
    wxText	*file;
    wxText	*path;
    wxButton	*ok;
    wxButton	*cancel;
    wxListBox	*dir_box;
    wxListBox	*file_box;
    char	*wildcard;
    int		flags;
};

//-----------------------------------------------------------------------------
// implemetation of wxDialogFunctionBox
//-----------------------------------------------------------------------------

wxFileDialogBox::wxFileDialogBox(char *WXUNUSED(message), char *caption,
				 wxWindow *parent, char *default_path,
				 char *default_filename, char *_wildcard, int _flags)
{
    wxLayoutConstraints *constr;

    // initialize defaults if necessary
    char buf[PATH_MAX+1];
    if (!default_path)
	if ((default_path=getcwd(buf, PATH_MAX)) == NULL)
	    default_path = ".";
    if (!default_filename)
	if ((default_filename=_wildcard) == NULL)
	    default_filename = "";

    wildcard = copystring(_wildcard);
    flags    = _flags;
    filename = NULL;
    // create dialogbox
    Create(parent, caption, TRUE, -1, -1, -1, -1, wxDEFAULT_DIALOG_STYLE,
	   "fileDialogBox");
    SetLabelPosition(wxVERTICAL);
    // create panel items
    file     = DEBUG_NEW wxText(this, NULL, "Name", default_filename,
				-1, -1, -1, -1, wxPROCESS_ENTER);
    ok       = DEBUG_NEW wxButton(this, NULL, "Ok");
    path     = DEBUG_NEW wxText(this, NULL, "Path", default_path,
				-1, -1, -1, -1, wxPROCESS_ENTER);
    cancel   = DEBUG_NEW wxButton(this, NULL, "Cancel");
    dir_box  = DEBUG_NEW wxListBox(this, NULL, "Directories", FALSE, -1, -1, -1, -1,
				   0, NULL, wxNEEDED_SB|wxHSCROLL);
    file_box = DEBUG_NEW wxListBox(this, NULL, "Files", FALSE, -1, -1, -1, -1,
				   0, NULL, wxNEEDED_SB|wxHSCROLL);
    // set file layout constraints
    constr = DEBUG_NEW wxLayoutConstraints;
    constr->left.SameAs(this, wxLeft, 5);	constr->top.SameAs(this, wxTop, 5);
    constr->right.SameAs(ok, wxLeft, 5);	constr->height.AsIs();
    file->SetConstraints(constr);
    // set ok layout constraints
    constr = DEBUG_NEW wxLayoutConstraints;
    constr->right.SameAs(this, wxRight, 5);	constr->top.SameAs(this, wxTop, 5);
    constr->width.SameAs(cancel, wxWidth);	constr->height.SameAs(file, wxHeight);
    ok->SetConstraints(constr);
    // set path layout constraints
    constr = DEBUG_NEW wxLayoutConstraints;
    constr->left.SameAs(this, wxLeft, 5);	constr->top.Below(file, 5);
    constr->width.SameAs(file, wxWidth);	constr->height.AsIs();
    path->SetConstraints(constr);
    // set cancel layout constraints
    constr = DEBUG_NEW wxLayoutConstraints;
    constr->right.SameAs(this, wxRight, 5);	constr->top.SameAs(path, wxTop);
    constr->width.AsIs();			constr->height.SameAs(path, wxHeight);
    cancel->SetConstraints(constr);
    // set dir_box layout constraints
    constr = DEBUG_NEW wxLayoutConstraints;
    constr->left.SameAs(this, wxLeft, 5);	constr->right.SameAs(this, wxCentreX,-5);
    constr->top.Below(path, 5);			constr->bottom.SameAs(this, wxBottom, 5);
    dir_box->SetConstraints(constr);
    // set file_box layout constraints
    constr = DEBUG_NEW wxLayoutConstraints;
    constr->left.SameAs(this, wxCentreX, 5);	constr->right.SameAs(this, wxRight, 3);
    constr->top.Below(path, 5);			constr->bottom.SameAs(this, wxBottom, 5);
    file_box->SetConstraints(constr);
}

Bool wxFileDialogBox::OnClose(void)
{
    filename = NULL;
    Show(FALSE);
    return FALSE;
}

void wxFileDialogBox::OnCommand(wxWindow &win, wxCommandEvent &event)
{
    wxWindow *item = &win;
    Bool     reload_dir = FALSE;

    if (item == cancel) { // if cancel was pressed, close box and return
	filename = NULL;
	Show(FALSE);
	return;
    }
    if (item == file_box) {
	if (event.eventType == wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND) 
	  item = ok; // double click in filebox is the same as pressing ok
	else
	  file->SetValue(event.commandString);
    }
    if (item == file && event.eventType == wxEVENT_TYPE_TEXT_ENTER_COMMAND)
	item = ok; // enter in file is the same as pressing ok
    if (item == path && event.eventType == wxEVENT_TYPE_TEXT_ENTER_COMMAND) {
	reload_dir = TRUE; // path changed => reload dir
	item = ok; // enter in path is the same as pressing ok
    }
    if (item == dir_box && event.eventType == wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND) {
	char buf[PATH_MAX+1]; // add selected dir to path
	int  pathlen;
	strcpy(buf, path->GetValue());
	pathlen = strlen(buf);
	if (buf[pathlen-1] != '/')  buf[pathlen++] = '/'; // add trailing /
	strcpy(buf+pathlen, dir_box->GetStringSelection());
	path->SetValue(buf);
	reload_dir = TRUE; // path changed => reload dir
	item = ok; // same as pressing ok
    }
    if (item == ok) { // something happened to select file or to change dir
	// expand full path and split it in filename and path
	char buf[PATH_MAX+1];
	wxExpandPath(buf, path->GetValue());
	path->SetValue(buf);
	if (wxIsWild(file->GetValue())) {// file is wildcard => reload
	    delete wildcard;
	    wildcard = copystring(file->GetValue());
	    reload_dir = TRUE;
	}
	if (reload_dir) {
	    ReadDir();
	} else {
	    // I have a path and a filename: concat them and close box
	    int  pathlen;
	    pathlen = strlen(buf);
	    if (buf[pathlen-1] != '/')  buf[pathlen++] = '/'; // add trailing /
	    strcpy(buf+pathlen, file->GetValue());
	    filename = copystring(buf);
	    Show(FALSE);
	    return;
	}
    }
}

/* MATTHEW: [6] */
void wxFileDialogBox::OnDefaultAction(wxItem *item)
{
  if (item == file_box || item == dir_box) {
    wxCommandEvent event(wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND);
    OnCommand(*item, event);
  }
}

void wxFileDialogBox::ReadDir(void)
{
    char buf[PATH_MAX+1];
    int pathlen;
    strcpy(buf, path->GetValue());
    pathlen = strlen(buf);
    if (buf[pathlen-1] != '/')  buf[pathlen++] = '/'; // add trailing /
    buf[pathlen] = '\0';

    wxStringList files;
    wxStringList dirs;

    DIR *dirstream;
    if ((dirstream = opendir(buf))) {
	// busy cursor
	wxBeginBusyCursor();
	// parent directory
	if (strcmp(buf, "/"))  dirs.Add("..");
	struct dirent *dir;
	while ((dir = readdir(dirstream))) {
	    // skip . and ../skip .??? if !wxSHOW_HIDDEN
	    if (dir->d_name[0] == '.')
		if (dir->d_name[1] == '\0' || dir->d_name[1] == '.'
		|| !(flags & wxSHOW_HIDDEN))
		    continue; // . | .. | .* & !wxSHOW_HIDDEN
	    // complete path
	    strcpy(buf+pathlen, dir->d_name);
	    // is current dir entry a directory?
	    DIR *teststream;
	    if ((teststream = opendir(buf)) && (readdir(teststream))) {
		dirs.Add(dir->d_name);
	    } else if (wildcard && wxMatchWild(wildcard, dir->d_name)) {
		files.Add(dir->d_name);
	    }
	    if (teststream)  closedir(teststream);
	}
	// sort file and dir list
	dirs.Sort();
	dir_box->Set(&dirs);
	files.Sort();
	file_box->Set(&files);
	// close dirstream
	if (dirstream)
	    closedir(dirstream);
	// un-busy cursor
	wxEndBusyCursor();
    } else {
	wxMessageBox("Specified Dir not readable", "Warning", wxOK, this);
	return;
    }
}

//-----------------------------------------------------------------------------
// wxFileSelector
//-----------------------------------------------------------------------------
char *wxFileSelector(char *message, char *default_path,
		     char *default_filename, char *WXUNUSED(default_extension),
		     char *wildcard, int flags, wxWindow *parent, int x, int y)
{
    static char *filename = NULL;
    // if there is an old filename delete it
    if (filename)
	delete filename;
    filename = NULL;

    // busy cursor
    wxBeginBusyCursor();
    // create dialog box
    wxFileDialogBox *box = DEBUG_NEW wxFileDialogBox(NULL, message, parent,
						     default_path, default_filename,
						     wildcard, flags);
    box->SetSize(x, y, wxFSB_WIDTH, wxFSB_HEIGHT, wxSIZE_AUTO);
    box->Layout();
    box->ReadDir();
    box->Fit();
    if (x < 0) box->Centre(wxHORIZONTAL);
    if (y < 0) box->Centre(wxVERTICAL);
    // un-busy cursor
    wxEndBusyCursor();
    // show frame
    box->Show(TRUE);
    filename = box->filename; // now owned by this function
    delete box;
    return filename;
}

char *wxLoadFileSelector(char *WXUNUSED(what), char *extension, char *default_name,
			 wxWindow *parent)
{
    if (*extension == '.')  ++extension;
    char wild[60];
    sprintf(wild, "*.%s", extension);
    return wxFileSelector("Load file", NULL, default_name,
			  (char*)extension, wild, 0, parent);
}

char *wxSaveFileSelector(char *WXUNUSED(what), char *extension, char *default_name,
			 wxWindow *parent)
{
    if (*extension == '.')  ++extension;
    char wild[60];
    sprintf(wild, "*.%s", extension);
    return wxFileSelector("Save file", NULL, default_name,
			  (char*)extension, wild, 0, parent);
}
