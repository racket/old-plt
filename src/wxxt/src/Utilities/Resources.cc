/*								-*- C++ -*-
 * $Id: Resources.cc,v 1.3 1999/07/14 00:20:44 mflatt Exp $
 *
 * Purpose: read/write .Xdefaults
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

#define  Uses_XLib
#define  Uses_wxList
#include "wx.h"

#include <ctype.h>
#include <string.h>
#include <unistd.h>

//-----------------------------------------------------------------------------
// We have a cache for writing different resource files,
// which will only get flushed when we call wxFlushResources().
// Build up a list of resource databases waiting to be written.
//-----------------------------------------------------------------------------
//wxList wxResourceCache(wxKEY_STRING); // now in Application/wxGloabaData

//-----------------------------------------------------------------------------
// utility functions for get/write resources
//-----------------------------------------------------------------------------

static char *GetResourcePath(char *buf, char *name, Bool create = FALSE)
{
    if (create && FileExists(name)) {
      strcpy(buf, name);
      return buf; // Exists so ...
    }
    if (*name == '/')
      strcpy(buf, name);
    if (create) {
      // Touch the file to create it
      FILE *fd = fopen(buf, "w");
      if (fd) fclose(fd);
    }
    return buf;
}

static char *GetIniFile(char *dest, const char *filename)
{
    char *home = NULL;
    if (filename) {
      strcpy(dest, filename);
    } else if ((home = wxGetUserHome(NULL)) != NULL) {
      strcpy(dest, home);
      if (dest[strlen(dest) - 1] != '/')
	strcat(dest, "/");
      strcat(dest, ".mred.resources");
    } else {
      dest[0] = '\0';    
    }
    return dest;
}

static XrmDatabase wxXrmGetFileDatabase(const char *s)
{
  /* because directory names crash XrmGetFileDatabase */
  if (!wxDirExists((char *)s))
    return XrmGetFileDatabase(s);
  else
    return NULL;
}

static void wxXMergeDatabases(void)
{
    XrmDatabase homeDB, serverDB, applicationDB, userDB;
    char filenamebuf[1024];

    char *filename = &filenamebuf[0];
    char *environment;
    char *classname = wxAPP_CLASS;
    char name[256];
    (void)strcpy(name, "/usr/lib/X11/app-defaults/");
    /* MATTHEW: */
    (void)strcat(name, classname ? classname : "wxWindows");

    // Get application defaults file, if any 
    if ((applicationDB = wxXrmGetFileDatabase(name)))
      (void)XrmMergeDatabases(applicationDB, &wxResourceDatabase);

    // Merge server defaults, created by xrdb, loaded as a property of the root
    // window when the server initializes and loaded into the display
    // structure on XOpenDisplay;
    // if not defined, use .Xdefaults
    if (XResourceManagerString(wxAPP_DISPLAY) != NULL) {
	serverDB = XrmGetStringDatabase(XResourceManagerString(wxAPP_DISPLAY));
    } else {
      // Get X defaults file, if any 
      char *home = wxGetUserHome(NULL), *dest;
      if (home) {
	dest = new char[strlen(home) + 20];
	
	strcpy(dest, home);
	if (dest[strlen(dest) - 1] != '/')
	  strcat(dest, "/");
	strcat(dest, ".Xdefaults");
	
	serverDB = wxXrmGetFileDatabase(dest);
      } else
	serverDB = NULL;
    }
    if (serverDB)
      XrmMergeDatabases(serverDB, &wxResourceDatabase);

    // Open XENVIRONMENT file, or if not defined, the .Xdefaults,
    // and merge into existing database

    if ((environment = getenv("XENVIRONMENT")) == NULL) {
      size_t len;
      environment = GetIniFile(filename, NULL);
      len = strlen(environment);
#if !defined(SVR4) || defined(__sgi)
      (void)gethostname(environment + len, 1024 - len);
#else
      (void)sysinfo(SI_HOSTNAME, environment + len, 1024 - len);
#endif
    }
    if ((homeDB = wxXrmGetFileDatabase(environment)))
      XrmMergeDatabases(homeDB, &wxResourceDatabase);


    // Get user defaults file, if any 
    char *home = wxGetUserHome(NULL), *dest;
    if (home) {
      dest = new char[strlen(home) + 20];
      
      strcpy(dest, home);
      if (dest[strlen(dest) - 1] != '/')
	strcat(dest, "/");
      strcat(dest, ".mred.resources");
      
      if ((userDB = wxXrmGetFileDatabase(dest)))
	(void)XrmMergeDatabases(userDB, &wxResourceDatabase);
    }
}

//-----------------------------------------------------------------------------
// called on application exit
//-----------------------------------------------------------------------------
void wxFlushResources(void)
{
    char nameBuffer[512];

    wxNode *node = wxResourceCache.First();
    while (node) {
	char *file = node->key.string;
	// If file doesn't exist, create it first.
	(void)GetResourcePath(nameBuffer, file, TRUE);

	XrmDatabase database = (XrmDatabase)node->Data();
	XrmPutFileDatabase(database, nameBuffer);
	XrmDestroyDatabase(database);
	wxNode *next = node->Next();
	delete node;
	node = next;
    }
}

//-----------------------------------------------------------------------------
// write resource functions
//-----------------------------------------------------------------------------
Bool wxWriteResource(const char *section, const char *entry, char *value,
		     const char *file)
{
    char buffer[500];

    if (!entry)
      return FALSE;

    (void)GetIniFile(buffer, file);

    XrmDatabase database;
    wxNode *node = wxResourceCache.Find(buffer);
    if (node)
      database = (XrmDatabase)node->Data();
    else {
      database = wxXrmGetFileDatabase(buffer);
      node = wxResourceCache.Append(buffer, (wxObject *)database);
    }
    char resName[300];
    strcpy(resName, section ? section : "wxWindows");
    strcat(resName, ".");
    strcat(resName, entry);

    int isnull = !database;
    XrmPutStringResource(&database, resName, value);
    if (isnull) {
      if (node)
	wxResourceCache.DeleteNode(node);
      wxResourceCache.Append(buffer, (wxObject *)database);
    }

    XrmPutFileDatabase(database, buffer);

    return TRUE;
}

Bool wxWriteResource(const char *section, const char *entry, float value,
		     const char *file)
{
    char buf[50];
    sprintf(buf, "%.4f", value);
    return wxWriteResource(section, entry, buf, file);
}

Bool wxWriteResource(const char *section, const char *entry, long value,
		     const char *file)
{
    char buf[50];
    sprintf(buf, "%ld", value);
    return wxWriteResource(section, entry, buf, file);
}

Bool wxWriteResource(const char *section, const char *entry, int value,
		     const char *file)
{
    char buf[50];
    sprintf(buf, "%d", value);
    return wxWriteResource(section, entry, buf, file);
}

//-----------------------------------------------------------------------------
// get resource functions
//-----------------------------------------------------------------------------
Bool wxGetResource(const char *section, const char *entry, char **value,
		   const char *file)
{
    if (!wxResourceDatabase)
	wxXMergeDatabases();

    XrmDatabase database;
    if (file) {
      char buffer[500];
      (void)GetIniFile(buffer, file);
      
      wxNode *node = wxResourceCache.Find(buffer);
      if (node)
	database = (XrmDatabase)node->Data();
      else {
	database = wxXrmGetFileDatabase(buffer);
	wxResourceCache.Append(buffer, (wxObject *)database);
      }
    } else
      database = wxResourceDatabase;
    
    XrmValue xvalue;
    char *str_type[20];
    char buf[150];
    strcpy(buf, section);
    strcat(buf, ".");
    strcat(buf, entry);

    Bool success = XrmGetResource(database, buf, "*", str_type, &xvalue);
    if (success) {
      if (*value)
	delete[] *value;
      *value = new char[xvalue.size + 1];
      strncpy(*value, xvalue.addr, (int)xvalue.size);
      return TRUE;
    }
    return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, float *value,
		   const char *file)
{
    char *s = NULL;
    Bool succ = wxGetResource(section, entry, &s, file);
    if (succ) {
	*value = (float)strtod(s, NULL);
	delete[]s;
	return TRUE;
    } else
	return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, long *value,
		   const char *file)
{
    char *s = NULL;
    Bool succ = wxGetResource(section, entry, &s, file);
    if (succ) {
	*value = strtol(s, NULL, 10);
	delete[]s;
	return TRUE;
    } else
	return FALSE;
}

Bool wxGetResource(const char *section, const char *entry, int *value,
		   const char *file)
{
    char *s = NULL;
    Bool succ = wxGetResource(section, entry, &s, file);
    if (succ) {
	// Handle True, False here 
	// True, Yes, Enables, Set or  Activated 
	if (*s == 'T' || *s == 'Y' || *s == 'E' || *s == 'S' || *s == 'A')
	    *value = TRUE;
	// False, No, Disabled, Reset, Cleared, Deactivated
	else if (*s == 'F' || *s == 'N' || *s == 'D' || *s == 'R' || *s == 'C')
	    *value = FALSE;
	// Handle as Integer
	else
	    *value = (int)strtol(s, NULL, 10);
	delete[]s;
	return TRUE;
    } else
	return FALSE;
}
