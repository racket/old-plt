/*								-*- C++ -*-
 * $Id: Utilities.h,v 1.2 1996/01/11 10:27:12 markus Exp $
 *
 * Purpose: common utilities
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

#ifndef wxUtilities_h
#define wxUtilities_h

#ifdef __GNUG__
#pragma interface
#endif

#undef  min
#define min(a,b) ((a)<(b)?(a):(b))
#undef  max
#define max(a,b) ((a)>(b)?(a):(b))
#undef  wxNumberOf
#define wxNumberOf(arr) ((int)(sizeof(arr)/sizeof(arr[0])))

// application utilities
void wxExit(void);
void wxFlushEvents(void);
Bool wxYield(void);

// File functions
Bool wxConcatFiles(char *src1, char *src2, char *dest);
char *wxContractPath(const char *fname, const char *envname, const char *user);
char *wxCopyAbsolutePath(const char *path);
Bool wxCopyFile(char *src, char *dest);
Bool wxDirExists(char *dirname);
void wxDos2UnixFilename(char *s);
char *wxExpandPath(char *buf, const char *name);
Bool wxFileExists(char *filename);
char *wxFileNameFromPath(char *path);
char *wxFindFirstFile(const char *spec, int flags=0);
char *wxFindNextFile(void);
char *wxGetHomeDir(char *buf);
char *wxGetUserHome(const char *user);
char *wxGetWorkingDirectory(char *buf=NULL, int sz=0);
char *wxGetTempFileName(char *prefix, char *buff=NULL);
Bool wxIsAbsolutePath(char *filename);
Bool wxIsWild(char *pattern);
Bool wxMatchWild(char *pattern, char *text, Bool dot_special=FALSE);
Bool wxMkdir(char *dir);
char *wxPathOnly(char *path);
char *wxRealPath (char *path);
Bool wxRemoveFile(char *file);
Bool wxRenameFile(char *old, char *_new);
Bool wxRmdir(char *dir);
Bool wxSetWorkingDirectory(char *dir);
void wxStripExtension(char *buffer);
void wxUnix2DosFilename(char *s);

#define DirExists wxDirExists
#define Dos2UnixFilename wxDos2UnixFilename
#define FileExists wxFileExists
#define FileNameFromPath wxFileNameFromPath
#define IsAbsolutePath wxIsAbsolutePath
#define PathOnly wxPathOnly
#define Unix2DosFilename wxUnix2DosFilename

// Network functions
Bool wxGetHostName(char *buf, int sz);
Bool wxGetEmailAddress(char *buf, int sz);
Bool wxGetUserId(char *buf, int sz);
Bool wxGetUserName(char *buf, int sz);

// String functions
char *copystring(const char *s);
void wxStringToDouble(char *s, double *number);
void wxStringToFloat(char *s, float *number);
void wxStringToInt(char *s, int *number);
void wxStringToLong(char *s, long *number);
char *wxDoubleToString(double number);
char *wxFloatToString(float number);
char *wxIntToString(int number);
char *wxLongToString(long number);
void wxGetLabelAndKey(char *label, char **clean_label, char **clean_key);
Bool wxStringMatch(char *one, char *two, Bool subString = TRUE,
		   Bool exact = FALSE);
int  wxStringEq(char *s1, char *s2);
char *wxStripMenuCodes(char *in, char *out);

#define StringToDouble wxStringToDouble
#define StringToFloat wxStringToFloat
#define StringToInt wxStringToInt
#define StringToLong wxStringToLong
#define DoubleToString wxDoubleToString
#define FloatToString wxFloatToString
#define IntToString wxIntToString
#define LongToString wxLongToString
#define StringMatch wxStringMatch

#define wxToUpper(C) (((C) >= 'a' && (C) <= 'z')? (C) - 'a' + 'A': (C))
#define wxToLower(C) (((C) >= 'A' && (C) <= 'Z')? (C) - 'A' + 'a': (C))

// GDI functions
class wxCursor;
Bool wxColourDisplay(void);
int  wxDisplayDepth(void);
void wxDisplaySize(int *width, int *height);
void wxSetCursor(wxCursor *cursor);

// System event functions

// Printer settings

// Clipboard functions

// busy cursor
class wxCursor;
void wxBeginBusyCursor(wxCursor *cursor = wxHOURGLASS_CURSOR);
void wxEndBusyCursor(void);
Bool wxIsBusy(void);

// find wxWindows objects
class wxFrame;
class wxWindow;
int wxFindMenuItemId(wxFrame *frame, char *menuString, char *itemString);
wxWindow *wxFindWindowByLabel(char *title, wxWindow *parent=NULL);
wxWindow *wxFindWindowByName(char *title, wxWindow *parent=NULL);

// Miscellaneous
long wxGetCurrentId(void);
long NewId(void);
void RegisterId(long id);
void wxDebugMsg(const char *fmt, ...);
void wxError(const char *msg, const char *title="wxWindows Error");
Bool wxExecute(char **argv, Bool Async = FALSE);
Bool wxExecute(const char *command, Bool Async = FALSE);
void wxFatalError(const char *msg, const char *title="wxWindows Fatal Error");
long wxGetFreeMemory(void);
int wxGetOsVersion(int *majorVsn, int *minorVsn);
Bool wxShell(const char *command = NULL);
void wxSleep(int nSecs);
char *wxNow(void);

// read/write resources
#if USE_RESOURCES
Bool wxGetResource(const char *section, const char *entry, char **value,
		   const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, float *value,
		   const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, long *value,
		   const char *file = NULL);
Bool wxGetResource(const char *section, const char *entry, int *value,
		   const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, char *value,
		     const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, float value,
		     const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, long value,
		     const char *file = NULL);
Bool wxWriteResource(const char *section, const char *entry, int value,
		     const char *file = NULL);
#endif // USE_RESOURCES

#endif // wxUtilities_h
