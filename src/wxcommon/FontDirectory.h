 /*								-*- C++ -*-
 * $Id: FontDirectory.h,v 1.3 1999/11/04 17:25:34 mflatt Exp $
 *
 * Purpose: wxWindows font name handling
 *
 * Authors: Markus Holzem, Julian Smart, and Matthew Flatt
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

#ifndef FontDirectory_h
#define FontDirectory_h

#ifdef __GNUG__
#pragma interface
#endif


class wxFontNameDirectory : public wxObject
{
  class wxHashTable *table;
  int nextFontId;
 public:
  wxFontNameDirectory(void);
  ~wxFontNameDirectory();
  void Initialize(void);
  char *GetScreenName(int fontid, int weight, int style);
  char *GetPostScriptName(int fontid, int weight, int style);
  char *GetAFMName(int fontid, int weight, int style);
  void SetScreenName(int fontid, int weight, int style, char *s);
  void SetPostScriptName(int fontid, int weight, int style, char *s);
  void SetAFMName(int fontid, int weight, int style, char *s);

  void Initialize(int fontid, int family, const char *name);
  int GetNewFontId(void);
  
  int FindOrCreateFontId(const char *name, int family);

  int GetFontId(const char *name);
  char *GetFontName(int fontid);
  int GetFamily(int fontid);
};

extern wxFontNameDirectory wxTheFontNameDirectory;

#endif /* FontDirectory_h */
