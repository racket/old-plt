/*								-*- C++ -*-
 * $Id: FontDirectory.cxx,v 1.2 1999/11/12 22:33:04 mflatt Exp $
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

#ifdef wx_xt
# define  Uses_wxApp
# define  Uses_wxFontNameDirectory
# include "wx.h"
# include <string.h>
#endif

char *font_defaults[] = {
  "FamilySystem", "System",
  "FamilyDefault", "Default",
  "FamilyRoman", "Roman",
  "FamilyDecorative", "Decorative",
  "FamilyModern", "Modern",
  "FamilyTeletype", "Teletype",
  "FamilySwiss", "Swiss",
  "FamilyScript", "Script",
  "FamilySymbol", "Symbol",

  "AfmMedium", "",
  "AfmBold", "Bo",
  "AfmLight", "",
  "AfmStraight", "",
  "AfmItalic", "${AfmSlant}",
  "AfmSlant", "O",
  "AfmStraightTimes", "",
  "AfmItalicTimes", "${AfmSlantTimes}",
  "AfmSlantTimes", "I",
  "AfmRoman", "Ro",
  "AfmTimes", "Times",
  "AfmHelvetica", "Helv",
  "AfmCourier", "Cour",

  "Afm___", "${AfmTimes,$[weight],$[style]}",

  "AfmTimes__", "${AfmTimes}${Afm$[weight]}${Afm$[style]Times}",
  "AfmTimesMediumStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimesLightStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimes_Italic", "${AfmTimes}${Afm$[weight]}${AfmItalicTimes}",
  "AfmTimes_Slant", "${AfmTimes}${Afm$[weight]}${AfmItalicTimes}",

  "AfmSwiss__", "${AfmHelvetica}${Afm$[weight]}${Afm$[style]}",
  "AfmModern__", "${AfmCourier}${Afm$[weight]}${Afm$[style]}",
  "AfmSymbol__", "Sym",

  "AfmTeletype__", "${AfmModern,$[weight],$[style]}",

  "PostScriptMediumStraight", "",
  "PostScriptMediumItalic", "-Oblique",
  "PostScriptMediumSlant", "-Oblique",
  "PostScriptLightStraight", "",
  "PostScriptLightItalic", "-Oblique",
  "PostScriptLightSlant", "-Oblique",
  "PostScriptBoldStraight", "-Bold",
  "PostScriptBoldItalic", "-BoldOblique",
  "PostScriptBoldSlant", "-BoldOblique",

  "PostScript___", "${PostScriptTimes,$[weight],$[style]}",

  "PostScriptTimesMedium", "",
  "PostScriptTimesLight", "",
  "PostScriptTimesBold", "Bold",

  "PostScriptTimes__", "Times${PostScript$[weight]$[style]}",
  "PostScriptTimesMediumStraight", "Times-Roman",
  "PostScriptTimesLightStraight", "Times-Roman",
  "PostScriptTimes_Slant", "Times-${PostScriptTimes$[weight]}Italic",
  "PostScriptTimes_Italic", "Times-${PostScriptTimes$[weight]}Italic",

  "PostScriptSwiss__", "Helvetica${PostScript$[weight]$[style]}",
  "PostScriptModern__", "Courier${PostScript$[weight]$[style]}",
  "PostScriptSymbol__", "Symbol",

  "PostScriptTeletype__", "${PostScriptModern,$[weight],$[style]}",

#ifdef wx_x
  "ScreenMedium", "medium",
  "ScreenBold", "bold",
  "ScreenLight", "light",
  "ScreenStraight", "r",
  "ScreenItalic", "i",
  "ScreenSlant", "o",

  "ScreenSystemBase", "*-lucida",
  "ScreenDefaultBase", "*-lucida",
  "ScreenRomanBase", "*-times",
  "ScreenDecorativeBase", "*-helvetica",
  "ScreenModernBase", "*-courier",
  "ScreenTeletypeBase", "*-lucidatypewriter",
  "ScreenSwissBase", "*-lucida",
  "ScreenScriptBase", "*-zapfchancery",
  "ScreenSymbolBase", "*-symbol",

  "ScreenStdSuffix", "-${Screen$[weight]}-${Screen$[style]}"
    "-normal-*-*-%d-*-*-*-*-*-*",

  "ScreenSystem__",
  "+-${ScreenSystemBase}${ScreenStdSuffix}",
  "ScreenDefault__",
  "+-${ScreenDefaultBase}${ScreenStdSuffix}",
  "ScreenRoman__",
  "+-${ScreenRomanBase}${ScreenStdSuffix}",
  "ScreenDecorative__",
  "+-${ScreenDecorativeBase}${ScreenStdSuffix}",
  "ScreenModern__",
  "+-${ScreenModernBase}${ScreenStdSuffix}",
  "ScreenTeletype__",
  "+-${ScreenTeletypeBase}${ScreenStdSuffix}",
  "ScreenSwiss__",
  "+-${ScreenSwissBase}${ScreenStdSuffix}",
  "ScreenScript__",
  "+-${ScreenScriptBase}${ScreenStdSuffix}",
  "ScreenSymbol__",
  "+-${ScreenSymbolBase}-medium-r-normal-*-*-%d-*-*-*-*-*-*",
#endif

#ifdef wx_msw
  "ScreenSystem__", "MS Sans Serif",
  "ScreenDefault__", "MS Sans Serif",
  "ScreenRoman__", "Times New Roman",
  "ScreenDecorative__", "Arial",
  "ScreenModern__", "Courier New",
  "ScreenTeletype__", "${ScreenModern$[weight];$[style]}",
  "ScreenSwiss__", "Arial",
  "ScreenScript__", "Arial",
  "ScreenSymbol__", "Symbol",
#endif

#ifdef wx_mac
  "ScreenDefault__", "applicationfont",
  "ScreenSystem__", "systemfont",
  "ScreenRoman__", "times",
  "ScreenDecorative__", "geneva",
  "ScreenModern__", "monaco", /* "courier" is also good */
  "ScreenTeletype__", "${ScreenModern,$[weight],$[style]}",
  "ScreenSwiss__", "helvetica",
  "ScreenScript__", "geneva",
  "ScreenSymbol__", "symbol",
#endif

  NULL
};

wxFontNameDirectory wxTheFontNameDirectory;

enum {
  wxWEIGHT_NORMAL,
  wxWEIGHT_BOLD,
  wxWEIGHT_LIGHT,
  wxNUM_WEIGHTS
  };

enum {
  wxSTYLE_NORMAL,
  wxSTYLE_ITALIC,
  wxSTYLE_SLANT,
  wxNUM_STYLES
  };

class wxSuffixMap {
 public:
  char *map[wxNUM_WEIGHTS][wxNUM_STYLES];
  void Initialize(const char *, const char *, int weight, int style);

  wxSuffixMap();
};

wxSuffixMap::wxSuffixMap() {
  int i, j;
  for (i = 0; i < wxNUM_WEIGHTS; i++)
    for (j = 0; j < wxNUM_STYLES; j++)
      map[i][j] = NULL;
}

class wxFontNameItem : public wxObject
{
 public:
  int id;
  int family;
  char *name;
  wxSuffixMap screen, printing, afm;
  Bool isfamily;
};

static int WCoordinate(int w)
{
  switch (w) {
  case wxBOLD:
    return wxWEIGHT_BOLD;
  case wxLIGHT:
    return wxWEIGHT_LIGHT;
  case wxNORMAL:
  default:
    return wxWEIGHT_NORMAL;
  }
}

static int SCoordinate(int s)
{
  switch (s) {
  case wxITALIC:
    return wxSTYLE_ITALIC;
  case wxSLANT:
    return wxSTYLE_SLANT;
  case wxNORMAL:
  default:
    return wxSTYLE_NORMAL;
  }
}

wxFontNameDirectory::wxFontNameDirectory(void)
{
  table = new wxHashTable(wxKEY_INTEGER, 20);
  nextFontId = 100; /* Larger than all family ids */
}

wxFontNameDirectory::~wxFontNameDirectory()
{
  delete table;
}

int wxFontNameDirectory::GetNewFontId(void)
{
  return (nextFontId++);
}

#ifdef wx_x
# define GET_CLASS_NAME wxTheApp->GetClassName()
#else
# define GET_CLASS_NAME wxTheApp->wx_class
#endif

static void SearchResource(const char *prefix, const char **names, int count, char **v)
{
  int k, i, j;
  char resource[1024], **defaults, *internal;

  k = 1 << count;

  *v = NULL;
  internal = NULL;

  for (i = 0; i < k; i++) {
    strcpy(resource, prefix);
    for (j = 0; j < count; j++) {
      if (!(i & (1 << j)))
	strcat(resource, names[j]);
      else
	strcat(resource, "_");
    }

    if (wxGetResource(GET_CLASS_NAME, (char *)resource, v) && **v)
      return;

    if (!internal) {
      defaults = font_defaults;
      while (*defaults) {
	if (!strcmp(*defaults, resource)) {
	  internal = defaults[1];
	  break;
	}
	defaults += 2;
      }
    }
  }

  if (internal)
    *v = copystring(internal);
}

void wxFontNameDirectory::Initialize()
{
  wxTheFontNameDirectory.Initialize(wxSYSTEM, wxSYSTEM, "System");
  wxTheFontNameDirectory.Initialize(wxDEFAULT, wxDEFAULT, "Default");
  wxTheFontNameDirectory.Initialize(wxDECORATIVE, wxDECORATIVE, "Decorative");
  wxTheFontNameDirectory.Initialize(wxROMAN, wxROMAN, "Roman");
  wxTheFontNameDirectory.Initialize(wxMODERN, wxMODERN, "Modern");
  wxTheFontNameDirectory.Initialize(wxTELETYPE, wxTELETYPE, "Teletype");
  wxTheFontNameDirectory.Initialize(wxSWISS, wxSWISS, "Swiss");
  wxTheFontNameDirectory.Initialize(wxSCRIPT, wxSCRIPT, "Script");
  wxTheFontNameDirectory.Initialize(wxSYMBOL, wxSYMBOL, "Symbol");
}

typedef char *a_charptr;

void wxSuffixMap::Initialize(const char *resname, const char *devresname,
			     int wt, int st)
{
  const char *weight, *style;
  char *v = NULL, *rname;
  int i;
  const char *names[3];

  {
    switch (wt) {
    case wxWEIGHT_NORMAL:
      weight = "Medium";
      break;
    case wxWEIGHT_LIGHT:
      weight = "Light";
      break;
    case wxWEIGHT_BOLD:
      default:
      weight = "Bold";
    }
    {
      switch (st) {
      case wxSTYLE_NORMAL:
	style = "Straight";
	break;
      case wxSTYLE_ITALIC:
	style = "Italic";
	break;
      case wxSTYLE_SLANT:
      default:
	style = "Slant";
      }

      names[0] = resname;
      names[1] = weight;
      names[2] = style;
      
      SearchResource(devresname, names, 3, &v);

      /* Expand macros in the found string: */
    found:
      int len, closer = 0, startpos = 0;

      len = (v ? strlen(v) : 0);
      for (i = 0; i < len; i++)
	if (v[i] == '$' && ((v[i+1] == '[') || (v[i+1] == '{'))) {
	  startpos = i;
	  if (v[i+1] == '[')
	    closer = ']';
	  else
	    closer = '}';
	  i++;
	} else if (v[i] == closer) {
	  int newstrlen;
	  const char *r = NULL;
	  char *naya, *name;
	  
	  name = v + startpos + 2;
	  v[i] = 0;

	  if (closer == '}') {
	    int i, count, len;
	    char **names;

	    for (i = 0, count = 1; name[i]; i++)
	      if (name[i] == ',')
		count++;
	    
	    len = i;

	    names = new a_charptr[count];
	    names[0] = name;
	    for (i = 0, count = 1; i < len; i++)
	      if (name[i] == ',') {
		names[count++] = name + i + 1;
		name[i] = 0;
	      }

	    SearchResource("", (const char **)names, count, (char **)&r);
	    delete[] names;

	    if (!r) {
	      for (i = 0; i < len; i++)
		if (!name[i])
		  name[i] = ',';
	      r = "";
	      printf("Bad resource name \"%s\" in font lookup\n", name);
	    }
	  } else if (!strcmp(name, "weight")) {
	    r = weight;
	  } else if (!strcmp(name, "style")) {
	    r = style;
	  } else if (!strcmp(name, "family")) {
	    r = resname;
	  } else {
	    r = "";
	    printf("Bad font macro name \"%s\"\n", name);
	  }
	  newstrlen = strlen(r);

	  naya = new char[len + newstrlen + 1];
	  memcpy(naya, v, startpos);
	  memcpy(naya + startpos, r, newstrlen);
	  memcpy(naya + startpos + newstrlen, v + i + 1, len - i + 1);

	  delete[] v;
	  v = naya;

	  goto found;
	}

      rname = (char *)((resname[0] == '@') ? resname + 1 : resname);

#if defined(wx_msw) || defined(wx_mac)
      if (!v)
	v = copystring(rname);
#endif
#ifdef wx_x
      if (!strcmp(devresname, "Screen")) {
	if (v && (v[0] == '+')) {
	  memmove(v, v + 1, strlen(v));
	} else {
	  int len;
	  char *src;
	  char *normalcy;
	  /* Build name using special heuristics:
	     -([^-]*) => -*-\1-<weight>-<style>-normal-*-*-%d-*-*-*-*-*-*
	     -([^-]*)-(.*) => -\1-\2-<weight>-<style>-normal-*-*-%d-*-*-*-*-*-*
	     ([^-].*[^-]) => \1
	     */
	  src = (v ? v : (char *)rname);
	  len = strlen(src);
	  if (src[0] == '-') {
	    int c = 0;
	    for (i = 0; i < len; i++)
	      if (src[i] == '-')
		c++;
	    
	    v = new char[len + 40];
	    char *prefix;
	    if (c < 2)
	      prefix = "-*";
	    else
	      prefix = "";
	    
	    if (c < 3) {
	      switch (wt) {
	      case wxWEIGHT_NORMAL:
		weight = "-medium";
		break;
	      case wxWEIGHT_LIGHT:
		weight = "-light";
		break;
	      case wxWEIGHT_BOLD:
	      default:
		weight = "-bold";
	      }
	    } else
	      weight = "";
	    
	    if (c < 4) {
	      switch (st) {
	      case wxSTYLE_NORMAL:
		style = "-r";
	      break;
	      case wxSTYLE_ITALIC:
		style = "-i";
		break;
	      case wxSTYLE_SLANT:
	      default:
		style = "-o";
	      }
	    } else
	      style = "";
	    
	    if (c < 5)
	      normalcy = "-normal";
	    else
	      normalcy = "";
	    
	    sprintf(v, "%s%s%s%s%s-*-*-%%d-*-*-*-*-*-*",
		    prefix, src, weight, style, normalcy);
	  } else
	    v = copystring(src);
	}
      }
#endif

      /* We have a final value: */
      map[wt][st] = v;
    }
  }
}

void wxFontNameDirectory::Initialize(int fontid, int family, const char *resname)
{
  wxFontNameItem *item = new wxFontNameItem;
  char *fam, resource[256];
  
  item->id = fontid;
  item->family = family;
  item->isfamily = (resname[0] != '@');
  
  sprintf(resource, "Family%s", resname);
  fam = NULL;
  SearchResource((const char *)resource, NULL, 0, (char **)&fam);
  if (fam) {
    if (!strcmp(fam, "System"))
      item->family = wxSYSTEM;
    else if (!strcmp(fam, "Default"))
      item->family = wxDEFAULT;
    else if (!strcmp(fam, "Roman"))
      item->family = wxROMAN;
    else if (!strcmp(fam, "Decorative"))
      item->family = wxDECORATIVE;
    else if (!strcmp(fam, "Modern"))
      item->family = wxMODERN;
    else if (!strcmp(fam, "Teletype"))
      item->family = wxTELETYPE;
    else if (!strcmp(fam, "Swiss"))
      item->family = wxSWISS;
    else if (!strcmp(fam, "Script"))
      item->family = wxSCRIPT;
    else if (!strcmp(fam, "Symbol"))
      item->family = wxSYMBOL;
  }

  item->name = copystring(resname);

  table->Put(fontid, item);
}

int wxFontNameDirectory::FindOrCreateFontId(const char *name, int family)
{
  int id;

  if ((id = GetFontId(name)))
    return id;

  id = GetNewFontId();
  char *s = new char[strlen(name) + 2];
  strcpy(s + 1, name);
  s[0] = '@';
  Initialize(id, family, s);

  return id;
}

char *wxFontNameDirectory::GetScreenName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* Check for init */
  if (!item->screen.map[wt][st])
    item->screen.Initialize(item->name, "Screen", wt, st);

  return item->screen.map[wt][st];
}

void wxFontNameDirectory::SetScreenName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return;

  int wt = WCoordinate(weight), st = SCoordinate(style);

#ifdef wx_x
  /* Safety: name must be less than 500 chars, and must not contain %
     except maybe one instance of %d. */
  int i, found_d = 0;
  for (i = 0; s[i]; i++) {
    if (i > 500) {
      s = NULL;
      break;
    }
    if (s[i] == '%') {
      if (found_d || (s[i+1] != 'd')) {
	s = NULL;
	break;
      } else
	found_d = 1;
    }
  }

  if (!s)
    return;
#endif

  item->screen.map[wt][st] = s;
}

char *wxFontNameDirectory::GetPostScriptName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* Check for init */
  if (!item->printing.map[wt][st])
    item->printing.Initialize(item->name, "PostScript", wt, st);

  return item->printing.map[wt][st];
}

void wxFontNameDirectory::SetPostScriptName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  item->printing.map[wt][st] = s;
}

char *wxFontNameDirectory::GetAFMName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* Check for init */
  if (!item->afm.map[wt][st])
    item->afm.Initialize(item->name, "Afm", wt, st);

  return item->afm.map[wt][st];
}

void wxFontNameDirectory::SetAFMName(int fontid, int weight, int style, char *s)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  item->afm.map[wt][st] = s;
}

char *wxFontNameDirectory::GetFontName(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  if (item->isfamily)
    return NULL;

  return item->name + 1;
}

int wxFontNameDirectory::GetFontId(const char *name)
{
  wxNode *node;

  table->BeginFind();

  while ((node = table->Next())) {
    wxFontNameItem *item = (wxFontNameItem *)node->Data();
    if (!item->isfamily && !strcmp(name, item->name+1))
      return item->id;
  }

  return 0;
}

int wxFontNameDirectory::GetFamily(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return wxDEFAULT;

  return item->family;
}


