 /*								-*- C++ -*-
 * $Id: FontDirectory.cc,v 1.1 1996/01/10 14:56:14 markus Exp $
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

#ifdef __GNUG__
#pragma implementation "FontDirectory.h"
#endif

#define  Uses_wxApp
#define  Uses_wxFontNameDirectory
#include "wx.h"
#include <string.h>

char *font_defaults[] = {
  "FamilySystem", "System",
  "FamilyDefault", "Default",
  "FamilyRoman", "Roman",
  "FamilyDecorative", "Decorative",
  "FamilyModern", "Modern",
  "FamilyTeletype", "Teletype",
  "FamilySwiss", "Swiss",
  "FamilyScript", "Script",

  "AfmMedium", "",
  "AfmBold", "Bo",
  "AfmLight", "",
  "AfmStraight", "",
  "AfmItalic", "${AfmSlant}",
  "AfmSlant", "O",
  "AfmRoman", "Ro",
  "AfmTimes", "Times",
  "AfmHelvetica", "Helv",
  "AfmCourier", "Cour",

  "Afm___", "${AfmTimes,$[weight],$[style]}",

  "AfmTimes__", "${AfmTimes}${Afm$[weight]}${Afm$[style]}",
  "AfmTimesMediumStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimesLightStraight", "${AfmTimes}${AfmRoman}",
  "AfmTimes_Italic", "${AfmTimes}$[weight]${AfmItalic}",
  "AfmTimes_Slant", "${AfmTimes}$[weight]${AfmItalic}",

  "AfmSwiss__", "${AfmHelvetica}${Afm$[weight]}${Afm$[style]}",
  "AfmModern__", "${AfmCourier}${Afm$[weight]}${Afm$[style]}",

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

#if WX_NORMALIZED_PS_FONTS
  "PostScript___", "${PostScriptTimes,$[weight],$[style]}",
#else
  "PostScriptRoman__", "${PostScriptTimes,$[weight],$[style]}",
  "PostScript___", "LucidaSans${PostScript$[weight]$[style]}",
#endif

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

  "PostScriptTeletype__", "${PostScriptModern,$[weight],$[style]}",

#if !WX_NORMALIZED_PS_FONTS
  "PostScriptScript__", "Zapf-Chancery-MediumItalic",
#endif

  "ScreenMedium", "medium",
  "ScreenBold", "bold",
  "ScreenLight", "light",
  "ScreenStraight", "r",
  "ScreenItalic", "i",
  "ScreenSlant", "o",

  "ScreenSystemBase", "*-lucida",
  "ScreenDefaultBase", "misc-fixed",
  "ScreenRomanBase", "*-times",
  "ScreenDecorativeBase", "*-helvetica",
  "ScreenModernBase", "*-courier",
  "ScreenTeletypeBase", "*-lucidatypewriter",
  "ScreenSwissBase", "*-lucida",
  "ScreenScriptBase", "*-zapfchancery",

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
  NULL
};

IMPLEMENT_DYNAMIC_CLASS(wxFontNameDirectory, wxObject)

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
  void Initialize(const char *, const char *, int weight, int style); /* MATTHEW: [6] */

  wxSuffixMap();
};

  /* MATTHEW: [6] zero map initially: */
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
  Bool isroman;
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
  nextFontId = 10; /* Larger than all family ids */
}

wxFontNameDirectory::~wxFontNameDirectory()
{
  delete table;
}

int wxFontNameDirectory::GetNewFontId(void)
{
  return (nextFontId++);
}

#if !USE_RESOURCES
#define wxGetResource(a, b, c) 0
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

    if (wxGetResource(wxTheApp->GetClassName(), (char *)resource, v) && **v)
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
}

typedef char *a_charptr;

/* MATTHEW: [6] Pass in weight & style and just do one */
void wxSuffixMap::Initialize(const char *resname, const char *devresname,
			     int wt, int st)
{
  const char *weight, *style;
  char *v = NULL;
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
	  src = (v ? v : (char *)resname);
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
  }

  item->name = copystring(resname);

  /* MATTHEW: [6] Delay this: */
#if 0
  item->screen.Initialize(resname, "Screen");
  item->printing.Initialize(resname, "PostScript");
  item->afm.Initialize(resname, "Afm");
#endif

  table->Put(fontid, item);
}

int wxFontNameDirectory::FindOrCreateFontId(const char *name, int family)
{
  int id;

  if ((id = GetFontId(name)))
    return id;

  id = GetNewFontId();
  Initialize(id, family, name);

  return id;
}

char *wxFontNameDirectory::GetScreenName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* MATTHEW: [6] Check for init */
  if (!item->screen.map[wt][st])
    item->screen.Initialize(item->name, "Screen", wt, st);

  return item->screen.map[wt][st];
}

char *wxFontNameDirectory::GetPostScriptName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* MATTHEW: [6] Check for init */
  if (!item->printing.map[wt][st])
    item->printing.Initialize(item->name, "PostScript", wt, st);

  return item->printing.map[wt][st];
}

char *wxFontNameDirectory::GetAFMName(int fontid, int weight, int style)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);

  if (!item)
    return NULL;

  int wt = WCoordinate(weight), st = SCoordinate(style);

  /* MATTHEW: [6] Check for init */
  if (!item->afm.map[wt][st])
    item->afm.Initialize(item->name, "Afm", wt, st);

  return item->afm.map[wt][st];
}

char *wxFontNameDirectory::GetFontName(int fontid)
{
  wxFontNameItem *item = (wxFontNameItem *)table->Get(fontid);
  
  if (!item)
    return NULL;

  return item->name;
}

int wxFontNameDirectory::GetFontId(const char *name)
{
  wxNode *node;

  table->BeginFind();

  while ((node = table->Next())) {
    wxFontNameItem *item = (wxFontNameItem *)node->Data();
    if (!strcmp(name, item->name))
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


