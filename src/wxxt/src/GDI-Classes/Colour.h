/*								-*- C++ -*-
 * $Id: Colour.h,v 1.1 1996/01/10 14:56:13 markus Exp $
 *
 * Purpose: classes to cover colours and colourmaps
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

#ifndef Colour_h
#define Colour_h

#ifdef __GNUG__
#pragma interface
#endif

#ifdef Have_X_Types
// easier access to X-Colormap
#define GETCOLORMAP(obj) (*((Colormap*)((obj)->GetHandle())))
#endif

class wxColour_Xintern;
class wxColourMap_Xintern;

class wxColourMap;

class wxColour : public wxObject { // colour representation
DECLARE_DYNAMIC_CLASS(wxColour)
public:
    wxColour(void);
    wxColour(unsigned char r, unsigned char g, unsigned char b);
    wxColour(wxColour& col);
    wxColour(const char *col);
    ~wxColour(void);

    wxColour& operator = (wxColour& src);
    wxColour& operator = (const char *src);

    Bool Ok(void) { return (X!=NULL); }

    void Get(unsigned char *r, unsigned char *b, unsigned char *g);
    void Set(unsigned char r, unsigned char b, unsigned char g);

    unsigned char Red(void);
    unsigned char Green(void);
    unsigned char Blue(void);

    // alloc and free X pixel value
    unsigned long GetPixel(wxColourMap* cmap=wxAPP_COLOURMAP);
    void FreePixel(Bool del);

    inline Bool  IsMutable(void) { return !locked; } 
    inline void  Lock(int d) { locked += d; }

private:
    friend wxColourMap;

    wxColour_Xintern* X; // the encapsulated X representation
    int locked;
};

class wxColourMap : public wxObject { // colourmap representation
DECLARE_DYNAMIC_CLASS(wxColourMap)
public:
    wxColourMap(Bool priv=TRUE);
    ~wxColourMap(void);

    void *GetHandle(void); // return Colormap*

    Bool Ok(void) { return (X!=NULL); }
private:
    friend wxColour;
    friend wxColourDatabase;

    wxColourMap_Xintern* X; // the encapsulated X representation
};

class wxColourDatabase : public wxList { // colour database representation
DECLARE_DYNAMIC_CLASS(wxColourDatabase)
public:
    wxColourDatabase(void) : wxList(wxKEY_STRING) {};
    ~wxColourDatabase(void);

    wxColour *FindColour(const char *colour);
    char *FindName(wxColour& colour);
};

#endif // Colour_h
