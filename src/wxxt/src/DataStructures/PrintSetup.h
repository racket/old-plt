/*								-*- C++ -*-
 * $Id: PrintSetup.h,v 1.1 1996/01/10 14:55:33 markus Exp $
 *
 * Purpose: print setup for postscript printing (setup/papertype/paperdatabase)
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

#ifndef PrintSetup_h
#define PrintSetup_h

#ifdef __GNUG__
#pragma interface
#endif

extern void wxInitializePrintSetupData(Bool init = TRUE);

class wxPrintSetupData : public wxObject {
DECLARE_DYNAMIC_CLASS(wxPrintSetupData)
public:
    wxPrintSetupData(void);
    ~wxPrintSetupData(void);

    void operator = (wxPrintSetupData& data);

    void  SetPrinterCommand(char *cmd);
    void  SetPaperName(char *paper);
    void  SetPrintPreviewCommand(char *cmd);
    void  SetPrinterOptions(char *flags);
    void  SetPrinterFile(char *f);
    void  SetAFMPath(char *f);
    void  SetPrinterMode(int mode);
    void  SetPrinterOrientation(int orient)
	{ printer_orient = orient; }
    void  SetPrinterScaling(float x, float y)
	{ printer_scale_x = x; printer_scale_y = y; }
    void  SetPrinterTranslation(float x, float y)
	{ printer_translate_x = x; printer_translate_y = y; }
    void  SetColour(Bool col)
	{ print_colour = col; }

    inline char *GetPrinterCommand(void)
	{ return printer_command; }
    inline char *GetPrintPreviewCommand(void)
	{ return preview_command; }
    inline char *GetPrinterOptions(void)
	{ return printer_flags; }
    inline char *GetPrinterFile(void)
	{ return printer_file; }
    inline char *GetPaperName(void)
	{ return paper_name; }
    inline int GetPrinterOrientation(void)
	{  return printer_orient; }
    inline void GetPrinterScaling(float *x, float *y)
	{ *x=printer_scale_x; *y=printer_scale_y; }
    inline void GetPrinterTranslation(float *x, float *y)
	{ *x=printer_translate_x; *y=printer_translate_y; }
    inline int GetPrinterMode(void)
	{ return printer_mode; }
    inline char *GetAFMPath(void)
	{ return afm_path; }
    inline Bool GetColour(void)
	{ return print_colour; }
private:
    friend class wxPostScriptDC;

    char   *printer_command;
    char   *preview_command;
    char   *printer_flags;
    char   *printer_file;
    int    printer_orient;
    float  printer_scale_x;
    float  printer_scale_y;
    float  printer_translate_x;
    float  printer_translate_y;
    int    printer_mode;
    char   *afm_path;
    char   *paper_name;
    Bool   print_colour;
};

class wxPrintPaperType : public wxObject {
DECLARE_DYNAMIC_CLASS(wxPrintPaperType)
public:
    wxPrintPaperType(char *name=NULL, int wmm=0, int hmm=0, int wp=0, int hp=0);
    ~wxPrintPaperType(void);
public:
    int   widthMM;
    int   heightMM;
    int   widthPixels;
    int   heightPixels;
    char  *pageName;
};

class wxPrintPaperDatabase : public wxList {
DECLARE_DYNAMIC_CLASS(wxPrintPaperDatabase)
public:
    wxPrintPaperDatabase(void);
    ~wxPrintPaperDatabase(void);

    void CreateDatabase(void);
    void ClearDatabase(void);

    void AddPaperType(char *name, int wmm, int hmm, int wp, int hp);
    wxPrintPaperType *FindPaperType(char *name);
};

// for compatibility, use now wxThePrintSetupData->METHOD

#define wxSetPrinterCommand(cmd) \
	wxThePrintSetupData->SetPrinterCommand(cmd)
#define wxSetPrintPreviewCommand(cmd) \
	wxThePrintSetupData->SetPrintPreviewCommand(cmd)
/* MATTHEW */
#define wxSetPrinterOptions(flags) \
	wxThePrintSetupData->SetPrinterOptions(flags)
#define wxSetPrinterOrientation(orientation) \
	wxThePrintSetupData->SetPrinterOrientation(orientation)
#define wxSetPrinterScaling(x, y) \
	wxThePrintSetupData->SetPrinterScaling(x, y)
/* MATTHEW */
#define wxSetPrinterTranslation(x, y) \
	wxThePrintSetupData->SetPrinterTranslation(x, y)
#define wxSetPrinterMode(mode) \
	wxThePrintSetupData->SetPrinterMode(mode)
#define wxSetPrinterFile(f) \
	wxThePrintSetupData->SetPrinterFile(f)
#define wxSetAFMPath(f) \
	wxThePrintSetupData->SetAFMPath(f)
#define wxGetPrinterCommand() \
	wxThePrintSetupData->GetPrinterCommand()
#define wxGetPrintPreviewCommand() \
	wxThePrintSetupData->GetPrintPreviewCommand()
#define wxGetPrinterOptions() \
	wxThePrintSetupData->GetPrinterOptions()
/* MATTHEW */
#define wxGetPrinterOrientation() \
	wxThePrintSetupData->GetPrinterOrientation()
#define wxGetPrinterScaling(x, y) \
	wxThePrintSetupData->GetPrinterScaling(x, y)
/* MATTHEW */
#define wxGetPrinterTranslation(x, y) \
	wxThePrintSetupData->GetPrinterTranslation(x, y)
#define wxGetPrinterMode() \
	wxThePrintSetupData->GetPrinterMode()
#define wxGetPrinterFile() \
	wxThePrintSetupData->GetPrinterFile()
#define wxGetAFMPath() \
	wxThePrintSetupData->GetAFMPath()

#endif // PrintSetup_h
