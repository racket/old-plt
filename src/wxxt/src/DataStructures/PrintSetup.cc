/*								-*- C++ -*-
 * $Id: PrintSetup.cc,v 1.1 1996/01/10 14:55:32 markus Exp $
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

#ifdef __GNUG__
#pragma implementation "PrintSetup.h"
#endif

#define  Uses_wxPrintSetup
#include "wx.h"

//-----------------------------------------------------------------------------
// defaults for the different platforms (should be done with a WXWIN.INI)
//-----------------------------------------------------------------------------

#if 0
#define PS_DEFAULT_PAPER  "A4 210 x 297 mm"
#endif
#define PS_DEFAULT_PAPER  "Letter 8 1/2 x 11 in"

#if defined(sun) && defined(wx_xview)
#	define PS_PREVIEW_COMMAND	"pageview"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	NULL
#	define PS_AFM_PATH		NULL
#elif defined(VMS)
#	define PS_PREVIEW_COMMAND	"view/format=ps/select=x_display"
#	define PS_PRINTER_COMMAND	"print"
#	define PS_PRINTER_OPTIONS	"/nonotify/queue=psqueue"
#	define PS_AFM_PATH		"sys$ps_font_metrics:"
#elif defined(__sgi)
#	define PS_PREVIEW_COMMAND	"dps"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	NULL
#	define PS_AFM_PATH		NULL
#elif defined(wx_x)
#	define PS_PREVIEW_COMMAND	"ghostview"
#	define PS_PRINTER_COMMAND	"lpr"
#	define PS_PRINTER_OPTIONS	NULL
#	define PS_AFM_PATH		NULL
#elif defined(wx_msw)
#	define PS_PREVIEW_COMMAND	NULL
#	define PS_PRINTER_COMMAND	"print"
#	define PS_PRINTER_OPTIONS	NULL
#	define PS_AFM_PATH		"c:\\windows\\system\\"
#else
#	define PS_PREVIEW_COMMAND	NULL
#	define PS_PRINTER_COMMAND	NULL
#	define PS_PRINTER_OPTIONS	NULL
#	define PS_AFM_PATH		NULL
#endif

//-----------------------------------------------------------------------------
// wxPrintSetup implementation
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxPrintSetupData, wxObject)

wxPrintSetupData::wxPrintSetupData(void)
{
    printer_command = NULL;
    preview_command = NULL;
    printer_flags = NULL;
    printer_orient = PS_PORTRAIT;
    printer_scale_x = 1.0;
    printer_scale_y = 1.0;
    printer_translate_x = 0.0;
    printer_translate_y = 0.0;
    printer_mode = PS_PREVIEW;
    afm_path = NULL;
    paper_name = NULL;
    print_colour = TRUE;
    printer_file = NULL;
}

wxPrintSetupData::~wxPrintSetupData(void)
{
    if (printer_command)
	delete[] printer_command;
    if (preview_command)
	delete[] preview_command;
    if (printer_flags)
	delete[] printer_flags;
    if (afm_path)
	delete[] afm_path;
    if (paper_name)
	delete[] paper_name;
    if (printer_file)
	delete[] printer_file;
}

void wxPrintSetupData::SetPrinterCommand(char *cmd)
{
    if (cmd == printer_command)
	return;
    if (printer_command)
	delete[] printer_command;
    if (cmd)
	printer_command = copystring(cmd);
    else
	printer_command = NULL;
}

void wxPrintSetupData::SetPrintPreviewCommand(char *cmd)
{
    if (cmd == preview_command)
	return;
    if (preview_command)
	delete[] preview_command;
    if (cmd)
	preview_command = copystring(cmd);
    else
	preview_command = NULL;
}

void wxPrintSetupData::SetPaperName(char *name)
{
    if (name == paper_name)
	return;
    if (paper_name)
	delete[] paper_name;
    if (name)
	paper_name = copystring(name);
    else
	paper_name = NULL;
}

void wxPrintSetupData::SetPrinterOptions(char *flags)
{
    if (printer_flags == flags)
	return;
    if (printer_flags)
	delete[] printer_flags;
    if (flags)
	printer_flags = copystring(flags);
    else
	printer_flags = NULL;
}

void wxPrintSetupData::SetPrinterFile(char *f)
{
    if (f == printer_file)
	return;
    if (printer_file)
	delete[] printer_file;
    if (f)
	printer_file = copystring(f);
    else
	printer_file = NULL;
}

void wxPrintSetupData::SetPrinterMode(int mode)
{
    printer_mode = PS_FILE;

    if (mode == PS_PREVIEW && preview_command
    ||  mode == PS_PRINTER && printer_command)
	printer_mode = mode;
}

void wxPrintSetupData::SetAFMPath(char *f)
{
    if (f == afm_path)
	return;
    if (afm_path)
	delete[] afm_path;
    if (f)
	afm_path = copystring(f);
    else
	afm_path = NULL;
}

void wxPrintSetupData::operator=(wxPrintSetupData& data)
{
    float x, y;

    SetPrinterCommand(data.GetPrinterCommand());
    SetPrintPreviewCommand(data.GetPrintPreviewCommand());
    SetPrinterOptions(data.GetPrinterOptions());
    SetPrinterOrientation(data.GetPrinterOrientation());
    SetPrinterMode(data.GetPrinterMode());
    SetAFMPath(data.GetAFMPath());
    SetPaperName(data.GetPaperName());
    SetColour(data.GetColour());

    data.GetPrinterTranslation(&x, &y);
    SetPrinterTranslation(x, y);
    data.GetPrinterScaling(&x, &y);
    SetPrinterScaling(x, y);
}

//-----------------------------------------------------------------------------
// wxInitializePrintSetupData
//-----------------------------------------------------------------------------

void wxInitializePrintSetupData(Bool init)
{
    if (init) {
	wxThePrintSetupData = DEBUG_NEW wxPrintSetupData;

	wxThePrintSetupData->SetPrintPreviewCommand(PS_PREVIEW_COMMAND);
	wxThePrintSetupData->SetPrinterOrientation(PS_PORTRAIT);
	wxThePrintSetupData->SetPrinterMode(PS_PREVIEW);
	wxThePrintSetupData->SetPaperName(PS_DEFAULT_PAPER);
	wxThePrintSetupData->SetPrinterCommand(PS_PRINTER_COMMAND);
	wxThePrintSetupData->SetPrinterOptions(PS_PRINTER_OPTIONS);
	wxThePrintSetupData->SetAFMPath(PS_AFM_PATH);
    } else {
	if (wxThePrintSetupData)
	    delete wxThePrintSetupData;
	wxThePrintSetupData = NULL;
    }
}

//-----------------------------------------------------------------------------
// wxPrintPaperType implementation
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxPrintPaperType, wxObject)

wxPrintPaperType::wxPrintPaperType(char *name, int wmm, int hmm, int wp, int hp)
{
    widthMM = wmm;
    heightMM = hmm;
    widthPixels = wp;
    heightPixels = hp;
    pageName = copystring(name);
}

wxPrintPaperType::~wxPrintPaperType(void)
{
    delete[] pageName;
}


//-----------------------------------------------------------------------------
// wxPrintPaperDatabase implementation
//-----------------------------------------------------------------------------

IMPLEMENT_DYNAMIC_CLASS(wxPrintPaperDatabase, wxList)

wxPrintPaperDatabase::wxPrintPaperDatabase(void) : wxList(wxKEY_STRING)
{
    DeleteContents(TRUE);
}

wxPrintPaperDatabase::~wxPrintPaperDatabase(void)
{
}

void wxPrintPaperDatabase::CreateDatabase(void)
{
    // Need correct values for page size in pixels.
    // Each unit is one 'point' = 1/72 of an inch.
    // NOTE: WE NEED ALSO TO MAKE ADJUSTMENTS WHEN TRANSLATING
    // in wxPostScriptDC code, so we can start from top left.
    // So access this database and translate by appropriate number
    // of points for this paper size. OR IS IT OK ALREADY?
    // Can't remember where the PostScript origin is by default.
    // Heck, someone will know how to make it hunky-dory...
    // JACS 25/5/95
  
    AddPaperType("A4 210 x 297 mm", 210, 297,         595, 842);
    AddPaperType("A3 297 x 420 mm", 297, 420,         842, 1191);
    AddPaperType("Letter 8 1/2 x 11 in", 216, 279,    612, 791);
    AddPaperType("Legal 8 1/2 x 14 in", 216, 356,     612, 1009);
}

void wxPrintPaperDatabase::ClearDatabase(void)
{
    Clear();
}

void wxPrintPaperDatabase::AddPaperType(char *name, int wmm, int hmm,
					int wp, int hp)
{
    Append(name, DEBUG_NEW wxPrintPaperType(name, wmm, hmm, wp, hp));
}

wxPrintPaperType *wxPrintPaperDatabase::FindPaperType(char *name)
{
  wxNode *node;

    if (node = Find(name))
	return (wxPrintPaperType*)node->Data();
    else
	return NULL;
}
